#include <llvm/ADT/Twine.h>

#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Constants.h>

#include <llvm/Pass.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Regex.h>

#include <llvm/Transforms/Utils/Cloning.h>

#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DebugInfoMetadata.h"

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <string>
#include <set>

using namespace llvm;

#define oprint(s) errs() << s << "\n"
#define oprintt(s) errs() << s << "\t"

static cl::opt<std::string>
obfFunc("instrument-func",
    cl::desc("Specify the single function to instrument"),
    cl::init(""), cl::NotHidden);

static cl::list<std::string>
Locations("instrument",
    cl::desc("Specify all the comma-separated lines ('$') / llvm IR identifiers ('%') where OSR points should be created"),
    cl::ZeroOrMore, cl::CommaSeparated, cl::NotHidden);

static cl::opt<int>
cclones("clones",
    cl::desc("Specify the amount of clones to produce for the function to instrument"),
    cl::init(1), cl::Optional);

static cl::opt<bool>
bbsnamed("bbs-already-named",
    cl::desc("Specify whether the pass needs to assign names to BBs (true) or not (false)"),
    cl::init(false), cl::Optional);

static cl::opt<bool>
noprints("suppress-prints",
    cl::desc("Specify whether to suppress prints or not"),
    cl::init(false), cl::Optional);

namespace {
    class OSRCreatorPass: public ModulePass {

    public:
        static char ID;
        unsigned long unique_id = 0;

        typedef std::vector<const llvm::Value*> IDToValueVec;

        Type* i32_type = nullptr;
        ArrayType* ArrayTy_0 = nullptr;
        Constant* zero = nullptr;

        std::string bbidGlobalPrefix = "BBID_";
        std::string variantsArrayPrefix = "alts_";
        std::string coinFunctionName = "toss_coin";
        std::string diceFunctionName = "throw_dice";

        std::map<Function*, std::vector<Function*>> clones;

        OSRCreatorPass(): ModulePass(ID) {}

        unsigned long getUniqueID() {
            return ++unique_id;
        }

        /**
         * @brief Create a Global Variable
         *
         * @param M the module
         * @param t the type of the variable
         * @param name the name of the variable
         * @return GlobalVariable*
         */
        GlobalVariable* createGlobalVar(Module& M, Type* t, std::string name) {
            M.getOrInsertGlobal(name, t);
            GlobalVariable* gVar = M.getNamedGlobal(name);

            gVar->setLinkage(GlobalValue::ExternalLinkage);
            gVar->setAlignment(4);
            gVar->setInitializer(this->zero);
            return gVar;
        }

        /**
         * @brief assign an unique id to the BasicBlocks in a function
         *
         * @param F the function whose BB's should be assigned identifiers
         */
        void assignIdToBBs(Function& F) {
            int j = 1;
            for (BasicBlock& BB : F.getBasicBlockList()) {
                BB.setName(Twine("BB_", std::to_string(j++)).str());
            }
        }

        /**
         * @brief Taken from
         * https://github.com/dcdelia/tinyvm/blob/7095461154d52e5dbef66ffba573854c6fb5fda0/src/Parser.cpp#L734
         */
        IDToValueVec computeSlotIDs(Function* F) {
            IDToValueVec vec;

#define REGSLOT(val)    vec.push_back(val)

            for (Function::const_arg_iterator arg = F->arg_begin(), endArg = F->arg_end();
                arg != endArg; ++arg) {
                if (!arg->hasName()) REGSLOT(arg);
            }

            for (BasicBlock& B : F->getBasicBlockList()) {
                if (!B.hasName()) REGSLOT(&B);

                for (Instruction& I : B.getInstList()) {
                    if (!I.getType()->isVoidTy() && !I.hasName()) REGSLOT(&I);
                }
            }

#undef REGSLOT

            return vec;
        }

        /**
         * @brief Taken from
         * https://github.com/dcdelia/tinyvm/blob/7095461154d52e5dbef66ffba573854c6fb5fda0/src/Parser.cpp#L716
         */
        IDToValueVec computeLineIDs(Function* F) {
            IDToValueVec vec;

#define REGLINE(val)    vec.push_back(val)
            for (BasicBlock& B : F->getBasicBlockList()) {

                REGLINE(&B);

                for (Instruction& I : B.getInstList()) {
                    REGLINE(&I);
                }

                vec.push_back(nullptr); // separate blocks as in LLVM's F->dump()
            }
#undef REGLINE

            return vec;
        }

        /**
         * @brief Convert a string into an IR ID
         *
         * Taken from:
         * https://github.com/dcdelia/tinyvm/blob/7095461154d52e5dbef66ffba573854c6fb5fda0/src/Parser.cpp#L826
         *
         * @param F function to work on
         * @param StrID identifier as a string
         * @param slotIDs
         * @param lineIDs
         * @return LLVM Value corresponding to the ID
         */
        const Value* getValueFromStrID(Function& F, std::string& StrID, IDToValueVec* slotIDs, IDToValueVec* lineIDs) {
            int length = StrID.length();
            if (length < 2) return nullptr;

            bool isLineNumber = false;

            if (StrID[0] == '$') isLineNumber = true;
            else if (StrID[0] != '%') return nullptr;

            std::string::const_iterator it = ++StrID.begin(); // skip first char
            while (it != StrID.end() && std::isdigit(*it)) ++it;
            bool isNumber = (it == StrID.end());

            // line number
            if (isLineNumber) {
                if (!isNumber || !lineIDs) return nullptr;
                int ID = (int)strtol(StrID.c_str() + 1, NULL, 10);

                if (ID == 0 || ID > lineIDs->size()) return nullptr;
                return (*lineIDs)[ID - 1];
            }

            // anonymous LLVM Value
            if (isNumber) {
                if (!slotIDs) return nullptr;
                int ID = (int)strtol(StrID.c_str() + 1, NULL, 10);
                if (ID >= slotIDs->size()) return nullptr;
                return (*slotIDs)[ID];
            }

            // named LLVM Value
            std::string name = StrID.substr(1, length - 1);
            llvm::StringRef nameRef(name);

            for (Function::const_arg_iterator arg = F.arg_begin(), endArg = F.arg_end();
                arg != endArg; ++arg) {
                if (arg->hasName() && arg->getName().equals(nameRef)) return arg;
            }

            for (BasicBlock& B : F.getBasicBlockList()) {
                if (B.hasName() && B.getName().equals(nameRef)) return &B;

                for (Instruction& I : B.getInstList()) {
                    if (I.hasName() && I.getName().equals(nameRef)) return &I;
                }
            }

            return nullptr;
        }

        /**
         * @brief identify a location in the IR from a string
         *
         * Taken from:
         * https://github.com/dcdelia/tinyvm/blob/7095461154d52e5dbef66ffba573854c6fb5fda0/src/Parser.cpp#L877
         *
         * @param F function to work on
         * @param LocID id of the location as a string
         * @return the llvm Instruction corresponding to @param LocID
         */
        const Instruction* getOSRLocationFromStrIDs(Function& F, const std::string& LocID) {
            IDToValueVec slotIDs = computeSlotIDs(&F);
            IDToValueVec lineIDs = computeLineIDs(&F);

            const Value* v = getValueFromStrID(F, const_cast<std::string&>(LocID), &slotIDs, &lineIDs);
            if (v == nullptr) {
                oprint("Unable to find location " << LocID << " in function " << F.getName().str()
                    << ". Did you forget to put the \'%\' (or \'$\') prefix?");
                return nullptr;
            }

            if (const BasicBlock* B = dyn_cast<BasicBlock>(v)) {
                oprint("BB Received!: " << *(B->getFirstNonPHI()));

                return B->getFirstNonPHI();
            }

            if (const Instruction* I = dyn_cast<Instruction>(v)) {
                if (isa<PHINode>(I)) {
                    oprint("ERROR: PHI instructions can't be used as OSR locations!");
                    return nullptr;
                }
                oprint("Instruction Received!: " << *I);
                return I;
            }

            oprint("ERROR: only instructions and basic blocks are valid OSR locations!");
            return nullptr;
        }

        /**
         * @brief Identify the IR points where to place OSR points
         *
         * @param F function the points belong to
         * @param strings list of IR identifiers of the points
         * @return list of instructions corresponding to the IDs from @param strings
         */
        std::vector<const Instruction*> identifyOSRPoints(Function& F, const std::vector<std::string>& strings) {
            std::vector<const Instruction*> obfPoints;

            for (const std::string s : strings) {
                const Instruction* inst = getOSRLocationFromStrIDs(F, s);
                obfPoints.push_back(inst);
            }

            return obfPoints;
        }

        /**
         * @brief Instrument a function to support OSR,
         * create the variants of said function and
         * fill the global array of variants.
         *
         * @param F function to instrument
         */
        void instrument(Function& F) {
            oprint(F.getName());

            Module* thisModule = F.getParent();

            Function* randFunc = thisModule->getFunction(coinFunctionName);
            Function* diceFunc = thisModule->getFunction(diceFunctionName);

            /* Clone the original function in other to decouple the store operations
            of the variants' pointers from the OSR instrumentation */
            Function* copy = createClone(&F, Twine(F.getName(), "_orig"));

            std::vector<BasicBlock*> BBsToInstrument;
            std::vector<std::string> instrumentedBBsNames;

            std::vector<const Instruction*> osrPoints;
            /* If BBs have not been named yet, identify the OSR points and assign IDs to them */
            if (!bbsnamed) {
                osrPoints = identifyOSRPoints(*copy, Locations);
                assignIdToBBs(*copy);
            }
            else { /* Just identify the already-named BBs */
                for (BasicBlock& BB : copy->getBasicBlockList()) {
                    for (std::string loc : Locations) {
                        if (BB.getName() == loc)
                            osrPoints.push_back(BB.getFirstNonPHI()); // Should this become `getInstList().front();` ?
                    }
                }
            }

            for (BasicBlock& BB : copy->getBasicBlockList()) {
                const Instruction* firstInst = BB.getFirstNonPHI(); // Should this become `getInstList().front();` ?
                auto it = find(osrPoints.begin(), osrPoints.end(), firstInst);
                if (it != osrPoints.end()) {
                    oprint(*firstInst);

                    BBsToInstrument.push_back(&BB);
                }
            }

            /* For func_orig, use the same input values as the original function */
            std::vector<Value*> Fargs;
            for (auto it = F.arg_begin(); it != F.arg_end(); it++) {
                //oprint("Arg: " << *it);
                Fargs.push_back(it);
            }

            std::vector<Value*> copyArgs;
            /* For the variants, use the same input values as the "original" function */
            for (auto it = copy->arg_begin(); it != copy->arg_end(); it++) {
                copyArgs.push_back(it);
            }

            /* Instrument the initial function for the OSR jumps */
            int j = 0;
            for (BasicBlock* BB : BBsToInstrument) {
                //oprint("Instrumenting BB: " << *BB->getFirstNonPHI());
                oprint("Instrumenting BB: " << BB->getName() << " - " << *BB->getFirstNonPHI()); // Should this become `getInstList().front();` ?

                j++;

                std::string oldName = BB->getName().str();

                /* Split the original block to avoid having to deal with branch instructions */
                Instruction* origFirstInst = &BB->getInstList().front();
                BasicBlock* origBlock = BB->splitBasicBlock(origFirstInst, "placeHolder");

                /* Create the condition block to decide whether to continue normal execution or to perform an OSR jump */
                BasicBlock* condBlock = origBlock->getPrevNode();
                condBlock->setName(Twine("CondBB_", std::to_string(j)));
                /* Remove the unconditional br created by splitBasicBlock()*/
                condBlock->getInstList().front().removeFromParent();
                origBlock->setName(oldName);
                IRBuilder<> condBuilder(condBlock);

                /* Call toss_coin() to decide whether to OSR-jump or not */
                oprint("Creating call to randFunc");
                CallInst* condCall = condBuilder.CreateCall(randFunc);
                oprint("Call Created");
                Value* condition = condBuilder.CreateICmpEQ(condCall, this->zero);

                /* Create the BB to handle the OSR-firing operations */
                BasicBlock* osrFireBlock = BasicBlock::Create(copy->getContext(), Twine("OSRBB_", std::to_string(j)).str(), copy, origBlock);
                oprint("Created new");

                IRBuilder<> osrFireBuilder(osrFireBlock);

                /* Store the ID of the Block in the global Var for this function */
                GlobalVariable* key = thisModule->getNamedGlobal(this->bbidGlobalPrefix + F.getName().str());
                std::string bbName = origBlock->getName();
                int id = std::stoi(bbName.substr(bbName.find('_') + 1, bbName.length()));
                /*StoreInst* store = */osrFireBuilder.CreateStore(osrFireBuilder.getInt32(id), key);

                /* Load the array of variants from the global variable and pick one */
                GlobalVariable* arr = thisModule->getNamedGlobal(this->variantsArrayPrefix + F.getName().str());
                LoadInst* load = osrFireBuilder.CreateLoad(arr);

                oprint("load created");

                /* Throw dice to choose which variant to pick as OSR target */
                std::vector<Value*> faces;
                faces.push_back(osrFireBuilder.getInt32(cclones));

                CallInst* coin = osrFireBuilder.CreateCall(diceFunc, faces);
                Value* mySel;
                CallInst* call;

                /* Extract a variant from the array according to the resulting dice throw */
                mySel = osrFireBuilder.CreateExtractValue(load, 0);
                for (int i = 1; i < cclones; i++) {
                    //oprint(i);
                    Value* ext = osrFireBuilder.CreateExtractValue(load, i);

                    Value* altCond = osrFireBuilder.CreateICmpEQ(coin, toConst(i));

                    mySel = osrFireBuilder.CreateSelect(altCond, ext, mySel);
                }
                oprint("Extraction created");

                /* Call the selected variant via indirect call */
                call = osrFireBuilder.CreateCall(mySel, copyArgs);
                CallInst* callI = cast<CallInst>(call);
                /* Transfrom the call into a tail call (which will become a jmp instruction in ASM) */
                callI->setTailCall(true);
                oprint("jump created");

                /* Create a return instruction to prevent the resuming the execution
                from the function performing the OSR jump */
                if (F.getReturnType() == Type::getVoidTy(F.getContext())) {
                    oprint("F returns void!");
                    osrFireBuilder.CreateRetVoid();
                }
                else
                    osrFireBuilder.CreateRet(callI);

                /* Add a conditional branch to decide what to do according to the coin toss */
                condBuilder.CreateCondBr(condition, osrFireBlock, origBlock);

                instrumentedBBsNames.push_back(oldName);
            }
            //oprint(*copy);

            /* Create copies of the instrumented F, they will become the variants to OSR-jump to */
            oprint("Starting cloning");
            createClones(copy);
            oprint("Cloning ended");

            /* Store the pointers to the clones into the global array for that function */
            std::string globalArrName = this->variantsArrayPrefix + F.getName().str();
            oprint("Trying to read: " << globalArrName);

            std::vector<Constant*> cs;
            for (Function* cl : clones[copy]) {
                Constant* tmp = dyn_cast<Constant>(cl);
                cs.push_back(ConstantExpr::getBitCast(tmp, cl->getType()));
            }

            ArrayRef<Constant*> alts(cs);
            Constant* array = ConstantArray::get(this->ArrayTy_0, alts);

            GlobalVariable* arr = thisModule->getNamedGlobal(this->variantsArrayPrefix + F.getName().str());

            /* Delete the body from the original F and insert a call to its copy 
            This is used to decouple the operation of filling the array of variants
            from the OSR-instrumented body of the original function */
            F.deleteBody();
            BasicBlock* setUpBlock = BasicBlock::Create(F.getContext(), "OSRSetup", &F);
            IRBuilder<> setUpBlockBuilder(setUpBlock);

            /* Store the array of function pointers inside the first BB of the original F */
            setUpBlockBuilder.CreateStore(array, arr);
            /* Call the OSR-instrumented copy */
            CallInst* oCall = setUpBlockBuilder.CreateCall(copy, Fargs);

            if (F.getReturnType() == Type::getVoidTy(F.getContext())) {
                oprint("F returns void!");
                setUpBlockBuilder.CreateRetVoid();
            }
            else
                setUpBlockBuilder.CreateRet(oCall);

            /* Instrument the clones of F_orig */
            instrumentClones(copy, instrumentedBBsNames, F.getName().str());
        }

        /**
         * @brief Instrument the clones of a function to support OSR.
         * This is done by setting up an additional entry block which
         * checks the value of the label that represents the point
         * where the execution should resume from.
         *
         * @param F the original function
         * @param instrumentedBBs the IDs of the BasicBlocks instrumented in @param F
         * @param origName the original name of @param F
         */
        void instrumentClones(Function* F, const std::vector<std::string>& instrumentedBBs, std::string origName) {
            oprint("Instrumenting clones of function " << F->getName());

            /* For each clone of F */
            for (Function* clone : clones[F]) {
                oprint(clone->getName());

                std::map<int, BasicBlock*> IDToBBMap;

                /* Get a ID<->BB mapping */
                for (BasicBlock& BB : clone->getBasicBlockList()) {
                    for (std::string BBname : instrumentedBBs) {
                        if (BB.getName() == BBname) {
                            int id = std::stoi(BBname.substr(BBname.find('_') + 1, BBname.length()));

                            IDToBBMap[id] = &BB;
                        }
                    }
                }

                /* Create an initial BB to handle the resumption of the execution */
                BasicBlock& initialBlock = clone->getBasicBlockList().front();
                BasicBlock* newEntryBlock = BasicBlock::Create(clone->getContext(), "newOSREntry", clone, &initialBlock);

                /* Load the label containing the BB id where the execution should be resumed from */
                IRBuilder<> newEntryBuilder(newEntryBlock);
                GlobalVariable* key = clone->getParent()->getNamedGlobal(this->bbidGlobalPrefix + origName);
                LoadInst* loadGlobal = newEntryBuilder.CreateLoad(key);

                // {BEGIN} Probably excessively complex way to make a variant print something @ runtime
                if (!noprints) {
                    Type* intType = Type::getInt32Ty(clone->getContext());
                    std::vector<Type*> printfArgsTypes({ Type::getInt8PtrTy(clone->getContext()) });
                    FunctionType* printfType = FunctionType::get(intType, printfArgsTypes, true);
                    FunctionCallee printfFunc = clone->getParent()->getOrInsertFunction("printf", printfType);
                    Value* str = newEntryBuilder.CreateGlobalStringPtr("[OSR] I am: %s - GlobalVar: %d\n", "str");
                    Value* funName = newEntryBuilder.CreateGlobalStringPtr(clone->getName().str(), "fnname");
                    std::vector<Value*> argsV;
                    argsV.push_back(str);
                    argsV.push_back(funName);
                    argsV.push_back(loadGlobal);
                    newEntryBuilder.CreateCall(printfFunc, argsV, "calltmp");
                }
                // {END} Probably excessively complex way to make a variant print something @ runtime

                /* Create a cascade of `select` operations to identify the actual BB to jump to */
                Value* mySel = nullptr;
                std::vector<BasicBlock*> dests;
                for (std::map<int, BasicBlock*>::iterator it = IDToBBMap.begin(); it != IDToBBMap.end(); it++) {
                    int id = it->first;
                    BasicBlock* BB = it->second;
                    Constant* constID = toConst(id);

                    Value* condition = newEntryBuilder.CreateICmpEQ(loadGlobal, constID);
                    if (mySel == nullptr) {
                        mySel = newEntryBuilder.CreateSelect(condition, BlockAddress::get(BB), BlockAddress::get(&initialBlock));
                        dests.push_back(&initialBlock);
                    }
                    else {
                        mySel = newEntryBuilder.CreateSelect(condition, BlockAddress::get(BB), mySel);
                    }
                    dests.push_back(BB);
                }

                /* Reset the label */
                newEntryBuilder.CreateStore(newEntryBuilder.getInt32(0), key);

                /* Create an indirect branch to the BB in order to resume the execution */
                IndirectBrInst* indBr = newEntryBuilder.CreateIndirectBr(mySel, IDToBBMap.size());
                for (BasicBlock* dest : dests)
                    indBr->addDestination(dest);
            }
        }

        /**
         * @brief replace a Constant's value with a new one
         *
         * @param C constant
         * @param newV new value
         * @param oldV old value
         * @return replaced constant
         */
        Constant* replaceConstant(Constant* C, Constant* newV, Constant* oldV) {
            if (ConstantStruct* S = dyn_cast<ConstantStruct>(C)) {
                SmallVector<Constant*, 8> Ops;
                for (unsigned i = 0, e = S->getNumOperands(); i != e; ++i) {
                    Constant* op = S->getOperand(i);
                    if (op == oldV)
                        Ops.push_back(newV);
                    else
                        Ops.push_back(replaceConstant(op, newV, oldV));
                }

                Constant* res = ConstantStruct::getAnon(Ops, true);
                return res;

            }
            else if (ConstantExpr* E = dyn_cast<ConstantExpr>(C)) {
                SmallVector<Constant*, 8> Ops;
                for (unsigned i = 0, e = E->getNumOperands(); i != e; ++i) {
                    Constant* op = E->getOperand(i);
                    if (op == oldV)
                        Ops.push_back(newV);
                    else
                        Ops.push_back(replaceConstant(op, newV, oldV));
                }

                Constant* res = E->getWithOperands(Ops);
                return res;

            }
            else {
                return C;
            }
        }

        /**
         * @brief Fix the prologue of a cloned function
         *
         * @param newF cloned function
         * @param oldF original function
         */
        void fixPrologue(Function* newF, Function* oldF) {
            if (!newF->hasPrologueData()) return;

            Constant* prologue = replaceConstant(newF->getPrologueData(), newF, oldF);
            newF->setPrologueData(prologue);
        }

        /**
         * @brief Create a single clone of a function
         *
         * @param F function to clone
         * @param name name of the clone
         * @return a clone of @param F
         */
        Function* createClone(Function* F, Twine name) {
            ValueToValueMapTy VMap;
            Function* clone = CloneFunction(F, VMap);

            clone->setName(name);
            fixPrologue(clone, F);

            return clone;
        }

        /**
         * @brief Create multiple clones of a Function
         * and put them in the clones map
         *
         * @param F function to be cloned
         */
        void createClones(Function* F) {
            oprint("Cloning: " << F->getName() << " x" << itostr(cclones) << " times!");

            for (int j = 0; j < cclones; j++) {
                int i = clones[F].size() + 1;
                Twine name = F->getName() + "_" + std::to_string(i);
                oprint(F->getName() + "_" + std::to_string(i));

                Function* clone = createClone(F, name);

                clones[F].push_back(clone);
            }
            oprint("Created: " << clones[F].size() << " new clones of Function " << F->getName());
        }

        /**
         * @brief Convert an integer to a ConstantInt
         *
         * @param a integer to convert
         * @return resulting ConstantInt
         */
        Constant* toConst(int a) {
            return ConstantInt::get(this->i32_type, a, true);
        }

        /**
         * @brief self-explanatory
         *
         * @param M module
         * @return true as the module will be modified
         */
        virtual bool runOnModule(Module& M) override {
            oprint("Running...");

            /* Initialize the constants and types used multiple times across this pass */
            this->i32_type = IntegerType::getInt32Ty(M.getContext());
            this->zero = ConstantInt::get(this->i32_type, 0, true);

            for (Function& F : M.getFunctionList()) {
                if (F.getName().str() == "toss_coin" || F.getName().str() == "throw_dice") {
                    oprint("***Function found: " << F.getName().str());
                    F.removeFnAttr(Attribute::OptimizeNone);
                    //F.removeFnAttr(Attribute::NoInline);
                    //F.addFnAttr(Attribute::AlwaysInline);
                }
            }


            std::set<Function*> functionSet;
            for (auto& F : M.getFunctionList()) {
                if (F.isDeclaration())
                    continue;

                const std::string& FName = F.getName();

                if (FName == obfFunc) {
                    oprint("Obfuscating function: " << obfFunc);
                    functionSet.insert(&F);
                }
            }

            while (!functionSet.empty()) {
                Function* F = *functionSet.begin();
                functionSet.erase(functionSet.begin());

                /* Firstly, create the global variable that will hold the BB's ID */
                createGlobalVar(M, Type::getInt32Ty(M.getContext()), this->bbidGlobalPrefix + F->getName().str());

                /* Initialize the global var that will contain the variants for F */
                this->ArrayTy_0 = ArrayType::get(F->getType(), cclones);

                std::string globalArrName = this->variantsArrayPrefix + F->getName().str();
                oprint("GlobalArrName: " << globalArrName);

                /*auto globalArr = */M.getOrInsertGlobal(globalArrName, this->ArrayTy_0);
                GlobalVariable* arr = M.getNamedGlobal(globalArrName);

                arr->setLinkage(GlobalValue::LinkageTypes::CommonLinkage);
                arr->setAlignment(4); // or 16?
                ConstantAggregateZero* const_array_2 = ConstantAggregateZero::get(this->ArrayTy_0);
                arr->setInitializer(const_array_2);

                instrument(*F);
            }

            return true;
        }
    };
} // namespace

char OSRCreatorPass::ID = 0;
RegisterPass<OSRCreatorPass> MP("osr-creator", "OSR Points Insertion Pass");