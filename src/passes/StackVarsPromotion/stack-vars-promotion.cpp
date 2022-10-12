#include "pass.h"

#include "llvm/IR/Module.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/CFG.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Transforms/Utils/CodeExtractor.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/LoopInfo.h"

#include "llvm/ADT/SCCIterator.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/Analysis/CallGraphSCCPass.h"
#include "llvm/IR/CFG.h"

#include <set>
using namespace llvm;

#define DEBUG_TYPE "stack-vars-promotion"
#define stackVarsPromotionPassLog(M) LLVM_DEBUG(dbgs() << "stackVarsPromotionPass: " << M << "\n")

#define oprint(s) outs() << s << "\n"
#define qprint(s) std::cout << s << std::endl

#define STACK_PROM_DEBUG 0

static cl::list<std::string>
Functions("stack-vars-promotion-funcs",
    cl::desc("Specify all the comma-separated function regexes for which to promote stack vars"),
    cl::ZeroOrMore, cl::CommaSeparated, cl::NotHidden);

static cl::opt<std::string>
funcPrefix("functions-prefix",
    cl::desc("Specify the prefix to use to decide the subset of functions to consider"),
    cl::init(""), cl::NotHidden);

typedef long imd_t;

namespace {

    class StackVarsPromotionPass : public ModulePass {

    public:
        static char ID;
        unsigned long unique_id;
        std::set<Function*> SCCFunctions;
        std::map<Function*, std::set<Function*>> FunctionToSCC;

        StackVarsPromotionPass() : ModulePass(ID) {
            unique_id = 0;
        }

        unsigned long getUniqueID() {
            return unique_id++;
        }

        /**
         * @brief Check if `F` just calls itself
         * 
         * @param F function to check
         * @return true if F calls itself
         * @return false if F does not call itself
         */
        bool isSimplyRecursive(Function* F) {
            for (auto& BB : *F)
                for (auto& I : BB.instructionsWithoutDebug())
                    if (auto* CB = dyn_cast<CallBase>(&I)) {
                        Function* Callee = dyn_cast<Function>(CB->getCalledOperand()->stripPointerCasts());

                        // Function calls itself
                        if (Callee == F) {
                            return true;
                        }
                    }
            return false;
        }

        /**
         * @brief Visit the `SCC` to gather the information 
         * needed in `FunctionToSCC` and `SCCFunctions`
         * 
         * @param SCC the Strongly Connected Component
         */
        void collectSCC(CallGraphSCC& SCC) {
            std::set<Function*> Functions;
            for (CallGraphNode* I : SCC) {
                Functions.insert(I->getFunction());
            }

            // If the SCC contains multiple nodes we know there is recursion.
            if (Functions.size() != 1) {
                for (Function* F : Functions) {
                    SCCFunctions.insert(F);
                    assert(!F->doesNotRecurse());

                    // A function should belong to a single SCC
                    assert(FunctionToSCC.find(F) == FunctionToSCC.end());
                    FunctionToSCC[F] = Functions;
                }
                // Take into account simple recursive functions
            }
            else {
                Function* F = *Functions.begin();
                if (F && isSimplyRecursive(F)) {
                    SCCFunctions.insert(F);
                    assert(!F->doesNotRecurse());

                    assert(FunctionToSCC.find(F) == FunctionToSCC.end());
                    FunctionToSCC[F] = Functions;
                }
            }
        }

        /**
         * @brief Return true if `F` is part of a SCC
         * 
         * @param F function to check
         * @return true if `F` is in a SCC
         * @return false if `F` is not in any SCC
         */
        bool isInSCC(Function* F) {
            return SCCFunctions.find(F) != SCCFunctions.end();
        }        

        /**
         * @brief Sometimes LLVM builds the CallGraph withouth taking into consideration those calls
         * that pass through a `bitcast` operation. We fix this here, revisiting the
         * functions and updating the CallGraph
         * 
         * @param M the module being analyzed
         * @param CG `M`'s Call Graph
         */
        void fixCallGraph(Module& M, CallGraph* CG) {
            oprint("Fixing the CG...");
            
            for (auto& F : M.getFunctionList()) {
                if (F.isDeclaration())
                    continue;
                for (auto& BB : F) {
                    for (auto& I : BB) {
                        if (CallBase* CB = dyn_cast<CallBase>(&I)) {
                            if (CB->isInlineAsm()) continue;

                            Function* Called = dyn_cast<Function>(CB->getCalledOperand()->stripPointerCasts());
                            if (!Called || Called->isDeclaration() || Called->isIntrinsic()) continue;

                            // If `Called` actually points to a function, but getCalledFunction
                            // returns null then we have spotted a missing function
                            if (CB->getCalledFunction() == nullptr) {
                                CallGraphNode* Node = CG->getOrInsertFunction(&F);
                                Node->addCalledFunction(CB, CG->getOrInsertFunction(Called));
                            }
                        }
                    }
                }
            }
        }


        /**
         * @brief promote static alloca to global variables
         * 
         * @param M the module being analyzed
         * @param allocas the alloca instructions to promote
         */
        void promoteStackVars(Module& M, std::set<AllocaInst*> allocas) {
            // DataLayout* DL = new DataLayout(&M);
            // iterate over all the allocas and promote them to global variables
            for (AllocaInst* AI : allocas) {

                Type* allocatedType = AI->getAllocatedType();
                Constant* const_init = Constant::getNullValue(allocatedType);
                Function* F = AI->getParent()->getParent();

                // if the function allocating the variable may recurse it is not safe to promote it
                if (isSimplyRecursive(F) || isInSCC(F)) {
                    oprint("Skipping " << F->getName().str() << " (Recursive or in SCC)");
                    continue;
                }

                std::string globalName = "promoted_stack_var_" + F->getName().str() + "_" + std::to_string(getUniqueID());
                M.getOrInsertGlobal(globalName, allocatedType);
                GlobalVariable* promotedVar = M.getNamedGlobal(globalName);

                promotedVar->setLinkage(GlobalValue::InternalLinkage);
                // if (STACK_PROM_DEBUG)
                promotedVar->setAlignment(64);
                // else
                    // promotedVar->setAlignment(AI->getAlignment());
                promotedVar->setInitializer(const_init);

                oprint("Promoting " << *AI << " (" << AI << ")" << " in " << F->getName().str() << " \n  to " << *promotedVar);
                //oprint("  of size: " << DL->getTypeAllocSize(allocatedType));

                AI->replaceAllUsesWith(promotedVar);
                AI->eraseFromParent();
            }
        }

        /**
         * @brief Collect the stack variables inside a function
         * 
         * @param F function to collect the stack vars from
         * @param allocas set where the collected vars will be saved
         */
        void collectStackVars(Function& F, std::set<AllocaInst*>& allocas) {
            // avoid promoting stack vars of a function that may recurse
            oprint("collecting stack vars in " << F.getName().str());
            oprint("recurses? " << (!(isSimplyRecursive(&F) || isInSCC(&F)) ? "no" : "yes"));

            // collect all the stack vars which are accessed by a tainted mem operation
            for (auto& BB : F) {
                for (auto& I : BB) {
                    if (AllocaInst* v = dyn_cast<AllocaInst>(&I)) {
                        // For now manage only static alloca
                        assert(v->isStaticAlloca());
                        if (!v->isStaticAlloca()) {
                            oprint("Continuing...");
                            continue; 
                        }
                        allocas.insert(const_cast<AllocaInst*>(v));
                    }
                }
            }
        }

        /**
         * @brief self-explanatory
         * 
         * @param M module
         * @return true as the module will be modified
         * @return false in case of errors
         */
        virtual bool runOnModule(Module& M) override {
            stackVarsPromotionPassLog("Running...");
            //DataLayout* DL = new DataLayout(&M); // unused

            /* Get the CG, fix it and collect SCC's */
            CallGraph* CG = &getAnalysis<CallGraphWrapperPass>().getCallGraph();
            // LLVM does not consider edges like `call (bitcast (func))` so insert them.
            // really llvm??
            fixCallGraph(M, CG);

            // Walk the callgraph in bottom-up SCC order.
            scc_iterator<CallGraph*> CGI = scc_begin(CG);

            CallGraphSCC CurSCC(*CG, &CGI);
            while (!CGI.isAtEnd()) {
                // Copy the current SCC and increment past it so that the pass can hack
                // on the SCC if it wants to without invalidating our iterator.
                const std::vector<CallGraphNode*>& NodeVec = *CGI;
                CurSCC.initialize(NodeVec);
                ++CGI;

                collectSCC(CurSCC);
            }            


            /* Iterate all functions in the module to collect which stack vars to promote */
            std::set<Function*> functionSet;
            std::set<AllocaInst*> allocas;
            std::vector<Regex*> FunctionRegexes;
            if (Functions.empty()) {
                if (funcPrefix == "") {
                    oprint("You must either provide a set of functions to work on (--obfuscate-funcs) or a prefix (--functions-prefix)");
                    return false;
                }
                else {
                    for (Function& F : M.getFunctionList()) {
                        if (F.isDeclaration())
                            continue;
                        if (F.getName().str().rfind(funcPrefix, 0) == 0)
                            functionSet.insert(&F);
                    }
                }
            }
            else {
                passListRegexInit(FunctionRegexes, Functions);

                for (auto& F : M.getFunctionList()) {
                    if (F.isDeclaration())
                        continue;

                    const std::string& FName = F.getName();
                    if (!passListRegexMatch(FunctionRegexes, FName))
                        continue;

                    functionSet.insert(&F);
                }
            }
            while (!functionSet.empty()) {
                Function* F = *functionSet.begin();
                functionSet.erase(functionSet.begin());
                collectStackVars(*F, allocas);
            }
            promoteStackVars(M, allocas);
            return true;
        }

        void getAnalysisUsage(AnalysisUsage& AU) const override {
            AU.addRequired<DominatorTreeWrapperPass>();
            AU.addRequired<PostDominatorTreeWrapperPass>();
            AU.addRequired<CallGraphWrapperPass>();
        }
    };

}

char StackVarsPromotionPass::ID = 0;
RegisterPass<StackVarsPromotionPass> MP("stack-vars-promotion", "Stack Vars Promotion Pass");