#!/bin/bash

fname=$1
out=$2

clang-9 -pipe -Wall -O0 -fomit-frame-pointer -march=ivybridge  $fname -o ${out}_O0
clang-9 -pipe -Wall -O1 -fomit-frame-pointer -march=ivybridge  $fname -o ${out}_O1
clang-9 -pipe -Wall -O3 -fomit-frame-pointer -march=ivybridge  $fname -o ${out}_O3