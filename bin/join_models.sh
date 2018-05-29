#!/bin/bash

name=$1

for file in all/*
do
        cut -d " " -f1 "$file" > "combined_"${name}
        break
done

for file in all/*
do
        join "$file" "combined_"${name} > tmp
        mv tmp "combined_"${name}
done

