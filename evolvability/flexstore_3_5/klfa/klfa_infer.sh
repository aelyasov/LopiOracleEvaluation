#!/bin/bash

# find . -name "*.csv" -type f | (while read line; do cat $line; echo "|"; done;) > output.txt
klfaTransformationRulesGenerator.sh output.txt
klfaCsvAnalysis.sh -kBehaviorK 2 applicationLevel training transformersConfig.txt preprocessingRules.txt output.txt 
