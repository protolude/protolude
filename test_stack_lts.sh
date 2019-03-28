#!/usr/bin/env bash
set +e

echo -e "\e[92mLTS 7.8"
STACK_YAML=stack-7.8.yaml  stack build  --no-terminal

echo -e "\e[92mLTS 7.10"
STACK_YAML=stack-7.10.yaml stack build  --no-terminal

echo -e "\e[92mLTS 8.0"
STACK_YAML=stack-8.0.yaml  stack build  --no-terminal

echo -e "\e[92mLTS 9.0"
STACK_YAML=stack-9.0.yaml  stack build  --no-terminal

echo -e "\e[92mLTS 10.0"
STACK_YAML=stack-10.0.yaml  stack build  --no-terminal

echo -e "\e[92mLTS 11.0"
STACK_YAML=stack-11.0.yaml  stack build  --no-terminal

echo -e "\e[92mLTS 12.0"
STACK_YAML=stack-12.0.yaml  stack build  --no-terminal

echo -e "\e[92mLTS 13.0"
STACK_YAML=stack-13.0.yaml  stack build  --no-terminal
