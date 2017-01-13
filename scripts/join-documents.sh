#!/bin/bash

find . -maxdepth 2 -path "./documents/*.conllu" | sort -V | xargs cat
