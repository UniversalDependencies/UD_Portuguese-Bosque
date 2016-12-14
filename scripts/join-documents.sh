#!/bin/bash

find . -path "./documents/*.conllu" | sort -V | xargs cat
