#!/bin/bash
cp pt-ud-train.conllu pt-ud-train-danbackup.conllu
cp pt-ud-dev.conllu pt-ud-dev-danbackup.conllu
cp pt-ud-test.conllu pt-ud-test-danbackup.conllu
perl ../tools/mergept.pl ../UD_Portuguese-Bosque/pt_bosque-ud-train.conllu pt-ud-train-danbackup.conllu > pt-ud-train.conllu
perl ../tools/mergept.pl ../UD_Portuguese-Bosque/pt_bosque-ud-dev.conllu pt-ud-dev-danbackup.conllu > pt-ud-dev.conllu
perl ../tools/mergept.pl bosque_test.conllu pt-ud-test-danbackup.conllu > pt-ud-test.conllu
