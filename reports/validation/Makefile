
SRC=../../documents
UDROOT=~/work/ud-tools

.PHONY: all clean

%.report : $(SRC)/%.conllu
	cat $< | python3 $(UDROOT)/validate.py --lang pt --max-err=0 2> $@ || true

all : $(patsubst %.conllu, %.report, $(notdir $(wildcard $(SRC)/*.conllu)))

clean:
	rm *.report
