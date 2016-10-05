
Issues found:

1. Gender none:

#+BEGIN_EXAMPLE
Gender=None -> Gender=Com
#+END_EXAMPLE

2. 133 lines in CP and 5 lines in CF with wrong size of columns!

#+BEGIN_SRC bash
$ awk -F "\t" '{print NF}' bosque_CP.udep.conll | sort | uniq -c
10989 0
5905 1
128169 10
 133 2
$ awk -F "\t" '{print NF}' bosque_CF.udep.conll | sort | uniq -c
9058 0
4772 1
80135 10
   5 2
#+END_SRC

