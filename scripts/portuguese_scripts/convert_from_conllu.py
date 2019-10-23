import sys
import pdb

filepath = sys.argv[1]
f = open(filepath)

for line in f:
    line = line.rstrip('\n')
    if line == '':
        print line
    elif line.startswith('#'):
        continue
    else:
        fields = line.split('\t')
        if '-' in fields[0]:
            assert fields[2:] == ['_'] * (len(fields) - 2), pdb.set_trace()
            continue
        print line

f.close()

