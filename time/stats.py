#!/usr/bin/python3

import sys


import xml.etree.ElementTree

for f in sys.argv[1:]:
    e = xml.etree.ElementTree.parse(f).getroot()
    for x in e.findall('TIMEX3'):
        print("{}\t{}\t{}\t{}".format(f,x.attrib['type'],x.attrib['value'],x.text))
    
    
    
