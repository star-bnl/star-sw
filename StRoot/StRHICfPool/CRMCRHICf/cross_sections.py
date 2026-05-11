#!/usr/bin/python
import os
import sys
import math
from subprocess import Popen, PIPE

sqrts_min=10 # GeV
sqrts_max=1e6 # GeV
lg_sqrts_min=math.log10(sqrts_min)
lg_sqrts_max=math.log10(sqrts_max)
sqrts_n=15 # log-bins

foutput_inel = open('xs_inel.tab', 'write')
foutput_inel.write('#sibyll\n')
foutput_inel.write('# E_cms (GeV)  XS_ine (mb)   dummy\n')


for sqrts_i in range(sqrts_n):

    lg_sqrts = lg_sqrts_min + (lg_sqrts_max-lg_sqrts_min) / (sqrts_n-1) * sqrts_i
    sqrts = math.pow(10, lg_sqrts)

    cmd = './crmc -x -m 5 -p 100 -P 100 -s ' + str(sqrts)
    p = Popen(['./crmc', '-x', '-m0', '-I1', '-i1', '-S' + str(sqrts)], stdout=PIPE, stderr=PIPE)
    output, err = p.communicate()
    rc = p.returncode

    if (rc!=0 and err!=''):
        print ("ERROR")
        print str(output)
        print str(err)
        print (str(rc))
        sys.exit(1)

    total = 0.
    ela = 0.
    inel = 0.
    for line in output.splitlines():
        if 'Total Cross Section (mb):' in line:
            ls = line.split(':')
            if (len(ls)!=2):
                print ("ERROR 2")
                sys.exit(1)
            total = float(ls[1])

        if 'Elastic Cross Section (mb):' in line:
            ls = line.split(':')
            if (len(ls)!=2):
                print ("ERROR 3")
                sys.exit(1)
            ela = float(ls[1])
            
            
        if 'Inel. Cross Section (mb) :' in line:
            ls = line.split(':')
            if (len(ls)!=2):
                print ("ERROR 4")
                sys.exit(1)
            inel = float(ls[1])

    if (total==0 and inel==0 and ela==0):
        print ("ERROR 5")
        print str(output)
        print str(err)
        print (str(rc))
        sys.exit(1)

    print str(sqrts) + " " + str(total) + " " + str(ela) + " " + str(inel)


    # printout tanguy's format...
    foutput_inel.write(str(sqrts) + ' ' + str(inel) + ' 0.0' + '\n')
    

foutput_inel.close()
