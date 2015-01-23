#!/usr/bin/python

import time
import os

while True:
    print "Time for a run:",time.ctime()
    os.system("./job.emconline_trg")
    #Enter time in seconds between updates, for now it's 10 minutes
    time.sleep(600)
