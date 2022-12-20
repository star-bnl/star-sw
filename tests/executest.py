#!/usr/bin/env python
#
# Use this script to read the `test/joblist.json` file with job information for
# STAR tests. It prints to the standard output the description and/or the
# actual test command for the job specified by its id 
#
# Usage:
#
# executest.py -d 45
# pp 200GeV, pythia minbias, year 2009
#
# executest.py -c 45
# root4star -b -q -l 'bfc.C(10, "tpcRS,TpxClu,fss,y2009a,Idst,IAna,l0,tpcI,ftpc,Tree,logger,ITTF,Sti,genvtx,NoSsdIt,NoSvtIt,MakeEvent,bbcSim,btofsim,vpdSim,tags,emcY2,EEfs,evout,IdTruth,geantout,big,fzin,MiniMcMk,clearmem", "/star-test-data/quick/rcf9993_01_1000evts.fzd")'

import argparse
import json
import os
from collections import namedtuple

JobInfo = namedtuple('JobInfo', ['jobid', 'descr', 'chopts', 'inpfile', 'inppath', 'outpath', 'nevents'])

def _json_object_hook(d):
    return namedtuple('JobInfo', d.keys())(*d.values())


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Form or execute test job")
    parser.add_argument("jobid")
    parser.add_argument("-d", "--description", default=False, action="store_true", help="Print job description")
    parser.add_argument("-c", "--command",     default=False, action="store_true", help="Print command")
    parser.add_argument("-a", "--attribute",   default='', choices=JobInfo._fields + ('fullpath', ), help="Print job's attribute")
    args = parser.parse_args()

    joblist_file = os.path.join(os.path.dirname(__file__), "joblist.json")

    with open(joblist_file, 'r') as jlfile:
        joblist = json.load(jlfile, object_hook=_json_object_hook)

    for job in joblist:
        if job.jobid != args.jobid: continue
        if args.description: print("{descr}".format(**job._asdict())) 
        if args.command:     print("root4star -b -q -l \'bfc.C({nevents}, \"{chopts}\", \"{inppath}/{inpfile}\")\'".format(**job._asdict()))
        if args.attribute:
            if args.attribute == 'fullpath':
                print("{}/{}".format(job.inppath, job.inpfile))
            else:
                print(job._asdict()[args.attribute])
