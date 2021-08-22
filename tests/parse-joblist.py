#!/usr/bin/env python
#
# Use this script to convert starreco files containing test job description to
# json format. The original files (sampleList*.txt) can be found in
# /star/u/starreco/devtest on RACF/SDCC machines.
#
# Usage:
#
# cd star-sw
# parse-joblist.py /star/u/starreco/devtest/sampleList_nonopt.reduced.txt > test/joblist.json

import argparse
import json
import os
import re
from collections import namedtuple

JobInfo = namedtuple('JobInfo', ['jobid', 'descr', 'chopts', 'inpfile', 'inppath', 'outpath', 'nevents'])


def parse_joblist(joblisting):
    re_block = re.compile(
        r"^\s*##(.*)"
         "\s*"
         "\njob(\d+)\s*{\s*"
         "\ninputfile\s*=(.+)"
         "\nout_dir\s*=(.+)"
         "\nchain\s*=(.+)"
         "\nnevents\s*=(.+)"
         "\n\s*}\s*$", re.MULTILINE)

    joblist = []
    for match in re_block.finditer(joblisting):
        matches = [match.strip() for match in match.groups()]

        descr, jobid, fpath, outpath, chopts, nevents = matches
        inpfile = os.path.basename(fpath)
        inppath = os.path.dirname(fpath)

        jobinfo = JobInfo(jobid, descr, chopts, inpfile, inppath, outpath, nevents)
        joblist.append(jobinfo)

    return joblist


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Parse file with test job info")
    parser.add_argument("joblist_file")
    args = parser.parse_args()

    with open(args.joblist_file, 'r') as jlfile:
        joblist = parse_joblist(jlfile.read())
        jobjson = json.dumps([job._asdict() for job in joblist], indent=4)
        print(jobjson)
