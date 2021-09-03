#!/usr/bin/env python
import os
import subprocess, shlex


def run_test_macro( name, cmd_ = 'root4star' ):

    print("[TEST: running root4star on %s]"%name)

    try:
        src = 'StRoot/StarGenerator/macros/starsim.%s.C'%name
        os.symlink( src, './starsim.C' )
    except:
        print("[FAIL: Unable to create symbolic link]")
        return False
    print("[PASS: Created symbolic link to %s]"%src)

    cmd_ = cmd_ + " -q -b starsim.C"
    cmd  = shlex.split(cmd_)

    try:
        result=subprocess.check_output(cmd,stderr=subprocess.STDOUT)
    except:
        print("[FAIL: could not exec root4star -q -b starsim.C]")
        return False
    print("[PASS: executed root4star -q -b starsim.C]")

    try:
        os.remove("starsim.C")
    except:
        pass


run_test_macro("pythia6")
run_test_macro("pythia8")
run_test_macro("hijing")
