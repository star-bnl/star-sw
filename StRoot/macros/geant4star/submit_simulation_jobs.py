#!/usr/bin/env python3

import os
import argparse
import subprocess
import ROOT
from ROOT import gROOT as root
import re


PWD = os.getcwd()

def main():
    parser = argparse.ArgumentParser(description ='Submit simulation jobs', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    # WORKINGDIR
    parser.add_argument( '-w', '--workdir', dest='WORKINGDIR', default=PWD, help="Submission directory" )
    # LOGDIR
    parser.add_argument( '-l', '--logdir', dest='LOGDIR', default='/star/simu/simu/g4star/.simulations/log/', help="Log directory" )
    # OUTDIR
    parser.add_argument( '-o', '--outdir', dest='OUTDIR', default='/star/simu/simu/g4star/.simulations/out/', help="Output directory" )    
    # JOBDIR
    parser.add_argument( '-j', '--jobdir', dest='JOBDIR', default='/star/simu/simu/g4star/.simulations/job/', help="Job directory" )    
    # BASELIB
    parser.add_argument( '-b', '--baselib', dest='BASELIB', default='SL26x', help="Specify the library" )
    # OVERLAY
    parser.add_argument(      '--overlay', dest='OVERLAY', default='config/v0.3.0-rhel7-root6.24.06', help="Spack overlay on the environment" )
    # GROUPDIR
    parser.add_argument( '-g', '--group', dest='GROUPDIR', default='/star/u/jeromel/work/STAR/group', help="Select the STAR group directory" )
    # NEVENTS
    parser.add_argument( '--nevents', dest='NEVENTS', default=1, help="Set number of events per job" )
    # NJOBS
    parser.add_argument( '--njobs', dest='NJOBS', default=1, help="Set the number of jobs to run" )
    # Job tag
    # parser.add_argument( '--tag', dest="TAG", default=None, help="Sets production tag" )
    # Offset
    parser.add_argument( '--offset', dest="OFFSET", default=0, help="Sets production tag" )    
    
    parser.add_argument( '--series', dest="series", default=None, help="Runs all productions in a given series... or matches anything containing" )

    parser.add_argument( '--list', dest="list", default=False, action="store_true", help="Lists available series and exists" )

    parser.add_argument( '--verbose', dest="verbose", default=False, action="store_true", help="Full output from SUMS submission" )

    parser.add_argument( '--no-submit', dest="submit", default=True, action="store_false", help="Print command to submit and exit." )

    parser.add_argument( '--embedding', dest="embedding", default=False, action="store_true", help="Submit embedding job." )        

    args = parser.parse_args()


    # Convert arguments to dictionary
    argd = vars(args)

    if ( argd.pop('list', False) ):
        root.LoadMacro("StRoot/macros/geant4star/StarSimOpts.h")
        ROOT.StarSimOpts(-1)
        return

    # Pop the series selector
    series = argd.pop('series', None)

    # Store result from every submitted job
    resultmap = {}    

    # If we are running embedding, submit the specified embedding *test* workflow and exit.  No
    # sanity checks here, so proceed at own risk.
    if args.embedding:
        entities=[]
        for arg,val in argd.items():
            entities.append(f"{arg}={val}")        
        k=series
        entities.append(f"TAG={k}")
        entities.append(f"JOBNAME={k.replace(':','_')}-")
        command = f"star-submit-template-beta -entities {','.join(entities)} -template StRoot/macros/geant4star/submit_embedding_jobs.xml"
        if args.submit:
            result_shell = subprocess.run(command, shell=True, capture_output=True, text=True)
            submission=result_shell.stdout.split('\n')
            resultmap[k]=submission
            
            if args.verbose:
                print(result_shell)
            else:                
                print( f'[{k}] {submission[-2]} {submission[-3]}' )



        else:
            print(command)        

        return
    

    # Create the list of jobs
    root.Macro("StRoot/macros/geant4star/StarSimOpts.h")

    jobs = ROOT.jobmap
    
    for k,_ in jobs:
        if series==None or series in k:
            entities=[]
            for arg,val in argd.items():
                entities.append(f"{arg}={val}")
            entities.append(f"TAG={k}")
            entities.append(f"JOBNAME={k.replace(':','_')}-")

            command = f"star-submit-template-beta -entities {','.join(entities)} -template StRoot/macros/geant4star/submit_simulation_jobs.xml"

            if args.submit:
                result_shell = subprocess.run(command, shell=True, capture_output=True, text=True)
                submission=result_shell.stdout.split('\n')
                resultmap[k]=submission

                if args.verbose:
                    print(result_shell)
                else:                
                    print( f'[{k}] {submission[-2]} {submission[-3]}' )



            else:
                print(command)

    

    
    
    



if __name__ == '__main__':
    main()
