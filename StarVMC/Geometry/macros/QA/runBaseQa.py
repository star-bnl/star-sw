####################################################################################################
#
# To run base QA on a set of files...
#
# You will want to modify the main() function defined near the end of the file.  The function
# creates a list of QaSet objects which hold information about the sample you will be processing.
# 
# 1) set the mudir to the directory where the micro DSTs reside, and 
# 2) set the imgdir to point to the directory where the output is to go
#
# Append as many QaSet objects to the list as you want to process.  Then execute the script
#
# python runBaseQa.py
#
import ROOT
import subprocess
import os

optipng = "/afs/rhic.bnl.gov/star/users/genevb/str/CODES/MISC/optipng"
doopt = False

class QaSet:
    def __init__(self):
        self.name      = None  # Name of the QA set
        self.title     = None  # Column heading title
        self.comments  = None  # Documentation string
        self.images    = []    # List of images
        self.mudir     = None  # Input directory for MuDst
        self.imgdir    = None  # Directory for images
        #self.nevents   = 99999 # Number of events to process
        self.nevents   = 5000000  # Number of events to process
        self.zmn       = -200.0
        self.zmx       = +200.0
        self.etamn     = -10.0
        self.etamx     = +10.0
        self.ptmn      = 0.0 # Min pT

qasets = []

def runBaseQa( qaset ):
    """
    Generate BaseQA plots using root4star for the specified input directory
    """

    indir  = qaset.mudir
    outdir = qaset.imgdir
    nev    = qaset.nevents

    isBatch = ROOT.gROOT.IsBatch()                 # set batch mode
    ROOT.gROOT.SetBatch( True )

    cmd = [
        'root4star',
        '-q',
        '-b',
        'mkBaseQaPlots.C("%s","%s",%i,%f,%f,%f,%f,%f)'%(indir,outdir,nev,qaset.zmn,qaset.zmx,qaset.etamn,qaset.etamx,qaset.ptmn)
        ]

    print cmd

    #subprocess.call( cmd )
    subprocess.Popen( cmd )

    # capture list of image files and process with optipng
    if doopt:
        qaset.images = os.listdir(outdir)
        for image in qaset.images:
            cmd = [
                optipng,
                '-o5',
                '%s/%s'%(qaset.imgdir,image)
                ]

            print "[ Optimize %s/%s ]"%(qaset.imgdir,image)
            subprocess.call( cmd )

    ROOT.gROOT.SetBatch( isBatch )                 # restore old mode

def buildQaPage( sets=[], www=None ):
    pass

def setup( mudir="/star/data99/reco/AuAu15/hijing_382/b0_20/minbias/y2017/gheisha_on/DEV_Dec_22_chain2/", imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Fwd_BaseQA/img/"  ):
    global qasets
    qa = QaSet()
    qa.name    = "runExample"
    qa.title   = "Example baseQA code"
    
    # mudir should point to a set of MuDSt files created from simulation
    qa.mudir   = mudir

    # imgdir should point to the output directory for the images.  It is good to place them somewhere in your web space.
    # NOTE: You need to point this to a valid directory
    qa.imgdir  = imgdir

    qasets.append(qa)
    
    
if __name__ == '__main__':

    # This will setup an example run.  You can setup multiple
    # runs, and they will be processed concurrently on your
    # node.  (Just be nice about it.)


#  setup( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/simpleFast/", imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/simpleFast/" )
 #  setup( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/ref6a/", imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/ftsref6a/" )
 #  setup( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/sitrver0/", imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/sitrver0/" )    
    setup( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/angle15/",  imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/angle15/" )
    setup( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/angle30/",  imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/angle30/" )
    setup( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/angle60/",  imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/angle60/" )
 #  setup( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/angle80/",  imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/angle80/" )        
 #  setup( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/sitrver1/", imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/sitrver1/" )
 #  setup( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/sitrver1_angle80/", imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/sitrver1_angle80/" )

#   setup( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/sitrver2/", imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/sitrver2/" )
#   setup( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/sitrver2_angle80/", imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/sitrver2_angle80/" )
    

#   setup ( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/sitrver1_fastsimu2/", imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/sitrver1_fastsimu2/" )
#   setup ( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/sitrver1_fastsimu2_angle80/", imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/sitrver1_fastsimu2_angle80/" )
#   setup ( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/sitrver2_fastsimu2/", imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/sitrver2_fastsimu2/" )
#   setup ( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/sitrver2_fastsimu2_angle80/", imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/sitrver2_fastsimu2_angle80/" )

#    setup ( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/sitrver2_fastsimu2_VtxErr/", imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/sitrver2_fastsimu2_VtxErr/" )
#    setup ( mudir="/star/u/xgn1992/work/FTS/Simulation/work_180312/06-22-2018-test/1kTracks/printGlobal/BatchJobs/OUT_MERGE/sitrver2_fastsimu2_VtxErr_angle80/", imgdir="/afs/rhic.bnl.gov/star/users/jwebb/WWW/2018/Forward2/sitrver2_fastsimu2_VtxErr_angle80/" )    


    # Generate base QA images
    for qa in qasets:
        print qa.mudir
        runBaseQa( qa )
    
