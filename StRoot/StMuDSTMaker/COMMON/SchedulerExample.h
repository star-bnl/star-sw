/***************************************************************************
 *
 * $Id: SchedulerExample.h,v 1.2 2004/05/02 04:10:13 perev Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef SchedulerExample_h
#define SchedulerExample_h

#include "StMaker.h"

#include <string>
using std::string;

class StMuDstMaker;
class StMuDst;
class TH1D;
class TH2D;
class TH3D;


/** 
    @class SchedulerExample
    @author laue
    A small example maker that can run in the chain using MuDst's and the Scheduler.
    Please see the macros/SchedulerExample.C and macros/SchedulerExample.xml scripts.
    To submit jobs to the queue, run 
    " host> star-submit StRoot/StMuDSTMaker/COMMON/macros/SchedulerExample.xml "
    from your working directory (above StRoot). 
    Make sure that the database querey returns valid entries and that the output directory exists.
 */
class SchedulerExample : public StMaker {
 public:
    SchedulerExample(const char* outputFile); ///< Constructor, takes output filename as an agument
    virtual ~SchedulerExample();
    /**
       The Init() method is called once before the event loop. Place initialisation code here 
       (e.g., get the pointers to other Makers, such as the StMuDstMaker which provides the events for the analysis. 
    */
    Int_t Init(); 
    /**
       The Make() method is called once per event. Place analysis code here.
    */
    Int_t  Make(); 
    /**
       The Finish method is called after the event loop has finished. Place clean-up code such as close of files, normalisation of hoistograms, etc here 
    */
    Int_t Finish(); 
    
 protected:
    string mOutputFile; 
    StMuDstMaker* mMuDstMaker;  ///< Pointer to the StMuDstMaker which provides the events to analyse.
    StMuDst* mMuDst; ///< Pointer to the StMuDst class, which holds all the events information. Will be updated in the event loop.

    TH1D* mPt;  
    TH1D* mEta;
    TH1D* mRefMultPos;
    TH1D* mRefMultNeg;
    TH1D* mRefMult;
    TH3D* mVertex;

    ClassDef(SchedulerExample,0)
};

#endif

/***************************************************************************
 *
 * $Log: SchedulerExample.h,v $
 * Revision 1.2  2004/05/02 04:10:13  perev
 * private => protected
 *
 * Revision 1.1  2003/01/23 21:59:50  laue
 * Modification to compile on Solaris.
 *
 *
 **************************************************************************/
