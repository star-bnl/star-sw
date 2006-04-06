 /***************************************************************************
 *
 * $Id: StEStructAnalysisMaker.cxx,v 1.4 2006/04/06 00:53:52 prindle Exp $
 *
 *************************************************************************
 *
 * Description:  This is a maker for general EStruct Analysis
 *               Requires at least 1 reader and 1 analysis 
 * 
 *  2 RULES::  Reader gives up event (creates new event)
 *             Analysis takes ownership of event (must delete!)
 *
 **************************************************************************/

#include <stdlib.h>
#include <math.h>
#include "StEStructAnalysisMaker.h" 

#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StEStructPool/EventMaker/StEStructCentrality.h"

//for memory mangement
#include "StMemoryInfo.hh"
//#include "StMuTimer.h"
//
#include "StChain.h"
#include "TFile.h"
#include "StMessMgr.h"


ClassImp(StEStructAnalysisMaker)

//--------------------------------------------------------------
StEStructAnalysisMaker::StEStructAnalysisMaker(const Char_t *name) : StMaker(name) {
    mtimer=NULL;
    mEventLoopCounter=0;
    mreader=0;
    manalysis[0]=0;
    mEventProcessedCounter=0;
    numAnalysis=0;
    doPrintMemoryInfo=false;
    mEventProcessedPerType=0;
    mQAHists = NULL;
}

//--------------------------------------------------------------
StEStructAnalysisMaker::~StEStructAnalysisMaker() { 

  for(int i=0;i<numAnalysis;i++) delete manalysis[i];
  if(mtimer)   delete mtimer;

}

//--------------------------------------------------------------
Int_t StEStructAnalysisMaker::Init()
{
  //  gMessMgr->Info() << "StEStructAnalysisMaker: Init()" << endm;    
  startTimer();

  if(!mreader || !numAnalysis){
    gMessMgr->Error()<<" StEStructAnalysisMaker::Init() no reader ";
    gMessMgr->Error()<<" Or no analysis object: Will Abort "<<endm;
    assert(0);
  }

  mEventProcessedPerType=new int[numAnalysis];
  for(int i=0;i<numAnalysis;i++) mEventProcessedPerType[i]=0;

  return kStOK;
}

//--------------------------------------------------------------
void
StEStructAnalysisMaker::Clear(Option_t *opt){ StMaker::Clear(); }

//--------------------------------------------------------------
Int_t
StEStructAnalysisMaker::Finish()
{
      if (doPrintMemoryInfo) {
         StMemoryInfo::instance()->snapshot();
         StMemoryInfo::instance()->print();
      }     
      for(int i=0;i<numAnalysis;i++) manalysis[i]->finish();
      mtimer->stop();

      // Do Not Delete This  -->  if(mQAHists) delete mQAHists;

      return kStOk; // remove this ....  StMaker::Finish();
}


//-----------------------------------------------------
Int_t
StEStructAnalysisMaker::Make(){


  if (doPrintMemoryInfo)StMemoryInfo::instance()->snapshot();

  if(!mreader || !manalysis[0] ){ 
     gMessMgr->Error()<<" No Reader in StEStructAnalysisMaker::Make() "<<endm;
     return kStFatal;
   }
  
   mEventLoopCounter++;
   //

   /////    <<<<    below is now gone, kaput, obsolete   >>>>>
   // model: each reader has a different cut file for event selection.
   //        if event doesn't pass a reader's cut, there is a check 
   //        for EOF and, if not, it goes to the next reader. If the event
   //        passes, then the analysis is run and the loop is broken
   //        Thus, an event can only be sent to 1 of the analysis modules.
   //

   mCurrentAnalysis = 0;

   if(!(pEStructEvent=mreader->next())){

     if(mreader->done()) return kStEOF;

   } else {

     int ia = getAnalysisIndex();         // 0 if numAnalysis = 1
     if(ia>=0 && ia < numAnalysis) {

       manalysis[ia]->doEvent(pEStructEvent);
       mEventProcessedCounter++;
       mEventProcessedPerType[ia]++;
       if(mQAHists)mQAHists->fillHistograms(pEStructEvent,mreader);
       mCurrentAnalysis = manalysis[ia];

     }
   }   


   if (doPrintMemoryInfo) {
     StMemoryInfo::instance()->snapshot();
     StMemoryInfo::instance()->print();
   }   

   return kStOK;   
}

int StEStructAnalysisMaker::getAnalysisIndex(){

  if(numAnalysis==1) return 0;

//   currently all we index on is centrality, 
//   but this method could be a switch statement
//   over some flag known to the maker

  StEStructCentrality* cent=StEStructCentrality::Instance();
  return cent->centrality( pEStructEvent->Centrality() );
}

//-----------------------------------------------------------------
void StEStructAnalysisMaker::writeQAHists(const char* fileName){

  TFile * tfq=new TFile(fileName,"RECREATE");
  if(!tfq){
    gMessMgr->Warning()<<" QAfile = "<<fileName<<" NOT OPENED!!"<<endm;
    return;
  }

  if(mQAHists)mQAHists->writeHistograms(tfq);
  for(int i=0;i<numAnalysis;i++)manalysis[i]->writeQAHists(tfq);

};


//-----------------------------------------------------------------
void StEStructAnalysisMaker::writeDiagnostics(int opt){

  for(int i=0;i<numAnalysis;i++)manalysis[i]->writeDiagnostics();
  
}

void StEStructAnalysisMaker::startTimer(){ 
  if(!mtimer) mtimer=new StMuTimer;
  mtimer->start();
}

void StEStructAnalysisMaker::stopTimer(){ if(mtimer)mtimer->stop(); }

void StEStructAnalysisMaker::compiledLoop(){
  /* no-op here but one could put the event loop of a macro here 
     for some speed increase
  */
};

//-----------------------------------------
//-----------------------------------------------------------
//-------------------------------------------------------------------------
//------------------------------------------------------------------------
//-------------------------------------------------------------------------

/***********************************************************************
 *
 * $Log: StEStructAnalysisMaker.cxx,v $
 * Revision 1.4  2006/04/06 00:53:52  prindle
 * Tried to rationalize the way centrality is defined.
 *   Now the reader gives a float to StEStructEvent and this float is
 * what is being used to define centrality. When we need a centrality
 * bin index we pass this number into the centrality singleton object.
 *
 * Revision 1.3  2006/04/04 22:05:03  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.2  2005/09/07 20:18:35  prindle
 *   AnalysisMaker: Keep track of currentAnalysis (for use in doEStruct macro)
 *   EventCuts.h:   Added trigger cuts including cucu and year 4.
 *   MuDstReader:   Added dE/dx histograms. Re-arranged code to count tracks
 *                    before making centrality cut.
 *   TrackCuts:     Random changes. Moved some variables from private to public.o
 *
 * Revision 1.1  2003/10/15 18:20:31  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/





