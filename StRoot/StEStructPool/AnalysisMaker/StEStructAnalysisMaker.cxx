 /***************************************************************************
 *
 * $Id: StEStructAnalysisMaker.cxx,v 1.1 2003/10/15 18:20:31 porter Exp $
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
    mreader[0]=0;
    manalysis[0]=0;
    mEventProcessedCounter=0;
    numReaders=numAnalysis=0;
    doPrintMemoryInfo=false;
    mEventProcessedPerType=0;
}

//--------------------------------------------------------------
StEStructAnalysisMaker::~StEStructAnalysisMaker() { 

  for(int i=0;i<numReaders;i++) delete mreader[i];
  for(int i=0;i<numAnalysis;i++) delete manalysis[i];
  if(mtimer) delete mtimer;
}


//--------------------------------------------------------------
Int_t StEStructAnalysisMaker::Init()
{
  gMessMgr->Info() << "StEStructAnalysisMaker: Init()" << endm;    
  startTimer();
  if(!numReaders || !numAnalysis || (numReaders != numAnalysis)){
    gMessMgr->Info()<<" StEStructAnalysisMaker::Init() numReaders!=numAnalysis"<<endm;
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
 
 return StMaker::Finish();
}

//-----------------------------------------------------
Int_t
StEStructAnalysisMaker::Make(){


  if (doPrintMemoryInfo)StMemoryInfo::instance()->snapshot();

  if(!mreader[0] || !manalysis[0] ){ 
     gMessMgr->Error()<<" No Reader in StEStructAnalysisMaker::Make() "<<endm;
     return kStFatal;
   }
  
   mEventLoopCounter++;
   //
   // model: each reader has a different cut file for event selection.
   //        if event doesn't pass a reader's cut, there is a check 
   //        for EOF and, if not, it goes to the next reader. If the event
   //        passes, then the analysis is run and the loop is broken
   //        Thus, an event can only be sent to 1 of the analysis modules.
   //
   //
   for(int i=0;i<numReaders;i++){
     if(!(pEStructEvent=mreader[i]->next())){
       if(mreader[i]->done()) return kStEOF;
     } else {
       manalysis[i]->doEvent(pEStructEvent);
       mEventProcessedCounter++;
       mEventProcessedPerType[i]++;
       break;
     }
   }

   if (doPrintMemoryInfo) {
     StMemoryInfo::instance()->snapshot();
     StMemoryInfo::instance()->print();
   }   

   return kStOK;   
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
 * Revision 1.1  2003/10/15 18:20:31  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/





