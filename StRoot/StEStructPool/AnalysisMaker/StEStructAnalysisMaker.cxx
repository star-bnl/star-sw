 /***************************************************************************
 *
 * $Id: StEStructAnalysisMaker.cxx,v 1.9 2012/11/16 21:19:05 prindle Exp $
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
    doReactionPlaneAnalysis=false;
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
      if (mtimer) {
          mtimer->stop();
      }

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
     if (mSorting) {
         return kStOK;
     }
     if(ia>=0 && ia < numAnalysis) {
       if(doReactionPlaneAnalysis) {
         pEStructEvent->SetPhiWgt(mWeightFile);
         //pEStructEvent->SetPhiWgt();
         pEStructEvent->ShiftPhi();  // This step modifies all Phi values to be w.r.t. the reaction plane
       }

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

//-----------------------------------------------------
Int_t StEStructAnalysisMaker::Make(StEStructEvent *ev) {


  if (doPrintMemoryInfo)StMemoryInfo::instance()->snapshot();

  if(!ev || !manalysis[0] ){ 
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
   pEStructEvent = ev;

     int ia = getAnalysisIndex();         // 0 if numAnalysis = 1
     if (mSorting) {
         return kStOK;
     }
     if(ia>=0 && ia < numAnalysis) {
       if(doReactionPlaneAnalysis) {
         pEStructEvent->SetPhiWgt(mWeightFile);
         //pEStructEvent->SetPhiWgt();
         pEStructEvent->ShiftPhi();  // This step modifies all Phi values to be w.r.t. the reaction plane
       }

       manalysis[ia]->doEvent(pEStructEvent);
       mEventProcessedCounter++;
       mEventProcessedPerType[ia]++;
       if(mQAHists)mQAHists->fillHistograms(pEStructEvent,mreader);
       mCurrentAnalysis = manalysis[ia];
     }

   if (doPrintMemoryInfo) {
     StMemoryInfo::instance()->snapshot();
     StMemoryInfo::instance()->print();
   }   

   return kStOK;   
}

int StEStructAnalysisMaker::getAnalysisIndex(){

  //if(numAnalysis==1) return 0;   // this was a bug, for jobs with 1 analysis centrality limits will be ignored

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

  tfq->Close();
}

//  quickSort
//
//  This public-domain C implementation by Darel Rex Finley.
//
//  * Returns YES if sort was successful, or NO if the nested
//    pivots went too deep, in which case your array will have
//    been re-ordered, but probably not sorted correctly.
//
//  * This function assumes it is called with valid parameters.
//
//  * Example calls:
//    quickSort(&myArray[0],5); // sorts elements 0, 1, 2, 3, and 4
//    quickSort(&myArray[3],5); // sorts elements 3, 4, 5, 6, and 7

bool StEStructAnalysisMaker::quickSort(int *arr, int elements) {

    #define  MAX_LEVELS  1000

    int  piv, beg[MAX_LEVELS], end[MAX_LEVELS], i=0, L, R, pM ;
    mIndex = new int[elements];
    for (int i=0;i<elements;i++) {
        mIndex[i] = i;
    }

    beg[0]=0; end[0]=elements;
    while (i>=0) {
        L=beg[i]; R=end[i]-1;
        if (L<R) {
            piv=arr[L];
            pM =mIndex[L];
            if (i==MAX_LEVELS-1) return false;
            while (L<R) {
                while (arr[R]>=piv && L<R) R--;
                if (L<R) {
                    mIndex[L]=mIndex[R];
                    arr[L++]=arr[R];
                }
                while (arr[L]<=piv && L<R) L++;
                if (L<R) {
                    mIndex[R]=mIndex[L];
                    arr[R--]=arr[L];
                }
            }
            arr[L]=piv;
            mIndex[L]=pM;
            beg[i+1]=L+1;
            end[i+1]=end[i];
            end[i++]=L;
        } else {
            i--;
        }
    }
    return true;
}
bool StEStructAnalysisMaker::quickSort(double *arr, int elements) {

    #define  MAX_LEVELS  1000

    double  piv;
    int  beg[MAX_LEVELS], end[MAX_LEVELS], i=0, L, R, pM ;
    mIndex = new int[elements];
    for (int i=0;i<elements;i++) {
        mIndex[i] = i;
    }

    beg[0]=0; end[0]=elements;
    while (i>=0) {
        L=beg[i]; R=end[i]-1;
        if (L<R) {
            piv=arr[L];
            pM =mIndex[L];
            if (i==MAX_LEVELS-1) return false;
            while (L<R) {
                while (arr[R]>=piv && L<R) R--;
                if (L<R) {
                    mIndex[L]=mIndex[R];
                    arr[L++]=arr[R];
                }
                while (arr[L]<=piv && L<R) L++;
                if (L<R) {
                    mIndex[R]=mIndex[L];
                    arr[R--]=arr[L];
                }
            }
            arr[L]=piv;
            mIndex[L]=pM;
            beg[i+1]=L+1;
            end[i+1]=end[i];
            end[i++]=L;
        } else {
            i--;
        }
    }
    return true;
}
int StEStructAnalysisMaker::GetSortedIndex(int i) {
    return mIndex[i];
}

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
}

void StEStructAnalysisMaker::SetReactionPlaneAnalysis(char* weightFile) {
  doReactionPlaneAnalysis = true;
  mWeightFile = weightFile;
  cout << "  Setting doReactionPlaneAnalysis = " << doReactionPlaneAnalysis << " with a weight file " << mWeightFile << endl;
}

//-----------------------------------------
//-----------------------------------------------------------
//-------------------------------------------------------------------------
//------------------------------------------------------------------------
//-------------------------------------------------------------------------

/***********************************************************************
 *
 * $Log: StEStructAnalysisMaker.cxx,v $
 * Revision 1.9  2012/11/16 21:19:05  prindle
 * Moved EventCuts, TrackCuts to EventReader. Affects most readers.
 * Added support to write and read EStructEvents.
 * Cuts: 3D histo support, switch to control filling of histogram for reading EStructEvents
 * EventCuts: A few new cuts
 * MuDstReader: Add 2D to some histograms, treat ToFCut, PrimaryCuts, VertexRadius histograms like other cut histograms.
 * QAHists: Add refMult
 * TrackCuts: Add some hijing cuts.
 *
 * Revision 1.8  2010/03/02 21:43:37  prindle
 *   Use outerHelix() for global tracks
 *   Add sensible triggerId histograms
 *   Starting to add support to sort events (available for Hijing)
 *
 * Revision 1.7  2007/01/26 17:09:25  msd
 * Minor bug fix in AnalysisMaker, cleaned up EmptyAnalysis
 *
 * Revision 1.6  2006/05/01 17:49:57  msd
 * Closed QA tfile
 *
 * Revision 1.5  2006/04/26 18:48:57  dkettler
 *
 * Added reaction plane determination for the analysis
 *
 * Revision 1.4  2006/04/06 00:53:52  prindle
 *   Tried to rationalize the way centrality is defined.
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





