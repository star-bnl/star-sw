/***************************************************************************
 *
 * $Id: StPidAmpMaker.cxx,v 1.8 2000/07/22 22:11:33 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             StPidAmpMaker is a mediator between StEvent and StPidAmpManager
 ***************************************************************************
 *
 * $Log: StPidAmpMaker.cxx,v $
 * Revision 1.8  2000/07/22 22:11:33  aihong
 * move some include files to StEventUtilities & change include path
 *
 * Revision 1.7  2000/07/12 15:38:33  aihong
 * update for real data
 *
 * Revision 1.6  2000/05/01 16:59:49  aihong
 * clean up
 *
 * Revision 1.5  2000/04/12 20:14:29  aihong
 * change to adapt to ROOT 2.24 and bug fixed with help from valery
 *
 * Revision 1.4  2000/04/11 15:45:25  aihong
 * change to adapt dividing trks by channel for faster filling
 *
 * Revision 1.3  2000/04/09 18:50:47  aihong
 * change Make() to read directly from dst tables instead of StEvent
 *
 * Revision 1.2  2000/04/09 16:36:43  aihong
 * change for adapting NHitDcaNet added
 *
 * Revision 1.1.1.1  2000/03/09 17:48:33  aihong
 * Installation of package
 *
 **************************************************************************/


#include "StPidAmpMaker/StPidAmpMaker.h"
#include "StChain.h"

#include "St_DataSetIter.h"
#include "StMessMgr.h"
#include "StEventTypes.h"

#include "tables/St_dst_track_Table.h"
#include "tables/St_dst_dedx_Table.h"
#include "tables/St_dst_vertex_Table.h"

#include "StEventUtilities/StPidAmpConst.hh"


void fillStPidAmpTrks4StandardStEvent(StEvent& event, StPidAmpTrkVector* trks, Int_t dedxMethod);
void fillStPidAmpTrks(St_dst_track* theTrackTable, St_dst_dedx* theDedxTable, St_dst_vertex* theVertexTable, StPidAmpTrkVector* trks, Int_t dedxMethod);
void readDataFromDisk(StPidAmpTrkVector* trks);//read trks from disk.
void writeTrks(St_dst_track* theTrackTable, St_dst_dedx* theDedxTable, St_dst_vertex* theVertexTable, Int_t dedxMethod);//write trks to disk for quik reading.vi readDataFromDisk.

ClassImp(StPidAmpMaker)

StPidAmpMaker::StPidAmpMaker(const Char_t *name) : StMaker(name)
{
    theManager    =new StPidAmpManager(); 

    ampTrks     =new StPidAmpTrkVector();

    mNHits4BG=0;
    theManager->passTrksAddress(ampTrks);
    mReadFromTable=kFALSE;
    mManualSetTrks=kFALSE;//will switch to true if SetTotalTracks() is called
    mDedxMethod=1; //truncated mean

}

StPidAmpMaker::~StPidAmpMaker() { /* noop */ }

Int_t
StPidAmpMaker::Init()
{


  ampTrks->reserve(8000000);

  return StMaker::Init();
}

void
StPidAmpMaker::Clear(Option_t *opt)
{
    StMaker::Clear();
}

Int_t
StPidAmpMaker::Finish()
{

  if (ampTrks->size()>=mTotalTrks4Run) {
  // readDataFromDisk(ampTrks);

    //release unused space back to memory.
    StPidAmpTrkVector tmpVector=*ampTrks;
    ampTrks->swap(tmpVector);

     
    //run...
    if (theManager->netSets()->size()==0) 
    theManager->bookADefaultChannelCollection("BAR"," ");


    theManager->process();
  } else {
  gMessMgr->Info()<<"Statistics is not enough for run. Require "<<mTotalTrks4Run<<" tracks to run this option "<<endm;
  gMessMgr->Info()<<"But got only "<<ampTrks->size()<<" tracks. Aborted. "<<endm;
  }
  
    return kStOK;
}

Int_t
StPidAmpMaker::Make()
{

  if (ampTrks->size()>=mTotalTrks4Run)    return kStEOF;
  

  if (mReadFromTable) {

  St_DataSet *dst_data = GetInputDS("dst");
  if (!dst_data) return 0;

  St_DataSetIter  local(dst_data);

  St_dst_track* globalTable   =( St_dst_track *)local["globtrk"];
  St_dst_dedx*  dst_dedxTable =(St_dst_dedx *)  local["dst_dedx"];
  St_dst_vertex *vertexTable  =(St_dst_vertex* )local["vertex"];

    // OK, we've got the tables. Pass them and process them.
     if (globalTable && dst_dedxTable && vertexTable) {
    fillStPidAmpTrks(globalTable, dst_dedxTable,vertexTable, ampTrks,mDedxMethod);
    //    writeTrks(globalTable, dst_dedxTable,vertexTable,mDedxMethod);
    return kStOK;

     } else return 0;

  } else {//read through standard StEvent

    StEvent* mEvent;
    mEvent = (StEvent *) GetInputDS("StEvent");
    if (! mEvent) return kStOK; // If no event, we're done
    StEvent& ev = *mEvent;
  
    fillStPidAmpTrks4StandardStEvent(ev,ampTrks,mDedxMethod);
    return kStOK;
  }
  
  

}

void
StPidAmpMaker::SetTotalTracks(Int_t totalTracks){
     mTotalTrks4Run=totalTracks;
     mManualSetTrks=kTRUE;
}


void
StPidAmpMaker::SetNHitsFilter2LastCollection(Int_t nhits){
       theManager->setNHits4BGNet(nhits);
}

void 
StPidAmpMaker::SetDedxMethod(TString method){
  TString theMethod=method;
  theMethod.ToUpper();
  if (theMethod.Contains("UNDEFINED"))            mDedxMethod=0;
  if ( (theMethod.Contains("TRUNCATEDMEAN")) && 
        !(theMethod.Contains("ENSEMBLE")) &&
        !(theMethod.Contains("WEIGHTED"))   )  
                                                  mDedxMethod=1;

  if (theMethod.Contains("ENSEMBLE"))             mDedxMethod=2;

  if (theMethod.Contains("LIKEHOOD"))             mDedxMethod=3;
  if (theMethod.Contains("WEIGHTED"))             mDedxMethod=4;
  if (theMethod.Contains("OTHER"))                mDedxMethod=5;
}





void 
StPidAmpMaker::AddDefaultChannelCollection(TString fitOpt, TString drawOpt){
    theManager->bookADefaultChannelCollection(fitOpt,drawOpt);
    gMessMgr->Info()<<"a default ChannelCollection is registered in Manager"<<endm;
    if (!mManualSetTrks) mTotalTrks4Run=1000000;
}





void 
StPidAmpMaker::AddNHitsChannelCollection(Int_t x1, Int_t x2,TString fitOpt, TString drawOpt){
    theManager->bookADefaultChannelCollection(fitOpt,drawOpt);
    gMessMgr->Info()<<"ignored two inputs "<<x1<<" "<<x2<<endm;
    gMessMgr->Info()<<"two inputs is for default option, the default ChannelCollection is registered in the Manager"<<endm;
   if (!mManualSetTrks) mTotalTrks4Run=1000000;

}


void 
StPidAmpMaker::AddNHitsChannelCollection(Int_t x1, Int_t x2, Int_t x3,TString fitOpt, TString drawOpt){
    theManager->bookANHitsChannelCollection(x1,x2,x3,fitOpt,drawOpt);
    gMessMgr->Info()<<"a nhits("<<x1<<" "<<x2<<" "<<x3<<") ChannelCollection is registered in the Manager "<<endm;
    if (!mManualSetTrks) mTotalTrks4Run=4000000;
}

void 
StPidAmpMaker::AddNHitsChannelCollection(Int_t x1, Int_t x2,Int_t x3, Int_t x4,TString fitOpt, TString drawOpt){
    theManager->bookANHitsChannelCollection(x1,x2,x3,x4,fitOpt,drawOpt);
    gMessMgr->Info()<<"a nhits("<<x1<<" "<<x2<<" "<<x3<<" "<<x4<<") ChannelCollection is registered in the Manager "<<endm;
    if (!mManualSetTrks) mTotalTrks4Run=4000000;
}



void 
StPidAmpMaker::AddNHitsChannelCollection(Int_t x1, Int_t x2, Int_t x3, Int_t x4, Int_t x5,TString fitOpt, TString drawOpt){
    theManager->bookANHitsChannelCollection(x1,x2,x3,x4,x5,fitOpt,drawOpt);
    gMessMgr->Info()<<"a nhits("<<x1<<" "<<x2<<" "<<x3<<" "<<x4<<x5<<" "<<") ChannelCollection is registered in the Manager"<<endm;
    if (!mManualSetTrks) mTotalTrks4Run=4000000;
}


void 
StPidAmpMaker::AddNHitsDcaChannelCollection(Int_t x1, Int_t x2,TString fitOpt,Double_t d1, Double_t d2, Double_t d3, TString drawOpt){
    theManager->bookADefaultChannelCollection(fitOpt,drawOpt);
    gMessMgr->Info()<<"ignored two inputs "<<x1<<" "<<x2<<endm;
    gMessMgr->Info()<<"two inputs is for default option, the default ChannelCollection is registered in the Manager"<<endm;
    if (!mManualSetTrks) mTotalTrks4Run=6000000;
}


void 
StPidAmpMaker::AddNHitsDcaChannelCollection(Int_t x1, Int_t x2, Int_t x3,TString fitOpt,Double_t d1, Double_t d2,  Double_t d3, TString drawOpt){
    theManager->bookANHitsDcaChannelCollection(x1,x2,x3,fitOpt,drawOpt,d1,d2,d3);
    gMessMgr->Info()<<"a nhits("<<x1<<" "<<x2<<" "<<x3<<") dca("<<d1<<" "<<d2<<" "<<d3<<")  ChannelCollection is registered in the Manager "<<endm;
    if (!mManualSetTrks) mTotalTrks4Run=6000000;
}

void 
StPidAmpMaker::AddNHitsDcaChannelCollection(Int_t x1, Int_t x2,Int_t x3, Int_t x4,TString fitOpt, Double_t d1,  Double_t d2, Double_t d3, TString drawOpt){

    theManager->bookANHitsDcaChannelCollection(x1,x2,x3,x4,fitOpt,drawOpt,d1,d2,d3);
    gMessMgr->Info()<<"a nhits("<<x1<<" "<<x2<<" "<<x3<<" "<<x4<<") dca("<<d1<<" "<<d2<<" "<<d3<<") ChannelCollection is registered in the Manager "<<endm;
    if (!mManualSetTrks) mTotalTrks4Run=6000000;
}



void 
StPidAmpMaker::AddNHitsDcaChannelCollection(Int_t x1, Int_t x2, Int_t x3, Int_t x4, Int_t x5,TString fitOpt, Double_t d1,  Double_t d2, Double_t  d3, TString drawOpt){
    theManager->bookANHitsDcaChannelCollection(x1,x2,x3,x4,x5,fitOpt,drawOpt,d1,d2,d3);
    gMessMgr->Info()<<"a nhits("<<x1<<" "<<x2<<" "<<x3<<" "<<x4<<x5<<" "<<") dca("<<d1<<" "<<d2<<" "<<d3<<") ChannelCollection is registered in the Manager "<<endm;
    if (!mManualSetTrks) mTotalTrks4Run=6000000;

}



void 
StPidAmpMaker::AddPtChannelCollection(Double_t x1, Double_t x2,TString fitOpt, TString drawOpt){
    theManager->bookADefaultChannelCollection(fitOpt,drawOpt);
    gMessMgr->Info()<<"ignored two inputs "<<x1<<" "<<x2<<endm;
    gMessMgr->Info()<<"two inputs is for default option, the default ChannelCollection is registered in the Manager"<<endm;
}


void 
StPidAmpMaker::AddPtChannelCollection(Double_t x1, Double_t x2, Double_t x3,TString fitOpt, TString drawOpt){
    theManager->bookAPtChannelCollection(x1,x2,x3,fitOpt,drawOpt);
    gMessMgr->Info()<<"a pt("<<x1<<" "<<x2<<" "<<x3<<") ChannelCollection is registered in the Manager "<<endm;
}

void 
StPidAmpMaker::AddPtChannelCollection(Double_t x1, Double_t x2,Double_t x3, Double_t x4,TString fitOpt, TString drawOpt){
    theManager->bookAPtChannelCollection(x1,x2,x3,x4,fitOpt,drawOpt);
    gMessMgr->Info()<<"a pt("<<x1<<" "<<x2<<" "<<x3<<" "<<x4<<") ChannelCollection is registered in the Manager "<<endm;
}

void 
StPidAmpMaker::AddPtChannelCollection(Double_t x1, Double_t x2, Double_t x3, Double_t x4, Double_t x5,TString fitOpt, TString drawOpt){
    theManager->bookAPtChannelCollection(x1,x2,x3,x4,x5,fitOpt,drawOpt);
    gMessMgr->Info()<<"a Pt("<<x1<<" "<<x2<<" "<<x3<<" "<<x4<<x5<<" "<<") ChannelCollection is registered in the Manager"<<endm;
}

    

void 
StPidAmpMaker::AddPtNHitsChannelCollection(Int_t n, Int_t* nhitsAry,Int_t p, Double_t* ptAry,TString fitOpt, TString drawOpt){
  
    Int_t j;

    theManager->bookAPtNHitsChannelCollection(n, nhitsAry, p, ptAry,fitOpt,drawOpt);
    gMessMgr->Info()<<"a Pt( ";
    for ( j=0; j<n; j++) gMessMgr->Info()<<nhitsAry[j]<<" ";
    gMessMgr->Info()<<")&NHits( ";
    for ( j=0; j<p; j++) gMessMgr->Info()<<ptAry[j]<<" ";
    gMessMgr->Info()<<") ChannelCollection is registered in the Manager "<<endm;
}



    



