/***************************************************************************
 *
 * $Id: StPidAmpMaker.cxx,v 1.6 2000/05/01 16:59:49 aihong Exp $
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

#include "tables/St_dst_track_Table.h"
#include "tables/St_dst_dedx_Table.h"
#include "tables/St_dst_vertex_Table.h"

#include "StPidAmpMaker/Include/StPidAmpConst.hh"



void fillStPidAmpTrks(St_dst_track* theTrackTable, St_dst_dedx* theDedxTable, St_dst_vertex* theVertexTable, StPidAmpTrkVector* trks);
void readDataFromDisk(StPidAmpTrkVector* trks);//read trks from disk.
void writeTrks(St_dst_track* theTrackTable, St_dst_dedx* theDedxTable, St_dst_vertex* theVertexTable);//write trks to disk for quik reading.vi readDataFromDisk.

ClassImp(StPidAmpMaker)

StPidAmpMaker::StPidAmpMaker(const Char_t *name) : StMaker(name)
{
    theManager    =new StPidAmpManager(); 

    ampTrks     =new StPidAmpTrkVector();

    mNHits4BG=0;
    theManager->passTrksAddress(ampTrks);

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
    // readDataFromDisk(ampTrks);

    //release unused space back to memory.
    StPidAmpTrkVector tmpVector=*ampTrks;
    ampTrks->swap(tmpVector);

     
    //run...
    if (theManager->netSets()->size()==0) 
    theManager->bookADefaultChannelCollection("BAR"," ");


    theManager->process();
 

    return kStOK;
}

Int_t
StPidAmpMaker::Make()
{
  St_DataSet *dst_data = GetInputDS("dst");
  if (!dst_data) return 0;

  St_DataSetIter  local(dst_data);

  St_dst_track* globalTable   =( St_dst_track *)local["globtrk"];
  St_dst_dedx*  dst_dedxTable =(St_dst_dedx *)  local["dst_dedx"];
  St_dst_vertex *vertexTable  =(St_dst_vertex* )local["vertex"];

    // OK, we've got the tables. Pass them and process them.

     if (globalTable && dst_dedxTable && vertexTable) {
    fillStPidAmpTrks(globalTable, dst_dedxTable,vertexTable, ampTrks);
    //    writeTrks(globalTable, dst_dedxTable,vertexTable);
    return kStOK;

     } else return 0;

}

void
StPidAmpMaker::SetNHitsFilter2LastCollection(Int_t nhits){
       theManager->setNHits4BGNet(nhits);
}



void 
StPidAmpMaker::AddDefaultChannelCollection(TString fitOpt, TString drawOpt){
    theManager->bookADefaultChannelCollection(fitOpt,drawOpt);
    gMessMgr->Info()<<"a default ChannelCollection is registered in Manager"<<endm;
}





void 
StPidAmpMaker::AddNHitsChannelCollection(Int_t x1, Int_t x2,TString fitOpt, TString drawOpt){
    theManager->bookADefaultChannelCollection(fitOpt,drawOpt);
    gMessMgr->Info()<<"ignored two inputs "<<x1<<" "<<x2<<endm;
    gMessMgr->Info()<<"two inputs is for default option, the default ChannelCollection is registered in the Manager"<<endm;
}


void 
StPidAmpMaker::AddNHitsChannelCollection(Int_t x1, Int_t x2, Int_t x3,TString fitOpt, TString drawOpt){
    theManager->bookANHitsChannelCollection(x1,x2,x3,fitOpt,drawOpt);
    gMessMgr->Info()<<"a nhits("<<x1<<" "<<x2<<" "<<x3<<") ChannelCollection is registered in the Manager "<<endm;
}

void 
StPidAmpMaker::AddNHitsChannelCollection(Int_t x1, Int_t x2,Int_t x3, Int_t x4,TString fitOpt, TString drawOpt){
    theManager->bookANHitsChannelCollection(x1,x2,x3,x4,fitOpt,drawOpt);
    gMessMgr->Info()<<"a nhits("<<x1<<" "<<x2<<" "<<x3<<" "<<x4<<") ChannelCollection is registered in the Manager "<<endm;
}



void 
StPidAmpMaker::AddNHitsChannelCollection(Int_t x1, Int_t x2, Int_t x3, Int_t x4, Int_t x5,TString fitOpt, TString drawOpt){
    theManager->bookANHitsChannelCollection(x1,x2,x3,x4,x5,fitOpt,drawOpt);
    gMessMgr->Info()<<"a nhits("<<x1<<" "<<x2<<" "<<x3<<" "<<x4<<x5<<" "<<") ChannelCollection is registered in the Manager"<<endm;
}


void 
StPidAmpMaker::AddNHitsDcaChannelCollection(Int_t x1, Int_t x2,TString fitOpt,Double_t d1, Double_t d2, Double_t d3, TString drawOpt){
    theManager->bookADefaultChannelCollection(fitOpt,drawOpt);
    gMessMgr->Info()<<"ignored two inputs "<<x1<<" "<<x2<<endm;
    gMessMgr->Info()<<"two inputs is for default option, the default ChannelCollection is registered in the Manager"<<endm;
}


void 
StPidAmpMaker::AddNHitsDcaChannelCollection(Int_t x1, Int_t x2, Int_t x3,TString fitOpt,Double_t d1, Double_t d2,  Double_t d3, TString drawOpt){
    theManager->bookANHitsDcaChannelCollection(x1,x2,x3,fitOpt,drawOpt,d1,d2,d3);
    gMessMgr->Info()<<"a nhits("<<x1<<" "<<x2<<" "<<x3<<") dca("<<d1<<" "<<d2<<" "<<d3<<")  ChannelCollection is registered in the Manager "<<endm;
}

void 
StPidAmpMaker::AddNHitsDcaChannelCollection(Int_t x1, Int_t x2,Int_t x3, Int_t x4,TString fitOpt, Double_t d1,  Double_t d2, Double_t d3, TString drawOpt){

    theManager->bookANHitsDcaChannelCollection(x1,x2,x3,x4,fitOpt,drawOpt,d1,d2,d3);
    gMessMgr->Info()<<"a nhits("<<x1<<" "<<x2<<" "<<x3<<" "<<x4<<") dca("<<d1<<" "<<d2<<" "<<d3<<") ChannelCollection is registered in the Manager "<<endm;
}



void 
StPidAmpMaker::AddNHitsDcaChannelCollection(Int_t x1, Int_t x2, Int_t x3, Int_t x4, Int_t x5,TString fitOpt, Double_t d1,  Double_t d2, Double_t  d3, TString drawOpt){
    theManager->bookANHitsDcaChannelCollection(x1,x2,x3,x4,x5,fitOpt,drawOpt,d1,d2,d3);
    gMessMgr->Info()<<"a nhits("<<x1<<" "<<x2<<" "<<x3<<" "<<x4<<x5<<" "<<") dca("<<d1<<" "<<d2<<" "<<d3<<") ChannelCollection is registered in the Manager "<<endm;
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



    



