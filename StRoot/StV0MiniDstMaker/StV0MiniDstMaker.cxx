// $Id: StV0MiniDstMaker.cxx,v 1.2 1999/08/13 12:38:16 jones Exp $
// $Log: StV0MiniDstMaker.cxx,v $
// Revision 1.2  1999/08/13 12:38:16  jones
// Major revision to merge StV0MiniDstMaker and StXiMiniDstMaker
//
// Revision 1.1  1999/07/13 12:42:24  jones
// *** empty log message ***
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StV0MiniDstMaker class                                               //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TFile.h"
#include "TOrdCollection.h"
#include "StChain.h"
#include "StEventMaker/StEventMaker.h"
#include "StEvent.h"
#include "StXiVertex.h"
#include "StXiMiniDst.hh"
#include "StV0MiniDstMaker.h"

#define v0_max 5000

TFile* muDst;

//_____________________________________________________________________________
StV0MiniDstMaker::StV0MiniDstMaker(const char *name, const char *file) : StMaker(name){
  mVertexType = undefined;
  mFileName = file;
  mCollection = 0;
  mEntries = 0;
}
//_____________________________________________________________________________
StV0MiniDstMaker::~StV0MiniDstMaker(){
}
//_____________________________________________________________________________
Int_t StV0MiniDstMaker::Init(){

  printf("In StV0MiniDstMaker::Init() ...\n"); 

  // Create output ROOT file
  muDst = new TFile(mFileName,"RECREATE");

  // Create a TOrdCollection
  mCollection = new TOrdCollection(v0_max);

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StV0MiniDstMaker::Make(){

  printf("In StV0MiniDstMaker::Make() ...\n");

  // Get pointer to event
  StEventMaker* evMaker = (StEventMaker *) GetMaker("events");
  if( ! evMaker->event() ) return kStOK; 
  StEvent& event = *(evMaker->event());

  // Obtain the vertex collection from StEvent
  StVertexCollection* vertices = event.vertexCollection();
  StVertexIterator iter;
  StVertex *vertex = 0;
  StVertex *primaryVertex = 0;
  StXiVertex *xiVertex = 0;
  StV0Vertex *v0Vertex = 0;
  StXiMiniDst *ximdst = 0;
  StV0MiniDst *v0mdst = 0;
  Int_t nLast = mEntries, nBad = 0;
  
  // First loop over vertices searches for primary vertex
  for( iter=vertices->begin(); iter!=vertices->end(); iter++) {
    vertex = *iter;
    if( vertex->type() == primary ) {
      primaryVertex = vertex;
      break;
    }
  }

  if( !primaryVertex ) {
    printf("StV0MiniDstMaker: Error - no primary vertex\n");
    return kStErr;
  }

  // Second loop over vertices builds linked list of v0 candidates
  for( iter=vertices->begin(); iter!=vertices->end(); iter++) {
    vertex = *iter;
    if( vertex->type() == mVertexType ) {
      switch(mVertexType) {
      case Xi:
	xiVertex = dynamic_cast<StXiVertex*>(vertex);
	v0Vertex = xiVertex->v0Vertex();
	if( v0Vertex ) {
	  ximdst = new StXiMiniDst(xiVertex,v0Vertex,primaryVertex);
	  mCollection->Add(ximdst);
	  mEntries++;
	} else {
	  nBad++;
	}
	break;
      case V0:
	v0Vertex = dynamic_cast<StV0Vertex*>(vertex);
	v0mdst = new StV0MiniDst(v0Vertex,primaryVertex);
	mCollection->Add(v0mdst);
	mEntries++;
	break;
      }
    }
  }
  
  printf("StV0MiniDstMaker: found %d candidates\n",mEntries-nLast);
  if( nBad != 0 )
    printf("StV0MiniDstMaker: Warning - %d with missing V0 vertices\n",nBad);

  return kStOK;
}
//_____________________________________________________________________________
Int_t StV0MiniDstMaker::Finish(){

  printf("In StV0MiniDstMaker::Finish() ...\n"); 

  // Change to output directory
  muDst->cd();

  // Write output
  mCollection->Write("MuDst",kSingleKey);

  // Close output file
  muDst->Close();

  return kStOK;
}
//_____________________________________________________________________________
TOrdCollection* StV0MiniDstMaker::Read(Int_t* nent) {
  mEntries = 0;
  mCollection = 0;
  StV0MiniDst* v0mdst;
  StXiMiniDst* ximdst;

  if( mVertexType == undefined ) {
    printf("StV0MiniDstMaker: Error - VertexType is undefined\n");
    return 0;
  }

  muDst = new TFile(mFileName);
  mCollection = (TOrdCollection *) muDst->Get("MuDst;1");
  if( mCollection ) {
    mEntries = mCollection->LastIndex()+1;
    printf("StV0MiniDstMaker: number in collection %d\n",mEntries);
  } else
    printf("StV0MiniDstMaker: NULL pointer to collection\n");
  
  for( Int_t i=0; i<mEntries; i++ ) {
    switch( mVertexType ) {
    case V0:
      v0mdst = (StV0MiniDst *) mCollection->At(i);
      v0mdst->UpdateV0();
      break;
    case Xi:
      ximdst = (StXiMiniDst *) mCollection->At(i);
      ximdst->UpdateV0();
      ximdst->UpdateXi();
      break;
    }
  }

  muDst->Close();

  *nent = mEntries;
  return mCollection;
}
//_____________________________________________________________________________
ClassImp(StV0MiniDstMaker)

