// $Id: StV0MiniDstMaker.cxx,v 1.7 2000/03/06 23:12:33 genevb Exp $
// $Log: StV0MiniDstMaker.cxx,v $
// Revision 1.7  2000/03/06 23:12:33  genevb
// Change to file directory before creating TTree
//
// Revision 1.6  2000/01/04 19:41:52  genevb
// Fixed linux compiler warnings
//
// Revision 1.5  1999/11/19 19:44:47  genevb
// Modified for StEvent 2.0
//
// Revision 1.4  1999/09/02 09:53:41  jones
// Protected Make() if called in read mode; Protected Read() if file not open
//
// Revision 1.3  1999/09/02 09:04:56  jones
// Added StEvMiniDst class, New file handling, Partially implemented TTrees
//
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
#include "TTree.h"
#include "TOrdCollection.h"
#include "TClonesArray.h"
#include "StChain.h"
#include "StEventMaker/StEventMaker.h"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "StV0Vertex.h"
#include "StXiVertex.h"
#include "StXiMiniDst.hh"
#include "StV0MiniDstMaker.h"
#include "StMessMgr.h"

#define MXENT 5000

TFile* muDst = 0;
TTree* tree = 0;
StXiMiniDst *xi = 0;
StV0MiniDst *v0 = 0;

//_____________________________________________________________________________
StV0MiniDstMaker::StV0MiniDstMaker(const char *name) : StMaker(name){
  mVertexType = kUndefinedVtxId;
  mFileName = "";
  mCollection = 0;
  mClonesArray = 0;
  mEntries = 0;
  mWriteFile = kFALSE;
  mUseTree = kFALSE;
}
//_____________________________________________________________________________
StV0MiniDstMaker::~StV0MiniDstMaker(){
}
//_____________________________________________________________________________
Int_t StV0MiniDstMaker::Init(){

  gMessMgr->Info("In StV0MiniDstMaker::Init() ..."); 

  if( mWriteFile ) {
    if( mUseTree ) {
      // Create a ROOT TTree, based on a TClonesArray
      if (muDst) muDst->cd();
      tree = new TTree("muDst","Strangeness Micro-DST");
      Int_t split=1;
      Int_t bsize=64000;
      switch( mVertexType ) {
      case kV0VtxId:
	mClonesArray = new TClonesArray("StV0MiniDst",MXENT);
	tree->Branch("V0",&mClonesArray,bsize,split);
	break;
      case kXiVtxId:
	mClonesArray = new TClonesArray("StXiMiniDst",MXENT);
	tree->Branch("Xi",&mClonesArray,bsize,split);
	break;
      default:
        gMessMgr->Error("StV0MiniDstMaker::Init(): unrecognised vertex type");
	return kStErr;
      }
    } else {
      // Create a TOrdCollection
      mCollection = new TOrdCollection(MXENT);
    }
  }

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StV0MiniDstMaker::Make(){

  gMessMgr->Info("In StV0MiniDstMaker::Make() ...");

  // Do nothing if file open for reading only
  if( ! mWriteFile ) return kStOk;

  // Get pointer to event
  StEventMaker* evMaker = (StEventMaker *) GetMaker("events");
  if( ! evMaker->event() ) return kStOK; 
  StEvent& event = *(evMaker->event());

  // First get primary vertex
  StPrimaryVertex *primaryVertex = event.primaryVertex();

  if( !primaryVertex ) {
    gMessMgr->Error("StV0MiniDstMaker: no primary vertex");
    return kStErr;
  }

  StEvMiniDst *ev = new StEvMiniDst(primaryVertex);
  StXiVertex *xiVertex = 0;
  StV0Vertex *v0Vertex = 0;
  Int_t nLast = mEntries, nBad = 0;
  unsigned int i;
  
  // Second, loop over vertices to build linked list of xi/v0 candidates
  StSPtrVecXiVertex& xiVertices = event.xiVertices();
  StSPtrVecV0Vertex& v0Vertices = event.v0Vertices();

  switch (mVertexType) {
  case kXiVtxId:
    for (i=0; i<xiVertices.size(); i++) {
      xiVertex = xiVertices[i];
      v0Vertex = xiVertex->v0Vertex();
      if (v0Vertex) {
        if( mUseTree ) { 
          new((*mClonesArray)[mEntries++]) StXiMiniDst(xiVertex,v0Vertex,ev);
        } else {
          xi = new StXiMiniDst(xiVertex,v0Vertex,ev);
          mCollection->Add(xi);
          mEntries++;
        }
      } else {
        nBad++;
      }
    } break;
  case kV0VtxId:
    for (i=0; i<v0Vertices.size(); i++) {
      v0Vertex = v0Vertices[i];
      if( mUseTree ) { 
        new((*mClonesArray)[mEntries++]) StV0MiniDst(v0Vertex,ev);
      } else {
        v0 = new StV0MiniDst(v0Vertex,ev);
        mCollection->Add(v0);
        mEntries++;
      }
    } break;
  default:
    gMessMgr->Error("StV0MiniDstMaker::Make(): Vertex type unrecognized.");
    return kStErr;
  }
  
  gMessMgr->Info() << "StV0MiniDstMaker: found " << (mEntries-nLast)
                   << " candidates" << endm;
  if( nBad != 0 )
    gMessMgr->Warning() << "StV0MiniDstMaker: " << nBad
                        << "with missing V0 vertices" << endm;

  if( mUseTree && mEntries==MXENT ) {
    gMessMgr->Info("StV0MiniDstMaker: filling tree");
    tree->Fill();
    mClonesArray->Clear();
    mEntries = mClonesArray->GetEntriesFast();
    //    mCollection->Delete();
    //    mEntries = mCollection->GetSize();
    if( mEntries !=0 )
      gMessMgr->Warning("StV0MiniDstMaker: collection not empty");
  }

  return kStOK;
}
//_____________________________________________________________________________
Int_t StV0MiniDstMaker::Finish(){

  gMessMgr->Info("In StV0MiniDstMaker::Finish() ..."); 

  if( mWriteFile ) {

    // Change to output directory
    muDst->cd();
    
    // Write output
    if( mUseTree ) {
      //    mClonesArray->Write("MuDst",kSingleKey);
      gMessMgr->Info() << "StV0MiniDstMaker: "
	               << mClonesArray->GetEntriesFast() << " Entries" << endm;
      tree->Fill();
      tree->Print();
      muDst->Write();
    } else
      mCollection->Write("MuDst",kSingleKey);
    
    // Close output file
    muDst->Close();
  }
    
  return kStOK;
}
//_____________________________________________________________________________
Int_t StV0MiniDstMaker::SetInputFile(const char* file) {
  mFileName = file;
  mWriteFile = kFALSE;

  if( ! (muDst = new TFile(mFileName,"READ")) ) {
    gMessMgr->Error() << "StV0MiniDstMaker: Error - opening file "
                      << mFileName << endm;
    return kStErr;
  }

  return kStOk;
}
//_____________________________________________________________________________
Int_t StV0MiniDstMaker::SetOutputFile(const char* file) {
  mFileName = file;
  mWriteFile = kTRUE;

  if( ! (muDst = new TFile(mFileName,"RECREATE")) ) {
    gMessMgr->Error() << "StV0MiniDstMaker: Error - opening file "
                      << mFileName << endm;
    return kStErr;
  }

  return kStOk;
}
//_____________________________________________________________________________
TOrdCollection* StV0MiniDstMaker::Read(Int_t* nent) {
  *nent = mEntries = 0;
  mCollection = 0;
  mClonesArray = 0;

  if( mVertexType == kUndefinedVtxId ) {
    gMessMgr->Error("StV0MiniDstMaker::Read(): VertexType is undefined");
    return 0;
  }

  if( ! muDst ) {
    gMessMgr->Error("StV0MiniDstMaker::Read(): File is not open");
    return 0;
  }

  if( mUseTree ) {
    tree = (TTree *) muDst->Get("muDst");
    TBranch *branch = tree->GetBranch("Xi");
    branch->SetAddress(&mClonesArray);
    gMessMgr->Info() << "StV0MiniDstMaker::Read(): "
                     << (Int_t) (tree->GetEntries()) << " events" << endm;
    tree->GetEntry(0); // Read first event into memory
  } else
    mCollection = (TOrdCollection *) muDst->Get("MuDst;1");

  if( mUseTree && mClonesArray )
    mEntries = mClonesArray->GetEntries();
  else if( mCollection )
    mEntries = mCollection->GetSize();
  else {
    gMessMgr->Error("StV0MiniDstMaker::Read(): NULL pointer to data");
    return 0;
  }

  gMessMgr->Info() << "StV0MiniDstMaker::Read(): number of entries "
                   << mEntries << endm;

  for( Int_t i=0; i<mEntries; i++ ) {
    switch( mVertexType ) {
    case kV0VtxId:
      if( mUseTree )
	v0 = (StV0MiniDst *) (*mClonesArray)[i];
      else
	v0 = (StV0MiniDst *) mCollection->At(i);
      v0->UpdateV0();
      break;
    case kXiVtxId:
      if( mUseTree )
	xi = (StXiMiniDst *) (*mClonesArray)[i];
      else
	xi = (StXiMiniDst *) mCollection->At(i);
      xi->UpdateV0();
      xi->UpdateXi();
      break;
    default:
      gMessMgr->Error("StV0MiniDstMaker::Read(): Vertex type unrecognized.");
      return 0;
    }
  }

  muDst->Close();

  *nent = mEntries;
  //  if( mUseTree ) 
  //    return mClonesArray;
  //  else
    return mCollection;
}
//_____________________________________________________________________________
ClassImp(StV0MiniDstMaker)
