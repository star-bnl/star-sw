// $Id: StV0MiniDstMaker.cxx,v 1.4 1999/09/02 09:53:41 jones Exp $
// $Log: StV0MiniDstMaker.cxx,v $
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
#include "StXiVertex.h"
#include "StXiMiniDst.hh"
#include "StV0MiniDstMaker.h"

#define MXENT 5000

TFile* muDst = 0;
TTree* tree = 0;
StXiMiniDst *xi = 0;
StV0MiniDst *v0 = 0;

//_____________________________________________________________________________
StV0MiniDstMaker::StV0MiniDstMaker(const char *name) : StMaker(name){
  mVertexType = undefined;
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

  printf("In StV0MiniDstMaker::Init() ...\n"); 

  if( mWriteFile ) {
    if( mUseTree ) {
      // Create a ROOT TTree, based on a TClonesArray
      tree = new TTree("muDst","Strangeness Micro-DST");
      Int_t split=1;
      Int_t bsize=64000;
      switch( mVertexType ) {
      case V0:
	mClonesArray = new TClonesArray("StV0MiniDst",MXENT);
	tree->Branch("V0",&mClonesArray,bsize,split);
	break;
      case Xi:
	mClonesArray = new TClonesArray("StXiMiniDst",MXENT);
	tree->Branch("Xi",&mClonesArray,bsize,split);
	break;
      default:
	printf("StV0MiniDstMaker::Init Error - unrecognised vertex type\n");
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

  printf("In StV0MiniDstMaker::Make() ...\n");

  // Do nothing if file open for reading only
  if( ! mWriteFile ) return kStOk;

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
  StEvMiniDst *ev = 0;
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

  ev = new StEvMiniDst(primaryVertex);

  // Second loop over vertices builds linked list of v0 candidates
  for( iter=vertices->begin(); iter!=vertices->end(); iter++) {
    vertex = *iter;
    if( vertex->type() == mVertexType ) {
      switch(mVertexType) {
      case Xi:
	xiVertex = dynamic_cast<StXiVertex*>(vertex);
	v0Vertex = xiVertex->v0Vertex();
	if( v0Vertex ) {
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
	break;
      case V0:
	v0Vertex = dynamic_cast<StV0Vertex*>(vertex);
	if( mUseTree ) {
	  new((*mClonesArray)[mEntries++]) StV0MiniDst(v0Vertex,ev);
	} else {
	  v0 = new StV0MiniDst(v0Vertex,ev);
	  mCollection->Add(v0);
	  mEntries++;
	}
	break;
      }
    }
  }
  
  printf("StV0MiniDstMaker: Info - found %d candidates\n",mEntries-nLast);
  if( nBad != 0 )
    printf("StV0MiniDstMaker: Warning - %d with missing V0 vertices\n",nBad);

  if( mUseTree && mEntries==MXENT ) {
    printf("StV0MiniDstMaker: Info - filling tree\n");
    tree->Fill();
    mClonesArray->Clear();
    mEntries = mClonesArray->GetEntriesFast();
    //    mCollection->Delete();
    //    mEntries = mCollection->GetSize();
    if( mEntries !=0 )
      printf("StV0MiniDstMaker: Warning - collection not empty\n");
  }

  return kStOK;
}
//_____________________________________________________________________________
Int_t StV0MiniDstMaker::Finish(){

  printf("In StV0MiniDstMaker::Finish() ...\n"); 

  if( mWriteFile ) {

    // Change to output directory
    muDst->cd();
    
    // Write output
    if( mUseTree ) {
      //    mClonesArray->Write("MuDst",kSingleKey);
      printf("StV0MiniDstMaker::Info - %d Entries\n",
	     mClonesArray->GetEntriesFast());
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
    printf("StV0MiniDstMaker: Error - opening file %s\n",mFileName);
    return kStErr;
  }

  return kStOk;
}
//_____________________________________________________________________________
Int_t StV0MiniDstMaker::SetOutputFile(const char* file) {
  mFileName = file;
  mWriteFile = kTRUE;

  if( ! (muDst = new TFile(mFileName,"RECREATE")) ) {
    printf("StV0MiniDstMaker: Error - opening file %s\n",mFileName);
    return kStErr;
  }

  return kStOk;
}
//_____________________________________________________________________________
TOrdCollection* StV0MiniDstMaker::Read(Int_t* nent) {
  *nent = mEntries = 0;
  mCollection = 0;
  mClonesArray = 0;

  if( mVertexType == undefined ) {
    printf("StV0MiniDstMaker::Read Error - VertexType is undefined\n");
    return 0;
  }

  if( ! muDst ) {
    printf("StV0MiniDstMaker::Read Error - File is not open\n");
    return 0;
  }

  if( mUseTree ) {
    tree = (TTree *) muDst->Get("muDst");
    TBranch *branch = tree->GetBranch("Xi");
    branch->SetAddress(&mClonesArray);
    printf("StV0MiniDstMaker::Read Info - %d events\n",(Int_t)tree->GetEntries());
    tree->GetEntry(0); // Read first event into memory
  } else
    mCollection = (TOrdCollection *) muDst->Get("MuDst;1");

  if( mUseTree && mClonesArray )
    mEntries = mClonesArray->GetEntries();
  else if( mCollection )
    mEntries = mCollection->GetSize();
  else {
    printf("StV0MiniDstMaker::Read Error - NULL pointer to data\n");
    return 0;
  }

  printf("StV0MiniDstMaker::Read Info - number of entries %d\n",mEntries);

  for( Int_t i=0; i<mEntries; i++ ) {
    switch( mVertexType ) {
    case V0:
      if( mUseTree )
	v0 = (StV0MiniDst *) (*mClonesArray)[i];
      else
	v0 = (StV0MiniDst *) mCollection->At(i);
      v0->UpdateV0();
      break;
    case Xi:
      if( mUseTree )
	xi = (StXiMiniDst *) (*mClonesArray)[i];
      else
	xi = (StXiMiniDst *) mCollection->At(i);
      xi->UpdateV0();
      xi->UpdateXi();
      break;
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
