// $Id: StStrangeMuDstMaker.cxx,v 1.2 2000/03/29 20:52:13 genevb Exp $
// $Log: StStrangeMuDstMaker.cxx,v $
// Revision 1.2  2000/03/29 20:52:13  genevb
// Added StKinkMuDst, replaced arrays
//
// Revision 1.1  2000/03/29 03:10:07  genevb
// Introduction of Strangeness Micro DST package
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StStrangeMuDstMaker strangeness micro DST maker                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TFile.h"
#include "TTree.h"
#include "StChain.h"
#include "StEventMaker/StEventMaker.h"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "StV0Vertex.h"
#include "StXiVertex.h"
#include "StKinkVertex.h"
#include "StStrangeEvMuDst.hh"
#include "StV0MuDst.hh"
#include "StXiMuDst.hh"
#include "StKinkMuDst.hh"
#include "StStrangeMuDstMaker.h"
#include "StMessMgr.h"
#include "TClonesArray.h"
#include "TBranch.h"

#define MXENT 5000
#define MXKINK 500
int nV0Entries = 0;
int nXiEntries = 0;
int nKinkEntries = 0;

//_____________________________________________________________________________
StStrangeMuDstMaker::StStrangeMuDstMaker(const char *name) : StMaker(name){
  doV0 = kFALSE;
  doXi = kFALSE;
  doKink = kFALSE;
  muDst = 0;
  tree = 0;
  evClonesArray = 0;
  v0ClonesArray = 0;
  xiClonesArray = 0;
  kinkClonesArray = 0;
  rw = StrangeNoFile;
  SetNumber(-2);
  evFile="evMuDst.root";
  v0File="v0MuDst.root";
  xiFile="xiMuDst.root";
  kinkFile="kinkMuDst.root";
}
//_____________________________________________________________________________
StStrangeMuDstMaker::~StStrangeMuDstMaker(){
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::Init(){

  gMessMgr->Debug("In StStrangeMuDstMaker::Init() ...");
  
  if (OpenFile() == kStErr) return kStErr;
  evClonesArray = new TClonesArray("StStrangeEvMuDst",1);
  
  if (rw == StrangeRead) {            // READING  the Micro Dst
    tree = (TTree*) muDst->Get("StrangeMuDst");
    if (!tree) {
      gMessMgr->Error() << "StStrangeMuDstMaker: no StrangeMuDst tree"
	                << " in file " << evFile << endm;
      return kStErr;
    }
    tree->SetBranchAddress("Event",&evClonesArray);
    if (doV0) {
      v0ClonesArray = new TClonesArray("StV0MuDst",MXENT);
      tree->SetBranchAddress("V0",&v0ClonesArray);
    }
    if (doXi) {
      xiClonesArray = new TClonesArray("StXiMuDst",MXENT);
      tree->SetBranchAddress("Xi",&xiClonesArray);
    }
    if (doKink) {
      xiClonesArray = new TClonesArray("StKinkMuDst",MXKINK);
      tree->SetBranchAddress("Kink",&kinkClonesArray);
    }

  } else {                           // CREATING the Micro Dst
    Int_t split=1;
    Int_t bsize=64000;
    tree = new TTree("StrangeMuDst","Strangeness Micro-DST");
    tree->SetDirectory(muDst);
    TBranch* branch = tree->Branch("Event",&evClonesArray,bsize,split);
    if (doV0) {
      v0ClonesArray = new TClonesArray("StV0MuDst",MXENT);
      branch = tree->Branch("V0",&v0ClonesArray,bsize,split);
      if (rw == StrangeWrite) branch->SetFile(v0File);
    }
    if (doXi) {
      xiClonesArray = new TClonesArray("StXiMuDst",MXENT);
      branch = tree->Branch("Xi",&xiClonesArray,bsize,split);
      if (rw == StrangeWrite) branch->SetFile(xiFile);
    }
    if (doXi) {
      kinkClonesArray = new TClonesArray("StKinkMuDst",MXKINK);
      branch = tree->Branch("Kink",&kinkClonesArray,bsize,split);
      if (rw == StrangeWrite) branch->SetFile(kinkFile);
    }
  }
  nV0Entries = 0;
  nXiEntries = 0;
  nKinkEntries = 0;
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::Make(){

  gMessMgr->Debug("In StStrangeMuDstMaker::Make() ...");

  StStrangeEvMuDst* ev = 0;

  if (rw == StrangeRead) {            // READING  the Micro Dst
    Int_t event_number = GetNumber();
    if (event_number == -2) {        // If event numbers aren't supplied,
      event_number = 0;              // start at 0 and increment ourselves.
      SetNumber(-1);                 // Use m_Number = -1 to indicate this.
    } else if (event_number == -1) {
      event_number = tree->GetReadEvent() + 1;
    }
    if (! tree->GetEvent(event_number)) return kStErr;   // Read the event
    Int_t j;
    Int_t nentries;
    ev = GetEvent();                                     // Tell the vertices
    if (doV0) {                                          // about the event
      nentries = v0ClonesArray->GetEntriesFast();
      for (j = 0; j<nentries; j++) {
        StV0MuDst* v0 = (StV0MuDst*) (*v0ClonesArray)[j];
	v0->SetEvent(ev);
      }
      gMessMgr->Info() << "StStrangeMuDstMaker: read " << nentries
                       << " V0 entries" << endm;
      nV0Entries += nentries;
    }
    if (doXi) {
      nentries = xiClonesArray->GetEntriesFast();
      for (j = 0; j<nentries; j++) {
        StXiMuDst* xi = (StXiMuDst*) (*xiClonesArray)[j];
	xi->SetEvent(ev);
      }
      gMessMgr->Info() << "StStrangeMuDstMaker: read " << nentries
                       << " Xi entries" << endm;
      nXiEntries += nentries;
    }

  } else {                           // CREATING the Micro Dst

    // Get pointer to event
    StEventMaker* evMaker = (StEventMaker *) GetMaker("events");
    if( ! evMaker->event() ) return kStOK; 
    StEvent& event = *(evMaker->event());

    // First get primary vertex
    StPrimaryVertex *primaryVertex = event.primaryVertex();

    if( !primaryVertex ) {
      gMessMgr->Error("StStrangeMuDstMaker: no primary vertex");
      return kStErr;
    }

    ev = new((*evClonesArray)[0]) StStrangeEvMuDst(primaryVertex);

    StV0Vertex *v0Vertex = 0;
    StXiVertex *xiVertex = 0;
    StKinkVertex *kinkVertex = 0;
    Int_t v0Entries = 0;
    Int_t xiEntries = 0;
    Int_t kinkEntries = 0;
  
    // Second, loop over vertices to build linked list of xi/v0 candidates
    size_t i;
    size_t entries = 0;

    if (doV0) {
      StSPtrVecV0Vertex& v0Vertices = event.v0Vertices();
      entries = v0Vertices.size();
      for (i=0; i<entries; i++) {
        v0Vertex = v0Vertices[i];
        new((*v0ClonesArray)[v0Entries++]) StV0MuDst(v0Vertex,ev);
      }
      gMessMgr->Info() << "StStrangeMuDstMaker: found " << entries
                       << " V0 candidates" << endm;
      nV0Entries += entries;
    }
    if (doXi) {
      Int_t nBad = 0;
      StSPtrVecXiVertex& xiVertices = event.xiVertices();
      entries = xiVertices.size();
      for (i=0; i<entries; i++) {
        xiVertex = xiVertices[i];
        v0Vertex = xiVertex->v0Vertex();
        if (v0Vertex) {
          new((*xiClonesArray)[xiEntries++]) StXiMuDst(xiVertex,v0Vertex,ev);
        } else {
          nBad++;
        }
      }
      gMessMgr->Info() << "StStrangeMuDstMaker: found " << entries
                       << " Xi candidates" << endm;
      if (nBad) gMessMgr->Warning() << "StStrangeMuDstMaker: " << nBad
                                    << "with missing V0 vertices" << endm;
      nXiEntries += entries;
    }
    if (doKink) {
      StSPtrVecKinkVertex& kinkVertices = event.kinkVertices();
      entries = kinkVertices.size();
      for (i=0; i<entries; i++) {
        kinkVertex = kinkVertices[i];
        new((*kinkClonesArray)[kinkEntries++]) StKinkMuDst(kinkVertex);
      }
      gMessMgr->Info() << "StStrangeMuDstMaker: found " << entries
                       << " Kink candidates" << endm;
      nKinkEntries += entries;
    }
    tree->Fill();
  }
  return kStOK;
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::Clear(Option_t *option){
  if (evClonesArray) evClonesArray->Clear();
  if (doV0 && v0ClonesArray) v0ClonesArray->Clear();
  if (doXi && xiClonesArray) xiClonesArray->Clear();
  if (doKink && kinkClonesArray) kinkClonesArray->Clear();
  StMaker::Clear(option);
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::Finish(){

  gMessMgr->Debug("In StStrangeMuDstMaker::Finish() ..."); 

//  tree->Print();
  if (rw == StrangeWrite) muDst->Write();
  if (muDst) muDst->Close();
   if (doV0) {
    gMessMgr->Info() << "StStrangeMuDstMaker: "
                     << nV0Entries << " V0 Entries" << endm;
    delete v0ClonesArray;
    v0ClonesArray = 0;
  }
  if (doXi) {
    gMessMgr->Info() << "StStrangeMuDstMaker: "
	             << nXiEntries << " Xi Entries" << endm;
    delete xiClonesArray;
    xiClonesArray = 0;
  }
  if (doXi) {
    gMessMgr->Info() << "StStrangeMuDstMaker: "
	             << nKinkEntries << " Kink Entries" << endm;
    delete kinkClonesArray;
    kinkClonesArray = 0;
  }
  delete evClonesArray;
  evClonesArray = 0;
  return kStOK;
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::SetWrite(char* eFile, char* vFile,
                                   char* xFile, char* kFile) {
  rw = StrangeWrite;
  SetFiles(eFile,vFile,xFile,kFile);
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::SetRead (char* eFile, char* vFile,
                                   char* xFile, char* kFile) {
  rw = StrangeRead;
  SetFiles(eFile,vFile,xFile,kFile);
}
//_____________________________________________________________________________
void StStrangeMuDstMaker::SetFiles (char* eFile, char* vFile,
                                    char* xFile, char* kFile) {
  if (eFile) evFile = eFile;
  if (vFile) v0File = vFile;
  if (xFile) xiFile = xFile;
  if (kFile) kinkFile = kFile;
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::OpenFile() {
  char* option=0;
  if (rw == StrangeRead)
    option = "READ";
  else if (rw == StrangeWrite)
    option = "RECREATE";
  else
    return kStOk;
    
  if( ! (muDst = new TFile(evFile,option)) ) {
    gMessMgr->Error() << "StStrangeMuDstMaker: Error - opening event file "
                      << evFile << endm;
    return kStErr;
  }
  return kStOk;
}
//_____________________________________________________________________________
StStrangeEvMuDst* StStrangeMuDstMaker::GetEvent() {
  if (evClonesArray) return (StStrangeEvMuDst*) (*evClonesArray)[0];
  return 0;
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::GetNV0() {
  if (v0ClonesArray) return v0ClonesArray->GetEntriesFast();
  return 0;
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::GetNXi() {
  if (xiClonesArray) return xiClonesArray->GetEntriesFast();
  return 0;
}
//_____________________________________________________________________________
Int_t StStrangeMuDstMaker::GetNKink() {
  if (kinkClonesArray) return kinkClonesArray->GetEntriesFast();
  return 0;
}
//_____________________________________________________________________________
StV0MuDst* StStrangeMuDstMaker::GetV0(Int_t i) {
  if (v0ClonesArray) return (StV0MuDst*) (*v0ClonesArray)[i];
  return 0;
}
//_____________________________________________________________________________
StXiMuDst* StStrangeMuDstMaker::GetXi(Int_t i) {
  if (xiClonesArray) return (StXiMuDst*) (*xiClonesArray)[i];
  return 0;
}
//_____________________________________________________________________________
StKinkMuDst* StStrangeMuDstMaker::GetKink(Int_t i) {
  if (kinkClonesArray) return (StKinkMuDst*) (*kinkClonesArray)[i];
  return 0;
}
//_____________________________________________________________________________
ClassImp(StStrangeMuDstMaker)
