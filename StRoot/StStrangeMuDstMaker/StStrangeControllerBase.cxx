// $Id: StStrangeControllerBase.cxx,v 2.1 2000/06/09 22:17:10 genevb Exp $
// $Log: StStrangeControllerBase.cxx,v $
// Revision 2.1  2000/06/09 22:17:10  genevb
// Allow MC data to be copied between DSTs, other small improvements
//
// Revision 2.0  2000/06/05 05:19:41  genevb
// New version of Strangeness micro DST package
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StStrangeControllerBase strangeness micro DST controller base class  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "StStrangeControllerInclude.h"
#include "StStrangeMuDst.hh"
#include "TROOT.h"
#include "TTree.h"
#include "TBranch.h"

StStrangeMuDstMaker* StStrangeControllerBase::currentMaker = 0;

ClassImp(StStrangeControllerBase)
//_____________________________________________________________________________
StStrangeControllerBase::StStrangeControllerBase(const char *name) :
TNamed(name,"StStrangeController") {
  masterMaker = currentMaker;
  doMc = masterMaker->GetDoMc();
  if ((dstMaker = masterMaker->GetSubDst())) {
    if (doMc) dstMaker->DoMc();
    dstMaker->Do(name);
  }
  tree = 0;
  file = 0;
  selections = 0;
  tempArray = 0;
  mcName = name;
  mcName += "Mc";
  assocName = name;
  assocName += "Assoc";
  TString str;
  ((str = "St") += name) += "MuDst";
  dataArray = new TClonesArray(str.Data(),max);
  dataClass = gROOT->GetClass(str.Data());
  if (doMc && !(dstMaker)) {
    (str = "St") += mcName;
    mcArray = new TClonesArray(str.Data(),max);
    assocArray = new TClonesArray("StStrangeAssoc",max);
    assocMaker =
        (StAssociationMaker*) (masterMaker->GetMaker("StAssociationMaker"));
  } else {
    mcArray = 0;
    assocArray = 0;
    assocMaker = 0;
  }
  nEntries = 0;
  entries = 0;
  mcEntries = 0;
  assocEntries = 0;

  increment = 500;
  max = 2000;
}
//_____________________________________________________________________________
StStrangeControllerBase::~StStrangeControllerBase() {
  delete dataArray;  dataArray  = 0;
  delete mcArray;    mcArray    = 0;
  delete assocArray; assocArray = 0;
  delete selections; selections = 0;
}
//_____________________________________________________________________________
StStrangeControllerBase* StStrangeControllerBase::Instantiate(const char* name){
  TString nom = name;
  TString first = nom;
  first.Remove(1).ToUpper();
  nom.Replace(0,1,first);
  nom.Prepend("St").Append("Controller");
  TClass* controlClass = gROOT->GetClass(nom.Data());
  return (StStrangeControllerBase*) ((controlClass) ? controlClass->New() : 0);
}
//_____________________________________________________________________________
void StStrangeControllerBase::InitReadDst() {
  tree = masterMaker->GetTree();
  tree->SetBranchAddress(GetName(),&dataArray);
  if (doMc) {
    if (!tree->GetBranch(mcName.Data())) {
      gMessMgr->Warning() << IsA()->GetName() <<
                        ": No MC data available, continuing without." << endm;
      doMc = kFALSE;
    } else {
      tree->SetBranchAddress(mcName.Data(),&mcArray);
      tree->SetBranchAddress(assocName.Data(),&assocArray);
    }
  }
}
//_____________________________________________________________________________
void StStrangeControllerBase::InitCreateDst(const char* filename) {
  tree = masterMaker->GetTree();
  file = const_cast<char*> (filename);
  AssignBranch(GetName(),&dataArray);
  if (doMc && !(dstMaker)) {
    AssignBranch(GetMcName(),&mcArray);
    AssignBranch(GetAssocName(),&assocArray);
  }
}
//_____________________________________________________________________________
void StStrangeControllerBase::InitCreateSubDst() {
  selections = new TArrayI(max);
  StStrangeControllerBase* dstController = GetDstController();
  tempArray = dstController->GetDataArray();
  if (doMc) {
    if (!dstMaker->GetTree()->GetBranch(mcName.Data())) {
      doMc = kFALSE;
    } else {
      mcArray = dstController->GetMcArray();
      assocArray = dstController->GetAssocArray();
      AssignBranch(GetMcName(),&mcArray);
      AssignBranch(GetAssocName(),&assocArray);
    }
  }
}
//_____________________________________________________________________________
TBranch* StStrangeControllerBase::AssignBranch(const char* name,
                                               TClonesArray** address) {
  static Int_t split=2;
  static Int_t bsize=64000;
  TBranch* branch = tree->Branch(name,address,bsize,split);
  if (masterMaker->GetMode() == StrangeWrite) branch->SetFile(file);
  return branch;
}
//_____________________________________________________________________________
Int_t StStrangeControllerBase::MakeCreateSubDst() {
  // If no entries to copy, skip data and association copying
  if (!entries) {
    if (doMc) assocArray->Clear();
    return kStOK;
  }

  // Must reassign associations with MC vertices
  if (doMc) {
    for (Int_t j=0;j<GetNAssoc(); j++) {
      StStrangeAssoc* assocEntry = GetAssoc(j);
      Bool_t kept = kFALSE;
      for (Int_t k=0; k<entries; k++) {
	if (assocEntry->indexRecoArray() == (*selections)[k]) {
	  assocEntry->setIndexRecoArray(k);
	  kept = kTRUE;
	  break;
	}
      }
      if (!kept) assocArray->RemoveAt(j);
    }
    assocArray->Compress();
  }

  Int_t classSize = dataClass->Size();
  // Event info copied directly from dstMaker.
  if ((*selections)[0] < 0) {  // Copy all from the event
    tree->SetBranchAddress(GetName(),&tempArray);
  } else if (entries) {        // Copy selected from the event
    Int_t asize = dataArray->GetSize();
    if (entries > asize) dataArray->Expand(entries+increment);
    StStrangeControllerBase* dstController = GetDstController();
    for (Int_t k=0; k<entries; k++) {
//    Replacing the following standard allocation with faster memcpy
//      new((*dataArray)[k]) StV0MuDst(*((StV0MuDst*)
//	    (GetDstController()->Get((*selections)[k]))));
      memcpy((*dataArray)[k],dstController->Get((*selections)[k]),classSize);
    }
  }
  PrintNumCand("copying",entries);
  nEntries += entries;
  
  return kStOK;
}
//_____________________________________________________________________________
void StStrangeControllerBase::Clear() {
  if (dstMaker) {                                // Making a subDST
    selections->Reset();
    if (tree->GetBranch(GetName())->GetAddress() != (char*) &dataArray) {
      tree->SetBranchAddress(GetName(),&dataArray);
    }
  }
  if (dataArray) {
    dataArray->Clear();
    entries = 0;
    if (doMc) {
      mcArray->Clear();
      mcEntries = 0;
      assocArray->Clear();
      assocEntries = 0;
    }
  }
}
//_____________________________________________________________________________
void StStrangeControllerBase::Finish() {
  TString fin = "Total ";
  if (masterMaker->GetMode() == StrangeRead) fin += "read:";
  else fin += "kept:";
  PrintNumCand(fin.Data(),nEntries);
}
//_____________________________________________________________________________
void StStrangeControllerBase::Select(Int_t i) {
  if ((!selections) || (!dstMaker) || (selections->At(0) < 0)) return;
  Int_t ent = GetDstController()->GetN();
  if (i < 0) {
    selections->AddAt(i,0);
    entries = ent;
  } else if (i < ent) {
    if (ent > selections->GetSize()) selections->Set(ent+increment);
    selections->AddAt(i,entries++);
  }
}
//_____________________________________________________________________________
void StStrangeControllerBase::PrintNumMc() {
  gMessMgr->Info() << IsA()->GetName() << ": found " <<
        mcEntries << " Monte Carlo " << GetName() << "s" << endm;
  gMessMgr->Info() << IsA()->GetName() << ": found " <<
        assocEntries << " " << GetName() << " associations" << endm;
}
