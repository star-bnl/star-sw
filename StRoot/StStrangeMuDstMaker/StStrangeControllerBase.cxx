// $Id: StStrangeControllerBase.cxx,v 2.0 2000/06/05 05:19:41 genevb Exp $
// $Log: StStrangeControllerBase.cxx,v $
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
#include "TTree.h"
#include "TBranch.h"

StStrangeMuDstMaker* StStrangeControllerBase::currentMaker = 0;

ClassImp(StStrangeControllerBase)
//_____________________________________________________________________________
StStrangeControllerBase::StStrangeControllerBase(const char *name) :
TNamed(name,"StStrangeController") {
  masterMaker = currentMaker;
  doMc = masterMaker->GetDoMc();
  dstMaker = masterMaker->GetSubDst();
  tree = 0;
  selections = 0;
  tempArray = 0;
  TString str;
  ((str = "St") += name) += "MuDst";
  dataArray = new TClonesArray(str.Data(),max);
  if (doMc) {
    ((str = "St") += name) += "Mc";
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
void StStrangeControllerBase::InitReadDst() {
  tree = masterMaker->GetTree();
  tree->SetBranchAddress(GetName(),&dataArray);
  if (doMc) {
    TString tName = fName;
    tName += "Mc";
    tree->SetBranchAddress(tName.Data(),&mcArray);
    tName = fName;
    tName += "Assoc";
    tree->SetBranchAddress(tName.Data(),&assocArray);
  }
}
//_____________________________________________________________________________
void StStrangeControllerBase::InitCreateDst(const char* file) {
  tree = masterMaker->GetTree();
  StrangeEnum rw = masterMaker->GetMode();
  Int_t split=2;
  Int_t bsize=64000;
  TBranch* branch = tree->Branch(GetName(),&dataArray,bsize,split);
  if (rw == StrangeWrite) branch->SetFile(file);
  if (doMc) {
    TString tName = fName;
    tName += "Mc";
    branch = tree->Branch(tName.Data(),&mcArray,bsize,split);
    if (rw == StrangeWrite) branch->SetFile(file);
    tName = fName;
    tName += "Assoc";
    branch =tree->Branch(tName.Data(),&assocArray,bsize,split);
    if (rw == StrangeWrite) branch->SetFile(file);
  }
}
//_____________________________________________________________________________
void StStrangeControllerBase::InitCreateSubDst() {
  selections = new TArrayI(max);
  tempArray = GetDstController()->GetDataArray();
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
  PrintNumCand("Finished",nEntries);
}
//_____________________________________________________________________________
void StStrangeControllerBase::Select(Int_t i) {
  if ((!dstMaker) || (selections->At(0) < 0)) return;
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
