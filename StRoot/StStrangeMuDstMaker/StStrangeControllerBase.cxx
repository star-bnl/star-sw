// $Id: StStrangeControllerBase.cxx,v 3.11 2002/05/20 21:37:12 genevb Exp $
// $Log: StStrangeControllerBase.cxx,v $
// Revision 3.11  2002/05/20 21:37:12  genevb
// Fixed problem with file names for branches
//
// Revision 3.10  2002/05/10 20:59:31  genevb
// Fixed bug with branch status and changed cuts split level
//
// Revision 3.9  2002/04/30 16:02:47  genevb
// Common muDst, improved MC code, better kinks, StrangeCuts now a branch
//
// Revision 3.8  2001/11/06 19:45:03  genevb
// Prepare for bug fix in Root 3.02/02
//
// Revision 3.7  2001/11/05 23:41:06  genevb
// Add more dEdx, B field info, careful of changes to TTree unrolling
//
// Revision 3.6  2001/09/14 21:39:02  genevb
// Adjustments to not depend on order in which maker Clear() is called
//
// Revision 3.5  2001/08/23 13:20:53  genevb
// Many bug workarounds...
//
// Revision 3.3  2000/12/18 21:35:18  genevb
// Introduced variable buffer-sizing
//
// Revision 3.2  2000/07/18 15:58:04  genevb
// Increased buffer size
//
// Revision 3.1  2000/07/17 20:28:40  genevb
// File size limitation workaround, some under the hood improvements
//
// Revision 3.0  2000/07/14 12:56:48  genevb
// Revision 3 has event multiplicities and dedx information for vertex tracks
//
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
static int bfirst=1;
static int rootVersion=gROOT->GetVersionInt();
static int doAbstractFix =
  ((rootVersion >= 30000) && (rootVersion < 30202) ? 1 : 0);

ClassImp(StStrangeControllerBase)
//_____________________________________________________________________________
StStrangeControllerBase::StStrangeControllerBase(Int_t type) :
TNamed(strTypeNames[type],"StStrangeController") {
  dstType = type;
  masterMaker = currentMaker;
  doMc = masterMaker->GetDoMc();
  if ((dstMaker = masterMaker->GetSubDst())) {
    if (doMc) dstMaker->DoMc();
    dstMaker->Do(dstType);
  }

  increment = 500;
  max = 1000;
  bsize=1024000;

  tree = 0;
  file = 0;
  selections = 0;
  keepers = 0;
  tempArray = 0;
  mcName = GetName();
  mcName += "Mc";
  assocName = GetName();
  assocName += "Assoc";

  TString str;
  ((str = "St") += GetName()) += "MuDst";
  dataClass = gROOT->GetClass(str.Data());
  (str = "St") += mcName;
  mcClass = gROOT->GetClass(str.Data());
  assocClass = gROOT->GetClass("StStrangeAssoc");

  dataArray = new TClonesArray(dataClass->GetName(),max);
  if (doMc && !(dstMaker)) {
    mcArray = new TClonesArray(mcClass->GetName(),max);
    assocArray = new TClonesArray(assocClass->GetName(),max);
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
}
//_____________________________________________________________________________
StStrangeControllerBase::~StStrangeControllerBase() {
  delete dataArray;  dataArray  = 0;
  delete mcArray;    mcArray    = 0;
  delete assocArray; assocArray = 0;
  delete selections; selections = 0;
  delete keepers;    keepers    = 0;
}
//_____________________________________________________________________________
StStrangeControllerBase* StStrangeControllerBase::Instantiate(Int_t type){
  TString nom = strTypeNames[type];
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
  TString statName;
  (statName = GetName()) += ".*";
  tree->SetBranchStatus(statName.Data(),1);
  tree->SetBranchAddress(GetName(),&dataArray);
  if (doMc) {
    if (!tree->GetBranch(mcName.Data())) {
      gMessMgr->Warning() << IsA()->GetName() <<
                        ": No MC data available, continuing without." << endm;
      doMc = kFALSE;
    } else {
      (statName = mcName) += ".*";
      tree->SetBranchStatus(statName.Data(),1);
      (statName = assocName) += ".*";
      tree->SetBranchStatus(statName.Data(),1);
      tree->SetBranchAddress(mcName.Data(),&mcArray);
      tree->SetBranchAddress(assocName.Data(),&assocArray);
    }
  }
}
//_____________________________________________________________________________
void StStrangeControllerBase::InitCreateDst() {
  tree = masterMaker->GetTree();
//  file = masterMaker->GetFile(dstType);
  file = masterMaker->GetFile(0);
  AssignBranch(GetName(),&dataArray);
  if (doMc && !(dstMaker)) {
    AssignBranch(GetMcName(),&mcArray);
    AssignBranch(GetAssocName(),&assocArray);
  }
}
//_____________________________________________________________________________
void StStrangeControllerBase::InitCreateSubDst() {
  selections = new TArrayI(max);
  keepers = new TArrayS(max);
  StStrangeControllerBase* dstController = GetDstController();
  tempArray = dstController->GetDataArray();
  if (doMc) {
    if (!dstMaker->GetTree()->GetBranch(mcName.Data())) {
      gMessMgr->Warning() << IsA()->GetName() <<
                        ": No MC data available, continuing without." << endm;
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

  // This is a *temporary* workaround for a bug in TBranchElement.
  // In Root 3.00/00-3.02/02, it is unable to handle inherited abstract classes.
  // GVB - Aug. 14, 2001
  // GVB - Nov.  6, 2001
  if (doAbstractFix && bfirst==1) {
    bfirst=0;
    long propl, *prop, pabs = 64;
    TClass* procl;
    
    procl = gROOT->GetClass("StV0I",kTRUE);
    propl = procl->Property();
    prop = (long*) (procl->GetClassInfo());
    if (propl & pabs) prop[1] = propl - pabs;
    
    procl = gROOT->GetClass("StXiI",kTRUE);
    propl = procl->Property();
    prop = (long*) (procl->GetClassInfo());
    if (propl & pabs) prop[1] = propl - pabs;
    
    procl = gROOT->GetClass("StKinkI",kTRUE);
    propl = procl->Property();
    prop = (long*) (procl->GetClassInfo());
    if (propl & pabs) prop[1] = propl - pabs;
  }
  // End of bug workaround.

  static Int_t split=99;
  TBranch* branch = tree->Branch(name,address,bsize,split);
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
  if (doMc && (selections->At(0) >= 0)) {
    for (Int_t j=0;j<GetNAssoc(); j++) {
      StStrangeAssoc* assocEntry = GetAssoc(j);
      Int_t inReAr = assocEntry->indexRecoArray();
      if (keepers->At(inReAr)) {
        Int_t k=0;
        while (selections->At(k) != inReAr) { k++; }
        assocEntry->setIndexRecoArray(k);
      } else {
        assocArray->RemoveAt(j);
      }
    }
    assocArray->Compress();
  }

  Int_t classSize = dataClass->Size();
  // Event info copied directly from dstMaker.
  if (selections->At(0) < 0) {  // Copy all from the event
    tree->SetBranchAddress(GetName(),&tempArray);
  } else if (entries) {         // Copy selected from the event
    Int_t asize = dataArray->GetSize();
    if (entries > asize) dataArray->Expand(entries+increment);
    StStrangeControllerBase* dstController = GetDstController();
    for (Int_t k=0; k<entries; k++) {
//    Replacing the following standard allocation with faster memcpy
//      new((*dataArray)[k]) StV0MuDst(*((StV0MuDst*)
//	    (GetDstController()->Get((*selections)[k]))));
      memcpy((*dataArray)[k],dstController->Get(selections->At(k)),classSize);
    }
  }
  PrintNumCand("copying",entries);
  nEntries += entries;
  
  return kStOK;
}
//_____________________________________________________________________________
void StStrangeControllerBase::Clear(Option_t* opt) {
  if (dstMaker) {                                // Making a subDST
    if (selections) {
      selections->Reset();
      keepers->Reset();
    }
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
    selections->Reset();
    selections->AddAt(i,0);
    keepers->Reset(1);
    entries = ent;
  } else if (i < ent) {
    if (ent > selections->GetSize()) selections->Set(ent+increment);
    if (ent > keepers->GetSize()) keepers->Set(ent);
    if (!(keepers->At(i))) {
      selections->AddAt(i,entries++);
      keepers->AddAt(1,i);
    }
  }
}
//_____________________________________________________________________________
void StStrangeControllerBase::Unselect(Int_t i) {
  if ((!selections) || (!dstMaker) || (!entries)) return;
  if (i<0) {
    entries = 0;
    selections->AddAt(0,0);
    keepers->Reset();
    return;
  }
  Int_t ent = GetDstController()->GetN();
  if (i >= ent) return;
  if (selections->At(0) < 0) {
    // If entire event selected, set to none and select each individual entry
    Unselect(-1);
    for (Int_t entry=0; entry<ent; entry++) {
      if (i != entry) Select(entry);
    }
  } else {
    keepers->AddAt(0,i);
    for (Int_t entry=(entries-1); entry>=0; entry--) {
      if (i == selections->At(entry)) {
        entries--;
	// Cheap and fast step: move last entry to the unselected one
	// Cost: order of selection is lost
        if (entries) selections->AddAt(selections->At(entries),entry);
	return;
      }
    }
  }
}
//_____________________________________________________________________________
void StStrangeControllerBase::PrintNumMc() {
  gMessMgr->Info() << IsA()->GetName() << ": found " <<
        mcEntries << " Monte Carlo " << GetName() << "s" << endm;
  gMessMgr->Info() << IsA()->GetName() << ": found " <<
        assocEntries << " " << GetName() << " associations" << endm;
}
