// $Id: StTagsMaker.cxx,v 1.1.1.1 2000/01/27 18:54:00 fisyak Exp $
// $Log: StTagsMaker.cxx,v $
// Revision 1.1.1.1  2000/01/27 18:54:00  fisyak
// Initial revision of Tags Maker
//

#include "StTagsMaker.h"
#include "StEvtHddr.h"
#include "StBFChain.h"
#include "tables/St_HighPtTag_Table.h"
#include "tables/St_FlowTag_Table.h"
#include "tables/St_HbtTag_Table.h"
#include "tables/St_PCollTag_Table.h"
#include "tables/St_ScaTag_Table.h"
#include "tables/St_StrangeTag_Table.h"
#include "StBFChain.h"
#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include "TClass.h"
#include "St_tableDescriptor.h"

static TClass *tabClass = 0;
static TTree  *fTree = 0; //!
static St_DataSet *fTagsList =  new St_DataSet("TagList");

ClassImp(StTagsMaker)
//_____________________________________________________________________________
StTagsMaker::StTagsMaker(const char *name):StMaker(name){
}
//_____________________________________________________________________________
StTagsMaker::~StTagsMaker(){
}
//_____________________________________________________________________________
Int_t StTagsMaker::Init(){
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StTagsMaker::Make(){
  if (fTree && fTagsList && tabClass) {
    St_DataSetIter next(fTagsList);
    St_DataSet *set = 0;
    while ((set = next())) {
      St_DataSet *ds = GetDataSet(set->GetTitle());
      if (ds) {
	if (ds->InheritsFrom(tabClass)) {
	  St_Table *tabl = (St_Table *) ds;
	  St_Table &tab  = *tabl;
	  void *address = tab[0];
	  fTree->SetBranchAddress(set->GetName(),address);
	}
	else {
	  if (strstr(ds->GetName(),"EvtHddr")){
	    EvtHddr_st fEvtHddr;
	    StEvtHddr *lEvtHddr = (StEvtHddr *) ds;
	    lEvtHddr->FillTag(&fEvtHddr);
	    fTree->SetBranchAddress("EvtHddr",&fEvtHddr);                  
	  }
	}
      }
    }
    fTree->Fill();
  }
  return kStOK;
}
//_____________________________________________________________________________
EDataSetPass StTagsMaker::GetTags (St_DataSet* ds,void * )
{
  Int_t bufsize= 64000;
  St_DataSet *newds = 0;
  TString type, name, leaflist;
  void *address = 0;
  if (!tabClass) tabClass  = gROOT->GetClass("St_Table"); 
  if (ds->InheritsFrom(tabClass) && strstr(ds->GetName(),"Tag")) {
    newds = new St_DataSet(ds->GetName());
    type = TString(ds->GetTitle());
    name = type;
    name.Append("_st");
    type.Append(".h");
    TClass CL(name.Data(),1,type.Data(),type.Data());
    TClass *cl = gROOT->GetClass(name.Data());
    if (cl) {
      St_tableDescriptor td(cl);
      leaflist = td.CreateLeafList();
      St_Table &tab = *((St_Table *)ds); 
      address = tab[0];
    }
  }
  else if (strstr(ds->GetName(),"EvtHddr")){   
    newds = new St_DataSet(ds->GetName(),fTagsList);
    type = TString("StEvtHddr.h");
    name = TString("EvtHddr_st");
    TClass CL(name.Data(),1,type.Data(),type.Data());
    TClass *cl = gROOT->GetClass(name.Data());
    if (cl) {
      St_tableDescriptor td(cl);
      leaflist = td.CreateLeafList();
      address = cl->New();
    }
  }
  else return kContinue;
  if (address) {
    fTree->Branch(ds->GetName(),address, leaflist.Data(), bufsize);
    newds->SetTitle(ds->Path());
    fTagsList->Add(newds); 
  }
  else SafeDelete(newds);
  return kContinue; 
}
//_____________________________________________________________________________
Int_t StTagsMaker::InitRun(int runumber) {
  if (!fTree) {
    TFile *f = ((StBFChain* )GetChain())->GetTFile();
    if (f) {
      f->cd();
      fTree = new TTree("Tag","BFC chain Tags");
      GetChain()->Pass(GetTags,0);
    }
  }
  return 0;
}








