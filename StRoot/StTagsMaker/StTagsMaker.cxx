// $Id: StTagsMaker.cxx,v 1.5 2000/06/05 22:06:26 vanyashi Exp $
// $Log: StTagsMaker.cxx,v $
// Revision 1.5  2000/06/05 22:06:26  vanyashi
// added const needed for tableDescriptor
//
// Revision 1.4  2000/05/25 15:00:05  vanyashi
// save structure names
//
// Revision 1.3  2000/05/20 01:08:27  vanyashi
// Write tags in split mode
//
// Revision 1.2  2000/02/02 21:20:55  fisyak
// Remove user parametes from GetTags
//
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
    TClass *cl = 0;
    void *address = 0;
    TString branchName;

    while ((set = next())) {
      St_DataSet *ds = GetDataSet(set->GetTitle());
      if (ds) {
	if (ds->InheritsFrom(tabClass)) {
	  St_Table *tabl = (St_Table *) ds;
	  St_Table &tab  = *tabl;
	  address = tab[0];
	  cl = gROOT->GetClass(ds->GetTitle());
	}
	else {
	  if (strstr(ds->GetName(),"EvtHddr")){
	    EvtHddr_st fEvtHddr;
	    StEvtHddr *lEvtHddr = (StEvtHddr *) ds;
	    lEvtHddr->FillTag(&fEvtHddr);
	    address = &fEvtHddr;
	    cl = gROOT->GetClass("EvtHddr_st");
	  }
	}
	if (cl) {
	  St_tableDescriptor td(cl);
	  for (UInt_t i=0;i<td.NumberOfColumns();i++){
	    branchName=ds->GetName();
	    branchName += ".";
	    branchName += td.ColumnName(i);
	    fTree->SetBranchAddress(branchName.Data(),(char*)address+td.Offset(i));
	  }
	}
      }
    }
    fTree->Fill();
  }
  return kStOK;
}
//_____________________________________________________________________________
EDataSetPass StTagsMaker::GetTags (St_DataSet* ds)
{
  St_DataSet *newds = 0;
  TString name, leaflist, branchName;
  TClass *cl = 0;
  Int_t bufsize= 64000;
  void *address = 0;

  const char *Name=ds->GetName();

  if (!tabClass) tabClass  = gROOT->GetClass("St_Table"); 
  if (ds->InheritsFrom(tabClass) && strstr(Name,"Tag")) {
    newds = new St_DataSet(Name);
    name = TString(ds->GetTitle());
    St_Table &tab = *((St_Table *)ds); 
    address = tab[0];
  }
  else if (strstr(Name,"EvtHddr")){   
    newds = new St_DataSet(Name,fTagsList);
    name = TString("EvtHddr_st");
    cl = new TClass(name.Data(),1,"StEvtHddr.h","StEvtHddr.h");
    if (cl) address = cl->New();
  }
  else return kContinue;

  // create separate branch for each tag (or array of tags)
  const Char_t TypeMapTBranch[]="\0FIISDiisbBC";
  cl = gROOT->GetClass(name.Data());
  if (cl) {
    St_tableDescriptor td(cl);
    for (UInt_t i=0;i<td.NumberOfColumns();i++){
      const Char_t *colName = td.ColumnName(i);

      //prepare name/type description for individual tags or tag arrays
      leaflist = colName;
      Int_t nDim = td.Dimensions(i);
      if (nDim) {
	const UInt_t *indx = td.IndexArray(i);
	Char_t buf[64];
	for (Int_t k=0; k<nDim; k++) {
	  sprintf(buf,"[%d]",indx[k]);
	  leaflist += buf;
	}
      }
      leaflist += "/";
      leaflist += TypeMapTBranch[(Int_t)td.ColumnType(i)];

      //save name of the structure (e.g. "FlowTag") in the TTree branchName
      branchName = Name;
      branchName += ".";
      branchName += colName;

      fTree->Branch(branchName.Data(),(char*)address+td.Offset(i),
		    leaflist.Data(),bufsize);
    }
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
      GetChain()->Pass(GetTags);
    }
  }
  return 0;
}








