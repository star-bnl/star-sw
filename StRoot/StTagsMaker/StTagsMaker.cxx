// $Id: StTagsMaker.cxx,v 1.7 2000/06/17 15:41:56 vanyashi Exp $
// $Log: StTagsMaker.cxx,v $
// Revision 1.7  2000/06/17 15:41:56  vanyashi
// Comments from idl files are added to tags
//
// Revision 1.6  2000/06/08 22:21:08  vanyashi
// event header tags fixed
//
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
#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include "TTable.h"
#include "TClass.h"
#include "TDataMember.h"
#include "TDataType.h"

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
    EvtHddr_st fEvtHddr;

    while ((set = next())) {
      St_DataSet *ds = GetDataSet(set->GetTitle());

      if (ds) {
	if (ds->InheritsFrom(tabClass)) {
	  St_Table *tabl = (St_Table *) ds;
	  St_Table &tab  = *tabl;
	  address = tab[0];
	  cl = gROOT->GetClass(ds->GetTitle());
	}
	else if (strstr(ds->GetName(),"EvtHddr")){
	  StEvtHddr *lEvtHddr = (StEvtHddr *) ds;
	  lEvtHddr->FillTag(&fEvtHddr);
	  address = &fEvtHddr;
	  cl = gROOT->GetClass("EvtHddr_st");
	}
	if (cl) {
	  TIter next(cl->GetListOfDataMembers());
	  TDataMember *member = 0;
	  while ( (member = (TDataMember *) next()) ) {
	    TString branchName=ds->GetName();
	    branchName += ".";
	    branchName += member->GetName();
	    fTree->SetBranchAddress(branchName.Data(),
				    (char*)address+member->GetOffset());
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
  newds = new St_DataSet(Name);

  if (!tabClass) tabClass  = gROOT->GetClass("St_Table"); 
  if (ds->InheritsFrom(tabClass) && strstr(Name,"Tag")) {
    name = TString(ds->GetTitle());
    St_Table &tab = *((St_Table *)ds); 
    address = tab[0];
  }
  else if (strstr(Name,"EvtHddr")){   
    name = TString("EvtHddr_st");
    cl = new TClass(name.Data(),1,"StEvtHddr.h","StEvtHddr.h");
    if (cl) address = cl->New();
  }
  else return kContinue;

  // create separate branch for each tag (or array of tags)
  cl = gROOT->GetClass(name.Data());
  if (cl) {
    TIter next(cl->GetListOfDataMembers());
    const Char_t *types;
    TDataMember *member = 0;
    while ( (member = (TDataMember *) next()) ) {
      const Char_t *colName = member->GetName();
      //prepare name/type description for individual tags or tag arrays
      leaflist = colName;
      Int_t nDim = member->GetArrayDim();
      Char_t buf[64];
      for (Int_t k=0; k<nDim; k++) {
	UInt_t indx = member->GetMaxIndex(k);
	sprintf(buf,"[%d]",indx);
	leaflist += buf;
      }
      //in ROOT everything beyond the '[' in leaf name remains as a title
      //see TLeaf::TLeaf(const char *name, const char *):TNamed(name,name) {
      TString Comment = member->GetTitle();
      Comment=Comment.Strip(TString::kBoth,' ');
      //these happen for ROOT Streamer
      Comment.ReplaceAll("!","");
      if (!Comment.IsNull()) {
	//these happen due to html restrictions
	Comment.ReplaceAll("&lt;",'<');
	Comment.ReplaceAll("&gt;",'>');
	//these are not allowed by leaflist restrictions
	Comment.ReplaceAll(":",' ');
	Comment.ReplaceAll("/",' ');
	//have to add '[' for non-arrays
	if (nDim==0) {
	  leaflist += '[';
	  leaflist += Comment;
	  leaflist += "] ";
	}
	else {
	  leaflist += " '";
	  leaflist += Comment;
	  leaflist += "' ";
	}
        //leafname is limited to char[64] -> cut off lengthy comments
	if (leaflist.Length()>64) leaflist.Resize(64);
      }
      //in ROOT everything before the '/' in leaflist is leafname, see
      //TBranch::TBranch(const char *name, void *address, const char *leaflist,...
      leaflist += "/";
      TDataType *memberType = member->GetDataType();
      types = memberType->GetTypeName();
      if (!strcmp("float", types))              leaflist += 'F';
      else if (!strcmp("int", types))           leaflist += 'I';
      else if (!strcmp("long", types))          leaflist += 'I';
      else if (!strcmp("short", types))         leaflist += 'S';
      else if (!strcmp("double", types))        leaflist += 'D';
      else if (!strcmp("unsigned int", types))  leaflist += 'i';
      else if (!strcmp("unsigned long", types)) leaflist += 'i';
      else if (!strcmp("unsigned short", types))leaflist += 's';
      else if (!strcmp("unsigned char", types)) leaflist += 'b';
      //this is used only in EvtHddr->mEventType
      else if (!strcmp("char", types))          leaflist += 'B';

      //save name of the structure (e.g. "FlowTag") in the TTree branchName
      branchName = Name;
      branchName += ".";
      branchName += colName;
      fTree->Branch(branchName.Data(),(char*)address+member->GetOffset(),
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








