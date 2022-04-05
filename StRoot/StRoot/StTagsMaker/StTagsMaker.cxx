// $Id: StTagsMaker.cxx,v 1.22 2019/04/23 15:42:11 jeromel Exp $
// $Log: StTagsMaker.cxx,v $
// Revision 1.22  2019/04/23 15:42:11  jeromel
// Sub-version merging to MAIN
//
// Revision 1.21.16.2  2019/02/23 06:02:31  genevb
// Use initial run number consistently
//
// Revision 1.21.16.1  2019/02/05 02:43:36  genevb
// shadow the tags also
//
// Revision 1.21  2009/11/23 16:47:59  fisyak
// Add Primary vertex position errors
//
// Revision 1.20  2008/07/29 14:43:51  fisyak
// Check that StTriggerIdCollection exists
//
// Revision 1.19  2007/04/26 04:15:40  perev
// Remove StBFChain dependency
//
// Revision 1.18  2007/04/17 05:09:41  perev
// GetTFile()==>StMaker. Jerome request
//
// Revision 1.17  2006/10/17 19:18:39  fisyak
// add Check that this chain is BFC one
//
// Revision 1.16  2006/03/24 14:34:50  fisyak
// Remove new TClass(name.Data(),1,StEvtHddr.h,StEvtHddr.h);This new forces to call Notify which cleans up all ROOT interanl pointer and prevent to write into TTRe more than 1 event. This bug found by Victor and Valery
//
// Revision 1.15  2004/11/24 04:05:26  jeromel
// Small leak of one newds fixed
//
// Revision 1.14  2004/08/17 20:52:11  perev
// Replace St_DataSet ==> TDataSet
//
// Revision 1.13  2004/07/30 20:31:38  fisyak
// Remove second i++
//
// Revision 1.12  2004/07/30 20:15:10  fisyak
// Synchronize Pcoll and Global Tags, remove TpcTags fro chain
//
// Revision 1.11  2004/07/30 14:15:10  fisyak
// Back to idl tag definition
//
// Revision 1.10  2004/07/29 22:56:59  fisyak
// Add Global tags
//
// Revision 1.9  2003/05/14 03:30:17  jeromel
// Zombied branch corrected . BugTracking #121
//
// Revision 1.8  2000/07/28 21:05:36  fisyak
// Remove dependence on StChain InitRun
//
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
#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include "TTable.h"
#include "TClass.h"
#include "TDataMember.h"
#include "TDataType.h"
#include "tables/St_GlobalTag_Table.h"
//#include "St_GlobalTag.h"
#include "StEvent.h"
#include "StTriggerData.h"
#include "StEventUtilities/StuRefMult.hh"
#include "StEventUtilities/StuFtpcRefMult.hh"
#include "StShadowMaker/StShadowMaker.h"
static TClass *tabClass = 0;
static TTree  *fTree = 0; //!
static TDataSet *fTagsList =  new TDataSet("TagList");

//TableImpl(GlobalTag);
ClassImp(StTagsMaker); 
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
  St_GlobalTag *tagtab= new St_GlobalTag("GlobalTag",1); AddData(tagtab);
  GlobalTag_st row;
  memset (&row, 0, sizeof(GlobalTag_st));
  StEvent* event = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
  if (event) {
    StPrimaryVertex *primVertex = event->primaryVertex();
    if (primVertex) {//    Primary vertex (x,y,z), with some flag if not found
      row.primaryVertexX =  primVertex->position().x();    // x-vertex
      row.primaryVertexY =  primVertex->position().y();    // y-vertex
      row.primaryVertexZ =  primVertex->position().z();    // z-vertex
      const StMatrixF   &m  = primVertex->covariantMatrix();
      if (m(1,1) > 0) row.sigmaPVX =  TMath::Sqrt(m(1,1));    // x-vertex
      if (m(2,2) > 0) row.sigmaPVY =  TMath::Sqrt(m(2,2));    // y-vertex
      if (m(3,3) > 0) row.sigmaPVZ =  TMath::Sqrt(m(3,3));    // z-vertex
    } else row.primaryVertexFlag = 99;
    StTriggerDetectorCollection *theTriggers = event->triggerDetectorCollection();
    if ( theTriggers){
      StCtbTriggerDetector &theCtb = theTriggers->ctb();
#if 0
      StZdcTriggerDetector &theZdc = theTriggers->zdc();
#endif
      //  Sum all CTB counter
      float ctbsum = 0;
      for (unsigned int islat=0; islat<theCtb.numberOfSlats(); islat++) 
	for (unsigned int itray=0; itray<theCtb.numberOfTrays(); itray++)
	  ctbsum += theCtb.mips(itray, islat, 0);
      row.CTBsum = ctbsum;  //    CTB sum StCtbTriggerDetector::mMips[mMaxTrays][mMaxSlats][0]
    }
    row.uncorrectedNumberOfPrimaries         = uncorrectedNumberOfPrimaries(*event);  
    row.uncorrectedNumberOfFtpcEastPrimaries = uncorrectedNumberOfFtpcEastPrimaries(*event);
    row.uncorrectedNumberOfFtpcWestPrimaries = uncorrectedNumberOfFtpcWestPrimaries(*event);
    StTriggerData *trigData = event->triggerData();
    if (trigData) 
      //      row.zdcWestHardSum = trigData->zdcAttenuated(west); Jamie said that this is wrong
      row.zdcHardSum = trigData->zdcAtChannel(10);
    StTriggerIdCollection *trgcol = event->triggerIdCollection();
    if (trgcol) {
#if 0
      const StTriggerId *l1 = trgcol->l1();
      const StTriggerId *l2 = trgcol->l2();
      const StTriggerId *l3 = trgcol->l3();
#endif
      const StTriggerId *nominal = trgcol->nominal();
      if(nominal) {
	vector<unsigned int> nominalVec = nominal->triggerIds();
	Int_t i = 0;
	for (vector<unsigned int>::iterator viter = nominalVec.begin();
	     viter != nominalVec.end(); ++viter) {
	  row.TriggerId[i++] = (*viter); //    Trigger Id's satisfied by an event
	}
      }
    }
    tagtab->AddAt(&row); if (Debug()) tagtab->Print(0,1);
  }
  if (!fTree) InitTags();
  if (fTree && fTagsList && tabClass) {
    TDataSetIter next(fTagsList);
    TDataSet *set = 0;
    TClass *cl = 0;
    void *address = 0;
    EvtHddr_st fEvtHddr;

    while ((set = next())) {
      TDataSet *ds = GetDataSet(set->GetTitle());

      if (ds) {
	if (ds->InheritsFrom(tabClass)) {
	  TTable *tabl = (TTable *) ds;
	  TTable &tab  = *tabl;
	  address = tab[0];
	  cl = gROOT->GetClass(ds->GetTitle());
	}
	else if (strstr(ds->GetName(),"EvtHddr")){
	  StEvtHddr *lEvtHddr = (StEvtHddr *) ds;
	  lEvtHddr->FillTag(&fEvtHddr);
	  address = &fEvtHddr;
          if (IAttr("shadow")) {
            fEvtHddr.mRunNumber = StShadowMaker::getRunNumber();
            if (fEvtHddr.mOldRunNumber>0)
              fEvtHddr.mOldRunNumber = fEvtHddr.mRunNumber;
            fEvtHddr.mEventNumber = StShadowMaker::getEventNumber(fEvtHddr.mEventNumber);
          }
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
EDataSetPass StTagsMaker::GetTags (TDataSet* ds)
{
  TString name, leaflist, branchName;
  TClass *cl = 0;
  Int_t bufsize= 64000;
  void *address = 0;

  const char *Name=ds->GetName();

  if (!tabClass) tabClass  = gROOT->GetClass("TTable"); 
  if (ds->InheritsFrom(tabClass) && strstr(Name,"Tag")) {
    name = TString(ds->GetTitle());
    TTable &tab = *((TTable *)ds); 
    address = tab[0];

  } else if (strstr(Name,"EvtHddr")){   
    name = TString("EvtHddr_st");
  } else {
    return kContinue;
  }

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
	if (nDim != 0) {
	  //leaflist += '[';
	  //leaflist += Comment;
	  //leaflist += "] ";
	  //}
	  //else {
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
    TDataSet *newds = new TDataSet(Name);
    newds->SetTitle(ds->Path());
    fTagsList->Add(newds); 
  }
  return kContinue; 
}
//_____________________________________________________________________________
Int_t StTagsMaker::InitTags() {
  if (!fTree) {
    StMaker *chain = GetChain();
    TFile *f = chain->GetTFile();
    if (f) {
      f->cd();
      fTree = new TTree("Tag","BFC chain Tags");
      chain->Pass(GetTags);
    }
  }
  return 0;
}








