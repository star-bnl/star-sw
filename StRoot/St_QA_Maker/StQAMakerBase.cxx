// $Id: StQAMakerBase.cxx,v 2.18 2002/04/23 01:59:56 genevb Exp $ 
// $Log: StQAMakerBase.cxx,v $
// Revision 2.18  2002/04/23 01:59:56  genevb
// Addition of BBC/FPD histos
//
// Revision 2.17  2002/03/11 20:59:58  genevb
// Fixed bug with placement of trigger hists
//
// Revision 2.16  2002/03/01 22:51:20  genevb
// Small set-to-zero possible bug fix
//
// Revision 2.15  2002/02/12 18:42:00  genevb
// Additional FTPC histograms
//
// Revision 2.14  2002/01/26 03:04:07  genevb
// Fixed some problems with fcl histos
//
// Revision 2.13  2002/01/21 22:09:24  genevb
// Include some ftpc histograms from StFtpcClusterMaker
//
// Revision 2.12  2001/12/28 09:19:13  genevb
// Adjustments for pp running
//
// Revision 2.11  2001/12/20 03:11:08  genevb
// pp trigger words 0x2XXX
//
// Revision 2.10  2001/09/10 18:00:13  genevb
// Another trigger word
//
// Revision 2.9  2001/08/29 20:45:15  genevb
// Trigger word histos
//
// Revision 2.8  2001/08/07 07:51:28  lansdell
// primvtx check for different multiplicities crashed for MC data, now fixed
//
// Revision 2.7  2001/05/23 00:14:53  lansdell
// more changes for qa_shift histograms
//
// Revision 2.6  2001/05/16 20:57:03  lansdell
// new histograms added for qa_shift printlist; some histogram ranges changed; StMcEvent now used in StEventQA
//
// Revision 2.5  2001/04/28 22:05:13  genevb
// Added EMC histograms
//
// Revision 2.4  2001/04/26 16:34:40  genevb
// Fixed some histogram ranges
//
// Revision 2.3  2000/08/25 22:06:15  genevb
// Raised high mult bin to 2500 global tracks
//
// Revision 2.2  2000/08/25 20:29:34  lansdell
// year1 print list changed slightly; cosmetic improvement of some event summary histograms
//
// Revision 2.1  2000/08/25 16:04:10  genevb
// Introduction of files
//
// 
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  StQAMakerBase base class for QA Histogram Makers                     //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#include "StMessMgr.h"
#include "StQAMakerBase.h"
#include "StQABookHist.h"
#include "QAH.h"
#include "TList.h"

ClassImp(StQAMakerBase)

//_____________________________________________________________________________
StQAMakerBase::StQAMakerBase(const char *name, const char *title, const char* type) :
 StMaker(name,title), QAMakerType(type) {

  hists = 0;
  histsList = new TList();
  histsSet = 0;
  multClass = 3;

//  - Set all the histogram booking constants

  ntrk   = 50;
  nmnpt  = 50;
  nmneta = 50;
  nxyz   = 50;

//  - Zero all histogram pointers
  mNullPrimVtx = 0; // histogram for number of events without primary vertex
  mMultClass = 0;   // histogram for number of events in mult classes
  mTrigWord = 0;    // histogram for event trigger words
  mTrigBits = 0;    // histogram for event trigger bits

// for method MakeEvSum - from table software monitor
  m_glb_trk_chg=0;          //! all charge east/west, tpc
  m_glb_trk_chgF=0;         //! all charge east/west, ftpc

}
//_____________________________________________________________________________
StQAMakerBase::~StQAMakerBase() {
  delete histsList;
}
//_____________________________________________________________________________
Int_t StQAMakerBase::Init(){
// Histogram booking must wait until first event Make() to determine event type
  firstEvent = kTRUE;
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StQAMakerBase::Make(){

  if (Debug())
    gMessMgr->Info(" In StQAMakerBase::Make()");

  if (firstEvent) BookHist();
  // See BookHist() for definitions of histsSet values,
  // which should be set during Make() of the derived QA Maker class
  switch (histsSet) {
    case (0) : {
      multClass = 0;
      hists = (StQABookHist*) histsList->At(0);
      break; }
    case (1) : {
      if (multiplicity < 50) multClass = 0;
      else if (multiplicity < 500) multClass = 1;
      else if (multiplicity < 2500) multClass = 2;
      else multClass = 3;

      mMultClass->Fill(multClass);
      if (!multClass) return kStOk;
      hists = (StQABookHist*) histsList->At((Int_t) (--multClass));
      break; }
    case (2) : {
      multClass = 0;
      hists = (StQABookHist*) histsList->At(0);
      break; }
    default  : {}
  }

  // Call methods to fill histograms


  // histograms from table event_summary
  MakeHistEvSum();
  // histograms from table globtrk
  MakeHistGlob();
  // histograms from table primtrk - must be done after global tracks
  MakeHistPrim();
  // histograms from table primtrk & dst_dedx
  MakeHistPID();
  // histograms from table dst_dedx
  MakeHistDE();
  // histograms from table point
  MakeHistPoint();
  // histograms from table g2t_rch_hit
  MakeHistRich();
  // histograms from table dst_vertex,dst_v0_vertex,dst_xi_vertex,dst_kinkVertex
  MakeHistVertex();
  // histograms from EMC in StEvent
  MakeHistEMC();
  // histograms from geant and reco tables 
  if (histsSet==0) MakeHistEval();
  // histograms from BBC in StEvent
  MakeHistBBC();
  // histograms from FPD in StEvent
  MakeHistFPD();

  return kStOk;
}
//_____________________________________________________________________________
void StQAMakerBase::NewQABookHist(const char* prefix) {  

  if (Debug())
    gMessMgr->Info() <<
      "StQAMakerBase: booking histograms with prefix: " << prefix << endm;
  hists = new StQABookHist(prefix);
  histsList->Add(hists);
}
//_____________________________________________________________________________
TH2F* StQAMakerBase::MH1F(const Text_t* name, const Text_t* title,
                          Int_t nbinsx, Axis_t xlow, Axis_t xup) {
  TH2F* h = QAH::MH1F(name,title,nbinsx,xlow,xup,(Int_t) multClass);
  if (multClass>1) {
    h->Rebin(0,"low mult");
    h->Rebin(1,"mid mult");
    h->Rebin(2,"high mult");
  }
  return h;
}
//_____________________________________________________________________________
void StQAMakerBase::BookHist() {  
// book histograms

  firstEvent = kFALSE;
  TString temp;

  switch (histsSet) {

    // Generic data (e.g. Monte Carlo) with just one multiplicity class
    case (0) : {
      NewQABookHist(QAMakerType.Data());
      break; }

    // Real data with three multiplicity classes (low, medium, high)
    case (1) : {
      (temp = QAMakerType) += "LM";
      NewQABookHist(temp.Data());
      (temp = QAMakerType) += "MM";
      NewQABookHist(temp.Data());
      (temp = QAMakerType) += "HM";
      NewQABookHist(temp.Data());
      break; }

    // pp data with just one multiplicity class
    case (2) : {
      NewQABookHist(QAMakerType.Data());
      break; }

    default  : {}
  }
  
  multClass = histsList->GetSize();
  QAH::maker = (StMaker*) (this);
  QAH::preString = QAMakerType;

  BookHistTrigger();
  BookHistGeneral();
  BookHistEvSum();
  BookHistFcl();
  for (Int_t i=0; i<multClass; i++)
    ((StQABookHist*) (histsList->At(i)))->BookHist(histsSet);
}
//_____________________________________________________________________________
void StQAMakerBase::BookHistGeneral(){  

  mNullPrimVtx = QAH::H1F("QaNullPrimVtx","event primary vertex check",40,-2,2);
  mNullPrimVtx->SetXTitle("has primary vertex? (yes = 1, no = -1)");
  mNullPrimVtx->SetYTitle("# of events");

  if (histsSet == 1) {
    mMultClass = QAH::H1F("QaMultClass","event multiplicity class",5,-0.5,4.5);
    mMultClass->SetXTitle("mult class (0=?/MC, 1=LM, 2=MM, 3=HM)");
    mMultClass->SetYTitle("# of events");
  }
}
//_____________________________________________________________________________
void StQAMakerBase::BookHistTrigger(){  

  QAH::maker = (StMaker*) (this);
  QAH::preString = QAMakerType;
  if (mTrigWord) return;
  mTrigWord = QAH::H1F("QaTrigWord","trigger word",8,0.5,8.5);
  mTrigWord->SetXTitle("1=MinBias, 2=Central, 3=Other Physics, 4=pp, 7=Laser, 8=Other");
  mTrigBits = QAH::H1F("QaTrigBits","trigger bits",32,-0.5,31.5);
}
//_____________________________________________________________________________
void StQAMakerBase::BookHistEvSum(){  

// for method MakeEvSum - from software monitor

  m_glb_trk_chg = MH1F("QaEvsumTotChg","softmon: all charge east/west,tpc",60,0,3);
  m_glb_trk_chgF = MH1F("QaEvsumTotChgF","softmon: all charge east/west,ftpc",60,0,3);
}
//_____________________________________________________________________________
void StQAMakerBase::BookHistFcl(){  

  // Get fcl histograms from FTPC makers
  if (!(hists->m_ftpc_chargestepW)) {
    // First try to get histograms from StFtpcClusterMaker named "ftpc_hits"
    StMaker* fhMaker = GetMaker("ftpc_hits");
    if (fhMaker) {
      hists->m_ftpc_chargestepW = (TH1F*) (fhMaker->GetHist("fcl_chargestepW"));
      AddHist(hists->m_ftpc_chargestepW);
      hists->m_ftpc_chargestepE = (TH1F*) (fhMaker->GetHist("fcl_chargestepE"));
      AddHist(hists->m_ftpc_chargestepE);
      hists->m_ftpc_fcl_radialW = (TH1F*) (fhMaker->GetHist("fcl_radialW"));
      AddHist(hists->m_ftpc_fcl_radialW);
      hists->m_ftpc_fcl_radialE = (TH1F*) (fhMaker->GetHist("fcl_radialE"));
      AddHist(hists->m_ftpc_fcl_radialE);
    } else {
      // "ftpc_hits" maker doesn't exist, so look in hist branch
      // *** Currently isn't working for bfcread_event_QAhist.C ***
      St_DataSet* hDS = GetDataSet("histBranch");
      if (hDS) {
        // hDS->ls(9);
        St_DataSet* fhDS = hDS->Find("ftpc_hitsHist");
        if (fhDS) {
          hists->m_ftpc_chargestepW =
	    (TH1F*) (fhDS->FindObject("fcl_chargestepW"));
          AddHist(hists->m_ftpc_chargestepW);
          hists->m_ftpc_chargestepE =
	    (TH1F*) (fhDS->FindObject("fcl_chargestepE"));
          AddHist(hists->m_ftpc_chargestepE);
          hists->m_ftpc_fcl_radialW =
	    (TH1F*) (fhDS->FindObject("fcl_radialW"));
          AddHist(hists->m_ftpc_fcl_radialW);
          hists->m_ftpc_fcl_radialE =
	    (TH1F*) (fhDS->FindObject("fcl_radialE"));
          AddHist(hists->m_ftpc_fcl_radialE);
        }
      }
    }
  }
}
//_____________________________________________________________________________
