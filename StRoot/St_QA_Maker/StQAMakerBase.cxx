// $Id: StQAMakerBase.cxx,v 2.3 2000/08/25 22:06:15 genevb Exp $ 
// $Log: StQAMakerBase.cxx,v $
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

// for method MakeEvSum - from table event_summary
  m_trk_tot_gd = 0;         //! number of good global tracks divided by total
  m_glb_trk_tot=0;          //! # tracks total from globtrk
  m_glb_trk_tot_sm=0;       //! # tracks total from globtrk, small range
  m_glb_trk_plusminus=0;    //! # trks pos/neg. 
  m_glb_trk_plusminus_sm=0; //! # trks pos/neg., small range
  m_glb_trk_chg=0;          //! all charge east/west, tpc
  m_glb_trk_prim=0;         //! # trks from primaries
  m_glb_trk_prim_sm=0;      //! # trks from primaries, small range
  m_vert_total=0;    //! total number of vertices
  m_vert_total_sm=0; //! total number of vertices, small range
  m_mean_pt=0;       //! mean pt value
  m_mean_pt_sm=0;    //! mean pt value, small range
  m_mean_eta=0;      //! mean eta value 
  m_rms_eta=0;       //! rms eta value 
  m_prim_vrtr=0;     //! primary vrtx r position
  m_prim_vrtx0=0;    //! primary vrtx x position
  m_prim_vrtx1=0;    //! primary vrtx y position
  m_prim_vrtx2=0;    //! primary vrtx z position


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
    default  : {}
  }

  // Call methods to fill histograms

  // histograms from table event_summary
  MakeHistEvSum();
  // histograms from table globtrk
  MakeHistGlob();
  // histograms from table dst_dedx
  MakeHistDE();
  // histograms from table primtrk
  MakeHistPrim();
  // histograms from table particle
  MakeHistGen();  
  // histograms from table primtrk & dst_dedx
  MakeHistPID();
  // histograms from table dst_vertex,dst_v0_vertex,dst_xi_vertex,dst_kinkVertex
  MakeHistVertex();
  // histograms from table point
  MakeHistPoint();
  // histograms from table g2t_rch_hit
  MakeHistRich();
  // histograms from geant and reco tables 
  MakeHistEval();

  return kStOk;
}
//_____________________________________________________________________________
void StQAMakerBase::NewQABookHist(const char* prefix) {  

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

    default  : {}
  }
  
  multClass = histsList->GetSize();
  QAH::maker = (StMaker*) (this);
  QAH::preString = QAMakerType;

  BookHistGeneral();
  BookHistEvSum();
  for (Int_t i=0; i<multClass; i++)
    ((StQABookHist*) (histsList->At(i)))->BookHist();
}
//_____________________________________________________________________________
void StQAMakerBase::BookHistGeneral(){  

  mNullPrimVtx = QAH::H1F("QaNullPrimVtx","event primary vertex check",40,-2,2);
  mNullPrimVtx->SetXTitle("has primary vertex? (yes = 1, no = -1)");
  mNullPrimVtx->SetYTitle("# of events");

  if (histsSet != 0) {
    mMultClass = QAH::H1F("QaMultClass","event multiplicity class",5,-0.5,4.5);
    mMultClass->SetXTitle("mult class (0=?/MC, 1=LM, 2=MM, 3=HM)");
    mMultClass->SetYTitle("# of events");
  }
}
//_____________________________________________________________________________
void StQAMakerBase::BookHistEvSum(){  

// for method MakeEvSum - from table event_summary

  m_trk_tot_gd = MH1F("QaEvsumTrkGoodDTotal",
    "evsum: num good global tracks over total",55,0.,1.1);
  m_trk_tot_gd->SetXTitle("number of good/total tracks");
  m_glb_trk_tot = MH1F("QaEvsumTrkTot",
    "evsum: num tracks total ",ntrk,0.,10000.);
  m_glb_trk_tot_sm = MH1F("QaEvsumTrkTotsm",
    "evsum: num tracks total ",ntrk,0.,20.);
  m_glb_trk_plusminus = MH1F("QaEvsumPlusMinusTrk",
    "evsum: num pos. over neg trks",ntrk,0.8,1.4);
  m_glb_trk_plusminus_sm = MH1F("QaEvsumPlusMinusTrksm",
    "evsum: num pos. over neg trks",ntrk,0.,4.);
  m_glb_trk_chg = MH1F("QaEvsumTotChg",
    "softmon: all charge east/west,tpc",60,0,3);
  m_glb_trk_prim = MH1F("QaEvsumTrkPrim",
    "evsum: num good tracks from primaries ",80,0.,4000.);
  m_glb_trk_prim_sm = MH1F("QaEvsumTrkPrimsm",
    "evsum: num good tracks from primaries ",80,0.,20.);
  m_vert_total = MH1F("QaEvsumVertTot",
    "evsum: total num of vertices",80,0.,2000.);
  m_vert_total_sm = MH1F("QaEvsumVertTotsm",
    "evsum: total num of vertices",80,0.,20.);
  m_mean_pt = MH1F("QaEvsumMeanPt","evsum: mean pt", nmnpt,0.,2.0);
  m_mean_pt_sm = MH1F("QaEvsumMeanPtsm","evsum: mean pt", nmnpt,0.,0.5);
  m_mean_eta = MH1F("QaEvsumMeanEta","evsum: mean eta", nmneta,-0.4,0.4);
  m_rms_eta = MH1F("QaEvsumRmsEta","evsum: rms eta", nmneta,0.,2.5);
  m_prim_vrtr = MH1F("QaEvsumPrimVertR",
    "evsum: R of primary vertex",40,0.,1.);
  m_prim_vrtx0 = MH1F("QaEvsumPrimVertX",
    "evsum: X of primary vertex",40,-1.,1.);
  m_prim_vrtx1 = MH1F("QaEvsumPrimVertY",
    "evsum: Y of primary vertex",40,-1.,1.);
  m_prim_vrtx2 = MH1F("QaEvsumPrimVertZ",
    "evsum: Z of primary vertex",nxyz,-50.,50.);
}
//_____________________________________________________________________________
