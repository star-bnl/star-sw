// $Id: StQAMakerBase.cxx,v 2.55 2019/12/17 19:08:01 genevb Exp $ 
// $Log: StQAMakerBase.cxx,v $
// Revision 2.55  2019/12/17 19:08:01  genevb
// Add more ETOF histograms
//
// Revision 2.54  2019/05/22 21:24:31  genevb
// Add sDCA vs. time-in-run
//
// Revision 2.53  2019/03/26 15:29:38  genevb
// Introduce ETOF
//
// Revision 2.52  2019/03/14 02:31:53  genevb
// Introduce iTPC plots
//
// Revision 2.51  2019/03/01 19:40:38  genevb
// Some minor Run 19 preparations, including first padrow hit
//
// Revision 2.50  2018/07/03 21:33:34  genevb
// Introduce EPD (code provided by J. Ewigleben)
//
// Revision 2.49  2018/05/02 21:07:40  genevb
// Initial accomodation for iTPC
//
// Revision 2.48  2017/02/25 03:24:30  genevb
// Run 17: remove HFT
//
// Revision 2.47  2016/05/13 22:04:49  genevb
// Address coverity findings: uninit vars, dead code, one PMD error, and one TOF error
//
// Revision 2.46  2015/07/17 19:09:03  genevb
// SSD copied for SST, and HFT histogams use SST now too
//
// Revision 2.45  2015/03/18 21:43:17  genevb
// Introduce Roman Pots histograms (K. Yip)
//
// Revision 2.44  2015/01/21 17:49:40  genevb
// Fix missing run14 cases, remove unused firstEventClass, re-work normalizations with StHistUtil
//
// Revision 2.43  2015/01/17 23:16:12  genevb
// protection from missing data/histograms
//
// Revision 2.42  2015/01/16 21:08:28  genevb
// Initial versions of HFT histograms
//
// Revision 2.41  2014/07/22 20:39:28  genevb
// Add MTD to Offline QA
//
// Revision 2.40  2014/01/30 19:44:06  genevb
// Additional TPC histogram for monitoring gas contamination
//
// Revision 2.39  2013/03/12 03:06:02  genevb
// Add FMS/FPD histograms for Run 13+
//
// Revision 2.38  2012/03/05 03:42:32  genevb
// Remove TPC XY dist, add TPC RPhi charge
//
// Revision 2.37  2012/02/08 22:10:35  genevb
// Updates for Run 12
//
// Revision 2.36  2011/05/26 19:59:38  genevb
// Cleanup in destructors
//
// Revision 2.35  2009/11/19 20:34:38  genevb
// Remove Event Summary (using defunct old software monitors)
//
// Revision 2.34  2009/03/27 21:18:36  genevb
// Add Jet Patch trigger histograms
//
// Revision 2.33  2007/12/12 19:50:55  genevb
// Update for trigger words
//
// Revision 2.32  2007/11/30 05:38:50  genevb
// Changes for Run8: mostly silicon removal, TOF addition
//
// Revision 2.31  2007/04/24 00:33:38  genevb
// Always do PMD hists now
//
// Revision 2.30  2005/02/22 19:38:39  genevb
// Do PMD hists only for year 4 and later (real data)
//
// Revision 2.29  2005/02/08 17:22:46  genevb
// PMD histo changes, handle estGlobal/ITTF tracks
//
// Revision 2.28  2004/12/13 15:52:37  genevb
// Numerous updates: PMD, primtrk, FPD, QAShift lists
//
// Revision 2.27  2004/02/12 16:54:24  genevb
// Separate MinBias histos
//
// Revision 2.26  2004/02/12 05:03:14  genevb
// Year 4 AuAu changes. New SVT histos.
//
// Revision 2.25  2004/01/10 01:10:18  genevb
// Preparations for Year 5, added some svt plots
//
// Revision 2.24  2003/04/19 00:17:50  genevb
// Updated for dAu/pp running
//
// Revision 2.23  2003/02/28 16:01:09  genevb
// Further improvements for previous check-in
//
// Revision 2.22  2003/02/28 06:17:56  genevb
// Allow StQAMakerBase::Make to be called for all events
//
// Revision 2.21  2003/02/20 20:09:54  genevb
// Several changes for new trigger scheme, dAu data
//
// Revision 2.20  2003/02/19 06:38:29  genevb
// Rework trigger and mult/event class sections
//
// Revision 2.19  2003/02/15 22:00:52  genevb
// Add tpcSectors, fix ftpc east/west charge
//
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
#include "StDetectorDbMaker/St_tpcPadConfigC.h"

ClassImp(StQAMakerBase)

//_____________________________________________________________________________
StQAMakerBase::StQAMakerBase(const char *name, const char *title, const char* type) :
 StMaker(name,title), QAMakerType(type) {

  hists = 0;
  histsList.Expand(32);
  histsList.SetOwner();
  Int_t i;
  for (i=0;i<32;i++) histsList.AddAt(0,i);
  histsSet = StQA_Undef;
  fillHists = kFALSE;
  eventCount = 0;
  eventClass = 3;
  ITTF = kFALSE;
  EST = -1; // -1 = unknown
  allTrigs = kFALSE;

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
  for (i=0; i<24; i++) mTpcSectorPlot[i] = 0;
  m_pnt_rpTQW = 0;
  m_pnt_rpTQE = 0;

// FTPC histograms
  m_ftpc_chargestepW=0; //! Chargestep from ftpc west
  m_ftpc_chargestepE=0; //! Chargestep from ftpc east
  m_ftpc_fcl_radialW=0;  //! ftpc west cluster radial position
  m_ftpc_fcl_radialE=0;  //! ftpc east cluster radial position

// TPC dE/dx over time
  m_dedx_Z3A=0; // dE/dx vs. drift distance

// signed DCA (impact parameter) over time
  m_glb_simpactTime=0; 

// Roman-Pot histograms
//  m_RP_ClusterLength=0; // testing
  for (i=0; i<kRP_MAXSEQ; i++) m_RP_clusters_xy[i] = 0 ;

// ETOF histograms
  for (i=0; i<10; i++) m_etofHist[i] = 0;

}
//_____________________________________________________________________________
StQAMakerBase::~StQAMakerBase() {
  if (mNullPrimVtx) delete mNullPrimVtx;
  if (mMultClass) delete mMultClass;
  if (mTrigWord) delete mTrigWord;
  if (mTrigBits) delete mTrigBits;
  for (Int_t i=0; i<24; i++) { if (mTpcSectorPlot[i]) delete mTpcSectorPlot[i]; };
  if (m_pnt_rpTQW) delete m_pnt_rpTQW;
  if (m_pnt_rpTQE) delete m_pnt_rpTQE;
}
//_____________________________________________________________________________
Int_t StQAMakerBase::Init() {
// Histogram booking must wait until first event Make() to determine event type
  eventCount = 0;
  return StMaker::Init();
}
//_____________________________________________________________________________
void StQAMakerBase::Clear(Option_t* opt) {
  StMaker::Clear(opt);
}
//_____________________________________________________________________________
Int_t StQAMakerBase::Make() {

  if (Debug())
    gMessMgr->Info(" In StQAMakerBase::Make()");

  if (!mNullPrimVtx) BookHist();
  // See BookHist() for definitions of histsSet values,
  // which should be set during Make() of the derived QA Maker class
  // event class also decided in derived Make()
  switch (histsSet) {
    case (StQA_AuAuOld) :
      mMultClass->Fill((float) eventClass);
      break;
    default : {}
  }
  if (!eventClass) { hists=0; return kStOk; }
  hists = (StQABookHist*) histsList.At((--eventClass));
  if (!hists) NewQABookHist();

    

  if (!fillHists) return kStOk;
  // Call methods to fill histograms

  // Those divided by event class:
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
  // histograms from table dst_vertex,dst_v0_vertex,dst_xi_vertex,dst_kinkVertex
  MakeHistVertex();
  // histograms from EMC in StEvent
  MakeHistEMC();
  // histograms from geant and reco tables 
  if (histsSet==StQA_MC) MakeHistEval();
  // histograms from BBC in StEvent
  if (histsSet==StQA_AuAuOld) MakeHistBBC();
  // histograms from FPD in StEvent
  if (histsSet==StQA_AuAuOld) MakeHistFPD();
  // histograms from PMD in StEvent
  if (histsSet<StQA_run12all) MakeHistPMD();
  // histograms from TOF in StEvent
  if (histsSet>=StQA_run8) MakeHistTOF(); 
  // histograms from FMS in StEvent
  if (histsSet>=StQA_run13) MakeHistFMS(); 
  // histograms from MTD in StEvent
  if (histsSet>=StQA_run12all) MakeHistMTD(); 
  // histograms from HFT (PXL, IST, SST) in StEvent
  if (histsSet>=StQA_run14 && histsSet<StQA_run17) {
    MakeHistHFT(); 
    MakeHistPXL(); 
    MakeHistIST(); 
    MakeHistSST(); 
  }
  // histograms from Roman-Pot in StEvent
  if (histsSet>=StQA_run15) MakeHistRP();
  // histograms from EPD in StEvent
  if (histsSet>=StQA_run18) {
    MakeHistEPD();
    MakeHistiTPC();
  }

  eventCount++;
  return kStOk;
}
//_____________________________________________________________________________
void StQAMakerBase::NewQABookHist() {
  const char* pre = prefix[eventClass].Data();
  if (Debug())
    gMessMgr->Info() <<
      "StQAMakerBase: booking histograms with prefix: " <<
      pre << endm;
  QAH::maker = (StMaker*) (this);
  hists = new StQABookHist(pre);
  histsList.AddAt(hists,eventClass);
  hists->BookHist(histsSet);
}
//_____________________________________________________________________________
TH2F* StQAMakerBase::MH1F(const Text_t* name, const Text_t* title,
                          Int_t nbinsx, Axis_t xlow, Axis_t xup) {
  TH2F* h = QAH::MH1F(name,title,nbinsx,xlow,xup,eventClass);
  if (eventClass>1) {
    h->Rebin(0,"low mult");
    h->Rebin(1,"mid mult");
    h->Rebin(2,"high mult");
  }
  return h;
}
//_____________________________________________________________________________
void StQAMakerBase::BookHist() {  
// book histograms

  Int_t tempClass = eventClass;

  if (allTrigs) {
    prefix[0] = QAMakerType;
    eventClass = 1;
  } else {
  switch (histsSet) {

  // Real data with three multiplicity classes (low, medium, high)

    case (StQA_AuAuOld) : {
      (prefix[0] = QAMakerType) += "LM";
      (prefix[1] = QAMakerType) += "MM";
      (prefix[2] = QAMakerType) += "HM";
      eventClass = 3;
      break; }

  // Real data with event classes for different triggers

    // any new StQAHistSetType values
    case (StQA_run19) :
    case (StQA_run18) :
    case (StQA_run17) :
    case (StQA_run15) :
    case (StQA_run14) :
    case (StQA_run13) :
    case (StQA_run12) :
    case (StQA_run8) :
    case (StQA_AuAu) : {
      (prefix[0] = QAMakerType) += "MB";  // Minbias
      (prefix[1] = QAMakerType) += "CL";  // Central
      (prefix[2] = QAMakerType) += "HT";  // HighTower
      (prefix[3] = QAMakerType) += "XX";  // OtherPhysics
      (prefix[4] = QAMakerType) += "JP";  // JetPatch
      eventClass = 5;
      break; }

    case (StQA_dAu) : {
      prefix[0] = QAMakerType;  // Minbias
      (prefix[1] = QAMakerType) += "HP";
      (prefix[2] = QAMakerType) += "XX";
      eventClass = 3;
      break; }

  // the following data sets use the defaults
 
   // Generic data (e.g. Monte Carlo) with just one event class
    case (StQA_MC) :

   // pp data with just one event class
    case (StQA_pp) :
    case (StQA_run12all) :

    default  : {
      prefix[0] = QAMakerType;
      eventClass = 1;
    }
  }
  }
  
  QAH::maker = (StMaker*) (this);
  QAH::preString = QAMakerType;

  BookHistTrigger();
  BookHistGeneral();
  BookHistDE();
  BookHistFcl();
  if (histsSet>=StQA_run13) BookHistFMS(); 
  if (histsSet>=StQA_run15) BookHistRP();
  if (histsSet>=StQA_run19) BookHistETOF();

  Int_t tempClass2 = eventClass;
  // Must book the histograms with no special prefix now
  for (eventClass=0; eventClass<tempClass2; eventClass++) {
    if (!(QAMakerType.CompareTo(prefix[eventClass]))) {
      NewQABookHist();
      break;
    }
  }
  eventClass = tempClass;
  
}
//_____________________________________________________________________________
void StQAMakerBase::BookHistGeneral(){  

  mNullPrimVtx = QAH::H1F("QaNullPrimVtx","event primary vertex check",40,-2,2);
  mNullPrimVtx->SetXTitle("has primary vertex? (yes = 1, no = -1)");
  mNullPrimVtx->SetYTitle("# of events");

  if (histsSet == StQA_AuAuOld) {
    mMultClass = QAH::H1F("QaMultClass","event multiplicity class",5,-0.5,4.5);
    mMultClass->SetXTitle("mult class (0=?/MC, 1=LM, 2=MM, 3=HM)");
    mMultClass->SetYTitle("# of events");
  }

  char namebuf[32];
  char titlebuf[64];
  for (Int_t i=0; i<24; i++) {
    int numOfRows = St_tpcPadConfigC::instance()->numberOfRows(i+1);
    sprintf(namebuf ,"Qa%sTpcSector%d",(numOfRows > 45 ? "i" : ""),i+1);
    sprintf(titlebuf,"Hits in %sTPC Sector %d",(numOfRows > 45 ? "i" : ""),i+1);
    //mTpcSectorPlot[i] = QAH::H2F(namebuf,titlebuf,104,-52.,52.,150,50.,200.);
    mTpcSectorPlot[i] = QAH::H2F(namebuf,titlebuf,104,-52.,52.,numOfRows+2,-0.5,((float) numOfRows)+1.5);
    mTpcSectorPlot[i]->SetXTitle("along padrows [cm]");
    //mTpcSectorPlot[i]->SetYTitle("across padrows [cm]");
    mTpcSectorPlot[i]->SetYTitle("padrow");
  }
  m_pnt_rpTQW    = QAH::H2F("QaPointRPTpcQW","point: r-phi distribution of charge, tpcW",20,58.75,196.75,72,0,TMath::TwoPi());
  m_pnt_rpTQE    = QAH::H2F("QaPointRPTpcQE","point: r-phi distribution of charge, tpcE",20,58.75,196.75,72,0,TMath::TwoPi());

  m_glb_simpactTime = QAH::H2F("QaGtrkSImpactTime","globtrk: signed impact param from prim vtx vs. time",
     18000,0.,3600.,120,-3.0,3.0);

}
//_____________________________________________________________________________
void StQAMakerBase::BookHistTrigger(){  

  QAH::maker = (StMaker*) (this);
  QAH::preString = QAMakerType;
  if (mTrigWord) return;
  mTrigWord = QAH::H1F("QaTrigWord","trigger word",8,0.5,8.5);
  mTrigWord->SetXTitle("1:MinBias 2:Central 3:HiPt 4:Jet 5:HiTower 6:OtherPhys");
  mTrigBits = QAH::H1F("QaTrigBits","trigger bits",32,-0.5,31.5);
}
//_____________________________________________________________________________
void StQAMakerBase::BookHistDE(){

  // Get dE/dx histogram from dE/dx maker
  if (!(m_dedx_Z3A)) {
    // First try to get histograms from dE/dx maker named "dEdxY2"
    StMaker* fhMaker = GetMaker("dEdxY2");
    if (fhMaker) {
      m_dedx_Z3A = (TH3F*) (fhMaker->GetHist("Z3A"));
      AddHist(m_dedx_Z3A);
    } else {
      // "dEdxY2" maker doesn't exist, so look in hist branch
      St_DataSet* hDS = GetDataSet("histBranch");
      if (hDS) {
        // hDS->ls(9);
        St_DataSet* fhDS = hDS->Find("dEdxY2Hist");
        if (fhDS) {
          m_dedx_Z3A = (TH3F*) (fhDS->FindObject("Z3A"));
          AddHist(m_dedx_Z3A);
        }
      }
    }
  }
}
//_____________________________________________________________________________
void StQAMakerBase::BookHistFcl(){

  // Get fcl histograms from FTPC makers
  if (!(m_ftpc_chargestepW)) {
    // First try to get histograms from StFtpcClusterMaker named "ftpc_hits"
    StMaker* fhMaker = GetMaker("ftpc_hits");
    if (fhMaker) {
      m_ftpc_chargestepW = (TH1F*) (fhMaker->GetHist("fcl_chargestepW"));
      AddHist(m_ftpc_chargestepW);
      m_ftpc_chargestepE = (TH1F*) (fhMaker->GetHist("fcl_chargestepE"));
      AddHist(m_ftpc_chargestepE);
      m_ftpc_fcl_radialW = (TH1F*) (fhMaker->GetHist("fcl_radialW"));
      AddHist(m_ftpc_fcl_radialW);
      m_ftpc_fcl_radialE = (TH1F*) (fhMaker->GetHist("fcl_radialE"));
      AddHist(m_ftpc_fcl_radialE);
    } else {
      // "ftpc_hits" maker doesn't exist, so look in hist branch
      // *** Currently isn't working for bfcread_event_QAhist.C ***
      St_DataSet* hDS = GetDataSet("histBranch");
      if (hDS) {
        // hDS->ls(9);
        St_DataSet* fhDS = hDS->Find("ftpc_hitsHist");
        if (fhDS) {
          m_ftpc_chargestepW =
	    (TH1F*) (fhDS->FindObject("fcl_chargestepW"));
          AddHist(m_ftpc_chargestepW);
          m_ftpc_chargestepE =
	    (TH1F*) (fhDS->FindObject("fcl_chargestepE"));
          AddHist(m_ftpc_chargestepE);
          m_ftpc_fcl_radialW =
	    (TH1F*) (fhDS->FindObject("fcl_radialW"));
          AddHist(m_ftpc_fcl_radialW);
          m_ftpc_fcl_radialE =
	    (TH1F*) (fhDS->FindObject("fcl_radialE"));
          AddHist(m_ftpc_fcl_radialE);
        }
      }
    }
  }
}
//_____________________________________________________________________________
void StQAMakerBase::BookHistFMS(){

  for(int qt = kQt1; qt < kQtError; ++qt) {

    std::string s;
    switch(qt) {
      case kQt1:
        s = "south-top";
        break;
      case kQt2:
        s = "south-bottom";
        break;
      case kQt3:
        s = "north-top";
        break;
      case kQt4:
        s = "north-bottom";
        break;
      case kFpd:
        s = "FPD";
        break;
    } // switch

    std::string name;
    std::string title;
    // Generate histogram names and titles for FMS QT crates.
    if(qt >= kQt1 and qt <= kQt4) {
      std::stringstream stream;
      stream << "fms_qt_channel_adc_crate_" << qt;
      name = stream.str();
      stream.str("");
      stream.clear();
      stream << "Input to FMS QT crate " << qt << " (" << s << ")";
      title = stream.str();
    } // if
    // ... or the FPD.
    else if(kFpd == qt) {
      name = "fpd_channel_adc";
      title = "Input to FPD QT crate";
    } // else if

    // Create the histogram.
    TH2F* h = new TH2F(name.c_str(),
                       title.c_str(),
                       kNChannels, 0., kNChannels,  // Channel axis bins
                       200, 0., kNAdc);             // ADC axis bins
    h->SetBit(TH1::kCanRebin);
    h->SetXTitle("slot * 32 + channel");
    h->SetYTitle("ADC");
    // Store the histogram.
    AddHist(h);
    mFMShistograms.insert(std::make_pair(qt, h));
  } // for

}
//_____________________________________________________________________________
void StQAMakerBase::BookHistRP(){

  //  m_RP_ClusterLength = QAH::H1F("RP_ClusterLength","Number of strips in each cluster",50,0,50);
  char rpname[kRP_MAXSEQ][5] = { "E1U", "E1D", "E2U", "E2D", "W1U", "W1D", "W2U", "W2D" }; // 2015

  char strs[20], strl[100];
  for ( Int_t i=0; i<kRP_MAXSEQ; i++ ) {
     sprintf(strs,"RP_cluster_xy_%s", rpname[i] ) ;
     sprintf(strl,"Clusters  Y vs X in %s", rpname[i] ) ;
     m_RP_clusters_xy[i] = QAH::H2F(strs, strl, 96, 0., 768., 96, 0., 768. );
  }

}
//_____________________________________________________________________________
void StQAMakerBase::BookHistETOF(){

  // Get ETOF histograms ETOF hit & match makers
  if (!(m_etofHist[0])) {
    int etofCnt = 0;
    // First try to get histograms from StEtofHitMaker
    StMaker* ehMaker = GetMaker("etofHit");
    if (ehMaker) {
      m_etofHist[etofCnt++] = (TH1*) (ehMaker->GetHist("etofHit_tof"));
      m_etofHist[etofCnt++] = (TH1*) (ehMaker->GetHist("averageTimeDiff_etofHits_btofHits"));
      m_etofHist[etofCnt++] = (TH1*) (ehMaker->GetHist("multiplicity_etofHits_btofHits"));
      m_etofHist[etofCnt++] = (TH1*) (ehMaker->GetHist("multiplicity_etofHits_epdEast"));
    }
    // First try to get histograms from StEtofHitMaker
    StMaker* emMaker = GetMaker("etofMatch");
    if (emMaker) {
      m_etofHist[etofCnt++] = (TH1*) (emMaker->GetHist("A_eTofHits_globalXY"));
      m_etofHist[etofCnt++] = (TH1*) (emMaker->GetHist("B_intersectionMult_etofMult"));
      m_etofHist[etofCnt++] = (TH1*) (emMaker->GetHist("G_matchCand_beta_signmom"));
      m_etofHist[etofCnt++] = (TH1*) (emMaker->GetHist("G_matchCand_timeOfFlight_pathLength"));
      m_etofHist[etofCnt++] = (TH1*) (emMaker->GetHist("G_primary_Intersection_validMatch"));
      m_etofHist[etofCnt++] = (TH1*) (emMaker->GetHist("H_matchCand_t0corr_1d"));
    }
    for (int i=0; i<etofCnt; i++) AddHist(m_etofHist[i]);
  }
}
//_____________________________________________________________________________

