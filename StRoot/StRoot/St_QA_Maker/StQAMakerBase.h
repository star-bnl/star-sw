// $Id: StQAMakerBase.h,v 2.34 2019/12/17 19:08:01 genevb Exp $ 
// $Log: StQAMakerBase.h,v $
// Revision 2.34  2019/12/17 19:08:01  genevb
// Add more ETOF histograms
//
// Revision 2.33  2019/05/22 21:24:31  genevb
// Add sDCA vs. time-in-run
//
// Revision 2.32  2019/03/26 15:29:38  genevb
// Introduce ETOF
//
// Revision 2.31  2019/03/14 02:31:53  genevb
// Introduce iTPC plots
//
// Revision 2.30  2019/03/01 19:40:38  genevb
// Some minor Run 19 preparations, including first padrow hit
//
// Revision 2.29  2018/07/03 21:33:34  genevb
// Introduce EPD (code provided by J. Ewigleben)
//
// Revision 2.28  2017/02/25 03:24:30  genevb
// Run 17: remove HFT
//
// Revision 2.27  2015/07/17 19:09:03  genevb
// SSD copied for SST, and HFT histogams use SST now too
//
// Revision 2.26  2015/03/18 21:43:17  genevb
// Introduce Roman Pots histograms (K. Yip)
//
// Revision 2.25  2015/01/21 17:49:40  genevb
// Fix missing run14 cases, remove unused firstEventClass, re-work normalizations with StHistUtil
//
// Revision 2.24  2015/01/16 21:08:28  genevb
// Initial versions of HFT histograms
//
// Revision 2.23  2014/08/06 11:43:53  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 2.22  2014/07/22 20:39:28  genevb
// Add MTD to Offline QA
//
// Revision 2.21  2014/01/30 19:44:06  genevb
// Additional TPC histogram for monitoring gas contamination
//
// Revision 2.20  2013/03/12 03:06:02  genevb
// Add FMS/FPD histograms for Run 13+
//
// Revision 2.19  2012/03/05 03:42:32  genevb
// Remove TPC XY dist, add TPC RPhi charge
//
// Revision 2.18  2012/02/08 22:10:35  genevb
// Updates for Run 12
//
// Revision 2.17  2009/11/19 20:34:38  genevb
// Remove Event Summary (using defunct old software monitors)
//
// Revision 2.16  2007/11/30 05:38:50  genevb
// Changes for Run8: mostly silicon removal, TOF addition
//
// Revision 2.15  2005/02/08 17:22:46  genevb
// PMD histo changes, handle estGlobal/ITTF tracks
//
// Revision 2.14  2004/12/13 15:52:37  genevb
// Numerous updates: PMD, primtrk, FPD, QAShift lists
//
// Revision 2.13  2004/02/12 05:03:17  genevb
// Year 4 AuAu changes. New SVT histos.
//
// Revision 2.12  2004/01/10 01:10:18  genevb
// Preparations for Year 5, added some svt plots
//
// Revision 2.11  2003/02/28 06:17:56  genevb
// Allow StQAMakerBase::Make to be called for all events
//
// Revision 2.10  2003/02/20 20:09:54  genevb
// Several changes for new trigger scheme, dAu data
//
// Revision 2.9  2003/02/19 06:38:29  genevb
// Rework trigger and mult/event class sections
//
// Revision 2.8  2003/02/15 22:00:52  genevb
// Add tpcSectors, fix ftpc east/west charge
//
// Revision 2.7  2002/04/23 01:59:56  genevb
// Addition of BBC/FPD histos
//
// Revision 2.6  2002/01/26 03:04:07  genevb
// Fixed some problems with fcl histos
//
// Revision 2.5  2001/12/28 09:19:13  genevb
// Adjustments for pp running
//
// Revision 2.4  2001/08/29 20:45:15  genevb
// Trigger word histos
//
// Revision 2.3  2001/05/16 20:57:03  lansdell
// new histograms added for qa_shift printlist; some histogram ranges changed; StMcEvent now used in StEventQA
//
// Revision 2.2  2001/04/28 22:05:13  genevb
// Added EMC histograms
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

#ifndef STAR_StQAMakerBase
#define STAR_StQAMakerBase

enum StQAHistSetType {
  StQA_Undef = -1,
  StQA_MC = 0,
  StQA_AuAuOld = 1,
  StQA_pp = 2,
  StQA_dAu = 3,
  StQA_AuAu = 4,
  StQA_run8 = 5,
  StQA_run12all = 6,
  StQA_run12 = 7,
  StQA_run13 = 8,
  StQA_run14 = 9,
  StQA_run15 = 10,
  StQA_run17 = 11,
  StQA_run18 = 12,
  StQA_run19 = 13
  // when adding more, search for StQAHistSetType for other changes
};

#include "StMaker.h"
class StQABookHist;
class TObjArray;
class TH1F;
class TH2F;
class TH3F;

// FMS needs
typedef std::map<int, TH1*> TH1PtrMap;
// Enumerate QT crate numbers 1-5 (1-4 are FMS, 5 is FPD).
enum StFmsQtCrateNumber {kQt1 = 1, kQt2, kQt3, kQt4, kFpd, kQtError};
/*
 Basic QT crate geometry.
 Note that physically there are 16 slots per crate, but only 12
 at most are currently actually used.
 */   
const int kNQtSlotsPerCrate = 12;
const int kNQtChannelsPerSlot = 32;
const int kNChannels = kNQtSlotsPerCrate * kNQtChannelsPerSlot;
const int kNAdc = 4096;

// RP Constants
const int kRP_MAXSEQ = 8 ;
const int kRP_MAXCHAIN = 4 ;

class StQAMakerBase : public StMaker {

// **************** Public Member Functions ***************************
 public:
  StQAMakerBase() {}
  StQAMakerBase(const char *name, const char *title, const char *type);
  virtual       ~StQAMakerBase();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual void   Clear(Option_t *);
  virtual void   UseHistSet(Int_t s) { histsSet=s; }
// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StQAMakerBase.h,v 2.34 2019/12/17 19:08:01 genevb Exp $ built " __DATE__ " " __TIME__ ; return cvs;}


// ******************** Histogram Booking Constants ************************
 protected:

  Int_t ntrk;
  Int_t nmnpt;  
  Int_t nmneta; 
  Int_t nxyz;   

// ***************** Histogram Pointers ***************************
 public:
  // histogram for number of events without primary vertex
  TH1F     *mNullPrimVtx;         //!
  // histogram for number of events in mult classes
  TH1F     *mMultClass;           //!
  // histograms for event trigger words/bits
  TH1F     *mTrigWord;            //!
  TH1F     *mTrigBits;            //!
  // histograms for TPC hits sector by sector
  TH2F     *mTpcSectorPlot[24];   //!
  TH2F     *m_pnt_rpTQW;
  TH2F     *m_pnt_rpTQE;

  // FTPC histograms
  TH1F     *m_ftpc_chargestepW; //! Chargestep from ftpc west
  TH1F     *m_ftpc_chargestepE; //! Chargestep from ftpc east
  TH1F     *m_ftpc_fcl_radialW;  //! ftpc west cluster radial position
  TH1F     *m_ftpc_fcl_radialE;  //! ftpc east cluster radial position

  // TPC dE/dx over time
  TH3F     *m_dedx_Z3A; // dE/dx vs. drift distance

  // signed DCA (impact parameter) over time
  TH2F     *m_glb_simpactTime; //! signed impact parameter from primary vertex vs. time

  // FMS histograms
  // ADC vs. channel histograms keyed by QT crate number.
  // Stores both FMS and FPD histograms.
  TH1PtrMap mFMShistograms;

  // Roman-Pot histograms
  //  TH1F     *m_RP_ClusterLength; // testing
  TH2F     *m_RP_clusters_xy[kRP_MAXSEQ] ; // cluster positions

  // ETOF histograms
  TH1      *m_etofHist[10]; //!

// **************** Members For Internal Use ***************************
 protected:
  TString QAMakerType;  // character string to prepend to each hist name/title
  TObjArray histsList;  //! pointers to the histogram classes for the
  StQABookHist* hists;  //! event class-dependent histograms
  Int_t histsSet;
  TString prefix[32];
  Int_t eventClass;
  Int_t eventCount;
  Bool_t fillHists;
  Bool_t ITTF;
  Int_t EST;
  Bool_t allTrigs;

  virtual void NewQABookHist();
  virtual TH2F* MH1F(const Text_t* name, const Text_t* title,
                     Int_t nbinsx, Axis_t xlow, Axis_t xup);

  virtual void BookHist();
  virtual void BookHistGeneral();
  virtual void BookHistTrigger();
  virtual void BookHistFcl();
  virtual void BookHistFMS();
  virtual void BookHistDE();
  virtual void BookHistRP();
  virtual void BookHistETOF();

  virtual void MakeHistGlob() = 0;
  virtual void MakeHistDE() = 0;
  virtual void MakeHistPrim() = 0;
  virtual void MakeHistPID() = 0;
  virtual void MakeHistVertex() = 0;
  virtual void MakeHistPoint() = 0;
  virtual void MakeHistEMC() {}
  virtual void MakeHistEval() = 0;
  virtual void MakeHistBBC() {}
  virtual void MakeHistFPD() {}
  virtual void MakeHistPMD() {}
  virtual void MakeHistTOF() {}
  virtual void MakeHistFMS() {}
  virtual void MakeHistMTD() {}
  virtual void MakeHistHFT() {}
  virtual void MakeHistPXL() {}
  virtual void MakeHistIST() {}
  virtual void MakeHistSST() {}
  virtual void MakeHistRP () {}
  virtual void MakeHistEPD() {}
  virtual void MakeHistiTPC() {}

  ClassDef(StQAMakerBase,0)   //needed for all code that will be used in CINT
};
    
#endif
