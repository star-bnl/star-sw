/*!
  \class StSpaceChargeEbyEMaker
  
  StSpaceChargeEbyEMaker performs event-by-event determination
  of the space charge correction for tracks, and sets it for
  the next event.

*/

#ifndef STAR_StSpaceChargeEbyEMaker
#define STAR_StSpaceChargeEbyEMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StMagUtilities;
class TH1F;
class TH2F;
class TH3F;
#include "StPhysicalHelixD.hh"
class TNtuple;
class St_spaceChargeCor;
class StRunInfo;
class StEvent;
class StTrack;
const int SCHN = 96;


class StSpaceChargeEbyEMaker : public StMaker {
 
public: 
  StSpaceChargeEbyEMaker(const char *name="SCEbyE");
  virtual       ~StSpaceChargeEbyEMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual Int_t  DecideSpaceCharge(int time);
  
          void   DoQAmode() { QAmode = kTRUE; }
          void   DoPrePassmode() { PrePassmode = kTRUE; }
          void   DoNtuple() { doNtuple = kTRUE; DoGaps(); }
          void   DoGaps() { doGaps = kTRUE; }
          void   DoSecGaps() { doSecGaps = kTRUE; DoGaps(); }
          void   DontReset() { doReset = kFALSE; }
          void   DoCalib() { Calibmode = kTRUE; DoQAmode(); DoNtuple(); }
          float  EvalCalib(TDirectory* hdir=0);
          void   DoTrackInfo(Int_t mode=1) { TrackInfomode = mode; }
          void   DoAsym() { Asymmode = kTRUE; }

          void   setVtxVpdAgree(float x) { vtxVpdAgree = x; }
          void   setVtxPCTs(UInt_t x) { vtxPCTs = x; }
          void   setVtxEmcMatch(UInt_t x) { vtxEmcMatch = x; }
          void   setVtxTofMatch(UInt_t x) { vtxTofMatch = x; }
          void   setVtxMinTrks(UInt_t x) { vtxMinTrks = x; }

          void   setMinTpcHits(UInt_t x) { minTpcHits = x; }
          void   setReqEmcMatch(Bool_t match = kTRUE)
            { reqEmcMatch = match; reqEmcOrTofMatch = kFALSE; }
          void   setReqTofMatch(Bool_t match = kTRUE)
            { reqTofMatch = match; reqEmcOrTofMatch = kFALSE; }
          void   setReqEmcOrTofMatch(Bool_t match = kTRUE)
            { reqEmcOrTofMatch = match; reqEmcMatch = kFALSE; reqTofMatch = kFALSE; }

  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StSpaceChargeEbyEMaker.h,v 1.21 2015/05/23 04:26:07 genevb Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  

protected:

  StEvent* event;
  int evt;
  int runid;
  int curhist;
  int lasttime;
  float sc;
  float esc;
  float scS[24];
  float escS[24];
  float scE;
  float escE;
  float scW;
  float escW;
  float lastsc;
  float lastEWRatio;
  int oldevt;
  int firstEvent;
  StRunInfo* runinfo;
  Bool_t did_auto;
  Bool_t Calibmode;
  Bool_t PrePassmode;
  Bool_t PrePassdone;
  Bool_t QAmode;
  Int_t  TrackInfomode;
  Bool_t Asymmode;
  Bool_t doNtuple;
  Bool_t doReset;
  Bool_t doGaps;
  Bool_t doSecGaps;
  UInt_t inGapRow;
  float  vtxVpdAgree;
  UInt_t vtxPCTs;
  UInt_t vtxEmcMatch;
  UInt_t vtxTofMatch;
  UInt_t vtxMinTrks;
  UInt_t minTpcHits;
  Bool_t reqEmcMatch;
  Bool_t reqTofMatch;
  Bool_t reqEmcOrTofMatch;

  float MINTRACKS;
  float MAXDIFFE;
  float MAXDIFFA;
  float SCALER_ERROR;

  StMagUtilities* m_ExB;     //!
  void BuildHist(int i, TH1F* aschist, TH1F** aschists);
  void FindSpaceCharge(TH1F* aschist, float& asc, float& aesc);
  double FindPeak(TH1*,float&);
  float oldness(int i,int j=-1);
  int imodHN(int i);
  float FakeAutoSpaceCharge();

  TH1F* schist;
  TH1F* schistS[24];
  TH1F* schistE;
  TH1F* schistW;
  TH1F* schists[SCHN];
  TH1F* schistsE[SCHN];
  TH1F* schistsW[SCHN];
  int times[SCHN];
  float ntrks[SCHN];
  float ntrksE[SCHN];
  float ntrksW[SCHN];
  float ntrksS[24];
  int evts[SCHN];
  float evtstbin[SCHN];
  float evtsnow;
  TNamed* SCcorrection;
  TNamed* GLcorrection;
  TNamed* SCEWRatio;
  
  // PrePass info
  TString tabname;
  void SetTableName();
  void WriteTableToFile();
  St_spaceChargeCor* SCTable();

  // QA hists
  TH1F* scehist;             //!
  TH1F* timehist;            //!
  TH3F* myhist;              //!
  TH3F* myhistN;             //!
  TH3F* myhistP;             //!
  TH3F* myhistE;             //!
  TH3F* myhistW;             //!
  TH2F* dczhist;             //!
  TH2F* dcehist;             //!
  TH3F* dcphist;             //!
  TH3F* dcahist;             //!
  TH3F* dcahistN;            //!
  TH3F* dcahistP;            //!
  TH3F* dcahistE;            //!
  TH3F* dcahistW;            //!

  // Gap hists
  TH2F* gapZhist;            //!
  TH2F* gapZhistneg;         //!
  TH2F* gapZhistpos;         //!
  TH2F* gapZhistS[24];       //!
  TH2F* gapZhistnegS[24];    //!
  TH2F* gapZhistposS[24];    //!


  // QA hists and ntuple
  TH1I* cutshist;            //!
  TNtuple* ntup;             //!

  float gapZfitslope;
  float egapZfitslope;
  float gapZfitintercept;
  float egapZfitintercept;
  float gapZdivslope;
  float egapZdivslope;
  float gapZfitslopeneg;
  float gapZfitinterceptneg;
  float gapZdivslopeneg;
  float gapZfitslopepos;
  float gapZfitinterceptpos;
  float gapZdivslopepos;
  float gapZfitslopeeast;
  float gapZfitintercepteast;
  float gapZdivslopeeast;
  float gapZfitslopewest;
  float gapZfitinterceptwest;
  float gapZdivslopewest;

  float gapZfitslopeS[24];
  float gapZfitinterceptS[24];
  float gapZdivslopeS[24];
  float gapZfitslopenegS[24];
  float gapZfitinterceptnegS[24];
  float gapZdivslopenegS[24];
  float gapZfitslopeposS[24];
  float gapZfitinterceptposS[24];
  float gapZdivslopeposS[24];


  void InitQAHists();
  void WriteQAHists();
  void FillQAHists(float,float,float,int,StPhysicalHelixD&,int);
  void FillGapHists(StTrack*,StPhysicalHelixD&,int,int);
  void DetermineGaps();
  TString DetermineGapHelper(TH2F*,float&,float&,float&);


  ClassDef(StSpaceChargeEbyEMaker, 0)
};
    
#endif

//_____________________________________________________________________________
// $Id: StSpaceChargeEbyEMaker.h,v 1.21 2015/05/23 04:26:07 genevb Exp $
// $Log: StSpaceChargeEbyEMaker.h,v $
// Revision 1.21  2015/05/23 04:26:07  genevb
// More vertex selection criteria: PCT daughters, and VPD z agreement
//
// Revision 1.20  2014/10/23 21:07:23  genevb
// Add GridLeak-by-sector codes, East/WestOff handling, and some code reformatting
//
// Revision 1.19  2014/08/06 11:43:32  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.18  2014/07/23 17:58:46  genevb
// Machinery for sector-by-sector Gaps (GridLeak) measurements
//
// Revision 1.17  2014/05/02 02:38:07  genevb
// TrackInfo mode with pile-up tracks too
//
// Revision 1.16  2014/01/02 20:54:28  genevb
// TrackInfomode, and Basic E/W asymmetry functionality
//
// Revision 1.15  2013/09/25 20:55:51  genevb
// Allow use of multiple PPVF vertices, introduce EmcOrTofMatch, keep track of Predict...() cuts
//
// Revision 1.14  2012/12/15 03:13:50  genevb
// Store used calibrations in histogram files
//
// Revision 1.13  2011/02/10 18:31:45  genevb
// Restore corrected coincidence rates, add QA histogram of where events/tracks are cut
//
// Revision 1.12  2010/06/09 20:24:53  genevb
// Modify interface to allow EMC and TOF matching requirements (needs implementation)
//
// Revision 1.11  2010/01/28 18:53:30  genevb
// Remove unneeded members
//
// Revision 1.10  2009/11/20 18:51:19  genevb
// Avoid compiler warning about unsigned comparison
//
// Revision 1.9  2009/11/16 22:02:19  genevb
// Loosen nDaughters cut, add BEMCmatch cut, PCT hits cut, enable padrow 13 for Run 9+
//
// Revision 1.8  2008/07/15 22:30:39  genevb
// Added evaluation of calibration performance
//
// Revision 1.7  2006/08/15 23:40:59  genevb
// Averaging was done improperly in DontReset mode
//
// Revision 1.6  2006/06/01 17:27:11  genevb
// Bug fix: gapd and gapf backwards; Improvements: gap fit intercepts, hist and fit ranges
//
// Revision 1.5  2006/01/05 19:12:53  genevb
// Added calib mode
//
// Revision 1.4  2005/07/06 22:51:40  fisyak
// use Templated StPhysicalHelixD
//
// Revision 1.3  2005/04/21 19:38:20  genevb
// Additional code for studying SpaceCharge
//
// Revision 1.2  2004/08/13 20:49:12  genevb
// Improve upon keeping method locked on for each event, and timestamp change
//
// Revision 1.1  2004/06/30 23:16:00  genevb
// Introduction of StSpaceChargeEbyEMaker
//
//
