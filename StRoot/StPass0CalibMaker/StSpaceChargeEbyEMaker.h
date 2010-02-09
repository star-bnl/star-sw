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
          void   DontReset() { doReset = kFALSE; }
          void   DoCalib() { Calibmode = kTRUE; DoQAmode(); DoNtuple(); }
          float  EvalCalib(TDirectory* hdir=0);

  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StSpaceChargeEbyEMaker.h,v 1.11 2010/01/28 18:53:30 genevb Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  

protected:

  StEvent* event;
  int evt;
  int runid;
  int curhist;
  int lasttime;
  float sc;
  float esc;
  float lastsc;
  int oldevt;
  StRunInfo* runinfo;
  Bool_t did_auto;
  Bool_t Calibmode;
  Bool_t PrePassmode;
  Bool_t PrePassdone;
  Bool_t QAmode;
  Bool_t doNtuple;
  Bool_t doReset;
  Bool_t doGaps;
  UInt_t inGapRow;

  int HN;
  float MINTRACKS;
  float MAXDIFFE;
  float MAXDIFFA;
  float SCALER_ERROR;

  StMagUtilities* m_ExB;     //!
  void BuildHist(int i);
  void FindSpaceCharge();
  double FindPeak(TH1*,float&);
  float oldness(int i,int j=-1);
  int imodHN(int i);
  float FakeAutoSpaceCharge();

  TH1F* schist;
  TH1F* schists[96];
  int times[96];
  float ntrks[96];
  int evts[96];
  float evtstbin[96];
  float evtsnow;
  
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


  // QA ntuple
  TNtuple* ntup;             //!

  void InitQAHists();
  void WriteQAHists();
  void FillQAHists(float,float,int,StPhysicalHelixD&,int);
  void FillGapHists(StTrack*,StPhysicalHelixD&,int,int);
  void DetermineGaps();
  void DetermineGapHelper(TH2F*,float&,float&,float&);


  ClassDef(StSpaceChargeEbyEMaker, 0)
};
    
#endif

//_____________________________________________________________________________
// $Id: StSpaceChargeEbyEMaker.h,v 1.11 2010/01/28 18:53:30 genevb Exp $
// $Log: StSpaceChargeEbyEMaker.h,v $
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
