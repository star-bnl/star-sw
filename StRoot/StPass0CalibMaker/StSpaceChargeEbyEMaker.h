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
class StPhysicalHelixD;
class St_spaceChargeCor;
class StRunInfo;

class StSpaceChargeEbyEMaker : public StMaker {
 
public: 
  StSpaceChargeEbyEMaker(const char *name="SCEbyE");
  virtual       ~StSpaceChargeEbyEMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual Int_t  DecideSpaceCharge(int time);
  
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StSpaceChargeEbyEMaker.h,v 1.2 2004/08/13 20:49:12 genevb Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  

protected:

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
  Bool_t PrePassmode;
  Bool_t PrePassdone;
  Bool_t QAmode;
  
  int HN;
  float MINTRACKS;
  float MAXDIFFE;
  float MAXDIFFA;
  float SCALER_ERROR;

  StMagUtilities* m_ExB;     //!
  StMaker* tpcDbMaker;       //!
  StMaker* tpcHitMoverMaker; //!
  void BuildHist(int i);
  void FindSpaceCharge();
  float oldness(int i,int j=-1);
  int imodHN(int i);
  float FakeAutoSpaceCharge();

  TH1F* schist;
  TH1F* schists[32];
  int times[32];
  float ntrks[32];
  float ntrkssum;
  int evts[32];
  float evtstbin[32];
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
  TH2F* dcehist;             //!
  TH2F* dcphist;             //!
  TH3F* dcahist;             //!
  TH3F* dcahistN;            //!
  TH3F* dcahistP;            //!
  TH3F* dcahistE;            //!
  TH3F* dcahistW;            //!
  void InitQAHists();
  void WriteQAHists();
  void FillQAHists(float,float,int,StPhysicalHelixD&,int);


  ClassDef(StSpaceChargeEbyEMaker, 0)
};
    
#endif

//_____________________________________________________________________________
// $Id: StSpaceChargeEbyEMaker.h,v 1.2 2004/08/13 20:49:12 genevb Exp $
// $Log: StSpaceChargeEbyEMaker.h,v $
// Revision 1.2  2004/08/13 20:49:12  genevb
// Improve upon keeping method locked on for each event, and timestamp change
//
// Revision 1.1  2004/06/30 23:16:00  genevb
// Introduction of StSpaceChargeEbyEMaker
//
//
