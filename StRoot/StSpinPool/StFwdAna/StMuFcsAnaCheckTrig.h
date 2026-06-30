/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  The purpoe of this class is to check and set the trigger information for the event in #StMuFcsAnaData::mTriggers. It is specifically tuned to look for FCS triggers in #StMuFcsAnaData::mTargetTrig.

  DESCRIPTION
  It will match triggers by name listed in #StMuFcsAnaData::mTargetTrig which is the triggers you want to use in the analysis. If a matching trigger was found it will set #StMuFcsAnaData::mValidTrigFound to true. It will also check if the FCS EM triggers were found. If #StMuFcsAnaData::mIgnoreTrig was set it will still check for matched triggers and FCS triggers but it will always set #StMuFcsAnaData::mValidTrigFound to true.

  CAVEATS
  The trigger matching only works if you have taken the time to create a text file that contains a list of all the FCS triggers and their starting and ending run numbers. It's format must be "TriggerName OfflineId StartRun EndRun". This text file must then be specified to #StMuFcsAnaDataMaker

  LOG
  @[January 14, 2026] > First instance where relevant functionality was copied from #StMuFcsTreeMaker

*/


#ifndef STMUFCSANACHECKTRIG_HH
#define STMUFCSANACHECKTRIG_HH

#include "StMuFcsVirtualAna.h"

class StMuFcsAnaCheckTrig : public StMuFcsVirtualAna
{
public:
  StMuFcsAnaCheckTrig();
  ~StMuFcsAnaCheckTrig();

  virtual UInt_t LoadHists(TFile* file, HistManager* histman, StMuFcsAnaData* data);
  virtual Int_t DoMake(StMuFcsAnaData* mufcsdata);

  void PaintMatchTriggers(TCanvas* canvas, const char* savename) const;
  
protected:
  TH1* mH1F_MatchFcsTriggers = 0;               ///< Triggers used in analysis
  
  ClassDef(StMuFcsAnaCheckTrig,1)
};

#endif

