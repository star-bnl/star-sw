// -*- mode: c++;-*-
// $Id: StppAnaPars.h,v 1.10 2008/08/03 00:26:18 tai Exp $
#ifndef STPPANAPARS_h
#define STPPANAPARS_h

#include <TObject.h>

class StjeParticleCollector;
class StjeJetFinderRunner;
class StjeJetCuts;

class StppAnaPars : public TObject {

public:

  StppAnaPars()
    : mPtMin(0.2)
    , mEtaMax(100.0)
    , mNhits(12)
    , mFlagMin(0)
    , mJetPtMin(3.5)
    , mJetEtaMax(100.0)
    , mJetEtaMin(0.0)
    , mJetNmin(0)
  { }

  void setCutPtMin(double v) { mPtMin = v; }
  double ptMin() const { return mPtMin; }

  void setAbsEtaMax(double v) { mEtaMax = v; }
  double etaMax() const { return mEtaMax; }

  void setJetPtMin(double v) { mJetPtMin = v; }
  double jetPtMin() const { return mJetPtMin; }

  void setJetEtaMax(double v) { mJetEtaMax = v; }
  double jetEtaMax() const { return mJetEtaMax; }

  void setJetEtaMin(double v) { mJetEtaMin =v; }
  double jetEtaMin() const { return mJetEtaMin; }

  void setJetNmin(int v) { mJetNmin = v; }
  int jetNmin() const { return mJetNmin; }

  void setNhits(int v) { mNhits=v; }
  int nHits() const { return mNhits; }

  void setFlagMin(int v) { mFlagMin = v; }
  int flagMin() const { return mFlagMin; }

private:

  double mPtMin;
  double mEtaMax;
  int mNhits;
  int mFlagMin;
    
  //Cut to accept found-jets
  double mJetPtMin;
  double mJetEtaMax;
  double mJetEtaMin;
  int mJetNmin;

  friend class StppJetAnalyzer;
  friend class StjeParticleCollector;
  friend class StjeJetFinderRunner;
  friend class StjeJetCuts;

  ClassDef(StppAnaPars,1)
};


#endif // STPPANAPARS_h
