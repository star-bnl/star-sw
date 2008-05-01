// -*- mode: c++;-*-
// $Id: StppAnaPars.h,v 1.5 2008/05/01 21:28:39 tai Exp $
#ifndef STPPANAPARS_h
#define STPPANAPARS_h

#include <TObject.h>

namespace StSpinJet {
  class StParticleCollector;
  class StJetFinderRunner;
}

class StppAnaPars : public TObject {

public:

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
  friend class StSpinJet::StParticleCollector;
  friend class StSpinJet::StJetFinderRunner;

  ClassDef(StppAnaPars,1)
};


#endif // STPPANAPARS_h
