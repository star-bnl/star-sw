// -*- mode: c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 8 Dec 2009
//

#ifndef ST_JET_PARTICLE_H
#define ST_JET_PARTICLE_H

#include "TLorentzVector.h"
#include "TParticlePDG.h"
#include "TDatabasePDG.h"

#include "StJetElement.h"

class StJetParticle : public StJetElement {
public:
  float                     m() const { return mM; }
  float                     e() const { return mE; }
  int                     pdg() const { return mPdg; }
  int                  status() const { return mStatus; }
  int             firstMother() const { return mFirstMother; }
  int              lastMother() const { return mLastMother; }
  int           firstDaughter() const { return mFirstDaughter; }
  int            lastDaughter() const { return mLastDaughter; }

  TLorentzVector fourMomentum() const
  {
    TLorentzVector p;
    p.SetPtEtaPhiE(mPt,mEta,mPhi,mE);
    return p;
  }

  const char* name() const
  {
    static const char* noname = "???";
    const TParticlePDG* ap = TDatabasePDG::Instance()->GetParticle(mPdg);
    return ap ? ap->GetName() : noname;
  }

private:
  friend class StjeJetEventTreeWriter;
  friend class StJetMaker2009;
  friend class StUEMaker2009;

  float mM;
  float mE;
  int   mPdg;
  int   mStatus;
  int   mFirstMother;
  int   mLastMother;
  int   mFirstDaughter;
  int   mLastDaughter;

  ClassDef(StJetParticle,2);
};

#endif	// ST_JET_PARTICLE_H
