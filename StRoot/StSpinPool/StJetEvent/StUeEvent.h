// -*- mode: c++ -*-

//
// Grant Webb <gdwebb@bnl.gov>
// Brookhaven National Laboratory
// 30 July 2015
//

#ifndef ST_UE_EVENT_H
#define ST_UE_EVENT_H

class TClonesArray;
class TVector3;
class TLorentzVector;
class StJetVertex;
class StJetTrack;
class StJetTower;
class StJetParticle;

#include "TObject.h"

class StUeEvent : public TObject {
public:
  StUeEvent();
  ~StUeEvent();

  void Clear(Option_t* option ="");

  int runId () const {return mRunId; } 
  int eventId () const { return mEventId; }
  float leadingJetpt() const { return mLeadingJetpt; }
  
  int numberOfVertices () const;
  int numberOfTracks   () const;
  int numberOfTowers   () const;
  int numberOfParticles() const;

  StJetVertex*     vertex(int i = 0) const;
  StJetTrack*       track(int i)     const;
  StJetTower*       tower(int i)     const;
  StJetParticle* particle(int i)     const;

  TClonesArray* vertices () const { return mVertices;  }
  TClonesArray* tracks   () const { return mTracks;    }
  TClonesArray* towers   () const { return mTowers;    }
  TClonesArray* particles() const { return mParticles; }

  StJetVertex*    newVertex  ();
  StJetTrack*     newTrack   ();
  StJetTower*     newTower   ();
  StJetParticle*  newParticle();

  float sumTrackPt() const;
  float sumTowerPt() const;
  float sumParticlePt() const;
  float sumPt() const { return sumTrackPt() + sumTowerPt();}
private:
  friend class StjeJetEventTreeWriter;
  friend class StJetMaker2009;
  friend class StUEMaker2009;

  int mRunId;
  int mEventId;
  float mLeadingJetpt;
  TDatime mDatime; // Event time should really be moved to skim tree

  TClonesArray* mVertices;
  TClonesArray* mTracks;
  TClonesArray* mTowers;
  TClonesArray* mParticles;

  ClassDef(StUeEvent,7);
};

#endif // ST_Ue_EVENT_H

