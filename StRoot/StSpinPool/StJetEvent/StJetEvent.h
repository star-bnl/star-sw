// -*- mode: c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 1 September 2009
//

#ifndef ST_JET_EVENT_H
#define ST_JET_EVENT_H

class TClonesArray;
class TVector3;
class TLorentzVector;
class StJetVertex;
class StJetCandidate;
class StJetTrack;
class StJetTower;
class StJetParticle;

#include "TObject.h"

class StJetEvent : public TObject {
public:
  StJetEvent();
  ~StJetEvent();

  void Clear(Option_t* option = "");

  int runId  () const { return mRunId;   }
  int eventId() const { return mEventId; }

  int numberOfVertices () const;
  int numberOfJets     () const;
  int numberOfTracks   () const;
  int numberOfTowers   () const;
  int numberOfParticles() const;

  StJetVertex*     vertex(int i = 0) const;
  StJetCandidate*     jet(int i)     const;
  StJetTrack*       track(int i)     const;
  StJetTower*       tower(int i)     const;
  StJetParticle* particle(int i)     const;

  TClonesArray* vertices () const { return mVertices;  }
  TClonesArray* jets     () const { return mJets;      }
  TClonesArray* tracks   () const { return mTracks;    }
  TClonesArray* towers   () const { return mTowers;    }
  TClonesArray* particles() const { return mParticles; }

  StJetVertex*    newVertex  ();
  StJetCandidate* newJet     (const TVector3& vertex, const TLorentzVector& fourMomentum);
  StJetTrack*     newTrack   ();
  StJetTower*     newTower   ();
  StJetParticle*  newParticle();

private:
  friend class StjeJetEventTreeWriter;
  friend class StJetMaker2009;

  int mRunId;
  int mEventId;

  TClonesArray* mVertices;
  TClonesArray* mJets;
  TClonesArray* mTracks;
  TClonesArray* mTowers;
  TClonesArray* mParticles;

  ClassDef(StJetEvent,5);
};

#endif // ST_JET_EVENT_H
