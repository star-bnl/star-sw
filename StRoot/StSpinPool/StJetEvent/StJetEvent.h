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

  int runId() const { return mRunId; }
  int eventId() const { return mEventId; }

  int numberOfVertices() const;
  const StJetVertex* vertex(int i) const;

  void setRunId(int runId) { mRunId = runId; }
  void setEventId(int eventId) { mEventId = eventId; }
  StJetVertex* newVertex();
  StJetCandidate* newJet(const TVector3& vertex, const TLorentzVector& fourMomentum);
  StJetTrack* newTrack();
  StJetTower* newTower();
  StJetParticle* newParticle();

private:
  int mRunId;
  int mEventId;
  TClonesArray* mVertices;
  TClonesArray* mJets;
  TClonesArray* mTracks;
  TClonesArray* mTowers;
  TClonesArray* mParticles;

  ClassDef(StJetEvent,4);
};

#endif // ST_JET_EVENT_H
