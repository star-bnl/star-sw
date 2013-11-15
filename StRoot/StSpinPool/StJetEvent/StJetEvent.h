// -*- mode: c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 1 September 2009
//

#ifndef ST_JET_EVENT_H
#define ST_JET_EVENT_H

class TClonesArray;
class StJetCandidate;
class StJetTrack;
class StJetTower;

class StJetEvent : public TObject {
public:
  StJetEvent();
  ~StJetEvent();

  void Clear(Option_t* option = "");

  int runId() const { return mRunId; }
  int eventId() const { return mEventId; }

  int numberOfJets() const;
  int numberOfTracks() const;
  int numberOfTowers() const;

  StJetCandidate* jet(int i) const;
  StJetTrack* track(int i) const;
  StJetTower* tower(int i) const;

  TClonesArray* jets() const { return mJets; }
  TClonesArray* tracks() const { return mTracks; }
  TClonesArray* towers() const { return mTowers; }

  void setRunId(int runId) { mRunId = runId; }
  void setEventId(int eventId) { mEventId = eventId; }
  StJetCandidate* addJet(const StJetCandidate* jet);
  StJetTrack* newTrack();
  StJetTower* newTower();

private:
  int mRunId;
  int mEventId;

  TClonesArray* mJets;
  TClonesArray* mTracks;
  TClonesArray* mTowers;

  ClassDef(StJetEvent,1);
};

#endif // ST_JET_EVENT_H
