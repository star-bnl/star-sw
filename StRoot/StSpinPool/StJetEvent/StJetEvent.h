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
#include "TDatime.h"

class StJetEvent : public TObject {
public:
  StJetEvent();
  ~StJetEvent();

  void Clear(Option_t* option = "");

  int runId  () const { return mRunId;   }
  int eventId() const { return mEventId; }

  // Event time should really be moved to skim tree
  const TDatime& dateTime() const { return mDatime; }
  int year  () const { return mDatime.GetYear  (); }
  int month () const { return mDatime.GetMonth (); }
  int day   () const { return mDatime.GetDay   (); }
  int hour  () const { return mDatime.GetHour  (); }
  int minute() const { return mDatime.GetMinute(); }
  int second() const { return mDatime.GetSecond(); }
  unsigned int unixTime() const { return mDatime.Convert(); }

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
  StJetCandidate* newJet     (const TVector3& vertex, const TLorentzVector& fourMomentum, float area = 0, float area_error = 0);
  StJetTrack*     newTrack   ();
  StJetTower*     newTower   ();
  StJetParticle*  newParticle();

private:
  friend class StjeJetEventTreeWriter;
  friend class StJetMaker2009;
  friend class StJetMaker2012;

  int mRunId;
  int mEventId;
  TDatime mDatime; // Event time should really be moved to skim tree

  TClonesArray* mVertices;
  TClonesArray* mJets;
  TClonesArray* mTracks;
  TClonesArray* mTowers;
  TClonesArray* mParticles;

  ClassDef(StJetEvent,6);
};

#endif // ST_JET_EVENT_H
