#ifndef franksPairCut_hh
#define franksPairCut_hh

// do I need these lines ?
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

class StHbtPair;
#include "StHbtMaker/Base/StHbtPairCut.h"

class franksPairCut : public StHbtPairCut{
public:
  franksPairCut();
  franksPairCut(const franksPairCut&);
  //~franksPairCut();

  virtual bool Pass(const StHbtPair*);
  virtual void EventBegin(const StHbtEvent*);
  virtual void EventEnd(const StHbtEvent*) { /* no-op */ }
  void SetQuality(const float, const float );
  void SetKt(const float, const float );
  void SetPt(const float, const float );
  void SetOpeningAngle(const float, const float );
  void SetEntranceSeparation(const float, const float );
  void SetRapidity(const float, const float );
  void SetEta(const float, const float );
  void SetQinv(const float, const float );
  void SetDecayLength(const float, const float);
  void SetAngleToPrimaryVertex(const float, const float);
  void SetDcaToPrimaryVertex(const float, const float);
  void SetDcaOfDaughters(const float, const float);
  void SetIdenticalMother(const int=0);
  //  void SetResonanceInfo(bool b=false); 
  franksPairCut* Clone();

  virtual StHbtString Report();


 private:
  bool leave(bool);
  //  bool mResonanceInfoOn;
  StHbtThreeVector mPrimaryVertex;
  int mIdenticalMother;
  float mQuality[2];
  float mKt[2];
  float mPt[2];
  float mRapidity[2];
  float mEta[2];
  float mQinv[2];
  float mOpeningAngle[2];
  float mEntranceSeparation[2];
  float mDecayLength[2];
  float mAngleToPrimaryVertex[2];
  float mDcaToPrimaryVertex[2];
  float mDcaOfDaughters[2];
  long mNPairsPassed;
  long mNPairsFailed;

 protected:


#ifdef __ROOT__
  ClassDef(franksPairCut, 1)
#endif
};

inline void franksPairCut::SetQuality(const float x, const float y) { mQuality[0]=x; mQuality[1]=y; }
inline void franksPairCut::SetKt(const float x, const float y) { mKt[0]=x; mKt[1]=y; }
inline void franksPairCut::SetPt(const float x, const float y) { mPt[0]=x; mPt[1]=y; }
inline void franksPairCut::SetOpeningAngle(const float x, const float y) { mOpeningAngle[0]=x; mOpeningAngle[1]=y; }
inline void franksPairCut::SetRapidity(const float x, const float y) { mRapidity[0]=x; mRapidity[1]=y; }
inline void franksPairCut::SetEta(const float x, const float y) { mEta[0]=x; mEta[1]=y; }
inline void franksPairCut::SetQinv(const float x, const float y) { mQinv[0]=x; mQinv[1]=y; }
inline void franksPairCut::SetEntranceSeparation(const float x, const float y) { mEntranceSeparation[0]=x; mEntranceSeparation[1]=y; }
inline void franksPairCut::SetDecayLength(const float x, const float y) { mDecayLength[0]=x; mDecayLength[1]=y; }
inline void franksPairCut::SetAngleToPrimaryVertex(const float x, const float y) { mAngleToPrimaryVertex[0]=x; mAngleToPrimaryVertex[1]=y; }
inline void franksPairCut::SetDcaToPrimaryVertex(const float x, const float y) { mDcaToPrimaryVertex[0]=x; mDcaToPrimaryVertex[1]=y; }
inline void franksPairCut::SetDcaOfDaughters(const float x, const float y) { mDcaOfDaughters[0]=x; mDcaOfDaughters[1]=y; }
inline void franksPairCut::SetIdenticalMother(const int i) { mIdenticalMother=i; }
inline void franksPairCut::EventBegin(const StHbtEvent* ev) { mPrimaryVertex = ev->PrimVertPos(); }
inline franksPairCut* franksPairCut::Clone() { franksPairCut* c = new franksPairCut(*this); return c;}
//inline void franksPairCut::SetResonanceInfo(bool b) {mResonanceInfoOn=b;}

#endif
