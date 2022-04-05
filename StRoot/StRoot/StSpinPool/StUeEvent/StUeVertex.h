#ifndef STUEVERTEX
#define STUEVERTEX

#include "TObject.h"
#include "TRefArray.h"
#include "TVector3.h"

#include "StUeJet.h"

class StUeVertex : public TObject{
 public:
  StUeVertex()
    : mPosition(-999,-999,-999)
    , mRanking(-999)
    {
    }

  ~StUeVertex(){
  }

  const TVector3&  position() const { return mPosition; }
  float ranking() const { return mRanking; }
  void setPosition(TVector3 pos) { mPosition = pos;}
  void setRanking(float ranking) { mRanking = ranking;}

  const TRefArray& ueJets() const { return mUeJets; }
  int numberOfUeJets() const { return mUeJets.GetEntriesFast(); }
  StUeJet* ueJet(int i) const { return (StUeJet*)mUeJets.At(i); }
  StUeJet* addUeJet(StUeJet* uejet) { mUeJets.Add((TObject*)uejet); return (StUeJet*)mUeJets.Last(); }

 private:

  TVector3 mPosition;
  float mRanking;

  TRefArray mUeJets;
  ClassDef(StUeVertex, 1);
};
#endif

  
