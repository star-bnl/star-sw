#ifndef franksPairCut_hh
#define franksPairCut_hh

// do I need these lines ?
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtPairCut.h"

class franksPairCut : public StHbtPairCut{
public:
  franksPairCut();
  franksPairCut(const franksPairCut&);
  //~franksPairCut();

  virtual bool Pass(const StHbtPair*);
  virtual void EventBegin(const StHbtEvent*);
  virtual void EventEnd(const StHbtEvent*) { /* no-op */ }
  franksPairCut* Clone();

  virtual StHbtString Report();


private:
  StHbtThreeVector mPrimaryVertex;
  long mNPairsPassed;
  long mNPairsFailed;

#ifdef __ROOT__
  ClassDef(franksPairCut, 1)
#endif
};

inline franksPairCut* franksPairCut::Clone() { franksPairCut* c = new franksPairCut(*this); return c;}
inline void franksPairCut::EventBegin(const StHbtEvent* ev) { mPrimaryVertex = ev->PrimVertPos(); }

#endif
