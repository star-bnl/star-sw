/***************************************************************************
 *
 * $Id: franksV0PairCut.h,v 1.1 2000/05/03 17:47:24 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 ***************************************************************************
 *
 * $Log: franksV0PairCut.h,v $
 * Revision 1.1  2000/05/03 17:47:24  laue
 * new pair cut
 *
 *
 **************************************************************************/


#ifndef franksV0PairCut_hh
#define franksV0PairCut_hh

// do I need these lines ?
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "StHbtMaker/Base/StHbtPairCut.h"

class franksV0PairCut : public StHbtPairCut{
public:
  franksV0PairCut();
  franksV0PairCut(const franksV0PairCut&);
  //~franksV0PairCut();

  virtual bool Pass(const StHbtPair*);
  virtual StHbtString Report();
  franksV0PairCut* Clone();

  void SetTrackIdCut(const short );
  short TrackIdCut() const;

private:
  short mTrackIdCut;
  long mNPairsPassed;
  long mNPairsFailed;

#ifdef __ROOT__
  ClassDef(franksV0PairCut, 1)
#endif
};

inline franksV0PairCut::franksV0PairCut(const franksV0PairCut& c) : StHbtPairCut(c) {
  mNPairsPassed = 0;
  mNPairsFailed = 0;
  mTrackIdCut = c.mTrackIdCut;

}
inline franksV0PairCut* franksV0PairCut::Clone() { franksV0PairCut* c = new franksV0PairCut(*this); return c;}

inline void  franksV0PairCut::SetTrackIdCut(const short x) { mTrackIdCut = x; }
inline short franksV0PairCut::TrackIdCut() const { return mTrackIdCut; }

#endif
