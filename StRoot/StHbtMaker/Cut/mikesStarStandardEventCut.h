/***************************************************************************
 *
 * $Id: mikesStarStandardEventCut.h,v 1.3 2003/09/02 17:58:21 perev Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   A simple event-wise cut that selects on multiplicity and z-position
 *   of primary vertex.  This one calculates and then cuts on the number of
 *   negative primary tracks with -0.5<eta<0.5, which is the STAR standard.
 *   The cuts are (copied from StEventUtilities/StuRefMult.hh
 * primary tracks only
 * flag > 0
 * charge < 0
 * fit points >= 10
 * abs(eta) < 0.5
 * dca < 3 cm
 *
 ***************************************************************************
 *
 * $Log: mikesStarStandardEventCut.h,v $
 * Revision 1.3  2003/09/02 17:58:21  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2001/04/25 17:57:46  perev
 * HPcorrs
 *
 * Revision 1.1  2000/09/04 16:27:15  lisa
 * added StarStandard multiplicity cut and modified mikesTrackCut to allow NOT cutting on charge sign
 *
 *
 **************************************************************************/

#ifndef mikesStarStandardEventCut_hh
#define mikesStarStandardEventCut_hh


#include "StHbtMaker/Base/StHbtEventCut.h"
#ifdef HPUX
#include "Stiostream.h"
#else
#include <fstream>
#endif

class mikesStarStandardEventCut : public StHbtEventCut {

public:

  mikesStarStandardEventCut();
  mikesStarStandardEventCut(mikesStarStandardEventCut&);
  //  ~mikesStarStandardEventCut();

  void SetEventMult(const int& lo,const int& hi);
  void SetVertZPos(const float& lo, const float& hi);
  int NEventsPassed();
  int NEventsFailed();

  virtual StHbtString Report();
  virtual bool Pass(const StHbtEvent*);

  mikesStarStandardEventCut* Clone();

private:   // here are the quantities I want to cut on...

  int mEventMult[2];      // range of multiplicity
  float mVertZPos[2];     // range of z-position of vertex

  long mNEventsPassed;
  long mNEventsFailed;

  ofstream* mOutFile;  //!

#ifdef __ROOT__
  ClassDef(mikesStarStandardEventCut, 0)
#endif

};

inline void mikesStarStandardEventCut::SetEventMult(const int& lo, const int& hi){mEventMult[0]=lo; mEventMult[1]=hi;}
inline void mikesStarStandardEventCut::SetVertZPos(const float& lo, const float& hi){mVertZPos[0]=lo; mVertZPos[1]=hi;}
inline int  mikesStarStandardEventCut::NEventsPassed() {return mNEventsPassed;}
inline int  mikesStarStandardEventCut::NEventsFailed() {return mNEventsFailed;}
inline mikesStarStandardEventCut* mikesStarStandardEventCut::Clone() { mikesStarStandardEventCut* c = new mikesStarStandardEventCut(*this); return c;}
inline mikesStarStandardEventCut::mikesStarStandardEventCut(mikesStarStandardEventCut& c) : StHbtEventCut(c) {
  mEventMult[0] = c.mEventMult[0];
  mEventMult[1] = c.mEventMult[1];
  mVertZPos[0] = c.mVertZPos[0];
  mVertZPos[1] = c.mVertZPos[1];
  mNEventsPassed = 0;
  mNEventsFailed = 0;
}


#endif
