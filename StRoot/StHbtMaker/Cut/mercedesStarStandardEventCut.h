/***************************************************************************
 *
 * $Id: mercedesStarStandardEventCut.h,v 1.2 2003/09/02 17:58:21 perev Exp $
 *
 * Author: Mercedes Lopez Noriega , Ohio State, mercedes@pacific.mps.ohio-state.edu
 *
 ***************************************************************************
 *
 * Description:part of STAR HBT Framework: StHbtMaker package
 * A event-wise cut taht cuts on multiplicity ("new" STAR standard multipliity 
 * definition for charge hadrons, 2001 data) and:
 * primary tracks only
 * flag > 0
 * fit points >= 10
 * abs(eta) < 0.5
 * dca < 3 cm
 * 
 ***************************************************************************
 *
 * $Log: mercedesStarStandardEventCut.h,v $
 * Revision 1.2  2003/09/02 17:58:21  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2002/02/22 15:50:01  mercedes
 * added StarStandard multiplicity cut for charge hadrons (2001 data)
 *
 * Revision 1.1  2002/02/22 16:27:15  mercedes
 * added StarStandard multiplicity cut for charge hadrons
 *
 **************************************************************************/


#ifndef mercedesStarStandardEventCut_hh
#define mercedesStarStandardEventCut_hh

#include "StHbtMaker/Base/StHbtEventCut.h"
#ifdef HPUX
#include "Stiostream.h"
#else
#include <fstream>
#endif

class mercedesStarStandardEventCut : public StHbtEventCut {

public:

  mercedesStarStandardEventCut();
  mercedesStarStandardEventCut(mercedesStarStandardEventCut&);
  //  ~mercedesStarStandardEventCut();

  void SetEventMult(const int& lo,const int& hi);
  void SetVertZPos(const float& lo, const float& hi);
  int NEventsPassed();
  int NEventsFailed();

  virtual StHbtString Report();
  virtual bool Pass(const StHbtEvent*);

  mercedesStarStandardEventCut* Clone();

private:   // here are the quantities I want to cut on...

  int mEventMult[2];      // range of multiplicity
  float mVertZPos[2];     // range of z-position of vertex

  long mNEventsPassed;
  long mNEventsFailed;

  ofstream* mOutFile;  //!

#ifdef __ROOT__
  ClassDef(mercedesStarStandardEventCut, 0)
#endif

};

inline void mercedesStarStandardEventCut::SetEventMult(const int& lo, const int& hi){mEventMult[0]=lo; mEventMult[1]=hi;}
inline void mercedesStarStandardEventCut::SetVertZPos(const float& lo, const float& hi){mVertZPos[0]=lo; mVertZPos[1]=hi;}
inline int  mercedesStarStandardEventCut::NEventsPassed() {return mNEventsPassed;}
inline int  mercedesStarStandardEventCut::NEventsFailed() {return mNEventsFailed;}
inline mercedesStarStandardEventCut* mercedesStarStandardEventCut::Clone() { mercedesStarStandardEventCut* c = new mercedesStarStandardEventCut(*this); return c;}
inline mercedesStarStandardEventCut::mercedesStarStandardEventCut(mercedesStarStandardEventCut& c) : StHbtEventCut(c) {
  mEventMult[0] = c.mEventMult[0];
  mEventMult[1] = c.mEventMult[1];
  mVertZPos[0] = c.mVertZPos[0];
  mVertZPos[1] = c.mVertZPos[1];
  mNEventsPassed = 0;
  mNEventsFailed = 0;
}

#endif
