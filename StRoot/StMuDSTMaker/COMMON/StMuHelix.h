/***************************************************************************
 *
 * $Id: StMuHelix.h,v 1.1 2002/03/08 17:04:18 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StMuHelix_h
#define StMuHelix_h

#include "StarClassLibrary/SystemOfUnits.h"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StThreeVectorF.hh"

#include "TObject.h"

class StMuHelix : public TObject {
 public:
  StMuHelix() { /* no-op */ }
  StMuHelix(StPhysicalHelixD hh, double field);
  StThreeVectorF p();
  StThreeVectorF origin();
  short q();
  float b();
 private:
  StThreeVectorF mP;
  StThreeVectorF mOrigin;
  short mQ;
  float mB;
  ClassDef(StMuHelix,1)
};


inline StThreeVectorF StMuHelix::p() { return mP; }
inline StThreeVectorF StMuHelix::origin() { return mOrigin; }
inline short StMuHelix::q() { return mQ; }
inline float StMuHelix::b() { return mB; }

#endif

/***************************************************************************
 *
 * $Log: StMuHelix.h,v $
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/
