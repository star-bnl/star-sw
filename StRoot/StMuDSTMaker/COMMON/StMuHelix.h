/***************************************************************************
 *
 * $Id: StMuHelix.h,v 1.2 2002/03/20 16:04:11 laue Exp $
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
  StThreeVectorF p() const;
  StThreeVectorF origin() const;
  short q() const;
  float b() const;
 private:
  StThreeVectorF mP;
  StThreeVectorF mOrigin;
  short mQ;
  float mB;
  ClassDef(StMuHelix,1)
};


inline StThreeVectorF StMuHelix::p() const { return mP; }
inline StThreeVectorF StMuHelix::origin() const { return mOrigin; }
inline short StMuHelix::q() const { return mQ; }
inline float StMuHelix::b() const { return mB; }

#endif

/***************************************************************************
 *
 * $Log: StMuHelix.h,v $
 * Revision 1.2  2002/03/20 16:04:11  laue
 * minor changes, mostly added access functions
 *
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/
