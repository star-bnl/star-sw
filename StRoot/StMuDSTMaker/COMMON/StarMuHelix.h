/***************************************************************************
 *
 * $Id: StarMuHelix.h,v 1.1 2002/03/05 15:41:09 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#ifndef StarMuHelix_h
#define StarMuHelix_h

#include "StarClassLibrary/SystemOfUnits.h"
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StThreeVectorF.hh"

#include "TObject.h"

class StarMuHelix : public TObject {
 public:
  StarMuHelix() { /* no-op */ }
  StarMuHelix(StPhysicalHelixD hh, double field);
  StThreeVectorF p();
  StThreeVectorF origin();
  short q();
  float b();
 private:
  StThreeVectorF mP;
  StThreeVectorF mOrigin;
  short mQ;
  float mB;
  ClassDef(StarMuHelix,1)
};


inline StThreeVectorF StarMuHelix::p() { return mP; }
inline StThreeVectorF StarMuHelix::origin() { return mOrigin; }
inline short StarMuHelix::q() { return mQ; }
inline float StarMuHelix::b() { return mB; }

#endif

/***************************************************************************
 *
 * $Log: StarMuHelix.h,v $
 * Revision 1.1  2002/03/05 15:41:09  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/
