/***************************************************************************
 *
 * $Id: StMuHelix.h,v 1.5 2004/05/02 04:10:14 perev Exp $
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

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif


class StMuHelix : public TObject {
 public:
  StMuHelix() { /* no-op */ }
  StMuHelix(StPhysicalHelixD hh, double field);
  const StThreeVectorF &p() const;
  const StThreeVectorF &origin() const;
  short q() const;
  float b() const;
  int   bad() const;
 protected:
  StThreeVectorF mP;
  StThreeVectorF mOrigin;
  short mQ;
  float mB;
  ClassDef(StMuHelix,1)
};


inline const StThreeVectorF &StMuHelix::p() const { return mP; }
inline const StThreeVectorF &StMuHelix::origin() const { return mOrigin; }
inline short StMuHelix::q() const { return mQ; }
inline float StMuHelix::b() const { return mB; }

#endif

/***************************************************************************
 *
 * $Log: StMuHelix.h,v $
 * Revision 1.5  2004/05/02 04:10:14  perev
 * private => protected
 *
 * Revision 1.4  2003/10/28 18:57:56  perev
 * BadData protection added
 *
 * Revision 1.3  2003/01/23 21:59:50  laue
 * Modification to compile on Solaris.
 *
 * Revision 1.2  2002/03/20 16:04:11  laue
 * minor changes, mostly added access functions
 *
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/
