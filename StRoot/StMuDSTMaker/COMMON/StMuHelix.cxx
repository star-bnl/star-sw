/***************************************************************************
 *
 * $Id: StMuHelix.cxx,v 1.1 2002/03/08 17:04:18 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include "StMuHelix.h"

StMuHelix::StMuHelix(StPhysicalHelixD hh, double field) {
  mP = hh.momentum(field*kilogauss);
  mOrigin = hh.origin();
  mQ = hh.charge( field*kilogauss );
  mB = field;
}

ClassImp(StMuHelix)
/***************************************************************************
 *
 * $Log: StMuHelix.cxx,v $
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/
