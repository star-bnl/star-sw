/***************************************************************************
 *
 * $Id: StarMuHelix.cxx,v 1.1 2002/03/05 15:41:09 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include "StarMuHelix.h"

StarMuHelix::StarMuHelix(StPhysicalHelixD hh, double field) {
  mP = hh.momentum(field*kilogauss);
  mOrigin = hh.origin();
  mQ = hh.charge( field*kilogauss );
  mB = field;
}

ClassImp(StarMuHelix)
/***************************************************************************
 *
 * $Log: StarMuHelix.cxx,v $
 * Revision 1.1  2002/03/05 15:41:09  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/
