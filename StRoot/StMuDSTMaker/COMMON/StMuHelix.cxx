/***************************************************************************
 *
 * $Id: StMuHelix.cxx,v 1.2 2003/10/28 18:57:56 perev Exp $
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

int StMuHelix::bad() const 
{
   double qwe;
   qwe = mOrigin.x();
   if (!::finite(qwe))		return 1;
   if (fabs(qwe) > 1000) 	return 1;

   qwe = mOrigin.y();
   if (!::finite(qwe))		return 2;
   if (fabs(qwe) > 1000) 	return 2;
   
   qwe = mOrigin.z();
   if (!::finite(qwe))		return 3;
   if (fabs(qwe) > 1000) 	return 3;

   qwe = mP.x();
   if (!::finite(qwe))		return 4;
   if (fabs(qwe) > 1000) 	return 4;

   qwe = mP.y();
   if (!::finite(qwe))		return 5;
   if (fabs(qwe) > 1000) 	return 5;
   
   qwe = mP.z();
   if (!::finite(qwe))		return 6;
   if (fabs(qwe) > 1000) 	return 7;

   qwe = fabs(mP.x())+fabs(mP.y());
   if (qwe <=1.e-10)   		return 7;

   if (abs(mQ)  >    10)        return 8;

   if (!::finite(mB))           return 9;
   if (fabs(mB) >  1000)        return 9;
   return 0;
}
ClassImp(StMuHelix)
/***************************************************************************
 *
 * $Log: StMuHelix.cxx,v $
 * Revision 1.2  2003/10/28 18:57:56  perev
 * BadData protection added
 *
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/
