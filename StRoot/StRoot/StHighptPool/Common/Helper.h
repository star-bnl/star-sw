/***************************************************************************
 *
 * $Id: Helper.h,v 1.1 2002/05/31 22:03:18 jklay Exp $                                      
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  This class contains methods used by StHiMicroMaker to 
 *		 generate highpt uDST's from StEvent for highpt Analysis.
 *
 ***************************************************************************
 *
 * $Log: Helper.h,v $
 * Revision 1.1  2002/05/31 22:03:18  jklay
 * Collected common utilities into one location
 *
 * Revision 1.2  2002/04/03 00:37:41  jklay
 * Fixed some bugs, added new version of dcaz
 *
 * Revision 1.2  2002/04/03 00:23:27  jklay
 * Fixed private member access bugs in analysis code
 *
 * Revision 1.1  2002/04/02 20:00:41  jklay
 * Bums highpt uDST Maker
 *
 *
 **************************************************************************/
#ifndef Helper_HH
#define Helper_HH

#include "StEventTypes.h"
#include "StPhysicalHelixD.hh"
//#include "StThreeVectorD.hh"
//#include "StThreeVectorF.hh"


double distance(double a1, double b1, double a2, double b2);
double distance(const StThreeVectorF& point1,
		const StThreeVectorF& point2);

double dca3d(const StPhysicalHelixD&, const StThreeVectorF& point);

double dca2d(const StPhysicalHelixD&, const StThreeVectorF& point,
	     const StThreeVectorF* origin=0);

double dcaz(const StPhysicalHelixD&, const StThreeVectorF& point, 
	    const StTrack* track=0);

/*	This was replaced by the above function - JLK 2.APR.2002
double dcaz(const StPhysicalHelixD&, const StThreeVectorF& point);
*/

double crossingAngle(const StPhysicalHelixD& helix,
		     const StTpcHit* hit, float bField=0);

double padrowDca(const StPhysicalHelixD& helix,
		 const StTpcHit* hit);

// helix stuff
//
double propagateToPadrow(const StPhysicalHelixD& helix,
			 const StTpcHit* hit);
double helixPadrowDca(const StPhysicalHelixD& helix,
		      const StTpcHit* hit);
double helixCrossingAngle(const StPhysicalHelixD& helix, 
		     const StTpcHit* hit, float bField);

double helixDca2D(const StPhysicalHelixD&,const StThreeVectorF& point);


// straight line residual stuff
//
StThreeVectorF lineAt2d(const StPhysicalHelixD&, const StThreeVectorF& point);

double 
linePadrowDca(const StPhysicalHelixD& helix,
	      const StTpcHit* hit);

double
lineCrossingAngle(const StPhysicalHelixD& helix,
		  const StTpcHit* hit);

double
lineCrossingAngle(const StPhysicalHelixD& helix,
		  const int sector);

double
lineDca2D(const StPhysicalHelixD& helix,
	  const StThreeVectorF& hit,
	  const StThreeVectorF& origin);

#endif
