/**
 * $Id: StMiniMcHelper.h,v 1.1 2006/05/22 18:55:15 calderon Exp $
 * \file  Helper.h
 * \brief Various helper functions using StPhysicalHelix and StThreeVectors
 * 
 *
 * \author Bum Choi
 * \date   March 2001
 * $Log: StMiniMcHelper.h,v $
 * Revision 1.1  2006/05/22 18:55:15  calderon
 * Changes from the original code by Bum to comply with STAR coding standards.
 * First thing is to change the name of the "Helper" file to something that is more in line with the file naming convention.
 * This does not fully solve all possible hiccups, because all the functions
 * in the "helper" file are defined in global scope.
 *
 *  
 */
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

double dcaz(const StPhysicalHelixD& helix, const StThreeVectorF& point,
	    const StTrack* track=0);

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

double helixCrossingAngle(float pt, float phi, int sector);

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

//
// $Log $
//
