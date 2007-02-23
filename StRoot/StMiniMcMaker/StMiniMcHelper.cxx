/**
 * $Id: StMiniMcHelper.cxx,v 1.2 2007/02/23 17:07:41 fisyak Exp $
 * \file  Helper.cxx
 * \brief Assorted utility functions.  Helix DCA's(2D, 3D, signed), crossing angles, padrow propagation.
 * 
 *
 * \author Bum Choi
 * \date   March 2001
 * $Log: StMiniMcHelper.cxx,v $
 * Revision 1.2  2007/02/23 17:07:41  fisyak
 * Resolve bug #682
 *
 * Revision 1.1  2006/05/22 18:55:14  calderon
 * Changes from the original code by Bum to comply with STAR coding standards.
 * First thing is to change the name of the "Helper" file to something that is more in line with the file naming convention.
 * This does not fully solve all possible hiccups, because all the functions
 * in the "helper" file are defined in global scope.
 *
 *  
 */

#include "StMiniMcHelper.h"
#include <cmath>
#include "TMath.h"

//#include "StPhysicalHelixD.hh"
#include "TRandom.h"

// from ben norman.
// global X & Y coordinates of X unit vector in local coords, by sector
// (X => parallel to padrow in XY plane, right handed wrt Y)
static Double_t sectorX[] = {
  0, 0,
  0.866025403784439, -0.5,
  0.5, -0.866025403784439,
  0, -1,
  -0.5, -0.866025403784439,
  -0.866025403784439, -0.5,
  -1, 0,
  -0.866025403784439, 0.5,
  -0.5, 0.866025403784438,
  0, 1,
  0.5, 0.866025403784439,
  0.866025403784438, 0.5,
  1, 0,
  0.866025403784439, 0.5,
  0.5, 0.866025403784438,
  0, 1,
  -0.5, 0.866025403784439,
  -0.866025403784439, 0.5,
  -1, 0,
  -0.866025403784439, -0.5,
  -0.5, -0.866025403784439,
  0, -1,
  0.5, -0.866025403784439,
  0.866025403784438, -0.5,
  1, 0
};

// global X & Y coordinates of Y unit vector in local coords, by sector
// (Y => normal to padrow in XY plane, radially outward from Z axis)
static Double_t sectorY[] = {
  0,0,
  0.5, 0.866025403784439,
  0.866025403784439, 0.5,
  1, 0,
  0.866025403784439, -0.5,
  0.5, -0.866025403784439,
  0, -1,
  -0.5, -0.866025403784439,
  -0.866025403784439, -0.5,
  -1, 0,
  -0.866025403784439, 0.5,
  -0.5, 0.866025403784438,
  0, 1,
  -0.5, 0.866025403784439,
  -0.866025403784439, 0.5,
  -1, 0,
  -0.866025403784439, -0.5,
  -0.5, -0.866025403784439,
  0, -1,
  0.5, -0.866025403784439,
  0.866025403784438, -0.5,
  1, 0,
  0.866025403784438, 0.5,
  0.5, 0.866025403784439,
  0, 1
};

//--------------------------------------------------------------
/*
  distance in 2d
 */

double distance(double a1, double b1, double a2, double b2){
  return ::sqrt( (a1-b1)*(a1-b1) + (a2-b2)*(a2-b2) );
}

/*
  distance in 3d
*/
double distance(const StThreeVectorF& point1,
		const StThreeVectorF& point2)
{
  return (point1-point2).mag();
}


double dca3d(const StPhysicalHelixD& helix, const StThreeVectorF& point)
{
  return helix.distance(point);

}

double dca2d(const StPhysicalHelixD& helix, const StThreeVectorF& point,
	     const StThreeVectorF* origin)
{
  if(fabs(helix.curvature())<=static_cast<double>(0)){
    return lineDca2D(helix,point,*origin);
  }
  else{
    return helixDca2D(helix,point);
  }
}


//
// i like this one better. dip angle in s-z plane 
// should be exact assuming a circle in x-y
// (redundant StTrack* for debugging)
double dcaz(const StPhysicalHelixD& helix, const StThreeVectorF& point,
	    const StTrack* track)
{
  double z0       = helix.origin().z();  
  double phi      = atan2(point.y()-helix.ycenter(), 
			  point.x()-helix.xcenter());
  int    h        = helix.h(); // -sign(q*B) (+ := ccw looking down from +z)
  double dphi     = h*(phi-helix.phase());
  
  // half circle assumption 
  dphi           = (fabs(dphi) < M_PI ) ? dphi : 
                    ((dphi<0) ? 2*M_PI + dphi : 2*M_PI - dphi);
  
  double arclength= (1./helix.curvature()) * dphi;
  
  double dcaZ =  (point.z() - (z0 + arclength*tan(helix.dipAngle())));
  /*
  if(gRandom->Rndm(1)<0.1){
    cout << "--------" << endl;
    cout << "zpoint=" << point.z() << endl;
    if(track)
      cout << "pt=" << track->geometry()->momentum().perp() 
	   << ",fit pts=" << track->fitTraits().numberOfFitPoints(kTpcId)
	   << endl;
    
    cout << ">>>zdca=" << dcaZ
	 << ", dphi=" << dphi*180./M_PI  
	 << ", z0=" << z0 << ",arclength=" << arclength << endl
	 << ", dip=" << helix.dipAngle()*180./M_PI
	 << ", arc/tan(dip)="<< arclength/tan(helix.dipAngle()) << endl
	 << ">>>dca.z= " << point.z()-helix.at(helix.pathLength(point)).z()
	 << endl;
    cout << "--------" << endl;
  }
  */
  return dcaZ;

}

/*
double dcaz(const StPhysicalHelixD& helix, const StThreeVectorF& point)
{
  pairD path = helix.pathLength(point.perp());
  
  const StThreeVectorD& pos1 = helix.at(path.first);
  const StThreeVectorD& pos2 = helix.at(path.second);
  const StThreeVectorD dis1 = point - pos1;
  const StThreeVectorD dis2 = point - pos2;
  
  double dcaZ = (dis1.mag() < dis2.mag()) ? dis1.z() : dis2.z();
  if(isnan(dcaZ)) return 999;
  return dcaZ;
}
*/

double crossingAngle(const StPhysicalHelixD& helix,
		     const StTpcHit* hit, float bField)
{
  if(fabs(helix.curvature())<=static_cast<double>(0)){
    return lineCrossingAngle(helix,hit);
  }
  else{
    return helixCrossingAngle(helix,hit,bField);
  }
}

double padrowDca(const StPhysicalHelixD& helix,
		 const StTpcHit* hit)
{
  if(fabs(helix.curvature())<=static_cast<double>(0)){
    return linePadrowDca(helix,hit);
  }
  else{
    return helixPadrowDca(helix,hit);
  }
}

//-----------------------------------------------------


// from ben

double
propagateToPadrow(const StPhysicalHelixD& helix,
		  const StTpcHit* hit)
{
  // calculate intersection of helix (circle) with hit's padrow.
  // There's a little trig to do, and 4 cases, but the net result is
  // that if we define
  // 
  // R = radius of helix's circular projection
  // d = distance from center of circle to hit
  // x = signed displacement from hit to track in sector's local x direction
  // dHat = unit vector pointing from center to hit
  // xHat = unit vector pointing along the padrow (x direction in sector's
  //        local coordinates)
  // cos(theta) = dHat . xHat
  //
  // Then
  //
  // x = -d*cos(theta) +- ::sqrt(R^2 - d^2*(1-cos^2(theta)))
  // 
  // '-' is used if cos(theta)<0, and '+' if cos(theta)>0.  The overall sign
  // is determined by whether or not the hit is in the circle, as above.

  double R = 1./helix.curvature();
  double dX = hit->position().x() - helix.xcenter();
  double dY = hit->position().y() - helix.ycenter();

  // before continuing, make sure that there *is* a solution, i.e., that the
  // circle does intersect the padrow.  We determine this by projecting the
  // vector between the hit and the circle's center onto the padrow's radial
  // normal.  If this is >= R, we have no intersection and return the helix
  // path length to the point nearest to the padrow.
  
  double d = TMath::Sqrt(dX*dX + dY*dY);
  double dPerp = dX*sectorY[2*hit->sector()] +     
                   dY*sectorY[2*hit->sector() + 1];  
  
  double x;
  if(dPerp >= R){
    // this shouldn't happen for hi-pt, but...
    //
    // take cross product (Sin) to find displacement along padrow to point
    // nearest circle
    x = - dX*sectorY[2*hit->sector() + 1] + // padrow normal x d    
          dY*sectorY[2*hit->sector()]; 
  }else{
    // find analytic solution of intersection
    double cosTheta = (dX*sectorX[2*hit->sector()] + 
                         dY*sectorX[2*hit->sector() + 1])/d;
    
    x = -d*cosTheta + (cosTheta<0 ? -1. : 1.) * 
        TMath::Sqrt(R*R - d*d*(1 - cosTheta*cosTheta));
  }

  // finally, propegate along local X direction x units from hit to track
  // (reuse dX,dY)
  dX = hit->position().x() + x*sectorX[2*hit->sector()];
  dY = hit->position().y() + x*sectorX[2*hit->sector() + 1];

  /*
  if(mDebug){
    cout << "  Position on helix & padrow in sector " << hit->sector()
         << " closest to point '" << hit->position() << "' is (" << dX << ", "
         << dY << ").   ";
  }
  */
  double s = helix.pathLength(dX, dY);

  /* non-analytic solution in helix, not used
  //   normal to plane
  StThreeVectorD n(sectorY[2*hit->sector()], sectorY[2*hit->sector() + 1], 0);
  //   point on plane
  double s = helix.pathLength(hit->position(), n);
  */
  return s;

}



double
helixPadrowDca(const StPhysicalHelixD& helix,
	       const StTpcHit* hit)
{
  // let propegateToPadrow do the work of finding the intersection of
  // the helix with the padrow
  double s = propagateToPadrow(helix, hit);
  double dX = hit->position().x() - helix.x(s);
  double dY = hit->position().y() - helix.y(s);
  double dca = TMath::Sqrt(dX*dX + dY*dY);

  // get our sign from the simple xyDca calculation
  if (helixDca2D(helix, hit->position()) < 0) dca *= -1.;

  /*
  if(mDebug){
    cout << "  Dca of helix '" << helix << "' to hit '"
         << hit->position() << "' along padrow is " << dca << endl;
  }
  */
  return dca;
}

double helixCrossingAngle(float pt,float phi,int sector)
{
  double px=pt*cos(phi);
  double py=pt*sin(phi);
  double cosTheta = (px*sectorY[2*sector] + 
                       py*sectorY[2*sector + 1])/pt;

  if (cosTheta < 0){
    cosTheta *= -1.;
    px *= -1; py *= -1;
  }

  double theta = TMath::ACos(cosTheta);
  if (px*sectorY[2*sector + 1] - 
      py*sectorY[2*sector] > 0) theta *= -1.;

  //  if(mDebug) cout << " finally theta=" << theta << endl;

  return theta;
}


double
helixCrossingAngle(const StPhysicalHelixD& helix, 
		   const StTpcHit* hit,
		   float bField){ 

  // first, we need the track momentum direction at the hit's padrow.
  double s = propagateToPadrow(helix, hit);

  // to suppress warning messages
  StPhysicalHelixD helixTemp(helix);
  
  StThreeVectorD p(helixTemp.momentumAt(s, bField));

  // calculate the cosine of the angle between the radially outward normal
  // to the padrow (i.e. the local Y unit vector) and the track tangent.
  double cosTheta = (p.x()*sectorY[2*hit->sector()] + 
                       p.y()*sectorY[2*hit->sector() + 1])/p.perp();
  
  /*
    if(mDebug){
    cout << "  Cos of rossing angle of helixTemp '" << helixTemp << "' in sector "
    << hit->sector() << " is " << cosTheta << ", ";
    }
  */
  // if the cosine is negative, take the inverse of the momentum vector
  // to bring it between 0 and pi/2.
  if (cosTheta < 0){
    cosTheta *= -1.;
    p.setX(-p.x());
    p.setY(-p.y());
  }

  //if(mDebug) cout << " changed to " << cosTheta << ", ";

  // use the cross product to determine whether this angle should be
  // negative or positive.
  // if cross product is along positive z, then p was clockwise from sectorY
  double theta = TMath::ACos(cosTheta);
  if (p.x()*sectorY[2*hit->sector() + 1] - 
      p.y()*sectorY[2*hit->sector()] > 0) theta *= -1.;

  //  if(mDebug) cout << " finally theta=" << theta << endl;

  return theta;
} // crossingAngle

double helixDca2D(const StPhysicalHelixD &helix, const StThreeVectorF& point)
{
   return (distance(helix.xcenter(),point.x(),helix.ycenter(),point.y())
	  -(1./helix.curvature()));
}

/*
  return StThreeVectorF of point on the line closet to the input point
  in xy plane.  z coordinate is ignored.
  
*/

StThreeVectorF
lineAt2d(const StPhysicalHelixD& helix,
	  const StThreeVectorF& point)
{
  // note: phase for straight lines is defined as phase=psi-pi/2.
  // where psi is the 'direction' the line is pointing. 
  // the relationship b/psi and the slope is 
  // slope = tan(psi).

  const StThreeVectorD& origin = helix.origin();
  double cosPhase = cos(helix.phase());
  double sinPhase = sin(helix.phase());
  
  double px = point.x();
  double py = point.y();
  double dx = px-origin.x();
  double dy = py-origin.y();

  double s = cosPhase*dy-sinPhase*dx;
  
  // point on line closest to point in xy plane.
  double xDca = origin.x() - s*sinPhase;
  double yDca = origin.y() + s*cosPhase;

  StThreeVectorF vec(xDca,yDca,0); // z coordinate ignored
  return vec;
}




double 
linePadrowDca(const StPhysicalHelixD& helix,
	      const StTpcHit* hit)
{
  // note: phase for straight lines is defined as phase=psi-pi/2.
  // where psi is the 'direction' the line is pointing. 
  // the relationship b/psi and the slope is 
  // slope = tan(psi).

   // point on line closest to hit in xy plane.
 
  StThreeVectorF at = lineAt2d(helix,hit->position());

  // vector perpendicular to the line in xy
  StThreeVectorF perp = (hit->position() - at);

  // padplane dca
  double cosTheta = 
    (perp.x()*sectorX[2*hit->sector()] +
     perp.y()*sectorX[2*hit->sector()+1])/perp.perp();
  
  double padrowDca = perp.perp()/fabs(cosTheta);

  // get sign from lineDca2D
  StThreeVectorF origin(0,0,0);
  double sign = lineDca2D(helix,hit->position(),origin);

  return (sign>=0) ? padrowDca : -padrowDca;
}

double
lineCrossingAngle(const StPhysicalHelixD& helix,
		  const StTpcHit* hit)
{
  // psi
  double psi = helix.phase() + TMath::Pi()/2.;
  double psiX = cos(psi);
  double psiY = sin(psi);

  double cosTheta =
    psiX*sectorY[2*hit->sector()] + psiY*sectorY[2*hit->sector()+1];

  // should never be negative
  if(cosTheta<0){
    cout << "%%% line crossing angle negative? "
	 << cosTheta << endl;
    return 99;
  }

  // sign from the cross product (psi X sector Normal).
  double sign = psiX*sectorY[2*hit->sector()+1]-psiY*sectorY[2*hit->sector()];

  return (sign>0) ? acos(cosTheta) : -acos(cosTheta);
}

double
lineCrossingAngle(const StPhysicalHelixD& helix,
		  const int sector)
{
  // psi
  double psi = helix.phase() + TMath::Pi()/2.;
  double psiX = cos(psi);
  double psiY = sin(psi);

  double cosTheta =
    psiX*sectorY[2*sector] + psiY*sectorY[2*sector+1];

  // should never be negative
  if(cosTheta<0){
    cout << "%%% line crossing angle negative? "
	 << cosTheta << endl;
    return 99;
  }

  // sign from the cross product (psi X sector Normal).
  double sign = psiX*sectorY[2*sector+1]-psiY*sectorY[2*sector];

  return (sign>0) ? acos(cosTheta) : -acos(cosTheta);
}

double
lineDca2D(const StPhysicalHelixD& helix,
	  const StThreeVectorF& hit,
	  const StThreeVectorF& origin)
{
  // point on line closest to hit (in xy plane)
  StThreeVectorF atHit = lineAt2d(helix,hit);

  // point on line closest to 'origin' in xy
  StThreeVectorF atOrigin = lineAt2d(helix,origin);

  // vector from atOrigin to atHit
  StThreeVectorF origin2atHit = (atHit-atOrigin);
  
  // vector fom atOrigin to hit
  StThreeVectorF origin2hit = (hit-atOrigin);

  // vector perpendicular to the line to hit.
  StThreeVectorF dca = (hit-atHit);

  // sign determined from origin2atHit X orgin2hit
  double cross = origin2atHit.x()*origin2hit.y() 
    - origin2atHit.y()*origin2hit.x();

  return (cross>=0) ? dca.perp() : - dca.perp();

}

//
// $Log $
//
