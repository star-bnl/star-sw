/**********************************************************
 * $Id: StRichArea.cxx,v 1.1 2000/04/03 19:36:07 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichArea.cxx,v $
 *  Revision 1.1  2000/04/03 19:36:07  horsley
 *  initial revision
 *
 *  
 *
 **********************************************************/

#include "StRichArea.h"
#include "StParticleDefinition.hh"
#include "StRichRingPoint.h"

#include "StRrsMaker/StRichGeometryDb.h"
#include "SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
using std::vector;
using namespace units;
#endif

StRichArea::StRichArea(StRichRingPoint* irp, 
		              StRichRingPoint* orp) {
  mInnerRing      = irp;
  mOuterRing      = orp;  
  mNumberOfSteps  = 1000.0;

  mPositiveDirection =  1.0;
  mNegativeDirection = -1.0;
  mSmallAngleStep    = 0.000174532925;  // ---> 0.01 degrees
  
  mSmallDistance  = 0.01*centimeter; 
  mCorrectForGap  = true;  // ---> here we are concerned with the gap correction

  myGeometryDb = StRichGeometryDb::getDb();

}

StRichArea::~StRichArea() { /* nopt */ }


double 
StRichArea::getTotalArea() {
  return mTotalArea;
}


double 
StRichArea::getPadPlaneArea() {
  return mPadPlaneArea;
}


void
StRichArea::calculateArea(double psiCut) {
  int i=0;

  double startPsi = M_PI;
  double stopInnerPsi,stopOuterPsi;
  
  getStoppingAngles(stopInnerPsi,stopOuterPsi,psiCut);
  mInnerAngleIncrement = (startPsi-stopInnerPsi)/mNumberOfSteps;
  mOuterAngleIncrement = (startPsi-stopOuterPsi)/mNumberOfSteps;
  
  mTotalArea    = 0.0;
  mPadPlaneArea = 0.0;
  // -----------  pi ---> 0 degs  ---------------------- //
  double innerPsi = startPsi;
  double outerPsi = startPsi; 
  mStatus = getRingPoints(innerPsi,outerPsi,mInXYA,mOutXYA,mPositiveDirection);
  for (i=0;i<mNumberOfSteps;i++) {
  
    if (getRingPoints(innerPsi,outerPsi,mInXYB,mOutXYB,mPositiveDirection)) {
      getAreaSegment(mInXYA,mOutXYA,mInXYB,mOutXYB,mTotalArea,mPadPlaneArea);
    
    }

    swapPoints(mInXYA,mOutXYA,mInXYB,mOutXYB);
  }
  
  // ---------   -pi ---> 0 degs  -------------------- //
  innerPsi = -startPsi;
  outerPsi = -startPsi;
  mStatus  = getRingPoints(innerPsi,outerPsi,mInXYA,mOutXYA,mNegativeDirection);
  for ( i=0;i<mNumberOfSteps;i++) {
  
    if (getRingPoints(innerPsi,outerPsi,mInXYB,mOutXYB,mNegativeDirection)) {
      getAreaSegment(mInXYA,mOutXYA,mInXYB,mOutXYB,mTotalArea,mPadPlaneArea);
    }
    
    swapPoints(mInXYA,mOutXYA,mInXYB,mOutXYB);

  }
  
}


void 
StRichArea::getStoppingAngles(double& ipsi, 
			               double& opsi, 
			               double psiCut) {
  StThreeVector<double> in,out;
  bool iStatus = false;
  bool oStatus = false;

  ipsi = 0.0;
  while (!iStatus) {
    iStatus = mInnerRing->getPoint(ipsi,in);
    if (!iStatus) ipsi = ipsi + mSmallAngleStep; 
  }

  opsi = 0.0;
  while (!oStatus) {
    oStatus = mOuterRing->getPoint(opsi,out);
    if (!oStatus) opsi = opsi + mSmallAngleStep; 
  }

  if (ipsi<psiCut) {ipsi = psiCut;}
  if (opsi<psiCut) {opsi = psiCut;}
  
    
  return;
}

bool
StRichArea::getRingPoints(double& iPsi, double& oPsi, 
			           StThreeVector<double>& ixy, 
			           StThreeVector<double>& oxy, 
			           int direction) {
 bool mInStatus,mOutStatus;
  
 mInStatus  = mInnerRing->getPoint(iPsi,ixy);
 mOutStatus = mOuterRing->getPoint(oPsi,oxy);
 
 iPsi       = iPsi - direction*mInnerAngleIncrement;  
 oPsi       = oPsi - direction*mOuterAngleIncrement;
 
 if (!mInStatus || !mOutStatus) return false;
 return true;
}


void
StRichArea::swapPoints(StThreeVector<double>& ixya ,StThreeVector<double>& oxya,
		              StThreeVector<double>& ixyb, StThreeVector<double>& oxyb) {
      
  // dont really have to swap the points, its 
  // enough to just set a = b
  ixya = ixyb;
  oxya = oxyb;
}

void
StRichArea::getAreaSegment(StThreeVector<double>& ixya,
			            StThreeVector<double>& oxya,
			            StThreeVector<double>& ixyb, 
			            StThreeVector<double>& oxyb,
			            double& totArea, 
                                    double& padArea) {
  
  if (outOfBoundsCorrection(ixya,oxya) && outOfBoundsCorrection(ixyb,oxyb) ) {
    
    // calculate total area segment here
    // no gap correction necessary    
    double term1,term2,term3,term4;
    term1 = (ixya.x()*oxya.y() - oxya.x()*ixya.y());
    term2 = (oxya.x()*oxyb.y() - oxyb.x()*oxya.y());
    term3 = (oxyb.x()*ixyb.y() - ixyb.x()*oxyb.y());
    term4 = (ixyb.x()*ixya.y() - ixya.x()*ixyb.y());
    totArea += 0.5*fabs(term1 + term2 + term3 + term4); 
    

    // calculate area segment that 
    // is actually on pad plane
    // i.e. do gap correction, if necessary
    // ---> specific case where inner and outer ring
    //          points are on different pad plane quadrants   
    StThreeVector<double> iEdgea,oEdgea,iEdgeb,oEdgeb;
    bool fullStatus1 = fullGapCorrectionNecessary(ixya,oxya,iEdgea,oEdgea);
    bool fullStatus2 = fullGapCorrectionNecessary(ixyb,oxyb,iEdgeb,oEdgeb); 
    bool areaSegmentDone = false;

    // gap correction !
    if (fullStatus1 && fullStatus2) { 
      double term1a,term2a,term3a,term4a,
	       term1b,term2b,term3b,term4b;
      term1a = (ixya.x()*iEdgea.y()   - iEdgea.x()*ixya.y());
      term2a = (iEdgea.x()*iEdgeb.y() - iEdgeb.x()*iEdgea.y());
      term3a = (iEdgeb.x()*ixyb.y()   - ixyb.x()*iEdgeb.y());
      term4a = (ixyb.x()*ixya.y()     - ixya.x()*ixyb.y());
      
      term1b = (oEdgea.x()*oxya.y()   - oxya.x()*oEdgea.y());
      term2b = (oxya.x()*oxyb.y()     - oxyb.x()*oxya.y());
      term3b = (oxyb.x()*oEdgeb.y()   - oEdgeb.x()*oxyb.y());
      term4b = (oEdgeb.x()*oEdgea.y() - oEdgea.x()*oEdgeb.y());
      
      padArea += 0.5*fabs(term1a + term2a + term3a + term4a) +
	         0.5*fabs(term1b + term2b + term3b + term4b);
      areaSegmentDone = true;
    }
    

    // here we correct for one point in gap, 
    // the other on padplane
    bool partialStatus1 = partialGapCorrectionNecessary(ixya,oxya);
    bool partialStatus2 = partialGapCorrectionNecessary(ixyb,oxyb); 
    
    if ( !areaSegmentDone && partialStatus1  && partialStatus2 ) {
      
      double term1c,term2c,term3c,term4c;
      term1c = (ixya.x()*oxya.y() - oxya.x()*ixya.y());
      term2c = (oxya.x()*oxyb.y() - oxyb.x()*oxya.y());
      term3c = (oxyb.x()*ixyb.y() - ixyb.x()*oxyb.y());
      term4c = (ixyb.x()*ixya.y() - ixya.x()*ixyb.y());
      padArea += 0.5*fabs(term1c + term2c + term3c + term4c); 
      areaSegmentDone = true;

    }    
    
    // here we check if either set of points are
    // fully inside the gap, 
    // if so, then don't do anything, as the padplane 
    // area shouldn't be added to in this case
    if ( (gapCheck(ixya) && gapCheck(oxya)) ||
	  (gapCheck(ixyb) && gapCheck(oxyb))) {
      areaSegmentDone = true;
    }


    // here no gap correction is necesary
    if (!areaSegmentDone) {
      padArea += 0.5*fabs(term1 + term2 + term3 + term4); 
    }
    
  }
  
}


bool 
StRichArea::fullGapCorrectionNecessary(StThreeVector<double>& ixy,   
				                    StThreeVector<double>& oxy,
				                    StThreeVector<double>& itemp, 
				                    StThreeVector<double>& otemp) {
  

  if ( !mCorrectForGap )   return false;

  // checks if one (or both) point(s) in gap
  if ( gapCheck(ixy) || gapCheck(oxy) )  return false;
  
  // checks if both points in same quadrant
  // if not then necessary to do gap correction
  if ( quadCheck(ixy,oxy) ) return false;
  

  /////////////   do gap correction   //////////////////
  // here we get the point closest to the outer point
  double phi = atan2(ixy.y()-oxy.y(),ixy.x()-oxy.x());    
  StThreeVector<double> tempo,tempi,tempa;
  StThreeVector<double> temp(100.0*mSmallDistance*centimeter,
			       100.0*mSmallDistance*centimeter,
			       100.0*mSmallDistance*centimeter);
  tempo = oxy;
  tempi = ixy;
  
  while (temp.mag()>mSmallDistance) {
    temp = tempi-tempo;
    double v = 0.5*temp.mag();
    tempa.setX(tempo.x() + v*cos(phi));
    tempa.setY(tempo.y() + v*sin(phi));
    if (quadCheck(oxy,tempa)) {tempo = tempa;} 
    else                     {tempi = tempa;}
  }
  otemp = tempa;
  

  // here we get the point closest to the inner point
  phi = atan2(oxy.y()-ixy.y(),oxy.x()-ixy.x());
  
  // set temp distance to some distance 
  // bigger than mSmallDistance
  temp.setX(100.0*mSmallDistance*centimeter);
  temp.setY(100.0*mSmallDistance*centimeter);
  temp.setZ(100.0*mSmallDistance*centimeter);

  tempo = oxy;
  tempi = ixy;
 
  while (temp.mag()>mSmallDistance) {
    temp = tempi - tempo;
    double v = 0.5*temp.mag();
    tempa.setX(tempi.x() + v*cos(phi));
    tempa.setY(tempi.y() + v*sin(phi));
    if (quadCheck(ixy,tempa)) {tempi = tempa;} 
    else                     {tempo = tempa;}
  }
  itemp = tempa;
  
  return true;
}

bool StRichArea::partialGapCorrectionNecessary(StThreeVector<double>& ixy,   
				                              StThreeVector<double>& oxy) {
  // if neither point in gap, no correction necessary
  if ( !gapCheck(ixy) && !gapCheck(oxy) ) {
    return false;
  }


  // if both points fall in gap, no correction possible
  if ( gapCheck(ixy) && gapCheck(oxy) ) {
    return false;
  }
    
    

  StThreeVector<double> tempo,tempi,tempa,temp;
  double phi;
  // inner ring point falls in gap, outer ok
  if (gapCheck(ixy)) {
    phi = atan2(ixy.y()-oxy.y(),ixy.x()-oxy.x());    
    temp.setX(100.0*mSmallDistance*centimeter);
    temp.setY(100.0*mSmallDistance*centimeter);
    temp.setZ(100.0*mSmallDistance*centimeter);
    
    tempo = oxy;
    tempi = ixy;
    
    int counter=0;
    while (temp.mag()>mSmallDistance) {
      temp = tempi - tempo;
      double v = 0.5*temp.mag();
      tempa.setX(tempo.x() + v*cos(phi));
      tempa.setY(tempo.y() + v*sin(phi));
      if (quadCheck(oxy,tempa)) {tempo = tempa;} 
      else                     {tempi = tempa;}
      if (counter>10000) break;
    }
    ixy = tempa;
    
    return true;
  }
  
  // outer ring point falls in gap, inner ok
  if (gapCheck(oxy)) {
   
    phi = atan2(ixy.y()-oxy.y(),ixy.x()-oxy.x());    
    temp.setX(100.0*mSmallDistance*centimeter);
    temp.setY(100.0*mSmallDistance*centimeter);
    temp.setZ(100.0*mSmallDistance*centimeter);
    
    tempo = oxy;
    tempi = ixy;
    
    int counter=0;
    
    while (temp.mag()>mSmallDistance) {
      temp = tempi - tempo;
      double v = 0.5*temp.mag();
      tempa.setX(tempo.x() + v*cos(phi));
      tempa.setY(tempo.y() + v*sin(phi));
      if (quadCheck(ixy,tempa)) {tempi = tempa;} 
      else                     {tempo = tempa;}
      if (counter>10000) break;
    
    }
    oxy = tempa;
    
    return true;
  }


  return false;
}


bool StRichArea::gapCheck(StThreeVector<double>& xy) {

  double xGapWidth = myGeometryDb->quadrantGapInX()/2.0;
  double yGapWidth = myGeometryDb->quadrantGapInY()/2.0;

  if ( fabs(xy.x()) < xGapWidth) return true;
  if ( fabs(xy.y()) < yGapWidth) return true;

  return false;
}


bool StRichArea::quadCheck(StThreeVector<double>& ixy,
			           StThreeVector<double>& oxy) {
 
  double gapWidth = myGeometryDb->quadrantGapInX()/2.0;
  
  // upper quad right  
  if (ixy.x() > gapWidth && oxy.x() > gapWidth &&
      ixy.y() > gapWidth && oxy.y() > gapWidth) return true;

  // upper quad left
  if (ixy.x() < -gapWidth && oxy.x() < -gapWidth &&
      ixy.y() > gapWidth && oxy.y() > gapWidth) return true;

  // lower quad right
  if (ixy.x() > gapWidth && oxy.x() > gapWidth &&
      ixy.y() < -gapWidth && oxy.y() < -gapWidth) return true;

  // lower quad left
  if (ixy.x() < -gapWidth && oxy.x() < -gapWidth &&
      ixy.y() < -gapWidth && oxy.y() < -gapWidth) return true;
  return false;
}


bool 
StRichArea::inBounds(StThreeVector<double> xy) {
  
  
  
  if ( (xy.x() > -myGeometryDb->radiatorDimension().x() && 
	xy.x() <  myGeometryDb->radiatorDimension().x() ) && 
       
       (xy.y() > -myGeometryDb->radiatorDimension().y() && 
	xy.y() <  myGeometryDb->radiatorDimension().y() ) ) { 
    return true;
  }
  
  return false;
}


bool 
StRichArea::outOfBoundsCorrection(StThreeVector<double>& ixy, StThreeVector<double>& oxy) {
 
  if (inBounds(oxy) && inBounds(ixy)) {
    return true;
  }
  
  if (inBounds(oxy)) {
      double phi = atan2(ixy.y()-oxy.y(),ixy.x()-oxy.y());    
      StThreeVector<double> tempo,tempi,tempa;
      StThreeVector<double> temp(100.0*mSmallDistance,
				   100.0*mSmallDistance,
				   100.0*mSmallDistance);
      tempo = oxy;
      tempi = ixy;
      while (temp.mag()>mSmallDistance) {
	temp = tempi-tempo;
	double v = 0.5*temp.mag();
	tempa.setX(tempo.x() + v*cos(phi));
	tempa.setY(tempo.y() + v*sin(phi));
	if (inBounds(tempa))  {tempi = tempa;} 
	if (!inBounds(tempa)) {tempo = tempa;}
      }
      ixy = tempa;
      return true;
  }
  
 
  if (inBounds(ixy))  {
    double phi = atan2(oxy.y()-ixy.y(),oxy.x()-ixy.x());    
    StThreeVector<double> tempo,tempi,tempa;
      StThreeVector<double> temp(100.0*mSmallDistance,
				   100.0*mSmallDistance,
				   100.0*mSmallDistance);
    tempo = oxy;
    tempi = ixy;
    while (temp.mag()>mSmallDistance)  {
      temp = tempi - tempo;
      double v = 0.5*temp.mag();
      tempa.setX(tempi.x() + v*cos(phi));
      tempa.setY(tempi.y() + v*sin(phi));
      if (inBounds(tempa))  {tempi = tempa;} 
      if (!inBounds(tempa)) {tempo = tempa;}
    }
    oxy = tempa;
    return true;
  }
  
  // if the points are not on the pad plane return false
  return false;
  
}
