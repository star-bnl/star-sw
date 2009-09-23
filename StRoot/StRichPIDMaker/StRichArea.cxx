/**********************************************************
 * $Id: StRichArea.cxx,v 2.9 2009/09/23 05:19:30 fine Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichArea.cxx,v $
 *  Revision 2.9  2009/09/23 05:19:30  fine
 *  add assert and file #1648 RT
 *
 *  Revision 2.8  2003/04/30 20:38:06  perev
 *  Warnings cleanup. Modified lines marked VP
 *
 *  Revision 2.7  2001/06/22 15:05:18  jeromel
 *  Removed unused variables cosineOfD idist testangle
 *
 *  Revision 2.6  2001/04/25 00:31:40  lasiuk
 *  HP changes.  removal of reprocessTraits()
 *
 *  Revision 2.5  2001/04/17 18:21:48  horsley
 *  made correction to monte carlo area calculation, not used in production
 *
 *  Revision 2.4  2000/11/22 23:24:49  horsley
 *  StRichArea.cxx monte carlo integration fix to the return value
 *
 *  Revision 2.3  2000/11/22 21:49:48  horsley
 *  StRichArea.cxx changed to remove the abort commands and replace with return false tp avoid aborting whole chain.
 *
 *  Revision 2.2  2000/11/21 16:24:22  horsley
 *  Major overhaul of StRichArea, introduced monte carlo integration cross check,
 *  all possible areas, angles calculated together. StRichRingCalculator, StRichPIDMaker modified to support new StRichArea. StRichPIDMaker's hit finder
 *  typo corrected.
 *
 *  Revision 2.1  2000/09/29 01:35:35  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.2  2000/05/19 19:06:10  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:07  horsley
 *  initial revision
 **********************************************************/

#include "StRichArea.h"
#include "StParticleDefinition.hh"
#include "StRichRingPoint.h"

#include "StRrsMaker/StRichGeometryDb.h"
#include "SystemOfUnits.h"

#include "StRichAreaSegment.h"
#include "StRichMinimization.h"
#include <cassert>

// root stuff (sorry!)
#include "TDatime.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
using std::min;
using std::max;
#endif



StRichArea::StRichArea(StRichRingPoint* irp, StRichRingPoint* orp) {
  //
  // these are the ring point functions 
  // passed by the ring calculator they 
  // describe the ring and are used to 
  // calculate individual points on the ring
  //
  mInnerRing      = irp;
  mOuterRing      = orp;  


  //
  // default number of steps taken in area integration
  // ie 180 ---> 0 degrees has nNumberOfSteps/2
  //  -180 ---> 0 degress has nNumberOfPoints/2
  //  can be reset using member function setPoints()
  //
  mNumberOfSteps  = 3600.0;
  

  //
  // used to define direction of angle step, 
  // ie (if the angle is increased or decresed after each step)
  //
  mPositiveDirection =  1.0;
  mNegativeDirection = -1.0;


  //
  // this is used to define a smallest angle step
  // used in the getStoppingAngle() member function
  //
  mSmallAngleStep = 0.0000174532925;  // ---> 0.01 degrees


  //
  // these data members are used in getStoppingAngle()
  // here mStopCounter is initialized 
  // mNotEnoughAngleForCalculation is set at 170 degrees
  // this is effective only for HIGHLY inclined tracks
  //
  mStopCounter = static_cast<int>(M_PI/mSmallAngleStep);
  mNotEnoughAngleForCalculation = 175.0*degree;



  //
  // this is used in the gap correction calculations
  // mSmallDistance is used to define a smallest 
  // step in the gap correction mTooManyCounts is 
  // a safety counter used to protect against infinite loops 
  //
  mSmallDistance  = 0.01*centimeter;
  mTooManyCounts = (int) (mSmallDistance*10000000.0);   


  //
  // here we are concerned with the gap correction
  // set flag for gap correction (true == make gap correction 
  //                                             false == no gap correction)
  mCorrectForGap  = true;  



  //
  // initialize angle data members
  // 
  //  mTotalAngleInActiveArea --> this is the total integrated angle contained 
  //                                                  in the fiducial area on the pad plane 
  //                                                  (with the gap correction included)
  //
  //  mTotalAngleInArea  --> same as mTotalAngleInActiveArea but no gap correction
  //
  //  mTotalAngleInActiveConstantArea --> this is the total integrated angle contained
  //                                                               in the fiducial area on the pad plane 
  //                                                               with a cut applied to maintain a maximum area
  //                                                               equal to the area of a normally incident particle
  //                                                               of the same momentum
  //                                                              (with the gap correction included)
  //
  // mTotalAngleInConstantArea --> same as  mTotalAngleInActiveConstantArea but
  //                                                     no gap correction 
  //
  // mTotalAngle --> the total angle of the possible cherenkov light that does 
  //                             not suffer total internal reflection in the quartz
  //
  mTotalArea  = 0.0;
  mTotalAngle = 0.0;

  mTotalAreaOnPadPlane  = 0;
  mTotalAngleOnPadPlane = 0;
  
  mTotalAreaOnActivePadPlane  = 0;
  mTotalAngleOnActivePadPlane = 0;
  
  mTotalConstantArea  = 0;
  mTotalConstantAngle = 0;

  mTotalConstantAreaOnPadPlane  = 0;
  mTotalConstantAngleOnPadPlane = 0;

  mTotalConstantAreaOnActivePadPlane  = 0;
  mTotalConstantAngleOnActivePadPlane = 0;



  //
  // here we initialize the constant area cut to 180 degrees
  // this means that the whole ring is cut away.
  // in the area calculation, this value is calculated
  //
  mConstantAreaAngleCut = M_PI;
  

  //
  // set flag for storing points used in calculation of ring
  //
  mDrawAreaRingPoints=false;


  //
  // get geometry database
  //
  myGeometryDb = StRichGeometryDb::getDb();


  //
  // data members for gap check, quad check
  //
  mXGapWidth = myGeometryDb->quadrantGapInX()/2.0;
  mYGapWidth = myGeometryDb->quadrantGapInY()/2.0;


  //
  // data members for inbounds checking 
  //
  mRadX = myGeometryDb->radiatorDimension().x();
  mRadY = myGeometryDb->radiatorDimension().y();
}


StRichArea::~StRichArea() { /* nopt */ }

vector<StRichAreaSegment >& StRichArea::getPtsToDraw() {
  return vectorOfPtsToDraw;
}


double StRichArea::calculateArea(double areaCut, double psiCut) {
  

  if (mDrawAreaRingPoints) {    
    vectorOfPtsToDraw.resize(0);
    vectorOfPtsToDraw.clear();
  }
 

  //
  // initialize data members 
  // many of these are already initialized in constructor
  // will do it again just in case function is called twice
  // or the calculateArea() function is called as 
  // well as the calculateConstantArea() funtion
  // 
  mTotalArea  = 0.0;
  mTotalAngle = 0.0;

  mTotalAreaOnPadPlane = 0;
  mTotalAngleOnPadPlane = 0;
  
  mTotalAreaOnActivePadPlane = 0;
  mTotalAngleOnActivePadPlane = 0;
  
  mTotalConstantArea = 0;
  mTotalConstantAngle = 0;

  mTotalConstantAreaOnPadPlane = 0;
  mTotalConstantAngleOnPadPlane = 0;

  mTotalConstantAreaOnActivePadPlane = 0;
  mTotalConstantAngleOnActivePadPlane = 0;
  
  mCorrectedAreaSegment   = 0.0;
  mUnCorrectedAreaSegment = 0.0;
  mAreaSegment = 0.0;

  mStartAngle             = M_PI;
  mConstantAreaAngleCut   = M_PI;

  

  //
  // check to see if ring is above cherenkov threshold
  //
  if (!mInnerRing->getTrack()->fastEnough(mInnerRing->getParticleType())) {
    cout << "StRichArea::calculateArea()  track not above threshold." << endl;
    return 0;
  }

  //
  // calculate the angle at which point
  // the calculation should stop 
  // (ie the ring has refracted away)
  // 
  mStopAngle = getStoppingAngle(psiCut);  
  if (mStopAngle==mNotEnoughAngleForCalculation) {
    cout << "StRichArea::calculateArea()  not enough ring. stop calculation." << endl;
    return 0;
  }


  //
  // calculate the amount by which the angle should 
  // change for each step
  //
  mAngleIncrement = M_PI/(mNumberOfSteps/2.0);
  int stopHere = (int) (mStopAngle/mAngleIncrement);

  
  // -----------  pi ---> 0 degs  ---------------------- // 
  
  //
  // here we start at 180 degrees and try and get a set 
  // of points to start the calculation.  To calculate an
  // area segment, need two sets of points (mInXYA, mOutXYA)
  // and (mInXYB, mOutXYB). Below we get the first set of points
  //   
  double angle1,angle2;
  angle1 = mStartAngle;
  if (!getRingPoints(angle1,angle2,mInXYA,mOutXYA,(int)mPositiveDirection)) {
    cout 
      << "StRichNewArea::calculateArea  a --> starting point returns false. abort!" << endl;
    //abort();
    return -999;
  }
  


  //
  // this is were we calculate the area from 180 -- 0 degrees
  //
  double tempArea  = 0.0;
  double tempConstantAreaAngleCuta = mStartAngle;
  int i;
  for (i=0;i<(mNumberOfSteps/2.0 - stopHere);i++) {


    //
    // define a area segement to be used in drawing 
    // points used in area calculation, taking into account
    // gap and edge effects, not actually used in calculating 
    // area though
    //
    StRichAreaSegment tempSegment;
    mCorrectedAreaSegment   = 0.0;
    mUnCorrectedAreaSegment = 0.0;
    mAreaSegment = 0.0;
    if (getRingPoints(angle1,angle2,mInXYB,mOutXYB,(int)mPositiveDirection)) { 
     
      //
      // here we calculate the area of a single area segment defined 
      // by the inner, outer points separated by a delta angle
      // area segment defined by 4 points: use area of n-sided polygon
      // to calculate the area of the segment
      // area = abs( 0.5*SUM(x_i * y_i+1   - x_i+1 * y_i)  )  where SUM is over i=1 .. k points
      // and x_k+1 = x_1  
      //
      getAreaSegment(tempSegment,angle1,angle2,mInXYA,mOutXYA,mInXYB,mOutXYB,
		     mCorrectedAreaSegment,mUnCorrectedAreaSegment,mAreaSegment);
      


      //
      // here we keep track of the different areas
      //
      if (mAreaSegment>0) {
	mTotalArea += mAreaSegment;
	mTotalAngle++;
      }

      if (mUnCorrectedAreaSegment>0) {
	mTotalAreaOnPadPlane  += mUnCorrectedAreaSegment;
	mTotalAngleOnPadPlane += 1.0*(mUnCorrectedAreaSegment/mAreaSegment);
      }

      if (mCorrectedAreaSegment>0) {
	mTotalAreaOnActivePadPlane  += mCorrectedAreaSegment;
	mTotalAngleOnActivePadPlane += 1.0*(mCorrectedAreaSegment/mAreaSegment);
      }
      


      //
      // constant area
      //      
      if ( (mTotalArea<areaCut/2.0)) {
	
	if (mAreaSegment>0) {
	  
	  tempConstantAreaAngleCuta -= mAngleIncrement;
	  mTotalConstantArea += mAreaSegment;
	  mTotalConstantAngle++;
	}
	
	if (mUnCorrectedAreaSegment>0) {
	  mTotalConstantAreaOnPadPlane  += mUnCorrectedAreaSegment;
	  mTotalConstantAngleOnPadPlane += 1.0*(mUnCorrectedAreaSegment/mAreaSegment);
	}
	
	if (mCorrectedAreaSegment>0) {
	  //
	  // if mDrawAreaRingPoints flag is set to TRUE, 
	  // store points used to define area segment
	  //
	  if (mDrawAreaRingPoints) {vectorOfPtsToDraw.push_back(tempSegment);}
	  
	  mTotalConstantAreaOnActivePadPlane  += mCorrectedAreaSegment;
	  mTotalConstantAngleOnActivePadPlane +=  1.0*(mCorrectedAreaSegment/mAreaSegment);
	}
 
      }
      
    }



    //
    // here we swap the points used in the calculation
    // 4 points define the polynomial.  The first two are 
    // one edge, the second two are the second edge.
    // after the calculation, the second points are copied 
    // over to the first set, and the second pair are then calculated 
    // above in the first part of the loop using the new angle
    //
    swapPoints(mInXYA,mOutXYA,mInXYB,mOutXYB);
  }




  // ---------   -pi ---> 0 degs  -------------------- //
  angle1 = -mStartAngle;

  if (!getRingPoints(angle1,angle2,mInXYA,mOutXYA,(int)mNegativeDirection)) {
    cout << "StRichNewArea::calculateArea  b --> starting point returns false. abort!" << endl;
    //abort();
    return -999;
  }

  tempArea=0;
  double tempConstantAreaAngleCutb = -mStartAngle;
  
  for (i=0;i<(mNumberOfSteps/2.0 - stopHere);i++) {
    StRichAreaSegment tempSegment;
    mCorrectedAreaSegment   = 0.0;
    mUnCorrectedAreaSegment = 0.0;
    mAreaSegment = 0.0;

    if (getRingPoints(angle1,angle2,mInXYB,mOutXYB,(int)mNegativeDirection)) {

      getAreaSegment(tempSegment,angle1,angle2,mInXYA,mOutXYA,mInXYB,mOutXYB,
		     mCorrectedAreaSegment,mUnCorrectedAreaSegment,mAreaSegment);
     
      //
      // here we keep track of the different areas
      //
      if (mAreaSegment>0) {
	mTotalArea += mAreaSegment;
	mTotalAngle++;
	tempArea += mAreaSegment;
      }

      if (mUnCorrectedAreaSegment>0) {
	mTotalAreaOnPadPlane  += mUnCorrectedAreaSegment;
	mTotalAngleOnPadPlane += 1.0*(mUnCorrectedAreaSegment/mAreaSegment);
      }

      if (mCorrectedAreaSegment>0) {
	mTotalAreaOnActivePadPlane  += mCorrectedAreaSegment;
	mTotalAngleOnActivePadPlane += 1.0*(mCorrectedAreaSegment/mAreaSegment);
      }
      


      //
      // constant area
      //      
      if ( (tempArea<areaCut/2.0)) {
	
	if (mAreaSegment>0) {
	  
	  tempConstantAreaAngleCutb += mAngleIncrement;
	  mTotalConstantArea += mAreaSegment;
	  mTotalConstantAngle++;
	}
	
	if (mUnCorrectedAreaSegment>0) {
	  mTotalConstantAreaOnPadPlane  += mUnCorrectedAreaSegment;
	  mTotalConstantAngleOnPadPlane += 1.0*(mUnCorrectedAreaSegment/mAreaSegment);
	}
	
	if (mCorrectedAreaSegment>0) {
	  //
	  // if mDrawAreaRingPoints flag is set to TRUE, 
	  // store points used to define area segment
	  //
	  if (mDrawAreaRingPoints) {vectorOfPtsToDraw.push_back(tempSegment);}
	  
	  mTotalConstantAreaOnActivePadPlane  += mCorrectedAreaSegment;
	  mTotalConstantAngleOnActivePadPlane += 1.0*(mCorrectedAreaSegment/mAreaSegment);
	}
	
      }

    }

    swapPoints(mInXYA,mOutXYA,mInXYB,mOutXYB);
  }



  // 
  // store the angle at which the constant area cut was reached
  // need to compare the angle for each 'wing' separately 
  // as edge effects can affect the calculation differently 
  // for the two different 'wings'  (ie the left wing can be mostly
  // out of bounds , giving a high angle cut , where as the 
  // other wing can be completely inbounds)
  // need to get the smallest constant angle  
  //
  tempConstantAreaAngleCuta = fabs(tempConstantAreaAngleCuta);
  tempConstantAreaAngleCutb = fabs(tempConstantAreaAngleCutb);
  mConstantAreaAngleCut = (tempConstantAreaAngleCuta < tempConstantAreaAngleCutb) ? 
    (tempConstantAreaAngleCuta) : (tempConstantAreaAngleCutb); 
  
  
  //
  // calculate the total angles
  //
  mTotalAngle                 *= mAngleIncrement;
  mTotalAngleOnPadPlane       *= mAngleIncrement;
  mTotalAngleOnActivePadPlane *= mAngleIncrement;


  mTotalConstantAngle                 *= mAngleIncrement;
  mTotalConstantAngleOnPadPlane       *= mAngleIncrement;
  mTotalConstantAngleOnActivePadPlane *= mAngleIncrement;



  //
  // return the corrected area
  //
  return mTotalArea;  
}


double StRichArea::getStoppingAngle(double cutAngle) {

  //
  // calculate angle where the calculation stops
  // stop angle will be different from 0 if ring
  // point is refracted away
  //

  int safetyCounter=0;
  StThreeVectorF out;
  
  //
  // set stop angle = 0
  //
  double stopAngle = 0.0;
  bool oStatus = mOuterRing->getPoint(stopAngle,out); 
  
  
  //
  // if oStatus == false then ring at 0 degree has refracted away
  // need to increment angle by "small" amount until find 
  // valid ring point
  //
  while (!oStatus) {

    //
    // increment angle, get ring point status
    // advance safety counter
    //
    stopAngle += mSmallAngleStep;
    oStatus = mOuterRing->getPoint(stopAngle,out); 
    safetyCounter++;
    
    if (safetyCounter>mStopCounter) {
      cout << "StRichArea::getStoppingAngle. " << endl;
      cout << "Not Enough Ring to Calculate Area." << endl;
      cout << "Track parameters: " << endl;
      cout << "-----------------------" << endl;
      cout << "p = " << mOuterRing->getTrack()->getMomentum() << endl;
      cout << "xy = " << mOuterRing->getTrack()->getImpactPoint() << endl;
      cout << "theta = " << mOuterRing->getTrack()->getTheta()/degree << endl;
      cout << "stop angle = " << stopAngle/degree << endl;
      cout << "safetyCounter = " << safetyCounter << endl;
      cout << "stopCounter = " << mStopCounter << endl;
      cout << "oStatus = " << oStatus << endl;
      return mNotEnoughAngleForCalculation;
    }
  }
  
  
  if (stopAngle<cutAngle) {return cutAngle;}
  return stopAngle;
}





bool
StRichArea::getRingPoints(double& angle1, double& angle2, 
			  StThreeVectorF& ixy, StThreeVectorF& oxy, int direction) {
  
  // 
  // initialize ring points to -999
  //
  StThreeVectorF tempInner(-999,-999,-999);
  StThreeVectorF tempOuter(-999,-999,-999);
  

  //
  // get ring points
  //
  bool mInStatus  = mInnerRing->getPoint(angle1,tempInner);
  bool mOutStatus = mOuterRing->getPoint(angle1,tempOuter);
  
  //
  // angle2 is for drawing rings only, not used in calculation
  // angle1 is incremented fro next iteration
  //
  angle2=angle1;  
  angle1 -= ((double) direction)*mAngleIncrement;  
  

  //
  // if inner and outer exist, assign ixy,oxy to ring point values
  //
  if (mInStatus && mOutStatus)  {
    ixy = tempInner;
    oxy = tempOuter;
  }
  

  //
  // check to se if ring points are invalid
  // should never happen, check to be sure
  // abort if does happen, fix problem!!!
  //
  if (isnan(ixy.x()) || isnan(ixy.y()) || isnan(ixy.z())) {
    cout << "StRichArea::getRingPoints()" << endl;
    cout << "track p = " << mInnerRing->getTrack()->getMomentum() << endl;
    cout << "track theta = " << mInnerRing->getTrack()->getTheta()/degree << endl;
    cout << "angle = " << angle1/degree << endl;
    cout << "problem track. abort() " << endl;
    //abort();
    return false;
  }

  //
  // return true if ring points exist, otherwise false
  //
  if (mInStatus && mOutStatus) {return true;}  
  return false;
}



void
StRichArea::swapPoints(StThreeVectorF& ixya,StThreeVectorF& oxya,
		         StThreeVectorF& ixyb, StThreeVectorF& oxyb) {

  //  
  // don't really have to swap the points, its 
  // enough to just set a = b
  //
  ixya = ixyb;
  oxya = oxyb;
}



void
StRichArea::getAreaSegment(StRichAreaSegment& tempSeg, 
			   double ang1, double ang2,
			   StThreeVectorF& ixya, StThreeVectorF& oxya,
			   StThreeVectorF& ixyb, StThreeVectorF& oxyb, 
			   double& correctedPadAreaSeg, double& unCorrectedPadAreaSeg,
			   double& totalAreaSeg) {
  

  

  //
  // here we calculate the total area, not taking into account
  // the edge of detector or the gap
  //
  double term1,term2,term3,term4;
  term1 = (ixya.x()*oxya.y() - oxya.x()*ixya.y());
  term2 = (oxya.x()*oxyb.y() - oxyb.x()*oxya.y());
  term3 = (oxyb.x()*ixyb.y() - ixyb.x()*oxyb.y());
  term4 = (ixyb.x()*ixya.y() - ixya.x()*ixyb.y());
  totalAreaSeg = 0.5*fabs(term1 + term2 + term3 + term4); 
  


  correctedPadAreaSeg = 0;
  //
  // check if one or both points fall out of bounds
  // if possible, pull points back inbounds, then
  // proceed with rest of calculation
  //
  if (outOfBoundsCorrection(ixya,oxya) && outOfBoundsCorrection(ixyb,oxyb) ) {  
   
    StThreeVectorF tempixya,tempoxya,tempixyb,tempoxyb;
    //
    // calculate total area segment here
    // no gap correction necessary    
    //
    double term1,term2,term3,term4;
    term1 = (ixya.x()*oxya.y() - oxya.x()*ixya.y());
    term2 = (oxya.x()*oxyb.y() - oxyb.x()*oxya.y());
    term3 = (oxyb.x()*ixyb.y() - ixyb.x()*oxyb.y());
    term4 = (ixyb.x()*ixya.y() - ixya.x()*ixyb.y());
    
    //
    // here we calculate the uncorrected area 
    //
    unCorrectedPadAreaSeg = 0.5*fabs(term1 + term2 + term3 + term4); 
    

    //
    // if not bothering with gap correction,
    // calculate area, and pass points used in calculation 
    // to area segment

    if (!mCorrectForGap) { 
      correctedPadAreaSeg = 0.5*fabs(term1 + term2 + term3 + term4); 
      
      if (mDrawAreaRingPoints) {
	tempSeg.setType(0); // no correction made

	tempSeg.addAngle(0,ang1);
	tempSeg.addAngle(1,ang2);

	tempSeg.addPoint(0,ixya);
	tempSeg.addPoint(1,ixyb);
	tempSeg.addPoint(10,oxyb);
	tempSeg.addPoint(11,oxya);
      }
      return;
    }
    

    //
    // order of cases is important, ie 1 then 2 then 3 , etc...
    // case 1: 1 pt in quadrant, 1 pt in gap (either set of pts)
    // make correction, pass corrected pts to next function   
    //
    tempixya=ixya;
    tempoxya=oxya;
    tempixyb=ixyb;
    tempoxyb=oxyb;
    
    bool setOneCheck = partialGapCorrection(ixya,oxya,tempixya,tempoxya);  
    bool setTwoCheck = partialGapCorrection(ixyb,oxyb,tempixyb,tempoxyb); 
    if ( !(setOneCheck*setTwoCheck) ) return;
    

    //
    // case 2: all pts in same quadrant
    //
    if (quadCheck(tempixya,tempoxya) && 
	quadCheck(tempixyb,tempoxyb) && 
	quadCheck(tempixya,tempoxyb)) {
      double term1,term2,term3,term4;
      term1 = (tempixya.x()*tempoxya.y() - tempoxya.x()*tempixya.y());
      term2 = (tempoxya.x()*tempoxyb.y() - tempoxyb.x()*tempoxya.y());
      term3 = (tempoxyb.x()*tempixyb.y() - tempixyb.x()*tempoxyb.y());
      term4 = (tempixyb.x()*tempixya.y() - tempixya.x()*tempixyb.y());
      correctedPadAreaSeg = 0.5*fabs(term1 + term2 + term3 + term4); 
      if (mDrawAreaRingPoints) {
	tempSeg.setType(1); // all pts in same quad

	tempSeg.addAngle(0,ang1);
	tempSeg.addAngle(1,ang2);

	tempSeg.addPoint(0,tempixya);
	tempSeg.addPoint(1,tempixyb);
	tempSeg.addPoint(10,tempoxyb);
	tempSeg.addPoint(11,tempoxya);
      }

      return;
    }
    

    //
    // case 3: 2 pts in one quadrant, 2 pts in adjacent quadrant
    //
    if (adjacentCheck(tempixya,tempoxya) && adjacentCheck(tempixyb,tempoxyb)) {
      StThreeVectorF iEdgea,oEdgea,iEdgeb,oEdgeb;
      if ( fullGapCorrectionNecessary(tempixya,tempoxya,iEdgea,oEdgea) &&
	   fullGapCorrectionNecessary(tempixyb,tempoxyb,iEdgeb,oEdgeb)) {
	
	if (mDrawAreaRingPoints) {
	  tempSeg.setType(2); // adjacent quad

	  tempSeg.addAngle(0,ang1);
	  tempSeg.addAngle(1,ang2);
	  
	  tempSeg.addPoint(0,tempixya);
	  tempSeg.addPoint(1,tempixyb);
	  tempSeg.addPoint(2,iEdgeb);
	  tempSeg.addPoint(3,iEdgea);
	  tempSeg.addPoint(8,oEdgea);
	  tempSeg.addPoint(9,oEdgeb);
	  tempSeg.addPoint(10,tempoxyb);
	  tempSeg.addPoint(11,tempoxya);
	}

	double term1a,term2a,term3a,term4a;
	double term1b,term2b,term3b,term4b;
	term1a = (tempixya.x()*iEdgea.y()   - iEdgea.x()*tempixya.y());
	term2a = (iEdgea.x()*iEdgeb.y()     - iEdgeb.x()*iEdgea.y());
	term3a = (iEdgeb.x()*tempixyb.y()   - tempixyb.x()*iEdgeb.y());
	term4a = (tempixyb.x()*tempixya.y() - tempixya.x()*tempixyb.y());
	
	term1b = (oEdgea.x()*tempoxya.y()   - tempoxya.x()*oEdgea.y());
	term2b = (tempoxya.x()*tempoxyb.y() - tempoxyb.x()*tempoxya.y());
	term3b = (tempoxyb.x()*oEdgeb.y()   - oEdgeb.x()*tempoxyb.y());
	term4b = (oEdgeb.x()*oEdgea.y()     - oEdgea.x()*oEdgeb.y());
	
	correctedPadAreaSeg = 0.5*fabs(term1a + term2a + term3a + term4a) 
	                    + 0.5*fabs(term1b + term2b + term3b + term4b);   
	return;
      }
    }
    


    //
    // case 4: 2 pts in one quadrant, 2 pts in non-adjacent quadrant
    //
    if (!adjacentCheck(tempixya,tempoxya) && !adjacentCheck(tempixyb,tempoxyb)) {
      StThreeVectorF ixyaa,ixyab,oxyaa,oxyab;
      StThreeVectorF ixyba,ixybb,oxyba,oxybb;
      if ( nonAdjacentGapCorrection(tempixya,tempoxya,ixyaa,oxyaa,ixyab,oxyab) &&
	   nonAdjacentGapCorrection(tempixyb,tempoxyb,ixyba,oxyba,ixybb,oxybb)) {
	
	if (mDrawAreaRingPoints) {
	  tempSeg.setType(3); // non-adjacent quad

	  tempSeg.addAngle(0,ang1);
	  tempSeg.addAngle(1,ang2);
	  
	  tempSeg.addPoint(0,tempixya);
	  tempSeg.addPoint(1,tempixyb);
	  tempSeg.addPoint(2,ixyba);
	  tempSeg.addPoint(3,ixyaa);
	  tempSeg.addPoint(4,ixyab);
	  tempSeg.addPoint(5,ixybb);
	  tempSeg.addPoint(6,oxybb);
	  tempSeg.addPoint(7,oxyab);
	  tempSeg.addPoint(8,oxyaa);
	  tempSeg.addPoint(9,oxyba);
	  tempSeg.addPoint(10,tempoxyb);
	  tempSeg.addPoint(11,tempoxya);
	}

	
	double term1a,term2a,term3a,term4a;
	double term1b,term2b,term3b,term4b;
	double term1c,term2c,term3c,term4c;
	term1a = (tempixya.x()*ixyaa.y()     - ixyaa.x()*tempixya.y());
	term2a = (ixyaa.x()*ixyba.y()        - ixyba.x()*ixyaa.y());
	term3a = (ixyba.x()*tempixyb.y()     - tempixyb.x()*ixyba.y());
	term4a = (tempixyb.x()*tempixya.y()  - tempixya.x()*tempixyb.y());
	
	term1b = (tempoxya.x()*oxyaa.y()     - oxyaa.x()*tempoxya.y());
	term2b = (oxyaa.x()*oxyba.y()        - oxyba.x()*oxyaa.y());
	term3b = (oxyba.x()*tempoxyb.y()     - tempoxyb.x()*oxyba.y());
	term4b = (tempoxyb.x()*tempoxya.y()  - tempoxya.x()*tempoxyb.y());
	
	term1c=0;
	term2c=0;
	term3c=0;
	term4c=0;
	if (ixybb.x()!=-999 && ixybb.y()!=-999 && ixyab.x()!=-999 && ixyab.y()!=-999) {
	  term1c = (ixyab.x()*oxyab.y() - oxyab.x()*ixyab.y());
	  term2c = (oxyab.x()*oxybb.y() - oxybb.x()*oxyab.y());
	  term3c = (oxybb.x()*ixybb.y() - ixybb.x()*oxybb.y());
	  term4c = (ixybb.x()*ixyab.y() - ixyab.x()*ixybb.y());
	}
	
	correctedPadAreaSeg = 0.5*fabs(term1a + term2a + term3a + term4a) 
	                    + 0.5*fabs(term1b + term2b + term3b + term4b)
	                    + 0.5*fabs(term1c + term2c + term3c + term4c);
	return;
      }
    }


    //
    // only the partial gap correction was necessary
    //
    double term1c,term2c,term3c,term4c;
    term1c = (tempixya.x()*tempoxya.y() - tempoxya.x()*tempixya.y());
    term2c = (tempoxya.x()*tempoxyb.y() - tempoxyb.x()*tempoxya.y());
    term3c = (tempoxyb.x()*tempixyb.y() - tempixyb.x()*tempoxyb.y());
    term4c = (tempixyb.x()*tempixya.y() - tempixya.x()*tempixyb.y());
    correctedPadAreaSeg = 0.5*fabs(term1c + term2c + term3c + term4c); 

    if (mDrawAreaRingPoints) {
      tempSeg.setType(4); // only partial gap
      
      tempSeg.addAngle(0,ang1);
      tempSeg.addAngle(1,ang2);
      
      tempSeg.addPoint(0,tempixya);
      tempSeg.addPoint(1,tempixyb);
      tempSeg.addPoint(10,tempoxyb);
      tempSeg.addPoint(11,tempoxya);
    }
    return;
 
  }
  
  return;
}


bool 
StRichArea::nonAdjacentGapCorrection(StThreeVectorF& ixy,  StThreeVectorF& oxy,
				        StThreeVectorF& ixya, StThreeVectorF& oxya,
				        StThreeVectorF& ixyb, StThreeVectorF& oxyb) {

  if ( !mCorrectForGap )   return false;

  //
  // checks if one (or both) point(s) in gap
  //
  if ( gapCheck(ixy) || gapCheck(oxy) )  return false;
    

  /////////////   do gap correction   //////////////////

  //
  // here we get the point closest to the outer point
  //
  double phi = atan2(ixy.y()-oxy.y(),ixy.x()-oxy.x());    
  StThreeVectorF tempo,tempi,tempa;
  StThreeVectorF temp(mSmallDistance*centimeter,
		      mSmallDistance*centimeter,
		      mSmallDistance*centimeter);
  tempo = oxy;
  tempi = ixy;


  //
  // this routine is very simple
  // vector defined by inner,outer points
  // cut vector in half each time, redefine
  // vector and cut in half again until point 
  // is no longer in gap  (and is in correct quadrant)
  //
  int safetyCheck=0;
  while (temp.mag()>mSmallDistance) { 
    safetyCheck++;
    if (safetyCheck>mTooManyCounts) {
      cout << "StRichArea::adjacentGapCorrectionNecessary  ---> abort!" << endl;
      cout << "Problem in outer ring gap correction. " << endl;      
      //  abort();
      return false;
    }
    
    temp = tempi-tempo;
    double v = 0.5*temp.mag();
    tempa.setX(tempo.x() + v*cos(phi));
    tempa.setY(tempo.y() + v*sin(phi));
    if (quadCheck(oxy,tempa)) {tempo = tempa;} 
    else {tempi = tempa;}
  }  
  oxya = tempa;
  
  //
  // here we check to see if the final corrected point 
  // lies along the original vector. if not, problem
  // exists, abort and fix!! @(*^@#%
  //
  if (!sanityCheck(ixy,oxya,oxy) ) {
    cout << "StRichArea::adjacentGapCorrectionNecessary() ---> abort()!" << endl;
    cout << "Problem in outer ring gap correction, sanity check." << endl;
    // abort();
    return false;
  }



  //
  // now get the next point across from the gap
  //
  oxyb=oxya;
  int gapCounter;
  for (gapCounter=0;gapCounter<mTooManyCounts;gapCounter++) {
    oxyb.setX(oxyb.x() + mSmallDistance*cos(phi));
    oxyb.setY(oxyb.y() + mSmallDistance*sin(phi));
    if (!gapCheck(oxyb) && !quadCheck(oxya,oxyb)) break;
  }

  //
  // here we get the point closest to the inner point
  //
  phi = atan2(oxy.y()-ixy.y(),oxy.x()-ixy.x());
  

  //
  // set temp distance to some distance 
  // bigger than mSmallDistance
  //
  temp.setX(mSmallDistance*centimeter);
  temp.setY(mSmallDistance*centimeter);
  temp.setZ(mSmallDistance*centimeter);

  tempo = oxy;
  tempi = ixy;
  
  //
  // this routine is very simple
  // vector defined by inner,outer points
  // cut vector in half each time, redefine
  // vector and cut in half again until point 
  // is no longer in gap  (and is in correct quadrant)
  //
  safetyCheck=0;
  while (temp.mag()>mSmallDistance) {

    safetyCheck++;
    if (safetyCheck>mTooManyCounts) {
      cout << "StRichArea::adjacentGapCorrectionNecessary  ---> abort!" << endl;
      cout << "Problem in inner ring gap correction. " << endl;      
      //abort();
      return false;
    }
    
    temp = tempi - tempo;
    double v = 0.5*temp.mag();
    tempa.setX(tempi.x() + v*cos(phi));
    tempa.setY(tempi.y() + v*sin(phi));
    if (quadCheck(ixy,tempa)) {tempi = tempa;} 
    else {tempo = tempa;}
  }

  ixya = tempa;
  if (!sanityCheck(ixy,ixya,oxy) ) {
    cout << "StRichArea::adjacentGapCorrectionNecessary() ---> abort()!" << endl;
    cout << "Problem in inner ring gap correction, sanity check." << endl;
    //abort();
    return false;
  }
  
  //
  // now get the next point across from the gap
  //
  ixyb=ixya;
  for (gapCounter=0;gapCounter<mTooManyCounts;gapCounter++) {
    ixyb.setX(ixyb.x() + mSmallDistance*cos(phi));
    ixyb.setY(ixyb.y() + mSmallDistance*sin(phi));
    if (!gapCheck(ixyb) && !quadCheck(ixya,ixyb)) break;
  }


  //
  // now check to see if the ixyb, oxyb are the right pts, 
  // if not, then set them ixyb=-999 and  oxyb=-999 (see page 88 in note book
  // for better explanation)
  //
  if ( (oxy-ixyb).mag() > (oxy-oxyb).mag() ) {
    ixyb.setX(-999);
    ixyb.setY(-999);
    oxyb.setX(-999);
    oxyb.setY(-999);
  }


  return true;
}




bool 
StRichArea::fullGapCorrectionNecessary(StThreeVectorF& ixy,  StThreeVectorF& oxy,
				          StThreeVectorF& itemp, StThreeVectorF& otemp) {
  
  if ( !mCorrectForGap )   return false;

  //
  // checks if one (or both) point(s) in gap
  //
  if ( gapCheck(ixy) || gapCheck(oxy) )  return false;
  

  //
  // checks if both points in same quadrant
  // if not then necessary to do gap correction
  //
  if ( quadCheck(ixy,oxy) ) return false;
  

  /////////////   do gap correction   //////////////////
  
  //
  // here we get the point closest to the outer point
  //
  double phi = atan2(ixy.y()-oxy.y(),ixy.x()-oxy.x());    
  StThreeVectorF tempo,tempi,tempa;
  StThreeVectorF temp(mSmallDistance*centimeter,
		      mSmallDistance*centimeter,
		      mSmallDistance*centimeter);
  tempo = oxy;
  tempi = ixy;



  //
  // this routine is very simple
  // vector defined by inner,outer points
  // cut vector in half each time, redefine
  // vector and cut in half again until point 
  // is no longer in gap  (and is in correct quadrant)
  //
  int safetyCheck=0;
  while (temp.mag()>mSmallDistance) { 
    safetyCheck++;
    if (safetyCheck>mTooManyCounts) {
      cout << "StRichArea::fullGapCorrectionNecessary  ---> abort!" << endl;
      cout << "Problem in outer ring gap correction. " << endl;      
      //  abort();
      return false;
    }
    
    temp = tempi-tempo;
    double v = 0.5*temp.mag();
    tempa.setX(tempo.x() + v*cos(phi));
    tempa.setY(tempo.y() + v*sin(phi));
    if (quadCheck(oxy,tempa)) {tempo = tempa;} 
    else {tempi = tempa;}
  }  
  otemp = tempa;
  
  if (!sanityCheck(ixy,otemp,oxy) ) {
    cout << "StRichArea::fullGapCorrectionNecessary() ---> abort()!" << endl;
    cout << "Problem in outer ring gap correction, sanity check." << endl;
    //  abort();
    return false;
  }
 

  //
  // here we get the point closest to the inner point
  //
  phi = atan2(oxy.y()-ixy.y(),oxy.x()-ixy.x());
  
  //
  // set temp distance to some distance 
  // bigger than mSmallDistance
  //
  temp.setX(mSmallDistance*centimeter);
  temp.setY(mSmallDistance*centimeter);
  temp.setZ(mSmallDistance*centimeter);

  tempo = oxy;
  tempi = ixy;
  


  //
  // this routine is very simple
  // vector defined by inner,outer points
  // cut vector in half each time, redefine
  // vector and cut in half again until point 
  // is no longer in gap  (and is in correct quadrant)
  //
  safetyCheck=0;
  while (temp.mag()>mSmallDistance) {

    safetyCheck++;
    if (safetyCheck>mTooManyCounts) {
      cout << "StRichArea::fullGapCorrectionNecessary  ---> abort!" << endl;
      cout << "Problem in inner ring gap correction. " << endl;      
      //abort();
      return false;
    }
    
    temp = tempi - tempo;
    double v = 0.5*temp.mag();
    tempa.setX(tempi.x() + v*cos(phi));
    tempa.setY(tempi.y() + v*sin(phi));
    if (quadCheck(ixy,tempa)) {tempi = tempa;} 
    else {tempo = tempa;}
  }

  itemp = tempa;
 
  if (!sanityCheck(ixy,itemp,oxy) ) {
    cout << "StRichArea::fullGapCorrectionNecessary() ---> abort()!" << endl;
    cout << "Problem in inner ring gap correction, sanity check." << endl;
    // abort();
    return false;
  }
  
  return true;
}

bool StRichArea::partialGapCorrection(StThreeVectorF& ixy,  StThreeVectorF& oxy,
				         StThreeVectorF& tempixy, StThreeVectorF& tempoxy) {
  
  //
  // if neither point in gap, no correction necessary
  //  
  if ( !gapCheck(ixy) && !gapCheck(oxy) ) { return true;}
  

  // Exclusive OR  -->  ^
  if (gapCheck(ixy)^gapCheck(oxy)) {
    StThreeVectorF tempo,tempi,tempa,temp;
    double phi;
    tempixy = ixy;
    tempoxy = oxy;
    
    //
    // inner ring point in gap
    //
    if (gapCheck(ixy)) {
      phi = atan2(ixy.y()-oxy.y(),ixy.x()-oxy.x());    
      temp.setX(mSmallDistance*centimeter);
      temp.setY(mSmallDistance*centimeter);
      temp.setZ(mSmallDistance*centimeter);
      
      int safetyCounter=0;
      tempa = ixy;
      while (safetyCounter<mTooManyCounts) {
      safetyCounter++;
      double v = 0.5*mSmallDistance;
      tempa.setX(tempa.x() - v*cos(phi)); 
      tempa.setY(tempa.y() - v*sin(phi));
      if (!gapCheck(tempa)) break;
      }
      
      //
      //  if (!sanityCheck(ixy,tempa,oxy) ) {abort();}
      //
      tempixy = tempa;
      return true;
    }
    
    
    
    //
    // outer ring point in gap
    //
    if (gapCheck(oxy)) {
      phi = atan2(ixy.y()-oxy.y(),ixy.x()-oxy.x());    
      temp.setX(mSmallDistance*centimeter);
      temp.setY(mSmallDistance*centimeter);
      temp.setZ(mSmallDistance*centimeter);
      
      int safetyCounter=0;    
      tempa=oxy;
      while (safetyCounter<mTooManyCounts) {
	safetyCounter++;
	double v = 0.5*mSmallDistance;
	tempa.setX(tempa.x() + v*cos(phi));
	tempa.setY(tempa.y() + v*sin(phi));
	if (!gapCheck(tempa)) break;
      }
      
      //
      //  if (!sanityCheck(ixy,tempa,oxy) ) {abort();}
      //
      tempoxy = tempa;
      return true;
    }
  }



  //
  // both points fall in gap, 
  // try to make correction  
  //
  else  {
    
    StThreeVectorF tempo,tempi,tempa,temp;
    double phi;
    tempixy = ixy;
    tempoxy = oxy;
    

    //
    // inner ring point in gap
    //
    if (gapCheck(ixy)) {
      phi = atan2(ixy.y()-oxy.y(),ixy.x()-oxy.x());    
      temp.setX(mSmallDistance*centimeter);
      temp.setY(mSmallDistance*centimeter);
      temp.setZ(mSmallDistance*centimeter);
      
      int safetyCounter=0;
      tempa = ixy;
      while (safetyCounter<mTooManyCounts) {
	if ((tempa-oxy).mag()<.1) { return false;} 
	safetyCounter++;
	double v = 0.5*mSmallDistance;
	tempa.setX(tempa.x() - v*cos(phi)); 
	tempa.setY(tempa.y() - v*sin(phi));
	if (!gapCheck(tempa)) break;
      }
      //  if (!sanityCheck(ixy,tempa,oxy) ) {abort();}
      tempixy = tempa;
    }
    
    
    //
    // outer ring point in gap
    //
    if (gapCheck(oxy)) {
      phi = atan2(ixy.y()-oxy.y(),ixy.x()-oxy.x());    
      temp.setX(mSmallDistance*centimeter);
      temp.setY(mSmallDistance*centimeter);
      temp.setZ(mSmallDistance*centimeter);
      
      int safetyCounter=0;    
      tempa=oxy;
      while (safetyCounter<mTooManyCounts) {
	if ((tempa-ixy).mag()<.1) { return false;}
	safetyCounter++;
	double v = 0.5*mSmallDistance;
	tempa.setX(tempa.x() + v*cos(phi));
	tempa.setY(tempa.y() + v*sin(phi));
	if (!gapCheck(tempa)) break;
      }
      //  if (!sanityCheck(ixy,tempa,oxy) ) {abort();}
      tempoxy = tempa;
    }
    
    return true;
  } 

  return false;
}




bool StRichArea::gapCheck(StThreeVectorF& xy) {

  if ( fabs(xy.x()) < mXGapWidth) return true;
  if ( fabs(xy.y()) < mYGapWidth) return true;

  return false;
}


bool StRichArea::quadCheck(StThreeVectorF& ixy, StThreeVectorF& oxy) {
 
  //  
  // upper quad right  
  //
  if (ixy.x() > mXGapWidth && oxy.x() > mXGapWidth &&
      ixy.y() > mYGapWidth && oxy.y() > mYGapWidth) return true;

  //
  // upper quad left
  //
  if (ixy.x() < -mXGapWidth && oxy.x() < -mXGapWidth &&
      ixy.y() > mYGapWidth && oxy.y() > mYGapWidth) return true;

  //
  // lower quad right
  //
  if (ixy.x() > mXGapWidth && oxy.x() > mXGapWidth &&
      ixy.y() < -mYGapWidth && oxy.y() < -mYGapWidth) return true;


  //
  // lower quad left
  //
  if (ixy.x() < -mXGapWidth && oxy.x() < -mXGapWidth &&
      ixy.y() < -mYGapWidth && oxy.y() < -mYGapWidth) return true;
  
  return false;
}


bool 
StRichArea::inBounds(StThreeVectorF& xy) {
  if ( (fabs(xy.x()) <  mRadX ) && (fabs(xy.y()) <  mRadY ) ) {return true;}
  return false;
}


bool 
StRichArea::adjacentCheck(StThreeVectorF& ixy, StThreeVectorF& oxy) {
  
  //
  // check to see if points are in adjacent quadrants
  //
   assert (0 && "Fix me please, There is no suitable StThreeVectorF method yet");
         
  StThreeVectorF check;// = ixy*oxy;
  if (check.x() > 0 || check.y() > 0) { return true;}
  
  return false;
}


bool 
StRichArea::outOfBoundsCorrection(StThreeVectorF& ixy, StThreeVectorF& oxy) {


  //
  // if either point falls out of bounds, ignore area segement
  // 
  if (inBounds(oxy) && inBounds(ixy)) {return true;}
  else return false;

  /*
  if (inBounds(oxy)) {
      double phi = atan2(ixy.y()-oxy.y(),ixy.x()-oxy.x());    
      StThreeVectorF tempo,tempi,tempa;

      StThreeVectorF temp(mSmallDistance,
				   mSmallDistance,
				   mSmallDistance);
      tempo = oxy;
      tempi = ixy;
      
      int safetyCheck=0;
      while (temp.mag()>mSmallDistance) {

	safetyCheck++;
	if (safetyCheck > mTooManyCounts) {
	  cout << "StRichArea::outOfBoundsCorrection  ---> abort!" << endl;
	  cout << "Problem in inner ring out-of-bounds correction. " << endl;
	  abort();	  
	}

	temp = tempi-tempo;
	double v = 0.5*temp.mag();
	
	tempa.setX(tempo.x() + v*cos(phi));
	tempa.setY(tempo.y() + v*sin(phi));

	if (inBounds(tempa))  {tempo = tempa;} 
	if (!inBounds(tempa)) {tempi = tempa;}
      }
     
      if (!sanityCheck(ixy,tempa,oxy) ) {
	cout << "StRichArea::outOfBoundsCorrection  ---> abort!" << endl;
	cout << "Problem in inner ring sanity check. " << endl;
	abort();	  
      } 

      ixy = tempa;

      return true;
  }
  
 
  if (inBounds(ixy))  {
    double phi = atan2(oxy.y()-ixy.y(),oxy.x()-ixy.x());    
    StThreeVectorF tempo,tempi,tempa;
      StThreeVectorF temp(mSmallDistance,
				   mSmallDistance,
				   mSmallDistance);
    tempo = oxy;
    tempi = ixy;

    int safetyCheck=0;
    while (temp.mag()>mSmallDistance)  {

      safetyCheck++;
      if (safetyCheck > mTooManyCounts) {
	cout << "StRichArea::outOfBoundsCorrection  ---> abort!" << endl;
	cout << "Problem in outer ring out-of-bounds correction. " << endl;
	abort();	  
	}
      
      
      temp = tempi - tempo;
      double v = 0.5*temp.mag();
     
      tempa.setX(tempi.x() + v*cos(phi));
      tempa.setY(tempi.y() + v*sin(phi));
      if (inBounds(tempa))  {tempi = tempa;} 
      if (!inBounds(tempa)) {tempo = tempa;}
    }
    
    if (!sanityCheck(ixy,tempa,oxy) ) {
      cout << "StRichArea::outOfBoundsCorrection  ---> abort!" << endl;
      cout << "Problem in outer ring sanity check. " << endl;
      abort();	  
    }
    
    oxy = tempa;
    return true;
  }
  
  */

  //
  // if the points are not on the pad plane return false
  //

  return false;  
}


bool StRichArea::sanityCheck(StThreeVectorF& ixy, StThreeVectorF& mxy, StThreeVectorF& oxy ) {

  //
  // simple check to see if the middle point falls within
  // the bounds of the inner, outer points
  //


  //
  // x check
  //
  bool xok=false;
  if ( (ixy.x() >= mxy.x() && oxy.x() <= mxy.x() )  ||
        (ixy.x() <= mxy.x() && oxy.x() >= mxy.x() ) ) {
    xok = true;
  }


  //
  // y check
  //
  bool yok = false;
  if ( (ixy.y() >= mxy.y() && oxy.y() <= mxy.y() )  ||
        (ixy.y() <= mxy.y() && oxy.y() >= mxy.y() ) ) {
    yok = true;
  }
  
  if (xok && yok) return true;
  return false;
}



//
// monte carlo integration
//

void StRichArea::getMonteCarloArea(double angleCut, TArrayD& array, int points) {

  //
  // clear the vector of points stored in container
  //
  mMonteCarloPointCollection.clear();



  // 
  // check to see if the track is above threshold
  //
  if (!mInnerRing->getTrack()->fastEnough(mInnerRing->getParticleType())) {
    cout << "StRichArea::getMonteCarloArea()  track not above threshold." << endl;
    return;
  }


  //
  // create minimization routines for inner,outer, mean rings
  // need to switch to Brians routines as soon as stable
  //
  StRichRingPoint* mMeanRing = new StRichRingPoint(mInnerRing->getTrack(),eMeanRing);
  mMeanRing->setParticleType(mInnerRing->getParticleType());

  mInnerMinimization = new StRichMinimization(mInnerRing);
  mOuterMinimization = new StRichMinimization(mOuterRing);
  mMeanMinimization  = new StRichMinimization(mMeanRing);
  

  //
  // calculate area of pad plane 
  //
  double padArea  = (2.0*mRadX)*(2.0*mRadY);
 


  //
  // get number of area elements from array
  // define bin size, conversion factor
  //
  int numberOfAreaElements = array.GetSize();
  int i;
  for (i=0;i<numberOfAreaElements;i++) {array[i] = 0;}
  
  double binSizeInDegrees = 180.0*(1.0/((double) numberOfAreaElements));
  double conversionFactor = (180.0/M_PI)*(1.0/binSizeInDegrees);
  double ang=0;

  
  
  // 
  // initialize monte carlo stuff
  //
  TDatime* t1 = new TDatime();   // to be used as a seed  
  TRandom* rand = new TRandom();
  Int_t tempSeed = t1->GetTime();
  rand->SetSeed(tempSeed);
  
  //
  // main monte carlo area calculation loop
  // create a large number of random points
  // find how many points are in area, n.
  // total number thrown = N.
  // area = n*(padPlaneArea/N)
  //    


  for (i=0;i<points;i++) {
    
    getMonteCarloPadPlanePoint(rand);

    ang=0.0;
    int areaIndex=0;
    if (monteCarloHitFinder(angleCut,ang)) {
      mMonteCarloPoint.setZ(1);
      areaIndex = (int) (fabs(ang)*(conversionFactor));
      if (areaIndex <= array.GetSize()-1) {array[areaIndex]++;}
      else {
	
	cout << "StRichArea::getMonteCarloArea()" << endl;
	cout << "problem addressing vector index! abort(). " << endl;
	cout << "angle = " << ang/degree << endl;
	cout << "areaIndex > array size " << areaIndex << "    " << array.GetSize() - 1 << endl;
	//abort();
	return ;
      }
    }

    //
    // store the points for diagnostics only
    //
    mMonteCarloPointCollection.push_back(mMonteCarloPoint);

  }
  
  double tempPoints = (double) points;
  for (i=0;i<array.GetSize();i++) {array[i] = array[i]*(padArea/tempPoints);}
  
  // 
  // take care of memory 
  //
  if (rand) delete rand;
  if (mMeanRing) delete mMeanRing;

  if (mInnerMinimization) delete mInnerMinimization;
  if (mOuterMinimization) delete mOuterMinimization;
  if (mMeanMinimization)  delete mMeanMinimization;

 
  // 
  // be sure to 0 all pointers (not really needed here,
  // but always a good thing to do)
  //
  rand = 0;
  mMeanRing = 0;

  mInnerMinimization = 0;
  mOuterMinimization = 0;
  mMeanMinimization  = 0;
}


vector<StThreeVectorF> StRichArea::getMonteCarloPoints() {
  return mMonteCarloPointCollection;
}



int StRichArea::monteCarloHitFinder(double angleCut,double& angle) {

  //double testangle=100;
  int oldmethod=0;
  if (mCorrectForGap) {
    // gap check 
    if ( fabs(mMonteCarloPoint.x()) < mXGapWidth) return 0;
    if ( fabs(mMonteCarloPoint.y()) < mYGapWidth) return 0;
  }
  
  // edge of detector check
  if ( (fabs(mMonteCarloPoint.x()) <= mRadX  ) && (fabs(mMonteCarloPoint.y()) <= mRadY) ) { 

    

    //     old method
    StThreeVectorF closestInnerRingPoint = mInnerMinimization->rotatedMin(mMonteCarloPoint);
    StThreeVectorF closestOuterRingPoint = mOuterMinimization->rotatedMin(mMonteCarloPoint);
    StThreeVectorF closestMeanRingPoint = mMeanMinimization->rotatedMin(mMonteCarloPoint);
    float innerAngle = mInnerMinimization->getPsi();
    
     float ringWidtha = (closestInnerRingPoint - closestOuterRingPoint).perp();
    float innerDista = (closestInnerRingPoint - mMonteCarloPoint).perp();
    float outerDista = (closestOuterRingPoint - mMonteCarloPoint).perp();
    
    if ( (innerDista/ringWidtha < 1.0) && (outerDista/ringWidtha < 1.0) && 
      (fabs(innerAngle) > this->getConstantAreaAngle())) {
      oldmethod = 1;
    } 
    
    

    // new method

    
    double mPrecision = 100*micrometer;
    const int maxIterForInitialPsi = 40;
    const int maxIterForRingPsi    = 50;
    
    
    //
    // Find the angle Psi on the inner ring that intersects
    // the line from the MIP to the hit
    //
    StThreeVectorF central = mInnerRing->getTrack()->getImpactPoint();
    StThreeVectorF referenceLine = (mMonteCarloPoint - central);
    StThreeVectorF pointOnRing;

    //
    // Find the minimum distance to the InnerRing
    // from the hit
    //
    //float idist = (closestInnerRingPoint - mMonteCarloPoint).perp();
    
    
    mInnerRing->getPoint(innerAngle,pointOnRing);
    if(pointOnRing.x() == FLT_MAX) {return 0;}
    
    StThreeVectorF ringPointLine = pointOnRing - central;
    double psi = innerAngle;
    
    //
    // Find the distance of closest approach from the ring
    // point to the reference line:
    //
    //        ^   ^
    // | A |  A x B
    //
    // where:
    //        A = ringPointLine
    //        B = referenceLine
    //
    // z component =
    //               AxBy - AyBx
    //
    int signOfTheta  = sign(ringPointLine.x()*referenceLine.y() - ringPointLine.y()*referenceLine.x());
    double sineTheta = abs( (ringPointLine.unit()).cross(referenceLine.unit()) ); 
    float minDistToRefLine = fabs(ringPointLine.mag())* sineTheta;
    
    
		    
    StThreeVectorF newPointOnRing;
    bool anotherIteration = true;
    bool convergence      = false;
    
    double step      = 1.*degree;
    double maxChange = 5.*degree;

    //
    // take the initial step according to the sign of theta
    //    
    if(signOfTheta<0) step *= -1.;
    
    int ctr = 0;
    
    while (anotherIteration && ctr<maxIterForInitialPsi) {  // while loop

      psi += step;
      ctr++; 
      mInnerRing->getPoint(psi,newPointOnRing);
      if(newPointOnRing.x() == FLT_MAX) {
	psi -= step;
	step *= 0.5;
	continue;
      }
      
      ringPointLine = newPointOnRing - central;
      int signOfNewTheta = sign(ringPointLine.x()*referenceLine.y() - ringPointLine.y()*referenceLine.x());
      sineTheta = abs( (ringPointLine.unit()).cross(referenceLine.unit()) ); 
		    
      double newDistToRefLine = abs(ringPointLine)* sineTheta;
      if(ctr > maxIterForRingPsi)
	anotherIteration = false;
      
      if (newDistToRefLine<mPrecision) {
	convergence = true;
	break;
      }
      
      if( (signOfTheta != signOfNewTheta) || ( (signOfTheta == signOfNewTheta) && (newDistToRefLine > minDistToRefLine) ) ) {
	// wrong way
	signOfTheta = signOfNewTheta;
	step *= -0.5;
      }
      else {
	//make the step size bigger, unless you are close
	//change in the distances (check angles)
	step = (step>0) ? min(maxChange,(step*1.2)) : max(-1.*maxChange,step*1.2);
	if(newDistToRefLine < 3.*centimeter) {step = (step>0) ? min(1.*degree,step) : max(-1.*degree,step);}
	
      } // else
      
      
      minDistToRefLine = newDistToRefLine;
      
    };
    
    if(!convergence) return 0;
    
    //
    // assign the point on the ring
    //
    StThreeVectorF innerRingPoint = newPointOnRing;
    
    //
    // Given an initial guess for psi, find the "line of constant psi"
    // which crosses both rings and intersects with the hit
    //
    
    
    //
    // determine the outerRingPoint with the corresponding Psi
    //
    StThreeVectorF outerRingPoint;

    int iterationNumber = 0;
    double littleStep   = 1.*degree;
    double modifiedPsi = psi;
    if(modifiedPsi < 0) littleStep *= -1.;
    
    while(iterationNumber<maxIterForInitialPsi) {
      iterationNumber++;
      mOuterRing->getPoint(modifiedPsi,outerRingPoint);
      
      if ( (outerRingPoint.x() == FLT_MAX) || 
	   ( 
	    (fabs(outerRingPoint.x()) > mRadY) || (fabs(outerRingPoint.y()) > mRadY) 
	    )
	   ) 
	{
	  modifiedPsi+=littleStep;
	}
      else {
	// we found a point
	break;
      }
    };
    
    psi = modifiedPsi;
    
    
    mInnerRing->getPoint(psi,innerRingPoint);
    mOuterRing->getPoint(psi,outerRingPoint);

    if( (fabs(outerRingPoint.x()) > mRadX) ||
	(fabs(outerRingPoint.y()) > mRadY) ||
	(fabs(innerRingPoint.x()) > mRadX) ||
	(fabs(innerRingPoint.y()) > mRadY)  ) {
     return 0;
    }
    
    innerRingPoint.setZ(0);
    outerRingPoint.setZ(0);
    mMonteCarloPoint.setZ(0);

    //
    // the constant Psi line between the inner and outer rings
    //    
    StThreeVectorF consPsiVector = outerRingPoint - innerRingPoint; 
    
    //
    // the line from the innerRing Point to the hit
    //
    StThreeVectorF consPsiRefLine = mMonteCarloPoint - innerRingPoint;
    
    //
    // minimize the distance of closest approach between the
    // two lines as a function of the angle of the psi line
    //
    //        ^   ^
    // | A |  A x B
    //
    // where:
    //        A = consPsiRefLine
    //        B = consPsiVector
    //
    // z component =
    //               AxBy - AyBx
    //
    
    signOfTheta = sign(consPsiRefLine.x()*consPsiVector.y() - consPsiRefLine.y()*consPsiVector.x());
    sineTheta   = abs( (consPsiRefLine.unit()).cross(consPsiVector.unit()) ); 
    
    double minDistToConsPsiRefLine = consPsiRefLine.perp() * sineTheta; 
    
    anotherIteration = true;
    convergence      = false;
    step = 1.*degree;

    StThreeVectorF newInnerPointOnRing;
    StThreeVectorF newOuterPointOnRing;
    double newMinDistToConsPsiRefLine;
    
    if(signOfTheta>0) step *= -1.;
    ctr = 0;
    while (anotherIteration || ctr<maxIterForRingPsi) {
      psi += step;
      ctr++;
      mInnerRing->getPoint(psi,newInnerPointOnRing);
      mOuterRing->getPoint(psi,newOuterPointOnRing);
	    
      if( (newInnerPointOnRing.x() == FLT_MAX) ||
	  (newOuterPointOnRing.x() == FLT_MAX) ) {
	psi-=step;
	step *=0.5;
	continue;
      }
      
      consPsiRefLine = mMonteCarloPoint - newInnerPointOnRing;
      consPsiVector  = newOuterPointOnRing - newInnerPointOnRing; 
      
      //
      //        ^   ^
      // | A |  A x B
      //
      // where:
      //        A = consPsiRefLine
      //        B = consPsiVector
	    
      int signOfNewTheta = sign(consPsiRefLine.x()*consPsiVector.y() - consPsiRefLine.y()*consPsiVector.x());
      sineTheta = abs( (consPsiRefLine.unit()).cross(consPsiVector.unit()) ); 
      
      newMinDistToConsPsiRefLine = abs(consPsiRefLine) * sineTheta;
      
      if( ctr > maxIterForRingPsi )
	anotherIteration = false;
      
      if(newMinDistToConsPsiRefLine < mPrecision) {
      convergence = true;
      	break;
      }

      
      if( (signOfTheta != signOfNewTheta) ||
	  ( (signOfTheta == signOfNewTheta) &&
	    (newMinDistToConsPsiRefLine > minDistToConsPsiRefLine) ) ) {
	//wrong way
	signOfTheta = signOfNewTheta;
	step *= -0.5;
      }
      else {
	// make the step size bigger if we are moving
	// in the right direction
	step = (step>0) ? min(maxChange,(step*1.2)) : max(-1.*maxChange,step*1.2);
	      
	if(newMinDistToConsPsiRefLine < 3.*centimeter) {
	  step = (step>0) ? min(1.*degree,step) : max(-1.*degree,step);
	}
      }
      
      minDistToConsPsiRefLine = newMinDistToConsPsiRefLine;
    };
    
    if(!convergence) {return 0;}
    
    StThreeVectorF lengthOfD = newOuterPointOnRing - newInnerPointOnRing;
    
    //
    // distance to hit at constant psi
    //
    StThreeVectorF hitDVector = mMonteCarloPoint - newInnerPointOnRing;
    int signOfD = sign(lengthOfD*hitDVector);
    //float cosineOfD = lengthOfD.unit()*hitDVector.unit();
    // double normalizedD = signOfD * (abs(hitDVector)/abs(lengthOfD));
    double normalizedD = signOfD*(hitDVector.perp()/lengthOfD.perp());		
    
    if (normalizedD>0 && normalizedD<1) {
   
      if (fabs(psi)> this->getConstantAreaAngle()) {return 1;}
      
      else {

	return 0;
      }
    }
    else return 0;
  }
  
  
  return 0;
       
}
    


void StRichArea::getMonteCarloPadPlanePoint(TRandom* rand) {
  
  //
  // generate a random point on the pad plane
  // dont worry about the gap here
  // it is taken into consideration at the montecarlo hit finder
  //
  
  mMonteCarloPoint.setX(rand->Rndm()*2.0*mRadX - mRadX);
  mMonteCarloPoint.setY(rand->Rndm()*2.0*mRadY - mRadY);
  mMonteCarloPoint.setZ(0.0);
}
