/**********************************************************
 * $Id: StRichArea.cxx,v 2.0 2000/08/09 16:26:17 gans Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichArea.cxx,v $
 *  Revision 2.0  2000/08/09 16:26:17  gans
 *  Naming Convention for TDrawable Ojects. All drawable objects now in StRichDisplayMaker
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

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichArea::StRichArea(StRichRingPoint* irp, 
		       StRichRingPoint* orp) {

  mInnerRing      = irp;
  mOuterRing      = orp;  

  mNumberOfSteps  = 3600.0;
  
  mPositiveDirection =  1.0;
  mNegativeDirection = -1.0;
  mSmallAngleStep    = 0.0000174532925;  // ---> 0.01 degrees
  mStopCounter       = static_cast<int>(M_PI/mSmallAngleStep);
  mNotEnoughAngleForCalculation = 179.0*degree;

  mSmallDistance  = 0.01*centimeter;
  mTooManyCounts = (int) (mSmallDistance*10000000.0);   
  mCorrectForGap  = true;  // ---> here we are concerned with the gap correction

  myGeometryDb = StRichGeometryDb::getDb();
  mTotalAngleOnPadPlane = 0;
}


StRichArea::~StRichArea() { /* nopt */ }

void StRichArea::correctForGap(bool gapSwitch) {
  mCorrectForGap = gapSwitch;
};

vector<StThreeVector<double> >& StRichArea::getPtsToDraw() {
  return vectorOfPtsToDraw;
}

double StRichArea::calculateArea(double psiCut) {
  vectorOfPtsToDraw.resize(0);
  vectorOfPtsToDraw.clear();

  mTotalAngleOnPadPlane = 0.0;
  mTotalArea      = 0.0;
  mAreaSegment    = 0.0;
  mStartAngle     = M_PI;
  
  mStopAngle      = getStoppingAngle(psiCut);  
  if (mStopAngle>mNotEnoughAngleForCalculation) return 0;
  
  mAngleIncrement = M_PI/(mNumberOfSteps/2.0);
  int stopHere = (int) (mStopAngle/mAngleIncrement);


  StThreeVector<double> innerPointForDrawing,outerPointForDrawing;
  // -----------  pi ---> 0 degs  ---------------------- // 
  double angle = mStartAngle;
  if (!getRingPoints(angle,mInXYA,mOutXYA,mPositiveDirection)) {
    cout << "StRichNewArea::calculateArea  a --> starting point returns false. abort!" << endl;
    abort();
  }
  
  
  for (int i=0;i<(mNumberOfSteps/2.0 - stopHere);i++) {
    mAreaSegment = 0.0;
    
    if (getRingPoints(angle,mInXYB,mOutXYB,mPositiveDirection)) {      
      getAreaSegment(mInXYA,mOutXYA,mInXYB,mOutXYB,mAreaSegment);
      mTotalArea += mAreaSegment;
      if (mAreaSegment>0) {mTotalAngleOnPadPlane++;}
    }

    if (mInnerPtForDrawingA.x() !=-999) vectorOfPtsToDraw.push_back(mInnerPtForDrawingA);
    if (mOuterPtForDrawingA.x() !=-999) vectorOfPtsToDraw.push_back(mOuterPtForDrawingA);
    if (mInnerPtForDrawingB.x() !=-999) vectorOfPtsToDraw.push_back(mInnerPtForDrawingB);
    if (mOuterPtForDrawingB.x() !=-999) vectorOfPtsToDraw.push_back(mOuterPtForDrawingB);
    if (mInnerPtForDrawingC.x() !=-999) vectorOfPtsToDraw.push_back(mInnerPtForDrawingC);
    if (mOuterPtForDrawingC.x() !=-999) vectorOfPtsToDraw.push_back(mOuterPtForDrawingC);

    swapPoints(mInXYA,mOutXYA,mInXYB,mOutXYB);
  }
  
  // ---------   -pi ---> 0 degs  -------------------- //
  angle = -mStartAngle;
  if (!getRingPoints(angle,mInXYA,mOutXYA,mNegativeDirection)) {
    cout << "StRichNewArea::calculateArea  b --> starting point returns false. abort!" << endl;
    abort();
  }

  for (int i=0;i<(mNumberOfSteps/2.0 - stopHere);i++) {
    mAreaSegment = 0.0;

    if (getRingPoints(angle,mInXYB,mOutXYB,mNegativeDirection)) {
      getAreaSegment(mInXYA,mOutXYA,mInXYB,mOutXYB,mAreaSegment);
      mTotalArea += mAreaSegment; 
      if (mAreaSegment>0) {mTotalAngleOnPadPlane++;}
    }


    if (mInnerPtForDrawingA.x() !=-999) vectorOfPtsToDraw.push_back(mInnerPtForDrawingA);
    if (mOuterPtForDrawingA.x() !=-999) vectorOfPtsToDraw.push_back(mOuterPtForDrawingA);
    if (mInnerPtForDrawingB.x() !=-999) vectorOfPtsToDraw.push_back(mInnerPtForDrawingB);
    if (mOuterPtForDrawingB.x() !=-999) vectorOfPtsToDraw.push_back(mOuterPtForDrawingB);
    if (mInnerPtForDrawingC.x() !=-999) vectorOfPtsToDraw.push_back(mInnerPtForDrawingC);
    if (mOuterPtForDrawingC.x() !=-999) vectorOfPtsToDraw.push_back(mOuterPtForDrawingC);
    
    swapPoints(mInXYA,mOutXYA,mInXYB,mOutXYB);
  }
  
 
  mTotalAngleOnPadPlane = mTotalAngleOnPadPlane*mAngleIncrement;
  return mTotalArea;
}



double StRichArea::getStoppingAngle(double cutAngle) {
    StThreeVector<double> in;
    double stopAngle = 0.0;
  
    bool iStatus = mInnerRing->getPoint(stopAngle,in); 
    
    int safetyCounter=0;
    while (!iStatus) {
      stopAngle += mSmallAngleStep;
      iStatus = mInnerRing->getPoint(stopAngle,in); 
      
      safetyCounter++;
      if (safetyCounter>mStopCounter) {
	cout << "Track parameters: " << endl;
	cout << "-----------------------" << endl;
	cout << "inner ring ---> p = " << mInnerRing->getTrack()->getMomentum() << endl;
	cout << "outer ring ---> p = " << mOuterRing->getTrack()->getMomentum() << endl;
	cout << "inner ring ---> xy = " << mInnerRing->getTrack()->getImpactPoint() << endl;
	cout << "outer ring ---> xy = " << mOuterRing->getTrack()->getImpactPoint() << endl;
	cout << "inner ring ---> theta = " << mInnerRing->getTrack()->getTheta()/degree 
	     << endl;
	cout << "outer ring ---> theta = " << mOuterRing->getTrack()->getTheta()/degree 
	     << endl;

	cout << "stop angle = " << stopAngle/degree << endl;
	cout << "safetyCounter = " << safetyCounter << endl;
	cout << "iStatus = " << iStatus << endl;
	
	//////////////  need to include a flag for this case and keep track of this
	////////////////////////////
      }
    }
    
    

    if (stopAngle<cutAngle) {
      return cutAngle;}
    
    return stopAngle;
}



double 
StRichArea::getTotalAngleOnPadPlane() {
  return mTotalAngleOnPadPlane;
}



bool
StRichArea::getRingPoints(double& angle, 
			          StThreeVector<double>& ixy, 
			          StThreeVector<double>& oxy, 
			          int direction) {
  
  StThreeVector<double> tempInner(0,0,0);
  StThreeVector<double> tempOuter(0,0,0);
  
  bool mInStatus  = mInnerRing->getPoint(angle,tempInner);
  bool mOutStatus = mOuterRing->getPoint(angle,tempOuter);
    
  angle -= ((double) direction)*mAngleIncrement;  
  
  if (mInStatus)  {ixy = tempInner;}
  if (mOutStatus) {oxy = tempOuter;}
  
  if (mInStatus) {
    return true;  
  }
  
  return false;
}



void
StRichArea::swapPoints(StThreeVector<double>& ixya,
		              StThreeVector<double>& oxya,
		              StThreeVector<double>& ixyb, 
		              StThreeVector<double>& oxyb) {
      
  // don't really have to swap the points, its 
  // enough to just set a = b
  ixya = ixyb;
  oxya = oxyb;
}



void
StRichArea::getAreaSegment(StThreeVector<double>& ixya,
			            StThreeVector<double>& oxya,
			            StThreeVector<double>& ixyb, 
			            StThreeVector<double>& oxyb, 
			            double& padAreaSeg) {
  
  mInnerPtForDrawingA.setX(-999);
  mInnerPtForDrawingA.setY(-999);
  mOuterPtForDrawingA.setX(-999);
  mOuterPtForDrawingA.setY(-999);
  
  mInnerPtForDrawingB.setX(-999);
  mInnerPtForDrawingB.setY(-999);
  mOuterPtForDrawingB.setX(-999);
  mOuterPtForDrawingB.setY(-999);
  
  mInnerPtForDrawingC.setX(-999);
  mInnerPtForDrawingC.setY(-999);
  mOuterPtForDrawingC.setX(-999);
  mOuterPtForDrawingC.setY(-999);

  padAreaSeg = 0;
  if (outOfBoundsCorrection(ixya,oxya) && outOfBoundsCorrection(ixyb,oxyb) ) {  
    StThreeVector<double> tempixya,tempoxya,tempixyb,tempoxyb;
 
    if (!mCorrectForGap) {
      mInnerPtForDrawingA = tempixya;
      mOuterPtForDrawingA = tempoxya;
      // calculate total area segment here
      // no gap correction necessary    
      double term1,term2,term3,term4;
      term1 = (ixya.x()*oxya.y() - oxya.x()*ixya.y());
      term2 = (oxya.x()*oxyb.y() - oxyb.x()*oxya.y());
      term3 = (oxyb.x()*ixyb.y() - ixyb.x()*oxyb.y());
      term4 = (ixyb.x()*ixya.y() - ixya.x()*ixyb.y());
      padAreaSeg = 0.5*fabs(term1 + term2 + term3 + term4); 
      return;
    }
    


    // order of cases is important, ie 1 then 2 then 3 , etc...
    // case 1: 1 pt in quadrant, 1 pt in gap (either set of pts)
    // make correction, pass corrected pts to next function   
    tempixya=ixya;
    tempoxya=oxya;
    tempixyb=ixyb;
    tempoxyb=oxyb;
    
    bool setOneCheck = partialGapCorrection(ixya,oxya,tempixya,tempoxya);  
    bool setTwoCheck = partialGapCorrection(ixyb,oxyb,tempixyb,tempoxyb); 
    if ( !(setOneCheck*setTwoCheck) ) return;
    

    // case 2: all pts in same quadrant
    if (quadCheck(tempixya,tempoxya) && 
	quadCheck(tempixyb,tempoxyb) && 
	quadCheck(tempixya,tempoxyb)) {
      double term1,term2,term3,term4;
      term1 = (tempixya.x()*tempoxya.y() - tempoxya.x()*tempixya.y());
      term2 = (tempoxya.x()*tempoxyb.y() - tempoxyb.x()*tempoxya.y());
      term3 = (tempoxyb.x()*tempixyb.y() - tempixyb.x()*tempoxyb.y());
      term4 = (tempixyb.x()*tempixya.y() - tempixya.x()*tempixyb.y());
      padAreaSeg = 0.5*fabs(term1 + term2 + term3 + term4); 
      mInnerPtForDrawingA = tempixya;
      mOuterPtForDrawingA = tempoxya;
      return;
    }
    

    // case 3: 2 pts in one quadrant, 2 pts in adjacent quadrant
    if (adjacentCheck(tempixya,tempoxya) && adjacentCheck(tempixyb,tempoxyb)) {
      StThreeVector<double> iEdgea,oEdgea,iEdgeb,oEdgeb;
      if ( fullGapCorrectionNecessary(tempixya,tempoxya,iEdgea,oEdgea) &&
	   fullGapCorrectionNecessary(tempixyb,tempoxyb,iEdgeb,oEdgeb)) {
	mInnerPtForDrawingA = tempixya;
	mOuterPtForDrawingA = tempoxya;
	mInnerPtForDrawingB = iEdgea;
	mOuterPtForDrawingB = oEdgea;
	
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
	
	padAreaSeg = 0.5*fabs(term1a + term2a + term3a + term4a) 
	           + 0.5*fabs(term1b + term2b + term3b + term4b);   
	return;
      }
    }
    

    // case 4: 2 pts in one quadrant, 2 pts in non-adjacent quadrant
    if (!adjacentCheck(tempixya,tempoxya) && !adjacentCheck(tempixyb,tempoxyb)) {
      StThreeVector<double> ixyaa,ixyab,oxyaa,oxyab;
      StThreeVector<double> ixyba,ixybb,oxyba,oxybb;
      if ( nonAdjacentGapCorrection(tempixya,tempoxya,ixyaa,oxyaa,ixyab,oxyab) &&
	   nonAdjacentGapCorrection(tempixyb,tempoxyb,ixyba,oxyba,ixybb,oxybb)) {
	mInnerPtForDrawingA = tempixya;
	mOuterPtForDrawingA = tempoxya;
	mInnerPtForDrawingB = ixyaa;
	mOuterPtForDrawingB = oxyaa;
	mInnerPtForDrawingC = ixyab;
	mOuterPtForDrawingC = oxyab;
	
	
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
	
	padAreaSeg = 0.5*fabs(term1a + term2a + term3a + term4a) 
	           + 0.5*fabs(term1b + term2b + term3b + term4b)
	           + 0.5*fabs(term1c + term2c + term3c + term4c);
	return;
      }
    }


    // only the partial gap correction was necessary
    double term1c,term2c,term3c,term4c;
    term1c = (tempixya.x()*tempoxya.y() - tempoxya.x()*tempixya.y());
    term2c = (tempoxya.x()*tempoxyb.y() - tempoxyb.x()*tempoxya.y());
    term3c = (tempoxyb.x()*tempixyb.y() - tempixyb.x()*tempoxyb.y());
    term4c = (tempixyb.x()*tempixya.y() - tempixya.x()*tempixyb.y());
    padAreaSeg = 0.5*fabs(term1c + term2c + term3c + term4c); 
    return;
 
  }
  
  return;
}


bool 
StRichArea::nonAdjacentGapCorrection(StThreeVector<double>& ixy,   
				     StThreeVector<double>& oxy,
				     StThreeVector<double>& ixya, 
				     StThreeVector<double>& oxya,
				     StThreeVector<double>& ixyb, 
				     StThreeVector<double>& oxyb) {
  if ( !mCorrectForGap )   return false;

  // checks if one (or both) point(s) in gap
  if ( gapCheck(ixy) || gapCheck(oxy) )  return false;
    

  /////////////   do gap correction   //////////////////
  // here we get the point closest to the outer point
  double phi = atan2(ixy.y()-oxy.y(),ixy.x()-oxy.x());    
  StThreeVector<double> tempo,tempi,tempa;
  StThreeVector<double> temp(mSmallDistance*centimeter,
			       mSmallDistance*centimeter,
			       mSmallDistance*centimeter);
  tempo = oxy;
  tempi = ixy;

  int safetyCheck=0;
  while (temp.mag()>mSmallDistance) { 
    safetyCheck++;
    if (safetyCheck>mTooManyCounts) {
      cout << "StRichArea::adjacentGapCorrectionNecessary  ---> abort!" << endl;
      cout << "Problem in outer ring gap correction. " << endl;      
      abort();}
    
    temp = tempi-tempo;
    double v = 0.5*temp.mag();
    tempa.setX(tempo.x() + v*cos(phi));
    tempa.setY(tempo.y() + v*sin(phi));
    if (quadCheck(oxy,tempa)) {tempo = tempa;} 
    else {tempi = tempa;}
  }  
  oxya = tempa;
  
  if (!sanityCheck(ixy,oxya,oxy) ) {
    cout << "StRichArea::adjacentGapCorrectionNecessary() ---> abort()!" << endl;
    cout << "Problem in outer ring gap correction, sanity check." << endl;
    abort();}

 
  // now get the next point across from the gap
  oxyb=oxya;
  for (int gapCounter=0;gapCounter<mTooManyCounts;gapCounter++) {
    oxyb.setX(oxyb.x() + mSmallDistance*cos(phi));
    oxyb.setY(oxyb.y() + mSmallDistance*sin(phi));
    if (!gapCheck(oxyb) && !quadCheck(oxya,oxyb)) break;
  }


  // here we get the point closest to the inner point
  phi = atan2(oxy.y()-ixy.y(),oxy.x()-ixy.x());
  
  // set temp distance to some distance 
  // bigger than mSmallDistance
  temp.setX(mSmallDistance*centimeter);
  temp.setY(mSmallDistance*centimeter);
  temp.setZ(mSmallDistance*centimeter);

  tempo = oxy;
  tempi = ixy;
  
  safetyCheck=0;
  while (temp.mag()>mSmallDistance) {

    safetyCheck++;
    if (safetyCheck>mTooManyCounts) {
      cout << "StRichArea::adjacentGapCorrectionNecessary  ---> abort!" << endl;
      cout << "Problem in inner ring gap correction. " << endl;      
      abort();}
    
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
    abort();}
  
  
   // now get the next point across from the gap
  ixyb=ixya;
  for (int gapCounter=0;gapCounter<mTooManyCounts;gapCounter++) {
    ixyb.setX(ixyb.x() + mSmallDistance*cos(phi));
    ixyb.setY(ixyb.y() + mSmallDistance*sin(phi));
    if (!gapCheck(ixyb) && !quadCheck(ixya,ixyb)) break;
  }

  // now check to see if the ixyb, oxyb are the right pts, 
  // if not, then set them ixyb=-999 and  oxyb=-999 (see page 88 in note book
  // for better explanation)
  if ( (oxy-ixyb).mag() > (oxy-oxyb).mag() ) {
    ixyb.setX(-999);
    ixyb.setY(-999);
    oxyb.setX(-999);
    oxyb.setY(-999);
  }


  return true;
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
  StThreeVector<double> temp(mSmallDistance*centimeter,
			       mSmallDistance*centimeter,
			       mSmallDistance*centimeter);
  tempo = oxy;
  tempi = ixy;

  int safetyCheck=0;
  while (temp.mag()>mSmallDistance) { 
    safetyCheck++;
    if (safetyCheck>mTooManyCounts) {
      cout << "StRichArea::fullGapCorrectionNecessary  ---> abort!" << endl;
      cout << "Problem in outer ring gap correction. " << endl;      
      abort();}
    
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
    abort();}
 
  // here we get the point closest to the inner point
  phi = atan2(oxy.y()-ixy.y(),oxy.x()-ixy.x());
  
  // set temp distance to some distance 
  // bigger than mSmallDistance
  temp.setX(mSmallDistance*centimeter);
  temp.setY(mSmallDistance*centimeter);
  temp.setZ(mSmallDistance*centimeter);

  tempo = oxy;
  tempi = ixy;
  
  safetyCheck=0;
  while (temp.mag()>mSmallDistance) {

    safetyCheck++;
    if (safetyCheck>mTooManyCounts) {
      cout << "StRichArea::fullGapCorrectionNecessary  ---> abort!" << endl;
      cout << "Problem in inner ring gap correction. " << endl;      
      abort();}
    
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
    abort();}
  
  return true;
}

bool StRichArea::partialGapCorrection(StThreeVector<double>& ixy,   
				                  StThreeVector<double>& oxy,
				                  StThreeVector<double>& tempixy, 
				                  StThreeVector<double>& tempoxy) {
  
  // if neither point in gap, no correction necessary
  if ( !gapCheck(ixy) && !gapCheck(oxy) ) {
    return true;
  }
  

  // Exclusive OR  -->  ^
  if (gapCheck(ixy)^gapCheck(oxy)) {
    StThreeVector<double> tempo,tempi,tempa,temp;
    double phi;
    tempixy = ixy;
    tempoxy = oxy;
    
    // inner ring point in gap
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
      //  if (!sanityCheck(ixy,tempa,oxy) ) {abort();}
      tempixy = tempa;
      return true;
    }
    
    
    
    // outer ring point in gap
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
      //  if (!sanityCheck(ixy,tempa,oxy) ) {abort();}
      tempoxy = tempa;
      return true;
    }
  }

  // both points fall in gap, 
  // try to make correction  
  else  {
    
    StThreeVector<double> tempo,tempi,tempa,temp;
    double phi;
    tempixy = ixy;
    tempoxy = oxy;
    
    // inner ring point in gap
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
    
    
    
    // outer ring point in gap
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
StRichArea::inBounds(StThreeVector<double>& xy) {
  
  if ( (xy.x() > -myGeometryDb->radiatorDimension().x() && 
	xy.x() <  myGeometryDb->radiatorDimension().x() ) && 
       
       (xy.y() > -myGeometryDb->radiatorDimension().y() && 
	xy.y() <  myGeometryDb->radiatorDimension().y() ) ) { 
    return true;
  }
  
  return false;
}


bool 
StRichArea::adjacentCheck(StThreeVector<double>& ixy, StThreeVector<double>& oxy) {
  StThreeVector<double> check = ixy*oxy;
  if (check.x() > 0 || check.y() > 0) {
    return true;
  }
  
  return false;
}


bool 
StRichArea::outOfBoundsCorrection(StThreeVector<double>& ixy, 
				                 StThreeVector<double>& oxy) {
 
  if (inBounds(oxy) && inBounds(ixy)) {
    return true;
  }

  if (inBounds(oxy)) {
      double phi = atan2(ixy.y()-oxy.y(),ixy.x()-oxy.x());    
      StThreeVector<double> tempo,tempi,tempa;

      StThreeVector<double> temp(mSmallDistance,
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
    StThreeVector<double> tempo,tempi,tempa;
      StThreeVector<double> temp(mSmallDistance,
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
  
  // if the points are not on the pad plane return false
  return false;
}


bool StRichArea::sanityCheck(StThreeVector<double>& ixy, 
			                  StThreeVector<double>& mxy, 
			                  StThreeVector<double>& oxy ) {

  // simple check to see if the middle point falls within
  // the bounds of the inner, outer points
  
  // x check
  bool xok=false;
  if ( (ixy.x() >= mxy.x() && oxy.x() <= mxy.x() )  ||
        (ixy.x() <= mxy.x() && oxy.x() >= mxy.x() ) ) {
    xok = true;
  }

  // y check
  bool yok = false;
  if ( (ixy.y() >= mxy.y() && oxy.y() <= mxy.y() )  ||
        (ixy.y() <= mxy.y() && oxy.y() >= mxy.y() ) ) {
    yok = true;
  }
  
  if (xok && yok) return true;
  return false;
}





