/**********************************************************
 * $Id: StRichRingCalculator.cxx,v 2.6 2000/11/22 16:58:42 lasiuk Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichRingCalculator.cxx,v $
 *  Revision 2.6  2000/11/22 16:58:42  lasiuk
 *  add this to argument
 *
 *  Revision 2.5  2000/11/21 16:24:23  horsley
 *  Major overhaul of StRichArea, introduced monte carlo integration cross check,
 *  all possible areas, angles calculated together. StRichRingCalculator, StRichPIDMaker modified to support new StRichArea. StRichPIDMaker's hit finder
 *  typo corrected.
 *
 *  Revision 2.4  2000/11/01 17:40:49  lasiuk
 *  add const to access member functions
 *
 *  Revision 2.3  2000/10/19 01:13:23  horsley
 *  added member functions to StRichPIDMaker to make cuts on hits, tracks, events.
 *  added normal distance sigma cut on hits, quartz and radiator pathlengths
 *  for individual photons, modified minimization routine to correct boundary
 *  problems
 *
 *  Revision 2.2  2000/09/29 17:55:51  horsley
 *  fixed bug in Minimization routine, included StMagF stuff (commented out)
 *  changed StRichRingPoint  HUGE_VALUE   ---> MAXFLOAT for default value
 *
 *  Revision 2.1  2000/09/29 01:35:37  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.3  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:08  horsley
 *  initial revision
 **********************************************************/

#include "StRichRingCalculator.h"
#include "StRichRingDefinition.h" 
#include "StRichArea.h"
#include "StRichMaterialsDb.h"



#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichRingCalculator::StRichRingCalculator(StRichTrack* track) {

    this->init(track);
}



StRichRingCalculator::StRichRingCalculator(StRichTrack* track,
					   StParticleDefinition* particle)  {
    this->init(track);
    this->setParticleType(particle);
}


void StRichRingCalculator::init(StRichTrack* track) {
  mInnerRing = new StRichRingPoint(track,eInnerRing);
  mOuterRing = new StRichRingPoint(track,eOuterRing);
  mMeanRing  = new StRichRingPoint(track,eMeanRing);
  
  mInnerMinimization = new StRichMinimization(mInnerRing);
  mMeanMinimization  = new StRichMinimization(mMeanRing);
  mOuterMinimization = new StRichMinimization(mOuterRing);
  
  mConstantAreaAngle=M_PI;
  
  mMonteCarloSwitch = false;
  mDrawRingPoints=false;

  mRichMaterialsDb = StRichMaterialsDb::getDb();
  mRichGeometryDb  = StRichGeometryDb::getDb();

  mMonteCarloArea.Set(18);
}


StRichRingCalculator::~StRichRingCalculator() {

    delete mInnerRing;
    delete mOuterRing;
    delete mMeanRing;

    mInnerRing = 0;
    mOuterRing = 0;
    mMeanRing = 0;
  
    delete mInnerMinimization;
    delete mOuterMinimization;
    delete mMeanMinimization;

    mInnerMinimization = 0;
    mOuterMinimization = 0;
    mMeanMinimization  = 0;
}


double StRichRingCalculator::calculateArea(bool gapCorrection, double angleCut, int npoints) {
  
  StRichArea areaCalc(mInnerRing,mOuterRing);
  areaCalc.correctForGap(gapCorrection);
  areaCalc.setPoints(npoints);

  areaCalc.drawAreaRingPoints(mDrawRingPoints);
  
  double constantArea = this->getNormalArea();
  double temp =0;
  temp = areaCalc.calculateArea(constantArea,angleCut);
  
  mConstantAreaAngle = areaCalc.getConstantAreaAngle();

  mTotalArea  = areaCalc.getTotalArea();
  mTotalAngle = areaCalc.getTotalAngle();
  mTotalAreaOnPadPlane  = areaCalc.getTotalAreaOnPadPlane();
  mTotalAngleOnPadPlane = areaCalc.getTotalAngleOnPadPlane();
  mTotalAreaOnActivePadPlane  = areaCalc.getTotalAreaOnActivePadPlane();
  mTotalAngleOnActivePadPlane = areaCalc.getTotalAngleOnActivePadPlane();

  mTotalConstantArea = areaCalc.getTotalConstantArea();
  mTotalConstantAngle = areaCalc.getTotalConstantAngle();
  mTotalConstantAreaOnPadPlane  = areaCalc.getTotalConstantAreaOnPadPlane();
  mTotalConstantAngleOnPadPlane = areaCalc.getTotalConstantAngleOnPadPlane();
  mTotalConstantAreaOnActivePadPlane  = areaCalc.getTotalConstantAreaOnActivePadPlane();
  mTotalConstantAngleOnActivePadPlane = areaCalc.getTotalConstantAngleOnActivePadPlane();
  
  if (mMonteCarloSwitch) {
    areaCalc.getMonteCarloArea(angleCut,mMonteCarloArea,10000); 
    mMonteCarloPoints = areaCalc.getMonteCarloPoints();
  }

  //
  // clear vector of points
  //
  vectorOfPtsToDraw.clear();
  vectorOfPtsToDraw.resize(0);
  vectorOfPtsToDraw  = areaCalc.getPtsToDraw();

 
  return mTotalArea;
}

void StRichRingCalculator::drawRingPoints(bool flag) {
  mDrawRingPoints = flag;
}

TArrayD StRichRingCalculator::getMonteCarloArea() {
  return mMonteCarloArea;
}


vector<StThreeVectorF>&  StRichRingCalculator::getMonteCarloPoints() {
  return mMonteCarloPoints;
}

void StRichRingCalculator::setMonteCarloSwitch(bool set) {
  mMonteCarloSwitch = set;
}

StRichRingPoint* StRichRingCalculator::getRing(StRichRingDefinition ringType) {

    if (ringType==eInnerRing)  return  mInnerRing;
    if (ringType==eOuterRing)  return  mOuterRing;
    if (ringType==eMeanRing)   return  mMeanRing;
    return 0;
}

double StRichRingCalculator::getRingWidth() const {
  return  (closestInnerRingPoint - closestOuterRingPoint).perp();
}

void StRichRingCalculator::setParticleType(StParticleDefinition* particle) { 

    mInnerRing->setParticleType(particle);
    mOuterRing->setParticleType(particle);
    mMeanRing->setParticleType(particle);
}

double StRichRingCalculator::getInnerDistance(StThreeVectorF& testPoint,
					      double& innerAngle) {

    closestInnerRingPoint = mInnerMinimization->rotatedMin(testPoint);
    innerAngle            = mInnerMinimization->getPsi();
    return (closestInnerRingPoint - testPoint).perp();
}

double StRichRingCalculator::getOuterDistance(StThreeVectorF& testPoint,
					      double& outerAngle) {

    closestOuterRingPoint = mOuterMinimization->rotatedMin(testPoint);
    outerAngle            = mOuterMinimization->getPsi();
    return (closestOuterRingPoint - testPoint).perp();
}

double StRichRingCalculator::getMeanDistance(StThreeVectorF& testPoint,
					     double& meanAngle) {

    closestMeanRingPoint = mMeanMinimization->rotatedMin(testPoint);
    meanAngle            = mMeanMinimization->getPsi();
    mMeanPathInQuartz   = mMeanMinimization->getMeanPathInQuartz();
    mMeanPathInRadiator = mMeanMinimization->getMeanPathInRadiator();
    return (closestMeanRingPoint - testPoint).perp();
}

void StRichRingCalculator::clear() {
  
  StThreeVectorF temp(0,0,0);
  closestInnerRingPoint = temp;
  closestOuterRingPoint = temp;
  closestMeanRingPoint  = temp;
   
  mMeanPathInRadiator = 0.0;
  mMeanPathInQuartz   = 0.0;  
}

vector<StRichAreaSegment >& StRichRingCalculator::getPtsToDraw() { return vectorOfPtsToDraw;}



double StRichRingCalculator::getNormalArea() {

  double normalArea=0;


  if (mInnerRing && mInnerRing->getParticleType() &&
      mInnerRing->getTrack() &&  mInnerRing->getTrack()->fastEnough( mInnerRing->getParticleType())) {

    double mass = mInnerRing->getParticleType()->mass();
    double p    = mInnerRing->getTrack()->getMomentum().mag();
    double beta = p/sqrt(p*p + mass*mass);

    // detector parameters used in light propagation to pad plane depth
    double depthRad  = mRichGeometryDb->radiatorDimension().z()*centimeter;  
    double depthQuar = mRichGeometryDb->quartzDimension().z()*centimeter;
    double depthProx = mRichGeometryDb->proximityGap()*centimeter;
    
    double freon[2];
    freon[0] = mRichMaterialsDb->indexOfRefractionOfC6F14At(mRichMaterialsDb->innerWavelength());
    freon[1] = mRichMaterialsDb->indexOfRefractionOfC6F14At(mRichMaterialsDb->outerWavelength());
    
    double quartz[2];
    quartz[0] = mRichMaterialsDb->indexOfRefractionOfQuartzAt(mRichMaterialsDb->innerWavelength());
    quartz[1] = mRichMaterialsDb->indexOfRefractionOfQuartzAt(mRichMaterialsDb->outerWavelength());
    
    double methane[2];
    methane[0] = mRichMaterialsDb->indexOfRefractionOfMethaneAt(mRichMaterialsDb->innerWavelength());
    methane[1] = mRichMaterialsDb->indexOfRefractionOfMethaneAt(mRichMaterialsDb->outerWavelength());
    
    
    double radii[2];
    for (int i=0;i<2;i++) {
      double cAngle = acos(1.0/(freon[i]*beta));
      double qAngle = asin((freon[i]/quartz[i])*sin(cAngle));
      double mAngle = asin((quartz[i]/methane[i])*sin(qAngle));
      if (i==0) {radii[i] = depthQuar*tan(qAngle) + depthProx*tan(mAngle);}
      else {radii[i] = depthRad*tan(cAngle) + depthQuar*tan(qAngle) + depthProx*tan(mAngle);}
    }
    normalArea = M_PI*(radii[1]*radii[1] - radii[0]*radii[0]);
  }

  return normalArea;
}

















