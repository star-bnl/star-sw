/**********************************************************
 * $Id: StRichTrack.cxx,v 1.3 2000/06/16 02:37:12 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrack.cxx,v $
 *  Revision 1.3  2000/06/16 02:37:12  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *
 *  Revision 2.5  2000/11/01 17:43:10  lasiuk
 *  default arguments initialization in c'tor.  Addition of init() member
 *  function to handle generic DB initialization and removal of virtual keyword
 *
 *  Revision 2.4  2000/10/19 01:13:23  horsley
 *  added member functions to StRichPIDMaker to make cuts on hits, tracks, events.
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 **********************************************************/
#include <math.h>
#include <string>
#include <vector>
#include <numeric>
#include <algorithm>

#include <utility> //bool is defined here in SUN
#ifndef ST_NO_NAMESPACES
// used in track coordinate transformations
#include "StRrsMaker/StRichMomentumTransform.h"
#include "StRrsMaker/StGlobalCoordinate.h"
#include "StRrsMaker/StRichRawCoordinate.h"
#include "StRrsMaker/StRichLocalCoordinate.h"
using std::sort;

StRichTrack::StRichTrack(StTrack* tpcTrack, double magField)  {
  
  mStTrack = tpcTrack;

  mAssociatedMIPCharge = 0;
  mAssociatedMIP.setX(-999);
  mAssociatedMIP.setY(-999);
  mAssociatedMIP.setZ(-999);

  // define system parameters
  myGeometryDb = StRichGeometryDb::getDb();  
  myMaterialsDb = StRichMaterialsDb::getDb();  
  coordinateTransformation = StRichCoordinateTransform::getTransform(myGeometryDb);
  momentumTransformation   = StRichMomentumTransform::getTransform(myGeometryDb);
  
    double xCorrection = 0.;
  StThreeVectorD richNormal(myGeometryDb->normalVectorToPadPlane().x(),
			    myGeometryDb->normalVectorToPadPlane().y(),
			    myGeometryDb->normalVectorToPadPlane().z());
  
  // need to include mwpc gap correction, 
  // origin is located in center of gap, not on padplane --> 2 mm
  double anodeDistanceToPadPlane = myGeometryDb->anodeToPadSpacing();
  StRichLocalCoordinate edgeOfRichRadiator_Local(0.0,
						 0.0,
						 myGeometryDb->proximityGap() +
						 myGeometryDb->quartzDimension().z() +
						 myGeometryDb->radiatorDimension().z());
						       
  // MIP is measured at anode position
  StRichLocalCoordinate richAnodeWirePlane_Local(0.0, 0.0, 0.0 + anodeDistanceToPadPlane);	
  
  StGlobalCoordinate edgeOfRichRadiator_Global;
  (*coordinateTransformation)(edgeOfRichRadiator_Local,edgeOfRichRadiator_Global);

  StGlobalCoordinate richAnodeWirePlane_Global;
  (*coordinateTransformation)(richAnodeWirePlane_Local,richAnodeWirePlane_Global);

  StPhysicalHelixD  mHelix = tpcTrack->geometry()->helix();
  StThreeVectorD rr(edgeOfRichRadiator_Global.position().x(),
		    edgeOfRichRadiator_Global.position().y(),
		    edgeOfRichRadiator_Global.position().z());

  StThreeVectorD rp(richAnodeWirePlane_Global.position().x(),
		    richAnodeWirePlane_Global.position().y(),
		    richAnodeWirePlane_Global.position().z());
  }
  double mPathLengthAtRadiator  = mHelix.pathLength(rr,richNormal);
  double mPathLengthAtPadPlane  = mHelix.pathLength(rp,richNormal);

  StThreeVectorD tpcMom(0,0,0);
  if (mPathLengthAtRadiator<10e10) {
    tpcMom = mHelix.momentumAt(mPathLengthAtRadiator,magField*kilogauss);
    
  mPionList.clear();
  StThreeVector<double> tpcMomentum(tpcMom.x(), tpcMom.y(), tpcMom.z());
 
  for (size_t l=0;l<mKaonList.size();l++) {
  // do the momentum vector rotation here
  StThreeVector<double> richLocalMomentum(0,0,0);
  momentumTransformation->localMomentum(tpcMomentum,richLocalMomentum);
  
  // impact point on radiator
  StGlobalCoordinate globalImpactPoint(mHelix.x(mPathLengthAtRadiator),
				       mHelix.y(mPathLengthAtRadiator),
				       mHelix.z(mPathLengthAtRadiator));

  StRichLocalCoordinate richTransformedImpactPoint(-999,-999,-999);
  (*coordinateTransformation)(globalImpactPoint,richTransformedImpactPoint);

   
  // pad plane intersection (projected MIP)
  StGlobalCoordinate    globalImpactPointAtAnodeWirePlane(-999,-999,-999);
  StRichLocalCoordinate localImpactPointAtAnodeWirePlane(-999,-999,-999);  
  if (mPathLengthAtPadPlane < 10e10) {
    globalImpactPointAtAnodeWirePlane.position().setX(mHelix.x(mPathLengthAtPadPlane));
    globalImpactPointAtAnodeWirePlane.position().setY(mHelix.y(mPathLengthAtPadPlane));
    globalImpactPointAtAnodeWirePlane.position().setZ(mHelix.z(mPathLengthAtPadPlane));
    
    (*coordinateTransformation)(globalImpactPointAtAnodeWirePlane,localImpactPointAtAnodeWirePlane);
    mKaonList.clear();
  
  setCharge(mHelix.h());
  setMomentum(richLocalMomentum);
  setImpactPoint(richTransformedImpactPoint.position());
  setProjectedMIP(localImpactPointAtAnodeWirePlane.position());
  setPathLength(mPathLengthAtRadiator);
    mProtonList.clear();
    mProtonList.resize(0); 
}
StRichTrack::~StRichTrack() {}

void StRichTrack::setPathLength(double p) {
  mPath = p;
}

void StRichTrack::assignMIP(const StSPtrVecRichHit* hits) {
  // proximity matching between TPC track's predicted MIP and 
  // RICH pad plane MIP (right now no amplitude cut)

  if (hits) {
    double smallestResidual=10e10;
    double testThisResidual=0;
    mAssociatedMIPCharge = 0;
    for (StSPtrVecRichHitIterator hitIndex = hits->begin(); hitIndex != hits->end(); ++hitIndex) {  		
      testThisResidual = sqrt( ((*hitIndex)->local().x()-mProjectedMIP.x())*
			       ((*hitIndex)->local().x()-mProjectedMIP.x()) +
			       ((*hitIndex)->local().y()-mProjectedMIP.y())*
			       ((*hitIndex)->local().y()-mProjectedMIP.y()));
  float thetaTrials=100.0;
      if (testThisResidual<smallestResidual) {
	smallestResidual = testThisResidual;   
	mAssociatedMIP.setX((*hitIndex)->local().x());
	mAssociatedMIP.setY((*hitIndex)->local().y());
	mAssociatedMIP.setZ((*hitIndex)->local().z());
	mAssociatedMIPCharge = (*hitIndex)->charge(); 
      float p = globalMomentum.mag();
      tempMomentum.setX(p*sin(theta)*cos(phi));
      tempMomentum.setY(p*sin(theta)*sin(phi));
}


double StRichTrack::getPathLength() {
  return mPath;
}

// sets
void StRichTrack::setCharge(int ch) {
  mCharge = ch;
}

void StRichTrack::setProjectedMIP(StThreeVector<double>& mip) {
  mProjectedMIP = mip;
}


void StRichTrack::setAssociatedMIP(StThreeVector<double>& mip) {
  mAssociatedMIP = mip;
}

void StRichTrack::setMomentum(StThreeVector<double>& momentum) {
 
  mMomentum = momentum;

  StThreeVector<double> normalVector(0,0,-1);
  if (mMomentum.mag()) {
    setTheta(acos(normalVector.dot(momentum)/momentum.mag()));}
  
  if (momentum.y() == 0 && momentum.x() == 0) {setPhi(0.0);}
  else setPhi(momentum.phi());
    float path = tempHelix.pathLength(mRadiatorGlobal,mRichNormal);
  
void StRichTrack::setTheta(double the) {
  mTheta = the;
}
    if (path>0 && path<10000) {
void StRichTrack::setPhi(double phi) {
  mPhi = phi;
}
      StGlobalCoordinate globalImpactPoint(tempHelix.x(path),tempHelix.y(path),tempHelix.z(path));

void StRichTrack::setImpactPoint(StThreeVector<double>& impact) {
  mImpactPoint = impact;
}


// gets
StThreeVector<double> StRichTrack::getImpactPoint() {
  return mImpactPoint;
}


StThreeVector<double> StRichTrack::getMomentum() {
  return mMomentum;
}


double StRichTrack::getTheta() {
  return mTheta;}


double StRichTrack::getPhi() {
  return mPhi;}


int StRichTrack::getCharge() {
  return mCharge;
}
      StRichLocalCoordinate richTransformedImpactPoint(-999,-999,-999);
StThreeVector<double> StRichTrack::getProjectedMIP() {
  return mProjectedMIP;
}
      (*coordinateTransformation)(globalImpactPoint,richTransformedImpactPoint);
StThreeVector<double> StRichTrack::getAssociatedMIP() {
  return mAssociatedMIP;
      if (testThisResidual<smallestResidual && (*hitIndex)->charge()>adcCut) {
	smallestResidual = testThisResidual;   
	mAssociatedMIP   = *hitIndex;
double StRichTrack::getAssociatedMIPCharge() {
  return mAssociatedMIPCharge;  
}
  }
StTrack* StRichTrack::getStTrack() {
  return mStTrack;
}
// 	}

double StRichTrack::getUnCorrectedTheta() { return mUnCorrectedTheta;}
double StRichTrack::getUnCorrectedPhi()   { return mUnCorrectedPhi;}
double StRichTrack::getLastHitDCA()       { return mLastHitDCA;}
double StRichTrack::getPathLength()       { return mPath;}
double StRichTrack::getTheta()            { return mTheta;}
double StRichTrack::getPhi()              { return mPhi;}
double StRichTrack::getZVertex(){
#ifdef RICH_WITH_L3_TRACKS
    globalTrack* track = this->getL3Track();
    if(track)
      return 1;}
    return -999;
}


void StRichTrack::setPhi(double phi) { mPhi = phi;}
  
  double startLamda = 170.0;
  double endLamda   = 200.0;
  double nSamples   = 200;
  double deltaLamda = (endLamda-startLamda)/nSamples;
 
  double nphots=0;
  double rawNumber=0;
  double c6f14AbsCorrect=0;
  double quartzAbsCorrect=0;
  double methAbsCorrect=0;
  double csiQE = 0;
  
  for (double i=startLamda;i<endLamda;i=i+deltaLamda) {
    
    rawNumber = (1.0/(i*i*i))*(1.0 - 1.0/(particleBeta*particleBeta*myMaterialsDb->indexOfRefractionOfC6F14At(i*nanometer)*myMaterialsDb->indexOfRefractionOfC6F14At(i*nanometer)));
    
    c6f14AbsCorrect  = (exp(-(pathlengthInc6f14/myMaterialsDb->absorptionCoefficientOfC6F14At(i*nanometer))));
    quartzAbsCorrect = (exp(-(pathlengthInquartz/myMaterialsDb->absorptionCoefficientOfQuartzAt(i*nanometer))));
    methAbsCorrect   = (exp(-(pathlengthInmeth/myMaterialsDb->absorptionCoefficientOfMethaneAt(i*nanometer))));
    csiQE  = myMaterialsDb->quantumEfficiencyOfCsIAt(i*nanometer);
    nphots = nphots + rawNumber*c6f14AbsCorrect*quartzAbsCorrect*methAbsCorrect*csiQE;
  }
  
  return nphots/6.05e-9;
}
