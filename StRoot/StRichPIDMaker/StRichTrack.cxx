/**********************************************************
 * $Id: StRichTrack.cxx,v 1.2 2000/05/19 19:06:11 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrack.cxx,v $
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
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
  
  // define system parameters
  StRichGeometryDb* myGeometryDb = StRichGeometryDb::getDb();
  
  StRichCoordinateTransform* coordinateTransformation =
        StRichCoordinateTransform::getTransform(myGeometryDb);
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
  StRichLocalCoordinate edgeOfRichPadPlane_Local(0.0, 0.0, 0.0 + anodeDistanceToPadPlane);	
  
  StGlobalCoordinate edgeOfRichRadiator_Global;
  (*coordinateTransformation)(edgeOfRichRadiator_Local,edgeOfRichRadiator_Global);

  StGlobalCoordinate edgeOfRichPadPlane_Global;
  (*coordinateTransformation)(edgeOfRichPadPlane_Local,edgeOfRichPadPlane_Global);
  
  StPhysicalHelixD  mHelix = tpcTrack->geometry()->helix();
  StThreeVectorD rr(edgeOfRichRadiator_Global.position().x(),
		    edgeOfRichRadiator_Global.position().y(),
		    edgeOfRichRadiator_Global.position().z());

  StThreeVectorD rp(edgeOfRichPadPlane_Global.position().x(),
		    edgeOfRichPadPlane_Global.position().y(),
		    edgeOfRichPadPlane_Global.position().z());
  }
  double mPathLengthAtRadiator  = mHelix.pathLength(rr,richNormal);
  double mPathLengthAtPadPlane  = mHelix.pathLength(rp,richNormal);

  StThreeVectorD tpcMom(0,0,0);
  if (mPathLengthAtRadiator<10e10) tpcMom = mHelix.momentumAt(mPathLengthAtRadiator,magField*kilogauss);
  StThreeVector<double> tpcMomentum(tpcMom.x(), tpcMom.y(), tpcMom.z());
 
  for (size_t l=0;l<mKaonList.size();l++) {
  // do the momentum vector rotation here
  StRichMomentumTransform* momentumTransformation 
    = StRichMomentumTransform::getTransform(myGeometryDb);

  StThreeVector<double> richLocalMomentum(0,0,0);
  momentumTransformation->localMomentum(tpcMomentum,richLocalMomentum);
  
  // impact point on radiator
  StGlobalCoordinate globalImpactPoint(mHelix.x(mPathLengthAtRadiator),
				       mHelix.y(mPathLengthAtRadiator),
				       mHelix.z(mPathLengthAtRadiator));

  StRichLocalCoordinate richTransformedImpactPoint(0,0,0);
  (*coordinateTransformation)(globalImpactPoint,richTransformedImpactPoint);

   
  // pad plane intersection (MIP)
  StGlobalCoordinate globalImpactPointAtPadPlane(-10000.0,-10000.0,-10000.0);
  if (mPathLengthAtPadPlane < 10e10) {
    globalImpactPointAtPadPlane.position().setX(mHelix.x(mPathLengthAtPadPlane));
    globalImpactPointAtPadPlane.position().setY(mHelix.y(mPathLengthAtPadPlane));
    globalImpactPointAtPadPlane.position().setZ(mHelix.z(mPathLengthAtPadPlane));
    mKaonList.clear();
  
  
  StRichLocalCoordinate localImpactPointAtPadPlane(0,0,0);
  (*coordinateTransformation)(globalImpactPointAtPadPlane,localImpactPointAtPadPlane);
    
  
  setCharge(mHelix.h());
  setMomentum(richLocalMomentum);
  setImpactPoint(richTransformedImpactPoint.position());
  setMIP(localImpactPointAtPadPlane.position());
  setPathLength(mPathLengthAtRadiator);
    mProtonList.clear();
    mProtonList.resize(0); 
}
StRichTrack::~StRichTrack() {}

void StRichTrack::setPathLength(double p) {
  mPath = p;
}

double StRichTrack::getPathLength() {
  return mPath;
}

// sets
void StRichTrack::setCharge(int ch) {
  mCharge = ch;
}

void StRichTrack::setMIP(StThreeVector<double>& mip) {
  mMIP = mip;
}

void StRichTrack::setMomentum(StThreeVector<double>& momentum) {
 
  mMomentum = momentum;

  StThreeVector<double> normalVector(0,0,-1);
  if (mMomentum.mag()) {
    setTheta(acos(normalVector.dot(momentum)/momentum.mag()));
  }
  
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
StThreeVector<double> StRichTrack::getMIP() {
  return mMIP;
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

    StRichMaterialsDb* myMaterialsDb = StRichMaterialsDb::getDb();
double StRichTrack::getPhi()              { return mPhi;}
double StRichTrack::getZVertex(){
    
    
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
