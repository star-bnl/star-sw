/**********************************************************
 * $Id: StRichTrack.cxx,v 1.1 2000/04/03 19:36:09 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrack.cxx,v $
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 *  initial revision
 *
 *  
 *
 *  Revision 2.4  2000/10/19 01:13:23  horsley
 *  added member functions to StRichPIDMaker to make cuts on hits, tracks, events.
 *  Revision 1.2  2000/05/19 19:06:11  horsley
 *  many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 **********************************************************/
// used in track coordinate transformations
#include "StRrsMaker/StRichMomentumTransform.h"
#include "StRrsMaker/StGlobalCoordinate.h"
#include "StRrsMaker/StRichRawCoordinate.h"
#include "StRrsMaker/StRichLocalCoordinate.h"
using std::sort;

StRichTrack::StRichTrack(StTrack* tpcTrack, double magField)  {
  
  mTrack = tpcTrack;

  // define system parameters
  StRichGeometryDb* myGeometryDb = StRichGeometryDb::getDb();
  
  StRichCoordinateTransform* coordinateTransformation =
        StRichCoordinateTransform::getTransform(myGeometryDb);
    double xCorrection = 0.;
  StThreeVectorD richNormal(myGeometryDb->normalVectorToPadPlane().x(),
			    myGeometryDb->normalVectorToPadPlane().y(),
			    myGeometryDb->normalVectorToPadPlane().z());
  
  StRichLocalCoordinate entryPointOfRichRadiator_Local(0.0,
						       0.0,
						       myGeometryDb->proximityGap() +
						       myGeometryDb->quartzDimension().z() +
						       myGeometryDb->radiatorDimension().z() );
						       
						       
  StGlobalCoordinate entryPointOfRichRadiator_Global;
  (*coordinateTransformation)(entryPointOfRichRadiator_Local,entryPointOfRichRadiator_Global);

  StThreeVectorD entryPointOfRichRadiator(entryPointOfRichRadiator_Global.position().x(),
					  entryPointOfRichRadiator_Global.position().y(),
					  entryPointOfRichRadiator_Global.position().z());
  
 
  StPhysicalHelixD  mHelix = tpcTrack->geometry()->helix();
  double mPathLength = mHelix.pathLength(entryPointOfRichRadiator,richNormal);

  StThreeVectorD tpcMom = mHelix.momentumAt(mPathLength,magField*tesla);
  StThreeVector<double> tpcMomentum(tpcMom.x(), tpcMom.y(), tpcMom.z());

  // do the momentum vector rotation here
  StRichMomentumTransform* momentumTransformation 
    = StRichMomentumTransform::getTransform();

  StThreeVector<double> richLocalMomentum;
  momentumTransformation->localMomentum(tpcMomentum,richLocalMomentum);
  
  // do the impact point transformation here
  StGlobalCoordinate globalImpactPoint(mHelix.x(mPathLength),
				       mHelix.y(mPathLength),
				       mHelix.z(mPathLength));

  StRichLocalCoordinate richTransformedImpactPoint;
  (*coordinateTransformation)(globalImpactPoint,richTransformedImpactPoint);

  StThreeVector<double> richLocalImpactPoint(richTransformedImpactPoint.position().x(),
					       richTransformedImpactPoint.position().y(),
					       richTransformedImpactPoint.position().z());
  
  setMomentum(richLocalMomentum);
  setImpactPoint(richLocalImpactPoint);
    mProtonList.clear();
    mProtonList.resize(0); 
}
StRichTrack::StRichTrack(StThreeVector<double>& tpcMom, 
			         StThreeVector<double>& impact)  {

  // here we initialize the track pointer to zero because 
  // we are creating the track from nothing! 
  mTrack = 0;

  StRichGeometryDb* myGeometryDb = StRichGeometryDb::getDb();
  StRichCoordinateTransform* coordinateTransformation =
    StRichCoordinateTransform::getTransform(myGeometryDb);
  
 // do the momentum vector rotation here
  StRichMomentumTransform* momentumTransformation 
    = StRichMomentumTransform::getTransform(myGeometryDb);

  StThreeVector<double> richLocalMomentum;
  momentumTransformation->localMomentum(tpcMom,richLocalMomentum);

  // do the impact point transformation here
  StGlobalCoordinate globalImpactPoint(impact.x(),impact.y(),impact.z());
  StRichLocalCoordinate transformedImpactPoint;
  (*coordinateTransformation)(globalImpactPoint,transformedImpactPoint);
  StThreeVector<double> richLocalImpactPoint(transformedImpactPoint.position().x(),
					       transformedImpactPoint.position().y(),
					       transformedImpactPoint.position().z());
  
  StRichLocalCoordinate testPoint(0,0,0);
  StGlobalCoordinate    resultOfTest;
  (*coordinateTransformation)(testPoint,resultOfTest);

  setMomentum(richLocalMomentum);
  setImpactPoint(richLocalImpactPoint);

}



StRichTrack::~StRichTrack() {}

StTrack* 
StRichTrack::getTrackPointer() {
  return mTrack;
}

void StRichTrack::setMomentum(StThreeVector<double>& momentum) {

  mMomentum = momentum;
  StThreeVector<double> normalVector(0,0,-1);
  if (mMomentum.mag()) {setTheta(acos(normalVector.dot(momentum)/momentum.mag()));}
  else setTheta(0.0);
  
  if (momentum.y() == 0 && momentum.x() == 0) {setPhi(0.0);}
  else setPhi(momentum.phi());
  
    float path = tempHelix.pathLength(mRadiatorGlobal,mRichNormal);
  
void StRichTrack::setTheta(double the) {
  mTheta = the;}
    if (path>0 && path<10000) {
void StRichTrack::setPhi(double phi) {
  mPhi = phi;}

void StRichTrack::setImpactPoint(StThreeVector<double>& impact) {
  mImpactPoint = impact;
}

StThreeVector<double> StRichTrack::getImpactPoint() {
  return mImpactPoint;}

StThreeVector<double> StRichTrack::getMomentum() {
  return mMomentum;}

double StRichTrack::getTheta() {
  return mTheta;}

double StRichTrack::getPhi() {
  return mPhi;}


double StRichTrack::getUnCorrectedPhi()   { return mUnCorrectedPhi;}
double StRichTrack::getLastHitDCA()       { return mLastHitDCA;}
double StRichTrack::getPathLength()       { return mPath;}

    StRichMaterialsDb* myMaterialsDb = StRichMaterialsDb::getDb();
    double indexOfRefraction = myMaterialsDb->indexOfRefractionOfC6F14At(219.0*nanometer);
    
    if ( p/sqrt(p*p + m*m) > (1./indexOfRefraction)) { 
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
