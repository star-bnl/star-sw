/**********************************************************
 * $Id: StRichTrack.cxx,v 2.1 2000/09/29 01:35:38 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichTrack.cxx,v $
 *  Revision 2.1  2000/09/29 01:35:38  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  return copy instead of reference
 *
 *  Revision 2.6  2000/11/07 14:13:24  lasiuk
 *  add possibility of .4*px/pz correction to the track extrapolation
 *
 *  Revision 2.5  2000/11/01 17:43:10  lasiuk
 *  default arguments initialization in c'tor.  Addition of init() member
 *  function to handle generic DB initialization and removal of virtual keyword
 *
 *  Revision 2.4  2000/10/19 01:13:23  horsley
 *  added member functions to StRichPIDMaker to make cuts on hits, tracks, events.
 *  added normal distance sigma cut on hits, quartz and radiator pathlengths
 *  for individual photons, modified minimization routine to correct boundary
 *  problems
 *
 *  Revision 2.3  2000/10/03 19:26:01  horsley
 *  fixed error in StRichTrack correct member function, now returns bool.
 *
 *  Revision 2.2  2000/09/29 17:55:51  horsley
 *  fixed bug in Minimization routine, included StMagF stuff (commented out)
 *  changed StRichRingPoint  HUGE_VALUE   ---> MAXFLOAT for default value
 *
 *  Revision 2.1  2000/09/29 01:35:38  horsley
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
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 *  initial revision
 **********************************************************/

#include <iostream.h>
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
#include "StRrsMaker/StRichCoordinateTransform.h"
using std::pair;
using std::adjacent_difference;
using std::max_element;
using std::sort;
#endif
#endif

#include "StParticleTypes.hh"
#include "StParticleDefinition.hh"
StRichTrack::StRichTrack()  {}

StRichTrack::StRichTrack(StThreeVectorF mom, StThreeVectorF imp)  {
 
  mStTrack  = 0;
  mPidTrait = 0;

#include "StRrsMaker/StRichGeometryDb.h"

  mAssociatedMIP = 0;
  
using namespace units;
// For sorting the possible MIP candiates
  myGeometryDb  = StRichGeometryDb::getDb();  
  myMaterialsDb = StRichMaterialsDb::getDb();  
  coordinateTransformation = StRichCoordinateTransform::getTransform(myGeometryDb);
  momentumTransformation   = StRichMomentumTransform::getTransform(myGeometryDb);  
  setUnCorrectedMomentum(mom);
  
  StThreeVectorF richNormal(myGeometryDb->normalVectorToPadPlane().x(),
			    myGeometryDb->normalVectorToPadPlane().y(),
			    myGeometryDb->normalVectorToPadPlane().z());
  
  setMomentum(mom);
  setImpactPoint(imp);
bool operator<(const candidate &a, const candidate &b) {return (a.second<b.second);};


StRichTrack::StRichTrack(globalTrack *track, double magField)  {
StRichTrack::StRichTrack(StThreeVectorF mom, StThreeVectorF imp)
    mPidTrait      = 0;
    mMagneticField = magField;
    mStTrack       = 0;
    mL3Track       = track; 
    mAssociatedMIP = 0;
{
    //
    myGeometryDb = StRichGeometryDb::getDb();  
    myMaterialsDb = StRichMaterialsDb::getDb();  
    coordinateTransformation = StRichCoordinateTransform::getTransform(myGeometryDb);
    momentumTransformation   = StRichMomentumTransform::getTransform(myGeometryDb);
    
    StThreeVectorF richNormal(myGeometryDb->normalVectorToPadPlane().x(),
			      myGeometryDb->normalVectorToPadPlane().y(),
			      myGeometryDb->normalVectorToPadPlane().z());
  mL3Track  = 0; 
#endif  

  this->init();
  
  // define system parameters
  this->setMomentum(mom);
  this->setImpactPoint(imp);
}

#ifdef RICH_WITH_L3_TRACKS
StRichTrack::StRichTrack(globalTrack *track, double magField)
    : mStTrack(0),              mPidTrait(0),    mAssociatedMIP(0),
      mMagneticField(magField), mL3Track(track),
{
    StRichLocalCoordinate richAnodeWirePlane_Local(0.0, 0.0, 0.0 + anodeDistanceToPadPlane);	
    // define system parameters
    (*coordinateTransformation)(edgeOfRichRadiator_Local,edgeOfRichRadiator_Global);
    // need to include mwpc gap correction, 
    // origin is located in center of gap, not on padplane --> 2 mm
    (*coordinateTransformation)(richAnodeWirePlane_Local,richAnodeWirePlane_Global);
						   0.0,
						   myGeometryDb->proximityGap() +
						   myGeometryDb->quartzDimension().z() +
						   myGeometryDb->radiatorDimension().z());
    
    // MIP is measured at anode position
    StRichLocalCoordinate richAnodeWirePlane_Local(0.,
    
						   0.,
						   0.+anodeDistanceToPadPlane);	
    StGlobalCoordinate edgeOfRichRadiator_Global;
    (*coordinateTransformation)(edgeOfRichRadiator_Local,
				edgeOfRichRadiator_Global);
    
    StGlobalCoordinate richAnodeWirePlane_Global;
    (*coordinateTransformation)(richAnodeWirePlane_Local,
				richAnodeWirePlane_Global);
    

    // L3 Stuff
    double x0=track->r0*cos(track->phi0);
    double y0=track->r0*sin(track->phi0);
    double z0=track->z0;

    //momentum
    double px=track->pt * cos(track->psi);
    double py=track->pt * sin(track->psi);
    double pz=track->pt * track->tanl;

    StThreeVectorD tpcMom(0,0,0);
			    StThreeVectorD(x0,y0,z0),
			    magField*kilogauss,
			    track->q);    
    
    StThreeVector<double> tpcMomentum(tpcMom.x(), tpcMom.y(), tpcMom.z());    
    
    StThreeVectorD rp(richAnodeWirePlane_Global.position().x(),
    StThreeVector<double> richLocalMomentum(0,0,0);
		      richAnodeWirePlane_Global.position().z());
    
    double mPathLengthAtRadiator  = mHelix.pathLength(rr,richNormal);
    double mPathLengthAtPadPlane  = mHelix.pathLength(rp,richNormal);
    
    StThreeVectorD tpcMom;
    if (mPathLengthAtRadiator<10e10 && mPathLengthAtRadiator>0) {
	tpcMom = mHelix.momentumAt(mPathLengthAtRadiator,magField*kilogauss);	
    }
    
    StThreeVectorD ppMomentum(0,0,0);
    StRichLocalCoordinate richTransformedImpactPoint(-999,-999,-999);
    (*coordinateTransformation)(globalImpactPoint,richTransformedImpactPoint);
//     double xCorrection = 0.4*(mMomentumAtPadPlane.x()/mMomentumAtPadPlane.z());
    double xCorrection = 0.;
    // correction to the x position of the impact point
    // If this correction is really due to the effect of
    // shifting the x position (z Global) of the track,
    // this may not be appropriate in the L3 unless we
    // use the offline hits as well
    double xCorrection = 0.4*(mMomentumAtPadPlane.x()/mMomentumAtPadPlane.z());
    //double xCorrection = 0.;
    // pad plane intersection (projected MIP)
    StGlobalCoordinate    globalImpactPointAtAnodeWirePlane(-999,-999,-999);
    StRichLocalCoordinate localImpactPointAtAnodeWirePlane(-999,-999,-999);  

    if (mPathLengthAtPadPlane < 10e10  && mPathLengthAtRadiator>0) {

    setMomentum(richLocalMomentum);
    setUnCorrectedMomentum(richLocalMomentum);
    setImpactPoint(richTransformedImpactPoint.position());
    setProjectedMIP(localImpactPointAtAnodeWirePlane.position());



StRichTrack::StRichTrack(StTrack* tpcTrack, double magField)  {
 
    mStTrack       = tpcTrack;
    mPidTrait      = 0;
    mMagneticField = magField;
    mAssociatedMIP = 0;
	    .setX(localImpactPointAtAnodeWirePlane.position().x()-xCorrection);
    : mStTrack(tpcTrack),  mPidTrait(0),  mAssociatedMIP(0),mMagneticField(magField)
    myGeometryDb  = StRichGeometryDb::getDb();  
    myMaterialsDb = StRichMaterialsDb::getDb();  
    coordinateTransformation = StRichCoordinateTransform::getTransform(myGeometryDb);
    momentumTransformation   = StRichMomentumTransform::getTransform(myGeometryDb);
    
    mRichNormal.setX(myGeometryDb->normalVectorToPadPlane().x());
    mRichNormal.setY(myGeometryDb->normalVectorToPadPlane().y());
    mRichNormal.setZ(myGeometryDb->normalVectorToPadPlane().z());
    
    // define system parameters

    StThreeVector<double> padPlaneMomentum(ppMomentum.x(),ppMomentum.y(),ppMomentum.z());

        

    this->setMomentum(richLocalMomentum);
    this->setUnCorrectedMomentum(richLocalMomentum);
    StRichLocalCoordinate edgeOfRichRadiator_Local(0.0,
						   0.0,
						   myGeometryDb->proximityGap() +
						   myGeometryDb->quartzDimension().z() +
						   myGeometryDb->radiatorDimension().z());
    
    : mStTrack(tpcTrack),       mPidTrait(0),  mAssociatedMIP(0),
    StRichLocalCoordinate richAnodeWirePlane_Local(0.0, 0.0, myGeometryDb->anodeToPadSpacing());

    // need to include mwpc gap correction, 
    // make correction for gap distance of 4 mm
    //
    StRichLocalCoordinate
	edgeOfRichRadiator_Local(0., 0., myGeometryDb->proximityGap() +
				 myGeometryDb->quartzDimension().z() +
				 myGeometryDb->radiatorDimension().z());
    (*coordinateTransformation)(richAnodeWirePlane_Local,richAnodeWirePlane_Global);
    // MIP is measured at anode position
    //
    StRichLocalCoordinate
	richAnodeWirePlane_Local(0., 0., myGeometryDb->anodeToPadSpacing());

    //
    // CTB in local coord.
    //
    StRichLocalCoordinate CTB_Local(0,0,240.0-214.0);

    StGlobalCoordinate edgeOfRichRadiator_Global;
    (*coordinateTransformation)(edgeOfRichRadiator_Local,edgeOfRichRadiator_Global);
    
    StGlobalCoordinate richAnodeWirePlane_Global;
    (*coordinateTransformation)(richAnodeWirePlane_Local,
				richAnodeWirePlane_Global);
    
    StGlobalCoordinate CTB_Global;
    (*coordinateTransformation)(CTB_Local,CTB_Global);


    
    StPhysicalHelixD  mHelix = tpcTrack->geometry()->helix();    

    mRadiatorGlobal.setX(edgeOfRichRadiator_Global.position().x());
    StThreeVectorF tpcMom(0,0,0);
    mRadiatorGlobal.setZ(edgeOfRichRadiator_Global.position().z());
		     
 
    mAnodeGlobal.setX(richAnodeWirePlane_Global.position().x());
    mAnodeGlobal.setY(richAnodeWirePlane_Global.position().y());
    mAnodeGlobal.setZ(richAnodeWirePlane_Global.position().z());
		
    mCTBGlobal.setX(CTB_Global.position().x());
    mCTBGlobal.setY(CTB_Global.position().y());
    mCTBGlobal.setZ(CTB_Global.position().z());
    StThreeVectorF richLocalMomentum(tempRichLocalMomentum.x(),tempRichLocalMomentum.y(),tempRichLocalMomentum.z());
    float mPathLengthAtCTB       = mHelix.pathLength(mCTBGlobal,mRichNormal);
    
    StThreeVectorF tpcMom;
    if (mPathLengthAtRadiator<10e10  && mPathLengthAtRadiator>0) {
      tpcMom = mHelix.momentumAt(mPathLengthAtRadiator,mMagneticField*kilogauss); 
    }
    
    StThreeVector<double> tpcMomentum(tpcMom.x(), tpcMom.y(), tpcMom.z());
    
    // do the momentum vector rotation here
    StThreeVector<double> tempRichLocalMomentum(0,0,0);
    momentumTransformation->localMomentum(tpcMomentum,tempRichLocalMomentum);
    
    StThreeVectorF richLocalMomentum(tempRichLocalMomentum.x(),
				     tempRichLocalMomentum.y(),
				     tempRichLocalMomentum.z());

    // impact point in CTB
    StThreeVectorF localCTBImpactPoint(-999,-999,-999);
    if (mPathLengthAtCTB>0 && mPathLengthAtCTB<10e10) {
    
    
				   mHelix.y(mPathLengthAtCTB),
				   mHelix.z(mPathLengthAtCTB));
    
    
    StThreeVectorF ppMomentum(0,0,0);
    
      localCTBImpactPoint.setX(richTransformedImpactPoint.position().x());
      localCTBImpactPoint.setZ(richTransformedImpactPoint.position().z());
    }

    // impact point on radiator
    StGlobalCoordinate globalImpactPoint(mHelix.x(mPathLengthAtRadiator),
					 mHelix.y(mPathLengthAtRadiator),
					 mHelix.z(mPathLengthAtRadiator));

      (*coordinateTransformation)(globalImpactPointAtAnodeWirePlane,localImpactPointAtAnodeWirePlane);
        
    StThreeVectorF ppMomentum;

    //
  StThreeVector<double> padPlaneMomentum(ppMomentum.x(),ppMomentum.y(),ppMomentum.z());
  StThreeVector<double> tempMomentum(0,0,0);
  momentumTransformation->localMomentum(padPlaneMomentum,tempMomentum);
  mMomentumAtPadPlane.setX(tempMomentum.x());
  mMomentumAtPadPlane.setY(tempMomentum.y());
  mMomentumAtPadPlane.setZ(tempMomentum.z());
  float lastHitDCA = mHelix.distance(tpcTrack->detectorInfo()->lastPoint());

	//
	// Topology Map (functionality should probably be put in Topology Map)
	//
	const StTrackTopologyMap& tMap = mStTrack->topologyMap();
	vector<int> rows;
	vector<int> emptyRows(1,0);
	for (int i=1; i<46; ++i) {
	  if (tMap.hasHitInRow(kTpcId,i)) { rows.push_back(i);} 
	  else { emptyRows.push_back(i);} 
	}
	emptyRows.push_back(46);

	setMaxGap(maxSeq(rows)); // max number of continuous empty rows
	setMaxChain(maxSeq(emptyRows)); // max number of continuous used rows
	setFirstRow(rows.front());
	setLastRow(rows.back());


  setMomentum(richLocalMomentum);
  setUnCorrectedMomentum(richLocalMomentum);

  StThreeVectorF tempHit(richTransformedImpactPoint.position().x(),
			 richTransformedImpactPoint.position().y(),
			 richTransformedImpactPoint.position().z());

  setImpactPoint(tempHit);

  StThreeVectorF tempMip(localImpactPointAtAnodeWirePlane.position().x(),
			 localImpactPointAtAnodeWirePlane.position().y(),
			 localImpactPointAtAnodeWirePlane.position().z());
  setProjectedMIP(tempMip);
  setProjectedCTB(localCTBImpactPoint);
  setPathLength(mPathLengthAtRadiator);
    this->setProjectedCTB(localCTBImpactPoint);
vector<StRichRingHit*> StRichTrack::getRingHits( StParticleDefinition* part) {
  if ( (part ==  StPionMinus::instance()) || (part == StPionPlus::instance())) {
    return mPionList; 
  }
}
  else if ( (part ==  StKaonMinus::instance()) || (part == StKaonPlus::instance())) {
    return mKaonList; 
  }
  
  else if ( (part ==  StProton::instance()) || (part == StAntiProton::instance())) {
    return mProtonList; 
  }

  vector<StRichRingHit*> tempvec(0);
  tempvec[0]=0;
  return tempvec;
}

vector<StRichRingHit*>
StRichTrack::getRingHits( StParticleDefinition* part) {
  for (size_t l=0;l<mPionList.size();l++) {
    delete mPionList[l];
    mPionList[l] = 0;
  }
  mPionList.clear();
  mPionList.resize(0);
    }
  for (size_t l=0;l<mKaonList.size();l++) {
    delete mKaonList[l];
    mKaonList[l] =0;
  }
  mKaonList.clear();
  mKaonList.resize(0);
    tempvec[0]=0;
  for (size_t l=0;l<mProtonList.size();l++) {
    delete mProtonList[l];
    mProtonList[l] = 0;
void  StRichTrack::addHit(StRichHit* hit, double dist, double angle, double radPath, 
  mProtonList.resize(0); 
    for (size_t l=0;l<mPionList.size();l++) {
	delete mPionList[l];
    mPionList.push_back(new StRichRingHit(hit,angle,dist,radPath,quaPath)); 
    }
    mPionList.clear();
    mPionList.resize(0);
    mKaonList.push_back(new StRichRingHit(hit,angle,dist,radPath,quaPath)); 
    for (size_t l=0;l<mKaonList.size();l++) {
	delete mKaonList[l];
	mKaonList[l] =0;
    mProtonList.push_back(new StRichRingHit(hit,angle,dist,radPath,quaPath));  
    mKaonList.clear();
    mKaonList.resize(0);
  
    for (size_t l=0;l<mProtonList.size();l++) {
	delete mProtonList[l];
	mProtonList[l] = 0;
    }
    mProtonList.clear();
    mProtonList.resize(0); 
}


void  StRichTrack::addHit(StRichHit* hit, double dist, double sigma, 
			  double angle, double radPath, 
			  double quaPath, StParticleDefinition* part) {
  
  if ( (part ==  StPionMinus::instance()) || (part == StPionPlus::instance())) {
    mPionList.push_back(new StRichRingHit(hit,angle,dist,sigma,radPath,quaPath)); 
  }
  
  else if ( (part ==  StKaonMinus::instance()) || (part == StKaonPlus::instance())) {
    mKaonList.push_back(new StRichRingHit(hit,angle,dist,sigma,radPath,quaPath)); 
  }
  
  else if ( (part ==  StProton::instance()) || (part == StAntiProton::instance())) {
    mProtonList.push_back(new StRichRingHit(hit,angle,dist,sigma,radPath,quaPath));  
  setUnCorrectedTheta(getTheta());
  }
  setUnCorrectedPhi(getPhi());
}
StRichTrack::~StRichTrack() {
  clearHits();
  
}




bool StRichTrack::correct() {

  if (!mStTrack) return false;
  
  StThreeVectorF tempMip(-999,-999,-999);
  if (mAssociatedMIP) { tempMip = mAssociatedMIP->local();}
  else return false;

  StThreeVectorF residual = tempMip-mProjectedMIP;
  if (residual.perp()>3.0*centimeter) { return false;}
  
  

  // store original values
  setUnCorrectedProjectedMIP(getProjectedMIP());
  setUnCorrectedImpactPoint(getImpactPoint());
  setUnCorrectedMomentum(getMomentum());


  // get momentum, x at assumed interaction point in CTB
  StPhysicalHelixD mHelix     = mStTrack->geometry()->helix();
  float mPathLengthToCTB      = mHelix.pathLength(mCTBGlobal,mRichNormal);
  if (mPathLengthToCTB>10000  || mPathLengthToCTB<0) return false;
  
  StThreeVectorF CTBPoint(mHelix.x(mPathLengthToCTB),mHelix.y(mPathLengthToCTB),mHelix.z(mPathLengthToCTB));
  StThreeVectorF globalMomentum = mHelix.momentumAt(mPathLengthToCTB, mMagneticField*kilogauss);

  float initialTheta = globalMomentum.theta();
  float initialPhi   = globalMomentum.phi();

  float thetaRange = 5.0*degree;
  float phiRange = 10.0*degree;
  
  float thetaTrials=100.0;
  float phiTrials=100.0;
  
  float thetaStart=initialTheta-thetaRange;
  float thetaFinish=initialTheta+thetaRange;

  float phiStart=initialPhi-phiRange;
  float phiFinish=initialPhi+phiRange;

  float deltaTheta=thetaRange/thetaTrials;
  float deltaPhi=phiRange/phiTrials;

  StThreeVectorF correctedMomentum(0,0,0);
  StThreeVectorF correctedProjectedMIP(0,0,0);
  StThreeVectorF correctedResidual = residual;
  StThreeVectorF tempMomentum(0,0,0);
  
  for (float theta=thetaStart;theta<thetaFinish;theta=theta+deltaTheta) {
    for (float phi=phiStart;phi<phiFinish;phi=phi+deltaPhi) {
      float p = globalMomentum.mag();
      tempMomentum.setX(p*sin(theta)*cos(phi));
      tempMomentum.setY(p*sin(theta)*sin(phi));
      tempMomentum.setZ(p*cos(theta));
    
      StPhysicalHelixD tempHelix(tempMomentum,CTBPoint,mMagneticField*kilogauss,mStTrack->geometry()->charge());
      float path = tempHelix.pathLength(mAnodeGlobal,mRichNormal);
      
      if (path>0 && path<10000) { 
	StGlobalCoordinate globalPadPlane(tempHelix.x(path),tempHelix.y(path),tempHelix.z(path));
	
	StRichLocalCoordinate localPadPlane;
	(*coordinateTransformation)(globalPadPlane,localPadPlane);
	StThreeVectorF tempLocal(localPadPlane.position().x(),
				 localPadPlane.position().y(),
				 localPadPlane.position().z());
	
	StThreeVectorF tempResidual = tempLocal-tempMip;
	
	if (tempResidual.perp()<correctedResidual.perp() && 
	    fabs(tempResidual.x()) < 1.1*fabs(tempResidual.y()) &&
	    fabs(tempResidual.x()) > 0.9*fabs(tempResidual.y())) {
	  correctedMomentum     = tempMomentum;
	  correctedResidual     = tempResidual;
	  correctedProjectedMIP = tempLocal;
	}
      }
    }
  }
      setTheta(acos(normalVector.dot(tempRichLocalMomentum)/tempRichLocalMomentum.mag()));
      setPhi(tempRichLocalMomentum.phi());

  } 
}



/*

float StRichTrack::func(vector<float> input) {
  
}


float StRichTrack::f1dim(float x) {
  int j;
  float f;
  
  vector<float> xt(ncom);
  for (j=0;j<ncom;j++) {xt[j]=pcom[j]+x*xicom[j];}
  return func(xt);
}


void StRichTrack::linmin(vector<float> p, vector<float> xi, int n, float *fret) {
  
  int j;
  float xx,xmin,fx,fb,fa,bx,ax;

  ncom=n;
  
  pcom.clear();
  pcom.resize(n);
  
  xicom.clear();
  xicom.resize(n);

  for (j=0;j<n;j++) {
    pcom[j]=p[j];
    xicom[j]=xi[j];
  if (correctedMomentum.mag()>0) {

  ax =  100.0;  // initial guess for bracketing function
  xx = -100.0;
    // impact point on radiator
  //  mnbrak(&ax,&xx,&bx,&fa,&fx,&fb);
  *fret = brent(ax,xx,bx,&xmin);
  for (j=0;j<n;j++) {
    xi[j] *= xmin;
    p[j] += xi[j]; 
  }
}


void StRichTrack::powell(vector<float>& p , float **xi, int *iter, float *fret) {
  int i,ibig,j;
  float del,fp,fptt,t;
  vector<float> pt,ptt,xit;
  
  pt.clear();
  pt.resize(n);

  ptt.clear();
  ptt.resize(n);
  
  xit.clear();
  xit.resize(n);
  
  
   *fret = func(p);
   for (j=0;j<n;j++) {pt[j]=p[j];}
   for (*iter=0;;++(*iter)) {
     fp=(*fret);
     ibig=0;
     del=0.0;
     
     
     for (i=0;i<n;i++) {
       for(j=0;j<n;j++) { xit[j]=xi[j][i];}
       fptt=(*fret);
       linmin(p,xit,n,fret);
       if (fabs(fptt-(*fret)) > del) {
	 del=fabs(fptt-(*fret));
	 ibig=i;
       }
     }
     
     // are we done yet?
     if (2.0*fabs(fp-(*fret)) <= ftol*(fabs(fp)+fabs(*fret))) {
       return;
     }
     
     if (*iter == ITMAX) {
       cout << "exceeded maximum number of iterations!" << endl;
       return;
     }
     
     for(j=0;j<n;j++) {
       ptt[j] = 2.0*p[j] - pt[j];
       xit[j] = p[j] - pt[j];
       pt[j]  = p[j]; 
     }

     fptt = func(ptt);
     if (fptt < fp) {
       t = 2.0*(fp-2.0*(*fret)+fptt)*sqrt(fp-(*fret)-del*sqrt(fp-fptt));
       if (t<0) {
	 linmin(p,xit,n,fret);
	 for(j=0;j<n;j++) {
	   xi[j][ibig] = xi[j][n];
	   xi[j][n]    = xit[j];
	 }
       }
     }
   }
}


 
#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))
#define ITMAX 200
#define CGOLD 0.3819660
#define ZEPS  1.0e-10
#define SHFT(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);


float StRichTrack::brent(double ax, double bx, double cx,double *xmin)  {
    int iter;
    double tol = 2.0e-4;
    double a,b,d,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm;
    double e=0.0;
    d=0.0;
   
    a=(ax < cx ? ax : cx);
    b=(ax > cx ? ax : cx);
    x=w=v=bx;
    fw=fv=fx=f1dim(x);
	
	for (iter=1;iter<=ITMAX;iter++) {
	        xm=0.5*(a+b);
		tol2=2.0*(tol1=tol*fabs(x)+ZEPS);
		if (fabs(x-xm) <= (tol2-0.5*(b-a))) {
		        *xmin=x;
			return fx;
		}
		if (fabs(e) > tol1) {
			r=(x-w)*(fx-fv);
			q=(x-v)*(fx-fw);
			p=(x-v)*q-(x-w)*r;
			q=2.0*(q-r);
			if (q > 0.0) p = -p;
			q=fabs(q);
			etemp=e;
			e=d;
			if (fabs(p) >= fabs(0.5*q*etemp) || p <= q*(a-x) || p >= q*(b-x))
				d=CGOLD*(e=(x >= xm ? a-x : b-x));
			else {
				d=p/q;
				u=x+d;
				if (u-a < tol2 || b-u < tol2)
					d=SIGN(tol1,xm-x);
			}
		} else {
			d=CGOLD*(e=(x >= xm ? a-x : b-x));
		}
		u=(fabs(d) >= tol1 ? x+d : x+SIGN(tol1,d));
		fu=f1dim(u);
	     
		if (fu <= fx) {
			if (u >= x) a=x; else b=x;
			SHFT(v,w,x,u)
			SHFT(fv,fw,fx,fu)
		} else {
			if (u < x) a=u; else b=u;
			if (fu <= fw || w == x) {
				v=w;
				w=u;
				fv=fw;
				fw=fu;
			} else if (fu <= fv || v == x || v == w) {
				v=u;
				fv=fu;
			}
		}
	}
	
	*xmin=x;
	return fx;
    float path = tempHelix.pathLength(mRadiatorGlobal,mRichNormal);
  
    if (path>0 && path<10000) {
      StGlobalCoordinate globalImpactPoint(tempHelix.x(path),tempHelix.y(path),tempHelix.z(path));
////////////////////////////////////////////

#undef ITMAX
#undef CGOLD
#undef ZEPS
#undef SHFT
// (C) Copr. 1986-92 Numerical Recipes Software &1245.@1. 


*/



bool StRichTrack::trajectoryCorrection() {
  // bad mip flag!
  StThreeVectorF tempMip(-999,-999,-999);
  if (mAssociatedMIP) { tempMip = mAssociatedMIP->local();}

  if ((tempMip- mProjectedMIP).perp()>3.0*centimeter) { return false;}

  setUnCorrectedTheta(getTheta());
  setUnCorrectedPhi(getPhi());
  setUnCorrectedImpactPoint(getImpactPoint());

  // not really? but should work!
  StThreeVectorD CTBNormal(myGeometryDb->normalVectorToPadPlane().x(),
			   myGeometryDb->normalVectorToPadPlane().y(),
			   myGeometryDb->normalVectorToPadPlane().z());
    
  // right now just a guess!!!
  StRichLocalCoordinate CTBPoint_Local(0.0,0.0,240.0-214.0);
  
  StGlobalCoordinate CTBPoint_Global;
  (*coordinateTransformation)(CTBPoint_Local,CTBPoint_Global);
  StThreeVectorF CTBPointTemp_Global(CTBPoint_Global.position().x(),
				     CTBPoint_Global.position().y(),
				     CTBPoint_Global.position().z());

  StPhysicalHelixD mHelix       = mStTrack->geometry()->helix();

  float mPathLengthToCTB       = mHelix.pathLength(CTBPointTemp_Global,CTBNormal);

  float padPlaneToRadiatorTop = myGeometryDb->proximityGap() 
                               + myGeometryDb->quartzDimension().z() 
                               + myGeometryDb->radiatorDimension().z();
  
  if (mPathLengthToCTB<10e10  && mPathLengthToCTB>0) {
    
    StGlobalCoordinate globalIntersectionPoint(mHelix.x(mPathLengthToCTB),
					       mHelix.y(mPathLengthToCTB),
					       mHelix.z(mPathLengthToCTB));
        
    StRichLocalCoordinate tempLocalIntersectionPoint(-999,-999,-999);
    (*coordinateTransformation)(globalIntersectionPoint,tempLocalIntersectionPoint);
  
    StThreeVectorF localIntersectionPoint(tempLocalIntersectionPoint.position().x(),
					  tempLocalIntersectionPoint.position().y(),
					  tempLocalIntersectionPoint.position().z());
    
    StThreeVectorF straightLine = tempMip-localIntersectionPoint;

    setImpactPoint(localIntersectionPoint + 
		   straightLine*((padPlaneToRadiatorTop-localIntersectionPoint.z())/straightLine.z()));
    
   StThreeVectorF normalVector(0,0,-1);
   if ( straightLine.mag()>0) {
     setTheta(acos(normalVector.dot(straightLine)/straightLine.mag()));}
   
   if ( straightLine.y() == 0 && straightLine.x() == 0) {setPhi(0.0);}
   else setPhi(straightLine.phi());  
   return true;
  }  

  else {return false;}
  return false; 
}
      StRichLocalCoordinate richTransformedImpactPoint(-999,-999,-999);
      (*coordinateTransformation)(globalImpactPoint,richTransformedImpactPoint);
      
      StThreeVectorF impact(richTransformedImpactPoint.position().x(),
			    richTransformedImpactPoint.position().y(),
			    richTransformedImpactPoint.position().z());
      
      // do the momentum vector rotation here
      StThreeVector<double> tempRichLocalMom(0,0,0);
  // proximity matching between TPC track's predicted MIP and 
  // RICH pad plane MIP 
      StThreeVectorF normalVector(0,0,-1);
  if (hits) {
      


     
    for (StSPtrVecRichHitIterator hitIndex = hits->begin(); hitIndex != hits->end(); ++hitIndex) { 
      testThisResidual = ((*hitIndex)->local() - mProjectedMIP).perp();      
      if (testThisResidual<smallestResidual && (*hitIndex)->charge()>adcCut) {
	smallestResidual = testThisResidual;   
	mAssociatedMIP   = *hitIndex;
      }
// 	break;
  }
// 	}
//     }


	cout << highestAmplitude << endl;
	    //cout << "StRichTrack::assignMIP()\n";
	    //cout << "\tTake smallest Residual" << endl;
StRichPidTraits* StRichTrack::getPidTrait()      { return mPidTrait;}
	    highestAmplitude = candidateHits[0].first->charge();
	}
	    
    }
    //
    
    // In the best case the eMip and eAssociatedMip flag should be set
    //
    else {
	cout << "StRichTrack::associateMIP()\n";
	cout << "\tWARNING no assocatied MIP" << endl;
    }

}


StRichHit*       StRichTrack::getAssociatedMIP() { return mAssociatedMIP;}
StTrack*         StRichTrack::getStTrack()       { return mStTrack;}


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
	return track->z0-track->tanl*track->r0*cos(track->psi-track->phi0);
#endif
    return -999;
}


int StRichTrack::getMaxGap()   { return mMaxGap;}
int StRichTrack::getMaxChain() { return mMaxChain;}
int StRichTrack::getFirstRow() { return mFirstRow;}
int StRichTrack::getLastRow()  { return mLastRow;}
int StRichTrack::maxSeq(vector<int>& seqrows) {
    vector<int> diffs(seqrows.size());
StThreeVectorF& StRichTrack::getUnCorrectedMomentum()     { return mUnCorrectedMomentum;}
    vector<int>::iterator max = max_element(diffs.begin()+1, diffs.end());
StThreeVectorF& StRichTrack::getUnCorrectedImpactPoint()  { return mUnCorrectedImpactPoint;}
StThreeVectorF& StRichTrack::getLastHit()                 { return mLastHit;}
StThreeVectorF& StRichTrack::getProjectedCTBPoint()       { return mProjectedCTB;} 
StThreeVectorF& StRichTrack::getProjectedMIP()            { return mProjectedMIP;}
StThreeVectorF& StRichTrack::getImpactPoint()             { return mImpactPoint;}
StThreeVectorF& StRichTrack::getMomentum()                { return mMomentum;}
StThreeVectorF& StRichTrack::getMomentumAtPadPlane()      { return mMomentumAtPadPlane;}
    double indexOfRefraction1 = myMaterialsDb->indexOfRefractionOfC6F14At(219.999*nanometer);
        
void StRichTrack::setPathLength(double p)        { mPath = p;}
	return 1;
    }
    
    return 0;
}

StThreeVectorF& StRichTrack::getUnCorrectedMomentum() { return mUnCorrectedMomentum;}
StThreeVectorF& StRichTrack::getUnCorrectedProjectedMIP() { return mUnCorrectedProjectedMIP;}
StThreeVectorF& StRichTrack::getUnCorrectedImpactPoint() { return mUnCorrectedImpactPoint;}
void StRichTrack::setLastHit(StThreeVectorF& hit) { mLastHit = hit;}
StThreeVectorF& StRichTrack::getProjectedCTBPoint() { return mProjectedCTB;} 
StThreeVectorF& StRichTrack::getProjectedMIP() { return mProjectedMIP;}
StThreeVectorF& StRichTrack::getImpactPoint() { return mImpactPoint;}
StThreeVectorF& StRichTrack::getMomentum() { return mMomentum;}
StThreeVectorF& StRichTrack::getMomentumAtPadPlane() { return mMomentumAtPadPlane;}

StRichPidTraits* StRichTrack::getPidTrait() { return mPidTrait; }
void StRichTrack::addPidTrait(StRichPidTraits* trait) { mPidTrait = trait;}

void StRichTrack::setPathLength(double p) { mPath = p;}
void StRichTrack::setLastHitDCA(double lastdca)  {mLastHitDCA = lastdca;}
void StRichTrack::setMaxGap(int n)   { mMaxGap=n;}
void StRichTrack::setMaxChain(int n) { mMaxChain=n;}
void StRichTrack::setFirstRow(int n) { mFirstRow=n;}
void StRichTrack::setLastRow(int n)  { mLastRow=n;}
void StRichTrack::setProjectedCTB(StThreeVectorF& ctb) {mProjectedCTB = ctb;} 
void StRichTrack::setProjectedMIP(StThreeVectorF& mip) {mProjectedMIP = mip;}
void StRichTrack::setTheta(double the) {mTheta = the;}
void StRichTrack::setPhi(double phi) { mPhi = phi;}
void StRichTrack::setImpactPoint(StThreeVectorF& impact) { mImpactPoint = impact;}
void StRichTrack::setUnCorrectedTheta(double the) {mUnCorrectedTheta=the;}
void StRichTrack::setUnCorrectedPhi(double phi) {mUnCorrectedPhi=phi;}
void StRichTrack::setUnCorrectedImpactPoint(StThreeVectorF& point) {mUnCorrectedImpactPoint=point;}
void StRichTrack::setUnCorrectedProjectedMIP(StThreeVectorF& point) {mUnCorrectedProjectedMIP=point;}
void StRichTrack::setUnCorrectedMomentum(StThreeVectorF& point) {mUnCorrectedMomentum=point;}
void StRichTrack::setLastHit(StThreeVectorF hit) { mLastHit = hit;}

void StRichTrack::useUnCorrected() {
  setMomentum(mUnCorrectedMomentum);
  setImpactPoint(mUnCorrectedImpactPoint);
  setProjectedMIP(mUnCorrectedProjectedMIP);
}

void StRichTrack::setMomentum(StThreeVectorF& momentum) {
  mMomentum = momentum;
  StThreeVectorF normalVector(0,0,-1);
  if (mMomentum.mag()) {
    setTheta(acos(normalVector.dot(momentum)/momentum.mag()));}
  if (momentum.y() == 0 && momentum.x() == 0) {setPhi(0.0);}
  else setPhi(momentum.phi());
}




bool StRichTrack::isGood(StParticleDefinition* particle) {   
    StRichRingPoint OuterRing(this,eOuterRing); // Get Ring
    OuterRing.setParticleType(particle);
    StThreeVectorF otemp;
    
    // See If valid point at back of ring
    // cut:   changed from pi          ---->                 pi/2 
    //            very back                                 at least half ring is ok

    bool goodTrack = OuterRing.getPoint(M_PI/2.0,otemp);
    return goodTrack;
}

double StRichTrack::getExpectedNPhots(StParticleDefinition* particle) {
  
  double pathlengthInc6f14 = myGeometryDb->radiatorDimension().z()*cos(getTheta());
  double pathlengthInquartz = myGeometryDb->quartzDimension().z()*cos(getTheta());
  double pathlengthInmeth = myGeometryDb->proximityGap()*cos(getTheta());

  double particleBeta = mMomentum.mag()
    /sqrt(particle->mass()*particle->mass() + mMomentum.mag()*mMomentum.mag());
  
  
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
