/**********************************************************
 * $Id: StRichMCTrack.cxx,v 2.2 2000/11/01 17:39:27 lasiuk Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMCTrack.cxx,v $
 *  Revision 2.2  2000/11/01 17:39:27  lasiuk
 *  clean up:
 *  CVSn: ----------------------------------------------------------------------
 *
 *  Revision 2.1  2000/09/29 01:35:36  horsley
 *  Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 *  Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 *  Revision 1.1  2000/06/16 02:37:11  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.1  2000/04/03 19:36:09  horsley
 *  initial revision
 **********************************************************/
#include "StRichMCTrack.h"
#include "StRichMCHit.h"

#include "StThreeVector.hh"
#include "StMcEvent.hh"

#include "StGlobals.hh"
#include "StMessMgr.h"

#include "St_DataSet.h"
#include "St_DataSetIter.h" 

#include "tables/St_g2t_track_Table.h"

#include "StMcEventTypes.hh" 

#include "StRrsMaker/StRichEnumeratedTypes.h"


#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichMCTrack::StRichMCTrack() {}

StRichMCTrack::StRichMCTrack(StTrack* t, double b) : StRichTrack(t,b) {
  mStMcTrack     = 0;
  mCommonTpcHits = 0;
  mRadiatorHits  = 0;
  mGapHits       = 0;
  mMCRecoMIP     = 0;  
  
  mRecoGeantPhotons.resize(0);
  mGeantPhotons.resize(0);
  
  // keep original tpc values
  mImpactPoint_TPC = getImpactPoint();
  mMomentum_TPC    = getMomentum();
  mMIP_TPC         = getProjectedMIP();

  mMCMomentum.setX(0.0);
  mMCMomentum.setY(0.0);
  mMCMomentum.setZ(0.0);

  mMCImpactPoint.setX(-999.0);
  mMCImpactPoint.setY(-999.0);
  mMCImpactPoint.setZ(-999.0);

  mMCMIP.setX(-999.0);
  mMCMIP.setY(-999.0);
  mMCMIP.setZ(-999.0);
}



void StRichMCTrack::setGeantPhotons(StMcEvent* mEvent) {
  mGeantPhotons.resize(0);

  cout << "about to fill tracks geant phots" << endl;
  cout << "this event has " << mEvent->tracks().size() << "  tracks" << endl;
  cout << "this tracks mc partner pointer = " <<  getStMcTrack() << endl;
  if (getStMcTrack()) cout << "this tracks p = " <<  getStMcTrack()->momentum() << endl;
  
  for (unsigned int i=0;i<mEvent->tracks().size();i++) {
    if (mEvent->tracks()[i]->geantId() == 50) {   
      if (mEvent->tracks()[i]->parent() == getStMcTrack()) {
	mGeantPhotons.push_back(mEvent->tracks()[i]->richHits()[0]);
      }
    }
  }
}




void StRichMCTrack::setRecoGeantPhotons(const StSPtrVecRichHit* richHits, 
					   StMcEvent* mcevent , St_g2t_track * geantTracks) {

  mRecoGeantPhotons.resize(0);

  if (!richHits || !mcevent || !geantTracks) return;

  // loop over hits
  StMcTrack*   theHitsStMcTrack=0;
  StRichMCHit* monteCarloRichHit=0;

  for (StSPtrVecRichHitConstIterator iter = richHits->begin();iter != richHits->end(); ++iter) {
    theHitsStMcTrack=0;
    monteCarloRichHit = dynamic_cast<StRichMCHit*>(*iter);  
    if (monteCarloRichHit && monteCarloRichHit->getMCInfo().process()==ePhoton) {    
      theHitsStMcTrack=getGeantTrack(monteCarloRichHit,mcevent,geantTracks);
      if (theHitsStMcTrack && theHitsStMcTrack == getStMcTrack() && theHitsStMcTrack->geantId()==50) {
	mRecoGeantPhotons.push_back(monteCarloRichHit);
      }
    }
  }  
}



vector<StRichMCHit*> StRichMCTrack::getRecoGeantPhotons() { return mRecoGeantPhotons;}

void StRichMCTrack::setStMcTrack(StMcTrack* tempStMcTrack) {

  mStMcTrack = tempStMcTrack;
  if (mStMcTrack) {

    double anodeDistanceToPadPlane = myGeometryDb->anodeToPadSpacing();
    double radiatorLowerBoundary_z = myGeometryDb->proximityGap() + myGeometryDb->quartzDimension().z();
    double radiatorUpperBoundary_z = radiatorLowerBoundary_z      + myGeometryDb->radiatorDimension().z(); 
    double oldZPos=0;

    for (size_t i=0;i<mStMcTrack->richHits().size();i++) {
      
      // get the impact point on radiator
      StGlobalCoordinate globalPoint(mStMcTrack->richHits()[i]->position().x(),
				     mStMcTrack->richHits()[i]->position().y(),
				     mStMcTrack->richHits()[i]->position().z());
      
      StRichLocalCoordinate tempRichLocalPoint(0,0,0);
      (*coordinateTransformation)(globalPoint,tempRichLocalPoint);
      StThreeVectorF richLocalPoint(tempRichLocalPoint.position().x(),
				    tempRichLocalPoint.position().y(),
				    tempRichLocalPoint.position().z());

      // geant mip at anode wire position      
      if ( fabs(richLocalPoint.z()) > 0.0 && fabs(richLocalPoint.z()) < 2.0*anodeDistanceToPadPlane ) {
	if (fabs(richLocalPoint.z()-anodeDistanceToPadPlane) <  fabs(mMCMIP.z()-anodeDistanceToPadPlane)) {
	  mMCMIP = richLocalPoint;
	}
	mGapHits++;
      } 

      
      // geant intersection with radiator
      if (richLocalPoint.z() > radiatorLowerBoundary_z &&  richLocalPoint.z() < radiatorUpperBoundary_z ) {
	// if hit in radiator        
	mRadiatorHits++;

	// do the momentum vector rotation here
	StThreeVectorF richLocalMomentum(0,0,0);
	StThreeVector<double> tempRichLocalMomentum(0,0,0);
	StThreeVector<double> richGlobalMomentum(mStMcTrack->richHits()[i]->localMomentum().x(),
						 mStMcTrack->richHits()[i]->localMomentum().y(),
						 mStMcTrack->richHits()[i]->localMomentum().z());
	momentumTransformation->localMomentum(richGlobalMomentum,tempRichLocalMomentum);
	richLocalMomentum.setX(tempRichLocalMomentum.x());
	richLocalMomentum.setY(tempRichLocalMomentum.y());
	richLocalMomentum.setZ(tempRichLocalMomentum.z());

	// parameterize eqn of line using momentum vector of track with the position
	// of geant hit as initial point (x0,y0,z0)
	// x = x0 + parameter*A    y = y0 + parameter*B    z = z0 + parameter*C
	// A = richLocalMomentum.x()/richLocalMomentum.mag(),  ect...
	double parameter = 0.0;
	double A = richLocalMomentum.x()/richLocalMomentum.mag();
	double B = richLocalMomentum.y()/richLocalMomentum.mag();
	double C = richLocalMomentum.z()/richLocalMomentum.mag();
	
	if ( fabs(richLocalMomentum.z()) > 0 && (richLocalPoint.z() > oldZPos) ) {
	
	  oldZPos = richLocalPoint.z();
	  parameter = (radiatorUpperBoundary_z - richLocalPoint.z())/C;
	  
	  richLocalPoint.setX(richLocalPoint.x() + parameter*A);
	  richLocalPoint.setY(richLocalPoint.y() + parameter*B);
	  richLocalPoint.setZ(richLocalPoint.z() + parameter*C);
	  
	  // cout << "impact point:: straight line approx " << endl;
	  //cout << "x = x0 + A*parameter     y = y0 + B*parameter      z = z0 + C*parameter" << endl;
 	  //cout << "parameter = " << parameter << endl;
 	  //cout << "A B C = " << A << "   " << B << "   " << C << endl;
 	  //cout << "setting geant impact point = " <<  richLocalPoint.position() << endl << endl;
	  setGeantMomentumAtRadiator(richLocalMomentum);  
	  setGeantImpactPointAtRadiator(richLocalPoint);
	}
      }   
    }   
  }  
}


StRichMCTrack::~StRichMCTrack() {}
StRichMCHit* StRichMCTrack::getGeantRecoMIP() { return mMCRecoMIP;}
StMcTrack*   StRichMCTrack::getStMcTrack()    { return mStMcTrack;}

StThreeVectorF& StRichMCTrack::getGeantMomentumAtRadiator()    { return mMCMomentum;}
StThreeVectorF& StRichMCTrack::getGeantImpactPointAtRadiator() { return mMCImpactPoint;}
StThreeVectorF& StRichMCTrack::getGeantMIP() { return mMCMIP;}

void StRichMCTrack::setCommonTpcHits(int nhits)            { mCommonTpcHits = nhits;}
void StRichMCTrack::setNumberOfPartners(int npart)         { mNumberOfPartners = npart;}
void StRichMCTrack::setGeantThetaAtRadiator(double theta)  { mMCTheta = theta;}
void StRichMCTrack::setGeantPhiAtRadiator(double phi)      { mMCPhi = phi;}
void StRichMCTrack::setGeantMIP(StThreeVectorF mip)        { mMCMIP = mip;}
void StRichMCTrack::setGeantImpactPointAtRadiator(StThreeVectorF& inputPoint) {mMCImpactPoint = inputPoint;}

int StRichMCTrack::getNumberOfGeantHitsInRadiator() { return mRadiatorHits;}
int StRichMCTrack::getNumberOfGeantHitsInGap()      { return mGapHits;}
int StRichMCTrack::getNumberOfPartners()            { return mNumberOfPartners;}
int StRichMCTrack::getCommonTpcHits()               { return mCommonTpcHits;}

double StRichMCTrack::getGeantThetaAtRadiator() { return mMCTheta;}
double StRichMCTrack::getGeantPhiAtRadiator()   { return mMCPhi;}

vector<StMcRichHit* > StRichMCTrack::getGeantPhotons() { return mGeantPhotons;}


void StRichMCTrack::setGeantRecoMIP(const StSPtrVecRichHit* hits, 
				    StMcEvent* mcevent, St_g2t_track * geantTracks) {

  mMCRecoMIP = 0;
  if (hits) {
    for (size_t hitIndex=0;hitIndex<hits->size();hitIndex++) {
      StRichMCHit * mchit = dynamic_cast<StRichMCHit*>( (*hits)[hitIndex]);
      if (mchit && mchit->getMCInfo().process()==eCharged) {
	if (getStMcTrack() == getGeantTrack(mchit,mcevent,geantTracks)) {
	  mMCRecoMIP =  mchit;
	  //cout << "found mip at index = " << hitIndex << endl;
	  //cout << "assoc mip = " << getAssociatedMIP()->local() << endl;
	  //cout << "hit = " << (*hits)[hitIndex]->local() << endl;
	  break;
	}
      }   
    }
  }  
}


void StRichMCTrack::setGeantMomentumAtRadiator(StThreeVectorF& inputMomentum) {
  mMCMomentum = inputMomentum;

  StThreeVectorF normalVector(0,0,-1);
  if (mMCMomentum.mag()) {
    setGeantThetaAtRadiator(acos(normalVector.dot(mMCMomentum)/mMCMomentum.mag()));
  }
  
  if (mMCMomentum.y() == 0 && mMCMomentum.x() == 0) {setGeantPhiAtRadiator(0.0);}
  else setGeantPhiAtRadiator(mMCMomentum.phi());   
}

void StRichMCTrack::useGeantInfo() {
  // cout << "using geant info" << endl;
 //  if (mStMcTrack) {
//     cout << "monte carlo tracks start veretx r =  " 
// 	 << mStMcTrack->startVertex()->position().perp() << endl;
//     if (mStMcTrack->stopVertex()) { 
//       if (mStMcTrack->stopVertex()->geantProcess() != 0) {
// 	cout << "monte carlo tracks stop veretx r =  " 
// 	     << mStMcTrack->stopVertex()->position().perp() << endl;
//       }
//     }
//   }
  /* 

  cout << "st track p = " << getMomentum() << endl;
  cout << "mc track p = " << mMCMomentum << endl;
  cout << endl;
  cout << "st track theta = " << getTheta()/degree << endl;
  cout << "mc track theta = " << mMCTheta/degree << endl;
  cout << endl;
  cout << "st track phi = " << getPhi()/degree << endl;
  cout << "mc track phi = " << mMCPhi/degree << endl;
  cout << endl;
  cout << "st impact pt = " << getImpactPoint() << endl;
  cout << "mc impact point = " << mMCImpactPoint << endl;
  cout << endl;
  cout << "st MIP = " << getProjectedMIP() << endl;
  cout << "mc MIP = " << mMCMIP << endl;
  cout << endl;
*/

  setImpactPoint(mMCImpactPoint);
  setMomentum(mMCMomentum);
  setProjectedMIP(mMCMIP);
}



void StRichMCTrack::useTPCInfo() {
  //cout << "using TPC info" << endl;

  setImpactPoint(mImpactPoint_TPC);
  setMomentum(mMomentum_TPC);
  setProjectedMIP(mMIP_TPC);
  /*
  cout << "st track p = " << getMomentum() << endl;
  cout << "mc track p = " << mMCMomentum << endl;
  cout << endl;
  cout << "st track theta = " << getTheta()/degree << endl;
  cout << "mc track theta = " << mMCTheta/degree << endl;
  cout << endl;
  cout << "st track phi = " << getPhi()/degree << endl;
  cout << "mc track phi = " << mMCPhi/degree << endl;
  cout << endl;
  cout << "st impact pt = " << getImpactPoint() << endl;
  cout << "mc impact point = " << mMCImpactPoint << endl;
  cout << endl;
  cout << "st MIP = " << getProjectedMIP() << endl;
  cout << "mc MIP = " << mMCMIP << endl;
  cout << endl;
  */
}





StMcTrack* StRichMCTrack::getGeantTrack(StRichMCHit* hit,
					StMcEvent* mcevt,
					St_g2t_track * geantTracks) {
    
    StMcTrack* mcTrack=0;
    if (!hit || !mcevt || !geantTracks) return mcTrack;

    g2t_track_st *trackList = geantTracks->GetTable();

    unsigned int hitIndex = 0;
    if (hit->getMCInfo().process()==ePhoton)       hitIndex = hit->getMCInfo().id();
    else if (hit->getMCInfo().process()==eCharged) hitIndex = hit->getMCInfo().trackp();
    else return 0;
  
    unsigned int  start   = 0;
    unsigned int  end     = mcevt->tracks().size()-1;
    unsigned int  index   = hitIndex;
    unsigned int  counter = 0;

    bool searching=true;

    while (searching) {
	if (index >= mcevt->tracks().size()-1) return 0;

	if (trackList[hitIndex].id == mcevt->tracks()[index]->key()) {
	    return mcevt->tracks()[index];
	}

	if (trackList[hitIndex].id > mcevt->tracks()[index]->key()) {
	    start=index;
	}
	else {
	    end=index;
	}
	index = (end-start)/2 + start;
	counter++;
	if (counter>mcevt->tracks().size()-1) searching=false; 
    }
    return mcTrack; 
}
