/**********************************************************
 * $Id: StRichMCTrack.cxx,v 1.1 2000/06/16 02:37:11 horsley Exp $
 *
 * Description:
 *  
 *
 *  $Log: StRichMCTrack.cxx,v $
 *  Revision 1.1  2000/06/16 02:37:11  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 *  Revision 1.1  2000/06/16 02:37:11  horsley
 *  many additions, added features to pad plane display (MIPS, rings, etc)
 *  along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 **********************************************************/

#include "StRichMCTrack.h"
#include "St_DataSet.h"
#include "StRichMCHit.h"

#include "StThreeVector.hh"
#include "StMcEvent.hh"
#include "StGlobals.hh"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_ftp_hit_Table.h"
#include "tables/St_g2t_rch_hit_Table.h"
#include "tables/St_g2t_svt_hit_Table.h"
#include "tables/St_g2t_tpc_hit_Table.h"

#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_particle_Table.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h" 


#include "StRrsMaker/StRichEnumeratedTypes.h"


#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

StRichMCTrack::StRichMCTrack() {}

StRichMCTrack::StRichMCTrack(StTrack* t, double b) : StRichTrack(t,b) {
  mCommonTpcHits = 0;

  // keep original tpc values
  mImpactPoint_TPC = getImpactPoint();
  mMomentum_TPC    = getMomentum();
  mMIP_TPC         = getProjectedMIP();
  mMCRecoMIP     = 0;  

  mMCMomentum.setX(0.0);
  mMCMomentum.setY(0.0);
  mMCMomentum.setZ(0.0);

  mMCImpactPoint.setX(-999.0);
  mMCImpactPoint.setY(-999.0);
  mMCImpactPoint.setZ(-999.0);

  mMCMIP.setX(-999.0);
  mMCMIP.setY(-999.0);
  mMCMIP.setZ(-999.0);

  mMCStopVertex.setX(-999.0);
  mMCStopVertex.setY(-999.0);
  mMCStopVertex.setZ(-999.0);
}
  mMCMIP.setY(-999.0);
StRichMCTrack::~StRichMCTrack() {
  mMCMIP.setZ(-999.0);
}

void
StRichMCTrack::setCommonTpcHits(int nhits) {
  mCommonTpcHits = nhits;
	mGeantPhotons.push_back(mEvent->tracks()[i]->richHits()[0]);
      }
int
StRichMCTrack::getCommonTpcHits() {
  return mCommonTpcHits;
}
    }
void
StRichMCTrack::setNumberOfPartners(int npart) {
  mNumberOfPartners = npart;
}
  }
int
StRichMCTrack::getNumberOfPartners() {
  return mNumberOfPartners;
}
}

// monte carlo
StMcTrack* StRichMCTrack::getStMcTrack() {
  return mStMcTrack;
}
void StRichMCTrack::setRecoGeantPhotons(const StSPtrVecRichHit* richHits, 
void StRichMCTrack::setGeantStopVertex(StThreeVector<double>& vert) {
  mMCStopVertex = vert;
}

StThreeVector<double>& StRichMCTrack::getGeantStopVertex() {
  return mMCStopVertex;
	mRecoGeantPhotons.push_back(monteCarloRichHit);
      }
    }
void StRichMCTrack::setGeantPhotons(StMcEvent* mEvent) {

  cout << "setting the GEANT phots" << endl;

 for (unsigned int i=0;i<mEvent->tracks().size();i++) {
   
   if (mEvent->tracks()[i]->geantId() == 50) {
     
     if (mEvent->tracks()[i]->parent() == this->getStMcTrack()) {
       
       StRichLocalCoordinate localPosition(0,0,0);
       StGlobalCoordinate globalPosition(mEvent->tracks()[i]->richHits()[0]->position().x(),
					 mEvent->tracks()[i]->richHits()[0]->position().y(),
					 mEvent->tracks()[i]->richHits()[0]->position().z());
       (*coordinateTransformation)(globalPosition,localPosition);
       mGeantPhotons.push_back(localPosition.position());
     }
   }
 }
 cout << "done setting geant phots" << endl;
}
  }  
vector<StThreeVector<double> > StRichMCTrack::getGeantPhotons() {
  return mGeantPhotons;
}



vector<StRichMCHit*> StRichMCTrack::getRecoGeantPhotons() { return mRecoGeantPhotons;}
  
  cout << "setting StMcTrack" << endl;


    
    // set tracks geant stop vertex
    if (mStMcTrack->stopVertex()) {
      StGlobalCoordinate testGlobal(mStMcTrack->stopVertex()->position().x(),
				    mStMcTrack->stopVertex()->position().y(),
				    mStMcTrack->stopVertex()->position().z());
      
      StRichLocalCoordinate testLocalPoint(0,0,0);
      (*coordinateTransformation)(testGlobal,testLocalPoint);
            
      mMCStopVertex.setX(mStMcTrack->stopVertex()->position().x());
      mMCStopVertex.setY(mStMcTrack->stopVertex()->position().y());
      mMCStopVertex.setZ(mStMcTrack->stopVertex()->position().z());    
    }
void StRichMCTrack::setStMcTrack(StMcTrack* tempStMcTrack) {

  mStMcTrack = tempStMcTrack;
  if (mStMcTrack) {

    double anodeDistanceToPadPlane = myGeometryDb->anodeToPadSpacing();

    for (unsigned int i=0;i<mStMcTrack->richHits().size();i++) {
    double radiatorUpperBoundary_z = radiatorLowerBoundary_z      + myGeometryDb->radiatorDimension().z(); 
    double oldZPos=0;

    for (size_t i=0;i<mStMcTrack->richHits().size();i++) {
      
      // get the impact point on radiator
      StRichLocalCoordinate richLocalPoint(0,0,0);
      (*coordinateTransformation)(globalPoint,richLocalPoint);
      (*coordinateTransformation)(globalPoint,tempRichLocalPoint);
      
      StThreeVectorF richLocalPoint(tempRichLocalPoint.position().x(),
      if ( fabs(richLocalPoint.position().z()) > 0.05 &&
	   fabs(richLocalPoint.position().z()) < 2.0*anodeDistanceToPadPlane ) {
	
	if (fabs(richLocalPoint.position().z()-anodeDistanceToPadPlane) < fabs(mMCMIP.z()-anodeDistanceToPadPlane)) {
	  mMCMIP = richLocalPoint.position();
      // geant mip at anode wire position      
      if ( fabs(richLocalPoint.z()) > 0.0 && fabs(richLocalPoint.z()) < 2.0*anodeDistanceToPadPlane ) {
	if (fabs(richLocalPoint.z()-anodeDistanceToPadPlane) <  fabs(mMCMIP.z()-anodeDistanceToPadPlane)) {
	  mMCMIP = richLocalPoint;
	}
	mGapHits++;
      if (richLocalPoint.position().z() > radiatorLowerBoundary_z && 
	  richLocalPoint.position().z() < radiatorUpperBoundary_z ) {  // if hit in radiator        
	
      
      // geant intersection with radiator
      if (richLocalPoint.z() > radiatorLowerBoundary_z &&  richLocalPoint.z() < radiatorUpperBoundary_z ) {
	StThreeVector<double> richLocalMomentum(0,0,0);

	// do the momentum vector rotation here
	StThreeVectorF richLocalMomentum(0,0,0);
	momentumTransformation->localMomentum(richGlobalMomentum,richLocalMomentum);
	
	richLocalMomentum.setX(tempRichLocalMomentum.x());
	richLocalMomentum.setY(tempRichLocalMomentum.y());
	richLocalMomentum.setZ(tempRichLocalMomentum.z());

	// parameterize eqn of line using momentum vector of track with the position
	// of geant hit as initial point (x0,y0,z0)
	// x = x0 + parameter*A    y = y0 + parameter*B    z = z0 + parameter*C
	// A = richLocalMomentum.x()/richLocalMomentum.mag(),  ect...
	double parameter = 0.0;
	if ( fabs(richLocalMomentum.z()) > 0 && (richLocalPoint.position().z() > oldZPos) ) {
	double B = richLocalMomentum.y()/richLocalMomentum.mag();
	  oldZPos = richLocalPoint.position().z();
	  parameter = (radiatorUpperBoundary_z - richLocalPoint.position().z())/C;
	if ( fabs(richLocalMomentum.z()) > 0 && (richLocalPoint.z() > oldZPos) ) {
	  richLocalPoint.position().setX(richLocalPoint.position().x() + parameter*A);
	  richLocalPoint.position().setY(richLocalPoint.position().y() + parameter*B);
	  richLocalPoint.position().setZ(richLocalPoint.position().z() + parameter*C);
	  
	  cout << "impact point:: straight line approx " << endl;
	  cout << "x = x0 + A*parameter     y = y0 + B*parameter      z = z0 + C*parameter" << endl;
 	  cout << "parameter = " << parameter << endl;
 	  cout << "A B C = " << A << "   " << B << "   " << C << endl;
 	  cout << "setting geant impact point = " <<  richLocalPoint.position() << endl << endl;
	  //cout << "x = x0 + A*parameter     y = y0 + B*parameter      z = z0 + C*parameter" << endl;
	  setGeantImpactPointAtRadiator(richLocalPoint.position());
 	  //cout << "A B C = " << A << "   " << B << "   " << C << endl;
    }


          
	  setGeantMomentumAtRadiator(richLocalMomentum);  
    
	  setGeantImpactPointAtRadiator(richLocalPoint);

  cout << "done setting StMcTrack" << endl;
	}
      }   
StThreeVector<double>& StRichMCTrack::getGeantMomentumAtRadiator() {
  return mMCMomentum;
}
    }   
StThreeVector<double>& StRichMCTrack::getGeantImpactPointAtRadiator() {
  return mMCImpactPoint;
	}
      }   
    }
StThreeVector<double>& StRichMCTrack::getGeantMIP() {
  return mMCMIP;
}


int StRichMCTrack::getNumberOfGeantHitsInRadiator() {
  return mRadiatorHits;
}

int StRichMCTrack::getNumberOfGeantHitsInGap() {
  return mGapHits;
}


void StRichMCTrack::setGeantMomentumAtRadiator(StThreeVector<double>& inputMomentum) {
}

  StThreeVector<double> normalVector(0,0,-1);
void StRichMCTrack::setGeantMomentumAtRadiator(StThreeVectorF& inputMomentum) {
  mMCMomentum = inputMomentum;

  StThreeVectorF normalVector(0,0,-1);
  if (mMCMomentum.mag()) {
  else setGeantPhiAtRadiator(mMCMomentum.phi()); 
  
}


void StRichMCTrack::setGeantImpactPointAtRadiator(StThreeVector<double>& inputPoint) {
  mMCImpactPoint = inputPoint;
}


void StRichMCTrack::setGeantThetaAtRadiator(double theta) {
  mMCTheta = theta;
}


void StRichMCTrack::setGeantPhiAtRadiator(double phi) {
  mMCPhi = phi;
}


double StRichMCTrack::getGeantThetaAtRadiator() {
  return mMCTheta;
}


double StRichMCTrack::getGeantPhiAtRadiator() {
  return mMCPhi;
}

void StRichMCTrack::setGeantMIP(StThreeVector<double> mip) {
  mMCMIP = mip;
  }
  
  if (mMCMomentum.y() == 0 && mMCMomentum.x() == 0) {setGeantPhiAtRadiator(0.0);}
 //  cout << "using geant info" << endl;
//   if (mStMcTrack) {

void StRichMCTrack::useGeantInfo() {
  // cout << "using geant info" << endl;
 //  if (mStMcTrack) {
//     cout << "monte carlo tracks start veretx r =  " 
// 	 << mStMcTrack->startVertex()->position().perp() << endl;
//     if (mStMcTrack->stopVertex()) { 
//       if (mStMcTrack->stopVertex()->geantProcess() != 0) {
//   } 
//   cout << "st track p = " << getMomentum() << endl;
//   cout << "mc track p = " << mMCMomentum << endl;
//   cout << endl;
//   cout << "st track theta = " << getTheta()/degree << endl;
//   cout << "mc track theta = " << mMCTheta/degree << endl;
//   cout << endl;
//   cout << "st track phi = " << getPhi()/degree << endl;
//   cout << "mc track phi = " << mMCPhi/degree << endl;
//   cout << endl;
//   cout << "st impact pt = " << getImpactPoint() << endl;
//   cout << "mc impact point = " << mMCImpactPoint << endl;
//   cout << endl;
//   cout << "st MIP = " << getProjectedMIP() << endl;
//   cout << "mc MIP = " << mMCMIP << endl;
//   cout << endl;
//       }
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
  cout << "mc MIP = " << mMCMIP << endl;
  // cout << "st track p = " << getMomentum() << endl;
//   cout << "mc track p = " << mMCMomentum << endl;
//   cout << endl;
//   cout << "st track theta = " << getTheta()/degree << endl;
//   cout << "mc track theta = " << mMCTheta/degree << endl;
//   cout << endl;
//   cout << "st track phi = " << getPhi()/degree << endl;
//   cout << "mc track phi = " << mMCPhi/degree << endl;
//   cout << endl;
//   cout << "st impact pt = " << getImpactPoint() << endl;
//   cout << "mc impact point = " << mMCImpactPoint << endl;
//   cout << endl;
//   cout << "st MIP = " << getProjectedMIP() << endl;
//   cout << "mc MIP = " << mMCMIP << endl;
//   cout << endl;
  cout << endl;
    counter++;
    if (counter>mcevt->tracks().size()-1) searching=false; 
  }
  return mcTrack; 
}


