//  17 may 01
//  ben norman

//Std
#include <math.h>

//SCL
#include "StThreeVector.hh"
#include "StPhysicalHelix.hh"
#include "StHelix.hh"
#include "StMatrixF.hh"
#include "SystemOfUnits.h"

//StEvent
#include "StEventTypes.h"

//StDb

#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StSvtLocalCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"
//#include "St_db_Maker/St_db_Maker.h"
#include "StSvtDbMaker/StSvtDbMaker.h"
#include "StSvtDbMaker/St_SvtDb_Reader.hh"

//StiMaker
#include "StiMaker/StiMaker.h"

//Sti
#include "StiLocalCoordinate.h"
#include "StiCoordinateTransform.h"
#include "StiDetector.h"
#include "StiDetectorContainer.h"
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiKalmanTrack.h"
#include "StiGeometryTransform.h"
#include "StiDetectorFinder.h"
#include "StiToolkit.h"

StiGeometryTransform* StiGeometryTransform::sinstance = 0;

double gRefAnleForSector(unsigned int sector);
/*
  template <class T>
  T g2dEulerRotation(const T& x, double beta)
  {
  //xprime = cos(beta)*x.x() + sin(beta)*x.y()
  //yprime = -1.*sin(beta)*x.x() + cos(beta)*x.y()
  return T( cos(beta)*x.x() + sin(beta)*x.y(),
  -1.*sin(beta)*x.x() + cos(beta)*x.y(),
  x.z()
  );
  }
*/

//Given a rotation R and error matrix E about the z-axis, we take E'=R*E*R_transpose
template <class T>
T gCovarianceRotation(const T& Error, double theta)
{
    enum Labels {x=1, y=2, z=3};
    
    if ( (Error.numRow()!=3) || (Error.numCol()!=3) ) {
	*(Messenger::instance(MessageType::kGeometryMessage)) <<"gCovarianceRotation()\t Error!: not 3 by 3 matrix.  Undefined Errors"<<endl;
    }

    //Make the rotation matrix
    //StMatrix::operator() (row, column)

    T R(3,3);
    //Diagonal
    R(x,x) = cos(theta); 
    R(y,y) = cos(theta);
    R(z,z) = 1.;

    //Off Diagonal
    R(x,y) = sin(theta);
    R(y,x) = -1.*sin(theta);

    R(x,z) = 0.; 
    R(y,z) = 0.;
    R(z,x) = 0.; 
    R(z,y) = 0.; 

    //Now Transform
    return ( R*Error*R.transpose() );
}

StiGeometryTransform::StiGeometryTransform()
{
    *(Messenger::instance(MessageType::kGeometryMessage)) <<"StiGeometryTransform::StiGeometryTransform()"<<endl;

    sinstance = this;

    m_pCoordinateTransform = StiToolkit::instance()->getCoordinateTransform();

    //*(Messenger::instance(MessageType::kGeometryMessage)) <<"\tLeaving StiGeometryTransform::StiGeometryTransform()"<<endl;
} // StiGeometryTransform()

StiGeometryTransform::~StiGeometryTransform()
{
    *(Messenger::instance(MessageType::kGeometryMessage)) <<"StiGeometryTransform::~StiGeometryTransform()"<<endl;
} // ~StiGeometryTransform

StiGeometryTransform* StiGeometryTransform::instance()
{
    return (sinstance) ? sinstance : new StiGeometryTransform();
}

void StiGeometryTransform::kill()
{
    if (sinstance) {
	delete sinstance;
	sinstance = NULL;
    }
}

void StiGeometryTransform::setStiHitError(const StHit* stHit, StiHit* stiHit, double theta)
{
    //For now, set all hit errors to fixed values:

    stiHit->setSxx(1.E-6); //cm
    stiHit->setSxy(1.E-6); //cm
    stiHit->setSyz(1.E-6); //cm
    stiHit->setSyy(150.E-4); //cm
    stiHit->setSzz(300.E-4); //cm
}

//Hit Translation routines

void StiGeometryTransform::operator() (const StPrimaryVertex* vtx, StiHit* stihit)
{
    //A primary vertex doesn't come from a detector, so it doesn't have a well defined refAngle and centerRadius
    //We'll define these two from global position in cylindrical coordinates
    //refAngle = arctan(global_y / global_x)
    //centerRadius = sqrt (global_x^2 + global_y^2)
    //We'll then say that Sti_x = centerRadius and Sti_y = 0, with Sti_z begin global z, as usual

    const StThreeVectorF& position = vtx->position();
    double pos = sqrt(position.x()*position.x() + position.y()*position.y() );
    double refangle = atan2( position.y(), position.x() );

    stihit->setRefangle( refangle );
    stihit->setPosition( pos );
    stihit->setX( pos );
    stihit->setY( 0. );
    stihit->setZ( position.z() );
    
    return;
}

void StiGeometryTransform::operator() (const StTpcHit* tpchit, StiHit* stihit)
{

    double refangle = 
        m_pCoordinateTransform->phiForTpcSector( tpchit->sector() );
    double pos = 
        m_pCoordinateTransform->positionForTpcPadrow( tpchit->padrow() );

    //We'll temporarily keep
    stihit->setStHit(const_cast<StTpcHit*>(tpchit));

    //Transform 
    StGlobalCoordinate gHit( tpchit->position() );
    StiLocalCoordinate lHit;
    m_pCoordinateTransform->globalTpcToSti(gHit, lHit, tpchit->sector());
    stihit->set(refangle, pos, lHit.position().x(), lHit.position().y(),
                lHit.position().z());

    //Now Transform Errors by hand (using matrix is too slow).
    setStiHitError(tpchit, stihit, refangle);
    
    //StMatrixF covMatrix = tpchit->covariantMatrix();
    //stihit->setError( gCovarianceRotation( covMatrix, stihit->refangle() ) );

/*
    cout << "StTpcHit: (" << tpchit->position().x() 
         << ", " << tpchit->position().y()
         << ", " << tpchit->position().z() 
         << ") sector=" << tpchit->sector() << endl
         << "StiHit:   (" << stihit->x() 
         << ", " << stihit->y()
         << ", " << stihit->z() 
         << ")" << endl;
*/
    /*
      //This is currently performed in the HitFiller to speed things up (MLM, 8/27/01)
      // find detector for this hit, remembering that we (ITTF) only use
      // 12 tpc sectors (1-12) instead of separate sectors for the east end
      StiDetectorFinder *pFinder = StiDetectorFinder::instance();
      char szBuf[100];
      int iIttfSector = 12 - (tpchit->sector() - 12)%12;
      sprintf(szBuf, "Tpc/Padrow_%d/Sector_%d", (int) tpchit->padrow(), iIttfSector);
      StiDetector* layer = pFinder->findDetector(szBuf);
      if (!layer) {
      *(Messenger::instance(MessageType::kGeometryMessage)) <<" Error, no layer for sector: "<<tpchit->sector()<<"\tpadrow: "<<tpchit->padrow()<<endl;
      }
      else {
      stihit->setDetector( layer );
      }
    */
    
    return;
}


void StiGeometryTransform::operator() (const StiHit* stihit, StTpcHit* tpchit)
{
}

void StiGeometryTransform::operator() (const StSvtHit* svthit, StiHit* stihit){
  
  //We'll temporarily keep
  stihit->setStHit(const_cast<StSvtHit*>(svthit));

  // There is some problem with StSvtHit::ladder() and StSvtHit::layer(),
  // so we'll calculate those here.

  StThreeVector<double> position(svthit->position().x(),
                                 svthit->position().y(),
                                 svthit->position().z());
  unsigned int iBarrel = m_pCoordinateTransform->svtBarrelForGlobal(position);
  unsigned int iLadder = m_pCoordinateTransform->svtLadderForGlobal(position);
  unsigned int iLayer = 2*iBarrel - (iLadder + 1)%2;
  
  
  if(iLayer!=svthit->layer() || iLadder!=svthit->ladder()){
    cout << "SvtHit [perp, phi]=[" << position.perp() << ", "
         << position.phi() << "], layer=" << svthit->layer()
         << " " << iLayer
         << ", ladder=" << svthit->ladder() << " " << iLadder << endl;
  }
  

  // first the position & ref angle
  double dRefAngle = m_pCoordinateTransform->phiForSvtBarrelLadder(
      iBarrel, iLadder);
  double dPosition = m_pCoordinateTransform->positionForSvtBarrel(iBarrel);

  //Transform coords
  StGlobalCoordinate gHit( svthit->position() );
  StiLocalCoordinate lHit;
  m_pCoordinateTransform->globalSvtToSti(gHit, lHit, iBarrel, iLadder);
  stihit->set(dRefAngle, dPosition, lHit.position().x(), lHit.position().y(),
              lHit.position().z());

  //Transform the error
  setStiHitError(svthit, stihit, dRefAngle);

  //We currently do this in StiHitFiller to speed up the process
  
  // find detector for this hit
  char szBuf[100];

  sprintf(szBuf, "Svt/Layer_%d/Ladder_%d/Wafers", iLayer, iLadder);
  StiDetector* layer = StiDetectorFinder::instance()->findDetector(szBuf);
  if (!layer) {
    *(Messenger::instance(MessageType::kGeometryMessage)) <<"Error, no detector for layer "<<iLayer<<"\tladder: "<<iLadder<<"\tABORT"<<endl;
    cerr <<"Error, no detector " << szBuf << endl;
    return;
  }
  
  stihit->setDetector( layer );

}

void StiGeometryTransform::operator() (const StiHit* stihit, StSvtHit* svthit){
}

void StiGeometryTransform::operator() (const StSsdHit* ssdhit, StiHit* stihit){
}

void StiGeometryTransform::operator() (const StiHit* stihit, StSsdHit* ssdhit){
}

void StiGeometryTransform::operator() (const StiKalmanTrackNode *pNode,
                                       StHelix *pHelix)
{
  // calculate the helix origin in global coords
  StThreeVector<double> origin(pNode->fX, pNode->fP0,pNode->fP1);
  origin.rotateZ(pNode->fAlpha);
  const StThreeVector<double> p=pNode->getGlobalMomentum();
  double h, phase;
  h = (pNode->getCharge()*StiKalmanTrackNode::getFieldConstant() <= 0) ? 1 : -1;
  phase = (p.y()==0&&p.x()==0) ? phase =(1-2.*h)*M_PI/4. : atan2(p.y(),p.x())-h*M_PI/2.;
  *pHelix = StHelix(pNode->getCurvature(),
		    pNode->getDipAngle(),
		    phase,
		    origin,
		    h);

  //*pHelix = StPhysicalHelix(pNode->getGlobalMomentum(), 
  //			    origin, 
  //                          StiKalmanTrackNode::getFieldConstant(), 
  //		    pNode->getCharge());
}

void StiGeometryTransform::operator() (StiHitContainer* hc, const StGlobalTrack* st, StiKalmanTrack* sti,
				       unsigned int maxHits, const StTpcHitFilter* filter) const
{
    Messenger& mess = *(Messenger::instance(MessageType::kGeometryMessage));
    
    mess <<"\n\n\nmaxHits:\t"<<maxHits<<"\tfilter"<<filter<<endl;
    
    //now get hits
    StPtrVecHit hits = st->detectorInfo()->hits(kTpcId);
    sort( hits.begin(), hits.end(), StHitRadiusGreaterThan() );
    hitvector hitvec;
    
    for (vector<StHit*>::iterator it=hits.begin();
	 it!=hits.end() && hitvec.size()<=maxHits; ++it) {
	StTpcHit* hit = dynamic_cast<StTpcHit*>(*it);
	if (!hit) {
	    mess <<"StiGeometryTransform::operator(GlobalTrack->KalmanTrack). Error:\t";
	    mess <<"StHit->StTpcHit cast failed.  Skip this point."<<endl;
	}
	else  {
	    //Find StiHit for this StHit
	    bool passedFilter=true;
	    if (filter!=0) 
		passedFilter = filter->operator()(*hit);
	    
	    if (passedFilter) {

		//pair<double, double> myPair =
		//  StiGeometryTransform::instance()->angleAndPosition(hit);		
		//double refAngle=myPair.first;
		//double position=myPair.second;		
		//const hitvector& stiHits = StiHitContainer::instance()->hits(refAngle, position);

		//Find the detector for this set of hits:
		char szBuf[100];
		unsigned int prow = hit->padrow();
		unsigned int sector=hit->sector();
		unsigned int iIttfSector=sector;
		if (sector>12) {
		    iIttfSector = 12 - (sector-12)%12;
		}
		sprintf(szBuf, "Tpc/Padrow_%d/Sector_%d",
			static_cast<int>(prow), static_cast<int>(iIttfSector));
		const StiDetector* layer =
		    StiDetectorFinder::instance()->findDetector(szBuf);
		if (!layer) {
		    cout <<"Error, no Detector for this hit. ERROR:\t";
		    cout <<"Detector for (sector,padrow): (";
		    cout <<iIttfSector<<","<<prow<<") not found.  Abort"<<endl;
		}
		const hitvector& stiHits = hc->hits(layer);

		if (stiHits.size()==0) 	{
		    mess <<"Error, no StiHits for this sector, padrow"<<endl;
		    sti=0;
		    return;
		}
		SameStHit mySameStHit;
		mySameStHit.stHit = hit;
		hitvector::const_iterator where = find_if(stiHits.begin(), stiHits.end(),
							  mySameStHit);
		if (where==stiHits.end()) {
		    mess <<"Error, no StiHit with this StHit was found"<<endl;
		    sti=0;
		    return;
		}
		else {
		    StiHit* theStiHit = *where;
		    //Push up the errors by 10x for these hits
		    //theStiHit->setSyy(theStiHit->syy()*10.);
		    //theStiHit->setSzz(theStiHit->szz()*10.);
		    hitvec.push_back(theStiHit);
		}
	    }
	}
    }
    
        //Now get the helix
    StPhysicalHelixD sthelix = st->geometry()->helix();
    
    //Get the (x-y) center of the circle and z0 in global coordinates,
    //that's what StiKalmanTrack needs:
    //note, StHelix origin is the first point on the track, not the center of the circle!
    StThreeVectorD stiGlobalOrigin( sthelix.xcenter(), sthelix.ycenter(), sthelix.origin().z());
    
    double curvature = sthelix.curvature();
    if (sthelix.h()<0) 
	curvature=-curvature;
    
    double tanLambda = tan(sthelix.dipAngle());
	    
    if (hitvec.size()<3) {
	mess << " Track Transform Error:\t"
	      <<" hitvec.size()<3.  Abort"<<endl;
	return;
    }
    sti->initialize(curvature, tanLambda, stiGlobalOrigin, hitvec);
    
}
