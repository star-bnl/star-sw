//  17 may 01
//  ben norman

//Std
#include <math.h>

//SCL
#include "StThreeVector.hh"
#include "StHelix.hh"
#include "StMatrixF.hh"

//StEvent
#include "StEventTypes.h"

//StDb

#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StSvtLocalCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StTpcDb/StTpcDb.h"

//Svt Tables

#include "tables/St_svg_geom_Table.h"
#include "tables/St_svg_config_Table.h"
#include "tables/St_svg_shape_Table.h"

//StiMaker
#include "StiMaker/StiMaker.h"

//Sti
#include "StiPlacement.h"
#include "StiDetector.h"
#include "StiDetectorContainer.h"
#include "StiMapUtilities.h"
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiKalmanTrack.h"
#include "StiGeometryTransform.h"
#include "StiHelixFitter.h"
#include "StiHelixCalculator.h"
#include "StiDetectorFinder.h"

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

    // read in svt geometry tables
    //*(Messenger::instance(MessageType::kGeometryMessage)) <<"Read in svt geometry tables: preparing to seg-fualt"<<endl;

    St_DataSetIter local(StiMaker::instance()->GetInputDB("svt"));
    //*(Messenger::instance(MessageType::kGeometryMessage)) <<"Instantiated local"<<endl;

    svgConfig = 
	dynamic_cast<St_svg_config *>(local("svgpars/config"))->GetTable()[0];
    //*(Messenger::instance(MessageType::kGeometryMessage)) <<"Instantiated svgConfig"<<endl;

    aSvgGeom = dynamic_cast<St_svg_geom *>(local("svgpars/geom"))->GetTable();
    //*(Messenger::instance(MessageType::kGeometryMessage)) <<"Instantiated aSvgGeom"<<endl;

    aSvgShape = dynamic_cast<St_svg_shape *>(local("svgpars/shape"))->GetTable();
    //*(Messenger::instance(MessageType::kGeometryMessage)) <<"Instantiated aSvgShape"<<endl;
    
    //*(Messenger::instance(MessageType::kGeometryMessage)) <<"instantiate TPC coord x-form"<<endl;
    tpcTransform = new StTpcCoordinateTransform(gStTpcDb);

    *(Messenger::instance(MessageType::kGeometryMessage)) <<"Generating Padrow Radius Map"<<endl;

    // store svt + ssd as padrows 1-7
    for (unsigned int padrow=1; padrow<=7; ++padrow){
      double center = centerForSvgLadder(padrow, 1).perp();
      mpadrowradiusmap.insert( padrow_radius_map_ValType( padrow, center ) );
    }

    // store ifc as padrow 10
    mpadrowradiusmap.insert( padrow_radius_map_ValType( 
        10, gStTpcDb->Dimensions()->ifcRadius()) );
    for (unsigned int padrow=1; padrow<=45; ++padrow) {
	double center = centerForTpcPadrow(1, padrow).perp();
        mpadrowradiusmap.insert( padrow_radius_map_ValType( padrow + 100, center ) );	
    }

    *(Messenger::instance(MessageType::kGeometryMessage)) <<"\nPadrow\tRadius"<<endl;
    
    //for (padrow_radius_map::const_iterator it=mpadrowradiusmap.begin();
    // it!=mpadrowradiusmap.end(); ++it) {
    //*(Messenger::instance(MessageType::kGeometryMessage)) <<(*it).first<<"\t"<<(*it).second<<endl;
    //}

    sinstance = this;
    //*(Messenger::instance(MessageType::kGeometryMessage)) <<"\tLeaving StiGeometryTransform::StiGeometryTransform()"<<endl;
} // StiGeometryTransform()

StiGeometryTransform::~StiGeometryTransform()
{
    *(Messenger::instance(MessageType::kGeometryMessage)) <<"StiGeometryTransform::~StiGeometryTransform()"<<endl;
    delete tpcTransform;
    tpcTransform = 0;
} // ~StiGeometryTransform

StiGeometryTransform* StiGeometryTransform::instance()
{
    return (sinstance) ? sinstance : new StiGeometryTransform();
}

void StiGeometryTransform::kill()
{
    if (sinstance) {
	delete sinstance;
	sinstance = 0;
    }
}

// returns the reference angle for the given sector number (out of the 
// given total).  This assumes the star convention where the highest
// numbered sector is at "12 o'clock", or pi/2, and the sector numbering
// _decreases_ with increasing phi.  [I guess this must have seemed like
// a good idea at the time....]
//
// returns in [0,2pi)
double StiGeometryTransform::phiForWestSector(int iSector, int nSectors) const
{
    
  int offset = nSectors/4;
  double deltaPhi = 2.*M_PI/nSectors;
  
  // make phi ~ sector (not -sector) and correct offset
  double dPhi = (offset - iSector)*deltaPhi;
  while(dPhi >= 2.*M_PI){ dPhi -= 2.*M_PI; }
  while(dPhi <  0.){      dPhi += 2.*M_PI; }

  return dPhi;
    
} // phiForWestSector

// as above, but numbering _increases_ with increasing phi.
double StiGeometryTransform::phiForEastSector(int iSector, int nSectors) const
{
    
    int offset = 3*nSectors/4;
    double deltaPhi = 2.*M_PI/nSectors;

    // correct offset
    double dPhi = (iSector - offset)*deltaPhi;
    while(dPhi >= 2.*M_PI){ dPhi -= 2.*M_PI; }
    while(dPhi <  0.){      dPhi += 2.*M_PI; }

    return dPhi;

} // phiForEastSector

int StiGeometryTransform::westSectorForPhi(double phi, int nSectors) const
{
    
    int offset = nSectors/4;
    double deltaPhi = 2.*M_PI/nSectors;  
    
    int iSector = 0;
    while(phi > deltaPhi/2.){ phi -= deltaPhi; iSector++; }
    while(phi < deltaPhi/2.){ phi += deltaPhi; iSector--; }
    
    iSector = offset - iSector;
    if(iSector<1){ iSector += nSectors; }
    
    return iSector;
    
} // westSectorForPhi

int StiGeometryTransform::eastSectorForPhi(double phi, int nSectors) const
{
    
    int offset = nSectors/4;
    double deltaPhi = 2.*M_PI/nSectors;  
    
    int iSector = 0;
    while(phi > deltaPhi/2.){ phi -= deltaPhi; iSector++; }
    while(phi < deltaPhi/2.){ phi += deltaPhi; iSector--; }
    
    iSector = iSector + (2*nSectors + offset);
    if(iSector>2*nSectors){ iSector -= nSectors; }
    
    return iSector;
    
} // eastSectorForPhi

// returns a vector (with z==0) pointing from the origin to  the center of the given padrow or ladder.
StThreeVector<double> StiGeometryTransform::centerForTpcPadrow(int sector, int padrow) const
{
    double radius = gStTpcDb->PadPlaneGeometry()->radialDistanceAtRow(padrow);
    double phi = phiForSector(sector, 12);
    
    return StThreeVector<double>(radius*cos(phi), radius*sin(phi), 0.);
}

// this uses the database convention of 6 svt layers + 1 ssd layer.
// Every other svt ladder is thus bogus (1,3,5,7 on layer 1, eg), but the xform  works regardless
StThreeVector<double> StiGeometryTransform::centerForSvgLadder(int layer, int ladder) const
{
    double radius = svgConfig.layer_radius[layer - 1];
    int nLadders = 2*svgConfig.n_ladder[layer - 1];
    double phi = phiForWestSector(ladder, nLadders);
    
    return StThreeVector<double>(radius*cos(phi), radius*sin(phi), 0.);
}
    
int StiGeometryTransform::sectorForTpcCoords(const StThreeVector<double> &vec) const
{
    return tpcTransform->sectorFromCoordinate(vec);
}

int StiGeometryTransform::padrowForTpcCoords(const StThreeVector<double> &vec) const
{
    int sector = sectorForTpcCoords(vec);
    return tpcTransform->rowFromLocal( tpcTransform->rotateToLocal(vec, sector));
}

// finds nearest real ladder (as above, could be bogus [electronics instead  of wafers]) and layer.
int StiGeometryTransform::layerForSvgCoords(const StThreeVector<double> &vec) const
{
    double minDeltaR = 200.;
    int minLayer = 0;
    for(int layer = 0; layer < 7; layer++){
	if( fabs(svgConfig.layer_radius[layer] - vec.perp()) < minDeltaR){
	    minDeltaR = fabs(svgConfig.layer_radius[layer] - vec.perp());
	    minLayer = layer;
	}
    }
    return minLayer + 1;
}

int StiGeometryTransform::ladderForSvgCoords(const StThreeVector<double> &vec) const
{
    int layer = layerForSvgCoords(vec);
    int nLadders = svgConfig.n_ladder[layer - 1];    
    return westSectorForPhi(vec.phi(), nLadders);
}
    
double StiGeometryTransform::phiForSector(int iSector, int nSectors) const
{
    double phi;
    if(iSector>nSectors){
	phi = phiForEastSector(iSector, nSectors);
    }
    else {
	phi = phiForWestSector(iSector, nSectors);
    }

    return phi;
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

    if (refangle<0.) refangle+=2.*M_PI;
    
    stihit->setRefangle( refangle );
    stihit->setPosition( pos );
    stihit->setX( pos );
    stihit->setY( 0. );
    stihit->setZ( position.z() );
    
    return;
}

void StiGeometryTransform::operator() (const StTpcHit* tpchit, StiHit* stihit)
{

    //Change if we change numbering scheme
    double refangle = phiForSector( tpchit->sector(), 12 );
    double pos = mpadrowradiusmap[ tpchit->padrow() + 100];
    stihit->setRefangle( refangle );
    stihit->setPosition( pos );

    //We'll temporarily keep
    stihit->setStHit(const_cast<StTpcHit*>(tpchit));

    //Make Tpc hits
    StGlobalCoordinate gHit( tpchit->position() );
    StTpcLocalSectorCoordinate lsHit;

    //Transform 
    tpcTransform->operator()(gHit, lsHit);

    //Keep z in global coordinates
    stihit->setZ( tpchit->position().z() );

    //Swap x for y (we switched sign of z, so we're ok with right-hand-rule)
    if (tpchit->position().z() > 0) {
	stihit->setX( lsHit.position().y() );
	stihit->setY( lsHit.position().x() );
    }

    //Take x -> -x, then swap x for y
    else {
	stihit->setX( lsHit.position().y() );
	stihit->setY( -1.*lsHit.position().x() );
    }

    
    //The errors take for god-damn ever to transform!
    //Now Transform Errors
    StMatrixF covMatrix = tpchit->covariantMatrix();
    stihit->setError( gCovarianceRotation( covMatrix, stihit->refangle() ) );
    
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

  // first the position & ref angle
  int iLadder = svthit->ladder();
  int nLadders = svgConfig.n_ladder[svthit->layer() - 1]*2; // 0-indexed
  double dRefAngle = phiForSector( iLadder, nLadders );
  double dPosition = mpadrowradiusmap[svthit->layer()]; // this is 1-indexed
  stihit->setRefangle( dRefAngle );
  stihit->setPosition( dPosition );

  // z is fine, we just need to do a 2d rotation so that x' is radially outward
  // from the wafer center.  Note that since the svt ladders have a zero
  // orientation angle, this _is_ the normal to the wafers.

  stihit->setZ( svthit->position().z() );
  stihit->setX( svthit->position().x() * cos(dRefAngle) +
                svthit->position().y() * sin(dRefAngle) );
  stihit->setY(-svthit->position().x() * sin(dRefAngle) +
                svthit->position().y() * cos(dRefAngle) );

  // find detector for this hit
  char szBuf[100];
  sprintf(szBuf, "Svg/Layer_%d/Ladder_%d/Ladder", (int) svthit->layer(),
          (int) (svthit->ladder() + 1)/2);
  StiDetector* layer = StiDetectorFinder::instance()->findDetector(szBuf);
  if (!layer) {
      *(Messenger::instance(MessageType::kGeometryMessage)) <<"Error, no detector for layer "<<svthit->layer()<<"\tladder: "<<svthit->ladder()<<"\tABORT"<<endl;
      return;
  }
  stihit->setDetector( layer );
}
void StiGeometryTransform::operator() (const StiHit* stihit, StSvtHit* svthit){
}

void StiGeometryTransform::operator() (const StSsdHit* ssdhit, StiHit* stihit){
  
  //We'll temporarily keep
  stihit->setStHit(const_cast<StSsdHit*>(ssdhit));

  // first the position & ref angle
  int nLadders = svgConfig.n_ladder[6];  // ssd is svg layer 7 (0-indexed)
  double dRefAngle = phiForSector( ssdhit->ladder(), nLadders );
  double dPosition = mpadrowradiusmap[7]; // this is 1-indexed
  stihit->setRefangle( dRefAngle );
  stihit->setPosition( dPosition );

  // z is fine, we just need to do a 2d rotation so that x' is radially outward
  // from the wafer center.  Note that since the ssd ladders have a nonzero
  // orientation angle, this is _not_ the normal to the wafers.

  stihit->setZ( ssdhit->position().z() );
  stihit->setX( ssdhit->position().x() * cos(dRefAngle) +
                ssdhit->position().y() * sin(dRefAngle) );
  stihit->setY(-ssdhit->position().x() * sin(dRefAngle) +
                ssdhit->position().y() * cos(dRefAngle) );

  // find detector for this hit
  StiDetectorFinder *pFinder = StiDetectorFinder::instance();
  char szBuf[100];
  sprintf(szBuf, "Svg/Layer_7/Ladder_%d/Ladder", (int) ssdhit->ladder());
  stihit->setDetector( pFinder->findDetector(szBuf) );
}

void StiGeometryTransform::operator() (const StiHit* stihit, StSsdHit* ssdhit){
}

void StiGeometryTransform::operator() (const StiKalmanTrackNode *pTrackNode,
                                       StHelix *pHelix){
	/*
  *(Messenger::instance(MessageType::kGeometryMessage)) << "StiKalmanTrackNode: x=" << pTrackNode->fX 
       << ", alpha=" << pTrackNode->fAlpha
       << ", y=" << pTrackNode->fP0
       << ", z=" << pTrackNode->fP1
       << ", c*x0=" << pTrackNode->fP2
       << ", c=" << pTrackNode->fP3
       << ", tanL=" << pTrackNode->fP4 << endl;
	*/
  // first, calculate the helix origin in global coords
  StThreeVector<double> origin(pTrackNode->fX, pTrackNode->fP0,
                               pTrackNode->fP1);
  /**(Messenger::instance(MessageType::kGeometryMessage)) << "Before rotation: x=" << origin.x()
       << ", y=" << origin.y()
       << ", z=" << origin.z() << endl;
	*/
  origin.rotateZ(pTrackNode->fAlpha);
  
	/**(Messenger::instance(MessageType::kGeometryMessage)) << "After rotation: x=" << origin.x()
       << ", y=" << origin.y()
       << ", z=" << origin.z() << endl;
	*/
  // dip angle & curvature easy
  *(Messenger::instance(MessageType::kGeometryMessage)) << "tanDip=" << pTrackNode->fP4 << endl;
  double dDip = atan(pTrackNode->fP4);
  *(Messenger::instance(MessageType::kGeometryMessage)) << "dip=" << dDip << endl;
  double dCurvature = pTrackNode->fP3;

  // now calculate azimuthal angle of the helix origin wrt the helix axis
  // in _local_ coordinates.
  double dDeltaX = pTrackNode->fX - pTrackNode->fP2/dCurvature;
  double dDeltaY = sqrt(1./(dCurvature*dCurvature) - dDeltaX*dDeltaX) *
      (dCurvature>0 ? -1 : 1); // sign(curvature) == -sign(Y-Y0)
  //*(Messenger::instance(MessageType::kGeometryMessage)) << "deltaX=" << dDeltaX << ", deltaY=" << dDeltaY << endl;
  double dPhi = atan2( dDeltaY, dDeltaX); // in [0,2pi]
  // now change to global coords
  dPhi -= pTrackNode->fAlpha;
  while(dPhi<0.){      dPhi += 2.*M_PI; };
  while(dPhi>2.*M_PI){ dPhi -= 2.*M_PI; };
  //*(Messenger::instance(MessageType::kGeometryMessage)) << "phi=" << dPhi << endl;

  // finally, need the sense of rotation.  Here we need the fact that
  // the track model assumes outward tracks (positive local x coord of mtm).
  int iH = dCurvature>0 ? 1 : -1;

  pHelix->setParameters( fabs(dCurvature), dDip, dPhi, origin, iH );

	/*
  *(Messenger::instance(MessageType::kGeometryMessage)) << "StHelix: x0=" << pHelix->x(0)
       << ", y0=" << pHelix->y(0)
       << ", z0=" << pHelix->z(0)
       << ", c=" << pHelix->curvature()
       << ", phi0=" << pHelix->phase()
       << ", dip=" << pHelix->dipAngle()
       << ", h=" << pHelix->h() << endl;
	*/
}

double StiGeometryTransform::positionForTpcPadrow(int padrow) const{

  padrow_radius_map::const_iterator where = mpadrowradiusmap.find(padrow+100);
  if (where==mpadrowradiusmap.end()) {
    *(Messenger::instance(MessageType::kGeometryMessage)) <<"StiGeometryTransform::angleAndPosition(). " <<
        "ERROR:\tpadrow not found"<<endl;
  }

  return (*where).second;

} // positionForTpcPadrow

pair<double, double> StiGeometryTransform::angleAndPosition(const StTpcHit *pHit) const
{
  double dRefAngle = phiForTpcSector( pHit->sector() );
  double dPosition = positionForTpcPadrow( pHit->padrow() );

  return pair<double, double>(dRefAngle, dPosition);
} // angleAndPosition


void StiGeometryTransform::operator() (const StGlobalTrack* st, StiKalmanTrack* sti,
				       unsigned int maxHits, const StTpcHitFilter* filter) const
{
    *(Messenger::instance(MessageType::kGeometryMessage)) <<"\n\n\nmaxHits:\t"<<maxHits<<"\tfilter"<<filter<<endl;
    
    //now get hits
    StPtrVecHit hits = st->detectorInfo()->hits(kTpcId);
    sort( hits.begin(), hits.end(), StHitRadiusGreaterThan() );
    hitvector hitvec;
    
    for (vector<StHit*>::iterator it=hits.begin();
	 it!=hits.end() && hitvec.size()<=maxHits; ++it) {
	StTpcHit* hit = dynamic_cast<StTpcHit*>(*it);
	if (!hit) {
	    *(Messenger::instance(MessageType::kGeometryMessage)) <<"StiGeometryTransform::operator(GlobalTrack->KalmanTrack). Error:\t";
	    *(Messenger::instance(MessageType::kGeometryMessage)) <<"StHit->StTpcHit cast failed.  Skip this point."<<endl;
	}
	else  {
	    //Find StiHit for this StHit
	    bool passedFilter=true;
	    if (filter!=0) 
		passedFilter = filter->operator()(*hit);
	    
	    if (passedFilter) {
		pair<double, double> myPair =
		    StiGeometryTransform::instance()->angleAndPosition(hit);
		double refAngle=myPair.first;
		double position=myPair.second;
		
		const hitvector& stiHits = StiHitContainer::instance()->hits(refAngle, position);
		if (stiHits.size()==0) 	{
		    *(Messenger::instance(MessageType::kGeometryMessage)) <<"Error, no StiHits for this sector, padrow"<<endl;
		    sti=0;
		    return;
		}
		SameStHit mySameStHit;
		mySameStHit.stHit = hit;
		hitvector::const_iterator where = find_if(stiHits.begin(), stiHits.end(),
							  mySameStHit);
		if (where==stiHits.end()) {
		    *(Messenger::instance(MessageType::kGeometryMessage)) <<"Error, no StiHit with this StHit was found"<<endl;
		    sti=0;
		    return;
		}
		else {
		    hitvec.push_back(*where);
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
    // *(Messenger::instance(MessageType::kGeometryMessage)) <<"StiGeometryTransform:  curvature: "<<curvature<<endl;
    
    double tanLambda = tan(sthelix.dipAngle());
	    
    // *(Messenger::instance(MessageType::kGeometryMessage)) <<"tanLambda: "<<tanLambda<<endl;

    if (hitvec.size()<3) {
	*(Messenger::instance(MessageType::kGeometryMessage)) << " Track Transform Error:\t"
						 <<" hitvec.size()<3.  Abort"<<endl;
	return;
    }
    //int size = hitvec.size();

    //StiHelixCalculator calc;
    //Use the outer 3 hits
    //const StThreeVectorF& outside = hitvec[size-3]->globalPosition();
    //const StThreeVectorF& middle = hitvec[size-2]->globalPosition();
    //const StThreeVectorF& inside = hitvec[size-1]->globalPosition();

    //Try 3 hits over a wide distance:
    //const StThreeVectorF& outside = hitvec.front()->globalPosition();
    //const StThreeVectorF& inside = hitvec.back()->globalPosition();
    //const StThreeVectorF& middle = (*(hitvec.begin()+hitvec.size()/2))->globalPosition();
        
    //calc.calculate( StThreeVector<double>(inside.x(),  inside.y(),  inside.z()),
    //	    StThreeVector<double>(middle.x(),  middle.y(),  middle.z()),
    //	    StThreeVector<double>(outside.x(), outside.y(), outside.z()) );
    
    // *(Messenger::instance(kSeedFinderMessage)) <<"origin: "<<calc.xCenter()<<" "
    //				       <<calc.yCenter()<<" "
    //				       <<calc.z0()<<" "
    //				       <<" curvature: "<<calc.curvature()<<" "
    //				       <<" tanLambda: "<<calc.tanLambda()<<endl;
    
    // *(Messenger::instance(kSeedFinderMessage)) <<"origin: "<<stiGlobalOrigin
    //				       <<" curvature: "<<curvature
    //				       <<" tanLambda: "<<tanLambda<<endl;
    //Test the calculator
    //sti->initialize( calc.curvature(), calc.tanLambda(),
    //	     StThreeVectorD(calc.xCenter(), calc.yCenter(), 0.),
    //	     hitvec);
    
    sti->initialize(curvature, tanLambda, stiGlobalOrigin, hitvec);
    
    //Test the fitter
    //StiHelixFitter* fitter = StiHelixFitter::instance();
    //bool worked = StiHelixFitter::instance()->fit(hitvec);
    // *(Messenger::instance(kSeedFinderMessage)) <<*(StiHelixFitter::instance())<<endl;
    
    //StThreeVectorF fitOrigin(fitter->xCenter(), fitter->yCenter(), fitter->z0());
    //sti->initialize(fitter->curvature(), fitter->tanLambda(), fitOrigin, hitvec);
    
}
