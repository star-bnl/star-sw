//  17 may 01
//  ben norman

//Std
#include <math.h>

//SCL
#include "StThreeVector.hh"
#include "StPhysicalHelix.hh"
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

//Svt Tables

//XX#include "tables/St_svg_geom_Table.h"
//XX#include "tables/St_svg_config_Table.h"
//XX#include "tables/St_svg_shape_Table.h"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"

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

//XX    St_DataSetIter local(StiMaker::instance()->GetInputDB("svt"));
    //*(Messenger::instance(MessageType::kGeometryMessage)) <<"Instantiated local"<<endl;

//XX    svgConfig = 
//XX	dynamic_cast<St_svg_config *>(local("svgpars/config"))->GetTable()[0];
    //*(Messenger::instance(MessageType::kGeometryMessage)) <<"Instantiated svgConfig"<<endl;

//XX    aSvgGeom = dynamic_cast<St_svg_geom *>(local("svgpars/geom"))->GetTable();
    //*(Messenger::instance(MessageType::kGeometryMessage)) <<"Instantiated aSvgGeom"<<endl;

//XX    aSvgShape = dynamic_cast<St_svg_shape *>(local("svgpars/shape"))->GetTable();
    //*(Messenger::instance(MessageType::kGeometryMessage)) <<"Instantiated aSvgShape"<<endl;
    
    StSvtDbMaker *pDbMaker = (StSvtDbMaker*) 
        StiMaker::instance()->GetMaker("svtDb");
    St_SvtDb_Reader *pDbReader = pDbMaker->get_SvtDb_Reader();
    m_pSvtGeometry = pDbReader->getGeometry();
    m_pSvtConfig = pDbReader->getConfiguration();

    //*(Messenger::instance(MessageType::kGeometryMessage)) <<"instantiate TPC coord x-form"<<endl;
    tpcTransform = new StTpcCoordinateTransform(gStTpcDb);

    *(Messenger::instance(MessageType::kGeometryMessage)) <<"Generating Padrow Radius Map"<<endl;

    // store svt as padrows 1-6
    for (unsigned int padrow=1; padrow<=6; ++padrow){
      double center = centerForSvtLadder(padrow, 2 - padrow%2).perp();
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

void StiGeometryTransform::setStiHitError(const StHit* stHit, StiHit* stiHit, double theta)
{
    //For now, set all hit errors to fixed values:

    stiHit->setSxx(1.E-6); //cm
    stiHit->setSxy(1.E-6); //cm
    stiHit->setSyz(1.E-6); //cm
    stiHit->setSyy(150.E-4); //cm
    stiHit->setSzz(300.E-4); //cm
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
  while(dPhi >=  M_PI){ dPhi -= 2.*M_PI; }
  while(dPhi <  -M_PI){ dPhi += 2.*M_PI; }

  return dPhi;
    
} // phiForWestSector

// as above, but numbering _increases_ with increasing phi.
double StiGeometryTransform::phiForEastSector(int iSector, int nSectors) const
{
    
    int offset = 3*nSectors/4;
    double deltaPhi = 2.*M_PI/nSectors;

    // correct offset
    double dPhi = (iSector - offset)*deltaPhi;
    while(dPhi >=  M_PI){ dPhi -= 2.*M_PI; }
    while(dPhi <  -M_PI){ dPhi += 2.*M_PI; }

    return dPhi;

} // phiForEastSector

int StiGeometryTransform::westSectorForPhi(double phi, int nSectors) const
{
    int offset = nSectors/4;
    double deltaPhi = 2.*M_PI/nSectors;  
    
    int iSector = 0;
    while(phi >  deltaPhi/2.){ phi -= deltaPhi; iSector++; }
    while(phi < -deltaPhi/2.){ phi += deltaPhi; iSector--; }
    
    iSector = offset - iSector;
    if(iSector<1){ iSector += nSectors; }
    
    return iSector;
    
} // westSectorForPhi

int StiGeometryTransform::eastSectorForPhi(double phi, int nSectors) const
{
    
    int offset = nSectors/4;
    double deltaPhi = 2.*M_PI/nSectors;  
    
    int iSector = 0;
    while(phi >  deltaPhi/2.){ phi -= deltaPhi; iSector++; }
    while(phi < -deltaPhi/2.){ phi += deltaPhi; iSector--; }
    
    iSector = iSector + (2*nSectors + offset);
    if(iSector>2*nSectors){ iSector -= nSectors; }
    
    return iSector;
    
} // eastSectorForPhi

// returns a vector (with z==0) pointing from the origin to  the center of the given padrow or ladder.((St_db_Maker*) GetMaker("db")
StThreeVector<double> StiGeometryTransform::centerForTpcPadrow(int sector, int padrow) const
{
    double radius = gStTpcDb->PadPlaneGeometry()->radialDistanceAtRow(padrow);
    double phi = phiForSector(sector, 12);
    
    return StThreeVector<double>(radius*cos(phi), radius*sin(phi), 0.);
}

// this uses the database convention of 6 svt layers == 3 svt barrels.
StThreeVector<double> StiGeometryTransform::centerForSvtLadder(int barrel, int ladder) const
{
  // "Barrel" here really means Layer (1-6)
    double radius = m_pSvtGeometry->getBarrelRadius(2*barrel - 1 + ladder%2);
  // "Barrel" here means Barrel (1-3)
    int nLadders = m_pSvtConfig->getNumberOfLadders(barrel);
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

int StiGeometryTransform::barrelForSvtCoords(const StThreeVector<double> &vec) const{
  return (layerAndLadderForSvtCoords(vec).first - 1)/2;
}

pair<int,int> StiGeometryTransform::layerAndLadderForSvtCoords(const StThreeVector<double> &vec) const
{
  int iLayer = 0, iLadder=0;
  int nBarrels = m_pSvtConfig->getNumberOfBarrels();
  for(int iBarrel = 1; iBarrel <= nBarrels; iBarrel++){

    // first, rotate to local coordinates
    int nLadders = m_pSvtConfig->getNumberOfLadders(iBarrel);
    double dRadius1 = m_pSvtGeometry->getBarrelRadius(2*iBarrel - 1);
    double dRadius2 = m_pSvtGeometry->getBarrelRadius(2*iBarrel);
    StThreeVector<double> vecLocal(vec);
    double dDeltaPhi = 2.*M_PI/nLadders;
    iLadder = nLadders/4;
    while(vecLocal.phi()>  dDeltaPhi/2.){ 
      vecLocal.rotateZ(-dDeltaPhi); 
      iLadder--;
    }
    while(vecLocal.phi()<=-dDeltaPhi/2.){ 
      vecLocal.rotateZ( dDeltaPhi); 
      iLadder++;
    }

    // this 0.44 is a threshhold based on the minimum difference in radii
    // between 2 layers in the same barrel is 0.89 (layers 3 & 4).
    if(fabs(vecLocal.x() - dRadius1)<0.44){
      iLayer = 2*iBarrel - 1;
      if((iLadder + iLayer)%2==0){
        // case when ladder overlap puts the point in another svt "sector"
        iLadder += (vecLocal.phi()>0 ? -1 : 1);
      }
      if(iLadder<1){        iLadder += nLadders; }
      if(iLadder>nLadders){ iLadder -= nLadders; }
      break;
    }else if(fabs(vecLocal.x() - dRadius2)<0.44){
      iLayer = 2*iBarrel;
      if((iLadder + iLayer)%2==0){
        // case when ladder overlap puts the point in another svt "sector"
        iLadder += (vecLocal.phi()>0 ? -1 : 1);
      }
      if(iLadder<1){        iLadder += nLadders; }
      if(iLadder>nLadders){ iLadder -= nLadders; }
      break;
    }
  }

  return pair<int, int>(iLayer, iLadder);
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

    //if ( (refangle>=-1.*M_PI && refangle<=M_PI)==false ) {
    //cout <<"Tpc Hit transform error: refangle not [-pi,pi]"<<endl;
    //}

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

    
    //Now Transform Errors by hand (using matrix is too slow).

    setStiHitError(tpchit, stihit, refangle);
    
    //StMatrixF covMatrix = tpchit->covariantMatrix();
    //stihit->setError( gCovarianceRotation( covMatrix, stihit->refangle() ) );
    
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
  pair<int, int> layerAndLadder = layerAndLadderForSvtCoords(position);
  int iLayer = layerAndLadder.first;
  int iLadder = layerAndLadder.second;
  int iBarrel = (iLayer + 1)/2;

//  if(iLayer!=svthit->layer() || iLadder!=svthit->ladder()){
//    cout << "SvtHit [perp, phi]=[" << position.perp() << ", "
//         << position.phi() << "], layer=" << svthit->layer()
//         << ", ladder=" << svthit->ladder() << endl;
//  }

  // first the position & ref angle
  int nLadders = m_pSvtConfig->getNumberOfLadders(iBarrel);
  double dRefAngle = phiForSector( iLadder, nLadders );
  double dPosition = m_pSvtGeometry->getBarrelRadius(iLayer);
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

/* //XX  
  //We'll temporarily keep
  stihit->setStHit(const_cast<StSsdHit*>(ssdhit));

  // first the position & ref angle
  StThreeVector<double> position(ssdhit->position().x(),
                                 ssdhit->position().y(),
                                 ssdhit->position().z());
  int nLadders = svgConfig.n_ladder[6];  // ssd is svg layer 7 (0-indexed)
  int iLadder = ladderForSvgCoords(position);
  double dRefAngle = phiForSector( iLadder, nLadders );
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
*/
}

void StiGeometryTransform::operator() (const StiHit* stihit, StSsdHit* ssdhit){
}

void StiGeometryTransform::operator() (const StiKalmanTrackNode *pTrackNode,
                                       StPhysicalHelix *pHelix){
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

  // thanks to StPhysicalHelix constructor, this is easy.

  // first, calculate the helix origin in global coords
  StThreeVector<double> origin(pTrackNode->fX, pTrackNode->fP0,
                               pTrackNode->fP1);
  origin.rotateZ(pTrackNode->fAlpha);
  
  // now get momentum at that point
  double adMomentum[3];
  pTrackNode->getMomentum(adMomentum);
  StThreeVector<double> momentum(adMomentum[0], adMomentum[1], adMomentum[2]);
  momentum.rotateZ(pTrackNode->fAlpha);
  
  // magnetic field and charge
  double dField = StiKalmanTrackNode::getFieldConstant();
  double dCharge = (dField*pTrackNode->fP3 > 0) ? -1. : 1.;

  *pHelix = StPhysicalHelix(momentum*GeV, origin*GeV, 
                            dField*tesla, dCharge*eplus);

/* This is the old way, gives same results:

  // dip angle & curvature easy
  // *(Messenger::instance(MessageType::kGeometryMessage)) 
  //<< "tanDip=" << pTrackNode->fP4 << endl;
  double dDip = atan(pTrackNode->fP4);
  // *(Messenger::instance(MessageType::kGeometryMessage))
  //<< "dip=" << dDip << endl;
  double dCurvature = fabs(pTrackNode->fP3);
  int iH = (pTrackNode->fP3 > 0) ? 1 : -1;

  // now calculate azimuthal angle of the helix origin wrt the helix axis
  // in _local_ coordinates.
  double dDeltaX = pTrackNode->fX - pTrackNode->fP2/pTrackNode->fP3;
  double dDeltaY = -sqrt(1./(dCurvature*dCurvature) - dDeltaX*dDeltaX) * iH;
  *(Messenger::instance(MessageType::kGeometryMessage))
      << "deltaX=" << dDeltaX << ", deltaY=" << dDeltaY << endl;
  double dPhi = atan2( dDeltaY, dDeltaX); // in [-pi,pi]
  // now change to global coords
  dPhi -= pTrackNode->fAlpha;
  while(dPhi <  -M_PI){ dPhi += 2.*M_PI; };
  while(dPhi >=  M_PI){ dPhi -= 2.*M_PI; };
  // *(Messenger::instance(MessageType::kGeometryMessage)) << "phi=" << dPhi << endl;

  StHelix *pHelix2 = new StHelix( fabs(dCurvature), dDip, dPhi, origin, iH );

  *(Messenger::instance(MessageType::kGeometryMessage)) 
      << "StHelix:         origin=" << pHelix2->origin() << endl
      << "StPhysicalHelix: origin=" << pHelix->origin() << endl
      << "StHelix:         curvature=" << pHelix2->curvature() << endl
      << "StPhysicalHelix: curvature=" << pHelix->curvature() << endl
      << "StHelix:         phase=" << pHelix2->phase() << endl
      << "StPhysicalHelix: phase=" << pHelix->phase() << endl
      << "StHelix:         dipAngle=" << pHelix2->dipAngle() << endl
      << "StPhysicalHelix: dipAngle=" << pHelix->dipAngle() << endl
      << "StHelix:         h=" << pHelix2->h() << endl
      << "StPhysicalHelix: h=" << pHelix->h() << endl;
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
