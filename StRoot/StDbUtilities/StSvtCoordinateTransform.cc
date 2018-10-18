/***********************************************************************
 *
 * $Id: StSvtCoordinateTransform.cc,v 1.44 2018/10/17 20:45:24 fisyak Exp $
 *
 * Author: Helen Caines April 2000
 *
 ***********************************************************************
 * Description:
 *
 * Geometrical transformation Routines for:
 * Raw Wafer Coordinate  <-->  Local Coordinate
 * Local Coordinate  <-->  Global Coordinate
 *
 * These Routines deal positions ONLY!
 *
 *
 ***********************************************************************/
#include "StTpcDb/StTpcDb.h" //Final transformation to mag. coords
#include "StSvtCoordinateTransform.hh"
#include "StCoordinates.hh"        // coordinate definitions
#include "StGlobals.hh"
#include "StMessMgr.h"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StSvtClassLibrary/StSvtWaferGeometry.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtHybridDriftVelocity.hh"
#include "StSvtClassLibrary/StSvtHybridDriftCurve.hh"
#include "StSvtClassLibrary/StSvtT0.hh"
#include <unistd.h>
#include "TF1.h"
#include "TString.h"
#include "TMath.h"
#include "St_svtCorrectionC.h"
#include "St_svtHybridDriftVelocityC.h"
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using namespace units;
#endif
static Int_t _debug = 0;

//_____________________________________________________________________________
StSvtCoordinateTransform::StSvtCoordinateTransform() {

  mFlag=0;
  mDeltaDriftVelocity = 1;
  mPoly9 = new TF1("mPoly9","pol9(0)",0.0,6.0);
}

//_____________________________________________________________________________

StSvtCoordinateTransform::StSvtCoordinateTransform(StTpcDb* /* gStTpcDb */) {
  mFlag=0;
  mPoly9=0;
}
//_____________________________________________________________________________

StSvtCoordinateTransform::~StSvtCoordinateTransform()
{
  SafeDelete(mPoly9);
 if (mFlag&1) delete mgeom; mgeom=0;
}

//_____________________________________________________________________________
void StSvtCoordinateTransform::setParamPointers( srs_srspar_st* param,
						 svg_geom_st* geom, 
						 svg_shape_st* shape,
						 StSvtConfig* config,
						 StSvtHybridCollection* driftVeloc,
						 StSvtT0* T0){
  mFlag=0;
  mgeom = new StSvtGeometry(param, geom, shape);
  mFlag |=1;
  mconfig = config;
  mDriftVelocity = 0;//YF  driftVeloc;
  mDriftCurve = NULL;
  mT0 = T0;
  mdriftVelCorr = 0; 
}
//_____________________________________________________________________________
void StSvtCoordinateTransform::setParamPointers( srs_srspar_st* param,
						 svg_geom_st* geom, 
						 svg_shape_st* shape,
						 StSvtConfig* config,
						 StSvtHybridCollection* driftVeloc,
						 StSvtHybridCollection* driftCurve,
						 StSvtT0* T0){

  
  mFlag=0;
  mgeom = new StSvtGeometry(param, geom, shape);
  mFlag |=1;
  mconfig = config;
  mDriftVelocity = 0;//YF driftVeloc;
  mDriftCurve = 0;//YF  driftCurve;
  mT0 = T0;
  mdriftVelCorr = 0; 
}
//_____________________________________________________________________________
void StSvtCoordinateTransform::setVelocityScale( double deltaV){
 
  mDeltaDriftVelocity = deltaV;
}
//____________________________________________________________________________
void StSvtCoordinateTransform::setParamPointers( StSvtGeometry* geom,
						 StSvtConfig* config,
						 StSvtHybridCollection* driftVeloc,
						 StSvtT0* T0){
  mgeom = geom;
  mconfig = config;
  mDriftVelocity = 0; //driftVeloc;
  mDriftCurve = NULL;
  mT0 = T0;
  mdriftVelCorr = 0; 

}
//____________________________________________________________________________
void StSvtCoordinateTransform::setParamPointers( StSvtGeometry* geom,
						 StSvtConfig* config,
						 StSvtHybridCollection* driftVeloc,
						 StSvtHybridCollection* driftCurve,
						 StSvtT0* T0){
  mgeom = geom;
  mconfig = config;
  mDriftVelocity = 0;//YF  driftVeloc;
  mDriftCurve = 0;//YF driftCurve;
  mT0 = T0;
  mdriftVelCorr = 0; 

}

//____________________________________________________________________________
void StSvtCoordinateTransform::setParamPointers( StSvtGeometry* geom,
						 StSvtConfig* config,
						 StSvtHybridCollection* driftVeloc,
						 StSvtHybridCollection* driftCurve,
						 StSvtT0* T0, 
						 St_svtCorrectionC*  driftVelCorr){
  mgeom = geom;
  mconfig = config;
  mDriftVelocity = 0;//YF driftVeloc;
  mDriftCurve = 0;//YF driftCurve;
  mT0 = T0;
  mdriftVelCorr = driftVelCorr; 

}

//_____________________________________________________________________________
//      Raw Data          -->  Global Coordinate

void StSvtCoordinateTransform::operator()(const StSvtWaferCoordinate& a, StGlobalCoordinate& c)
{
  StSvtLocalCoordinate b;


  this->operator()(a,b);
  this->operator()(b,c);

  return;

}
//_____________________________________________________________________________

//     Global Coordinate  --> Raw data

void StSvtCoordinateTransform::operator()(const StGlobalCoordinate& a, StSvtWaferCoordinate& c, Int_t Id)
{

  StSvtLocalCoordinate b;

 
  this->operator()(a,b, Id);

  this->operator()(b,c);
  return;

}
//_____________________________________________________________________________

//      Raw Data          -->  SVT Local  Coordinate
void StSvtCoordinateTransform::operator()(const StSvtWaferCoordinate& a, StSvtLocalCoordinate& b)

{
  b.setLayer(a.layer());
  b.setLadder(a.ladder());
  b.setWafer(a.wafer());
  b.setHybrid(a.hybrid());

  St_svtHybridDriftVelocityC *d = St_svtHybridDriftVelocityC::instance();
  b.setPosition(StThreeVector<double>(d->CalcU(a.barrel(),a.ladder(),a.wafer(),a.hybrid(),a.timebucket(),a.anode()),
				      d->CalcV(a.hybrid(),a.anode()),
				      0.0));
  return;
}
//_____________________________________________________________________________


// SVT Local  Coordinate  -->   Raw coordinate

void StSvtCoordinateTransform::operator()(const StSvtLocalCoordinate& a, StSvtWaferCoordinate& b)
{
  //StThreeVector<double> pos(0,0,0);

  b.setLayer(a.layer());
  b.setLadder(a.ladder());
  b.setWafer(a.wafer());
  b.setHybrid(a.hybrid());
  
  //  int idShape = 0;
  St_svtHybridDriftVelocityC *d = St_svtHybridDriftVelocityC::instance();
  b.setTimeBucket(-99);
  b.setAnode(-99);
  if (d) {
    b.setTimeBucket(d->UnCalcU(b.barrel(),b.ladder(),b.wafer(),b.hybrid(),a.position().x()));
    b.setAnode(d->UnCalcV(b.hybrid(),a.position().y()));
  }
  return;

}

//_____________________________________________________________________________
//  Svt Local  --> Global
 
void StSvtCoordinateTransform::operator()(const StSvtLocalCoordinate& a, StGlobalCoordinate& b)
{

  StThreeVector<double> x(0,0,0);
  
  LocaltoGlobal(a, x, -1);

  b.setPosition(x);
		 

}

//_____________________________________________________________________________

// Svt Global --> Local

void StSvtCoordinateTransform::operator()(const StGlobalCoordinate& a,  StSvtLocalCoordinate& b, Int_t Id)
{
  // Id = 10000*shell + 1000*layer + 100*wafer + ladder;
  Int_t id = Id%10000;
  Int_t layer = (id/1000);
  //  Int_t barrel = (layer - 1)/2 + 1;
  Int_t ladder = id%100;
  Int_t wafer  = (id - 1000*layer)/100;
  b.setLayer(layer);
  b.setLadder(ladder);
  b.setWafer(wafer);
  GlobaltoLocal( a.position(), b, Id, -1);
  b.setHybrid(2);
  if( b.position().x() < 0) b.setHybrid(1);
}

//_____________________________________________________________________________

int StSvtCoordinateTransform::LocaltoGlobal(const StSvtLocalCoordinate& a, StThreeVector<double>& x, int index){
/***********************************************************************/
/*                       TRANSFORMATION ROUTINES                       */


  //     DESCRIPTION:  local to global mapping for svt points

  //    Input Arguments:
  //    xp    : local coordinate
  //   geom->x    : wafer origin
  //   geom->d    : wafer drift direction
  //   geom->t    : wafer transverse direction
  //   geom->n    : wafer normal

  //   Output Arguments:
  //    x     : global coordinate

  //   Functional Description:  
  //   Make a local to global mapping by using the origin of the wafer coordinate and the
  //  3 vectors defining the normal to the plane, the drift direction, and the transverse direction.
  //     xp[0]  : component in the drift direction
  //     xp[1]  : component in the transverse direction
  //     xp[2]  : component in the direction normal to the wafer; should be small ...

  //     Error Conditions: none


  if( index < 0){
    index = mgeom->getWaferIndex(mgeom->getBarrelID(a.layer(),a.ladder()),(int)a.ladder(),(int)a.wafer());

  }
  StSvtWaferGeometry *waferGeom = 0;
  if (index >= 0 && index < 216)
    waferGeom = (StSvtWaferGeometry*)mgeom->at(index);
  if (!waferGeom) {
    x.setX(-999);
    x.setY(-999);
    x.setZ(-999);
    return 0;
  }
  if (_debug) waferGeom->print();
      
  Double_t xl[3] = {a.position().x(), a.position().y(), a.position().z()};
  if (_debug) cout << "xl \t" << xl[0] << "\t" << xl[1] << "\t" << xl[2] << endl;
  Double_t xg[3];
  waferGeom->LocalToMaster(xl,xg);
  if (_debug) cout << "xg \t" << xg[0] << "\t" << xg[1] << "\t" << xg[2] << endl;
  x.setX(xg[0]); x.setY(xg[1]); x.setZ(xg[2]); 
  if (_debug) cout << "x \t" << x.x() << "\t" << x.y() << "\t" << x.z() << endl;
  
  return index;
}

//___________________________________________________________________________

int StSvtCoordinateTransform::GlobaltoLocal( const StThreeVector<double>& x, StSvtLocalCoordinate& b , int HardWarePos, int index  )
{

/*     DESCRIPTION:   global to local mapping for svt points
       c     Input Arguments:
       c       x     : global coordinate
       c       geom->x    : wafer origin
       c       geom->d    : wafer drift direction
       c       geom->t    : wafer transverse direction
       c       geom->n    : wafer normal
       c
       c     Output Arguments:
       c       xp    : local coordinate
       c
       c     Functional Description: 
       c      Make a global to local mapping by using the origin of the wafer coordinate and the
       c     3 vectors defining the normal to the plane, the drift direction, and the transverse direction.
       c     xp.x  : component in the drift direction
       c     xp.y  : component in the transverse direction
       c     xp.z  : component in the direction normal to the wafer; should be small ...
       c
       c     Created 1-Nov-93  C. Pruneau, WSU
       c
       c     Error Conditions: none
       c
*/
  
  //     Executable Code
  //     ===============
  
  
  StSvtWaferGeometry* waferGeom = NULL;

  if( index < 0){
    index = mgeom->getWaferIndex(HardWarePos%10000);
  }

  if (index >= 0 && index<216)
    waferGeom = (StSvtWaferGeometry*)mgeom->at(index);
  //cout << "HardWarePos = " << HardWarePos << ", index = " << index << ", waferGeom = " << waferGeom << endl;

  if (!waferGeom) {
    b.position().setX(-999);
    b.position().setY(-999);
    b.position().setZ(-999);
    return 0;
  }
  
  
  //  xl[0] = x.x() - mgeom[index].x[0];
  //  xl[1] = x.y() - mgeom[index].x[1];
  //  xl[2] = x.z() - mgeom[index].x[2];
  
  //  b.position().setX(xl[0]*mgeom[index].d[0] + xl[1]*mgeom[index].d[1] + xl[2]*mgeom[index].d[2]);
  //  b.position().setY(xl[0]*mgeom[index].t[0] + xl[1]*mgeom[index].t[1] + xl[2]*mgeom[index].t[2]);
  //  b.position().setZ(xl[0]*mgeom[index].n[0] + xl[1]*mgeom[index].n[1] + xl[2]*mgeom[index].n[2]);

  Double_t xg[3] = {x.x(), x.y(), x.z()};
  Double_t xl[3];
  if (_debug) waferGeom->print();
  if (_debug) cout << "xg \t" << xg[0] << "\t" << xg[1] << "\t" << xg[2] << endl;
  
  waferGeom->MasterToLocal(xg,xl);
  if (_debug) cout << "xl \t" << xl[0] << "\t" << xl[1] << "\t" << xl[2] << endl;
  b.position().setX(xl[0]); b.position().setY(xl[1]); b.position().setZ(xl[2]); 
  if (_debug) cout << "x \t" << b.position().x() << "\t" << b.position().y() << "\t" << b.position().z() << endl;

  // cout << index << " " << b.position() <<  " " << waferGeom->x(0) << " " <<   waferGeom->x(1) << " " << waferGeom->x(2)  << "  " << x.x() << " "
//        << x.y() << " " << x.z() << endl;
  return index;
  
}

//_____________________________________________________________________________
//_____________________________________________________________________________

int StSvtCoordinateTransform::IsOnWaferZ(   const StThreeVector<double>& GlobalPosition,
					    int HardWarePos){

  //Find out for a given z coord and Hardware pos is it on the wafer

  StSvtLocalCoordinate LocalPosition;


  GlobaltoLocal(GlobalPosition, LocalPosition, HardWarePos, -1);

  return (fabs(LocalPosition.position().y()) <3.1525)? 1:0;
}

//_____________________________________________________________________________

int StSvtCoordinateTransform::IsOnWaferR(   const StThreeVector<double>& GlobalPosition,
					    int HardWarePos){

  //Find out for a given z coord and Hardware pos is it on the wafer

  StSvtLocalCoordinate LocalPosition;

  GlobaltoLocal(GlobalPosition, LocalPosition, HardWarePos, -1);


  if( fabs(LocalPosition.position().x()) < 3.1525    &&
      fabs(LocalPosition.position().z()) < 0.15  ) return 1;
  return 0;
}

