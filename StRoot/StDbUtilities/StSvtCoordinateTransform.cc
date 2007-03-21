/***********************************************************************
 *
 * $Id: StSvtCoordinateTransform.cc,v 1.39 2007/03/21 16:41:07 fisyak Exp $
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

StSvtCoordinateTransform::StSvtCoordinateTransform(StTpcDb* gStTpcDb) {
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
  double t0;
  if (mT0)
    t0=mT0->getT0();
  else
    t0 = 0;

  b.setLayer(a.layer());
  b.setLadder(a.ladder());
  b.setWafer(a.wafer());
  b.setHybrid(a.hybrid());

#if 0
  double t = a.timebucket() - t0;

  b.setPosition(StThreeVector<double>(CalcDriftLength(a,t),
				      CalcTransLength(a.anode()),0.0));
#else
  St_svtHybridDriftVelocityC *d = St_svtHybridDriftVelocityC::instance();
  b.setPosition(StThreeVector<double>(d->CalcDriftLength(a.barrel(),a.ladder(),a.wafer(),a.hybrid(),a.timebucket()),
				      d->CalcTransLength(a.anode()),
				      0.0));
#endif
  
  //  int idShape = 0;
  
  
  // Shift position because it is the North hybrid. Place relative to the center of the wafer, hybrid=1, anode 1=East
  if( a.hybrid() == 1 ){
    
    //    b.position().setX( b.position().x() - mshape[idShape].shape[0]);
    //    b.position().setY( b.position().y() - mshape[idShape].shape[1]);
    b.position().setX( b.position().x() - mgeom->getWaferLength());
    b.position().setY( b.position().y() - mgeom->getWaferWidth());
  }
  
  //Shift position because it is South hybrid. Place relative to the center of the wafer. Hybrid=2, anode 1=West
  
  
  if( a.hybrid() == 2){
    
    //    b.position().setX( mshape[idShape].shape[0] - b.position().x());
    //    b.position().setY( mshape[idShape].shape[1] - b.position().y());
    b.position().setX( mgeom->getWaferLength() - b.position().x());
    b.position().setY( mgeom->getWaferWidth() - b.position().y());
  }
#if 0
  if (mdriftVelCorr) {
    Double_t u = b.position().x();
    Double_t du = mdriftVelCorr->CalcCorrection(b.layer(),b.ladder(),b.wafer(),b.hybrid(),u);
    b.position().setX(u - du);
  }
#endif
  return;
}
//_____________________________________________________________________________


// SVT Local  Coordinate  -->   Raw coordinate

void StSvtCoordinateTransform::operator()(const StSvtLocalCoordinate& a, StSvtWaferCoordinate& b)
{
  double t0;
  if (mT0)
    t0=mT0->getT0();
  else
    t0 = 0;

  StThreeVector<double> pos(0,0,0);

  b.setLayer(a.layer());
  b.setLadder(a.ladder());
  b.setWafer(a.wafer());
  b.setHybrid(a.hybrid());
  
  //  int idShape = 0;
  
  
  // Shift position because it is the North hybrid. Local coords are placed relative to the center of the wafer, hybrid=1, anode 1=East so undo this
  if( a.hybrid() == 1 ){
    
    //   pos.setX( a.position().x() + mshape[idShape].shape[0]);
    //   pos.setY( a.position().y() + mshape[idShape].shape[1]);
    pos.setX( a.position().x() + mgeom->getWaferLength());
    pos.setY( a.position().y() + mgeom->getWaferWidth());
  }
  
  //Shift position because it is South hybrid. Local coords are placed relative to the center of the wafer. Hybrid=2, anode 1=West so undo this
  
  
  if( a.hybrid() == 2){
    
    //    pos.setX( mshape[idShape].shape[0] - a.position().x());
    //    pos.setY( mshape[idShape].shape[1] - a.position().y());
    pos.setX( mgeom->getWaferLength() - a.position().x());
    pos.setY( mgeom->getWaferWidth() - a.position().y());
  }
  

#if 0
  double t = UnCalcDriftLength(a,pos.x()) + t0;
  b.setTimeBucket(t);
  b.setAnode(UnCalcTransLength(pos.y()));
#else
  St_svtHybridDriftVelocityC *d = St_svtHybridDriftVelocityC::instance();
  b.setTimeBucket(d->UnCalcDriftLength(b.barrel(),b.ladder(),b.wafer(),b.hybrid(),pos.x()));
  b.setAnode(d->UnCalcTransLength(pos.y()));
#endif
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
  // Id = 1000*layer + 100*wafer + ladder;
  Int_t layer = Id/1000;
  //  Int_t barrel = (layer - 1)/2 + 1;
  Int_t ladder = Id%100;
  Int_t wafer  = (Id - 1000*layer)/100;
  if (! IsOnWaferR(a.position(),Id) ) {
    b.setWafer(-99);
    b.setLayer(-99);
    b.setLadder(-99);
    b.position().setX(-99);
    b.position().setY(-99);
    b.position().setZ(-99);
  }
  else{
    b.setLayer(layer);
    b.setLadder(ladder);
    b.setWafer(wafer);
    GlobaltoLocal( a.position(), b, Id, -1);
    b.setHybrid(2);
    if( b.position().x() < 0) b.setHybrid(1);
  }
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
#if 0  
  x.setX(waferGeom->x(0) + xl[0]*waferGeom->d(0) + xl[1]*waferGeom->t(0) + xl[2]*waferGeom->n(0)); 
  x.setY(waferGeom->x(1) + xl[0]*waferGeom->d(1) + xl[1]*waferGeom->t(1) + xl[2]*waferGeom->n(1));
  x.setZ(waferGeom->x(2) + xl[0]*waferGeom->d(2) + xl[1]*waferGeom->t(2) + xl[2]*waferGeom->n(2)); 
#else
  x.setX(xg[0]); x.setY(xg[1]); x.setZ(xg[2]); 
#endif
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
    index = mgeom->getWaferIndex(HardWarePos);
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
#if 0 
  xl[0] = x.x() - waferGeom->x(0);
  xl[1] = x.y() - waferGeom->x(1);
  xl[2] = x.z() - waferGeom->x(2);

  b.position().setX(xl[0]*waferGeom->d(0) + xl[1]*waferGeom->d(1) + xl[2]*waferGeom->d(2));
  b.position().setY(xl[0]*waferGeom->t(0) + xl[1]*waferGeom->t(1) + xl[2]*waferGeom->t(2));
  b.position().setZ(xl[0]*waferGeom->n(0) + xl[1]*waferGeom->n(1) + xl[2]*waferGeom->n(2));
#else
  b.position().setX(xl[0]); b.position().setY(xl[1]); b.position().setZ(xl[2]); 
#endif 
  if (_debug) cout << "x \t" << b.position().x() << "\t" << b.position().y() << "\t" << b.position().z() << endl;

  // cout << index << " " << b.position() <<  " " << waferGeom->x(0) << " " <<   waferGeom->x(1) << " " << waferGeom->x(2)  << "  " << x.x() << " "
//        << x.y() << " " << x.z() << endl;
  return index;
  
}

//_____________________________________________________________________________

double StSvtCoordinateTransform::CalcDriftLength(const StSvtWaferCoordinate& a, double x){

  //Gives drift distance of spt in cm in local coords from timebuckets


  // There is a smaller drift velocity in the focusing region 
  //  This has to be used when having only the average drift velocity across 
  //  the all detector 
       
  //  mparam->vd = average velocity from y=0 to y=3cm
  //   v2 = velocity in drift region ( y > ofoc cm)
  //   v1 = velocity in focusing region ( y < ofoc cm)
  //     a,b = parameters from Sanjeev's fit 

  //double aa,bb,cc,v2,v1,t;
  double distance;
  
  //double a=1.80;
  //double b=240000;
  // Correct value double d=0.27;
  //double d = 0.;
  //double l=mshape[0].shape[0];



//   aa=l/mparam->vd;
//   bb=(b*l/mparam->vd-l-a*d+d);
//   cc=(d*b-b*l);
//   v2=(-bb+::sqrt(::pow(bb,2)-4*aa*cc))/(2*aa);
//   v1=(v2+b)/a; /* using Sanjeev's fit from the bench */
  
//   v1=v1*0.8;  
  
//   t=x/mparam->fsca;
  
  
//   distance= (v1*t);
    
//   if (distance>d) distance=d+v2*(t-d/v1);
  
  // hard wired for the time being (07/29/2001) MM
  //float vd = 675000;
  //float fsca = 25000000;

  float fsca;
  if (mT0)
    fsca = mT0->getFsca();
  else
    fsca = 25000000;

  int barrel=-1;
  if (a.layer()>0) barrel = (a.layer()+1)/2;
  int ladder = a.ladder();
  int wafer = a.wafer();
  int hybrid = a.hybrid();

  float vd = -1;
  int index;

#if 0
  int anode;
  float td = -1;
  double Ratio = 1;
  if (mDriftVelocity && mDriftCurve) 
    {
      index = mDriftVelocity->getHybridIndex(barrel,ladder,wafer,hybrid);
      if (index >= 0 && mDriftVelocity->at(index))
	{
	  vd = ((StSvtHybridDriftVelocity*)mDriftVelocity->at(index))->getV3(1)*
	    mDeltaDriftVelocity;
	  td = 3.0*10e5 / vd;  // time for longest drift (3 cm)
	  if (a.anode()<=80)
	    anode=1;  // anode 40 fit
	  else if (a.anode()>80 && a.anode()<=160)
	    anode=2;  // anode 120 fit
	  else if (a.anode()>160)
	    anode=3;  // anode 200 fit
	  else
	    gMessMgr->Warning() << "Error: non-sensical anode number: " << x << endm;
	
// 	  cout << "Working on anode: " << a.anode() << endl;

	  for(Int_t j=1; j<=10; j++)
	    {
	      mPoly9->SetParameter(j-1,((StSvtHybridDriftCurve*)mDriftCurve->at(index))->getParameter(anode,j));
	      // gMessMgr->Info() << "got parameter (" << j-1 << "): " << mPoly9->GetParameter(j-1) << " for index " << index <<  endm;
	      
	    }
	  
	  double NewDist= mPoly9->Eval(td); // get bench distance at datas 3cm drift
	  Ratio = 3.0*10/(NewDist); // Richard stores in mm/Mus must fix that some time
	  distance = mPoly9->Eval(x*Ratio*10e5/fsca)/10;
	  //cout << " Full drift " << mPoly9->Eval(td*Ratio)/10 << " new distance " << distance << " original distance " <<  vd*x/fsca << endl;
	  if( TMath::Abs(distance-vd*x/fsca) > 0.03) {
	    //cout << " index = " << index << "Got crazy result using data drift vel " << 
	    // distance << " " << vd*x/fsca <<  endl;
	   
	    return vd*x/fsca;
	  }
	  //cout << "Got good result " << 
	  // distance << " index = " << index <<" " << vd*x/fsca <<  endl;
	  return distance;
	  
	}
    }
  else 
#endif
    if (mDriftVelocity) 
   {
      //       gMessMgr->Warning() << "mDriftCurve is NULL: " << x << endm;
      index = mDriftVelocity->getHybridIndex(barrel,ladder,wafer,hybrid);
      if (index >= 0 && mDriftVelocity->at(index))
	{
	  vd = ((StSvtHybridDriftVelocity*)mDriftVelocity->at(index))->getV3(1)*
	    mDeltaDriftVelocity;
	}
    
    }
  if (vd < 0)
    vd = 675000*mDeltaDriftVelocity;

  

  //cout << "index = " << index << ", vd = " << vd << endl;

  //distance = mparam->vd*x/mparam->fsca;
  distance = vd*x/fsca;
  return distance;
  
}

//_____________________________________________________________________________

double StSvtCoordinateTransform::UnCalcDriftLength(const StSvtLocalCoordinate& a, double x){

  //Gives drift distance of spt in timebuckets from cm in local coords



  // There is a smaller drift velocity in the focusing region 
  //  This has to be used when having only the average drift velocity across 
  //  the all detector 
       
  //  mparam->vd = average velocity from y=0 to y=3cm
  //   v2 = velocity in drift region ( y > ofoc cm)
  //   v1 = velocity in focusing region ( y < ofoc cm)
  //     a,b = parameters from Sanjeev's fit 

 //  double aa,bb,cc,v2,v1,t;
  
//   double a=1.80;
//   double b=240000;
//   //COrrect in principle double d=0.27;
//   double d=0;
//   double l=mshape[0].shape[0];



//   aa=l/mparam->vd;
//   bb=(b*l/mparam->vd-l-a*d+d);
//   cc=(d*b-b*l);
//   v2=(-bb+::sqrt(::pow(bb,2)-4*aa*cc))/(2*aa);
//   v1=(v2+b)/a; /* using Sanjeev's fit from the bench */
  
//   v1=v1*0.8;  
  
//   t= x/v1;

//   if (x>d) t = (x-d)/v2 + d/v1;

  // hard wired for the time being (07/29/2001) MM
  //float vd = 675000;
  //float fsca = 25000000;

  float fsca = (mT0)? mT0->getFsca():25000000;

  int barrel = (a.layer()>0) ? (a.layer()+1)/2 : -1;

  float vd = -1;
  if (mDriftVelocity) {
    int index = mDriftVelocity->getHybridIndex(barrel,a.ladder(),a.wafer(),a.hybrid());
    if (index >= 0 && mDriftVelocity->at(index))
      vd = ((StSvtHybridDriftVelocity*)mDriftVelocity->at(index))->getV3(1)*
	mDeltaDriftVelocity;
  }
  if (vd < 0)
    vd = 675000*mDeltaDriftVelocity;

  double t;
  //t = x/mparam->vd;
  t = x/vd;
  
  //return (t*mparam->fsca);
  return (t*fsca);

}
//_____________________________________________________________________________

double StSvtCoordinateTransform::CalcTransLength(double x){

  // Gives transverse distance of spt in cm in local coords (anode dir)
  //       return x*mparam->pitch;
  return x*mgeom->getAnodePitch();
}


//_____________________________________________________________________________

double StSvtCoordinateTransform::UnCalcTransLength(double x){

  // Gives transverse distance of spt in cm in local coords (anode dir)
  //       return x/mparam->pitch;
  return x/mgeom->getAnodePitch();
}

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

