/***********************************************************************
 *
 * $Id: StSvtCoordinateTransform.cc,v 1.20 2003/02/11 23:37:26 caines Exp $
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
#include <unistd.h>
#include "TString.h"

#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using namespace units;
#endif


//_____________________________________________________________________________
StSvtCoordinateTransform::StSvtCoordinateTransform() {

}

//_____________________________________________________________________________

StSvtCoordinateTransform::StSvtCoordinateTransform(StTpcDb* gStTpcDb) {

}
//_____________________________________________________________________________

StSvtCoordinateTransform::~StSvtCoordinateTransform() {
 }

//_____________________________________________________________________________
void StSvtCoordinateTransform::setParamPointers( srs_srspar_st* param,
						 svg_geom_st* geom, 
						 svg_shape_st* shape,
						 StSvtConfig* config,
						 StSvtHybridCollection* driftVeloc){
  mgeom = new StSvtGeometry(param, geom, shape);
  mconfig = config;
  mDriftVelocity = driftVeloc;
  
};
//____________________________________________________________________________
void StSvtCoordinateTransform::setParamPointers( StSvtGeometry* geom,
						 StSvtConfig* config,
						 StSvtHybridCollection* driftVeloc){
  mgeom = geom;
  mconfig = config;
  mDriftVelocity = driftVeloc;
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

void StSvtCoordinateTransform::operator()(const StGlobalCoordinate& a, StSvtWaferCoordinate& c)
{

  StSvtLocalCoordinate b;

 
  this->operator()(a,b);

  this->operator()(b,c);
  return;

}
//_____________________________________________________________________________

//      Raw Data          -->  SVT Local  Coordinate
void StSvtCoordinateTransform::operator()(const StSvtWaferCoordinate& a, StSvtLocalCoordinate& b)

{

  double t0=11;

  b.setLayer(a.layer());
  b.setLadder(a.ladder());
  b.setWafer(a.wafer());
  b.setHybrid(a.hybrid());


  double t = a.timebucket() - t0;

  b.setPosition(StThreeVector<double>(CalcDriftLength(a,t),
				      CalcTransLength(a.anode()),0.0));
  
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
  
  return;
}
//_____________________________________________________________________________


// SVT Local  Coordinate  -->   Raw coordinate

void StSvtCoordinateTransform::operator()(const StSvtLocalCoordinate& a, StSvtWaferCoordinate& b)
{

  double t0=11;

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
  


  double t = UnCalcDriftLength(a,pos.x()) + t0;
  b.setTimeBucket(t);
  b.setAnode(UnCalcTransLength(pos.y()));

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

void StSvtCoordinateTransform::operator()(const StGlobalCoordinate& a,  StSvtLocalCoordinate& b)
{

  int barrel, HardWarePos, Found;
  int iladder, ibarrel, NWafer;
  int ladderRangeLo, ladderRangeHi, ladderMax;


  // Find out what barrel hit is on

  double r = (a.position().x()*a.position().x() +
	      a.position().y()*a.position().y());

  if( r > 169) barrel = 3;
  else if( r > 75) barrel = 2;
  else barrel = 1;


  //Exception for year1 ladder Only one ladder which is wrongly labelled as barrel 3
  //even though its at r=10.4

  //  if( mgeom[0].id > 5000) barrel = 3;
  if( TString(mconfig->getConfiguration()) == "Y1L") barrel = 3;    

  // Find out what wafer hit is on

     HardWarePos = 1000*(2*barrel)+1;

     for( NWafer=1; NWafer<= mconfig->getNumberOfWafers(barrel); NWafer++){

       if ( IsOnWaferZ(a.position(),HardWarePos+100*NWafer)) break;
       if ( IsOnWaferZ(a.position(),HardWarePos+100*NWafer+1)) break;
     }

     b.setWafer(NWafer);


  switch (barrel){
    
  case 1:
      
      if( a.position().x() >= 0 && a.position().y() >= 0){
	ladderRangeLo = 1;
	ladderRangeHi = 2;
	ladderMax = 8;
      }
      else if(a.position().x() >= 0 && a.position().y() < 0){
	ladderRangeLo = 2;
	ladderRangeHi = 4;
	ladderMax = 0;
      }
      else if(a.position().x() < 0 && a.position().y() < 0){
	ladderRangeLo = 4;
	ladderRangeHi = 6;
	ladderMax = 0;
      }
      else if(a.position().x() < 0 && a.position().y() >= 0){
	ladderRangeLo = 6;
	ladderRangeHi = 8;
	ladderMax = 0;
      }
    
    
    break;
   
  case 2:
      
    if( a.position().x() > 0 && a.position().y() > 0){
      ladderRangeLo = 1;
      ladderRangeHi = 3;
      ladderMax = 12;
    }
    else if(a.position().x() > 0 && a.position().y() < 0){
      ladderRangeLo = 3;
      ladderRangeHi = 6;
      ladderMax = 0;
    }
    else if(a.position().x() < 0 && a.position().y() < 0){
      ladderRangeLo = 6;
      ladderRangeHi = 9;
      ladderMax = 0;
    }
    else if(a.position().x() < 0 && a.position().y() > 0){
      ladderRangeLo = 9;
      ladderRangeHi = 12;
      ladderMax = 0;
    }
    
    
    break;
    
  case 3:
   if( a.position().x() > 0 && a.position().y() > 0){
      ladderRangeLo = 1;
      ladderRangeHi = 4;
      ladderMax = 16;
    }
    else if(a.position().x() > 0 && a.position().y() < 0){
      ladderRangeLo = 4;
      ladderRangeHi = 8;
      ladderMax = 0;
    }
    else if(a.position().x() < 0 && a.position().y() < 0){
      ladderRangeLo = 8;
      ladderRangeHi = 12;
      ladderMax = 0;
    }
    else if(a.position().x() < 0 && a.position().y() > 0){
      ladderRangeLo = 12;
      ladderRangeHi = 16;
      ladderMax = 0;
    }

      break;
  }
    
  //Exception for year1 ladder Only one ladder which is wrongly labelled as barrel 3
  // ladder 1even though its at r=10.4 at 12 0'Clock
  
  //  if( mgeom[0].id > 5000) ladderRangeLo = 1; 
  if( TString(mconfig->getConfiguration()) == "Y1L") ladderRangeLo = 1;

  Found = 0;
  ibarrel = (barrel*2)-1;
  do{
    for( iladder=ladderRangeLo; iladder<=ladderRangeHi; iladder++){
      HardWarePos = 1000*ibarrel+100*b.wafer()+iladder;
      if( IsOnWaferR(a.position(),HardWarePos)) {
	Found = 1;
	break;
      }
    }
    if( Found) break;
    ibarrel++;
  }while (ibarrel<=barrel*2);
  
  if( !Found && ladderMax !=0){
    
    for(  ibarrel=(barrel*2)-1; ibarrel<=barrel*2; ibarrel++){
      iladder = ladderMax; 
      HardWarePos = 1000*ibarrel+100*b.wafer()+iladder;
      if( IsOnWaferR(a.position(),HardWarePos)) {
	Found = 1;
	break;
      }
    }
  }

  if( !Found){

    b.setWafer(-99);
    b.setLayer(-99);
    b.setLadder(-99);
    b.position().setX(-99);
    b.position().setY(-99);
    b.position().setZ(-99);
  }
  else{

    b.setLayer(ibarrel);
    b.setLadder(iladder);
    
    GlobaltoLocal( a.position(), b, HardWarePos, -1);

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

  float xl[3];

  /*
  int  HardWarePos, Gotit=0;


  if( index < 0){
    HardWarePos = 1000*a.layer()+100*a.wafer()+a.ladder();
    for( index=0; index< 216; index++){
      if( mgeom[index].id ==  HardWarePos) {
	Gotit++;
	break;
      }
    }
    
    if( !Gotit) {
      x.setX(-999);
      x.setY(-999);
      x.setZ(-999);
      return 0;
      
      
    }
  }
  */

  StSvtWaferGeometry* waferGeom = NULL;

  if( index < 0){
    index = mgeom->getWaferIndex(mgeom->getBarrelID(a.layer(),a.ladder()),(int)a.ladder(),(int)a.wafer());

  }
  if (index >= 0 && index < 216)
    waferGeom = (StSvtWaferGeometry*)mgeom->at(index);

  if (!waferGeom) {
    x.setX(-999);
    x.setY(-999);
    x.setZ(-999);
    return 0;
  }
      
  xl[0] = a.position().x();
  xl[1] = a.position().y();
  xl[2] = a.position().z();
  
  //  x.setX(mgeom[index].x[0] + xl[0]*mgeom[index].d[0] + xl[1]*mgeom[index].t[0] + xl[2]*mgeom[index].n[0]); 
  //  x.setY(mgeom[index].x[1] + xl[0]*mgeom[index].d[1] + xl[1]*mgeom[index].t[1] + xl[2]*mgeom[index].n[1]);
  //  x.setZ(mgeom[index].x[2] + xl[0]*mgeom[index].d[2] + xl[1]*mgeom[index].t[2] + xl[2]*mgeom[index].n[2]); 
  
  x.setX(waferGeom->x(0) + xl[0]*waferGeom->d(0) + xl[1]*waferGeom->t(0) + xl[2]*waferGeom->n(0)); 
  x.setY(waferGeom->x(1) + xl[0]*waferGeom->d(1) + xl[1]*waferGeom->t(1) + xl[2]*waferGeom->n(1));
  x.setZ(waferGeom->x(2) + xl[0]*waferGeom->d(2) + xl[1]*waferGeom->t(2) + xl[2]*waferGeom->n(2)); 
  
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

  //  int Gotit=0;
  float  xl[3];
  
  //     Executable Code
  //     ===============
  

  /*
  if( index < 0){
    for( index=0; index< 216; index++){
      if( mgeom[index].id == HardWarePos ) {
	Gotit++;
	break;
      }
    }
    
    
    if( !Gotit) {
      b.position().setX(-998);
      b.position().setY(-998);
      b.position().setZ(-998);
      return -1;
    }
    
  }
  */
  
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
 
  xl[0] = x.x() - waferGeom->x(0);
  xl[1] = x.y() - waferGeom->x(1);
  xl[2] = x.z() - waferGeom->x(2);

  b.position().setX(xl[0]*waferGeom->d(0) + xl[1]*waferGeom->d(1) + xl[2]*waferGeom->d(2));
  b.position().setY(xl[0]*waferGeom->t(0) + xl[1]*waferGeom->t(1) + xl[2]*waferGeom->t(2));
  b.position().setZ(xl[0]*waferGeom->n(0) + xl[1]*waferGeom->n(1) + xl[2]*waferGeom->n(2));
 

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
//   v2=(-bb+sqrt(pow(bb,2)-4*aa*cc))/(2*aa);
//   v1=(v2+b)/a; /* using Sanjeev's fit from the bench */
  
//   v1=v1*0.8;  
  
//   t=x/mparam->fsca;
  
  
//   distance= (v1*t);
    
//   if (distance>d) distance=d+v2*(t-d/v1);
  
  // hard wired for the time being (07/29/2001) MM
  //float vd = 675000;
  float fsca = 25000000;

  int barrel;
  if ((a.layer()==1) || (a.layer()==2)) barrel = 1;
  if ((a.layer()==3) || (a.layer()==4)) barrel = 2;
  if ((a.layer()==5) || (a.layer()==6)) barrel = 3;
  int ladder = a.ladder();
  int wafer = a.wafer();
  int hybrid = a.hybrid();

  float vd = -1;
  int index;
  if (mDriftVelocity) {
    index = mDriftVelocity->getHybridIndex(barrel,ladder,wafer,hybrid);
    if (index > 0)
      vd = ((StSvtHybridDriftVelocity*)mDriftVelocity->at(index))->getV3(1);
  }
  if (vd < 0)
    vd = 675000;

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
//   v2=(-bb+sqrt(pow(bb,2)-4*aa*cc))/(2*aa);
//   v1=(v2+b)/a; /* using Sanjeev's fit from the bench */
  
//   v1=v1*0.8;  
  
//   t= x/v1;

//   if (x>d) t = (x-d)/v2 + d/v1;

  // hard wired for the time being (07/29/2001) MM
  //float vd = 675000;
  float fsca = 25000000;

  int barrel;
  if ((a.layer()==1) || (a.layer()==2)) barrel = 1;
  if ((a.layer()==3) || (a.layer()==4)) barrel = 2;
  if ((a.layer()==5) || (a.layer()==6)) barrel = 3;

  float vd = -1;
  int index;
  if (mDriftVelocity) {
    index = mDriftVelocity->getHybridIndex(barrel,a.ladder(),a.wafer(),a.hybrid());
    if (index > 0)
      vd = ((StSvtHybridDriftVelocity*)mDriftVelocity->at(index))->getV3(1);
  }
  if (vd < 0)
    vd = 675000;

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

  if( LocalPosition.position().y() > -3.1525 && LocalPosition.position().y() < 3.1525)
    return 1;
  else
    return 0;
}

//_____________________________________________________________________________

int StSvtCoordinateTransform::IsOnWaferR(   const StThreeVector<double>& GlobalPosition,
					    int HardWarePos){

  //Find out for a given z coord and Hardware pos is it on the wafer

  StSvtLocalCoordinate LocalPosition;

  GlobaltoLocal(GlobalPosition, LocalPosition, HardWarePos, -1);


  if( LocalPosition.position().x() > -3.1525    &&
      LocalPosition.position().x() < 3.1525     &&
      LocalPosition.position().z() > -.05   &&
      LocalPosition.position().z() < .05    )
    return 1;
  else
    return 0;
}

