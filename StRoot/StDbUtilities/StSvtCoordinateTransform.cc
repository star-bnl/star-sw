/***********************************************************************
 *
 * $Id: StSvtCoordinateTransform.cc,v 1.3 2000/08/26 20:37:59 caines Exp $
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
#include "StSvtCoordinateTransform.hh"
#include "StCoordinates.hh"        // coordinate definitions
#include <unistd.h>
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using namespace units;
#endif

StSvtCoordinateTransform::StSvtCoordinateTransform() {
}
//_____________________________________________________________________________

StSvtCoordinateTransform::~StSvtCoordinateTransform() { /* nopt */ }

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

  double t0=0;

  b.setLayer(a.layer());
  b.setLadder(a.ladder());
  b.setWafer(a.wafer());
  b.setHybrid(a.hybrid());


  double t = a.timebucket() - t0;

  b.position().setX(CalcDriftLength(t));
  b.position().setY(CalcTransLength(a.anode()));
  b.position().setZ(0.015);

  
  int idShape = 0;
  
  
  // Shift position because it is the North hybrid. Place relative to the center of the wafer, hybrid=1, anode 1=East
  if( a.hybrid() == 1 ){
    
    b.position().setX( b.position().x() - mshape[idShape].shape[0]);
    b.position().setY( b.position().y() - mshape[idShape].shape[1]);
  }
  
  //Shift position because it is South hybrid. Place relative to the center of the wafer. Hybrid=2, anode 1=West
  
  
  if( a.hybrid() == 2){
    
    b.position().setX( mshape[idShape].shape[0] - b.position().x());
    b.position().setY( mshape[idShape].shape[1] - b.position().y());
  }
  
  return;
}
//_____________________________________________________________________________


// SVT Local  Coordinate  -->   Raw coordinate

void StSvtCoordinateTransform::operator()(const StSvtLocalCoordinate& a, StSvtWaferCoordinate& b)
{

  double t0=0;

  StThreeVector<double> pos(0,0,0);

  b.setLayer(a.layer());
  b.setLadder(a.ladder());
  b.setWafer(a.wafer());
  b.setHybrid(a.hybrid());
  
  int idShape = 0;
  
  
  // Shift position because it is the North hybrid. Local coords are placed relative to the center of the wafer, hybrid=1, anode 1=East so undo this
  if( a.hybrid() == 1 ){
    
   pos.setX( a.position().x() + mshape[idShape].shape[0]);
   pos.setY( a.position().y() + mshape[idShape].shape[1]);
  }
  
  //Shift position because it is South hybrid. Local coords are placed relative to the center of the wafer. Hybrid=2, anode 1=West so undo this
  
  
  if( a.hybrid() == 2){
    
    pos.setX( mshape[idShape].shape[0] - a.position().x());
    pos.setY( mshape[idShape].shape[1] - a.position().y());
  }
  


  double t = UnCalcDriftLength(pos.x()) + t0;
  b.setTimeBucket(t);
  b.setAnode(UnCalcTransLength(pos.y()));

  return;

}

//_____________________________________________________________________________
//  Svt Local  --> Global
 
void StSvtCoordinateTransform::operator()(const StSvtLocalCoordinate& a, StGlobalCoordinate& b)
{

  StThreeVector<double> x(0,0,0);
  
  LocaltoGlobal(a, x);
  
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
  else if( r > 64) barrel = 2;
  else barrel = 1;


  //Exception for year1 ladder Only one ladder which is wrongly labelled as barrel 3
  //even though its at r=10.4

  if( mgeom[0].id > 5000) barrel = 3;
      

  // Find out what wafer hit is on

     HardWarePos = 1000*(2*barrel)+1;

     for( NWafer=1; NWafer<= mconfig->getNumberOfWafers(barrel); NWafer++){

       if ( IsOnWaferZ(a.position(),HardWarePos+100*NWafer)) break;
     }

     b.setWafer(NWafer);


  switch (barrel){
    
  case 1:
      
      if( a.position().x() > 0 && a.position().y() > 0){
	ladderRangeLo = 1;
	ladderRangeHi = 2;
	ladderMax = 8;
      }
      else if(a.position().x() > 0 && a.position().y() < 0){
	ladderRangeLo = 2;
	ladderRangeHi = 4;
	ladderMax = 0;
      }
      else if(a.position().x() < 0 && a.position().y() < 0){
	ladderRangeLo = 4;
	ladderRangeHi = 6;
	ladderMax = 0;
      }
      else if(a.position().x() < 0 && a.position().y() > 0){
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
  
  if( mgeom[0].id > 5000) ladderRangeLo = 1; 

  Found = 0;
  ibarrel = (barrel*2)-2;
  while(ibarrel<barrel*2 && Found == 0){
    ibarrel++;
    for( iladder=ladderRangeLo; iladder<=ladderRangeHi; iladder++){
      HardWarePos = 1000*ibarrel+100*b.wafer()+iladder;
      if( IsOnWaferR(a.position(),HardWarePos)) {
	Found = 1;
	break;
      }
    }
  }
  
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
    
    GlobaltoLocal( a.position(), b, HardWarePos);

    b.setHybrid(2);
    if( b.position().x() < 0) b.setHybrid(1);
  }

}

//_____________________________________________________________________________

void  StSvtCoordinateTransform::LocaltoGlobal(const StSvtLocalCoordinate& a, StThreeVector<double>& x){
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
  int index, HardWarePos, Gotit=0;

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
    return;
  }
  
  
  xl[0] = a.position().x();
  xl[1] = a.position().y();
  xl[2] = a.position().z();
  
  x.setX(mgeom[index].x[0] + xl[0]*mgeom[index].d[0] + xl[1]*mgeom[index].t[0] + xl[2]*mgeom[index].n[0]); 
  x.setY(mgeom[index].x[1] + xl[0]*mgeom[index].d[1] + xl[1]*mgeom[index].t[1] + xl[2]*mgeom[index].n[1]);
  x.setZ(mgeom[index].x[2] + xl[0]*mgeom[index].d[2] + xl[1]*mgeom[index].t[2] + xl[2]*mgeom[index].n[2]); 
  
}

//___________________________________________________________________________

void StSvtCoordinateTransform::GlobaltoLocal( const StThreeVector<double>& x, StSvtLocalCoordinate& b , int HardWarePos  )
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

  int index, Gotit=0;
  float  xl[3];
  
  //     Executable Code
  //     ===============
  

  for( index=0; index< 216; index++){
    if( mgeom[index].id == HardWarePos ) {
      Gotit++;
      break;
    }
  }
  

  if( !Gotit) {
    b.position().setX(-999);
    b.position().setY(-999);
    b.position().setZ(-999);
    return;
  }

  
  xl[0] = x.x() - mgeom[index].x[0];
  xl[1] = x.y() - mgeom[index].x[1];
  xl[2] = x.z() - mgeom[index].x[2];
  
  b.position().setX(xl[0]*mgeom[index].d[0] + xl[1]*mgeom[index].d[1] + xl[2]*mgeom[index].d[2]);
  b.position().setY(xl[0]*mgeom[index].t[0] + xl[1]*mgeom[index].t[1] + xl[2]*mgeom[index].t[2]);
  b.position().setZ(xl[0]*mgeom[index].n[0] + xl[1]*mgeom[index].n[1] + xl[2]*mgeom[index].n[2]); 
  
}

//_____________________________________________________________________________

double StSvtCoordinateTransform::CalcDriftLength(double x){

  //Gives drift distance of spt in cm in local coords from timebuckets


  // There is a smaller drift velocity in the focusing region 
  //  This has to be used when having only the average drift velocity across 
  //  the all detector 
       
  //  mparam->vd = average velocity from y=0 to y=3cm
  //   v2 = velocity in drift region ( y > ofoc cm)
  //   v1 = velocity in focusing region ( y < ofoc cm)
  //     a,b = parameters from Sanjeev's fit 

  double aa,bb,cc,v2,v1,t,distance;
  
  double a=1.80;
  double b=240000;
  double d=0.27;
  double l=mshape[0].shape[0];



  aa=l/mparam->vd;
  bb=(b*l/mparam->vd-l-a*d+d);
  cc=(d*b-b*l);
  v2=(-bb+sqrt(pow(bb,2)-4*aa*cc))/(2*aa);
  v1=(v2+b)/a; /* using Sanjeev's fit from the bench */
  
  v1=v1*0.8;  
  
  t=x/mparam->fsca;
  
  
  distance= (v1*t);
    
  if (distance>d) distance=d+v2*(t-d/v1);
  
  return distance;
  
}
//_____________________________________________________________________________

double StSvtCoordinateTransform::UnCalcDriftLength(double x){

  //Gives drift distance of spt in timebuckets from cm in local coords



  // There is a smaller drift velocity in the focusing region 
  //  This has to be used when having only the average drift velocity across 
  //  the all detector 
       
  //  mparam->vd = average velocity from y=0 to y=3cm
  //   v2 = velocity in drift region ( y > ofoc cm)
  //   v1 = velocity in focusing region ( y < ofoc cm)
  //     a,b = parameters from Sanjeev's fit 

  double aa,bb,cc,v2,v1,t,distance;
  
  double a=1.80;
  double b=240000;
  double d=0.27;
  double l=mshape[0].shape[0];



  aa=l/mparam->vd;
  bb=(b*l/mparam->vd-l-a*d+d);
  cc=(d*b-b*l);
  v2=(-bb+sqrt(pow(bb,2)-4*aa*cc))/(2*aa);
  v1=(v2+b)/a; /* using Sanjeev's fit from the bench */
  
  v1=v1*0.8;  
  
  t=x*mparam->fsca;
  t /=v1;

  if (x>d) t = (x-d)/v2 + d/v1;
  
  return (t*mparam->fsca);

}
//_____________________________________________________________________________

double StSvtCoordinateTransform::CalcTransLength(double x){

  // Gives transverse distance of spt in cm in local coords (anode dir)
       return x*mparam->pitch;
}


//_____________________________________________________________________________

double StSvtCoordinateTransform::UnCalcTransLength(double x){

  // Gives transverse distance of spt in cm in local coords (anode dir)
       return x/mparam->pitch;
}

//_____________________________________________________________________________

int StSvtCoordinateTransform::IsOnWaferZ(   const StThreeVector<double>& GlobalPosition,
					    int HardWarePos){

  //Find out for a given z coord and Hardware pos is it on the wafer

  StSvtLocalCoordinate LocalPosition;


  GlobaltoLocal(GlobalPosition, LocalPosition, HardWarePos);

  if( LocalPosition.position().y() > -3. && LocalPosition.position().y() < 3.)
    return 1;
  else
    return 0;
}

//_____________________________________________________________________________

int StSvtCoordinateTransform::IsOnWaferR(   const StThreeVector<double>& GlobalPosition,
					    int HardWarePos){

  //Find out for a given z coord and Hardware pos is it on the wafer

  StSvtLocalCoordinate LocalPosition;

  GlobaltoLocal(GlobalPosition, LocalPosition, HardWarePos);

  if( LocalPosition.position().x() > -3. && LocalPosition.position().x() < 3.)
    return 1;
  else
    return 0;
}

