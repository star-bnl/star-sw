/***********************************************************************
 *
 * $Id: StSvtCoordinateTransform.cc,v 1.1 2000/08/21 16:17:27 calderon Exp $
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
  b.position().setX(CalcDriftLength(a.timebucket()));
  b.position().setY(CalcTransLength(a.anode()));
  b.position().setZ(0.015);
  b.setLayer(a.layer());
  b.setLadder(a.ladder());
  b.setWafer(a.wafer());
  b.setHybrid(a.hybrid());
  
  int idShape = (int)mconfig[0].layer_shape[a.layer()-1]-1;
  
  
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

  StThreeVector<double> pos(0,0,0);

  b.setLayer(a.layer());
  b.setLadder(a.ladder());
  b.setWafer(a.wafer());
  b.setHybrid(a.hybrid());
  
  int idShape = (int)mconfig[0].layer_shape[a.layer()-1]-1;
  
  
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
  
  b.setTimeBucket(UnCalcDriftLength(pos.x()));
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

  int index, barrel, HardWarePos, Found;
  int iladder, ilayer, ibarrel, NWafer;
  int ladderRangeLo, ladderRangeHi, ladderMax;


  // Find out what barrel hit is on

  double r = (a.position().x()*a.position().x() +
	      a.position().y()*a.position().y());

  if( r > 169) barrel = 3;
  else if( r > 64) barrel = 2;
  else barrel = 1;




  // Find out what wafer hit is on

     HardWarePos = 1000*(2*barrel)+1;

     for( NWafer=1; NWafer<= mconfig[0].n_wafer[2*barrel]; NWafer++){

       if ( IsOnWaferZ(a.position(),HardWarePos+100*NWafer)) break;
     }

     b.setWafer(NWafer);


  switch (barrel){
    
  case '1':
      
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
    
  case '2':
      
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
    
  case '3':
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
    
    
  
  for( ibarrel=(barrel*2)-1; ibarrel<=barrel*2; ibarrel++){
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
  int index;

  for( index=0; index< 200; index++){
    if( mgeom[index].id == 1000*a.layer()+100*a.wafer()+a.ladder() ) break;
  }
  
  
  xl[0] = a.position().x();
  xl[1] = a.position().y();
  xl[2] = a.position().z();
  
  x.setX(mgeom[index].x[0] + xl[0]*mgeom[index].d[0] + xl[1]*mgeom[index].t[0] + xl[2]*mgeom[index].n[0]); 
  x.setY(mgeom[index].x[1] + xl[0]*mgeom[index].d[1] + xl[1]*mgeom[index].t[1] + xl[2]*mgeom[index].n[1]);
  x.setZ(mgeom[index].x[2] + xl[0]*mgeom[index].d[2] + xl[1]*mgeom[index].t[2] + xl[2]*mgeom[index].n[2]); 
  
}

//___________________________________________________________________________

void StSvtCoordinateTransform::GlobaltoLocal( const StThreeVector<double>& x, StSvtLocalCoordinate& b , int index  )
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

      float  xl[3];

//     Executable Code
//     ===============

      
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

      return (x/mparam->fsca)*mparam->vd;
}
//_____________________________________________________________________________

double StSvtCoordinateTransform::UnCalcDriftLength(double x){

  //Gives drift distance of spt in timebuckets from cm in local coords

      return (x/mparam->vd)/mparam->fsca;
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

int StSvtCoordinateTransform::IsOnWaferZ(   const StThreeVector<double>& GlobalPosition, int HardWarePos){

  //Find out for a given z coord and Hardware pos is it on the wafer

  int index;

  StSvtLocalCoordinate LocalPosition;

  for( index=0; index< 200; index++){
    if( mgeom[index].id == HardWarePos ) break;
  }
  

  GlobaltoLocal(GlobalPosition, LocalPosition, index);

  if( LocalPosition.position().y() > -3. && LocalPosition.position().y() < 3.)
    return 1;
  else
    return 0;
}

//_____________________________________________________________________________

int StSvtCoordinateTransform::IsOnWaferR(   const StThreeVector<double>& GlobalPosition, int HardWarePos){

  //Find out for a given z coord and Hardware pos is it on the wafer

  int index;

  StSvtLocalCoordinate LocalPosition;

  for( index=0; index< 200; index++){
    if( mgeom[index].id == HardWarePos ) break;
  }
  

  GlobaltoLocal(GlobalPosition, LocalPosition, index);

  if( LocalPosition.position().x() > -3. && LocalPosition.position().x() < 3.)
    return 1;
  else
    return 0;
}

