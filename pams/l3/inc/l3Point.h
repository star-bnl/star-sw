/*:--------------------------------------------------------------------
**: FILE:       l3Point.h
**: HISTORY:
**:             7/07/99  Install in the library
**:                      For the moment only 6 bytes per cluster even though
**:                      DAQ documentation calls for 8
**:            10/7/99   replace t2lConstants with sl3GeoConstants
**:--------------------------------------------------------------------*/
#ifndef L3POINT
#define L3POINT
#include <math.h>
#include <stdio.h>
#include "sl3GeoConstants.h"

typedef float mword ;

/*:-------------------------------------------------------------------
**: CLASS:       l3Cluster
**: DESCRIPTION: Online cluster information
**:
**: AUTHORS:     Pablo Yepes, yepes@rice.edu
**:
**:-------------------------------------------------------------------*/
class l3Cluster {
public:
   unsigned short pad ;
   unsigned short time ;
   unsigned short dedx ;

   void print ( ) {
       printf ( " pad %d time %d \n ", pad, time ) ;
   }
};

/*:-------------------------------------------------------------------
**: CLASS:       l3Point  
**: DESCRIPTION: Online tpc space point
**:
**: AUTHORS:     Pablo Yepes, yepes@rice.edu
**:
**:-------------------------------------------------------------------*/

class l3Point {
   short   row ;
   mword x ;
   mword y ;
   mword z ;
public:
   short Row() { return row ; } ;
   mword X() { return x ; } 
   mword Y() { return y ; } 
   mword Z() { return z ; } 
   mword Phi ( ) {
      mword phi = atan2(y,x);
      if ( phi < 0 ) phi += 6.283185307 ;
      return phi ;
   }
   void setRow ( short rowIn ) { row = rowIn ; }  
//
   l3Point () { x = y = z = 0 ; } ;
   l3Point ( mword xx, mword yy, mword zz ) {
      set ( xx, yy, zz ) ;
   }
   l3Point ( int rw, mword xx, mword yy, mword zz ) {
      set ( rw, xx, yy, zz ) ;
   }
//****************************************************************
//****************************************************************
   float distance ( l3Point p ) {
      float dx = x - p.X() ;
      float dy = y - p.Y() ;
      float dz = z - p.Z() ;
      return (float)sqrt(dx*dx+dy*dy+dz*dz) ;
   }
//****************************************************************
   l3Point toLocal ( int sector  ) {
//
// rotate these coordinates
//
      int is = sector - 1 ;
      
      float xx = SectorSin[is] * x - SectorCos[is] * y;
      float yy = SectorSin[is] * y + SectorCos[is] * x;
      float zz = (float)fabs(z);

      l3Point p(row,xx,yy,zz);

      return p ;
   }
//************************************************************************
   void set ( int rw, l3Cluster cluster ) {
      row = rw ;
      int p = row-1; 
      x = (float)(((float)(cluster.pad))/64. - (nPadsInRow[p] >> 1)+.5) * padSpacing[p];
      y = padrowOffset[p];
      z = (float)(offset + driftLength - (((double)cluster.time)/64.) * timeScale);
   }
//****************************************************************
   l3Cluster toCluster (   ) {
//
    l3Cluster cluster ;
    cluster.pad  = (int)(64.*(( x /  padSpacing[row-1] -0.5 ) + (nPadsInRow[row-1] >> 1))) ;
    cluster.time = (int)(64.*((driftLength+offset-fabs(z))/timeScale));

    return cluster ;
  }
//****************************************************************
   l3Point toGlobal ( int sector  ) {
// 
// rotate these coordinates
//
      int is = sector - 1 ;
      short sgn = 1 ;

      float xx = SectorSin[is] * x + SectorCos[is] * y;
      float yy = SectorSin[is] * y - SectorCos[is] * x;
      if ( sector > 12 ) sgn = 1 ;
      float zz = sgn*z;

      l3Point p(row,xx,yy,zz);

      return p ;
   }
//****************************************************************
   void print ( ) {
      printf ( " row x y z %d %f %f %f \n", row, x, y, z ) ;
   }
//****************************************************************
   void set ( float xx, float yy, float zz ) {
      x = xx ; y = yy ; z = zz ;
   }
//
   void set ( int rw, float xx, float yy, float zz ) {
      row    = rw ;
      x = xx ; y = yy ; z = zz ;
   }
};
#endif
