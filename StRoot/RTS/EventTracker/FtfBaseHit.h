#ifndef FTFBASEHIT
#define FTFBASEHIT

//#include "FtfGeneral.h"

#include "l3Coordinates.h"

class FtfBaseTrack ;

/* Ftf3DHit moved to base/l3Coordinates.h:l3ThreeVector
  
   This allows assignment operators between l3ThreeVector/Ftf3DHit and
   l3xyzCoordinate

class Ftf3DHit 
{
public:
   Ftf3DHit (  ) { x =  y =  z = 0. ; } ;
   Ftf3DHit ( float _x, float _y, float _z ){ x = _x ; y = _y ; z = _z ; } ; 
   void set ( float _x, float _y, float _z ){ x = _x ; y = _y ; z = _z ; } ; 
   float x ;
   float y ;
   float z ;
}; 
*/

typedef l3ThreeVector Ftf3DHit;


class FtfBaseHit
{         
   public:
      void         print ( ) ;
      void         print ( int level ) ;

      int          id ;
      short        row   ;         // Row #     
      void         *track;         // track to which the pnt was assigned
      void         *nextTrackHit  ;// Next track hit             
      void         *nextMcTrackHit;// Next MC track hit
      float        xyChi2 ;        // Chi2 in xy                 
      float        szChi2 ;        // Chi2 in sz                 
      float        x    ; 
      float        y    ;
      float        z    ;
      float        dx   ;          // error on the x coordinate  
      float        dy   ;          // error on the y coordinate  
      float        dz   ;          // error on the z coordinate  
      float        q    ;          // total charge assigned to this point 
      float        wxy  ;          // x-y weight x-y
      float        wz   ;          // z weight on z
      float        s    ;          // Track trajectory
} ;
#endif

