#ifndef FTFBASEHIT
#define FTFBASEHIT

#include "Stl3Util/ftf/FtfGeneral.h"


class FtfBaseTrack ;

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

