#ifndef FTFBASEHIT
#define FTFBASEHIT
#include "FtfGeneral.h"

#ifdef SL3ROOT
#include "Rtypes.h"
#else
#define ClassDef(a,b)
#endif

   class FtfBaseTrack ;

   class FtfBaseHit {         
   public:
       void         print ( ) ;
       void         print ( int level ) ;
       int          id    ;         // Primary key 
       short        row   ;         // Row #     
       FtfBaseTrack *track;         // track to which the pnt was assigned
       FtfBaseHit   *nextTrackHit  ;// Next track hit             
       FtfBaseHit   *nextMcTrackHit;// Next MC track hit
       float        xyChi2 ;        // Chi2 in xy                 
       float        szChi2 ;        // Chi2 in sz                 
       float        dx   ;          // error on the x coordinate  
       float        dy   ;          // error on the y coordinate  
       float        dz   ;          // error on the z coordinate  
       float        x    ;          // reconstructed x coordinate 
       float        y    ;          // reconstructed y coordinate 
       float        z    ;          // reconstructed z coordinate 
       float        q    ;          // total charge assigned to this point 
       float        wxy  ;          // x-y weight x-y
       float        wz   ;          // z weight on z
       float        s    ;          // Track trajectory
       ClassDef(FtfBaseHit,1)

   } ;
#endif

