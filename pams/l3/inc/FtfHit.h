#ifndef FTFHIT
#define FTFHIT
#include "FtfBaseHit.h"

#ifdef SL3ROOT
#include "Rtypes.h"
#else
#define ClassDef(a,b)
#endif

   class FtfPara  ;
   class FtfTrack ;
   

   class FtfHit: public FtfBaseHit {         
   public:
       void         printLinks ( ) ;
       void         printLinks ( int level ) ;
       void         setStatus ( FtfTrack* this_track ) ;

       long         id ;
       short        phiIndex ;        // Phi index    
       short        etaIndex ;        // Eta index    
       int          mcTrackId;        // id of a MC track   
       FtfHit       *nextVolumeHit ;  // Next volume hit            
       FtfHit       *nextRowHit    ;  // Next row hit               
       float        r    ;            // radius                     
       float        phi  ;            // azimuthal angle            
       float        dphi ;            // Error in phi               
       float        eta  ;            // hit pseudorapidity         
       float        xp   ;            // x conformal coordinate 
       float        yp   ;            // y conformal coordinate 
       ClassDef(FtfHit,1)
   } ;
#endif

