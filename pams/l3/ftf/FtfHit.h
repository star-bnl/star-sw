#ifndef FTFHIT
#define FTFHIT
   class FtfPara  ;
   class FtfTrack ;
   

   class FtfHit {         
   public:
       void         print ( int level ) ;
       void	    show  ( int color ) ;
       void         setStatus ( FtfTrack* this_track, FtfPara *para ) ;


       int          id    ;           // Primary key 
       short        row   ;           // Row #     
       short        phiIndex ;        // Phi index    
       short        etaIndex ;        // Eta index    
       FtfTrack     *track;           // id of a track to which the pnt was assgn 
       int          mcTrackId;        // id of a MC track   
       FtfHit       *nextVolumeHit ;  // Next volume hit            
       FtfHit       *nextRowHit    ;  // Next row hit               
       FtfHit       *nextTrackHit  ;  // Next track hit             
       FtfHit       *nextMcTrackHit;  // Next MC track hit                   
       float        xyChi2 ;          // Chi2 in xy                 
       float        szChi2 ;          // Chi2 in sz                 
       float        dx   ;            // error on the x coordinate  
       float        dy   ;            // error on the y coordinate  
       float        dz   ;            // error on the z coordinate  
       float        wxy  ;            // x-y weight x-y             
       float        wz   ;            // z weight on z              
       float        s    ;            // Track trajectory           
       float        r    ;            // radius                     
       float        phi  ;            // azimuthal angle            
       float        dphi ;            // Error in phi               
       float        eta  ;            // hit pseudorapidity         
       float        x    ;            // reconstructed x coordinate 
       float        y    ;            // reconstructed y coordinate 
       float        z    ;            // reconstructed z coordinate 
       float        xp   ;            // x conformal coordinate 
       float        yp   ;            // y conformal coordinate 
       float        q    ;            // total charge assigned to this point 
   } ;
#endif

