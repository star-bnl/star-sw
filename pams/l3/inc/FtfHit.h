#ifndef FTFHIT
#define FTFHIT
   class FtfPara  ;
   class FtfTrack ;
   

   class FtfHit {         
   public:
       void         Print ( int level ) ;
       void	    Show  ( int color ) ;
       void         Set_Status ( FtfTrack* this_track, FtfPara *para ) ;


       int          id    ;     // Primary key 
       short        i_r   ;     // Row #     
       short        i_phi ;     // Phi index    
       short        i_eta ;     // Eta index    
       FtfTrack     *track;     // id of a track to which the pnt was assgn 
       int          mc_track;   // id of a MC track   
       FtfHit       *nxvhit  ;  // Next volume hit            
       FtfHit       *nxrhit  ;  // Next row hit               
       FtfHit       *nxthit  ;  // Next track hit             
       FtfHit       *nxmhit  ;  // Next MC track hit                   
       float        chi2_xy ;   // Chi2 in xy                 
       float        chi2_sz ;   // Chi2 in sz                 
       float        dx;         // error on the x coordinate  
       float        dy;         // error on the y coordinate  
       float        dz;         // error on the z coordinate  
       float        wxy  ;      // x-y weight x-y             
       float        wz   ;      // z weight on z              
       float        s    ;      // Track trajectory           
       float        r    ;      // radius                     
       float        phi  ;      // azimuthal angle            
       float        dphi ;      // Error in phi               
       float        eta  ;      // hit pseudorapidity         
       float        x;          // reconstructed x coordinate 
       float        y;          // reconstructed y coordinate 
       float        z;          // reconstructed z coordinate 
       float        xp   ;      // x conformal coordinate 
       float        yp   ;      // y conformal coordinate 
       float        q    ;      // total charge assigned to this point 
   } ;
#endif

