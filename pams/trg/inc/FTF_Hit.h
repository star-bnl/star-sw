#ifndef FTFHIT
#define FTFHIT
   class FTF_Para  ;
   class FTF_Track ;
   

   class FTF_Hit {         
   public:
       void         Print ( int level ) ;
       void	    Show  ( int color ) ;
       void         Set_Status ( FTF_Track* this_track, FTF_Para *para ) ;


       int          id    ;     // Primary key 
       short        i_r   ;     // Row #     
       short        i_phi ;     // Phi index    
       short        i_eta ;     // Eta index    
       FTF_Track    *track;     // id of a track to which the pnt was assgn 
       int          mc_track;   // id of a MC track   
       FTF_Hit      *nxvhit  ;  // Next volume hit            
       FTF_Hit      *nxrhit  ;  // Next row hit               
       FTF_Hit      *nxthit  ;  // Next track hit             
       FTF_Hit      *nxmhit  ;  // Next MC track hit          
       FTF_Hit      *nxghit  ;  // Next ghost hit          
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

