#ifndef  FTFBASETRACK
#define  FTFBASETRACK
#include "FTF_Hit.h"
#include "FTF_Para.h"
//
//    Base Track class
//
  
  class BaseTrack { 
      
   
	public:
	   BaseTrack ( ) ;
	   FTF_Hit  *first_hit;// First hit belonging to track
	   FTF_Hit  *last_hit ;// Last  hit belonging to track
	   FTF_Hit  *current_hit ;
	   int       Fit_Helix   (  ) ;
	   int Fit_Circle        (  ) ;
	   
       int Fit_Line          (  ) ;
	   int Get_Errors_Circle_Fit ( float a, float b, float r ) ;
	   
	   void      Print       ( int level ) ;
	   
	   inline virtual   void start_loop(){ current_hit = first_hit ; } ;
       inline virtual   void next_hit ( ) = 0 ; 
       inline virtual   int  done      () { return current_hit != 0 ; } ;
	  
       int       id     ;  // primary key 
       short     flag   ;  // Primaries flag=1, Secondaries flag=0      
       int       n_hits ;  // Number of points assigned to that track
       short      q  ;      // charge 
       float     chi2[2];  // chi squared of the momentum fit 
       float     dedx;     // dE/dx information 
       float     pt  ;     // pt (transverse momentum) at (r,phi,z) 
       float     phi0;     // azimuthal angle of the first point 
       float     psi ;     // azimuthal angle of the momentum at (r,.. 
       float     r0  ;     // r (in cyl. coord.) for the first point 
       float     tanl;     // tg of the dip angle at (r,phi,z) 
       float     z0  ;     // z coordinate of the first point 
	   float     dpt ;
	   float     dpsi;
	   float     dz0 ;
       float     eta ;
	   float     dtanl ;

	   FTF_Para *para       ;    // Parameters pointer     

   } ;
#endif

