#ifndef  FTFBASETRACK
#define  FTFBASETRACK
#include "FtfGeneral.h"
#include "FtfBaseHit.h"
#include "FtfPara.h"

#ifdef SL3ROOT
#include "Rtypes.h"
#else
#define ClassDef(a,b)
#endif
//
//    Base Track class
//
  class FtfBaseTrack { 
      
public:
    FtfBaseTrack ( ) ;
    FtfBaseHit *firstHit;// First hit belonging to track
    FtfBaseHit *lastHit ;// Last  hit belonging to track
    FtfBaseHit *currentHit ;
    int        fitHelix   (  ) ;
    int        fitCircle  (  ) ;
	   
    int        fitLine    (  ) ;
    int        getErrorsCircleFit ( float a, float b, float r ) ;
	   
    void       Print       ( int level ) ;
	   
    inline virtual   void startLoop( ){ currentHit = firstHit ; } ;
    inline virtual   void nextHit  ( ) = 0 ; 
    inline virtual   int  done     ( ) { return currentHit != 0 ; } ;
	  
    int       id     ;  // primary key 
    short     flag   ;  // Primaries flag=1, Secondaries flag=0      
    int       nHits  ;  // Number of points assigned to that track
    short     q  ;      // charge 
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

    FtfPara  *para  ;    // Parameters pointer     
    ClassDef(FtfBaseTrack,1)


   } ;
#endif

