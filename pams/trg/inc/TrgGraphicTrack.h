#ifndef  TRGGRAPHICTRACK
#define  TRGGRAPHICTRACK

#include "TrgGraphicHit.h"

class TrgGraphicHitP {
public:
   TrgGraphicHitP ( ) : pointer(0) { } ;
   TrgGraphicHit* pointer ;
};

class TrgGraphic ;
//
//    Base Track class
//
  
class TrgGraphicTrack { 
      
public:
   TrgGraphicTrack  ( ) : currentHit(0) , currentHitIndex(0), hitList(0) {} ; 
   ~TrgGraphicTrack ( ) { if ( hitList != 0 ) delete[] hitList ; } ; 

   void plot    ( TrgGraphic *g, int color, float label_scale, int label_pos ) ;
   void plotFit ( TrgGraphic *gr, float r_min, float r_max, int color ) ;
   void print ( int level ) ;
   void startLoop ( ) { currentHitIndex = 0 ; currentHit = hitList[0].pointer ; } ;
   int  done      ( ) { return (currentHitIndex < nHits) ; } ;
   void nextHit   ( ) ;
   float     Pt() { return pt ; } ;

   void      add  ( TrgGraphicHit* hit ) ;
   void      fill ( int lid,   int   lnhits, int   charge,  
                    float lpt, float lpsi,   float tanl,
                    float lr0, float lphi0,  float lz0,
                    float chi2_xy, float chi2_sz ) ;
   
private:
   int       id     ;  // primary key 
   short     flag   ;  // Primaries flag=1, Secondaries flag=0      
   int       nHits  ;  // Number of points assigned to this track
   int       maxHits;  // Maximum number of points assigned to this track
   short     q      ;  // charge 
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
//
   TrgGraphicHitP* hitList ;
   TrgGraphicHit*  currentHit ;
   int             currentHitIndex ;
   } ;
#endif

