//:>----------------------------------------------------------------------
//: FILE:      FtfTrack.h
//: HISTORY:
//:           27jan2000  start writting changes 
//:                      refHit replaced by xRefHit and yRefHit
//:           27jan2000  VOLUME, ROW and AREA classes replaced by FtfContainer
//:            9mar2000  trackLength moved to FtfBaseTrack
//:            9mar2000  use void pointers
//:>----------------------------------------------------------------------
#ifndef FTFTRACK
#define FTFTRACK
#include "Stl3Util/ftf/FtfGeneral.h"
#include "Stl3Util/ftf/FtfBaseTrack.h"
#include "Stl3Util/ftf/FtfHit.h"
#include "Stl3Util/ftf/FtfPara.h"

int const USE_SEGMENT= 1 ;
int const USE_FOLLOW = 2 ;
int const GO_DOWN    =-1 ;
int const GO_UP      = 1 ;

class FtfTrack : public FtfBaseTrack {
	  
public:
   friend class FtfFinder ;

   void      add                   ( FtfHit   *thisHit, int way ) ;
   void      add                   ( FtfTrack *thisTrack ) ;
   int       buildTrack            ( FtfHit *firstHit, FtfContainer *volume ) ;
   void      dEdx                  ( ) ;
   void      deleteCandidate       ( ) ;
   void      fill                  ( ) ;
   void      fillPrimary           ( double &xc, double &yc, double &rc,
                                     double xPar, double yPar ) ;
   void      fillSecondary         ( double &xc, double &yc, double xPar, double yPar ) ;
   int       follow                ( FtfContainer *volume, int way, int rowToStop ) ;
   int       followHitSelection    ( FtfHit *baseHit, FtfHit *candidateHit ) ;
   FtfTrack* getNextTrack ( )      { return (FtfTrack *)nxatrk ; } ; 
   int       mergePrimary          ( FtfContainer   *trackArea ) ;
   void      reset                 ( ) ;
   FtfHit    *seekNextHit          ( FtfContainer  *volume, FtfHit *baseHit,
	                             int nradiusSteps, int whichFunction ) ;
   int     segment               ( FtfContainer *volume, int way ) ;
   int     segmentHitSelection ( FtfHit *baseHit, FtfHit *candidateHit ) ;
   void    *nxatrk  ;      
        
#ifdef DEBUG
   void debugAsk                 ( ) ;
   void debugDeleteCandidate     ( ) ;
   void debugFill                ( ) ;
   void debugFollowCandidate     ( FtfHit *candidate_hit ) ;
   void debugFollowSuccess       ( double dxy, double dsz, double lchi2_xy,
                                   double lchi2_sz, double chi2_min,
                                   FtfHit *candidate_hit ) ;
   void debugInVolume            ( FtfHit *base_hit, FtfHit *current_hit ) ;
   void debugNew                 ( ) ;
#endif
		
   float   lastXyAngle ;    // Angle in the xy plane of line connecting to last hits        
   typedef double vfit ;
		
   vfit    xRefHit ;
   vfit    yRefHit ;
   vfit    xLastHit ;
   vfit    yLastHit ;

   vfit    s11Xy  ;       // Fit Parameters
   vfit    s12Xy  ;
   vfit    s22Xy  ;
   vfit    g1Xy   ;
   vfit    g2Xy   ;       
   vfit    s11Sz  ;
   vfit    s12Sz  ;
   vfit    s22Sz  ;
   vfit    g1Sz   ;
   vfit    g2Sz   ; 

   vfit    ddXy, a1Xy, a2Xy ;    /*fit par in xy */
   vfit    ddSz, a1Sz, a2Sz ;    /*fit par in sz */
//private:
   inline virtual   void nextHit (){ currentHit = ((FtfBaseHit *)currentHit)->nextTrackHit ; } ;
	   
} ;
#endif

