#ifndef FTFTRACK
#define FTFTRACK
#include "FtfGeneral.h"
#include "FtfBaseTrack.h"
#include "FtfHit.h"
#include "FtfPara.h"
#include "FtfVolume.h"
#ifdef SL3ROOT
#include "Rtypes.h"
#else
#define ClassDef(a,b)
#endif


int const USE_SEGMENT= 1 ;
int const USE_FOLLOW = 2 ;
int const GO_DOWN    =-1 ;
int const GO_UP      = 1 ;

class FtfTrack : public FtfBaseTrack {
	  
public:
   friend class FtfFinder ;

   void    add                   ( FtfHit   *thisHit, int way ) ;
   void    add                   ( FtfTrack *thisTrack ) ;
   int     buildTrack            ( FtfHit *firstHit, VOLUME *volume ) ;
   void    dEdx                  ( ) ;
   void    deleteCandidate       ( ) ;
   void    fill                  ( ) ;
   void    fillPrimary           ( double &xc, double &yc, double &rc ) ;
   void    fillSecondary         ( double &xc, double &yc, double &rc) ;
   int     follow                ( VOLUME *volume, int way, int rowToStop ) ;
   int     followHitSelection    ( FtfHit *baseHit, FtfHit *candidateHit ) ;
   int     mergePrimary          ( AREA   *trackArea ) ;
   void    reset                 ( ) ;
   FtfHit  *seekNextHit          ( VOLUME  *volume, 
                                   FtfHit *baseHit,
			           int     nradiusSteps,
                                   int     whichFunction ) ;
   int     segment               ( VOLUME *volume, int way ) ;
   int     segmentHitSelection ( FtfHit *baseHit, FtfHit *candidateHit ) ;
   FtfTrack *nxatrk  ;      
        
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
   FtfHit* refHit ; // Hit use as reference for secondary tracks
		
   typedef double vfit ;

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
   float   trackLength ;
//private:
   inline virtual   void nextHit (){ currentHit = currentHit->nextTrackHit ; } ;
	   
   ClassDef(FtfTrack,1)
   } ;
#endif

