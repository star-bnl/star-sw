#ifndef FTFTRACK
#define FTFTRACK
#include "FtfGeneral.h"
#include "FtfBaseTrack.h"
#include "FtfHit.h"
#include "FtfPara.h"
#include "FtfVolume.h"

int const USE_SEGMENT= 1 ;
int const USE_FOLLOW = 2 ;
int const GO_DOWN    =-1 ;
int const GO_UP      = 1 ;

class FtfTrack : public FtfBaseTrack {
	  
public:
   friend class FtfFinder ;

   void    Add                     ( FtfHit   *this_hit, int way ) ;
   void    Add                     ( FtfTrack *this_track ) ;
   int     Build_Track             ( FtfHit *first_hit, VOLUME *volume ) ;
   void    dEdx                    ( ) ;
   void    Delete_Candidate        ( ) ;
   void    Fill                    ( ) ;
   void    Fill_Primary            ( double &xc, double &yc, double &rc ) ;
   void    Fill_Secondary          ( double &xc, double &yc, double &rc) ;
   int     Follow                  ( VOLUME *volume, int way, int row_to_stop ) ;
   int     Follow_Hit_Selection    ( FtfHit *base_hit, FtfHit *candidate_hit ) ;
   int     Merge_Primary           ( AREA   *track_area ) ;
   void    Reset                   ( ) ;
   FtfHit  *Seek_Next_Hit          ( VOLUME  *volume, 
                                     FtfHit *base_hit,
			             int     n_r_step,
                                     int     which_function ) ;
   int     Segment                 ( VOLUME *volume, int way ) ;
   int     Segment_Hit_Selection   ( FtfHit *base_hit, FtfHit *candidate_hit ) ;
   FtfTrack *nxatrk  ;      
        
#ifdef DEBUG
   void Debug_Ask                  ( ) ;
   void Debug_Delete_Candidate     ( ) ;
   void Debug_Fill                 ( ) ;
   void Debug_Follow_Candidate     ( FtfHit *candidate_hit ) ;
   void Debug_Follow_Success       ( float dxy, float dsz, float lchi2_xy,
                                     float lchi2_sz, float chi2_min,
                                     FtfHit *candidate_hit ) ;
   void Debug_in_Volume            ( FtfHit *base_hit, FtfHit *current_hit ) ;
   void Debug_New                  ( ) ;
#endif

		
   float  last_xy_angle ;    // Angle in the xy plane of line connecting to last hits        
   FtfHit* ref_hit ; // Hit use as reference for secondary tracks
		

   typedef double vfit ;

   vfit    s11_xy  ;       // Fit Parameters
   vfit    s12_xy  ;
   vfit    s22_xy  ;
   vfit    g1_xy   ;
   vfit    g2_xy   ;       
   vfit    s11_sz  ;
   vfit    s12_sz  ;
   vfit    s22_sz  ;
   vfit    g1_sz   ;
   vfit    g2_sz   ; 

   vfit    dd_xy, a1_xy, a2_xy ;    /*fit par in xy */
   vfit    dd_sz, a1_sz, a2_sz ;    /*fit par in sz */
   float   strack ;
private:
   inline virtual   void next_hit (){ current_hit = current_hit->nxthit ; } ;

	   
   } ;
#endif

