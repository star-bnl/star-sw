#ifndef FTFTRACK
#define FTFTRACK
#include "FTF_general.h"
#include "BaseTrack.h"
#include "FTF_Hit.h"
#include "FTF_Para.h"
#include "FTF_Volume.h"

int const USE_SEGMENT= 1 ;
int const USE_FOLLOW = 2 ;
int const GO_DOWN    =-1 ;
int const GO_UP      = 1 ;

class FTF_Track : public BaseTrack {
	  
public:
     friend class FTFinder ;

    void    Add                     ( FTF_Hit   *this_hit, int way ) ;
    void    Add                     ( FTF_Track *this_track ) ;
    int     Build_Track             ( FTF_Hit *first_hit, VOLUME *volume ) ;
    void    dEdx                    ( ) ;
    void    Delete_Candidate        ( ) ;
    void    Fill                    ( ) ;
    void    Fill_Primary            ( double &xc, double &yc, double &rc ) ;
    void    Fill_Secondary          ( double &xc, double &yc, double &rc) ;
    int     Follow                  ( VOLUME *volume, int way, int row_to_stop ) ;
    int     Follow_Hit_Selection    ( FTF_Hit *base_hit, FTF_Hit *candidate_hit ) ;
    int     Merge_Primary           ( AREA   *track_area ) ;
    void    Reset                   ( ) ;
    FTF_Hit *Seek_Next_Hit          ( VOLUME  *volume, 
                                      FTF_Hit *base_hit,
                                      int     n_r_step,
                                      int     which_function ) ;
    int     Segment                 ( VOLUME *volume, int way ) ;
    int     Segment_Hit_Selection   ( FTF_Hit *base_hit, FTF_Hit *candidate_hit ) ;
    FTF_Track *nxatrk  ;      
        
#ifdef DEBUG
    void Debug_Ask                  ( ) ;
    void Debug_Delete_Candidate     ( ) ;
    void Debug_Fill                 ( ) ;
    void Debug_Follow_Candidate     ( FTF_Hit *candidate_hit ) ;
    void Debug_Follow_Success       ( float dxy, float dsz, float lchi2_xy,
                                      float lchi2_sz, float chi2_min,
                                      FTF_Hit *candidate_hit ) ;
    void Debug_in_Volume            ( FTF_Hit *base_hit, FTF_Hit *current_hit ) ;
    void Debug_New                  ( ) ;
#endif
		
   float  last_xy_angle ;    // Angle in the xy plane of line connecting to last hits        
		
private:
   inline virtual   void next_hit (){ current_hit = current_hit->nxthit ; } ;

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

   FTF_Hit* ref_hit ; // Hit use as reference for secondary tracks
	   
 } ;
#endif

