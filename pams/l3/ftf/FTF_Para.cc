//:>------------------------------------------------------------------
//: FILE:       FTF_Para.cpp
//: HISTORY:
//:             28oct1996 version 1.00
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FTF_Para
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include "FTFinder.h"


void FTF_Para::setDefaults (void)
{
/*  Define cuts - this should be obsolete */

   mod_row           = 1    ;
   chi2_hit_cut      = 500.F  ;
   chi2_hit_good     = 100.F ;
   chi2_track_cut    = 250.F ;
   dr_segm           = 1     ;
   dr_track          = 3     ;
   dEdx              = 0     ;
   dEdx_n_truncate   = 20    ;
   dphi              = 0.10F * mod_row ;
   deta              = 0.10F * mod_row ;
   dphi_merge        = 0.02F  ;
   deta_merge        = 0.02F  ;
   eta_min           = -2.5F  ;
   eta_min_track     = -2.2F  ;
   eta_max           =  2.5F  ;
   eta_max_track     =  2.2F  ;
   event_reset       =  1     ;
   get_errors        =  0     ;
   ghost_flag        =  0     ;
   go_backwards      =  0     ;
   good_distance     =  1.F * mod_row ;
   init              =  0 ;
   merge_primaries   =  0    ;
   phi_min           =  (float)(-0.000001/To_deg)  ;
   phi_min_track     =  (float)(-0.000001/To_deg)  ;
   phi_max           = (float)(360.2/To_deg)  ;
   phi_max_track     = (float)(360.2/To_deg)  ;
   max_dis_segm      = 100.F * mod_row ;
   mn_hit_trk        = 5      ;
   n_hit_segm        = 3      ;
   n_eta             = 60     ;
   n_eta_track       = 60     ;
   n_phi             = 20     ;
   n_phi_track       = 60     ;
   n_pass_primaries  = 1      ;
   n_pass_secondaries= 0      ;
   row_end           = 1      ;
   row_start         = 45     ;
   segment_max_angle = 10.F/To_deg ;
   sz_fit_flag       = 1      ;
   xy_error_scale    = 1.0F   ;
   sz_error_scale    = 1.0F   ;
   bfield            = 0.5F   ;
   phiShift          = 0.0    ;
   
   pt_min_helix_fit  = 100.F  ;
   r_vertex          = 0.F    ;
   x_vertex          = 0.F    ;
   y_vertex          = 0.F    ;
   z_vertex          = 0.F    ;
   dx_vertex         = 0.005F ;
   dy_vertex         = 0.005F ;
   phi_vertex        = 0.F    ;

   return  ;
}
