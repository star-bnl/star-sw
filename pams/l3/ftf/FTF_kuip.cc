//:>------------------------------------------------------------------
//: FILE:       FTF_kuip.cpp
//: HISTORY:
//:             03nov1996 version 1.00
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: FUNCTIONS:   ftf_kuip_*
//: DESCRIPTION: Interface between FTF and kuip
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include <math.h>
#include <string.h>
#include "cfortran.h"
#include "kuip.h"
#include "TrackerFrame.h"
#include "FTFinder.h"

extern    TrackerFrame    frame ;
extern    FTFinder        tracker ;

//****************************************************************************
//      Variable_set
//      Set variables
//****************************************************************************
#ifndef NT
extern "C" void ftf_kuip_variable_set_ ( )
#else
extern "C" void FTF_kuip_variable_set  ( )
#endif
{
   FTF_Para *para = tracker.para ;

   char variable[3] ;
   int value ;
   float rvalue ;

   strcpy ( variable, ku_getc (  ) ) ;

   value  = ku_geti ( ) ;
   rvalue = (float)value ;

   if       ( strcmp(variable,"BFI")==0 ) para->bfield          = rvalue/10 ;
#ifdef DEBUG
   else if  ( strcmp(variable,"DBL")==0 ) para->debug_level     = value ;
   else if  ( strcmp(variable,"DBH")==0 ) para->debug_hit       = value ;
#endif
   else if  ( strcmp(variable,"DPH")==0 ) para->dphi            = rvalue/1000 ;
   else if  ( strcmp(variable,"DPM")==0 ) para->dphi_merge      = rvalue/1000 ;
   else if  ( strcmp(variable,"DED")==0 ) para->dEdx            = value  ;
   else if  ( strcmp(variable,"DEM")==0 ) para->deta_merge      = rvalue/1000 ;
   else if  ( strcmp(variable,"DEN")==0 ) para->dEdx_n_truncate = value  ;
   else if  ( strcmp(variable,"DET")==0 ) para->deta            = rvalue/1000 ; 
   else if  ( strcmp(variable,"DRS")==0 ) para->dr_segm         = value ;
   else if  ( strcmp(variable,"DRT")==0  ) para->dr_track        = value ;
   else if  ( strcmp(variable,"EMI")==0 ) {
           para->init            = 0 ;
           para->eta_min = rvalue/10.F ;
   }
   else if  ( strcmp(variable,"EMA")==0 ) {
           para->init            = 0 ;
           para->eta_max = rvalue/10.F ;
   }
   else if  ( strcmp(variable,"FPM")==0 ) para->pt_min_helix_fit= rvalue / 10 ;
   else if  ( strcmp(variable,"GBK")==0 ) para->go_backwards    = value ;
   else if  ( strcmp(variable,"GDD")==0 ) para->good_distance   = rvalue / 100 ;
   else if  ( strcmp(variable,"GER")==0 ) para->get_errors      = value ;
   else if  ( strcmp(variable,"GHO")==0 ) para->ghost_flag      = value ;
   else if  ( strcmp(variable,"MDR")==0 ) para->mod_row         = value ;
   else if  ( strcmp(variable,"MDS")==0 ) para->max_dis_segm    = rvalue ;
   else if  ( strcmp(variable,"MER")==0 ) {
	   para->init            = 0 ;
	   para->merge_primaries = value ;
   }
   else if  ( strcmp(variable,"NPH")==0 ) {
		para->init            = 0 ;
		para->n_phi           = value ;
   }
   else if  ( strcmp(variable,"NPT")==0 ) {
		para->init            = 0 ;
		para->n_phi_track     = value ;
   }  
   else if  ( strcmp(variable,"NET")==0 ) {
	   para->init            = 0 ;
	   para->n_eta           = value ;
   }
   else if  ( strcmp(variable,"NPP")==0 ) para->n_pass_primaries= value ;
   else if  ( strcmp(variable,"NPS")==0 ) para->n_pass_secondaries= value ;
   else if  ( strcmp(variable,"NTT")==0 ) {
	   para->init            = 0 ;
	   para->n_eta_track     = value ;
   }
   else if  ( strcmp(variable,"PMI")==0 ) {
	   para->init            = 0 ;
	   para->phi_min = rvalue/To_deg ;
   }
   else if  ( strcmp(variable,"PMA")==0 ) {
	   para->init            = 0 ;
	   para->phi_max = rvalue/To_deg ;
   }
   else if  ( strcmp(variable,"RND")==0 ) {
	   para->init            = 0 ;
	   para->row_end         = value ;
   }
   else if  ( strcmp(variable,"RST")==0 ) {
	   para->row_start       = value ;
	   para->init            = 0 ;
   }
   else if  ( strcmp(variable,"SER")==0 ) para->xy_error_scale  = (float)value ;
   else if  ( strcmp(variable,"SEZ")==0 ) para->sz_error_scale  = (float)value ;
   else if  ( strcmp(variable,"SMA")==0 ) para->segment_max_angle= (float)value/To_deg ;
   else if  ( strcmp(variable,"SZF")==0 ) para->sz_fit_flag     = value ;
   else if  ( strcmp(variable,"CHC")==0 ) para->chi2_hit_cut    = (float)value ;
   else if  ( strcmp(variable,"CHG")==0 ) para->chi2_hit_good   = (float)value ;
   else if  ( strcmp(variable,"CTC")==0 ) para->chi2_track_cut  = (float)value ;
   else if  ( strcmp(variable,"CTC")==0 ) para->chi2_track_cut  = (float)value ;
   else if  ( strcmp(variable,"MHT")==0 ) para->mn_hit_trk      = value ;
   else if  ( strcmp(variable,"NHS")==0 ) para->n_hit_segm      = value ;
   else if  ( strcmp(variable,"VWE")==0 ) para->xy_weight_vertex= rvalue/1000 ;
   else if  ( strcmp(variable,"XVX")==0 ) para->x_vertex        = rvalue ;
   else if  ( strcmp(variable,"YVX")==0 ) para->y_vertex        = rvalue ;
   else if  ( strcmp(variable,"ZVX")==0 ) para->z_vertex        = rvalue ;
   else
   {
	  printf ( "\n Magnetic Field  (bfi) %f ", para->bfield  ) ;
#ifdef DEBUG
      printf ( "\n Debug Level     (dbl) %d ", para->debug_level  ) ;
      printf ( "\n Debug Hit       (dbh) %d ", para->debug_hit    ) ;
#endif
      printf ( "\n Phi range       (dph) %f ", para->dphi         ) ;
      printf ( "\n Track mer phi   (dpm) %f ", para->dphi_merge   ) ;
      printf ( "\n dEx switch      (ded) %d ", para->dEdx         ) ;
      printf ( "\n Track mer eta   (dem) %f ", para->deta_merge   ) ;
      printf ( "\n dEdx n trunca.  (den) %d ", para->dEdx_n_truncate   ) ;
      printf ( "\n Eta search range(det) %f ", para->deta         ) ;
      printf ( "\n R range for seg (drs) %d ", para->dr_segm      ) ;
      printf ( "\n R range for trk (drt) %d ", para->dr_track     ) ;
      printf ( "\n eta_min         (emi) %f ", para->eta_min ) ;
      printf ( "\n eta_min         (ema) %f ", para->eta_max ) ;
      printf ( "\n Fit Tracks      (fpm) %f ", para->pt_min_helix_fit ) ;
      printf ( "\n Go Backwards    (gbk) %d ", para->go_backwards ) ;
      printf ( "\n Segm good dist. (gdd) %f ", para->good_distance   ) ;
      printf ( "\n Get Errors      (ger) %d ", para->get_errors   ) ;
      printf ( "\n Ghost flag      (gho) %d ", para->ghost_flag   ) ;
      printf ( "\n Max dis segment (mds) %f ", para->max_dis_segm    ) ;
      printf ( "\n Module row      (mdr) %d ", para->mod_row         ) ;
      printf ( "\n Merge prim.     (mer) %d ", para->merge_primaries ) ;
      printf ( "\n n_phi           (nph) %d ", para->n_phi       ) ;
      printf ( "\n n_phi_track     (npt) %d ", para->n_phi_track ) ;
      printf ( "\n n_eta           (net) %d ", para->n_eta       ) ;
      printf ( "\n n_pass_prima.   (npp) %d ", para->n_pass_primaries) ;
      printf ( "\n n_pass_secon.   (nps) %d ", para->n_pass_secondaries ) ;
      printf ( "\n n_eta_track     (ntt) %d ", para->n_eta_track ) ;
      printf ( "\n phi_min         (pmi) %f ", para->phi_min*To_deg ) ;
      printf ( "\n phi_max         (pma) %f ", para->phi_max*To_deg ) ;
      printf ( "\n Row end         (rnd) %d ", para->row_end     ) ;
      printf ( "\n Row start       (rst) %d ", para->row_start   ) ;
      printf ( "\n xy scale error  (ser) %f ", para->xy_error_scale  ) ;
      printf ( "\n sz scale error  (sez) %f ", para->sz_error_scale  ) ;
      printf ( "\n Seg. max angle  (sma) %f ", para->segment_max_angle*To_deg  ) ;
      printf ( "\n sz fit flag     (szf) %d ", para->sz_fit_flag     ) ;
      printf ( "\n Chi2 hit cut    (chc) %f ", para->chi2_hit_cut    ) ;
      printf ( "\n Chi2 hit good   (chg) %f ", para->chi2_hit_good   ) ;
      printf ( "\n Chi2 track cut  (ctc) %f ", para->chi2_track_cut  ) ;
      printf ( "\n Min hits per trk(mht) %d ", para->mn_hit_trk      ) ;
      printf ( "\n # hits segment  (nhs) %d ", para->n_hit_segm      ) ;
      printf ( "\n xy Weight Vertex(vwe) %f ", para->xy_weight_vertex ) ;
	  printf ( "\n X vertex        (xvx) %f ", para->x_vertex ) ;
	  printf ( "\n Y vertex        (yvx) %f ", para->y_vertex ) ;
	  printf ( "\n Z vertex        (zvx) %f ", para->z_vertex ) ;
      printf ( "\n  " ) ;
   }
}
