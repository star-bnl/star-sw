//:>------------------------------------------------------------------
//: FILE:       ctg.cc
//: HISTORY:
//:
//:             10oct96-      ppy- STAF version        
//:
//:<------------------------------------------------------------------
#include <math.h>
#include "ctg.h"
#include "ctfgeo.h"

extern "C" void    MessageOut( const char *msg );

float const Pi    = acos(-1.) ;
float const Twopi = 2. * Pi ;
float const Todeg = 360. / Twopi ;


long type_of_call ctg_(
  TABLE_HEAD_ST     *geo_h,      CTG_GEO_ST       *geo,
  TABLE_HEAD_ST     *slat_phi_h, CTG_SLAT_PHI_ST  *slat_phi,
  TABLE_HEAD_ST     *slat_eta_h, CTG_SLAT_ETA_ST  *slat_eta,    
  TABLE_HEAD_ST     *slat_h,     CTG_SLAT_ST      *slat     ) {
//:>--------------------------------------------------------------------
//: MODULE:      CTG
//: DESCRIPTION: Sets CTF(CTB/TOF) Geometry tables
//: AUTHOR:      Pablo Yepes, ppy, yepes@physics.rice.edu
//: ARGUMENTS:y
//:          IN:
//:       INOUT:
//:             Geo             - General geometry parameters
//:             Slat_phi        - Slat phi parameters
//:             Slat_eta        - Slat eta parameters
//:             Slat            - Slat parameters
//:         OUT:
//: RETURNS:    STAF Condition Value
//:>--------------------------------------------------------------------
//
   int   message_id = 0 ;
   char  OutMessage[100] ;
   float const Todeg = 360. / Twopi ;
//
//    Check tables
//
   if ( geo_h->maxlen != 1 ) {
     sprintf ( OutMessage, " Geo maxlen = %d is not valid ", geo_h->maxlen ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
   }
//
//    Check tables
//
   long n_phi = geo->n_counter_phi * geo->n_tray_phi ;
   long n_eta = geo->n_counter_eta * geo->n_tray_eta ;
//
//    Table with one entry per counter in phi
//
   if ( slat_phi_h->maxlen < n_phi || slat_phi_h->maxlen < 1 ) {
     sprintf ( OutMessage, " slat_phi maxlen = %d is not valid ", slat_phi_h->maxlen ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
   }
//
//    Table with one entry per counter in eta
//
   if ( slat_eta_h->maxlen < n_eta || slat_eta_h->maxlen < 1 ) {  
     sprintf ( OutMessage, " slat_eta maxlen = %d is not valid ", slat_eta_h->maxlen ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
   }
//
//    Table with one entry per counter in eta
//
   if ( slat_h->maxlen < n_eta * n_phi || slat_eta_h->maxlen < 1 ) {
     sprintf ( OutMessage, " slat maxlen = %d is not valid ", slat_h->maxlen ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
   }

//
//       Set initialize flag to true
//
   geo->init  = 1 ;
//
//    Check detector limits
//
   if ( geo->i_eta_min < 1  ) {
     sprintf ( OutMessage, " Ieta min = %d is not valid ", geo->i_eta_min ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
   }
//
   if ( geo->i_eta_max > n_eta  ) {
     sprintf ( OutMessage, " Ieta max = %d is not valid ", geo->i_eta_max ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
   }
//
   if ( geo->i_phi_min < 1  ) {
     sprintf ( OutMessage, " Iphi min = %d is not valid ", geo->i_phi_min ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
   }
//
   if ( geo->i_phi_max > n_phi  ) {
     sprintf ( OutMessage, " Iphi max = %d is not valid ", geo->i_phi_max ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
   }
//
//    Check slat dimensions
//
   if ( geo->counter_width * geo->n_counter_phi > geo->tray_width ) {   
     sprintf ( OutMessage, " Slat and Tray length are not consistent " ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
   }
//
//    Calculate eta max & min for each slat along eta
//
   float angle, dr, r_min, r_max ;
   float theta, theta_min, theta_max ;
//
//     Loop over slat in eta
//
   for ( int i = 0 ; i < n_eta ; i++ ) {
//
      slat_eta[i].ieta = i + 1 ;
//
      slat_eta[i].z    = ( slat_eta[i].z_min + slat_eta[i].z_max ) / 2. ;
//
//    Calculate delta r (dr) in half a slat
//
      angle = tan ( acos ( slat_eta[i].cosang ) ) ;
      dr    = fabs( ( slat_eta[i].z_max - slat_eta[i].z_min ) / 2. * angle ) ;
//
      r_min  = slat_eta[i].r - dr ;
      r_max  = slat_eta[i].r + dr ;
// 
//    Theta for middle and extremes of slat
//
      theta     = atan2(slat_eta[i].r,slat_eta[i].z    ) ;
      theta_min = atan2(r_min,        slat_eta[i].z_min) ;
      theta_max = atan2(r_max,        slat_eta[i].z_max) ;
// 
//     Pseudorapidity for middle and extremes of slat
//
      slat_eta[i].eta     = -log(tan(theta/2.))     ;
      slat_eta[i].eta_min = -log(tan(theta_min/2.)) ;
      slat_eta[i].eta_max = -log(tan(theta_max/2.)) ;
//
   } // End slat_eta loop
//
//    Now go for slat characteristics in phi
//
//    Loop over trays
//

   long    i_phi, i_tray, i_counter ;
   float  delta_phi_tray = Twopi / geo->n_tray_phi ;
   float  phi_tray, y0 ;

   for ( i_tray = 0 ; i_tray < geo->n_tray_phi ; i_tray++ ) {
//
//     Get angle center tray
//
      phi_tray   = geo->tray_phi_zero + i_tray * delta_phi_tray ;
//
//    Loop over counters in tray
//
      for ( i_counter = 0 ; i_counter < geo->n_counter_phi ; i_counter++ ) {
//
//     Get general index
//
         i_phi = i_tray * geo->n_counter_phi + i_counter ;
//
//     Coordinates center counter
//
         y0  = - geo->tray_width + 2. * (float(i_counter)+0.5) * geo->counter_width ; 
// 
//    Get phi angles 
//
         slat_phi[i_phi].iphi    = i_phi + 1 ;
         slat_phi[i_phi].phi     = Todeg * ( phi_tray + atan2(y0, slat_eta[0].r ) ) ;
         if ( slat_phi[i_phi].phi < 0     ) slat_phi[i_phi].phi+=Twopi ; 
         if ( slat_phi[i_phi].phi > 360 ) slat_phi[i_phi].phi-=Twopi ; 
         slat_phi[i_phi].phi_min = Todeg * ( phi_tray + atan2(y0-geo->counter_width, slat_eta[0].r ) ) ;
         if ( slat_phi[i_phi].phi_min < 0     ) slat_phi[i_phi].phi_min+=360.F;
         if ( slat_phi[i_phi].phi_min > 360 ) slat_phi[i_phi].phi_min-=360.F ;
         slat_phi[i_phi].phi_max = Todeg * ( phi_tray + atan2(y0+geo->counter_width, slat_eta[0].r ) ) ;
         if ( slat_phi[i_phi].phi_max < 0     ) slat_phi[i_phi].phi_max+=360.F ;
         if ( slat_phi[i_phi].phi_max > 360 ) slat_phi[i_phi].phi_max-=360.F ;
 
//
      } // End loop over counters
   } // End loop over trays
//
//   Set number entries
//
   slat_phi_h->nok = n_phi ;
   slat_eta_h->nok = n_eta ;
//
//   That's it 
//
   return STAFCV_OK ;
}
long ctg_i_eta ( float z,
                 TABLE_HEAD_ST *slat_eta_h, CTG_SLAT_ETA_ST  *slat_eta ) {
//:>--------------------------------------------------------------------
//: FUNCTION:    ctg_i_eta
//: DESCRIPTION: Finds eta index given the z position
//: AUTHOR:      Pablo Yepes, ppy, yepes@physics.rice.edu
//: ARGUMENTS:y
//:          IN:
//:             z               - z coordinate
//:             Slat_eta_h      - Slat_eta header table 
//:             Slat_eta        - Slat_eta table
//: RETURNS:    eta index of hit counter
//:>--------------------------------------------------------------------
//
   char  OutMessage[100] ;
//
//      Look for right slat, chose last closest to hit
//
   long  i_sel = 0 ;
//
//      Look for write slat
//
   for ( int i_eta = 0 ; i_eta < slat_eta_h->nok ; i_eta++ ) {
      if ( z > 0 ) {
         if ( z > slat_eta[i_eta].z_min  &&  
              z < slat_eta[i_eta].z_max ) { 
           i_sel = slat_eta[i_eta].ieta ;
           break ; 
         }
      }
      else {
         if ( z < slat_eta[i_eta].z_min  &&
              z > slat_eta[i_eta].z_max ) {
            i_sel = slat_eta[i_eta].ieta ;
            break ;
         }
      }
   }
//
//   If value was found return
//
   if ( i_sel > 1 ) return i_sel ;
//
//  If value not found take closest slat
//
   float dz ;
   float z_min = 210.F ;
   for ( i_eta = 0 ; i_eta < slat_eta_h->nok ; i_eta++ ) { 
      dz = fabs(z-slat_eta[i_eta].z_min) ; 
      if ( dz < z_min ) {
         z_min = dz ; 
         i_sel = slat_eta[i_eta].ieta ;
      }
      dz = fabs(z-slat_eta[i_eta].z_max) ;
      if ( dz < z_min ) {
         z_min = dz ;
         i_sel = slat_eta[i_eta].ieta ;
      }
   }
//
   if ( i_sel < 1 ) { 
      sprintf ( OutMessage, " ctg_i_eta, z = %f outside acceptance ", z ) ;
      MessageOut ( OutMessage ) ;
   }
   return i_sel ;
}
long ctg_i_phi ( float phi,
                 TABLE_HEAD_ST *geo_h,      CTG_GEO_ST       *geo,
                 TABLE_HEAD_ST *slat_phi_h, CTG_SLAT_PHI_ST  *slat_phi ) {
//:>--------------------------------------------------------------------
//: FUNCTION:    ctg_i_phi
//: DESCRIPTION: Finds phi index given the phi angle 
//: AUTHOR:      Pablo Yepes, ppy, yepes@physics.rice.edu
//: ARGUMENTS:y
//:          IN:
//:             z               - z coordinate
//:             geo_h           - Geometry table header
//:             geo             - Geometry table
//:             Slat_phi_h      - Slat phi header 
//:             Slat_phi        - Slat phi 
//: RETURNS:    phi index of hit counter
//:>--------------------------------------------------------------------
//
   char  OutMessage[100] ;
   long  n_phi = geo->n_counter_phi * geo->n_tray_phi ;
//
//   Assume all counters cover the same fraction of phi
//   (Pretty good approximation) and guess phi index
//
   phi -= geo->tray_phi_zero; 
   if ( phi < 0     ) phi = phi + Twopi ;
   if ( phi > Twopi ) phi = phi - Twopi ;
// long phi_guess = phi / ( Twopi / n_phi ) + 1 ; 
   long phi_guess = phi / ( Twopi / n_phi )  ; 
//
   long i_phi_next, right_left = 1 ;
   float phi_min, phi_max ;
//
   short counter = 0 ;
   for ( long i_phi = phi_guess ;; i_phi+=right_left ) {
//
//    Get index of next counter
//
         if ( i_phi      < 1     ) i_phi      = i_phi + n_phi ;
         if ( i_phi      > n_phi ) i_phi      = i_phi - n_phi ;
         i_phi_next = i_phi + 2 * right_left ;
         if ( i_phi_next < 1     ) i_phi_next = i_phi_next + n_phi ;
         if ( i_phi_next > n_phi ) i_phi_next = i_phi_next - n_phi ;
//
//   Phi min and max depending on direction
//
         if ( right_left > 0 ) {
            phi_min = slat_phi[i_phi-1].phi_max / Todeg ;
            phi_max = slat_phi[i_phi_next-1].phi_min / Todeg ;
         } 
         else {
            phi_min = slat_phi[i_phi_next-1].phi_max / Todeg ;
            phi_max = slat_phi[i_phi-1].phi_min / Todeg ;
         }
//
//   Check whether phi is in the current counter range
//       
         if ( phi > phi_min ) {
              if ( phi_max < phi_min ) phi_max+=Twopi ;
              if ( phi < phi_max ) return i_phi + right_left  ;
         }
         else if ( phi < phi_max ) {
              if ( phi_min > phi_max ) phi_min-=Twopi ;
              if ( phi > phi_min ) return i_phi + right_left ;
         }
//
//    Check whether we should change directions
//
         if ( right_left < 1 && phi > phi_max ) right_left =  1 ;
         if ( right_left > 1 && fabs(phi-phi_min) < Pi
                             && phi < phi_min ) right_left = -1 ;
//
//    Avoid infinite loop
//
         counter++ ;
         if ( counter > n_phi ) break ;
    }
//
//
    sprintf ( OutMessage, " ctg_i_phi, phi = %f outside acceptance ", phi ) ;
    MessageOut ( OutMessage ) ;
    return 0 ;
}
//
long ctg_index ( long i_phi, long i_eta, long n_eta ){ 
//:>--------------------------------------------------------------------
//: FUNCTION:    ctg_index
//: DESCRIPTION: Gets 1 dimensional index from i_iphi & i_eta
//: AUTHOR:      Pablo Yepes, ppy, yepes@physics.rice.edu
//: ARGUMENTS:y
//:          IN:
//:             i_phi           - phi index   
//:             i_eta           - eta index  
//:             n_eta           - Number counters in eta
//: RETURNS:    Global index             
//:>--------------------------------------------------------------------
//
   long i ;
   return (n_eta) * (i_phi-1) + (i_eta- 1) ; 
} 

