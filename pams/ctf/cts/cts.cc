/*:>--------------------------------------------------------------------
**: FILE:       cts.cc        
**: HISTORY:
**:             03feb97-      ppy- STAF version
**:             19mar98-      ppy- zero suppression introduced
**:                           this may mess up physical and electronic noise
**:
**:<------------------------------------------------------------------*/
#include <math.h>
#include <iostream.h>
#include <malloc.h>
#include "cts.h"
#include "ctfgeo.h"
//
//  Function definitions
//
extern "C" void    MessageOut( const char *msg );
//
void cts_get_ctf_indexes ( long detector, long volume, long &i_phi, long &i_eta ) ;
void cts_get_ctb_indexes ( long volume, long &i_phi, long &i_eta ) ;
void cts_get_tof_indexes ( long volume, long &i_phi, long &i_eta ) ;
long cts_detector_response (
                TABLE_HEAD_ST  *mhit_h,     G2T_CTF_HIT_ST  *mhit ,
                TABLE_HEAD_ST  *track_h,    G2T_TRACK_ST    *track ,
                TABLE_HEAD_ST  *geo_h,      CTG_GEO_ST      *geo ,
                TABLE_HEAD_ST  *slat_h,     CTG_SLAT_ST     *slat ,
                TABLE_HEAD_ST  *slat_phi_h, CTG_SLAT_PHI_ST *slat_phi ,
                TABLE_HEAD_ST  *slat_eta_h, CTG_SLAT_ETA_ST *slat_eta ,
                TABLE_HEAD_ST  *mpara_h,    CTS_MPARA_ST    *mpara ,
                TABLE_HEAD_ST  *mslat_h,    CTS_MSLAT_ST    *mslat ) ;

void cts_electronic_noise (
                TABLE_HEAD_ST  *geo_h,     CTG_GEO_ST     *geo,
                TABLE_HEAD_ST  *mpara_h,   CTS_MPARA_ST   *mpara,
                TABLE_HEAD_ST  *slat_h,    CTG_SLAT_ST    *slat,
                TABLE_HEAD_ST  *raw_h,     CTU_RAW_ST     *raw   ) ;
void cts_fill_event (
                TABLE_HEAD_ST  *track_h,   G2T_TRACK_ST   *track,
                TABLE_HEAD_ST  *mslat_h,   CTS_MSLAT_ST   *mslat,    
                TABLE_HEAD_ST  *event_h,   CTS_EVENT_ST   *event ) ;

void cts_fill_raw (
                TABLE_HEAD_ST  *geo_h,     CTG_GEO_ST     *geo,
                TABLE_HEAD_ST  *mpara_h,   CTS_MPARA_ST   *mpara,
                TABLE_HEAD_ST  *mslat_h,   CTS_MSLAT_ST   *mslat,
                TABLE_HEAD_ST  *slat_h,    CTG_SLAT_ST    *slat,
                TABLE_HEAD_ST  *raw_h,     CTU_RAW_ST     *raw   ) ;
void cts_physical_noise ( 
                long           i_phi,      long           i_eta,
                long&          n_slats_on,
                long           n_phe,      float          time,
                TABLE_HEAD_ST  *mslat_h,   CTS_MSLAT_ST   *mslat,
                TABLE_HEAD_ST  *geo_h,     CTG_GEO_ST     *geo   ) ;
float cts_slat_response_exp ( float& dz,
                              CTS_MPARA_ST* mpara ) ;
float cts_slat_response_table ( float& z, float& d, float& tof,
                                CTS_MPARA_ST* mpara ) ;


//
extern "C" float rg32_() ;
extern "C" float rndm_() ;
extern "C" void  hf2_ ( long* ihis, float* x, float* y, float* w );
//
//   Common variables
//
long  const max_local_index = 6000 ;
long  local_index[max_local_index] ;
float const Pi    = acos(-1.) ;
float const Todeg = 180. / Pi ;
#define min(a,b)    ( ( (a) < (b) ) ? (a) : (b) )
//
extern "C" long type_of_call cts_(
  TABLE_HEAD_ST           *mhit_h,    G2T_CTF_HIT_ST             *mhit ,
  TABLE_HEAD_ST          *track_h,      G2T_TRACK_ST            *track ,
  TABLE_HEAD_ST            *geo_h,        CTG_GEO_ST              *geo ,
  TABLE_HEAD_ST           *slat_h,       CTG_SLAT_ST             *slat ,
  TABLE_HEAD_ST       *slat_phi_h,   CTG_SLAT_PHI_ST         *slat_phi ,
  TABLE_HEAD_ST       *slat_eta_h,   CTG_SLAT_ETA_ST         *slat_eta ,
  TABLE_HEAD_ST          *mpara_h,      CTS_MPARA_ST            *mpara ,
  TABLE_HEAD_ST          *event_h,      CTS_EVENT_ST            *event ,
  TABLE_HEAD_ST          *mslat_h,      CTS_MSLAT_ST            *mslat ,
  TABLE_HEAD_ST            *raw_h,        CTU_RAW_ST              *raw )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    cts_
**: DESCRIPTION: Simulates CTB/TOF response
**:  
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:             mhit_h    - header Structure for mhit
**:               mhit    - MC Hit table from g2t
**:            track_h    - header Structure for track
**:              track    - MC track info
**:              geo_h    - header Structure for geo
**:                geo    - Geometry parameters
**:             slat_h    - header Structure for slat
**:               slat    - Slat info
**:         slat_phi_h    - header Structure for slat_phi
**:           slat_phi    - Slat phi info
**:         slat_eta_h    - header Structure for slat_eta
**:           slat_eta    - Slat eta info
**:            mpara_h    - header Structure for mpara
**:              mpara    - Control this module
**:      OUT:
**:            event_h    - header Structure for event
**:              event    - Comparison MC and ADC info    
**:            mslat_h    - header Structure for mslat
**:              mslat    - Slat Monte Carlo information   
**:              raw_h    - header Structure for raw
**:                raw    - Raw data
**: 
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
   char  OutMessage[50] ;
//
//   Check there are input hits
//
   if ( mhit_h->nok < 1 ) {
     sprintf ( OutMessage, " No input hits " ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
   }
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
   if ( slat_h->maxlen < n_phi * n_eta || slat_h->maxlen < 1 ) {
     sprintf ( OutMessage, " slat maxlen = %d is not valid ", slat_h->maxlen ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
   }
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
//    Check detector id
//
   if ( geo->detector != 1 && geo->detector !=2  ) {
     sprintf ( OutMessage, " Unknown detector id = %d  ", geo->detector ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
   }
//
//    Check local index dimensions (I had problems with dynamic allocation on the ibm )
//
   if ( n_eta * n_phi > max_local_index ) {
     sprintf ( OutMessage, " Local index not long enough, %d ", max_local_index ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
   }
//
//   Initialize cts
//
   raw_h->nok    = 0 ;
   mslat_h->nok  = 0 ;
   memset ( mslat, 0, (int)mslat_h->maxlen*sizeof(mslat[0]) ) ;
   memset ( raw,   0,   (int)raw_h->maxlen*sizeof(  raw[0]) ) ;
//
//   Simulate counter response
//
   cts_detector_response ( mhit_h,   mhit, 
                           track_h,    track, 
                           geo_h,      geo,
                           slat_h,     slat, 
                           slat_phi_h, slat_phi,
                           slat_eta_h, slat_eta,
                           mpara_h,    mpara,   
                           mslat_h,    mslat ) ;
//
//    Fill raw table
//
   cts_fill_raw ( geo_h, geo, mpara_h, mpara,
                  mslat_h, mslat, slat_h, slat, raw_h, raw ) ;
//
//   Add electronic noise if requested
//
   if ( mpara->elec_noise < 0 )
      cts_electronic_noise ( geo_h, geo, mpara_h, mpara,
                             slat_h, slat, raw_h,   raw ) ;
//
//   Fill info about event
//
   cts_fill_event ( track_h, track, mslat_h, mslat, event_h, event ) ;
//
//  That's it
//
   return STAFCV_OK;
}
//
long cts_detector_response (
  TABLE_HEAD_ST           *mhit_h,    G2T_CTF_HIT_ST             *mhit ,
  TABLE_HEAD_ST          *track_h,      G2T_TRACK_ST            *track ,
  TABLE_HEAD_ST            *geo_h,        CTG_GEO_ST              *geo ,
  TABLE_HEAD_ST           *slat_h,       CTG_SLAT_ST             *slat ,
  TABLE_HEAD_ST       *slat_phi_h,   CTG_SLAT_PHI_ST         *slat_phi ,
  TABLE_HEAD_ST       *slat_eta_h,   CTG_SLAT_ETA_ST         *slat_eta ,
  TABLE_HEAD_ST          *mpara_h,      CTS_MPARA_ST            *mpara ,
  TABLE_HEAD_ST          *mslat_h,      CTS_MSLAT_ST            *mslat )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    cts_detector_response
**: DESCRIPTION: Simulates CTB/TOF detector response
**:              Loops over g2t CTF hits and fills mslat table
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:             mhit_h    - header Structure for mhit
**:               mhit    - MC Hit table from g2t
**:            track_h    - header Structure for track
**:              track    - MC track info
**:              geo_h    - header Structure for geo
**:                geo    - Geometry parameters
**:             slat_h    - header Structure for slat
**:               slat    - Slat info
**:         slat_phi_h    - header Structure for slat_phi
**:           slat_phi    - Slat phi info
**:         slat_eta_h    - header Structure for slat_eta
**:           slat_eta    - Slat eta info
**:            mpara_h    - header Structure for mpara
**:              mpara    - Control this module
**:      OUT:
**:            mslat_h    - header Structure for mslat
**:              mslat    - Monte Carlo slat information   
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
   char  OutMessage[50] ;
//
//    Total number of counters in phi and eta
//
   long n_phi = geo->n_counter_phi * geo->n_tray_phi ;
   long n_eta = geo->n_counter_eta * geo->n_tray_eta ;
   long i_phi, i_eta ;

   for ( int i = 0 ; i < n_phi*n_eta ; i++ ) local_index[i] = -1 ;
   long n_slats_on = 0 ;
//
//   Loop over hits
//
   for ( int i_hit = 0 ; i_hit < mhit_h->nok ; i_hit++ ) {
//
//   Get indexes
//   If routines return 0 something went wrong
//
      if ( mpara->geo_from_geant ){
         cts_get_ctf_indexes ( geo->detector, mhit[i_hit].volume_id, i_phi, i_eta ) ;
#ifdef TEST
         long i_eta_test = ctg_i_eta ( mhit[i_hit].x[2], slat_eta_h, slat_eta ) ;
         float phi = atan2 ( mhit[i_hit].x[1], mhit[i_hit].x[0] ) ;
         long i_phi_test = ctg_i_phi ( phi, geo_h, geo, slat_phi_h, slat_phi ) ;
         if ( i_phi != i_phi_test ){
            cout<<" phi "<<phi<<endl ;
            cout<<" iphi iphi_test "<<i_phi<<" "<<i_phi_test<<endl ;
         }
         if ( i_eta != i_eta_test ){
            cout<<" z   "<<mhit[i_hit].x[2]<<endl ;
            cout<<" ieta ieta_test "<<i_eta<<" "<<i_eta_test<<endl ;
         }
#endif
      }
      else {
         i_eta = ctg_i_eta ( mhit[i_hit].x[2], slat_eta_h, slat_eta ) ;
         float phi = atan2 ( mhit[i_hit].x[1], mhit[i_hit].x[0] ) ;
         i_phi = ctg_i_phi ( phi, geo_h, geo, slat_phi_h, slat_phi ) ;
      }
//
//     Check indexes don't go crazy
//
      if (  i_phi < geo->i_phi_min || i_phi > geo->i_phi_max
         || i_eta < geo->i_eta_min || i_eta > geo->i_eta_max ) {
           sprintf ( OutMessage, " Hit %d has Phi or Eta indexes %d %d out of range ", 
                                   i_hit, i_phi, i_eta ) ;
           MessageOut ( OutMessage ) ;
           continue ;
      } 
//
//       Go from Table index to C index
//
      i_phi-- ;
      i_eta-- ; 
//
//  Get distance to counter edge
//
      float Length = (slat_eta[i_eta].z_max - mhit[i_hit].x[2])
                     / slat_eta[i_eta].cosang ;
//
//    Check length makes sense
//
      float max_distance = ( slat_eta[i_eta].z_max - slat_eta[i_eta].z_min ) 
                           / slat_eta[i_eta].cosang ;
      if ( Length < -1 * mpara->position_tolerance || 
           Length - mpara->position_tolerance > max_distance  ) { 
           sprintf ( OutMessage, " cts: Hit %d has wrong distance: %f  ",
                                   i_hit, Length ) ;
           MessageOut ( OutMessage ) ;
           continue ;
      }
//
//    Check what kind of slat response model is requested
//    and get number of photoelectrons
//
      long n_phe ;
      if ( !(mpara->slat_para) ) {
//
//         Exponential attenuation
//
         n_phe = mhit[i_hit].de * cts_slat_response_exp ( Length, mpara ) ;
      }
      else {
//
//     Get distance to closest edge
//
         float phi    = atan2 ( mhit[i_hit].x[1], mhit[i_hit].x[0] ) ;
         if ( phi < 0 ) phi += 2. * Pi ;
         float d_phi_1 = fabs(slat_phi[i_phi].phi_min/Todeg-phi) ;
         if ( d_phi_1 > Pi ) d_phi_1 = 2. * Pi - d_phi_1 ;
         float d_phi_2 = fabs(slat_phi[i_phi].phi_max/Todeg-phi) ;
         if ( d_phi_2 > Pi ) d_phi_2 = 2. * Pi - d_phi_2 ;
         float d_edge = geo->r * min(d_phi_1,d_phi_2) ;
//
         n_phe = mhit[i_hit].de * 
                 cts_slat_response_table ( Length, d_edge, mhit[i_hit].tof, mpara ) ;
      }
//
//     Check the slat number
//
      long index = ctg_index ( i_phi+1, i_eta+1, n_eta ) ;
//
//    Verify indexes
//
      if ( slat[index].i_phi != i_phi+1 ){
         MessageOut ( " cts_detector_response: i_phi mismatch " ) ;
         continue ;
      }
//
      if ( slat[index].i_eta != i_eta+1 ){
         MessageOut ( " cts_detector_response: i_eta mismatch " ) ;
         continue ;
      }
//
      long i_slat = local_index[index] ;
      if ( i_slat < 0 ) {   
         i_slat = n_slats_on ;
         local_index[index] = i_slat ;
         mslat[i_slat].i_phi = i_phi + 1 ;
         mslat[i_slat].i_eta = i_eta + 1 ;
         n_slats_on++ ;
      }
//
//     Store # photoelectrons and deposited Energy
//     and momentum of particles going through
// 
      mslat[i_slat].pm_length = Length ;
      mslat[i_slat].z_hit     = mhit[i_hit].x[2] ;
      mslat[i_slat].n_phe    += n_phe ;
      mslat[i_slat].n_hits++ ;
      mslat[i_slat].de       += mhit[i_hit].de ;
//
//        Get measured time
//
      float time = mhit[i_hit].tof + mpara->delay * Length ;
      for ( float tt=0 ; tt <= 0 ; tt = time ) 
         tt = time + rg32_() * mpara->time_res * sqrt(Length) ;
//
//        Keep real time of fastest particle
//
      if ( mslat[i_slat].tof == 0 || mhit[i_hit].tof < mslat[i_slat].tof )    
             mslat[i_slat].tof = mhit[i_hit].tof ;
//
//        Keep measured time of fastest particle
//
      if ( mslat[i_slat].time < 0 || time < mslat[i_slat].time ) 
             mslat[i_slat].time = time ;
//
//        Keep some information about the last particle
//
      float px                = mhit[i_hit].p[0] ;
      float py                = mhit[i_hit].p[1] ;
      float pz                = mhit[i_hit].p[2] ;
      float ptot              = sqrt(px*px+py*py+pz*pz) ;
      mslat[i_slat].ptot      = ptot ;
      mslat[i_slat].mc_trk_id = mhit[i_hit].track_p ;
      mslat[i_slat].s_length  = mhit[i_hit].s_track ;
      mslat[i_slat].ds        = mhit[i_hit].ds ;
//
//     Generate noise only in phys_noise% of slats
//
      if ( rndm_() < mpara->phys_noise ) 
             cts_physical_noise ( i_phi, i_eta, n_slats_on,
                                  n_phe, time,
                                  mslat_h, mslat,   
                                  geo_h,   geo   ) ;
   } // end loop over hits
//
//   Store number of slats with signal
//
   mslat_h->nok  = n_slats_on ;
//
//  That's it
//
   return STAFCV_OK;
}
//
void cts_fill_event (
                TABLE_HEAD_ST  *track_h,   G2T_TRACK_ST   *track,
                TABLE_HEAD_ST  *mslat_h,   CTS_MSLAT_ST   *mslat,    
                TABLE_HEAD_ST  *event_h,   CTS_EVENT_ST   *event ) {
/*:>--------------------------------------------------------------------
**: ROUTINE:    cts_fill_event
**: DESCRIPTION: Fills event table with event information
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:           track_h     - MC track    header table
**:           track       - MC track    table
**:           mslat_h     - MC slat     header table
**:           mslat       - MC slat     table
**:      OUT:
**:           event_h     - event       header table
**:           event       - event       table
**:>------------------------------------------------------------------*/
   long i ;
   char  OutMessage[50] ;
//
   long n_event = event_h->nok ;
//
   if ( n_event >= event_h->maxlen ) {
     sprintf ( OutMessage, " cts_fill_event: n_event = %d > maxlen = %d  ",
                             n_event, event_h->maxlen ) ;
     MessageOut ( OutMessage ) ;
     return ;
   }

//
//   Loop over mslats
//
   event[n_event].adc_sum    = 0 ;
   event[n_event].n_ctf      = 0 ;
   event[n_event].n_ctf_prim = 0 ;
   event[n_event].n_prim     = 0 ;
//
   for ( i = 0 ; i < mslat_h->nok ; i++ ) {
      event[n_event].adc_sum += mslat[i].adc ;
      event[n_event].n_ctf   += mslat[i].n_hits ;
   }
//
   for ( i = 0 ; i < track_h->nok ; i++ ) {
      if ( track[i].start_vertex_p == 1 ) { 
         event[n_event].n_prim++ ;
         if ( track[i].hit_ctb_p || track[i].hit_tof_p )  
            event[n_event].n_ctf_prim++ ;
      }
   }
//
   event_h->nok = n_event + 1 ;
}
void cts_fill_raw (
  TABLE_HEAD_ST          *geo_h,        CTG_GEO_ST            *geo ,
  TABLE_HEAD_ST          *mpara_h,      CTS_MPARA_ST          *mpara,
  TABLE_HEAD_ST          *mslat_h,      CTS_MSLAT_ST          *mslat,
  TABLE_HEAD_ST          *slat_h,       CTG_SLAT_ST           *slat,
  TABLE_HEAD_ST          *raw_h,        CTU_RAW_ST            *raw   ) {
/*:>--------------------------------------------------------------------
**: ROUTINE:    cts_fill_raw       
**: DESCRIPTION: Fills the raw table      
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:           geo_h       - geometry    header table
**:           geo         - geometry    table 
**:           mslat_h     - MC slat     hader table
**:           mslat       - MC slat     table 
**:           slat_h      - slat        header table 
**:           slat        - slat        table
**:      OUT:
**:           raw_h       - raw         table header
**:           raw         - raw         table 
**:>------------------------------------------------------------------*/
  
//
//   Loop over slats
// 
   long index, i_raw ;
   long i_phi, i_eta, n_eta, n_phi ;
   float offset ;
//
   n_phi = geo->n_counter_phi * geo->n_tray_phi ;
   n_eta = geo->n_counter_eta * geo->n_tray_eta ;
//
//   Set Adc Tdc to zero
//
   for ( index = 0 ; index < n_eta*n_phi ; index++ ) {
       raw[index].adc = 0. ;
       raw[index].tdc = 0. ;
   }
//
   for ( int i_slat = 0 ; i_slat < mslat_h->nok ; i_slat++ ) {
//
//    Get index
//
      i_phi = mslat[i_slat].i_phi ;
      i_eta = mslat[i_slat].i_eta ;
      i_raw = index = ctg_index ( i_phi, i_eta, n_eta  ) ;
      if ( mpara->zero_suppression ) i_raw = i_slat ;
//
//     Adc 
//
      offset = slat[index].offset_adc + rg32_()* slat[index].ods_adc ;
      raw[i_raw].adc = (int)((float)mslat[i_slat].n_phe * mpara->nphe_to_adc) 
                       + offset ;
      if ( raw[i_raw].adc > mpara->adc_overflow ) 
                   raw[i_raw].adc = mpara->adc_overflow ;
      mslat[index].adc = raw[i_raw].adc ;
//
//     Tdc
//
      offset = slat[index].offset_tdc + rg32_()* (float)slat[index].ods_tdc ;
      raw[i_raw].tdc = mslat[i_slat].time / slat[index].cc_tdc + offset ;
      mslat[i_slat].tdc = raw[i_raw].tdc ;
//
      raw[i_raw].i_phi = mslat[i_slat].i_phi ; 
      raw[i_raw].i_eta = mslat[i_slat].i_eta ;
   }
//
//   If zero suppression stop here
//
   if ( mpara->zero_suppression ) {
      raw_h->nok = mslat_h->nok ;
      return ;
   }
//
//   Fill the rest of the slats with pedestals and offsets
//
   for ( index = 0 ; index < n_eta*n_phi ; index++ ) {

      if ( raw[index].adc != 0. ) continue ;

      raw[index].i_phi = slat[index].i_phi ;
      raw[index].i_eta = slat[index].i_eta ;
      raw[index].adc   = slat[index].offset_adc + rg32_()* slat[index].ods_adc ;
      raw[index].tdc   = slat[index].offset_tdc + rg32_()* slat[index].ods_tdc ;
   }
//
//   Set number of slats with raw data
//
   raw_h->nok = n_phi * n_eta ;
//
//  That's it
//
}
//
void cts_electronic_noise (
  TABLE_HEAD_ST          *geo_h,        CTG_GEO_ST            *geo,
  TABLE_HEAD_ST          *mpara_h,      CTS_MPARA_ST          *mpara,
  TABLE_HEAD_ST          *slat_h,       CTG_SLAT_ST           *slat,
  TABLE_HEAD_ST          *raw_h,        CTU_RAW_ST            *raw   ) {

/*:>--------------------------------------------------------------------
**: ROUTINE:    cts_electronic_noise
**: DESCRIPTION: Put random noise in counters
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:           geo_h       - geometry    header table
**:           geo         - geometry    table
**:           slat_h      - slat        header table 
**:           slat        - slat        table
**:      OUT:
**:           raw_h       - raw         table header
**:           raw         - raw         table
**:>------------------------------------------------------------------*/
  
//
  long index, i_slat ;
  long n_slats_on = raw_h->nok ;
//
//  Loop over counters
//
  for ( long i_phi = geo->i_phi_min-1 ; i_phi < geo->i_phi_max ; i_phi++ )
     for ( long i_eta = geo->i_eta_min-1 ; i_eta < geo->i_eta_max ; i_eta++ ) {
//
//    Get index
//
        index = ctg_index ( i_phi, i_eta,
                            geo->n_counter_eta * geo->n_tray_eta ) ;
//
        i_slat = local_index[index] ;
//
//    Check whether slat was fired
//

        if ( i_slat >= 0 ) continue ;
//
//     Check whether noise should be generated
//
        if ( rndm_() > mpara->elec_noise ) continue ;
//
        n_slats_on++ ;
        i_slat = n_slats_on ;
        local_index[index] = i_slat ;
//
//      Generated ADC&TDC signals
//
        raw[i_slat].adc = mpara->amin_noise +
               rndm_() * (mpara->amax_noise-mpara->amin_noise) ;

        raw[i_slat].tdc = mpara->tmin_noise +
               rndm_() * (mpara->tmax_noise-mpara->tmin_noise) ;
//
//     Add info in Raw
//
        raw[i_slat].i_phi = i_phi ;
        raw[i_slat].i_eta = i_eta ;
     }
//
//   Update # entries
//
   raw_h->nok = n_slats_on ;
//
//  That's it
//
}
//
void cts_get_ctf_indexes ( long detector, long volume, long& i_phi, long& i_eta ) {
/*:>--------------------------------------------------------------------
**: ROUTINE:    cts_get_ctf_indexes
**: DESCRIPTION: Decodes the g2t volume id 
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:           detector    - Detector id (1=CTB,2=TOF)
**:           volume      - Volume id from g2t
**:      OUT:
**:           i_phi       - Phi index     
**:           i_eta       - Eta index     
**:>------------------------------------------------------------------*/

//
    if ( detector == 1 ) cts_get_ctb_indexes ( volume, i_phi, i_eta ) ;
    else                 cts_get_tof_indexes ( volume, i_phi, i_eta ) ;
}
//
void cts_get_ctb_indexes ( long volume, long &i_phi, long &i_eta ) {
/*:>--------------------------------------------------------------------
**: ROUTINE:    cts_get_ctb_indexes
**: DESCRIPTION: Decodes the g2t CTB volume id
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:           volume      - Volume id from g2t
**:      OUT:
**:           i_phi       - Phi index
**:           i_eta       - Eta index
**:>------------------------------------------------------------------*/

//
    long i1 ;
    i1     = int(volume/100) ;
    i_phi  = fmod(volume,100) ;
    if ( i1 < 20 ) {
       i_phi = 14 - i_phi ;
       if ( i_phi < 1 ) i_phi = i_phi + 60 ;
       if ( i1 == 11 ) i_eta = 3 ;
         else 
       if ( i1 == 12 ) i_eta = 4 ;
    }
    else if ( i1 > 20 ) {
       i_phi = i_phi - 42 ;
       if ( i_phi < 1 ) i_phi = i_phi + 60 ;
       if ( i1 == 21 ) i_eta = 2 ;
          else 
       if ( i1 == 22 ) i_eta = 1 ;
    }
    else
       cout<<" ctg_get_ctb_indexes: I_eta error "<<endl ;
}
//
void cts_get_tof_indexes ( long volume, long &i_phi, long &i_eta ) {
/*:>--------------------------------------------------------------------
**: ROUTINE:    cts_get_tof_indexes
**: DESCRIPTION: Decodes the g2t TOF volume id 
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:           volume      - Volume id from g2t
**:      OUT:
**:           i_phi       - Phi index
**:           i_eta       - Eta index
**:>------------------------------------------------------------------*/

    short i_tray_eta,    i_tray_phi ;
    short i_counter_eta, i_counter_phi ; 

    i_tray_eta    = int(volume/100000) ;
    i_counter_eta = (short)fmod(volume,100000)/1000 ;
    i_tray_phi    = (short)fmod(volume,1000)/10 ;
    i_counter_phi = (short)fmod(volume,10) ;

    if ( i_tray_eta == 1 ) {
       i_phi = 14 - i_tray_phi ;
       if ( i_phi < 1 ) i_phi = i_phi + 60 ;
       i_phi = i_phi * 5 - i_counter_phi + 1 ;
    }
    else if ( i_tray_eta == 2 ) {
       i_phi = i_tray_phi - 42 ;
       if ( i_phi < 1 ) i_phi = i_phi + 60 ;
       i_phi = i_phi * 5 + i_counter_phi - 5 ;
    }
    else
       cout<<" ctg_get_tof_indexes: I_eta error "<<endl ;
//
    if ( i_tray_eta == 1 ) i_eta = i_counter_eta + 10 ;
       else
    if ( i_tray_eta == 2 ) i_eta = 11 - i_counter_eta ;

}
//
void cts_physical_noise ( long           i_phi,        long            i_eta,
                          long&           n_slats_on,
                          long           n_phe,        float           time,
                          TABLE_HEAD_ST  *mslat_h,     CTS_MSLAT_ST    *mslat,
                          TABLE_HEAD_ST  *geo_h,       CTG_GEO_ST      *geo   ) {

/*:>--------------------------------------------------------------------
**: ROUTINE:     cts_physical_noise 
**: DESCRIPTION: Generates noise in adyacent counters
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:    INOUT:
**:           mslat_h     - MC slat table header
**:           mslat       - MC slat table
**:           geo_h       - raw table header
**:           geo         - raw table       
**:>------------------------------------------------------------------*/

//
//      Choose a random slat around present slat
//
  long noisy_i_phi = -99999 ;
  long noisy_i_eta = -99999 ;
//
//       Check this makes sense
//
   for ( ; noisy_i_phi < geo->i_phi_min
        || noisy_i_phi > geo->i_phi_max
        || noisy_i_eta < geo->i_eta_min
        || noisy_i_eta > geo->i_eta_max ; ) {
       noisy_i_phi = i_phi + (int)(3.*rndm_())-1 ;
       noisy_i_eta = i_eta + (int)(3.*rndm_())-1 ;
   }
//
//      Get index
//
   long index = ctg_index ( noisy_i_phi, noisy_i_eta, 
                            geo->n_counter_eta * geo->n_tray_eta ) ;
//
//     Check the slat number
//
   long i_slat = local_index[index] ;
   if ( i_slat < 0 ) {
      n_slats_on++ ;
      i_slat = n_slats_on ;
      local_index[index]  = i_slat ;
      mslat[i_slat].i_phi = noisy_i_phi ;
      mslat[i_slat].i_eta = noisy_i_eta ;
   }
//
//   Store noise info now
//
   mslat[i_slat].n_phe += (1.F+rg32_()) * n_phe ;
//
//   Time now
//
   if ( mslat->time == 0 || time < mslat[i_slat].time ) 
                    mslat[i_slat].time = time ;
//
//   That's it
//
}



float cts_slat_response_exp ( float& dz,
                              CTS_MPARA_ST* mpara ) {

/*:>--------------------------------------------------------------------
**: ROUTINE:     cts_slat_response_exp
**: DESCRIPTION: Returns slat response as function of impact position
**:              using a exponential attenuation
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:           dz          - Distance to PM
**:           mpara       - Module parameters
**:>------------------------------------------------------------------*/

//
//      Choose a random slat around present slat
//
//
//     Get number of photoelectrons
//
     return  mpara->GeV_2_n_photons
           * mpara->cath_eff
           * mpara->cath_surf * mpara->surf_loss
           * exp(-dz / mpara->attlen ) ;
}


float cts_slat_response_table ( float& z, float& dd, float& tof,
                                CTS_MPARA_ST* mpara ) { 

/*:>--------------------------------------------------------------------
**: ROUTINE:     cts_slat_response_table
**: DESCRIPTION: Returns slat response as function of impact position
**:              using a parametrization table
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:           z           - Distance to PM
**:           dd          - Distance to closest edge
**:           tof         - Impact Time of Flight   
**:           mpara       - Module parameters
**:>------------------------------------------------------------------*/
   float weight ;
   char  OutMessage[50] ;
//
//   Check array dimensions
//
   long max_z = sizeof(mpara->z_grid) / sizeof(mpara->z_grid[0]) ;
//
   if ( mpara->n_z < 1 || mpara->n_z > max_z ) {
     sprintf ( OutMessage, " cts_slat_response_table: n_z = %d, max_z = %d  ",
                             mpara->n_z, max_z ) ;
     MessageOut ( OutMessage ) ;
     return -1 ;
   }
//
   long max_d = sizeof(mpara->d_grid) / sizeof(mpara->d_grid[0]) ;
   if ( mpara->n_d < 1 || mpara->n_d > max_d ) {
     sprintf ( OutMessage, " cts_slat_response_table: n_d = %d, max_d = %d  ",
                             mpara->n_d, max_d ) ;
     MessageOut ( OutMessage ) ;
     return -1 ;
   }
//
   long max_zd = sizeof(mpara->slat_response) / sizeof(mpara->slat_response[0]) ;
   if ( mpara->n_d < 1 || mpara->n_d > max_d ) {
     sprintf ( OutMessage, " cts_slat_response_table: n_d * n_z = %d, max_zd = %d  ",
                             mpara->n_d*mpara->n_z, max_zd ) ;
     MessageOut ( OutMessage ) ;
     return -1 ;
   }
//
   long max_t = sizeof(mpara->time_response) / sizeof(mpara->time_response[0]) ;
   if ( mpara->n_time < 1 || mpara->n_time > max_t ) {
     sprintf ( OutMessage, " cts_slat_response_table: n_time = %d, max_t = %d  ",
                             mpara->n_time, max_t ) ;
     MessageOut ( OutMessage ) ;
     return -1 ;
   }
//
   if ( dd > mpara->d_grid[mpara->n_d-1] ) {
     if ( dd > mpara->d_grid[mpara->n_d-1]+mpara->position_tolerance ) {
        sprintf ( OutMessage, " Distance to center slat, d %f out of range  ", dd ) ;
        MessageOut ( OutMessage ) ;
     }
     dd = mpara->d_grid[mpara->n_d-1] ;
   }
//
//   Find z bin and dd bin     
//
   long i,j ;
//
//    Use parametrization grid when close to tube
//
   if ( z < mpara->z_grid[mpara->n_z-1] ) { 
      for ( i=1 ; i<mpara->n_z ; i++ ) if ( z  <= mpara->z_grid[i] ) break ;
      for ( j=1 ; j<mpara->n_d ; j++ ) if ( dd <= mpara->d_grid[j] ) break ;
//
//     Interpolate the response from the four corners of the bin.
//
      float zfrac =(z -mpara->z_grid[i-1])/(mpara->z_grid[i]-mpara->z_grid[i-1]) ;
      float dfrac =(dd-mpara->d_grid[j-1])/(mpara->d_grid[j]-mpara->d_grid[j-1]) ;
      long n_z    = mpara->n_z ;
      float p1    = dfrac*(mpara->slat_response[j*n_z+i]-
                           mpara->slat_response[(j-1)*n_z+i])+
                           mpara->slat_response[(j-1)*n_z+i] ;
      float p2    = dfrac*(mpara->slat_response[j*n_z+(i-1)]-
                           mpara->slat_response[(j-1)*n_z+(i-1)])+
                           mpara->slat_response[(j-1)*n_z+(i-1)] ;
      weight      = zfrac * (p1-p2) + p2 ;
   }
//
//   Otherwise just get attenuation
//
   else if ( z < 60. ) weight=228.+(60.-z)*0.55  ;     
   else if ( z < 80. ) weight=213.+(80.-z)*0.75  ;      
   else if ( z < 100.) weight=192.+(100.-z)*1.05 ;      
   else if ( z < 128.) weight=157.+(128.-z)*1.25 ;
   else weight=154.+(130.-z)*1.5 ;
//
//  Normalize result to z=15, d-7
//
   weight /= 249. ; 
   weight = weight * mpara->GeV_2_n_photons
                   * mpara->cath_eff
                   * mpara->cath_surf * mpara->surf_loss ;
//
//    get the time of signal integration in ns. 
//    Assume the gate starts at mpara->gate_t0. 
//    The width of the gate is taken from mpara->gate_width
//
   float time = 1.e9 * ( mpara->gate_width -(tof-mpara->gate_t0+z*mpara->delay) );
   long  i_time = (long)time/2 ;
//
   float tipart ;
   if      ( i_time < 0           ) tipart = 0.0 ;
   else if (time >= mpara->n_time ) tipart = 1.0 ;
   else                             tipart = 0.01*mpara->time_response[i_time] ;
//
   return weight*tipart ;
}
