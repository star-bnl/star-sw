#include <iostream.h>
#include "ctu.h"
#include "ctfgeo.h"
//
extern "C" void    MessageOut( const char *msg );
//
long type_of_call ctu_(
  TABLE_HEAD_ST           *geo_h,         CTG_GEO_ST            *geo,
  TABLE_HEAD_ST           *slat_h,       CTG_SLAT_ST            *slat,
  TABLE_HEAD_ST           *raw_h,         CTU_RAW_ST            *raw,
  TABLE_HEAD_ST           *cor_h,         CTU_COR_ST            *cor ) {
/*:>--------------------------------------------------------------------
*: HISTORY:
*:             17feb97-      ppy- STAF version
*:<--------------------------------------------------------------------
**: DESCRIPTION: Simulates CTB/TOF unpacking
**:
**: AUTHOR:     ppy - P.P. Yepes, yepes@physics.rice.edu
**: ARGUMENTS:
**:       IN:
**:              geo_h    - geo header
**:                geo    - Geometry parameters
**:             slat_h    - Slat info header
**:               slat    - Slat info
**:              raw_h    - raw header
**:                raw    - raw data
**:      OUT:
**:              cor_h    - cor header
**:                cor    - corrected information          
**: 
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
//
  char  OutMessage[50] ;
//
//    Make sure geometry table has something         
//
  if ( geo_h->maxlen != 1 ) {
     sprintf ( OutMessage, " Geo maxlen = %d is not valid ", geo_h->maxlen ) ;
     MessageOut ( OutMessage ) ;
     return STAFCV_BAD ;
  }
//
//     Make sure geometry package was initialized    
//
  if ( geo[0].init == 0 ) {
     MessageOut ( " ctu: ctg needs to be run before running ctu " ) ;
     return STAFCV_BAD ;
  }
//
//    Check there is some raw data
//
//
//     Make sure geometry package was initialized    
//
  if ( raw_h->nok < 1 ) {
     MessageOut ( " ctu: Raw table is empty " ) ;
     return STAFCV_BAD ;
  }
//
//    Check cor dimension
//
  if ( cor_h->maxlen < raw_h->nok ) {
     MessageOut ( " ctu: Cor table shorter than entries in raw " ) ;
     return STAFCV_BAD ;
  } 
//
//    Loop over slats to get raw data
//
  long index ;
  long n_eta = geo->n_counter_eta * geo->n_tray_eta ;
//
  for ( int i_slat = 0 ; i_slat < raw_h->nok ; i_slat++ ) {
//
//    Get index
//
     index = ctg_index ( raw[i_slat].i_phi, raw[i_slat].i_eta, n_eta  ) ;
//
     cor[i_slat].i_phi = raw[i_slat].i_phi ;
     cor[i_slat].i_eta = raw[i_slat].i_eta ;
     cor[i_slat].n     = ( raw[i_slat].adc - slat[i_slat].offset_adc )
                       * slat[i_slat].cc_adc ;
     cor[i_slat].time  = ( raw[i_slat].tdc - slat[i_slat].offset_tdc )
                       * slat[i_slat].cc_tdc ;
  }
//
//    Set number of slats with raw data
//
  cor_h->nok = raw_h->nok ;
//
  return STAFCV_OK ;
//
//    That's it
//
}
