/*:>--------------------------------------------------------------------
**: FILE:       mwu.cc          
**: HISTORY:
**:             21jan98-v000a-ppy- Created by Pablo Yepes 
**:             Previous version in Fortran by James Withfield, CMU
**:
**:  Id: idl.y,v 1.28 1997/12/20 23:18:56 ward Exp  
**:<------------------------------------------------------------------*/
#include "mwu.h"

long type_of_call mwu_(
  TABLE_HEAD_ST            *cal_h,        MWC_CAL_ST              *cal ,
  TABLE_HEAD_ST            *raw_h,        MWC_RAW_ST              *raw ,
  TABLE_HEAD_ST            *cor_h,        MWC_COR_ST              *cor )
{
/*:>--------------------------------------------------------------------
**:>--------------------------------------------------------------------
**: ROUTINE:    MWS
**: DESCRIPTION: Unpacks MWC data
**:   
**: AUTHOR:     ppy - Pablo Yepes  yepes@physics.rice.edu
**: ARGUMENTS:
**:          IN:
**:       INOUT:
**:              cal     - calibarion table
**:              raw     - Raw       output table for MWC.
**:         OUT:
**:              cor     - Corrected output table for MWC.
**:
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
//
//    Check there is some raw data
//
   if ( raw_h->nok < 0 )  return STAFCV_BAD ;
//    
//   Loop over slats to get raw data
//
   long index ;
   for ( long i_sector = 0 ; i_sector < raw_h->nok ; i_sector++ ) { 
      index = raw[i_sector].sector ;
      cor[i_sector].sector   = raw[i_sector].sector ;
      cor[i_sector].no_mip   = raw[i_sector].count * cal[i_sector].cc ;
   }
//
//    Define the number of slats with raw data
//
   cor_h->nok = raw_h->nok ;
//
   return STAFCV_OK;
}
