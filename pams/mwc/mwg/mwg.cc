/*:>--------------------------------------------------------------------
**: FILE:       mwg.cc         
**: HISTORY:
**:             21jan98-v001a-ppy- C++ version created 
**:<------------------------------------------------------------------*/
#include "mwg.h"

long type_of_call mwg_(
  TABLE_HEAD_ST           *geom_h,        MWC_GEO_ST             *geom ,
  TABLE_HEAD_ST            *cal_h,        MWC_CAL_ST              *cal )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    MWG
**: DESCRIPTION: Fills MWC geometry table
**: AUTHOR:      James Withfield from CMU
**:              Moved to C++ by Pablo Yepes (yepes@physics.rice.edu)
**: ARGUMENTS:
**:       INOUT:
**:             geom     - Geometry structure
**:              cal     - Calibration parameters
**:  
**: RETURNS:    STAF Condition Value
**:>-------------------------------------------------------------------*/
//
//     Set general geometry numbers
//
   geom->init  =   1 ;
   if (geom->nphi  < 0) geom->nphi  =   12 ;
   if (geom->neta  < 0) geom->neta  =   16 ;
   if (geom->r1min < 0) geom->r1min =  53.0 ;
   if (geom->r1max < 0) geom->r1max = 121.0 ;
   if (geom->r2min < 0) geom->r2min = 122.5950 ;
//
//    Set Calibration constants and pedestals
//
   long n_sectors = 4 * geom->nphi * geom->neta ;
//
   if ( n_sectors <= cal_h->maxlen ) {
      for ( long i_sec = 1 ; i_sec < n_sectors ; i_sec++ ) 
          cal[i_sec].cc = 1 ; 
      cal_h->nok = n_sectors ;
   }
   else {
      printf ( " \n MWC Calibration array too small ! " ) ;
      return STAFCV_OK ;
   }       

   return STAFCV_OK;
}

