/*:>-------------------------------------------------------------------
**: FILE:       l3cl.cc (StAF version)
**:
**: HISTORY:     last change - 05/28/98  cs: UNIX-version running 
**:                                          without StAF
**:                            06/02/98  cs: StAF version
**:                            06/15/98  ppy
**:   
**:  
**:<------------------------------------------------------------------*/

#include "PAM.h"
#include "l3cl.h"
#include "l3cl_inc.h"
#include <stdlib.h>



extern "C" long type_of_call l3cl_(
  TABLE_HEAD_ST     *para_h,       L3CL_PARA_ST    *para,
  TABLE_HEAD_ST     *pad_h,        TSS_TPPAD_ST    *pad,
  TABLE_HEAD_ST     *pixel_h,      TSS_TPPIXEL_ST  *pixel,        
  TABLE_HEAD_ST     *hit_h,        TCL_TPHIT_ST    *hit   )


{
/*:>--------------------------------------------------------------------
**: ROUTINE:    l3cl_
**: DESCRIPTION: Prepares data and calls fast clusterfinder   
**:
**: 
**: AUTHOR:     ppy - Pablo P. Yepes, yepes@physics.rice.edu
**:             cs  - Christof Struck, struck@star.physics.yale.edu
**:
**: ARGUMENTS:
**:       IN:
**:        l3cl_para      - Level 3 clusterfinder parameters
**:       IN:
**:        tss_tppad      \
**:        tss_tppixel    - TPC Pixels
**:      OUT:
**:        tcl_tphit      - TPC Space Points
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/

  TSS_TPPAD_ST *pad_pointer = pad;
  int sector ;

  hit_h->nok = 0;

  init_phys_map();
  init_pointers();
  init_table();
  init_other();
//
//    Set hit memory to 0
//
  memset(hit, 0, hit_h->maxlen * sizeof(TCL_TPHIT_ST));


  while( pad_pointer <= &(pad[(pad_h->nok)-1]) )  {
      reset_other() ;
      sector = pad_pointer->tpc_row /100 ;
//
//    If sector wanted get clusters
//
      if ( ( sector >= para->FirstSector && sector <= para->LastSector ) ||
           para->LastSector == 0 ){ 
         pad_pointer = init_clusters( sector, pad_pointer, pad_h, pad, pixel );
         FindClusters();
         WriteDataToTable( sector, hit_h, hit );  
      }
//
//    If sectors is not wanted look for next sector
//
      else {
        while( sector == pad_pointer->tpc_row /100 && pad_pointer <= &(pad[(pad_h->nok)-1]) )
        pad_pointer++;
      }
  }
 

  return STAFCV_OK;
}
