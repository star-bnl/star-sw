/*:>-------------------------------------------------------------------
**: FILE:       l3cl.cc (StAF version)
**:
**: HISTORY:     last change - 05/28/98  cs: UNIX-version running 
**:                                          without StAF
**:                            06/02/98  cs: StAF version
**:                            06/15/98  ppy
**:                            09/17/98  cs: new l3 pixel format
**:                            07/06/99  ppy: comment out writting hits, sl3Hit table
**:                                      changed, code needs to be modified to write buffer
**:                            07/07/99  ppy: SLHIT replaced with SL3BUFFER
**:  
**:<------------------------------------------------------------------*/

#include "PAM.h"
#include "l3cl.h"
#include "l3cl_inc.h"
#include <stdlib.h>

extern "C" long l3cl_(
  TABLE_HEAD_ST     *para_h,       L3CLPARA_ST        *para,
  TABLE_HEAD_ST     *pad_h,        L3CLPAD_ST         *pad,
  TABLE_HEAD_ST     *pixel_h,      TYPE_SHORTDATA_ST  *pixel,        
  TABLE_HEAD_ST     *hit_h,        SL3BUFFER_ST       *hit   )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    l3cl_
**: DESCRIPTION: Prepares data and calls fast clusterfinder   
**:
**: 
**: AUTHOR:     ppy - Pablo P. Yepes, yepes@rice.edu
**:             cs  - Christof Struck, struck@star.physics.yale.edu
**:
**: ARGUMENTS:
**:       IN:
**:        l3cl_para      - Level 3 clusterfinder parameters
**:       IN:
**:        l3cl_pad       - 
**:        l3cl_pixel     - TPC Pixels, l3 format
**:       INOUT:
**:        l3cl_hit       - TPC Space Points, l3 format
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
//
  if ( l3clInitPhysMap()    ) return STAFCV_BAD ;
  if ( l3clAllocateMemory() ) return STAFCV_BAD ;
//
  l3clInitPointers();
  l3clInitTable();
  l3clInitOther() ;
  l3clInitClusters     ( pad_h, pad, pixel );
  l3clFindClusters     ( para->StartRow, para->EndRow );
  int  initialNok = hit_h->nok ;
/*
  l3clWriteDataToTable ( hit_h, hit );  
//
//   Include sector in row
//
  for ( int ihit = initialNok ; ihit < hit_h->nok ; ihit++ ) {
      hit[ihit].row = hit[ihit].row + 100 * para->sector ; 
  }
*/
//
  l3clFreeMemory() ;
// 
  return STAFCV_OK;
}
