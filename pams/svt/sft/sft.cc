/*
 *  FILE:        sft.cc  
 *  DESCRIPTION: SVT Fast Tracker and vertex-finder
 *  AUTHOR:      Dave Read (dmr) read@physics.utexas.edu
 *  HISTORY:     17 Apr 97	dmr		(created)     
 *
 */

#include <math.h>
#include "sft_am.h"

extern "C"
{
   int sft_main (
         TABLE_HEAD_ST     *sft_par_h,     SFT_PAR_ST          *sftpar,
         TABLE_HEAD_ST     *spt_h,         SCS_SPT_ST          *spt,
         TABLE_HEAD_ST     *sft_vertex,    SFT_VERTEX_ST       *vertex);
};


/*  MODULE:      SFT
 *  DESCRIPTION: Top-level module
 *  AUTHOR:      Dave Read (dmr) read@physics.utexas.edu
 *  ARGUMENTS:
 *           IN:
 *        INOUT:
 *          OUT:
 *  RETURNS:    STAF Condition Value
 *
 */


long
sft_am_ (TABLE_HEAD_ST     *sft_par_h,     SFT_PAR_ST          *sftpar,
         TABLE_HEAD_ST     *spt_h,         SCS_SPT_ST          *spt,
         TABLE_HEAD_ST     *sft_vertex,    SFT_VERTEX_ST       *vertex)
{
   sft_main (sft_par_h, sftpar, spt_h, spt, sft_vertex, vertex);
   return STAFCV_OK;
}
