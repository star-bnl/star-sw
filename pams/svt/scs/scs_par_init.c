/*------------------------------------------------------------------
FILE:         scs_par_init.c
DESCRIPTION:  scs - cluster finder - parameter initialization

              Only the non already initialized parameters 
	      are set.

AUTHOR:       Dave Read, C.A.Pruneau
BUGS:         -- STILL IN DEVELOPMENT --
HISTORY:      August 18, 1996
------------------------------------------------------------------*/

/*------------------------------------------------------------------
ROUTINE:      long scs_par_init
DESCRIPTION:  initialize the scs parameters
ARGUMENTS:    par   : parameters
RETURN VALUE: STAF Condition Value
------------------------------------------------------------------*/
#include "scs_par_init.h"

long type_of_call scs_par_init_(TABLE_HEAD_ST   *par_h, SCS_PAR_ST *par ) 
{

/*  fCorrection  = 15.75 + 2.87 * Cl->x[0] + 1.796 * sqr(Cl->x[0]); */

   if (par[0].drift_corr[0] == 0)
      par[0].drift_corr[0] = 13.77;

   if (par[0].drift_corr[1] == 0)
      par[0].drift_corr[1] = 7.836;

   if (par[0].drift_corr[2] == 0)
      par[0].drift_corr[2] = -1.558;

   if (par[0].drift_corr[3] == 0)
      par[0].drift_corr[3] = 0.4804;

   if (par[0].cuts[0] == 0)
      par[0].cuts[0] = 0.2;

   if (par[0].cuts[1] == 0)
      par[0].cuts[1] = 0.9;

   if (par[0].cuts[2] == 0)
      par[0].cuts[2] = 0.1;

   if (par[0].cuts[3] == 0)
      par[0].cuts[3] = 1.4;

   if (par[0].cuts[4] == 0)
      par[0].cuts[4] = 1.4;

   if (par[0].cuts[5] == 0)
      par[0].cuts[5] = 0.9;

   if (par[0].cuts[6] == 0)
      par[0].cuts[6] = 0.9;

  return STAFCV_OK;
}
