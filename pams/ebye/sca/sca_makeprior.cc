// ------- System includes -------------- 
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
// ------- STAF/ROOT generated includes ------- 
#include "sca_makeprior.h"

// Main routine
long type_of_call sca_makeprior_(
       TABLE_HEAD_ST   *sca_prior_h,       SCA_PRIOR_ST  *sca_prior)
{
  //>--------------------------------------------------------------------
  // ROUTINE:     sca_makeprior_
  // DESCRIPTION: Make the final normalized  prior for a given set of events.
  //              Must be run in  run_sca.kumac  at the end of the event loop 
  //        
  // AUTHOR:      Dhammika Weerasundara -- University of Washington
  //              dhammika@gibbs.npl.washington.edu
  //
  // ARGUMENTS:
  //          IN:
  //       INOUT:
  //             sca_prior      -  sca final prior 
  //             sca_prior_h    -  Header Structure for  sca_prior 
  //         OUT:
  //
  // RETURNS:    STAF Condition Value
  //
  // HISTORY:    
  //      09/11/1998       DSW     Original
  //
  //>--------------------------------------------------------------------

  int i;

  // Normalize the prior & set 'reference built' flag
  if (sca_prior[0].w != 0) {
    for ( i = 1 ; i < sca_prior_h->nok ; i++ )
      sca_prior[i].w = sca_prior[i].w / sca_prior[0].w;
    sca_prior[0].w = 0;
  }
  
  return STAFCV_OK;
}  /* end makeprior() */
