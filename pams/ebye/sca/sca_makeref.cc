// ------- System includes -------------- 
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
// ------- STAF/ROOT generated includes ------- 
#include "sca_makeref.h"

// Main routine
long type_of_call sca_makeref_(
       TABLE_HEAD_ST   *sca_switch_h,       SCA_SWITCH_ST  *sca_switch,
       TABLE_HEAD_ST   *sca_ensemble_ave_h, SCA_OUT_ST     *sca_ensemble_ave )
{
  //>--------------------------------------------------------------------
  // ROUTINE:     sca_makeref_
  // DESCRIPTION: Make the ensemble average of scale-local variables for
  //              given data set to produce a reference. Must be run
  //              at the end of the event loop. 
  //        
  // AUTHOR:      Dhammika Weerasundara -- University of Washington
  //              dhammika@gibbs.npl.washington.edu
  //
  // ARGUMENTS:
  //          IN:
  //             sca_switch         -  sca switch table    
  //             sca_switch_h       -  Header Structure for sca_switch
  //       INOUT:
  //             sca_ensemble_ave   -  sca ensemble average 
  //             sca_ensemble_ave_h -  Header Structure for sca_ensemble_ave 
  //         OUT:
  //
  // RETURNS:    STAF Condition Value
  //
  // HISTORY:    
  //      06/19/1995       SJ Bailey         Orginal was written to run
  //                       JG Reid           in NA49/DSPACK environment.
  //
  //      08/10/1998       D  Weerasundara   Adapted to run in STAF/ROOT.
  //      08/10/1998       LD Carr  Debugged
  //                          -- lcarr@u.washington.edu
  //>--------------------------------------------------------------------

  float n_events = (float)sca_switch->nEvents;

  for (int  irow=0;  irow < sca_ensemble_ave_h->nok; irow++) {
    sca_ensemble_ave[irow].volume    /= n_events;
    sca_ensemble_ave[irow].entropy   /= n_events;
    sca_ensemble_ave[irow].dim       /= n_events;
    sca_ensemble_ave[irow].info      /= n_events;
    sca_ensemble_ave[irow].dim_lower /= n_events;
  }
  return STAFCV_OK;
}  /* end makeref() */
