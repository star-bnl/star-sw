/* sca_runsca.cc
 * written 7/98 by D. Weerasundara -- dhammika@gibbs.npl.washington.edu
 *-----------------------------------------------------------
 * This is the flow control code that interfaces with staf
 * It is also being used for root but has not been optimized
 *   for that application
 *-----------------------------------------------------------
 * History:
 * 8/7/98 L.D. Carr -- modified to take different switches
 *-----------------------------------------------------------
 */

#include <stdlib.h>
#include "sca_runsca.h"
#include "Dataset.hh"
long alloc_prior(TABLE_HEAD_ST *sca_prior_h, SCA_PRIOR_ST   *sca_prior);
long type_of_call sca_runsca_(
       TABLE_HEAD_ST   *sca_switch_h,       SCA_SWITCH_ST  *sca_switch,
       TABLE_HEAD_ST   *sca_const_h,        SCA_CONST_ST   *sca_const,
       TABLE_HEAD_ST   *sca_in_h,           SCA_IN_ST      *sca_in,  
       TABLE_HEAD_ST   *sca_ensemble_ave_h, SCA_OUT_ST     *sca_ensemble_ave,
       TABLE_HEAD_ST   *sca_prior_h,        SCA_PRIOR_ST   *sca_prior,  
       TABLE_HEAD_ST   *sca_out_h,          SCA_OUT_ST     *sca_out  )   
{

#ifndef  SCALE_OK
#define    SCALE_OK     0
#endif

#ifndef  SCALE_BAD
#define    SCALE_BAD 1
#endif

  char *raw_data_name = "sca_data";
  char *sca_data_name = "sca";
  long iret;
  static int callcnt=0;
  printf(" This is call number %d.\n",++callcnt);
  if (!sca_switch->makePrior   && 
      !sca_switch->makeEnsembleAve  &&
      !sca_switch->doAnalysis ) {
    cerr << "Invalid switch values.   Exit " << endl;
    return STAFCV_BAD;
  }
  if (sca_switch->makePrior){
    sca_prior_h[0].nok = PRIOR_SIZE;
    iret = alloc_prior (sca_prior_h, sca_prior);
    if (iret)
      {
	cerr << "alloc_prior failed. Exit " << endl;
	return STAFCV_BAD;
      }
  }
  //create the basic object for sca
  Dataset dataSet(sca_const_h, sca_const,
		  sca_prior_h, sca_prior,
		  sca_in_h,    sca_in,
		  raw_data_name,
		  sca_data_name );
  //CalcEntropy is a method common to all options
    iret = dataSet.CalcEntropy();
    if (iret)
      {
	cerr << "CalcEntropy failed. Exit " << endl;
	return STAFCV_BAD;
      }
  //flow control for prior, ensemble, and analysis begins here
  if ( sca_switch->makePrior ) {
    iret = dataSet.fillPrior(sca_prior_h, sca_prior);
    if (iret)
      {
	cerr << "fillPrior failed. Exit " << endl;
	return  STAFCV_BAD;
      }
  } //end makePrior option
  else if ( sca_switch->makeEnsembleAve ) {
    iret = dataSet.CalcDimension();
    if (iret)
      {
	cerr << "CalcDimension failed. Exit " << endl;
	return  STAFCV_BAD;
      }
    iret = dataSet.fillScaOutTable(sca_ensemble_ave_h, sca_ensemble_ave);
    if (iret)
      {
	cerr << "fillScaOutTable failed. Exit " << endl;
	return  STAFCV_BAD;
      }
  } //end makeEnsemble option
  //begin doAnalysis option
  else {
    iret = dataSet.CalcDimension();
    if (iret)
      {
	cerr << "CalcDimension failed. Exit " << endl;
	return  STAFCV_BAD;
      }
    //Here should be a makeRef method
    //  -- not yet implemented
    //iret = dataSet.makeRef(prior and ensemble_ave pointers);
    if (iret)
      {
	cerr << "CalcDimension failed. Exit " << endl;
	return  STAFCV_BAD;
      }
    iret = dataSet.fillScaOutTable(sca_out_h, sca_out);
    if (iret)
      {
	cerr << "fillScaOutTable failed. Exit " << endl;
	return  STAFCV_BAD;
      }
  } //end doAnalysis option

  return STAFCV_OK;
} //end sca_runsca

//define alloc_prior
long alloc_prior(TABLE_HEAD_ST *sca_prior_h, SCA_PRIOR_ST   *sca_prior) 
{
  int i;
  if ( ! sca_prior[0].w ){
    for ( i = 1 ; i < PRIOR_SIZE ; i++ ) sca_prior[i].w = 0;
    sca_prior[0].w = 1;
  }
  else 
    sca_prior[0].w++;
  sca_prior[0].marker = 1;
  return 0;
}
