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
#endif   SCALE_OK
#ifndef  SCALE_BAD
#define    SCALE_BAD 1
#endif   SCALE_BAD
  char *raw_data_name = "sca_data";
  char *sca_data_name = "sca";
  long iret;
  static int callcnt=0;
  printf(" 16:35:14 This is call number %d.\n",++callcnt);
  if(callcnt==20) printf("\007\n");
  if (!sca_switch->makePrior   && 
      !sca_switch->doAnalysis  &&
      !sca_switch->useDeltaD ) {
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
  /* bbb below */
  if ( !sca_switch->useDeltaD ) {
    printf("We are taking the big if()\n");
    Dataset dataSet(sca_const_h, sca_const,
		    sca_prior_h, sca_prior,
		    sca_in_h,    sca_in,
                    raw_data_name,
		    sca_data_name );
    printf("Calling the suspect dataSet.CalcEntropy\n");
    iret = dataSet.CalcEntropy();
    printf("Back from the suspect dataSet.CalcEntropy\n");
    if (iret)
      {
	cerr << "CalcEntropy failed. Exit " << endl;
	return STAFCV_BAD;
      }
    /* bbb above */
    if ( sca_switch->doAnalysis )
      {
	cout<<endl<<"Made it to CalcDimension"<<endl;
	iret = dataSet.CalcDimension();
	if (iret)
	  {
	    cerr << "CalcDimension failed. Exit " << endl;
	    return  STAFCV_BAD;
	  }
      }
    if ( sca_switch->makePrior) {
      iret = dataSet.fillPrior(sca_prior_h, sca_prior);
      if (iret)
	{
	  cerr << "fillPrior failed. Exit " << endl;
	  return  STAFCV_BAD;
	}
    }
  }
  return STAFCV_OK;
}
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
