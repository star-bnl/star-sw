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

  //>--------------------------------------------------------------------
  // ROUTINE:     sca_runSca_
  // DESCRIPTION: The driving routine for the Scaled Correlation Analysis.
  //        
  // AUTHOR:      Dhammika Weerasundara -- University of Washington
  //              dhammika@gibbs.npl.washington.edu
  //
  // ARGUMENTS:
  //          IN:
  //             sca_const          -  sca constant table    
  //             sca_const_h        -  Header Structure for sca_const   
  //             sca_switch         -  sca switch table    
  //             sca_const_h        -  Header Structure for sca_switch1
  //             sca_in             -  sca input table    
  //             sca_in_h           -  Header Structure for sca_in   
  //       INOUT:
  //             sca_ensemble_ave   -  sca ensemble average 
  //             sca_ensemble_ave_h -  Header Structure for sca_ensemble_ave 
  //             sca_prior          -  prior table    
  //             sca_prior_h        -  Header Structure for sca_prior
  //         OUT:
  //             sca_out            -  sca output table
  //             sca_out_h          -  Header Structure for sca_out  
  //
  // RETURNS:    STAF Condition Value
  //
  // HISTORY:    
  //      06/19/1995       SJ Bailey  Orginal was written to run
  //                       JG Reid    in NA49/DSPACK environment.
  //
  //      06/26/1998       DW         Adapted to run in STAF.
  //
  //      08/01/1998       LD Carr    Modified and debugged
  //                                  lcarr@u.washington.edu 
  //
  //>--------------------------------------------------------------------

#ifndef  SCALE_OK
#define    SCALE_OK     0
#endif   SCALE_OK
#ifndef  SCALE_BAD
#define    SCALE_BAD 1
#endif   SCALE_BAD


  // A few constants to this control module (sorry)
  char *raw_data_name = "sca_data";
  char *sca_data_name = "sca";
  long iret;

  // Check for the right combination of switches 
  if (!sca_switch->makePrior   && 
      !sca_switch->doAnalysis  &&
      !sca_switch->useDeltaD ) {
    cerr << "Invalid switch values.   Exit " << endl;
    return STAFCV_BAD;
  }


  // Do alloc_prior
  if (sca_switch->makePrior){

    //set sca_prior to have correct number of instances of table
    sca_prior_h[0].nok = PRIOR_SIZE;
    
    iret = alloc_prior (sca_prior_h, sca_prior);
    if (iret)
      {
	cerr << "alloc_prior failed. Exit " << endl;
	return STAFCV_BAD;
      }
  }

  // Initialize dataSet and perform the scaled correlation analysis
  if ( !sca_switch->useDeltaD ) {

    Dataset dataSet(sca_const_h, sca_const,
		    sca_prior_h, sca_prior,
		    sca_in_h,    sca_in,
                    raw_data_name,
		    sca_data_name );
    
    
    iret = dataSet.CalcEntropy();
    if (iret)
      {
	cerr << "CalcEntropy failed. Exit " << endl;
	return STAFCV_BAD;
      }
    if ( sca_switch->doAnalysis )
      {

	//test
	//LDC
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
  //if ( sca_switch->useDeltaD ) {
    // Determine "dimension transport" by taking the difference between
    // the sca_out and the ensemble reference.
    // Skip this part for the time being. First get the entropy and dim 
    // running.
  //iret = sca_deltad(sca_out_h, sca_out, sca_ensemble_h, sca_ensemble_ave);
  //if (iret)
  // {
  //  cerr << "Dimension transport calculation failed. Exit " << endl;
  //  return  STAFCV_BAD;
  //}
  //}
  return STAFCV_OK;
}  // end of sca_runSca


long alloc_prior(TABLE_HEAD_ST *sca_prior_h, SCA_PRIOR_ST   *sca_prior) 
{
  int i;


  // initialize if 0
  if ( ! sca_prior[0].w ){
    // Initialize prior
    for ( i = 1 ; i < PRIOR_SIZE ; i++ ) sca_prior[i].w = 0;
    sca_prior[0].w = 1;
  }
  else 
    sca_prior[0].w++;

  sca_prior[0].marker = 1;
  return 0;

} // end of alloc_prior




