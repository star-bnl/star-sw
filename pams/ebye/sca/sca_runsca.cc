// ------- System includes -------------- 
#include <stdlib.h>
// ------- STAF/ROOT generated includes ------- 
#include "sca_runsca.h"
// ------- Local includes -------  
#include "Dataset.hh"

#ifdef   DEBUG
#undef   DEBUG 
#endif
#define  DEBUG  0
 
// Function prototypes
long alloc_prior(TABLE_HEAD_ST *sca_prior_h, SCA_PRIOR_ST   *sca_prior);


long ensemble_sum(
    TABLE_HEAD_ST   *sca_out_h,          SCA_OUT_ST     *sca_out  ,
    TABLE_HEAD_ST   *sca_ensemble_ave_h, SCA_OUT_ST     *sca_ensemble_ave);  

long sca_deltad(
       TABLE_HEAD_ST   *sca_ensemble_ave_h, SCA_OUT_ST     *sca_ensemble_ave,
       TABLE_HEAD_ST   *sca_out_h,          SCA_OUT_ST     *sca_out  );  

// Main routine
long type_of_call sca_runsca_(
       TABLE_HEAD_ST   *sca_switch_h,       SCA_SWITCH_ST  *sca_switch,
       TABLE_HEAD_ST   *sca_const_h,        SCA_CONST_ST   *sca_const,
       TABLE_HEAD_ST   *sca_in_h,           SCA_IN_ST      *sca_in,  
       TABLE_HEAD_ST   *sca_ensemble_ave_h, SCA_OUT_ST     *sca_ensemble_ave,
       TABLE_HEAD_ST   *sca_prior_h,        SCA_PRIOR_ST   *sca_prior,  
       TABLE_HEAD_ST   *sca_out_h,          SCA_OUT_ST     *sca_out  )   
{

  //>--------------------------------------------------------------------
  // ROUTINE:     sca_runsca_
  //
  // DESCRIPTION: The driving routine that interfaces 
  //              the Scaled Correlation Analysis (SCA) package
  //              with STAF and ROOT.
  //
  // AUTHOR:      Dhammika Weerasundara -- University of Washington
  //              dhammika@gibbs.npl.washington.edu
  //
  // ARGUMENTS:
  //          IN:
  //             sca_const          -  sca constant table    
  //             sca_const_h        -  Header Structure for sca_const   
  //             sca_switch         -  sca switch table    
  //             sca_switch_h       -  Header Structure for sca_switch
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
  //      06/26/1998       DW         Adapted to run in STAF/ROOT.
  //
  //      08/01/1998       LD Carr    Modified and debugged
  //                                  lcarr@u.washington.edu 
  //
  //      08/07/1998       LD Carr    Modified to take different switches
  //
  //      08/07/1998       DW         Put debug print statements on a 
  //                                    DEBUG switch. 
  //                                  Removed SCALE_OK & SCALE_BAD definitions 
  //                                    (they are not being used). 
  //                                  Added functions to calculate  ensemble 
  //                                    sum and dimension transport.
  //
  //      08/10/1998       LD Carr    Debugged DW additions
  //
  //      08/12/1998       DSW        Removed a call to dataSet.fillScaOutTable
  //                                  following a call to  sca_deltad. 
  //
  //      09/01/1998       DSW        More refinement and clean ups.
  //
  //      09/02/1998       DSW        Re-set sca_prior[0].marker=1 at the 
  //                                  beginning  of each event when
  //                                  sca_switch->makePrior=0 
  //
  //      09/12/1998       DSW        Cleaned up ensemble_sum & sca_deltad
  //                                  routines.
  //                                  Added loading sca_ensemble_ave_h->nok
  //                                  in ensemble_sum.
  //                                  Fixed calculating numscales & nraks
  //                                  in sca_deltad.
  //
  //>--------------------------------------------------------------------


  char *raw_data_name = "sca_data";
  char *sca_data_name = "sca";
  long iret;
  static int callcnt=0;

  //  ========================  Begin Executable Code  =============

  if (DEBUG) printf(" This is call number %d.\n",++callcnt);
  if (!sca_switch->makePrior   && 
      !sca_switch->makeEnsembleAve  &&
      !sca_switch->doAnalysis ) {
    cerr << "Invalid switch values.   Exit " << endl;
    return STAFCV_BAD;
  }
  if (sca_switch->makePrior){
    iret = alloc_prior (sca_prior_h, sca_prior);
    if (iret) {
      cerr << "alloc_prior failed. Exit " << endl;
      return STAFCV_BAD;
    }
  }
  else
    sca_prior[0].marker = 1;
  // Reset prior_p[0].marker=1 at the beginning of each event
  // when calculating ensemble average or  doing analysis. 
  // DSW  Sep0 2, 1998
  
  //create the basic object for sca
  Dataset dataSet(sca_const_h, sca_const,
		  sca_prior_h, sca_prior,
		  sca_in_h,    sca_in,
		  raw_data_name,
		  sca_data_name );

  //CalcEntropy is a method common to all options
  iret = dataSet.CalcEntropy();
  if (iret) {
    cerr << "CalcEntropy failed. Exit " << endl;
    return STAFCV_BAD;
  }
  
  //flow control for prior, ensemble, and analysis begins here
  
  if ( sca_switch->makePrior ) {  // Fill prior table 
    iret = dataSet.fillPrior(sca_prior_h, sca_prior);
    if (iret) {
      cerr << "fillPrior failed. Exit " << endl;
      return  STAFCV_BAD;
    }
  } //end makePrior option
  
  // Removed the duplicated calls to dataSet.CalcDimension & 
  // dataSet.fillScaOutTable. They are commomn to both
  // makeEnsembleAve & doAnalysis options.  -- DSW  Aug 10,1998

  // Do CalcDimension() and fill sca_out table 
  if ( sca_switch->makeEnsembleAve || sca_switch->doAnalysis) {
    iret = dataSet.CalcDimension();
    if (iret) {
      cerr << "CalcDimension failed. Exit " << endl;
      return  STAFCV_BAD;
    }
    iret = dataSet.fillScaOutTable(sca_out_h, sca_out);
    if (iret) {
      cerr << "fillScaOutTable failed. Exit " << endl;
      return  STAFCV_BAD;
    }
  } //end makeEnsemble and/or doAnalysis option(s).

  // Added  ensemble_sum.  -- DSW  Aug 10, 1998
  if ( sca_switch->makeEnsembleAve ) {
    iret = ensemble_sum( sca_out_h, sca_out, sca_ensemble_ave_h, 
			 sca_ensemble_ave );
    if (iret) {
      cerr << "ensemble_sum failed. Exit " << endl;
      return  STAFCV_BAD;
    }
  }

  // Added  sca_deltad.  -- DSW  Aug 10, 1998
  if ( sca_switch->doAnalysis ) {
      iret = sca_deltad ( sca_out_h, sca_out, sca_ensemble_ave_h, 
			 sca_ensemble_ave );
    if (iret) {
      cerr << "sca_deltad failed. Exit " << endl;
      return  STAFCV_BAD;
    }
  }
  
  return STAFCV_OK;
} //end sca_runsca

//------------------------------- define alloc_prior
long alloc_prior(TABLE_HEAD_ST *sca_prior_h, SCA_PRIOR_ST   *sca_prior) 
{
  int i;

  //set sca_prior to have correct number of instances of table
  if ( sca_prior_h[0].nok == 1){
    sca_prior_h[0].nok = PRIOR_SIZE;
    sca_prior[0].w = 0;
      }
  // initialize if 0
  if ( ! sca_prior[0].w ){
    // Initialize prior
    for ( i = 1 ; i < sca_prior_h[0].nok ; i++ ) sca_prior[i].w = 0;
    sca_prior[0].w = 1;
  }
  else 
    sca_prior[0].w++;

  sca_prior[0].marker = 1;
  return 0;

} // end of alloc_prior


//------------------------------- define ensemble_sum
long ensemble_sum(
    TABLE_HEAD_ST   *sca_out_h,          SCA_OUT_ST     *sca_out  ,
    TABLE_HEAD_ST   *sca_ensemble_ave_h, SCA_OUT_ST     *sca_ensemble_ave  ) {
  
  int irow=0; 
  static int first_event=1;

  
  sca_ensemble_ave_h->nok = 0;
  for (irow=0; irow < sca_out_h->nok; irow++) {
    if (!first_event) {
      // Added print statements to check scale_x and rank matching
      // between  sca_ensemble_ave and  sca_out. ---  DSW  Aug 12, 1998. 
      if (sca_ensemble_ave[irow].scale_x != sca_out[irow].scale_x)
	cerr << "ensemble_sum and sca_out scale_x mismatch" << endl;
      if (sca_ensemble_ave[irow].rank != sca_out[irow].rank)
	cerr << "ensemble_sum and sca_out rank  mismatch" << endl;
    }
    sca_ensemble_ave[irow].scale_x    = sca_out[irow].scale_x;
    sca_ensemble_ave[irow].scale_y    = sca_out[irow].scale_y;
    sca_ensemble_ave[irow].rank       = sca_out[irow].rank;
    sca_ensemble_ave[irow].volume    += sca_out[irow].volume;
    sca_ensemble_ave[irow].entropy   += sca_out[irow].entropy;
    sca_ensemble_ave[irow].dim       += sca_out[irow].dim;
    sca_ensemble_ave[irow].info      += sca_out[irow].info;
    sca_ensemble_ave[irow].dim_lower += sca_out[irow].dim_lower;
    sca_ensemble_ave_h->nok++;
  }
  // Re-set first_event                DSW  Sep 11, 1998
  first_event = 0;
  return 0;
}  // end of  ensemble_sum


//------------------------------- define  sca_deltad
long sca_deltad(
       TABLE_HEAD_ST   *sca_ensemble_ave_h, SCA_OUT_ST     *sca_ensemble_ave,
       TABLE_HEAD_ST   *sca_out_h,          SCA_OUT_ST     *sca_out  )  
{
  int irow =0, irank=0, nranks=0,  numscales=0, prev_rank=0;
  int iscale=0, iref_scale=0;
  
  double info_slope, dim_lower_slope, reference_info, reference_dim_lower;
  
  // Rows in sca_ensemble_ave & sca_out are in ascending order in 
  // scales and rank. Loop over the first rank to determine the number 
  // of scales. Note that each rank has the same number of scales. 
  numscales = 0;
  for ( irow=0; irow < sca_out_h->nok; irow++ ) {
    if ( sca_out[0].rank  != sca_out[irow].rank )
      break;
    numscales++;
  }
  
  //  Determine number of ranks.
  nranks    = 0; 
  prev_rank = 0;
  for ( irow=0; irow < sca_out_h->nok; irow++ ) {
    if (prev_rank != sca_out[irow].rank ) {
      prev_rank = sca_out[irow].rank ;
      nranks++;
    }
  }
  
  // Loop over ranks.
  irank = 0;
  while (irank < nranks) {
    // Walk through the scales for this rank
    // Skip to the starting scale for this rank. 
    iscale = iref_scale = irank*numscales;
    // Walk through the rest of the scales 
    while( sca_out[iscale].scale_x < sca_ensemble_ave[iref_scale].scale_x)
      iscale++;
    for(;iscale < (irank+1)*numscales; iscale++){
      //Skip to the appropiate reference scale
     while(iref_scale < (irank+1)*numscales-1 && 
	   sca_ensemble_ave[iref_scale+1].scale_x < sca_out[iscale].scale_x)
       iref_scale++;
     if(iref_scale == (irank+1)*numscales-1)
       break;  // at end of reference set 
     // Do a linear interpolation to the correct scale for comparing
     // This handles the case that the reference set used a different
     // step through scale than the dataset did. 
     info_slope = (sca_ensemble_ave[iref_scale+1].info - 
		   sca_ensemble_ave[iref_scale].info)/
       (sca_ensemble_ave[iref_scale+1].scale_x - 
	sca_ensemble_ave[iref_scale].scale_x);
     
     dim_lower_slope = (sca_ensemble_ave[iref_scale+1].dim_lower - 
			sca_ensemble_ave[iref_scale].dim_lower)/
       (sca_ensemble_ave[iref_scale+1].scale_x - 
	sca_ensemble_ave[iref_scale].scale_x);
     
     reference_info = sca_ensemble_ave[iref_scale].info + 
       info_slope*(sca_out[iscale].scale_x - 
		   sca_ensemble_ave[iref_scale].scale_x);
     
     reference_dim_lower = sca_ensemble_ave[iref_scale].dim_lower + 
       dim_lower_slope*(sca_out[iscale].scale_x - 
			sca_ensemble_ave[iref_scale].scale_x);
     
     sca_out[iscale].info=sca_out[iscale].info - reference_info;
     
     sca_out[iscale].dim_lower=sca_out[iscale].dim_lower - reference_dim_lower;
     
    } // end of looping through scale */
    irank++;   // increment rank by one
  } // end of looping over rank

  return 0;
} // end of sca_deltad

