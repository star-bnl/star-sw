/***************************************************************************
 *
 * $Id: tcc.cc,v 1.1 1998/11/25 19:42:55 kathy Exp $
 *
 * Author: L.D. Carr --> lcarr@u.washington.edu  9/1/98
 ***************************************************************************
 *
 * Description:  tcc.cc loops over all clusters in sector and calls morphology
 *                     measures on each one with StTccClusterMorphology 
 *
 * General Note:  My variable naming scheme has a prefix which indicates scope.
 *         a         --> passed variable
 *         the      --> locally instantiated variable
 *         its       --> class data member
 *         global  --> globally defined variable
 *
 ***************************************************************************
 *
 * $Log: tcc.cc,v $
 * Revision 1.1  1998/11/25 19:42:55  kathy
 * added Lincoln Carr's tcc module to tcl package -- changes to idl,inc,tcl, kumac subdirectories
 *
 * Revision 1.4  1998/11/20 15:04:38  lcarr
 * Added clusterFlag to classify clusters based on system of cuts to be
 * developed.
 * Added access to tcc_params.idl and tcc_cluster_index.idl to various methods,
 * so that the user can changes constants and cut parameters.
 * Added read macro to init_tcc.kumac to get parameter table tcc_params.xdf
 *
 * Revision 1.3  1998/11/18 10:46:56  lcarr
 * Implements a minimal staf interface that output cluster loop index only to tcc_morphology.
 *
 * Revision 1.2  1998/11/18 08:25:37  lcarr
 * Added morphology data class and commented out most of code for testing purposes.  Also added copy constructors.
 *
 * Revision 1.1  1998/11/17 09:12:01  lcarr
 * All tcc code added to repository
 *
 **************************************************************************/

#include "tcc.h"
#include "StTccClusterMorphology.hh"

void out_of_memory()
{
  cerr << "Out Of Memory. Program Is Terminating." << endl;
  exit(-1);
} // end out_of_memory

long type_of_call tcc_(  TABLE_HEAD_ST *aInnerSectorAdcTable_h, 
			 TYPE_SHORTDATA_ST *aInnerSectorAdcTable ,
			 TABLE_HEAD_ST *aOuterSectorAdcTable_h, 
			 TYPE_SHORTDATA_ST *aOuterSectorAdcTable ,
			 TABLE_HEAD_ST *aSequenceTable_h, 
			 TCL_TP_SEQ_ST *aSequenceTable ,
			 TABLE_HEAD_ST *aClusterTable_h,  
			 TCL_TPCLUSTER_ST  *aClusterTable,
			 TABLE_HEAD_ST *aMorphologyTable_h, 
			 TCC_MORPHOLOGY_ST *aMorphologyTable,
			 TABLE_HEAD_ST *aTccParameterTable_h, 
			 TCC_PARAMS_ST *aTccParameterTable,
			 TABLE_HEAD_ST *aClusterIndexTable_h, 
			 TCC_CLUSTER_INDEX_ST *aClusterIndexTable )  {

  
  //variable declarations
  register int theClusterLoopIndex;
  int theCurrentSector, theSetBit;
  StTccClusterMorphology *theMorphologyObject;
  theMorphologyObject = new StTccClusterMorphology;

  set_new_handler(out_of_memory);   //replaces assert ( ... != 0 );

  // set number of instances of tcc_morphology table for staf
  aMorphologyTable_h[0].nok = aClusterTable_h[0].nok;

  // Note: aClusterIndexTable.cluster_index marks starting point for
  // next sector

  //initialize flow variables
  theSetBit = 1;
  theCurrentSector 
    = aClusterTable[aClusterIndexTable[0].cluster_index].tpc_row/100;

  // begin loop through all clusters in this sector
  for ( theClusterLoopIndex = aClusterIndexTable[0].cluster_index; 
	theClusterLoopIndex < aClusterTable_h[0].nok
	  && theSetBit == 1; 
	theClusterLoopIndex++ ) {
    
    //check to make sure haven't moved on to next sector
    if( aClusterTable[theClusterLoopIndex].tpc_row/100
	!= theCurrentSector ) {
      theSetBit = 0; // i.e. exit loop
    }

    else {
      theMorphologyObject->initialize( theClusterLoopIndex );
      
      theMorphologyObject->
	calculateMorphology(  aInnerSectorAdcTable_h, aInnerSectorAdcTable,
			      aOuterSectorAdcTable_h, aOuterSectorAdcTable , 
			      aSequenceTable_h, aSequenceTable ,
			      aClusterTable_h,  aClusterTable );

      // momentarily a placeholder -- see applyCuts() method
      // is StTccClusterMorphology.cc
      theMorphologyObject->
	applyCuts( aTccParameterTable_h, aTccParameterTable );

      
      theMorphologyObject->
	writeToTable( aSequenceTable_h, aSequenceTable, 
		      aClusterTable_h, aClusterTable, 
		      aMorphologyTable_h, aMorphologyTable,
		      aTccParameterTable_h, aTccParameterTable );    
    } // end else
    
  } // end loop over all clusters

  //set starting point in cluster table for next sector
  aClusterIndexTable[0].cluster_index = theClusterLoopIndex;

  delete theMorphologyObject;
  
  return STAFCV_OK;
} // end tcc
