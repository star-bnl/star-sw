/***************************************************************************
 *
 * $Id: StTccClusterMorphology.hh,v 1.1 1998/11/25 19:42:43 kathy Exp $
 *
 * Author: L.D. Carr --> lcarr@u.washington.edu  9.1.98
 ***************************************************************************
 *
 * Description:  StTccClusterMorphology.hh
 *
 * General Note:  My variable naming scheme has a prefix which indicates scope.
 *         a         --> passed variable
 *         the      --> locally instantiated variable
 *         its       --> class data member
 *         global  --> globally defined variable
 *
 ***************************************************************************
 *
 * $Log: StTccClusterMorphology.hh,v $
 * Revision 1.1  1998/11/25 19:42:43  kathy
 * added Lincoln Carr's tcc module to tcl package -- changes to idl,inc,tcl, kumac subdirectories
 *
 * Revision 1.4  1998/11/20 15:03:15  lcarr
 * Added clusterFlag to classify clusters based on system of cuts to be
 * developed.
 * Added access to tcc_params.idl and tcc_cluster_index.idl to various methods,
 * so that the user can changes constants and cut parameters.
 *
 * Revision 1.3  1998/11/18 10:46:53  lcarr
 * Implements a minimal staf interface that output cluster loop index only to tcc_morphology.
 *
 * Revision 1.2  1998/11/18 08:25:34  lcarr
 * Added morphology data class and commented out most of code for testing purposes.  Also added copy constructors.
 *
 * Revision 1.1  1998/11/17 09:11:58  lcarr
 * All tcc code added to repository
 *
 **************************************************************************/

#ifndef StTccClusterMorphology_h
#define StTccClusterMorphology_h

// C++ includes
#include <stdlib.h>
#include <iostream.h>
#include <math.h>
#include <assert.h>
#include <new.h>

// StTcc includes
#include "StTccMorphologyDataClass.hh"

// idl table includes
#include "PAM.h"
#include "tcl_tp_seq.h"
#include "type_shortdata.h"
#include "tcl_tpcluster.h"
#include "tcc_morphology.h"
#include "tcc_params.h"

// Note: include here cluster tables from other detectors/software
// if StTccClusterMorphology is to be used on them
// e.g. #include"L3ClusterTable.h"

class StTccClusterMorphology {
public:
  StTccClusterMorphology(); // does nothing
  StTccClusterMorphology& operator=(const StTccClusterMorphology
				    &aRightHandSide);
  virtual ~StTccClusterMorphology();
  // one could overload all methods which 
  // take table arguments to do L3, SVT, etc.
  
  int calculateMorphology(  TABLE_HEAD_ST *aInnerSectorAdcTable_h, 
			    TYPE_SHORTDATA_ST *aInnerSectorAdcTable ,
			    TABLE_HEAD_ST *aOuterSectorAdcTable_h, 
			    TYPE_SHORTDATA_ST *aOuterSectorAdcTable ,
			    TABLE_HEAD_ST *aSequenceTable_h, 
			    TCL_TP_SEQ_ST *aSequenceTable ,
			    TABLE_HEAD_ST *aClusterTable_h,  
			    TCL_TPCLUSTER_ST  *aClusterTable );
  int calculateMeanXandMeanY( TABLE_HEAD_ST *aClusterTable_h,  
			      TCL_TPCLUSTER_ST  *aClusterTable );
  int applyCuts( TABLE_HEAD_ST *aTccParameterTable_h,  
		 TCC_PARAMS_ST  *aTccParameterTable );

  int writeToTable( TABLE_HEAD_ST *aSequenceTable_h,
		    TCL_TP_SEQ_ST *aSequenceTable ,
		    TABLE_HEAD_ST *aClusterTable_h,  
		    TCL_TPCLUSTER_ST  *aClusterTable,
		    TABLE_HEAD_ST *aMorphologyTable_h, 
		    TCC_MORPHOLOGY_ST *aMorphologyTable,
		    TABLE_HEAD_ST *aTccParameterTable_h,  
		    TCC_PARAMS_ST  *aTccParameterTable );
  int writeToFile();
  void initialize( int aClusterLoopIndex );
private:
  int pixelLoop1(  TABLE_HEAD_ST *aInnerSectorAdcTable_h, 
		   TYPE_SHORTDATA_ST *aInnerSectorAdcTable ,
		   TABLE_HEAD_ST *aOuterSectorAdcTable_h, 
		   TYPE_SHORTDATA_ST *aOuterSectorAdcTable ,
		   TABLE_HEAD_ST *aSequenceTable_h, 
		   TCL_TP_SEQ_ST *aSequenceTable ,
		   TABLE_HEAD_ST *aClusterTable_h,  
		   TCL_TPCLUSTER_ST  *aClusterTable );
  int pixelLoop2(  TABLE_HEAD_ST *aInnerSectorAdcTable_h, 
		   TYPE_SHORTDATA_ST *aInnerSectorAdcTable ,
		   TABLE_HEAD_ST *aOuterSectorAdcTable_h, 
		   TYPE_SHORTDATA_ST *aOuterSectorAdcTable ,
		   TABLE_HEAD_ST *aSequenceTable_h, 
		   TCL_TP_SEQ_ST *aSequenceTable ,
		   TABLE_HEAD_ST *aClusterTable_h,  
		   TCL_TPCLUSTER_ST  *aClusterTable );
  int readParameterTable();
  int itsClusterLoopIndex;
  StTccMorphologyDataClass itsMorphology;
  
};
#endif // end StTccClusterMorphology_h
