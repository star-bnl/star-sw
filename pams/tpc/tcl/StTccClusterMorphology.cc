/***************************************************************************
 *
 * $Id: StTccClusterMorphology.cc,v 1.1 1998/11/25 19:42:54 kathy Exp $
 *
 * Author: L.D. Carr --> lcarr@u.washington.edu  9/1/98
 ***************************************************************************
 *
 * Description:  StTccClusterMorphology.cc
 *
 * General Note:  My variable naming scheme has a prefix which indicates scope.
 *         a         --> passed variable
 *         the      --> locally instantiated variable
 *         its       --> class data member
 *         global  --> globally defined variable
 *
 ***************************************************************************
 *
 * $Log: StTccClusterMorphology.cc,v $
 * Revision 1.1  1998/11/25 19:42:54  kathy
 * added Lincoln Carr's tcc module to tcl package -- changes to idl,inc,tcl, kumac subdirectories
 *
 * Revision 1.4  1998/11/20 15:04:37  lcarr
 * Added clusterFlag to classify clusters based on system of cuts to be
 * developed.
 * Added access to tcc_params.idl and tcc_cluster_index.idl to various methods,
 * so that the user can changes constants and cut parameters.
 * Added read macro to init_tcc.kumac to get parameter table tcc_params.xdf
 *
 * Revision 1.3  1998/11/18 10:46:52  lcarr
 * Implements a minimal staf interface that output cluster loop index only to tcc_morphology.
 *
 * Revision 1.2  1998/11/18 08:25:33  lcarr
 * Added morphology data class and commented out most of code for testing purposes.  Also added copy constructors.
 *
 * Revision 1.1  1998/11/17 09:11:57  lcarr
 * All tcc code added to repository
 *
 **************************************************************************/

#include "StTccClusterMorphology.hh"

StTccClusterMorphology::StTccClusterMorphology() {

  //does nothing

} // end default constructor

StTccClusterMorphology& StTccClusterMorphology::operator=(const StTccClusterMorphology&
							  aRightHandSide) {
  
  if( this != &aRightHandSide ) {

    this->itsClusterLoopIndex = aRightHandSide.itsClusterLoopIndex;
    this->itsMorphology = aRightHandSide.itsMorphology;

  }

  return *this;
}

StTccClusterMorphology::~StTccClusterMorphology() {

  //does nothing

} //end destructor

int StTccClusterMorphology
::calculateMorphology( TABLE_HEAD_ST *aInnerSectorAdcTable_h, 
		       TYPE_SHORTDATA_ST *aInnerSectorAdcTable,
		       TABLE_HEAD_ST *aOuterSectorAdcTable_h, 
		       TYPE_SHORTDATA_ST *aOuterSectorAdcTable,
		       TABLE_HEAD_ST *aSequenceTable_h, 
		       TCL_TP_SEQ_ST *aSequenceTable,
		       TABLE_HEAD_ST *aClusterTable_h,  
		       TCL_TPCLUSTER_ST *aClusterTable ) {
  
  pixelLoop1( aInnerSectorAdcTable_h, aInnerSectorAdcTable,
	      aOuterSectorAdcTable_h, aOuterSectorAdcTable,
	      aSequenceTable_h, aSequenceTable,
	      aClusterTable_h,  aClusterTable );
  pixelLoop2( aInnerSectorAdcTable_h, aInnerSectorAdcTable,
	      aOuterSectorAdcTable_h, aOuterSectorAdcTable,
	      aSequenceTable_h, aSequenceTable,
	      aClusterTable_h,  aClusterTable );
  calculateMeanXandMeanY( aClusterTable_h, aClusterTable );
  
  return STAFCV_OK;
} // end calculateMorphology()

int StTccClusterMorphology
::writeToTable( TABLE_HEAD_ST *aSequenceTable_h, 
		TCL_TP_SEQ_ST *aSequenceTable,
		TABLE_HEAD_ST *aClusterTable_h,  
		TCL_TPCLUSTER_ST *aClusterTable,
		TABLE_HEAD_ST *aMorphologyTable_h, 
		TCC_MORPHOLOGY_ST *aMorphologyTable,
		TABLE_HEAD_ST *aTccParameterTable_h,  
		TCC_PARAMS_ST  *aTccParameterTable ) {

  //should pull these number from params table
  float theTpcLength = aTccParameterTable[0].tpcLength;
  float theNumTimeBins = aTccParameterTable[0].numTimeBins;
  float theLinearizationFactor = aTccParameterTable[0].linearizationFactor;
  float theTanLinearizationFactor = tan( theLinearizationFactor );

  // give the cluster an id number and write out preliminary
  // data which is just a copy of data in other tpc tables
  aMorphologyTable[itsClusterLoopIndex].clusterId 
    = itsClusterLoopIndex + 1;
  aMorphologyTable[itsClusterLoopIndex].rowNumber 
    = itsMorphology.itsRowNumber;
  aMorphologyTable[itsClusterLoopIndex].sectorNumber 
    = aClusterTable[itsClusterLoopIndex].tpc_row/100;
  aMorphologyTable[itsClusterLoopIndex].numberOfSequences 
    = aClusterTable[itsClusterLoopIndex].nseq;
  aMorphologyTable[itsClusterLoopIndex].numberOfHits 
    = aClusterTable[itsClusterLoopIndex].nhits;
  
  
  //write out data obtained in pixelLoop1( ... )
  aMorphologyTable[itsClusterLoopIndex].numberOfPads 
    = itsMorphology.itsNumberOfPads;
  aMorphologyTable[itsClusterLoopIndex].numberOfPixels 
    = itsMorphology.itsNumberOfPixels;
  aMorphologyTable[itsClusterLoopIndex].totalCharge 
    = itsMorphology.itsTotalCharge;
  aMorphologyTable[itsClusterLoopIndex].averageCharge 
    = (float)itsMorphology.itsTotalCharge
    /(float)itsMorphology.itsNumberOfPixels;
  aMorphologyTable[itsClusterLoopIndex].meanPadPosition 
    = itsMorphology.itsMeanPadPosition;
  aMorphologyTable[itsClusterLoopIndex].meanTimePosition 
    = itsMorphology.itsMeanTimePosition;
  aMorphologyTable[itsClusterLoopIndex].maxCharge 
    = itsMorphology.itsMaxCharge;
  aMorphologyTable[itsClusterLoopIndex].meanZ
    = itsMorphology.itsMeanTimePosition
    *theTpcLength/theNumTimeBins;

  //write out data obtained in pixelLoop2( ... )
  aMorphologyTable[itsClusterLoopIndex].padSigma1 
    = sqrt(itsMorphology.itsPadSigma1);
  aMorphologyTable[itsClusterLoopIndex].timeSigma1 
    = sqrt(itsMorphology.itsTimeSigma1);
  aMorphologyTable[itsClusterLoopIndex].padTimeSigma1 
    = sqrt(fabs(itsMorphology.itsPadTimeSigma1));
  aMorphologyTable[itsClusterLoopIndex].padSigma2 
    = sqrt(itsMorphology.itsPadSigma2);
  aMorphologyTable[itsClusterLoopIndex].timeSigma2 
    = sqrt(itsMorphology.itsTimeSigma2);
  aMorphologyTable[itsClusterLoopIndex].padTimeSigma2 
    = sqrt(fabs(itsMorphology.itsPadTimeSigma2));
  aMorphologyTable[itsClusterLoopIndex].ecc1
    = itsMorphology.itsPadTimeSigma1
    /aMorphologyTable[itsClusterLoopIndex].padSigma1
    /aMorphologyTable[itsClusterLoopIndex].timeSigma1;
  aMorphologyTable[itsClusterLoopIndex].ecc2
    = itsMorphology.itsPadTimeSigma2
    /aMorphologyTable[itsClusterLoopIndex].padSigma2
    /aMorphologyTable[itsClusterLoopIndex].timeSigma2;
  aMorphologyTable[itsClusterLoopIndex].linEcc1 
    = tan( theLinearizationFactor 
	   *aMorphologyTable[itsClusterLoopIndex].ecc1 )
    / theTanLinearizationFactor;
  aMorphologyTable[itsClusterLoopIndex].linEcc2 
    = tan( theLinearizationFactor 
	   *aMorphologyTable[itsClusterLoopIndex].ecc2 )
    / theTanLinearizationFactor;

  // write data obtained in calculateMeanXandMeanY( ... )    
  aMorphologyTable[itsClusterLoopIndex].meanX 
    = itsMorphology.itsMeanX;
  aMorphologyTable[itsClusterLoopIndex].meanY 
    = itsMorphology.itsMeanY;

  //write data obtained in applyCuts()
  aMorphologyTable[itsClusterLoopIndex].clusterFlag 
    = itsMorphology.itsClusterFlag;

  
  /////////////////////////////////////////////////////////////
  // test function only
  // type cast here for double = atan(double);
  //     aMorphologyTable[itsClusterLoopIndex].crossAngle
  // = radiansToDegrees * atan((double)(theMeanX/((1950.0) - theMeanY)));
  ////////////////////////////////////////////////////////////
  
  return STAFCV_OK;

} // end writeToTable()

int StTccClusterMorphology
::applyCuts( TABLE_HEAD_ST *aTccParameterTable_h,  
	     TCC_PARAMS_ST  *aTccParameterTable) {

  int theNumTpcSlices = aTccParameterTable[0].numTpcSlices;

  //placeholder
  itsMorphology.itsClusterFlag = 1;

  return STAFCV_OK;

} // end applyCuts()

int StTccClusterMorphology
::pixelLoop1( TABLE_HEAD_ST *aInnerSectorAdcTable_h, 
	      TYPE_SHORTDATA_ST *aInnerSectorAdcTable,
	      TABLE_HEAD_ST *aOuterSectorAdcTable_h, 
	      TYPE_SHORTDATA_ST *aOuterSectorAdcTable,
	      TABLE_HEAD_ST *aSequenceTable_h, 
	      TCL_TP_SEQ_ST *aSequenceTable,
	      TABLE_HEAD_ST *aClusterTable_h,  
	      TCL_TPCLUSTER_ST *aClusterTable ) {
  
  // flow variables
  register int theSequenceLoopIndex, thePixelLoopIndex;
  int pointerToFirstSequence = aClusterTable[itsClusterLoopIndex].jseq;
  int pointerToFirstPixelInSequence;
  int theSequenceLength;

  // calculational variables
  int theTimePosition = 0;
  int theAdcValue = 0;
  int thePadPosition = 0;
  int thePreviousPadPosition = 0;
  int theMeanPadPositionSum = 0;
  int theMeanTimePositionSum = 0;

  //initialization of some variables
  itsMorphology.itsRowNumber = aClusterTable[itsClusterLoopIndex].tpc_row%100;

  // begin loop through sequences
  for ( theSequenceLoopIndex = 0; 
	theSequenceLoopIndex < aClusterTable[itsClusterLoopIndex].nseq;
	theSequenceLoopIndex++ ) {

    // get pad information for this sequence
    thePadPosition = aSequenceTable[pointerToFirstSequence - 1].secpad;

    if( thePreviousPadPosition != thePadPosition ) 
      itsMorphology.itsNumberOfPads++;
    thePreviousPadPosition = thePadPosition;
    
    // set up flow parameters for loop through pixels in sequence
    // the - 1 offsets are to accomodate tcl fortran style arrays
    pointerToFirstPixelInSequence = 
      aSequenceTable[pointerToFirstSequence - 1].jpix - 1;
    theSequenceLength = 
      aSequenceTable[pointerToFirstSequence - 1].tdc_hi 
      - aSequenceTable[pointerToFirstSequence - 1].tdc_low + 1;
    
    //begin loop through pixels in sequence    
    // Note:  I have use the <= condition in the for loop
    // to account for the case where theSequenceLength == 0
    for ( thePixelLoopIndex = pointerToFirstPixelInSequence; 
	  thePixelLoopIndex <
	    (pointerToFirstPixelInSequence + theSequenceLength);
	  thePixelLoopIndex++ ) {

      // get adc and time information out of tables
      
      if ( itsMorphology.itsRowNumber <= 13 )
	theAdcValue = aInnerSectorAdcTable[thePixelLoopIndex].data;
      else
	theAdcValue = aOuterSectorAdcTable[thePixelLoopIndex].data;
      
      theTimePosition = aSequenceTable[pointerToFirstSequence - 1].tdc_low
	+ (thePixelLoopIndex - pointerToFirstPixelInSequence);      

      // sum calculated quantities
      itsMorphology.itsNumberOfPixels++;
      itsMorphology.itsTotalCharge  += theAdcValue;
      theMeanPadPositionSum += (thePadPosition * theAdcValue);
      theMeanTimePositionSum += (theTimePosition * theAdcValue);

      // cumulative sort to find max charge
      if( itsMorphology.itsMaxCharge < theAdcValue ) itsMorphology.itsMaxCharge = theAdcValue;

    } // end loop through pixels in sequence
    
    // flow control
    if( theSequenceLoopIndex < (aClusterTable[itsClusterLoopIndex].nseq - 1)) {
      pointerToFirstSequence = 
	aSequenceTable[pointerToFirstSequence-1].next;
    }
  } // end loop through all sequences in cluster 

  // normalize theMeanPadPositionSum and theMeanTimePositionSum
  itsMorphology.itsMeanPadPosition = 
    (float)theMeanPadPositionSum / (float)itsMorphology.itsTotalCharge;
  itsMorphology.itsMeanTimePosition = 
    (float)theMeanTimePositionSum / (float)itsMorphology.itsTotalCharge;

  return STAFCV_OK;
} // end pixelLoop1()

int StTccClusterMorphology
::pixelLoop2( TABLE_HEAD_ST *aInnerSectorAdcTable_h, 
	      TYPE_SHORTDATA_ST *aInnerSectorAdcTable,
	      TABLE_HEAD_ST *aOuterSectorAdcTable_h, 
	      TYPE_SHORTDATA_ST *aOuterSectorAdcTable,
	      TABLE_HEAD_ST *aSequenceTable_h, 
	      TCL_TP_SEQ_ST *aSequenceTable,
	      TABLE_HEAD_ST *aClusterTable_h,  
	      TCL_TPCLUSTER_ST *aClusterTable ) {
  
    
  // flow variables
  register int theSequenceLoopIndex, thePixelLoopIndex;
  int pointerToFirstSequence = aClusterTable[itsClusterLoopIndex].jseq;
  int pointerToFirstPixelInSequence;
  int theSequenceLength;

  // calculational variables
  int thePadPosition = 0;
  int theTimePosition = 0;
  int theAdcValue = 0;
  float theNormalizedAdcValue = 0.0;
  float thePadSpread = 0.0;
  float theTimeSpread = 0.0;

  // These are used in the special case of one time bin or one pad.
  // Could be reset as needed -- could also come from parameter table
  const float theOnePadSigma = 0.5;
  const float theOneTimeBinSigma = 0.5;
  
  // begin loop through sequences
  for ( theSequenceLoopIndex = 0; 
	theSequenceLoopIndex < aClusterTable[itsClusterLoopIndex].nseq;
	theSequenceLoopIndex++ ) {
    
    // get pad information for this sequence
    thePadPosition = aSequenceTable[pointerToFirstSequence - 1].secpad;
    
    // set up flow parameters for loop through pixels in sequence
    // the - 1 offsets are to accomodate tcl fortran style arrays
    pointerToFirstPixelInSequence = 
      aSequenceTable[pointerToFirstSequence - 1].jpix - 1;
    theSequenceLength = 
      aSequenceTable[pointerToFirstSequence - 1].tdc_hi 
      - aSequenceTable[pointerToFirstSequence - 1].tdc_low + 1;
    
    
    //begin loop through pixels in sequence
    // Note:  I have use the <= condition in the for loop
    // to account for the case where theSequenceLength == 0
    for ( thePixelLoopIndex = pointerToFirstPixelInSequence; 
	  thePixelLoopIndex <
	    (pointerToFirstPixelInSequence + theSequenceLength);
	  thePixelLoopIndex++ ) {
      
      if ( itsMorphology.itsRowNumber <= 13 )
	theAdcValue = aInnerSectorAdcTable[thePixelLoopIndex].data;
      else
	theAdcValue = aOuterSectorAdcTable[thePixelLoopIndex].data;
      
      theTimePosition = aSequenceTable[pointerToFirstSequence - 1].tdc_low
	+ (thePixelLoopIndex - pointerToFirstPixelInSequence);      
      theNormalizedAdcValue = 
	(float)theAdcValue / (float)itsMorphology.itsTotalCharge;
      

      //calculate the intermediate variables
      thePadSpread = (float)thePadPosition 
	- itsMorphology.itsMeanPadPosition;
      theTimeSpread = (float)theTimePosition 
	- itsMorphology.itsMeanTimePosition;

      // check for special cases;
      if( thePadSpread == 0 )
	thePadSpread = theOnePadSigma / sqrt(theNormalizedAdcValue);
      if( theTimeSpread == 0 )
	theTimeSpread = theOneTimeBinSigma / sqrt(theNormalizedAdcValue);
      	
	// calculate all the variances
	itsMorphology.itsPadSigma1 +=
	  thePadSpread * thePadSpread * theNormalizedAdcValue;
	itsMorphology.itsTimeSigma1 += 
	  theTimeSpread * theTimeSpread * theNormalizedAdcValue;
	itsMorphology.itsPadTimeSigma1 += 
	  thePadSpread * theTimeSpread * theNormalizedAdcValue;
	itsMorphology.itsPadSigma2 += 
	  (0.5 
	   * (thePadSpread + theTimeSpread)
	   * (thePadSpread + theTimeSpread)
	   * theNormalizedAdcValue);
	itsMorphology.itsTimeSigma2 +=
	  (0.5 
	   * (thePadSpread - theTimeSpread)
	   * (thePadSpread - theTimeSpread)
	   * theNormalizedAdcValue);
	itsMorphology.itsPadTimeSigma2+=
	  (0.5 
	   * (thePadSpread + theTimeSpread)
	   * (thePadSpread - theTimeSpread)
	   * theNormalizedAdcValue);
       
    } // end second loop through pixels in sequence

    // flow control
    if( theSequenceLoopIndex < (aClusterTable[itsClusterLoopIndex].nseq - 1))
      pointerToFirstSequence = 
	aSequenceTable[pointerToFirstSequence-1].next;

  } // end loop through all sequences in cluster 
  
  return STAFCV_OK;
} // end pixelLoop2()

int StTccClusterMorphology
::calculateMeanXandMeanY( TABLE_HEAD_ST *aClusterTable_h,  
			  TCL_TPCLUSTER_ST *aClusterTable) {

  //this function is very ugly.  Suggestions are welcome.  LDC

  //variable definitions
  int theRowNumber;
  float thePadLength;
  float theNumPadsInRow;
  float offset;

  //initialize variables
  theRowNumber = aClusterTable[itsClusterLoopIndex].tpc_row%100;

  //begin meanXandMeanY calculations
  // do the outer sector case first
  if(theRowNumber > 13 ) {
    thePadLength = 6.7;
    itsMorphology.itsMeanY = (1272)
      + ((float)(theRowNumber-14) 
	 * (20.0));

      // have to do special cases for theMeanX
      // due to geometry oddities of tpc
      if(theRowNumber<19) {
	theNumPadsInRow = 
	  (float)(98 + 2*(theRowNumber - 14));
	offset = (0.5)*theNumPadsInRow;
	itsMorphology.itsMeanX = (itsMorphology.itsMeanPadPosition - offset)*thePadLength;
      }
      else if(theRowNumber<23) {
	theNumPadsInRow = 
	  (float)(106 + 2*(theRowNumber - 19));
	offset = (0.5)*theNumPadsInRow;
	itsMorphology.itsMeanX = 
	  (itsMorphology.itsMeanPadPosition - offset)*thePadLength;
      }
      else if(theRowNumber<29) {
	theNumPadsInRow = 
	  (float)(112 + 2*(theRowNumber - 23));
	offset = (0.5)*theNumPadsInRow;
	itsMorphology.itsMeanX = 
	  (itsMorphology.itsMeanPadPosition - offset)*thePadLength;
      }
      else if(theRowNumber<33) {
	theNumPadsInRow = 
	  (float)(122 + 2*(theRowNumber - 29));
	offset = (0.5)*theNumPadsInRow;
	itsMorphology.itsMeanX = 
	  (itsMorphology.itsMeanPadPosition - offset)*thePadLength;
      }
      else if(theRowNumber<39) {
	theNumPadsInRow = 
	  (float)(128 + 2*(theRowNumber - 33));
	offset = (0.5)*theNumPadsInRow;
	itsMorphology.itsMeanX = 
	  (itsMorphology.itsMeanPadPosition - offset)*thePadLength;
      }
      else if(theRowNumber<43) {
	theNumPadsInRow = 
	  (float)(138 + 2*(theRowNumber - 39));
	offset = (0.5)*theNumPadsInRow;
	itsMorphology.itsMeanX = 
	  (itsMorphology.itsMeanPadPosition - offset)*thePadLength;
      }
      else {
	theNumPadsInRow = (float)(144);
	offset = (0.5)*theNumPadsInRow;
	itsMorphology.itsMeanX = (itsMorphology.itsMeanPadPosition - offset)*thePadLength;

      } // end meanX cases	
    } // end if statement for outer sector case

    // now do inner sector case
    else {
      thePadLength = 3.35;

      // do meanY cases
      if( theRowNumber < 8 ) {
	itsMorphology.itsMeanY = 
	  (float)( (theRowNumber-1) 
		   * 48 + 600 );
      }
      else itsMorphology.itsMeanY = 
	     (float)( ((theRowNumber-8) 
		       * 52) + 936 );

      // do meanX cases
      if( theRowNumber < 5 ) {
	theNumPadsInRow = 
	  (float)(88 + 8*(theRowNumber - 1));
	offset = (0.5)*theNumPadsInRow;
	itsMorphology.itsMeanX = 
	  (itsMorphology.itsMeanPadPosition - offset)*thePadLength;
      }
      else {
	theNumPadsInRow = 
	  (float)(118 + 8*(theRowNumber - 5));
	offset = (0.5)*theNumPadsInRow;
	itsMorphology.itsMeanX = 
	  (itsMorphology.itsMeanPadPosition - offset)*thePadLength;
      } // end meanX cases
    } // end inner sector case
  
  return STAFCV_OK;
} // end calculateMeanXandMeanY()

void StTccClusterMorphology::initialize( int aClusterLoopIndex ) {
  
  itsClusterLoopIndex = aClusterLoopIndex;
  itsMorphology.zeroOutData();
}
