/***************************************************************************
 *
 * $Id: StTccMorphologyDataClass.cc,v 1.1 1998/11/25 19:42:55 kathy Exp $
 *
 * Author: L.D. Carr --> lcarr@u.washington.edu  9.1.98
 ***************************************************************************
 *
 * Description:  StTccMorphologyDataClass.cc
 *
 * General Note:  My variable naming scheme has a prefix which indicates scope.
 *         a         --> passed variable
 *         the      --> locally instantiated variable
 *         its       --> class data member
 *         global  --> globally defined variable
 *
 ***************************************************************************
 *
 * $Log: StTccMorphologyDataClass.cc,v $
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
 * Revision 1.2  1998/11/18 10:46:54  lcarr
 * Implements a minimal staf interface that output cluster loop index only to tcc_morphology.
 *
 * Revision 1.1  1998/11/18 08:25:35  lcarr
 * Added morphology data class and commented out most of code for testing purposes.  Also added copy constructors.
 *
 * Revision 1.1  1998/11/17 09:11:58  lcarr
 * All tcc code added to repository
 *
 **************************************************************************/

#include "StTccMorphologyDataClass.hh"

StTccMorphologyDataClass::StTccMorphologyDataClass() {

  itsClusterId = 0;
  itsRowNumber = 0;
  itsSectorNumber = 0;
  itsNumberOfSequences = 0;
  itsNumberOfPixels = 0;
  itsNumberOfPads = 0;
  itsNumberOfHits = 0;
  itsTotalCharge = 0;
  itsMaxCharge = 0;
  itsAverageCharge = (double)0;
  itsMeanPadPosition = (double)0;
  itsMeanTimePosition = (double)0;
  itsPadSigma1 = (double)0;
  itsTimeSigma1 = (double)0;
  itsPadTimeSigma1 = (double)0;
  // 2 = orthogonal direction to pad-time for quadrupole
  itsPadSigma2 = (double)0;
  itsTimeSigma2 = (double)0;
  itsPadTimeSigma2 = (double)0;
  itsEcc1 = (double)0;        // eccentricity
  itsEcc2 = (double)0;
  itsLinEcc1 = (double)0;     // linearizedEccentricity
  itsLinEcc2 = (double)0;
  itsMeanX = (double)0;
  itsMeanY = (double)0;
  itsMeanZ = (double)0;
  itsClusterFlag = 0;

}

StTccMorphologyDataClass& StTccMorphologyDataClass::operator=(const StTccMorphologyDataClass&
							  aRightHandSide) {
  
  if( this != &aRightHandSide ) {

  this->itsClusterId = aRightHandSide.itsClusterId;
  this->itsRowNumber = aRightHandSide.itsRowNumber;
  this->itsSectorNumber = aRightHandSide.itsSectorNumber;
  this->itsNumberOfSequences = aRightHandSide.itsNumberOfSequences;
  this->itsNumberOfPixels = aRightHandSide.itsNumberOfPixels;
  this->itsNumberOfPads = aRightHandSide.itsNumberOfPads;
  this->itsNumberOfHits = aRightHandSide.itsNumberOfHits;
  this->itsTotalCharge = aRightHandSide.itsTotalCharge;
  this->itsMaxCharge = aRightHandSide.itsMaxCharge;
  this->itsAverageCharge = aRightHandSide.itsAverageCharge;
  this->itsMeanPadPosition = aRightHandSide.itsMeanPadPosition;
  this->itsMeanTimePosition = aRightHandSide.itsMeanTimePosition;
  this->itsPadSigma1 = aRightHandSide.itsPadSigma1;
  this->itsTimeSigma1 = aRightHandSide.itsTimeSigma1;
  this->itsPadTimeSigma1 = aRightHandSide.itsPadTimeSigma1;
  this->itsPadSigma2 = aRightHandSide.itsPadSigma2;
  this->itsTimeSigma2 = aRightHandSide.itsTimeSigma2;
  this->itsPadTimeSigma2 = aRightHandSide.itsPadTimeSigma2;
  this->itsEcc1 = aRightHandSide.itsEcc1;
  this->itsEcc2 = aRightHandSide.itsEcc2;
  this->itsLinEcc1 = aRightHandSide.itsLinEcc1;
  this->itsLinEcc2 = aRightHandSide.itsLinEcc2;
  this->itsMeanX = aRightHandSide.itsMeanX;
  this->itsMeanY = aRightHandSide.itsMeanY;
  this->itsMeanZ = aRightHandSide.itsMeanZ;
  this->itsClusterFlag = aRightHandSide.itsClusterFlag;

  }

  return *this;
}


StTccMorphologyDataClass::~StTccMorphologyDataClass() {

  //does nothing

}

void StTccMorphologyDataClass::zeroOutData() {

  itsClusterId = 0;
  itsRowNumber = 0;
  itsSectorNumber = 0;
  itsNumberOfSequences = 0;
  itsNumberOfPixels = 0;
  itsNumberOfPads = 0;
  itsNumberOfHits = 0;
  itsTotalCharge = 0;
  itsMaxCharge = 0;
  itsAverageCharge = (double)0;
  itsMeanPadPosition = (double)0;
  itsMeanTimePosition = (double)0;
  itsPadSigma1 = (double)0;
  itsTimeSigma1 = (double)0;
  itsPadTimeSigma1 = (double)0;
  // 2 = orthogonal direction to pad-time for quadrupole
  itsPadSigma2 = (double)0;
  itsTimeSigma2 = (double)0;
  itsPadTimeSigma2 = (double)0;
  itsEcc1 = (double)0;        // eccentricity
  itsEcc2 = (double)0;
  itsLinEcc1 = (double)0;     // linearizedEccentricity
  itsLinEcc2 = (double)0;
  itsMeanX = (double)0;
  itsMeanY = (double)0;
  itsMeanZ = (double)0;
  itsClusterFlag = 0;

}
