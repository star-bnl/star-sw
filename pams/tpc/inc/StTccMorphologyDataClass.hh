/***************************************************************************
 *
 * $Id: StTccMorphologyDataClass.hh,v 1.1 1998/11/25 19:42:44 kathy Exp $
 *
 * Author: L.D. Carr --> lcarr@u.washington.edu  9.1.98
 ***************************************************************************
 *
 * Description:  StTccMorphologyDataClass.hh
 *
 * General Note:  My variable naming scheme has a prefix which indicates scope.
 *         a         --> passed variable
 *         the      --> locally instantiated variable
 *         its       --> class data member
 *         global  --> globally defined variable
 *
 ***************************************************************************
 *
 * $Log: StTccMorphologyDataClass.hh,v $
 * Revision 1.1  1998/11/25 19:42:44  kathy
 * added Lincoln Carr's tcc module to tcl package -- changes to idl,inc,tcl, kumac subdirectories
 *
 * Revision 1.4  1998/11/20 15:03:16  lcarr
 * Added clusterFlag to classify clusters based on system of cuts to be
 * developed.
 * Added access to tcc_params.idl and tcc_cluster_index.idl to various methods,
 * so that the user can changes constants and cut parameters.
 *
 * Revision 1.2  1998/11/18 10:46:55  lcarr
 * Implements a minimal staf interface that output cluster loop index only to tcc_morphology.
 *
 * Revision 1.1  1998/11/18 08:25:36  lcarr
 * Added morphology data class and commented out most of code for testing purposes.  Also added copy constructors.
 *
 * Revision 1.1  1998/11/17 09:11:58  lcarr
 * All tcc code added to repository
 *
 **************************************************************************/

#ifndef StTccMorphologyDataClass_h
#define StTccMorphologyDataClass_h

class StTccMorphologyDataClass {

public:
  StTccMorphologyDataClass();   //just zeros out data members
  StTccMorphologyDataClass& operator=(const StTccMorphologyDataClass&
				      aRightHandSide);
  virtual ~StTccMorphologyDataClass();
  void zeroOutData();    //does the same thing as the constructor

  //data members
  int    itsClusterId;
  int 	 itsRowNumber;
  int	 itsSectorNumber;
  int	 itsNumberOfSequences;
  int	 itsNumberOfPixels;
  int	 itsNumberOfPads;
  int	 itsNumberOfHits;
  int	 itsTotalCharge;
  int	 itsMaxCharge;
  double itsAverageCharge;
  double itsMeanPadPosition;
  double itsMeanTimePosition;
  double itsPadSigma1;
  double itsTimeSigma1;
  double itsPadTimeSigma1;
  // 2 = orthogonal direction to pad-time for quadrupole
  double itsPadSigma2;
  double itsTimeSigma2;
  double itsPadTimeSigma2;
  double itsEcc1;        // eccentricity
  double itsEcc2;
  double itsLinEcc1;     // linearizedEccentricity
  double itsLinEcc2;
  double itsMeanX;
  double itsMeanY;
  double itsMeanZ;
  int    itsClusterFlag;
};
#endif // end StTccMorphologyDataClass_h
