/***************************************************************************
 *
 * Author: Dominik Flierl, flierl@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a object which uses TMinuit to fit correlationfunctions
 *  
 **************************************************************************/

#ifndef dFitter3d_hh
#define dFitter3d_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "TMinuit.h"

class dFitter3d : public TObject
{
    public :
	////
	// constructors
	////
	dFitter3d( TMinuit* gMinuit, StHbt3DHisto* numerator, StHbt3DHisto* denominator, TString opt, TString opt ) ;
        ~dFitter3d() ;
    
	////
	// essential members
	////
	// fill internal arrays
	void FillInternalArrays() ;
	// trigger minimazation using TMinuit
	void doFit() ;

	////
	// fit methods
	////
	// set fitting method to be used (MML or Chi2)
	void setFitMethod(string opt) ;
	// the general fcn which will call fcnChi2 or fcnMML
	void mfcn(Int_t &nParamters, Double_t *gin, Double_t &finalChi2, Double_t *parameterSet, Int_t iflag) ;
	// define chi2 which will be mimized by TMinuit
	void fcnChi2(Int_t &nParamters, Double_t *gin, Double_t &finalChi2, Double_t *parameterSet, Int_t iflag) ;
	// define Maximum Likelihood which will be minimized by TMinuit
	void fcnMml(Int_t &nParamters, Double_t *gin, Double_t &finalChi2, Double_t *parameterSet, Int_t iflag) ;
	// ln of factorial value of arg : helper function for Maximum Likelihood
	double lnfactorial(double arg) ;

  
	////
	// correlation function
	////
	// set correlation function to be fitted (YKP or BP)
	void setCorrFctn(string opt) ;
	// return C2 at 'position' with 'parameterSet' for either ykp or bp
	double mCorrelationFunction(StThreeVectorD& position, double* parameterSet ) ;
	// return C2 for the YKP parametrization at 'position' with 'parameterSet'
	double ykpCorrelationFunction(StThreeVectorD& position, double* parameterSet ) ;
	// return C2 for the BP parametrization at 'position' with 'parameterSet'
	double bpCorrelationFunction(StThreeVectorD& position, double* parameterSet ) ;


	////
	// general stuff
	////
	// get histos
	StHbt3DHisto* Numerator() ;
	StHbt3DHisto* Denominator() ;
	StHbt3DHisto* Ratio() ;
	// get/set norm
	void SetNormFactor(double norm) ;
	double Norm() ;
	// get/set thresholod
	void SetThresholdNumerator(double thresN) ; 
	void SetThresholdDenominator(double thresD) ;
	double ThresholdNumerator() ; 
	double ThresholdDenominator() ;
	// set input histos
	void SetHistos(StHbt3DHisto* numerator, StHbt3DHisto* denominator) ;
    
	

	private :	

	// pointer to TMinuit itself ( it's already global, but well you know ... :) )
	TMinuit* mMinuit ;
	
	// internal Arrays
	int mInternalArraySize ;
	double* mRatioInternalArray ;
	double* mNumeratorInternalArray ;
	double* mDenominatorInternalArray ;
	double* mErrorInternalArray ;
	StThreeVectorD* mVariablePositionArray ;


	// option switches
	// chi2 or mml
	string mFitMethod ;
	// ykp or bp
	string mCorrFctnType ;

	// basic functions which are not yet provided by ROOT itself 
	void Bin1ToBin3(StHbt3DHisto* histo, int bin, int& binx, int& biny, int& binz) ;
		
	// count how often we called fcn -> check minuit :)
	int countMinuitCalls ;
	
	// the histos 
	// 3d authentic
	StHbt3DHisto* mNumerator ; 
	StHbt3DHisto* mDenominator ;
	StHbt3DHisto* mRatio ; 
	
	// parameters
	double mNorm ;
	double mThresholdNumerator ;
	double mThresholdDenominator ; 
	
	// constants
	double mhc ;  // 0.197 GeVfm
	double mhc2 ; // 0.038 (GeVfm)^2
	
#ifdef __ROOT__
  ClassDef(dFitter3d, 0)
#endif

};

////
// inline functions
////
// 3d-original
inline  StHbt3DHisto* dFitter3d::Numerator(){return mNumerator;}
inline  StHbt3DHisto* dFitter3d::Denominator(){return mDenominator;}
inline  StHbt3DHisto* dFitter3d::Ratio(){return mRatio;}
// norm
inline double  dFitter3d::Norm(){return mNorm; }
inline void dFitter3d::SetNormFactor(double norm)  { mNorm = norm ; }
//  threshold
inline double  dFitter3d::ThresholdNumerator(){return mThresholdNumerator; }
inline double  dFitter3d::ThresholdDenominator(){return mThresholdDenominator; }
inline void dFitter3d::SetThresholdNumerator(double thresN) { mThresholdNumerator = thresN ; }
inline void dFitter3d::SetThresholdDenominator(double thresD) { mThresholdDenominator = thresD ; }

#endif
