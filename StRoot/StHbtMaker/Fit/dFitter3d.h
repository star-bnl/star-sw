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


#include "TH3.h"
#include "TH1.h"
#include "TMinuit.h"
#include "TVector3.h"
#include "TString.h"
#include "TObject.h"

class dFitter3d : public TObject
{
    public :
	////
	// constructors
	////
	dFitter3d( TMinuit* gMinuit, TH3D* numerator, TH3D* denominator, TString opt, TString opt2 ) ;
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
	void setFitMethod(TString opt) ;
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
	void setCorrFctn(TString opt) ;
	// return C2 at 'position' with 'parameterSet' for either ykp or bp
	double mCorrelationFunction(TVector3& position, double* parameterSet ) ;
	// return C2 for the YKP parametrization at 'position' with 'parameterSet'
	double ykpCorrelationFunction(TVector3& position, double* parameterSet ) ;
	// return C2 for the BP parametrization at 'position' with 'parameterSet'
	double bpCorrelationFunction(TVector3& position, double* parameterSet ) ;


	////
	// general stuff
	////
	// get histos
	TH3D* Numerator() ;
	TH3D* Denominator() ;
	TH3D* Ratio() ;
	// get/set norm
	void SetNormFactor(double norm) ;
	double Norm() ;
	// get/set thresholod
	void SetThresholdNumerator(double thresN) ; 
	void SetThresholdDenominator(double thresD) ;
	double ThresholdNumerator() ; 
	double ThresholdDenominator() ;
	// set input histos
	void SetHistos(TH3D* numerator, TH3D* denominator) ;
	// get Minuit
	TMinuit* getMinuit() { return mMinuit ; } ; 
	// options
	void SetSphereLimit(double limit) { mSphereLimit = limit*limit ; } ;
	double GetSphereLimit() { return mSphereLimit; } ;
    
	
	private :	

	// pointer to TMinuit itself ( it's already global, but well you know ... :) )
	TMinuit* mMinuit ;
	
	// internal Arrays
	int mInternalArraySize ;
	double* mRatioInternalArray ;
	double* mNumeratorInternalArray ;
	double* mDenominatorInternalArray ;
	double* mErrorInternalArray ;
	TVector3* mVariablePositionArray ;


	// options
	// chi2 or mml
	TString mFitMethod ;
	// ykp or bp
	TString mCorrFctnType ;
	// spherelimit
	double mSphereLimit ;

	// basic functions which are not yet provided by ROOT itself 
	void Bin1ToBin3(TH3D* histo, int bin, int& binx, int& biny, int& binz) ;
		
	// count how often we called fcn -> check minuit :)
	int countMinuitCalls ;
	
	// the histos 
	// 3d authentic
	TH3D* mNumerator ; 
	TH3D* mDenominator ;
	TH3D* mRatio ; 
	
	// parameters
	double mNorm ;
	double mThresholdNumerator ;
	double mThresholdDenominator ; 
	
	// constants
	double mhc ;  // 0.197 GeVfm
	double mhc2 ; // 0.038 (GeVfm)^2
	
	// root dictionary
#ifdef __ROOT__
	ClassDef(dFitter3d, 0)
#endif

};

////
// inline functions
////
// 3d-original
inline  TH3D* dFitter3d::Numerator(){return mNumerator;}
inline  TH3D* dFitter3d::Denominator(){return mDenominator;}
inline  TH3D* dFitter3d::Ratio(){return mRatio;}
// norm
inline double  dFitter3d::Norm(){return mNorm; }
inline void dFitter3d::SetNormFactor(double norm)  { mNorm = norm ; }
//  threshold
inline double  dFitter3d::ThresholdNumerator(){return mThresholdNumerator; }
inline double  dFitter3d::ThresholdDenominator(){return mThresholdDenominator; }
inline void dFitter3d::SetThresholdNumerator(double thresN) { mThresholdNumerator = thresN ; }
inline void dFitter3d::SetThresholdDenominator(double thresD) { mThresholdDenominator = thresD ; }

// ifndef dFitter3d_hh
#endif
