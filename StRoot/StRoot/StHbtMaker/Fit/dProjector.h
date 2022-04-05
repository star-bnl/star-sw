/***************************************************************************
 *
 * Author: Dominik Flierl, flierl@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   this is an object which produces projections of 3d histograms
 *  
 **************************************************************************/

#ifndef dProjector_hh
#define dProjector_hh

#include "dFitter3d.h"
#include "TString.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "Stiostream.h"

class dProjector 
{
    public :
	////
	// constructors
	////
	dProjector(dFitter3d* mFitter) ;
        virtual ~dProjector() ;

	////
	// do project
	////
	TH1D* get1dProjection(TString axis, double xmin, double xmax, double ymin, double ymax, double zmin, double zmax) ;
	TH2D* get2dProjection(TString axis, double xmin, double xmax, double ymin, double ymax, double zmin, double zmax) ;
	
	////
	// setters & getters
	////
	
	// set/get 3d histos
	void setNumerator(TH3D* numerator) { mNumerator = numerator ; } ;
	void setDenominator(TH3D* denominator) { mDenominator = denominator ; }   ;
	void setRatio(TH3D* ratio) { mRatio = ratio ; } ;
	TH3D* getNumerator() { return mNumerator ; } ;
	TH3D* getDenominator() { return mDenominator ; }  ;
	TH3D* getRatio() { return mRatio ; } ;

	// set/get norm
	void setNorm(double norm) { mNorm = norm ; }  ;
	double getNorm() { return mNorm ; } ;

	// set/get thresholds
	void setThresholdNumerator(double thresN) { mThresholdNumerator = thresN ; } ; 
	void setThresholdDenominator(double thresD) { mThresholdDenominator =  thresD ; } ;
	double getThresholdNumerator() { return mThresholdNumerator ; } ; 
	double getThresholdDenominator() { return mThresholdDenominator ; } ;
	
	// set limits for the ratio
	void setRatioMin(double min) { mRatioMin = min ; } ;
	void setRatioMax(double max) { mRatioMax = max ; } ;

	// return histos
	TH1D* get1dFit() { return m1dfit ; } ;
	TH1D* get1dNorm() { return m1dnor ; } ;
	TH2D* get2dFit() { return m2dfit ; } ;
	TH2D* get2dNorm() { return m2dnor ; } ;


	private :	
	
	// the histos 
	// 3d authentic
	TH3D* mNumerator ; 
	TH3D* mDenominator ;
	TH3D* mRatio ;

	// projections
	TH1D* m1dpro ;
	TH1D* m1dfit ;
	TH1D* m1dnor ;
	TH2D* m2dpro ;
	TH2D* m2dfit ;
	TH2D* m2dnor ;
	// fit parameters
	double* mFitParameters; 
	dFitter3d* mFitter;

	// thresholds
	double mNorm ;
	double mThresholdNumerator ;
	double mThresholdDenominator ; 
	double mRatioMin ;
	double mRatioMax ;
		
#ifdef __ROOT__
  ClassDef(dProjector, 0)
#endif

};

#endif
