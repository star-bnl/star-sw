
#include <assert.h>

#include "TError.h"
#include "TMath.h"

#include "StMessMgr.h"
#include "StCentrality.h"

ClassImp(StCentrality)

	using std::vector ;

	//____________________________________________________________________________________________________
	// Re-weighting correction
Double_t Reweighting(const Double_t* x, const Double_t* par)
{
	return 1.0 - TMath::Exp(-par[0]*TMath::Power(x[0], par[1]));
}

//____________________________________________________________________________________________________
// Default constructor
	StCentrality::StCentrality(const TString system, const TString type)
: mType(type), mNpp(0.0), mK(0.0), mX(0.0)
{
	for(UInt_t im=0; im<mNMode; im++){
		mMultiplicityCut[im].clear();
		mCentralityMin[im].clear();
		mCentralityMax[im].clear();
	}

	mParReweighting[0] = 0.0 ;
	mParReweighting[1] = 0.0 ;

	/// Initialize centrality bin
	if( system.CompareTo("auau_200gev", TString::kIgnoreCase) == 0 )      Init_AuAu200GeV() ;
	else if( system.CompareTo("auau_62gev", TString::kIgnoreCase) == 0 )  Init_AuAu62GeV() ;
	else if( system.CompareTo("auau_39gev", TString::kIgnoreCase) == 0 )  Init_AuAu39GeV() ;
	else if( system.CompareTo("auau_27gev", TString::kIgnoreCase) == 0 )  Init_AuAu27GeV() ;
	else if( system.CompareTo("auau_19gev", TString::kIgnoreCase) == 0 )  Init_AuAu19GeV() ;
	else if( system.CompareTo("auau_11gev", TString::kIgnoreCase) == 0 )  Init_AuAu11GeV() ;
	else if( system.CompareTo("auau_7gev", TString::kIgnoreCase) == 0 )   Init_AuAu7GeV() ;
	else if( system.CompareTo("dau_200GeV", TString::kIgnoreCase) == 0 )  Init_dAu200GeV() ;
	else if( system.CompareTo("cucu_200GeV", TString::kIgnoreCase) == 0 ) Init_CuCu200GeV() ;
	else if( system.CompareTo("zrzr_200GeV", TString::kIgnoreCase) == 0 ) Init_ZrZr200GeV() ;
	else if( system.CompareTo("uu_200GeV", TString::kIgnoreCase) == 0 )   Init_UU200GeV() ;
	else if( system.CompareTo("pbpb_2760GeV", TString::kIgnoreCase) == 0 ) Init_PbPb2760GeV() ;
	else{
		Error("StCentrality", "can't find system %s. See below for current available systems", system.Data());
		help();
		assert(0);
	}

	/// Make sure centrality bin size is equal (and not empty)
	if( mMultiplicityCut[0].empty() ) Warning("StCentrality", "Bins for multiplicity cut is empty. Program will be crashed ...");
	if( mCentralityMin[0].empty() )   Warning("StCentrality", "Minimum centrality is empty. Program will be crashed ...");
	if( mCentralityMax[0].empty() )   Warning("StCentrality", "Maximum centrality is empty. Program will be crashed ...");

	// Cuts
	const Bool_t isBinSizeEqual0 =
		(mMultiplicityCut[0].size() == mMultiplicityCut[1].size())
		&& (mMultiplicityCut[1].size() == mMultiplicityCut[2].size())
		&& (mMultiplicityCut[2].size() == mMultiplicityCut[0].size())
		;
	if(!isBinSizeEqual0){
		Error("StCentrality", 
				Form("Bin size for arrays are not equal: (default, small, large) = (%3d, %3d, %3d)",
					mMultiplicityCut[0].size(), mMultiplicityCut[1].size(), mMultiplicityCut[2].size())
			 );
		assert(isBinSizeEqual0);
	}

	// Among cut, min, max
	const Bool_t isBinSizeEqual1 =
		(mMultiplicityCut[0].size() == mCentralityMin[0].size())
		&& (mCentralityMin[0].size() == mCentralityMax[0].size())
		&& (mCentralityMax[0].size() == mMultiplicityCut[0].size())
		;
	if(!isBinSizeEqual1){
		Error("StCentrality", 
				Form("Bin size for arrays are not equal: (mult, centmin, centmax) = (%3d, %3d, %3d)",
					mMultiplicityCut[0].size(), mCentralityMin[0].size(), mCentralityMax[0].size())
			 );
		assert(isBinSizeEqual1);
	}

	/// Print info. (check all modes)
	for(UInt_t im=0; im<mNMode; im++){
		for(UInt_t ic=0; ic<mMultiplicityCut[im].size(); ic++){
			LOG_INFO << Form("StCentrality  multiplicity cut M > %4d for centrality %3d - %3d (%%)", 
					mMultiplicityCut[im][ic], (Int_t)mCentralityMin[im][ic], (Int_t)mCentralityMax[im][ic])
				<< endm;
		}// centrality
	}// mode

}

//____________________________________________________________________________________________________
// Default destructor
StCentrality::~StCentrality()
{
	for(UInt_t im=0; im<mNMode; im++){
		mMultiplicityCut[im].clear();
		mCentralityMin[im].clear();
		mCentralityMax[im].clear();
	}
}

//____________________________________________________________________________________________________
void StCentrality::help()
{
	LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm;
	LOG_INFO << "StCentrality::help  Current available systems for Init(const Char_t* system) are:" << endm;
	LOG_INFO << "system         descriptions" << endm;
	LOG_INFO << "AuAu_200GeV    Au + Au at 200 GeV" << endm;
	LOG_INFO << "AuAu_39GeV     Au + Au at 39 GeV" << endm;
	LOG_INFO << "AuAu_11GeV     Au + Au at 11.5 GeV" << endm;
	LOG_INFO << "AuAu_19GeV     Au + Au at 19.6 GeV" << endm;
	LOG_INFO << "AuAu_7GeV      Au + Au at 7.7 GeV" << endm;
	LOG_INFO << "dAu_200GeV      d + Au at 200 GeV" << endm;
	LOG_INFO << endm;
	LOG_INFO << "  NOTE: input system name is case insensitive" << endm;
	LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm;
}

//____________________________________________________________________________________________________
Double_t StCentrality::GetCentrality(const UInt_t multiplicity, const UInt_t mode) const
{
	/// Get centrality bin from multiplicity
	///  mode  
	///   0        default centrality bins
	///   1        5% lower centrality bins (4.75, 9.5, 19.5, ...)
	///   2        5% higher centrality bins

	// Check mode
	if( mode >= mNMode ){
		Error("StCentrality::GetCentrality", "Invalid mode, mode=%3d. reutnr -1", mode);
		return -1.0 ;
	}

	// Make sure array has some entries
	if( mMultiplicityCut[mode].empty() ){
		Error("StCentrality::GetCentrality", "multiplicity cuts for centrality have not implemented. return -1");
		return -1.0 ;
	}

	// Find centrality bin
	for(UInt_t icent=0; icent<mMultiplicityCut[mode].size(); icent++){
		if( multiplicity > mMultiplicityCut[mode][icent] ) 
			return (mCentralityMin[mode][icent]+mCentralityMax[mode][icent])/2.0;
	}

	return -1.0 ;
}

//____________________________________________________________________________________________________
Double_t StCentrality::GetNppError(const Double_t npp) const
{
	if ( mType.CompareTo("default", TString::kIgnoreCase) == 0 )       return 0.0 ;
	else if ( mType.CompareTo("low", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StCentrality::GetNppError  type=low  add -5% error on npp" << endm;
		return -0.05 * npp ;
	}
	else if ( mType.CompareTo("high", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StCentrality::GetNppError  type=high  add +5% error on npp" << endm;
		return 0.05 * npp ;
	}
	else {
		Warning("StCentrality::GetNppError", "Unknown type, type=%s. Set error = 0", mType.Data());
		return 0.0 ;
	}
}

//____________________________________________________________________________________________________
void StCentrality::Init_AuAu200GeV()
{
	// Run4 stuffs
#if 0
	// Centrality definition in 200 GeV Run4
	// Define multiplicity cut from central to peripheral
	mMultiplicityCut[0].push_back( 514 - 1 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	mMultiplicityCut[0].push_back( 432 - 1 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back( 10.0 );
	mMultiplicityCut[0].push_back( 302 - 1 ); mCentralityMin[0].push_back( 10.0 );  mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back( 208 - 1 ); mCentralityMin[0].push_back( 20.0 );  mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back( 138 - 1 ); mCentralityMin[0].push_back( 30.0 );  mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  88 - 1 ); mCentralityMin[0].push_back( 40.0 );  mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  52 - 1 ); mCentralityMin[0].push_back( 50.0 );  mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  28 - 1 ); mCentralityMin[0].push_back( 60.0 );  mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(  14 - 1 ); mCentralityMin[0].push_back( 70.0 );  mCentralityMax[0].push_back( 80.0 );
#endif

	/// Initialize Au + Au collisiona at 200 GeV (Run10)
	//----------------------------------------------------------------------------------------------------
	//  Update on Feb/04/2011
	//    npp and x are determined from published spectra paper, PRC79, 034909 (2009)
	//    npp = 2.43
	//    k = 2.0 (probably k dependence is weak, need to study)
	//    x = 0.13 from interpolation between 19.6 and 200 GeV data (PHOBOS, PRC70, 021902, 2004)
	//
	//    - Multiplicity dependent efficiency with d=0.14
	//    - Additional constant efficiency loss 17% (0.17)
	//      (need to be implemented somehow, this is currently done by temporary fix in StNegativeBinomial.cxx)
	//
	//      NOTE: parameters are not final yet (Feb/04/2011)
	//----------------------------------------------------------------------------------------------------
	//  0.00 -  5.00 (%) :  M >  441
	//  5.00 - 10.00 (%) :  M >  375
	// 10.00 - 15.00 (%) :  M >  317
	// 15.00 - 20.00 (%) :  M >  266
	// 20.00 - 25.00 (%) :  M >  221
	// 25.00 - 30.00 (%) :  M >  182
	// 30.00 - 35.00 (%) :  M >  148
	// 35.00 - 40.00 (%) :  M >  118
	// 40.00 - 45.00 (%) :  M >   93
	// 45.00 - 50.00 (%) :  M >   72
	// 50.00 - 55.00 (%) :  M >   55
	// 55.00 - 60.00 (%) :  M >   41
	// 60.00 - 65.00 (%) :  M >   30
	// 65.00 - 70.00 (%) :  M >   21
	// 70.00 - 75.00 (%) :  M >   15
	// 75.00 - 80.00 (%) :  M >   10
	// 80.00 - 85.00 (%) :  M >    6
	// 85.00 - 90.00 (%) :  M >    4
	//

	// Error on x (energy specific ???)
	// NOTE: type 'low' and 'high' are for npp, x is anti-correlated with npp
	Double_t xError = 0.0 ; // absolute error
	if( mType.CompareTo("low", TString::kIgnoreCase) == 0 )       xError = 0.02 ; // npp is low, x is high
	else if( mType.CompareTo("high", TString::kIgnoreCase) == 0 ) xError = -0.02 ; // npp is high, x is low

	const Double_t npp = 2.07 ; // default npp
	mNpp = npp + GetNppError(npp) ; // 5% error on npp
	mK   = 2.00 ;
	mX   = 0.14 + xError ;
	mEfficiency  = 0.15 ;
	mTriggerBias = 1.00 ;
	//  mTriggerBias = 0.835 ; // Run10 200 GeV

	// Final centrality bins
	// Define multiplicity cut from central to peripheral
	mMultiplicityCut[0].push_back( 469 ); mCentralityMin[0].push_back( 0.0 ); mCentralityMax[0].push_back( 5.0 );
	mMultiplicityCut[0].push_back( 398 ); mCentralityMin[0].push_back( 5.0 ); mCentralityMax[0].push_back( 10.0 );
	mMultiplicityCut[0].push_back( 336 ); mCentralityMin[0].push_back( 10.0 ); mCentralityMax[0].push_back( 15.0 );
	mMultiplicityCut[0].push_back( 281 ); mCentralityMin[0].push_back( 15.0 ); mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back( 234 ); mCentralityMin[0].push_back( 20.0 ); mCentralityMax[0].push_back( 25.0 );
	mMultiplicityCut[0].push_back( 192 ); mCentralityMin[0].push_back( 25.0 ); mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back( 155 ); mCentralityMin[0].push_back( 30.0 ); mCentralityMax[0].push_back( 35.0 );
	mMultiplicityCut[0].push_back( 124 ); mCentralityMin[0].push_back( 35.0 ); mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  98 ); mCentralityMin[0].push_back( 40.0 ); mCentralityMax[0].push_back( 45.0 );
	mMultiplicityCut[0].push_back(  75 ); mCentralityMin[0].push_back( 45.0 ); mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  57 ); mCentralityMin[0].push_back( 50.0 ); mCentralityMax[0].push_back( 55.0 );
	mMultiplicityCut[0].push_back(  42 ); mCentralityMin[0].push_back( 55.0 ); mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  31 ); mCentralityMin[0].push_back( 60.0 ); mCentralityMax[0].push_back( 65.0 );
	mMultiplicityCut[0].push_back(  22 ); mCentralityMin[0].push_back( 65.0 ); mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(  15 ); mCentralityMin[0].push_back( 70.0 ); mCentralityMax[0].push_back( 75.0 );
	mMultiplicityCut[0].push_back(  10 ); mCentralityMin[0].push_back( 75.0 ); mCentralityMax[0].push_back( 80.0 );
	// -5%                                 +5%
	mMultiplicityCut[1].push_back( 473 );  mMultiplicityCut[2].push_back( 465 );
	mMultiplicityCut[1].push_back( 405 );  mMultiplicityCut[2].push_back( 391 );
	mMultiplicityCut[1].push_back( 345 );  mMultiplicityCut[2].push_back( 327 );
	mMultiplicityCut[1].push_back( 292 );  mMultiplicityCut[2].push_back( 271 );
	mMultiplicityCut[1].push_back( 245 );  mMultiplicityCut[2].push_back( 223 );
	mMultiplicityCut[1].push_back( 204 );  mMultiplicityCut[2].push_back( 181 );
	mMultiplicityCut[1].push_back( 168 );  mMultiplicityCut[2].push_back( 144 );
	mMultiplicityCut[1].push_back( 136 );  mMultiplicityCut[2].push_back( 113 );
	mMultiplicityCut[1].push_back( 109 );  mMultiplicityCut[2].push_back(  87 );
	mMultiplicityCut[1].push_back(  86 );  mMultiplicityCut[2].push_back(  66 );
	mMultiplicityCut[1].push_back(  67 );  mMultiplicityCut[2].push_back(  49 );
	mMultiplicityCut[1].push_back(  51 );  mMultiplicityCut[2].push_back(  35 );
	mMultiplicityCut[1].push_back(  38 );  mMultiplicityCut[2].push_back(  25 );
	mMultiplicityCut[1].push_back(  28 );  mMultiplicityCut[2].push_back(  17 );
	mMultiplicityCut[1].push_back(  20 );  mMultiplicityCut[2].push_back(  11 );
	mMultiplicityCut[1].push_back(  14 );  mMultiplicityCut[2].push_back(   7 );

	// Set same centrality bins 
	for(UInt_t ic=0; ic<mCentralityMin[0].size(); ic++){
		for(UInt_t im=1; im<mNMode; im++){
			mCentralityMin[im].push_back( mCentralityMin[0][ic] );
			mCentralityMax[im].push_back( mCentralityMax[0][ic] );
		}
	}
}

//____________________________________________________________________________________________________
void StCentrality::Init_AuAu62GeV()
{
	/// Initialize Au + Au collisiona at 62.4 GeV (Run10)
	//----------------------------------------------------------------------------------------------------
	// Update on Jul/21/2011
	//   - Just copy from 200 GeV, should be updated later on
	//
	//----------------------------------------------------------------------------------------------------
	// Update on Nov/02/2011 (by Alex's study)
	//   - npp = 1.6
	//   - k = 2
	//   - x = 0.12
	//   - multiplicity dependent efficiency (d=0.14)
	//----------------------------------------------------------------------------------------------------

	// Error on x
	// NOTE: type 'low' and 'high' are for npp, x is anti-correlated with npp
	Double_t xError = 0.0 ; // absolute error
	if( mType.CompareTo("low", TString::kIgnoreCase) == 0 )       xError = 0.02 ; // npp is low, x is high
	else if( mType.CompareTo("high", TString::kIgnoreCase) == 0 ) xError = -0.02 ; // npp is high, x is low

	const Double_t npp = 1.6 ; // default npp
	mNpp = npp + GetNppError(npp) ; // 5% error on npp
	mK   = 2.00 ;
	mX   = 0.12 + xError ;
	mEfficiency  = 0.14 ;
	mTriggerBias = 1.00 ;

	// Final centrality bins
	// Define multiplicity cut from central to peripheral
	mMultiplicityCut[0].push_back( 339 ); mCentralityMin[0].push_back( 0.0 ); mCentralityMax[0].push_back( 5.0 );
	mMultiplicityCut[0].push_back( 285 ); mCentralityMin[0].push_back( 5.0 ); mCentralityMax[0].push_back( 10.0 );
	mMultiplicityCut[0].push_back( 239 ); mCentralityMin[0].push_back( 10.0 ); mCentralityMax[0].push_back( 15.0 );
	mMultiplicityCut[0].push_back( 199 ); mCentralityMin[0].push_back( 15.0 ); mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back( 165 ); mCentralityMin[0].push_back( 20.0 ); mCentralityMax[0].push_back( 25.0 );
	mMultiplicityCut[0].push_back( 135 ); mCentralityMin[0].push_back( 25.0 ); mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back( 109 ); mCentralityMin[0].push_back( 30.0 ); mCentralityMax[0].push_back( 35.0 );
	mMultiplicityCut[0].push_back(  88 ); mCentralityMin[0].push_back( 35.0 ); mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  69 ); mCentralityMin[0].push_back( 40.0 ); mCentralityMax[0].push_back( 45.0 );
	mMultiplicityCut[0].push_back(  54 ); mCentralityMin[0].push_back( 45.0 ); mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  41 ); mCentralityMin[0].push_back( 50.0 ); mCentralityMax[0].push_back( 55.0 );
	mMultiplicityCut[0].push_back(  30 ); mCentralityMin[0].push_back( 55.0 ); mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  22 ); mCentralityMin[0].push_back( 60.0 ); mCentralityMax[0].push_back( 65.0 );
	mMultiplicityCut[0].push_back(  16 ); mCentralityMin[0].push_back( 65.0 ); mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(  11 ); mCentralityMin[0].push_back( 70.0 ); mCentralityMax[0].push_back( 75.0 );
	mMultiplicityCut[0].push_back(   7 ); mCentralityMin[0].push_back( 75.0 ); mCentralityMax[0].push_back( 80.0 );

	// -5%                                 // +5%
	mMultiplicityCut[1].push_back( 342 );  mMultiplicityCut[2].push_back( 336 );
	mMultiplicityCut[1].push_back( 290 );  mMultiplicityCut[2].push_back( 280 );
	mMultiplicityCut[1].push_back( 245 );  mMultiplicityCut[2].push_back( 232 );
	mMultiplicityCut[1].push_back( 206 );  mMultiplicityCut[2].push_back( 192 );
	mMultiplicityCut[1].push_back( 173 );  mMultiplicityCut[2].push_back( 157 );
	mMultiplicityCut[1].push_back( 143 );  mMultiplicityCut[2].push_back( 127 );
	mMultiplicityCut[1].push_back( 118 );  mMultiplicityCut[2].push_back( 101 );
	mMultiplicityCut[1].push_back(  96 );  mMultiplicityCut[2].push_back(  80 );
	mMultiplicityCut[1].push_back(  77 );  mMultiplicityCut[2].push_back(  62 );
	mMultiplicityCut[1].push_back(  61 );  mMultiplicityCut[2].push_back(  47 );
	mMultiplicityCut[1].push_back(  47 );  mMultiplicityCut[2].push_back(  35 );
	mMultiplicityCut[1].push_back(  36 );  mMultiplicityCut[2].push_back(  25 );
	mMultiplicityCut[1].push_back(  27 );  mMultiplicityCut[2].push_back(  18 );
	mMultiplicityCut[1].push_back(  20 );  mMultiplicityCut[2].push_back(  12 );
	mMultiplicityCut[1].push_back(  14 );  mMultiplicityCut[2].push_back(   8 );
	mMultiplicityCut[1].push_back(  10 );  mMultiplicityCut[2].push_back(   5 );


	// Set same centrality bins 
	for(UInt_t ic=0; ic<mCentralityMin[0].size(); ic++){
		for(UInt_t im=1; im<mNMode; im++){
			mCentralityMin[im].push_back( mCentralityMin[0][ic] );
			mCentralityMax[im].push_back( mCentralityMax[0][ic] );
		}
	}
}


//____________________________________________________________________________________________________
void StCentrality::Init_AuAu39GeV()
{
	/// Initialize Au + Au collisiona at 39 GeV
	//----------------------------------------------------------------------------------------------------
	//    npp = 1.56 from 0.528*log(sqrt(sNN)) - 0.3696 
	//    (Paul's study in http://www.star.bnl.gov/protected/ebye/sorensen/bes/sorensen_glauber_9.2.pdf)
	//    k = 2.0 (probably k dependence is weak, need to study)
	//    x = 0.12 from interpolation between 19.6 and 200 GeV data (PHOBOS, PRC70, 021902, 2004)
	//      NOTE: parameters are not final yet (Apr/15/2010)
	//----------------------------------------------------------------------------------------------------
	//    Update on Apr/26/2010
	//    npp = 1.6 (best fit)
	//    k   = 2.0 (no big change from 2 to 5)
	//    x   = 0.12
	//    eff = 80%
	//      * All above parameters were determined by using low multiplicity cut off M > 30
	//
	//    Relative error on npp +/- 5% --> +/- 17% error on x (i.e. 0.12 +/- 0.02)
	//
	//----------------------------------------------------------------------------------------------------
	//    Update on Jul/29/2010
	//    npp = 1.52
	//    k   = 1.5
	//    x   = 0.12 (see above)
	//    eff : Multiplicity dependent efficiency with d = 0.14
	//      * All above parameters were determined by using low multiplicity cut off M > 30
	//
	//    Re-weighting correction parameters: p0 = 0.92 +/- 0.03,  p1 = 0.43 +/- 0.01
	//    These parameter are ANTI-correlated

	// Error on x (energy specific ???)
	// NOTE: type 'low' and 'high' are for npp, x is anti-correlated with npp
	Double_t xError = 0.0 ; // absolute error
	if( mType.CompareTo("low", TString::kIgnoreCase) == 0 )       xError = 0.02 ; // npp is low, x is high
	else if( mType.CompareTo("high", TString::kIgnoreCase) == 0 ) xError = -0.02 ; // npp is high, x is low

	const Double_t npp = 1.52 ; // default npp
	mNpp = npp + GetNppError(npp) ; // 5% error on npp
	mK   = 1.50 ;
	mX   = 0.12 + xError ;
	mEfficiency  = 0.14 ;
	mTriggerBias = 1.00 ;

	// Parameters for re-weighting correction
	mParReweighting[0] = 0.92 ;
	mParReweighting[1] = 0.43 ;

	// Assign +/- 2 sigma for re-weighting correction
	if( mType.CompareTo("lowrw", TString::kIgnoreCase) == 0 ){
		mParReweighting[0] = 0.98 ; // + 2sigma
		mParReweighting[1] = 0.41 ; // - 2sigma
	}
	else if ( mType.CompareTo("highrw", TString::kIgnoreCase) == 0 ){
		mParReweighting[0] = 0.86 ; // - 2sigma
		mParReweighting[1] = 0.45 ; // + 2sigma
	}

	LOG_INFO << "StCentrality::Init_AuAu39GeV  Use " << mType << " type: re-weighting correction will be "
		<< Form("1-exp(-%1.2f*refmult^{%1.2f})", mParReweighting[0], mParReweighting[1])
		<< endm;

#if 0
	// Define multiplicity cut from central to peripheral
	mMultiplicityCut[0].push_back( 316 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	mMultiplicityCut[0].push_back( 265 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back( 10.0 );
	mMultiplicityCut[0].push_back( 185 ); mCentralityMin[0].push_back( 10.0 );  mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back( 125 ); mCentralityMin[0].push_back( 20.0 );  mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back(  81 ); mCentralityMin[0].push_back( 30.0 );  mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  50 ); mCentralityMin[0].push_back( 40.0 );  mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  28 ); mCentralityMin[0].push_back( 50.0 );  mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  15 ); mCentralityMin[0].push_back( 60.0 );  mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(   7 ); mCentralityMin[0].push_back( 70.0 );  mCentralityMax[0].push_back( 80.0 );

	// -5%                                +5%
	mMultiplicityCut[1].push_back( 319 ); mMultiplicityCut[2].push_back( 313 ); //  0-5%
	mMultiplicityCut[1].push_back( 270 ); mMultiplicityCut[2].push_back( 260 ); //  5-10%
	mMultiplicityCut[1].push_back( 192 ); mMultiplicityCut[2].push_back( 178 ); // 10-20%
	mMultiplicityCut[1].push_back( 133 ); mMultiplicityCut[2].push_back( 118 ); // 20-30%
	mMultiplicityCut[1].push_back(  89 ); mMultiplicityCut[2].push_back(  74 ); // 30-40%
	mMultiplicityCut[1].push_back(  57 ); mMultiplicityCut[2].push_back(  44 ); // 40-50%
	mMultiplicityCut[1].push_back(  34 ); mMultiplicityCut[2].push_back(  24 ); // 50-60%
	mMultiplicityCut[1].push_back(  19 ); mMultiplicityCut[2].push_back(  12 ); // 60-70%
	mMultiplicityCut[1].push_back(  10 ); mMultiplicityCut[2].push_back(   5 ); // 70-80%
#endif

	// Final centrality bins
	// Define multiplicity cut from central to peripheral
	//  mMultiplicityCut[0].push_back( 316 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	//  mMultiplicityCut[0].push_back( 265 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back( 10.0 );
	// Temporary to include 7.5%
	mMultiplicityCut[0].push_back( 316 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	mMultiplicityCut[0].push_back( 290 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back(  7.5 );
	mMultiplicityCut[0].push_back( 265 ); mCentralityMin[0].push_back(  7.5 );  mCentralityMax[0].push_back( 10.0 );

	mMultiplicityCut[0].push_back( 222 ); mCentralityMin[0].push_back( 10.0 );  mCentralityMax[0].push_back( 15.0 );
	mMultiplicityCut[0].push_back( 185 ); mCentralityMin[0].push_back( 15.0 );  mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back( 153 ); mCentralityMin[0].push_back( 20.0 );  mCentralityMax[0].push_back( 25.0 );
	mMultiplicityCut[0].push_back( 125 ); mCentralityMin[0].push_back( 25.0 );  mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back( 102 ); mCentralityMin[0].push_back( 30.0 );  mCentralityMax[0].push_back( 35.0 );
	mMultiplicityCut[0].push_back(  81 ); mCentralityMin[0].push_back( 35.0 );  mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  64 ); mCentralityMin[0].push_back( 40.0 );  mCentralityMax[0].push_back( 45.0 );
	mMultiplicityCut[0].push_back(  50 ); mCentralityMin[0].push_back( 45.0 );  mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  38 ); mCentralityMin[0].push_back( 50.0 );  mCentralityMax[0].push_back( 55.0 );
	mMultiplicityCut[0].push_back(  28 ); mCentralityMin[0].push_back( 55.0 );  mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  21 ); mCentralityMin[0].push_back( 60.0 );  mCentralityMax[0].push_back( 65.0 );
	mMultiplicityCut[0].push_back(  15 ); mCentralityMin[0].push_back( 65.0 );  mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(  10 ); mCentralityMin[0].push_back( 70.0 );  mCentralityMax[0].push_back( 75.0 );
	mMultiplicityCut[0].push_back(   7 ); mCentralityMin[0].push_back( 75.0 );  mCentralityMax[0].push_back( 80.0 );

	// -5%                                 +5%
	mMultiplicityCut[1].push_back( 319 );  mMultiplicityCut[2].push_back( 313 );
	mMultiplicityCut[1].push_back( 293 );  mMultiplicityCut[2].push_back( 286 ); // temp
	mMultiplicityCut[1].push_back( 270 );  mMultiplicityCut[2].push_back( 260 );
	mMultiplicityCut[1].push_back( 228 );  mMultiplicityCut[2].push_back( 216 );
	mMultiplicityCut[1].push_back( 192 );  mMultiplicityCut[2].push_back( 178 );
	mMultiplicityCut[1].push_back( 160 );  mMultiplicityCut[2].push_back( 145 );
	mMultiplicityCut[1].push_back( 133 );  mMultiplicityCut[2].push_back( 118 );
	mMultiplicityCut[1].push_back( 110 );  mMultiplicityCut[2].push_back(  94 );
	mMultiplicityCut[1].push_back(  89 );  mMultiplicityCut[2].push_back(  74 );
	mMultiplicityCut[1].push_back(  72 );  mMultiplicityCut[2].push_back(  57 );
	mMultiplicityCut[1].push_back(  57 );  mMultiplicityCut[2].push_back(  44 );
	mMultiplicityCut[1].push_back(  44 );  mMultiplicityCut[2].push_back(  33 );
	mMultiplicityCut[1].push_back(  34 );  mMultiplicityCut[2].push_back(  24 );
	mMultiplicityCut[1].push_back(  26 );  mMultiplicityCut[2].push_back(  17 );
	mMultiplicityCut[1].push_back(  19 );  mMultiplicityCut[2].push_back(  12 );
	mMultiplicityCut[1].push_back(  14 );  mMultiplicityCut[2].push_back(   8 );
	mMultiplicityCut[1].push_back(  10 );  mMultiplicityCut[2].push_back(   5 );

	// Set same centrality bins 
	for(UInt_t ic=0; ic<mCentralityMin[0].size(); ic++){
		for(UInt_t im=1; im<mNMode; im++){
			mCentralityMin[im].push_back( mCentralityMin[0][ic] );
			mCentralityMax[im].push_back( mCentralityMax[0][ic] );
		}
	}

}

//____________________________________________________________________________________________________
void StCentrality::Init_AuAu27GeV()
{
	//Initialized Au+Au collisions at 27 GeV
	//Updated on 07/28/2019
	//    npp = 1.27 from fitting, Refmult>100
	//    k   = 1.80 
	//    x   = 0.16
	//    eff = 0.11
	//    with d = 0.14 (factor for multiplicity dependent efficiency)
	//----------------------------------------------------------------------------------------------------

	// Error on x (energy specific ???)
	// NOTE: type 'low' and 'high' are for npp, x is anti-correlated with npp
	Double_t xError = 0.0 ; // absolute error
	if( mType.CompareTo("low", TString::kIgnoreCase) == 0 )       xError = 0.02 ;  // npp is low, x is high
	else if( mType.CompareTo("high", TString::kIgnoreCase) == 0 ) xError = -0.02 ; // npp is high, x is low

	const Double_t npp = 1.27 ;
	mNpp = npp + GetNppError(npp) ;
	mK   = 1.80 ;
	mX   = 0.16 + xError ;
	mEfficiency  = 0.11 ;
	mTriggerBias = 1.00 ;

	mMultiplicityCut[0].push_back( 295 ); mCentralityMin[0].push_back( 0.0 ); mCentralityMax[0].push_back( 5.0 );
	mMultiplicityCut[0].push_back( 245 ); mCentralityMin[0].push_back( 5.0 ); mCentralityMax[0].push_back( 10.0 );
	mMultiplicityCut[0].push_back( 204 ); mCentralityMin[0].push_back( 10.0 ); mCentralityMax[0].push_back( 15.0 );
	mMultiplicityCut[0].push_back( 169 ); mCentralityMin[0].push_back( 15.0 ); mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back( 138 ); mCentralityMin[0].push_back( 20.0 ); mCentralityMax[0].push_back( 25.0 );
	mMultiplicityCut[0].push_back( 113 ); mCentralityMin[0].push_back( 25.0 ); mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back(  91 ); mCentralityMin[0].push_back( 30.0 ); mCentralityMax[0].push_back( 35.0 );
	mMultiplicityCut[0].push_back(  72 ); mCentralityMin[0].push_back( 35.0 ); mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  57 ); mCentralityMin[0].push_back( 40.0 ); mCentralityMax[0].push_back( 45.0 );
	mMultiplicityCut[0].push_back(  44 ); mCentralityMin[0].push_back( 45.0 ); mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  33 ); mCentralityMin[0].push_back( 50.0 ); mCentralityMax[0].push_back( 55.0 );
	mMultiplicityCut[0].push_back(  25 ); mCentralityMin[0].push_back( 55.0 ); mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  18 ); mCentralityMin[0].push_back( 60.0 ); mCentralityMax[0].push_back( 65.0 );
	mMultiplicityCut[0].push_back(  13 ); mCentralityMin[0].push_back( 65.0 ); mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(   9 ); mCentralityMin[0].push_back( 70.0 ); mCentralityMax[0].push_back( 75.0 );
	mMultiplicityCut[0].push_back(   6 ); mCentralityMin[0].push_back( 75.0 ); mCentralityMax[0].push_back( 80.0 );
	mMultiplicityCut[1].push_back( 298 );  mMultiplicityCut[2].push_back( 293 );
	mMultiplicityCut[1].push_back( 250 );  mMultiplicityCut[2].push_back( 241 );
	mMultiplicityCut[1].push_back( 209 );  mMultiplicityCut[2].push_back( 198 );
	mMultiplicityCut[1].push_back( 175 );  mMultiplicityCut[2].push_back( 162 );
	mMultiplicityCut[1].push_back( 146 );  mMultiplicityCut[2].push_back( 132 );
	mMultiplicityCut[1].push_back( 120 );  mMultiplicityCut[2].push_back( 106 );
	mMultiplicityCut[1].push_back(  98 );  mMultiplicityCut[2].push_back(  84 );
	mMultiplicityCut[1].push_back(  79 );  mMultiplicityCut[2].push_back(  66 );
	mMultiplicityCut[1].push_back(  64 );  mMultiplicityCut[2].push_back(  51 );
	mMultiplicityCut[1].push_back(  50 );  mMultiplicityCut[2].push_back(  38 );
	mMultiplicityCut[1].push_back(  39 );  mMultiplicityCut[2].push_back(  28 );
	mMultiplicityCut[1].push_back(  30 );  mMultiplicityCut[2].push_back(  20 );
	mMultiplicityCut[1].push_back(  22 );  mMultiplicityCut[2].push_back(  14 );
	mMultiplicityCut[1].push_back(  16 );  mMultiplicityCut[2].push_back(  10 );
	mMultiplicityCut[1].push_back(  12 );  mMultiplicityCut[2].push_back(   6 );
	mMultiplicityCut[1].push_back(   8 );  mMultiplicityCut[2].push_back(   4 );

	//  /// Initialize Au + Au collisiona at 27 GeV
	//  //----------------------------------------------------------------------------------------------------
	//  //  Update on Dec/15/2011
	//  //    npp = 1.385 from fitting, M>50
	//  //    k = 1.65 
	//  //    x = 0.12
	//  //    with d = 0.14 (factor for multiplicity dependent efficiency)
	//  //----------------------------------------------------------------------------------------------------

	//  // Error on x (energy specific ???)
	//  // NOTE: type 'low' and 'high' are for npp, x is anti-correlated with npp
	//  Double_t xError = 0.0 ; // absolute error
	//  if( mType.CompareTo("low", TString::kIgnoreCase) == 0 )       xError = 0.02 ;  // npp is low, x is high
	//  else if( mType.CompareTo("high", TString::kIgnoreCase) == 0 ) xError = -0.02 ; // npp is high, x is low

	//  const Double_t npp = 1.385 ; // default npp
	//  //  const Double_t npp = 1.390 ; // default npp
	//  mNpp = npp + GetNppError(npp) ;
	//  mK   = 1.65 ;
	//  mX   = 0.12 + xError ;
	//  mEfficiency  = 0.14 ;
	//  mTriggerBias = 1.00 ;

	//  // Centrality bins (Final)
	//  mMultiplicityCut[0].push_back( 288 ); mCentralityMin[0].push_back( 0.0 ); mCentralityMax[0].push_back( 5.0 );
	//  mMultiplicityCut[0].push_back( 241 ); mCentralityMin[0].push_back( 5.0 ); mCentralityMax[0].push_back( 10.0 );
	//  mMultiplicityCut[0].push_back( 201 ); mCentralityMin[0].push_back( 10.0 ); mCentralityMax[0].push_back( 15.0 );
	//  mMultiplicityCut[0].push_back( 168 ); mCentralityMin[0].push_back( 15.0 ); mCentralityMax[0].push_back( 20.0 );
	//  mMultiplicityCut[0].push_back( 139 ); mCentralityMin[0].push_back( 20.0 ); mCentralityMax[0].push_back( 25.0 );
	//  mMultiplicityCut[0].push_back( 114 ); mCentralityMin[0].push_back( 25.0 ); mCentralityMax[0].push_back( 30.0 );
	//  mMultiplicityCut[0].push_back(  92 ); mCentralityMin[0].push_back( 30.0 ); mCentralityMax[0].push_back( 35.0 );
	//  mMultiplicityCut[0].push_back(  74 ); mCentralityMin[0].push_back( 35.0 ); mCentralityMax[0].push_back( 40.0 );
	//  mMultiplicityCut[0].push_back(  58 ); mCentralityMin[0].push_back( 40.0 ); mCentralityMax[0].push_back( 45.0 );
	//  mMultiplicityCut[0].push_back(  45 ); mCentralityMin[0].push_back( 45.0 ); mCentralityMax[0].push_back( 50.0 );
	//  mMultiplicityCut[0].push_back(  35 ); mCentralityMin[0].push_back( 50.0 ); mCentralityMax[0].push_back( 55.0 );
	//  mMultiplicityCut[0].push_back(  26 ); mCentralityMin[0].push_back( 55.0 ); mCentralityMax[0].push_back( 60.0 );
	//  mMultiplicityCut[0].push_back(  19 ); mCentralityMin[0].push_back( 60.0 ); mCentralityMax[0].push_back( 65.0 );
	//  mMultiplicityCut[0].push_back(  13 ); mCentralityMin[0].push_back( 65.0 ); mCentralityMax[0].push_back( 70.0 );
	//  mMultiplicityCut[0].push_back(   9 ); mCentralityMin[0].push_back( 70.0 ); mCentralityMax[0].push_back( 75.0 );
	//  mMultiplicityCut[0].push_back(   6 ); mCentralityMin[0].push_back( 75.0 ); mCentralityMax[0].push_back( 80.0 );
	//  mMultiplicityCut[1].push_back( 290 );  mMultiplicityCut[2].push_back( 285 );
	//  mMultiplicityCut[1].push_back( 245 );  mMultiplicityCut[2].push_back( 236 );
	//  mMultiplicityCut[1].push_back( 206 );  mMultiplicityCut[2].push_back( 196 );
	//  mMultiplicityCut[1].push_back( 174 );  mMultiplicityCut[2].push_back( 161 );
	//  mMultiplicityCut[1].push_back( 145 );  mMultiplicityCut[2].push_back( 132 );
	//  mMultiplicityCut[1].push_back( 121 );  mMultiplicityCut[2].push_back( 107 );
	//  mMultiplicityCut[1].push_back(  99 );  mMultiplicityCut[2].push_back(  85 );
	//  mMultiplicityCut[1].push_back(  81 );  mMultiplicityCut[2].push_back(  67 );
	//  mMultiplicityCut[1].push_back(  65 );  mMultiplicityCut[2].push_back(  52 );
	//  mMultiplicityCut[1].push_back(  51 );  mMultiplicityCut[2].push_back(  40 );
	//  mMultiplicityCut[1].push_back(  40 );  mMultiplicityCut[2].push_back(  29 );
	//  mMultiplicityCut[1].push_back(  31 );  mMultiplicityCut[2].push_back(  21 );
	//  mMultiplicityCut[1].push_back(  23 );  mMultiplicityCut[2].push_back(  15 );
	//  mMultiplicityCut[1].push_back(  17 );  mMultiplicityCut[2].push_back(  10 );
	//  mMultiplicityCut[1].push_back(  12 );  mMultiplicityCut[2].push_back(   7 );
	//  mMultiplicityCut[1].push_back(   9 );  mMultiplicityCut[2].push_back(   4 );


	// Set same centrality bins 
	for(UInt_t ic=0; ic<mCentralityMin[0].size(); ic++)
	{
		for(UInt_t im=1; im<mNMode; im++)
		{
			mCentralityMin[im].push_back( mCentralityMin[0][ic] );
			mCentralityMax[im].push_back( mCentralityMax[0][ic] );
		}
	}

}

//____________________________________________________________________________________________________
void StCentrality::Init_AuAu19GeV()
{
	/// Initialize Au + Au collisiona at 19.6 GeV
	//----------------------------------------------------------------------------------------------------
	//  Update on Jun/13/2010 (just copy from 11.5 GeV, not final)
	//    npp = 1.07 from fitting, M>30
	//    k = 2.0 (not final, but probably ok)
	//    x = 0.12 (see above)
	//    with d = 0.14 (factor for multiplicity dependent efficiency)
	//----------------------------------------------------------------------------------------------------
	//  Update on Jul/13/2010
	//    npp = 1.29 from fitting, M>50
	//    k = 1.8 (2.0 should be fine for centrality)
	//    x = 0.12
	//    with d = 0.14 (factor for multiplicity dependent efficiency)
	//----------------------------------------------------------------------------------------------------

	// Error on x (energy specific ???)
	// NOTE: type 'low' and 'high' are for npp, x is anti-correlated with npp
	Double_t xError = 0.0 ; // absolute error
	if( mType.CompareTo("low", TString::kIgnoreCase) == 0 )       xError = 0.02 ;  // npp is low, x is high
	else if( mType.CompareTo("high", TString::kIgnoreCase) == 0 ) xError = -0.02 ; // npp is high, x is low

	const Double_t npp = 1.29 ; // default npp
	mNpp = npp + GetNppError(npp) ;
	mK   = 1.80 ;
	mX   = 0.12 + xError ;
	mEfficiency  = 0.14 ;
	mTriggerBias = 1.00 ;

	// Define multiplicity cut from central to peripheral
	// -- Very preliminary, version 0
	// ---> Wrong.
	// 0.00 - 5.00 (%) :  M >  268 (im=531, M=268.5, bin=   0) (sum, total, fraction>cut) = (1865909.564, 36584023.104, 0.051>0.050)
	// 5.00 - 10.00 (%) :  M >  227 (im=572, M=227.5, bin=   1) (sum, total, fraction>cut) = (3659792.503, 36584023.104, 0.100>0.100)
	// 10.00 - 15.00 (%) :  M >  191 (im=608, M=191.5, bin=   2) (sum, total, fraction>cut) = (5520660.789, 36584023.104, 0.151>0.150)
	// 15.00 - 20.00 (%) :  M >  161 (im=638, M=161.5, bin=   3) (sum, total, fraction>cut) = (7330091.938, 36584023.104, 0.200>0.200)
	// 20.00 - 25.00 (%) :  M >  134 (im=665, M=134.5, bin=   4) (sum, total, fraction>cut) = (9203867.067, 36584023.104, 0.252>0.250)
	// 25.00 - 30.00 (%) :  M >  111 (im=688, M=111.5, bin=   5) (sum, total, fraction>cut) = (11055589.347, 36584023.104, 0.302>0.300)
	// 30.00 - 35.00 (%) :  M >   92 (im=707, M=92.5, bin=   6) (sum, total, fraction>cut) = (12811790.742, 36584023.104, 0.350>0.350)
	// 35.00 - 40.00 (%) :  M >   75 (im=724, M=75.5, bin=   7) (sum, total, fraction>cut) = (14634728.713, 36584023.104, 0.400>0.400)
	// 40.00 - 45.00 (%) :  M >   60 (im=739, M=60.5, bin=   8) (sum, total, fraction>cut) = (16514693.859, 36584023.104, 0.451>0.450)
	// 45.00 - 50.00 (%) :  M >   47 (im=752, M=47.5, bin=   9) (sum, total, fraction>cut) = (18447727.788, 36584023.104, 0.504>0.500)
	// 50.00 - 55.00 (%) :  M >   37 (im=762, M=37.5, bin=  10) (sum, total, fraction>cut) = (20227672.215, 36584023.104, 0.553>0.550)
	// 55.00 - 60.00 (%) :  M >   28 (im=771, M=28.5, bin=  11) (sum, total, fraction>cut) = (22164445.031, 36584023.104, 0.606>0.600)
	// 60.00 - 65.00 (%) :  M >   21 (im=778, M=21.5, bin=  12) (sum, total, fraction>cut) = (24030566.783, 36584023.104, 0.657>0.650)
	// 65.00 - 70.00 (%) :  M >   16 (im=783, M=16.5, bin=  13) (sum, total, fraction>cut) = (25698037.289, 36584023.104, 0.702>0.700)
	// 70.00 - 75.00 (%) :  M >   11 (im=788, M=11.5, bin=  14) (sum, total, fraction>cut) = (27877720.702, 36584023.104, 0.762>0.750)
	// 75.00 - 80.00 (%) :  M >    8 (im=791, M=8.5, bin=  15) (sum, total, fraction>cut) = (29625858.979, 36584023.104, 0.810>0.800)
	// 0.00 - 4.75 (%) :  M >  271 (im=528, M=271.5, bin=   0) (sum, total, fraction>cut) = (1746865.153, 36584023.104, 0.048>0.048)
	// 4.75 - 9.50 (%) :  M >  230 (im=569, M=230.5, bin=   1) (sum, total, fraction>cut) = (3516397.766, 36584023.104, 0.096>0.095)
	// 9.50 - 14.25 (%) :  M >  196 (im=603, M=196.5, bin=   2) (sum, total, fraction>cut) = (5242402.709, 36584023.104, 0.143>0.143)
	// 14.25 - 19.00 (%) :  M >  166 (im=633, M=166.5, bin=   3) (sum, total, fraction>cut) = (7009696.381, 36584023.104, 0.192>0.190)
	// 19.00 - 23.75 (%) :  M >  141 (im=658, M=141.5, bin=   4) (sum, total, fraction>cut) = (8691141.983, 36584023.104, 0.238>0.237)
	// 23.75 - 28.50 (%) :  M >  118 (im=681, M=118.5, bin=   5) (sum, total, fraction>cut) = (10463893.991, 36584023.104, 0.286>0.285)
	// 28.50 - 33.25 (%) :  M >   98 (im=701, M=98.5, bin=   6) (sum, total, fraction>cut) = (12230763.287, 36584023.104, 0.334>0.333)
	// 33.25 - 38.00 (%) :  M >   81 (im=718, M=81.5, bin=   7) (sum, total, fraction>cut) = (13960155.911, 36584023.104, 0.382>0.380)
	// 38.00 - 42.75 (%) :  M >   66 (im=733, M=66.5, bin=   8) (sum, total, fraction>cut) = (15724764.315, 36584023.104, 0.430>0.427)
	// 42.75 - 47.50 (%) :  M >   53 (im=746, M=53.5, bin=   9) (sum, total, fraction>cut) = (17511835.309, 36584023.104, 0.479>0.475)
	// 47.50 - 52.25 (%) :  M >   43 (im=756, M=43.5, bin=  10) (sum, total, fraction>cut) = (19120910.397, 36584023.104, 0.523>0.522)
	// 52.25 - 57.00 (%) :  M >   33 (im=766, M=33.5, bin=  11) (sum, total, fraction>cut) = (21038381.483, 36584023.104, 0.575>0.570)
	// 57.00 - 61.75 (%) :  M >   26 (im=773, M=26.5, bin=  12) (sum, total, fraction>cut) = (22657326.941, 36584023.104, 0.619>0.618)
	// 61.75 - 66.50 (%) :  M >   20 (im=779, M=20.5, bin=  13) (sum, total, fraction>cut) = (24334148.323, 36584023.104, 0.665>0.665)
	// 66.50 - 71.25 (%) :  M >   15 (im=784, M=15.5, bin=  14) (sum, total, fraction>cut) = (26079513.531, 36584023.104, 0.713>0.713)
	// 71.25 - 76.00 (%) :  M >   11 (im=788, M=11.5, bin=  15) (sum, total, fraction>cut) = (27877720.702, 36584023.104, 0.762>0.760)
	// 0.00 - 5.25 (%) :  M >  266 (im=533, M=266.5, bin=   0) (sum, total, fraction>cut) = (1945845.654, 36584023.104, 0.053>0.053)
	// 5.25 - 10.50 (%) :  M >  223 (im=576, M=223.5, bin=   1) (sum, total, fraction>cut) = (3851324.498, 36584023.104, 0.105>0.105)
	// 10.50 - 15.75 (%) :  M >  186 (im=613, M=186.5, bin=   2) (sum, total, fraction>cut) = (5804143.067, 36584023.104, 0.159>0.158)
	// 15.75 - 21.00 (%) :  M >  155 (im=644, M=155.5, bin=   3) (sum, total, fraction>cut) = (7722828.743, 36584023.104, 0.211>0.210)
	// 21.00 - 26.25 (%) :  M >  128 (im=671, M=128.5, bin=   4) (sum, total, fraction>cut) = (9661467.345, 36584023.104, 0.264>0.263)
	// 26.25 - 31.50 (%) :  M >  105 (im=694, M=105.5, bin=   5) (sum, total, fraction>cut) = (11584938.211, 36584023.104, 0.317>0.315)
	// 31.50 - 36.75 (%) :  M >   85 (im=714, M=85.5, bin=   6) (sum, total, fraction>cut) = (13527498.620, 36584023.104, 0.370>0.368)
	// 36.75 - 42.00 (%) :  M >   68 (im=731, M=68.5, bin=   7) (sum, total, fraction>cut) = (15473958.886, 36584023.104, 0.423>0.420)
	// 42.00 - 47.25 (%) :  M >   54 (im=745, M=54.5, bin=   8) (sum, total, fraction>cut) = (17364335.845, 36584023.104, 0.475>0.473)
	// 47.25 - 52.50 (%) :  M >   42 (im=757, M=42.5, bin=   9) (sum, total, fraction>cut) = (19296755.163, 36584023.104, 0.527>0.525)
	// 52.50 - 57.75 (%) :  M >   32 (im=767, M=32.5, bin=  10) (sum, total, fraction>cut) = (21253100.432, 36584023.104, 0.581>0.578)
	// 57.75 - 63.00 (%) :  M >   24 (im=775, M=24.5, bin=  11) (sum, total, fraction>cut) = (23177632.234, 36584023.104, 0.634>0.630)
	// 63.00 - 68.25 (%) :  M >   18 (im=781, M=18.5, bin=  12) (sum, total, fraction>cut) = (24982936.706, 36584023.104, 0.683>0.683)
	// 68.25 - 73.50 (%) :  M >   13 (im=786, M=13.5, bin=  13) (sum, total, fraction>cut) = (26916490.129, 36584023.104, 0.736>0.735)
	// 73.50 - 78.75 (%) :  M >    9 (im=790, M=9.5, bin=  14) (sum, total, fraction>cut) = (28990174.993, 36584023.104, 0.792>0.788)
	// 78.75 - 84.00 (%) :  M >    6 (im=793, M=6.5, bin=  15) (sum, total, fraction>cut) = (31082429.908, 36584023.104, 0.850>0.840)
	//

	// Final centrality bins
	// 0.00 - 5.00 (%)   :  M >  263 (im=536, M=263.5, bin=   0) (sum, total, fraction>cut) = (1990649.322, 39152407.031, 0.051>0.050)
	// 5.00 - 10.00 (%)  :  M >  220 (im=579, M=220.5, bin=   1) (sum, total, fraction>cut) = (3932718.338, 39152407.031, 0.100>0.100)
	// 10.00 - 15.00 (%) :  M >  183 (im=616, M=183.5, bin=   2) (sum, total, fraction>cut) = (5911300.888, 39152407.031, 0.151>0.150)
	// 15.00 - 20.00 (%) :  M >  152 (im=647, M=152.5, bin=   3) (sum, total, fraction>cut) = (7860151.100, 39152407.031, 0.201>0.200)
	// 20.00 - 25.00 (%) :  M >  125 (im=674, M=125.5, bin=   4) (sum, total, fraction>cut) = (9839908.223, 39152407.031, 0.251>0.250)
	// 25.00 - 30.00 (%) :  M >  102 (im=697, M=102.5, bin=   5) (sum, total, fraction>cut) = (11809650.159, 39152407.031, 0.302>0.300)
	// 30.00 - 35.00 (%) :  M >   83 (im=716, M=83.5, bin=   6) (sum, total, fraction>cut) = (13705762.079, 39152407.031, 0.350>0.350)
	// 35.00 - 40.00 (%) :  M >   66 (im=733, M=66.5, bin=   7) (sum, total, fraction>cut) = (15697507.669, 39152407.031, 0.401>0.400)
	// 40.00 - 45.00 (%) :  M >   52 (im=747, M=52.5, bin=   8) (sum, total, fraction>cut) = (17654337.142, 39152407.031, 0.451>0.450)
	// 45.00 - 50.00 (%) :  M >   40 (im=759, M=40.5, bin=   9) (sum, total, fraction>cut) = (19666308.865, 39152407.031, 0.502>0.500)
	// 50.00 - 55.00 (%) :  M >   30 (im=769, M=30.5, bin=  10) (sum, total, fraction>cut) = (21739702.884, 39152407.031, 0.555>0.550)
	// 55.00 - 60.00 (%) :  M >   23 (im=776, M=23.5, bin=  11) (sum, total, fraction>cut) = (23540510.015, 39152407.031, 0.601>0.600)
	// 60.00 - 65.00 (%) :  M >   17 (im=782, M=17.5, bin=  12) (sum, total, fraction>cut) = (25471310.968, 39152407.031, 0.651>0.650)
	// 65.00 - 70.00 (%) :  M >   12 (im=787, M=12.5, bin=  13) (sum, total, fraction>cut) = (27562143.469, 39152407.031, 0.704>0.700)
	// 70.00 - 75.00 (%) :  M >    8 (im=791, M=8.5, bin=  14) (sum, total, fraction>cut) = (29853827.818, 39152407.031, 0.763>0.750)
	// 75.00 - 80.00 (%) :  M >    6 (im=793, M=6.5, bin=  15) (sum, total, fraction>cut) = (31386989.094, 39152407.031, 0.802>0.800)
	// 0.00 - 4.75 (%)   :  M >  266 (im=533, M=266.5, bin=   0) (sum, total, fraction>cut) = (1868877.506, 39152407.031, 0.048>0.048)
	// 4.75 - 9.50 (%)   :  M >  224 (im=575, M=224.5, bin=   1) (sum, total, fraction>cut) = (3736885.828, 39152407.031, 0.095>0.095)
	// 9.50 - 14.25 (%)  :  M >  188 (im=611, M=188.5, bin=   2) (sum, total, fraction>cut) = (5623883.068, 39152407.031, 0.144>0.143)
	// 14.25 - 19.00 (%) :  M >  158 (im=641, M=158.5, bin=   3) (sum, total, fraction>cut) = (7458337.777, 39152407.031, 0.190>0.190)
	// 19.00 - 23.75 (%) :  M >  131 (im=668, M=131.5, bin=   4) (sum, total, fraction>cut) = (9372350.178, 39152407.031, 0.239>0.237)
	// 23.75 - 28.50 (%) :  M >  109 (im=690, M=109.5, bin=   5) (sum, total, fraction>cut) = (11177088.210, 39152407.031, 0.285>0.285)
	// 28.50 - 33.25 (%) :  M >   89 (im=710, M=89.5, bin=   6) (sum, total, fraction>cut) = (13073333.249, 39152407.031, 0.334>0.333)
	// 33.25 - 38.00 (%) :  M >   72 (im=727, M=72.5, bin=   7) (sum, total, fraction>cut) = (14957151.313, 39152407.031, 0.382>0.380)
	// 38.00 - 42.75 (%) :  M >   58 (im=741, M=58.5, bin=   8) (sum, total, fraction>cut) = (16773822.999, 39152407.031, 0.428>0.427)
	// 42.75 - 47.50 (%) :  M >   46 (im=753, M=46.5, bin=   9) (sum, total, fraction>cut) = (18612835.049, 39152407.031, 0.475>0.475)
	// 47.50 - 52.25 (%) :  M >   35 (im=764, M=35.5, bin=  10) (sum, total, fraction>cut) = (20646606.832, 39152407.031, 0.527>0.522)
	// 52.25 - 57.00 (%) :  M >   27 (im=772, M=27.5, bin=  11) (sum, total, fraction>cut) = (22464915.089, 39152407.031, 0.574>0.570)
	// 57.00 - 61.75 (%) :  M >   20 (im=779, M=20.5, bin=  12) (sum, total, fraction>cut) = (24447945.353, 39152407.031, 0.624>0.617)
	// 61.75 - 66.50 (%) :  M >   15 (im=784, M=15.5, bin=  13) (sum, total, fraction>cut) = (26239770.921, 39152407.031, 0.670>0.665)
	// 66.50 - 71.25 (%) :  M >   11 (im=788, M=11.5, bin=  14) (sum, total, fraction>cut) = (28067060.740, 39152407.031, 0.717>0.712)
	// 71.25 - 76.00 (%) :  M >    8 (im=791, M=8.5, bin=  15) (sum, total, fraction>cut) = (29853827.818, 39152407.031, 0.763>0.760)
	// 0.00 - 5.25 (%)   :  M >  261 (im=538, M=261.5, bin=   0) (sum, total, fraction>cut) = (2073119.952, 39152407.031, 0.053>0.053)
	// 5.25 - 10.50 (%)  :  M >  216 (im=583, M=216.5, bin=   1) (sum, total, fraction>cut) = (4130946.974, 39152407.031, 0.106>0.105)
	// 10.50 - 15.75 (%) :  M >  178 (im=621, M=178.5, bin=   2) (sum, total, fraction>cut) = (6207230.441, 39152407.031, 0.159>0.158)
	// 15.75 - 21.00 (%) :  M >  146 (im=653, M=146.5, bin=   3) (sum, total, fraction>cut) = (8276842.338, 39152407.031, 0.211>0.210)
	// 21.00 - 26.25 (%) :  M >  119 (im=680, M=119.5, bin=   4) (sum, total, fraction>cut) = (10326627.455, 39152407.031, 0.264>0.263)
	// 26.25 - 31.50 (%) :  M >   96 (im=703, M=96.5, bin=   5) (sum, total, fraction>cut) = (12373773.701, 39152407.031, 0.316>0.315)
	// 31.50 - 36.75 (%) :  M >   76 (im=723, M=76.5, bin=   6) (sum, total, fraction>cut) = (14487729.614, 39152407.031, 0.370>0.368)
	// 36.75 - 42.00 (%) :  M >   60 (im=739, M=60.5, bin=   7) (sum, total, fraction>cut) = (16495966.197, 39152407.031, 0.421>0.420)
	// 42.00 - 47.25 (%) :  M >   46 (im=753, M=46.5, bin=   8) (sum, total, fraction>cut) = (18612835.049, 39152407.031, 0.475>0.473)
	// 47.25 - 52.50 (%) :  M >   35 (im=764, M=35.5, bin=   9) (sum, total, fraction>cut) = (20646606.832, 39152407.031, 0.527>0.525)
	// 52.50 - 57.75 (%) :  M >   26 (im=773, M=26.5, bin=  10) (sum, total, fraction>cut) = (22718278.145, 39152407.031, 0.580>0.578)
	// 57.75 - 63.00 (%) :  M >   19 (im=780, M=19.5, bin=  11) (sum, total, fraction>cut) = (24774922.765, 39152407.031, 0.633>0.630)
	// 63.00 - 68.25 (%) :  M >   13 (im=786, M=13.5, bin=  12) (sum, total, fraction>cut) = (27094272.205, 39152407.031, 0.692>0.683)
	// 68.25 - 73.50 (%) :  M >    9 (im=790, M=9.5, bin=  13) (sum, total, fraction>cut) = (29201368.616, 39152407.031, 0.746>0.735)
	// 73.50 - 78.75 (%) :  M >    6 (im=793, M=6.5, bin=  14) (sum, total, fraction>cut) = (31386989.094, 39152407.031, 0.802>0.788)
	// 78.75 - 84.00 (%) :  M >    4 (im=795, M=4.5, bin=  15) (sum, total, fraction>cut) = (33378906.955, 39152407.031, 0.853>0.840)

	mMultiplicityCut[0].push_back( 263 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	mMultiplicityCut[0].push_back( 220 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back( 10.0 );
	mMultiplicityCut[0].push_back( 183 ); mCentralityMin[0].push_back( 10.0 );  mCentralityMax[0].push_back( 15.0 );
	mMultiplicityCut[0].push_back( 152 ); mCentralityMin[0].push_back( 15.0 );  mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back( 125 ); mCentralityMin[0].push_back( 20.0 );  mCentralityMax[0].push_back( 25.0 );
	mMultiplicityCut[0].push_back( 102 ); mCentralityMin[0].push_back( 25.0 );  mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back(  83 ); mCentralityMin[0].push_back( 30.0 );  mCentralityMax[0].push_back( 35.0 );
	mMultiplicityCut[0].push_back(  66 ); mCentralityMin[0].push_back( 35.0 );  mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  52 ); mCentralityMin[0].push_back( 40.0 );  mCentralityMax[0].push_back( 45.0 );
	mMultiplicityCut[0].push_back(  40 ); mCentralityMin[0].push_back( 45.0 );  mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  30 ); mCentralityMin[0].push_back( 50.0 );  mCentralityMax[0].push_back( 55.0 );
	mMultiplicityCut[0].push_back(  23 ); mCentralityMin[0].push_back( 55.0 );  mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  17 ); mCentralityMin[0].push_back( 60.0 );  mCentralityMax[0].push_back( 65.0 );
	mMultiplicityCut[0].push_back(  12 ); mCentralityMin[0].push_back( 65.0 );  mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(   8 ); mCentralityMin[0].push_back( 70.0 );  mCentralityMax[0].push_back( 75.0 );
	mMultiplicityCut[0].push_back(   6 ); mCentralityMin[0].push_back( 75.0 );  mCentralityMax[0].push_back( 80.0 );

	// -5%                                 +5%
	mMultiplicityCut[1].push_back( 266 );  mMultiplicityCut[2].push_back( 261 );
	mMultiplicityCut[1].push_back( 224 );  mMultiplicityCut[2].push_back( 216 );
	mMultiplicityCut[1].push_back( 188 );  mMultiplicityCut[2].push_back( 178 );
	mMultiplicityCut[1].push_back( 158 );  mMultiplicityCut[2].push_back( 146 );
	mMultiplicityCut[1].push_back( 131 );  mMultiplicityCut[2].push_back( 119 );
	mMultiplicityCut[1].push_back( 109 );  mMultiplicityCut[2].push_back(  96 );
	mMultiplicityCut[1].push_back(  89 );  mMultiplicityCut[2].push_back(  76 );
	mMultiplicityCut[1].push_back(  72 );  mMultiplicityCut[2].push_back(  60 );
	mMultiplicityCut[1].push_back(  58 );  mMultiplicityCut[2].push_back(  46 );
	mMultiplicityCut[1].push_back(  46 );  mMultiplicityCut[2].push_back(  35 );
	mMultiplicityCut[1].push_back(  35 );  mMultiplicityCut[2].push_back(  26 );
	mMultiplicityCut[1].push_back(  27 );  mMultiplicityCut[2].push_back(  19 );
	mMultiplicityCut[1].push_back(  20 );  mMultiplicityCut[2].push_back(  13 );
	mMultiplicityCut[1].push_back(  15 );  mMultiplicityCut[2].push_back(   9 );
	mMultiplicityCut[1].push_back(  11 );  mMultiplicityCut[2].push_back(   6 );
	mMultiplicityCut[1].push_back(   8 );  mMultiplicityCut[2].push_back(   4 );

	// Set same centrality bins 
	for(UInt_t ic=0; ic<mCentralityMin[0].size(); ic++){
		for(UInt_t im=1; im<mNMode; im++){
			mCentralityMin[im].push_back( mCentralityMin[0][ic] );
			mCentralityMax[im].push_back( mCentralityMax[0][ic] );
		}
	}

}

//____________________________________________________________________________________________________
void StCentrality::Init_AuAu11GeV()
{
	/// Initialize Au + Au collisiona at 11.5 GeV
	//----------------------------------------------------------------------------------------------------
	//  Update on Jun/13/2010
	//    npp = 1.07 from fitting, M>30
	//    k = 2.0 (not final, but probably ok)
	//    x = 0.12 (see above)
	//    with d = 0.14 (factor for multiplicity dependent efficiency)
	//----------------------------------------------------------------------------------------------------
	//  Update on Oct/25/2010 (with full production)
	//    npp = 1.06 from fitting, M>30
	//    k = 2.0
	//    x = 0.12 (see above)
	//    with d = 0.14 (factor for multiplicity dependent efficiency)
	//
	//    ---> Stick to the 1.07
	//----------------------------------------------------------------------------------------------------

	// Error on x (energy specific ???)
	// NOTE: type 'low' and 'high' are for npp, x is anti-correlated with npp
	Double_t xError = 0.0 ; // absolute error
	if( mType.CompareTo("low", TString::kIgnoreCase) == 0 )       xError = 0.02 ;  // npp is low, x is high
	else if( mType.CompareTo("high", TString::kIgnoreCase) == 0 ) xError = -0.02 ; // npp is high, x is low

	const Double_t npp = 1.07 ; // default npp
	//  const Double_t npp = 1.06 ; // default npp
	mNpp = npp + GetNppError(npp) ;
	mK   = 2.00 ;
	mX   = 0.12 + xError ;
	mEfficiency  = 0.14 ;
	mTriggerBias = 1.00 ;

	// Define multiplicity cut from central to peripheral
	//   For the fast offline
	// from
#if 0
	mMultiplicityCut[0].push_back( 221 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	mMultiplicityCut[0].push_back( 184 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back( 10.0 );
	mMultiplicityCut[0].push_back( 127 ); mCentralityMin[0].push_back( 10.0 );  mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back(  86 ); mCentralityMin[0].push_back( 20.0 );  mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back(  56 ); mCentralityMin[0].push_back( 30.0 );  mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  34 ); mCentralityMin[0].push_back( 40.0 );  mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  19 ); mCentralityMin[0].push_back( 50.0 );  mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  10 ); mCentralityMin[0].push_back( 60.0 );  mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(   5 ); mCentralityMin[0].push_back( 70.0 );  mCentralityMax[0].push_back( 80.0 );

	// -5%                                +5%
	mMultiplicityCut[1].push_back( 223 ); mMultiplicityCut[2].push_back( 219 ); //  0-5%
	mMultiplicityCut[1].push_back( 188 ); mMultiplicityCut[2].push_back( 181 ); //  5-10%
	mMultiplicityCut[1].push_back( 132 ); mMultiplicityCut[2].push_back( 123 ); // 10-20%
	mMultiplicityCut[1].push_back(  91 ); mMultiplicityCut[2].push_back(  81 ); // 20-30%
	mMultiplicityCut[1].push_back(  61 ); mMultiplicityCut[2].push_back(  51 ); // 30-40%
	mMultiplicityCut[1].push_back(  39 ); mMultiplicityCut[2].push_back(  30 ); // 40-50%
	mMultiplicityCut[1].push_back(  23 ); mMultiplicityCut[2].push_back(  16 ); // 50-60%
	mMultiplicityCut[1].push_back(  13 ); mMultiplicityCut[2].push_back(   8 ); // 60-70%
	mMultiplicityCut[1].push_back(   7 ); mMultiplicityCut[2].push_back(   3 ); // 70-80%

	//   For the full production
	mMultiplicityCut[0].push_back( 219 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	mMultiplicityCut[0].push_back( 183 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back( 10.0 );
	mMultiplicityCut[0].push_back( 126 ); mCentralityMin[0].push_back( 10.0 );  mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back(  85 ); mCentralityMin[0].push_back( 20.0 );  mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back(  55 ); mCentralityMin[0].push_back( 30.0 );  mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  34 ); mCentralityMin[0].push_back( 40.0 );  mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  19 ); mCentralityMin[0].push_back( 50.0 );  mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  10 ); mCentralityMin[0].push_back( 60.0 );  mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(   5 ); mCentralityMin[0].push_back( 70.0 );  mCentralityMax[0].push_back( 80.0 );

	// -5%                                +5%
	mMultiplicityCut[1].push_back( 221 ); mMultiplicityCut[2].push_back( 217 ); //  0-5%
	mMultiplicityCut[1].push_back( 186 ); mMultiplicityCut[2].push_back( 179 ); //  5-10%
	mMultiplicityCut[1].push_back( 131 ); mMultiplicityCut[2].push_back( 121 ); // 10-20%
	mMultiplicityCut[1].push_back(  91 ); mMultiplicityCut[2].push_back(  80 ); // 20-30%
	mMultiplicityCut[1].push_back(  60 ); mMultiplicityCut[2].push_back(  50 ); // 30-40%
	mMultiplicityCut[1].push_back(  38 ); mMultiplicityCut[2].push_back(  30 ); // 40-50%
	mMultiplicityCut[1].push_back(  23 ); mMultiplicityCut[2].push_back(  16 ); // 50-60%
	mMultiplicityCut[1].push_back(  13 ); mMultiplicityCut[2].push_back(   8 ); // 60-70%
	mMultiplicityCut[1].push_back(   6 ); mMultiplicityCut[2].push_back(   3 ); // 70-80%
#endif

	// Final centrality bins
	// Define multiplicity cut from central to peripheral
	//  mMultiplicityCut[0].push_back( 221 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	//  mMultiplicityCut[0].push_back( 184 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back( 10.0 );

	// Temporary to include 7.5%
	mMultiplicityCut[0].push_back( 221 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	mMultiplicityCut[0].push_back( 202 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back(  7.5 );
	mMultiplicityCut[0].push_back( 184 ); mCentralityMin[0].push_back(  7.5 );  mCentralityMax[0].push_back( 10.0 );

	mMultiplicityCut[0].push_back( 153 ); mCentralityMin[0].push_back( 10.0 );  mCentralityMax[0].push_back( 15.0 );
	mMultiplicityCut[0].push_back( 127 ); mCentralityMin[0].push_back( 15.0 );  mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back( 105 ); mCentralityMin[0].push_back( 20.0 );  mCentralityMax[0].push_back( 25.0 );
	mMultiplicityCut[0].push_back(  86 ); mCentralityMin[0].push_back( 25.0 );  mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back(  70 ); mCentralityMin[0].push_back( 30.0 );  mCentralityMax[0].push_back( 35.0 );
	mMultiplicityCut[0].push_back(  56 ); mCentralityMin[0].push_back( 35.0 );  mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  44 ); mCentralityMin[0].push_back( 40.0 );  mCentralityMax[0].push_back( 45.0 );
	mMultiplicityCut[0].push_back(  34 ); mCentralityMin[0].push_back( 45.0 );  mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  26 ); mCentralityMin[0].push_back( 50.0 );  mCentralityMax[0].push_back( 55.0 );
	mMultiplicityCut[0].push_back(  19 ); mCentralityMin[0].push_back( 55.0 );  mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  14 ); mCentralityMin[0].push_back( 60.0 );  mCentralityMax[0].push_back( 65.0 );
	mMultiplicityCut[0].push_back(  10 ); mCentralityMin[0].push_back( 65.0 );  mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(   7 ); mCentralityMin[0].push_back( 70.0 );  mCentralityMax[0].push_back( 75.0 );
	mMultiplicityCut[0].push_back(   5 ); mCentralityMin[0].push_back( 75.0 );  mCentralityMax[0].push_back( 80.0 );

	// -5%                                 +5%
	mMultiplicityCut[1].push_back( 223 );  mMultiplicityCut[2].push_back( 219 );
	mMultiplicityCut[1].push_back( 204 );  mMultiplicityCut[2].push_back( 199 ); // temp
	mMultiplicityCut[1].push_back( 188 );  mMultiplicityCut[2].push_back( 181 );
	mMultiplicityCut[1].push_back( 158 );  mMultiplicityCut[2].push_back( 149 );
	mMultiplicityCut[1].push_back( 132 );  mMultiplicityCut[2].push_back( 123 );
	mMultiplicityCut[1].push_back( 110 );  mMultiplicityCut[2].push_back( 100 );
	mMultiplicityCut[1].push_back(  91 );  mMultiplicityCut[2].push_back(  81 );
	mMultiplicityCut[1].push_back(  75 );  mMultiplicityCut[2].push_back(  64 );
	mMultiplicityCut[1].push_back(  61 );  mMultiplicityCut[2].push_back(  51 );
	mMultiplicityCut[1].push_back(  49 );  mMultiplicityCut[2].push_back(  39 );
	mMultiplicityCut[1].push_back(  39 );  mMultiplicityCut[2].push_back(  30 );
	mMultiplicityCut[1].push_back(  30 );  mMultiplicityCut[2].push_back(  22 );
	mMultiplicityCut[1].push_back(  23 );  mMultiplicityCut[2].push_back(  16 );
	mMultiplicityCut[1].push_back(  17 );  mMultiplicityCut[2].push_back(  11 );
	mMultiplicityCut[1].push_back(  13 );  mMultiplicityCut[2].push_back(   8 );
	mMultiplicityCut[1].push_back(   9 );  mMultiplicityCut[2].push_back(   5 );
	mMultiplicityCut[1].push_back(   7 );  mMultiplicityCut[2].push_back(   3 );

	// Set same centrality bins 
	for(UInt_t ic=0; ic<mCentralityMin[0].size(); ic++){
		for(UInt_t im=1; im<mNMode; im++){
			mCentralityMin[im].push_back( mCentralityMin[0][ic] );
			mCentralityMax[im].push_back( mCentralityMax[0][ic] );
		}
	}

}

//____________________________________________________________________________________________________
void StCentrality::Init_AuAu7GeV()
{
	/// Initialize Au + Au collisiona at 7.7 GeV
	//----------------------------------------------------------------------------------------------------
	//    npp = 0.71 from 0.528*log(sqrt(sNN)) - 0.3696 
	//    (Paul's study in http://www.star.bnl.gov/protected/ebye/sorensen/bes/sorensen_glauber_9.2.pdf)
	//    k = 2.0 (probably k dependence is weak, need to study)
	//    x = 0.118 --> 0.12 from extrapolation between 19.6 and 200 GeV data (PHOBOS, PRC70, 021902, 2004)
	//      NOTE: parameters are not final yet (May/03/2010)
	//----------------------------------------------------------------------------------------------------
	//    Update: May/07/2010
	//    npp = 0.86 from fitting, M>30
	//    k = 2.0 (not final, but probably ok)
	//    x = 0.12 (see above)
	//    with d = 0.14 (factor for multiplicity dependent efficiency)
	//----------------------------------------------------------------------------------------------------
	//    Update: Sep/08/2010
	//    npp = 0.89 from fitting, M>30
	//    k = 2.0
	//    x = 0.12 (see above)
	//    with d = 0.14 (factor for multiplicity dependent efficiency)
	//----------------------------------------------------------------------------------------------------

	// Error on x (energy specific ???)
	// NOTE: type 'low' and 'high' are for npp, x is anti-correlated with npp
	Double_t xError = 0.0 ; // absolute error
	if( mType.CompareTo("low", TString::kIgnoreCase) == 0 )       xError = 0.02 ;  // npp is low, x is high
	else if( mType.CompareTo("high", TString::kIgnoreCase) == 0 ) xError = -0.02 ; // npp is high, x is low

	const Double_t npp = 0.89 ; // default npp
	mNpp = npp + GetNppError(npp) ;
	mK   = 2.00 ;
	mX   = 0.12 + xError ;
	mEfficiency  = 0.14 ;
	mTriggerBias = 1.00 ;

	// Fast offline (obsolete)
	// Define multiplicity cut from central to peripheral
	// from
	//  mMultiplicityCut[0].push_back( 179 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	//  mMultiplicityCut[0].push_back( 149 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back( 10.0 );
	//  mMultiplicityCut[0].push_back( 103 ); mCentralityMin[0].push_back( 10.0 );  mCentralityMax[0].push_back( 20.0 );
	//  mMultiplicityCut[0].push_back(  69 ); mCentralityMin[0].push_back( 20.0 );  mCentralityMax[0].push_back( 30.0 );
	//  mMultiplicityCut[0].push_back(  45 ); mCentralityMin[0].push_back( 30.0 );  mCentralityMax[0].push_back( 40.0 );
	//  mMultiplicityCut[0].push_back(  27 ); mCentralityMin[0].push_back( 40.0 );  mCentralityMax[0].push_back( 50.0 );
	//  mMultiplicityCut[0].push_back(  16 ); mCentralityMin[0].push_back( 50.0 );  mCentralityMax[0].push_back( 60.0 );
	//  mMultiplicityCut[0].push_back(   8 ); mCentralityMin[0].push_back( 60.0 );  mCentralityMax[0].push_back( 70.0 );
	//  mMultiplicityCut[0].push_back(   4 ); mCentralityMin[0].push_back( 70.0 );  mCentralityMax[0].push_back( 80.0 );
	// 
	//  // -5%                                +5%
	//  mMultiplicityCut[1].push_back( 181 ); mMultiplicityCut[2].push_back( 177 ); //  0-5%
	//  mMultiplicityCut[1].push_back( 152 ); mMultiplicityCut[2].push_back( 146 ); //  5-10%
	//  mMultiplicityCut[1].push_back( 107 ); mMultiplicityCut[2].push_back(  99 ); // 10-20%
	//  mMultiplicityCut[1].push_back(  74 ); mMultiplicityCut[2].push_back(  65 ); // 20-30%
	//  mMultiplicityCut[1].push_back(  49 ); mMultiplicityCut[2].push_back(  41 ); // 30-40%
	//  mMultiplicityCut[1].push_back(  31 ); mMultiplicityCut[2].push_back(  24 ); // 40-50%
	//  mMultiplicityCut[1].push_back(  19 ); mMultiplicityCut[2].push_back(  13 ); // 50-60%
	//  mMultiplicityCut[1].push_back(  10 ); mMultiplicityCut[2].push_back(   6 ); // 60-70%
	//  mMultiplicityCut[1].push_back(   5 ); mMultiplicityCut[2].push_back(   3 ); // 70-80%

	// Final centrality bins
	// Define multiplicity cut from central to peripheral
	//  mMultiplicityCut[0].push_back( 185 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	//  mMultiplicityCut[0].push_back( 154 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back( 10.0 );

	// Temporary to include 7.5%
	mMultiplicityCut[0].push_back( 185 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	mMultiplicityCut[0].push_back( 169 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back(  7.5 );
	mMultiplicityCut[0].push_back( 154 ); mCentralityMin[0].push_back(  7.5 );  mCentralityMax[0].push_back( 10.0 );

	mMultiplicityCut[0].push_back( 128 ); mCentralityMin[0].push_back( 10.0 );  mCentralityMax[0].push_back( 15.0 );
	mMultiplicityCut[0].push_back( 106 ); mCentralityMin[0].push_back( 15.0 );  mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back(  87 ); mCentralityMin[0].push_back( 20.0 );  mCentralityMax[0].push_back( 25.0 );
	mMultiplicityCut[0].push_back(  72 ); mCentralityMin[0].push_back( 25.0 );  mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back(  58 ); mCentralityMin[0].push_back( 30.0 );  mCentralityMax[0].push_back( 35.0 );
	mMultiplicityCut[0].push_back(  46 ); mCentralityMin[0].push_back( 35.0 );  mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  37 ); mCentralityMin[0].push_back( 40.0 );  mCentralityMax[0].push_back( 45.0 );
	mMultiplicityCut[0].push_back(  28 ); mCentralityMin[0].push_back( 45.0 );  mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  22 ); mCentralityMin[0].push_back( 50.0 );  mCentralityMax[0].push_back( 55.0 );
	mMultiplicityCut[0].push_back(  16 ); mCentralityMin[0].push_back( 55.0 );  mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  12 ); mCentralityMin[0].push_back( 60.0 );  mCentralityMax[0].push_back( 65.0 );
	mMultiplicityCut[0].push_back(   8 ); mCentralityMin[0].push_back( 65.0 );  mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(   6 ); mCentralityMin[0].push_back( 70.0 );  mCentralityMax[0].push_back( 75.0 );
	mMultiplicityCut[0].push_back(   4 ); mCentralityMin[0].push_back( 75.0 );  mCentralityMax[0].push_back( 80.0 );

	// -5%                                 +5%
	mMultiplicityCut[1].push_back( 187 );  mMultiplicityCut[2].push_back( 183 );
	mMultiplicityCut[1].push_back( 171 );  mMultiplicityCut[2].push_back( 167 ); // temp
	mMultiplicityCut[1].push_back( 157 );  mMultiplicityCut[2].push_back( 152 );
	mMultiplicityCut[1].push_back( 132 );  mMultiplicityCut[2].push_back( 125 );
	mMultiplicityCut[1].push_back( 110 );  mMultiplicityCut[2].push_back( 102 );
	mMultiplicityCut[1].push_back(  92 );  mMultiplicityCut[2].push_back(  83 );
	mMultiplicityCut[1].push_back(  76 );  mMultiplicityCut[2].push_back(  67 );
	mMultiplicityCut[1].push_back(  62 );  mMultiplicityCut[2].push_back(  54 );
	mMultiplicityCut[1].push_back(  51 );  mMultiplicityCut[2].push_back(  42 );
	mMultiplicityCut[1].push_back(  41 );  mMultiplicityCut[2].push_back(  33 );
	mMultiplicityCut[1].push_back(  32 );  mMultiplicityCut[2].push_back(  25 );
	mMultiplicityCut[1].push_back(  25 );  mMultiplicityCut[2].push_back(  18 );
	mMultiplicityCut[1].push_back(  19 );  mMultiplicityCut[2].push_back(  13 );
	mMultiplicityCut[1].push_back(  15 );  mMultiplicityCut[2].push_back(   9 );
	mMultiplicityCut[1].push_back(  11 );  mMultiplicityCut[2].push_back(   7 );
	mMultiplicityCut[1].push_back(   8 );  mMultiplicityCut[2].push_back(   4 );
	mMultiplicityCut[1].push_back(   5 );  mMultiplicityCut[2].push_back(   3 );

	// Set same centrality bins 
	for(UInt_t ic=0; ic<mCentralityMin[0].size(); ic++){
		for(UInt_t im=1; im<mNMode; im++){
			mCentralityMin[im].push_back( mCentralityMin[0][ic] );
			mCentralityMax[im].push_back( mCentralityMax[0][ic] );
		}
	}

}

#if 0
//____________________________________________________________________________________________________
void StCentrality::Init_SmSm200GeV()
{
	/// Initialize Au + Au collisiona at 200 GeV
	//----------------------------------------------------------------------------------------------------
	//    From the NBD fit, chi2/NDF ~ 1.5
	//    npp = 2.28
	//    k = 0.75
	//    x = 0.14 (PHOBOS, PRC70, 021902, 2004)
	//----------------------------------------------------------------------------------------------------
	//    Update: Jul/09/2010
	mNpp = 2.28 ;
	mK   = 0.75 ;
	mX   = 0.14 ;
	mEfficiency  = 0.14 ; // Multiplicity dependent efficiency
	mTriggerBias = 1.00 ;

	// Centrality definition in 200 GeV Run4
	// Define multiplicity cut from central to peripheral
	mMultiplicityCut[0].push_back( 514 - 1 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	mMultiplicityCut[0].push_back( 432 - 1 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back( 10.0 );
	mMultiplicityCut[0].push_back( 302 - 1 ); mCentralityMin[0].push_back( 10.0 );  mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back( 208 - 1 ); mCentralityMin[0].push_back( 20.0 );  mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back( 138 - 1 ); mCentralityMin[0].push_back( 30.0 );  mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  88 - 1 ); mCentralityMin[0].push_back( 40.0 );  mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  52 - 1 ); mCentralityMin[0].push_back( 50.0 );  mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  28 - 1 ); mCentralityMin[0].push_back( 60.0 );  mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(  14 - 1 ); mCentralityMin[0].push_back( 70.0 );  mCentralityMax[0].push_back( 80.0 );

	// Set same centrality bins 
	for(UInt_t ic=0; ic<mCentralityMin[0].size(); ic++){
		for(UInt_t im=1; im<mNMode; im++){
			mMultiplicityCut[im].push_back( mMultiplicityCut[0][ic] );
			mCentralityMin[im].push_back( mCentralityMin[0][ic] );
			mCentralityMax[im].push_back( mCentralityMax[0][ic] );
		}
	}
}
#endif


//____________________________________________________________________________________________________
void StCentrality::Init_dAu200GeV()
{
	/// Initialize Au + Au collisiona at 200 GeV
	//----------------------------------------------------------------------------------------------------
	//    From the NBD fit, chi2/NDF ~ 1.5
	//    npp = 2.28
	//    k = 0.75
	//    x = 0.14 (PHOBOS, PRC70, 021902, 2004)
	//----------------------------------------------------------------------------------------------------
	//    Update: Jul/09/2010
	mNpp = 2.28 ;
	mK   = 0.75 ;
	mX   = 0.14 ;
	mEfficiency  = 0.14 ; // Multiplicity dependent efficiency
	mTriggerBias = 1.00 ;

	// Define multiplicity cut from central to peripheral (copy from 39 GeV)
	mMultiplicityCut[0].push_back( 291 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	mMultiplicityCut[0].push_back( 241 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back( 10.0 );
	mMultiplicityCut[0].push_back( 163 ); mCentralityMin[0].push_back( 10.0 );  mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back( 109 ); mCentralityMin[0].push_back( 20.0 );  mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back(  70 ); mCentralityMin[0].push_back( 30.0 );  mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  43 ); mCentralityMin[0].push_back( 40.0 );  mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  24 ); mCentralityMin[0].push_back( 50.0 );  mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  13 ); mCentralityMin[0].push_back( 60.0 );  mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(   6 ); mCentralityMin[0].push_back( 70.0 );  mCentralityMax[0].push_back( 80.0 );

	// Set same centrality bins 
	for(UInt_t ic=0; ic<mCentralityMin[0].size(); ic++){
		for(UInt_t im=1; im<mNMode; im++){
			mMultiplicityCut[im].push_back( mMultiplicityCut[0][ic] );
			mCentralityMin[im].push_back( mCentralityMin[0][ic] );
			mCentralityMax[im].push_back( mCentralityMax[0][ic] );
		}
	}
}

//____________________________________________________________________________________________________
void StCentrality::Init_CuCu200GeV()
{
	/// Initialize Cu + Cu collisiona at 200 GeV
	// Copy from Au+Au 200 GeV
	//----------------------------------------------------------------------------------------------------
	mNpp = 2.43 ;
	mK   = 2.00 ;
	mX   = 0.13 ;
	mEfficiency  = 0.14 ; // Multiplicity dependent efficiency
	mTriggerBias = 1.00 ;

	// Define multiplicity cut from central to peripheral
	mMultiplicityCut[0].push_back( 441 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	mMultiplicityCut[0].push_back( 375 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back( 10.0 );
	mMultiplicityCut[0].push_back( 317 ); mCentralityMin[0].push_back( 10.0 );  mCentralityMax[0].push_back( 15.0 );
	mMultiplicityCut[0].push_back( 266 ); mCentralityMin[0].push_back( 15.0 );  mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back( 221 ); mCentralityMin[0].push_back( 20.0 );  mCentralityMax[0].push_back( 25.0 );
	mMultiplicityCut[0].push_back( 182 ); mCentralityMin[0].push_back( 25.0 );  mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back( 148 ); mCentralityMin[0].push_back( 30.0 );  mCentralityMax[0].push_back( 35.0 );
	mMultiplicityCut[0].push_back( 118 ); mCentralityMin[0].push_back( 35.0 );  mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  93 ); mCentralityMin[0].push_back( 40.0 );  mCentralityMax[0].push_back( 45.0 );
	mMultiplicityCut[0].push_back(  72 ); mCentralityMin[0].push_back( 45.0 );  mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  55 ); mCentralityMin[0].push_back( 50.0 );  mCentralityMax[0].push_back( 55.0 );
	mMultiplicityCut[0].push_back(  41 ); mCentralityMin[0].push_back( 55.0 );  mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  30 ); mCentralityMin[0].push_back( 60.0 );  mCentralityMax[0].push_back( 65.0 );
	mMultiplicityCut[0].push_back(  21 ); mCentralityMin[0].push_back( 65.0 );  mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(  15 ); mCentralityMin[0].push_back( 70.0 );  mCentralityMax[0].push_back( 75.0 );
	mMultiplicityCut[0].push_back(  10 ); mCentralityMin[0].push_back( 75.0 );  mCentralityMax[0].push_back( 80.0 );

	// -5%                                 +5%
	mMultiplicityCut[1].push_back( 445 );  mMultiplicityCut[2].push_back( 438 );
	mMultiplicityCut[1].push_back( 382 );  mMultiplicityCut[2].push_back( 369 );
	mMultiplicityCut[1].push_back( 325 );  mMultiplicityCut[2].push_back( 309 );
	mMultiplicityCut[1].push_back( 276 );  mMultiplicityCut[2].push_back( 257 );
	mMultiplicityCut[1].push_back( 232 );  mMultiplicityCut[2].push_back( 211 );
	mMultiplicityCut[1].push_back( 193 );  mMultiplicityCut[2].push_back( 171 );
	mMultiplicityCut[1].push_back( 159 );  mMultiplicityCut[2].push_back( 137 );
	mMultiplicityCut[1].push_back( 129 );  mMultiplicityCut[2].push_back( 108 );
	mMultiplicityCut[1].push_back( 104 );  mMultiplicityCut[2].push_back(  83 );
	mMultiplicityCut[1].push_back(  82 );  mMultiplicityCut[2].push_back(  63 );
	mMultiplicityCut[1].push_back(  64 );  mMultiplicityCut[2].push_back(  47 );
	mMultiplicityCut[1].push_back(  49 );  mMultiplicityCut[2].push_back(  34 );
	mMultiplicityCut[1].push_back(  37 );  mMultiplicityCut[2].push_back(  24 );
	mMultiplicityCut[1].push_back(  27 );  mMultiplicityCut[2].push_back(  16 );
	mMultiplicityCut[1].push_back(  19 );  mMultiplicityCut[2].push_back(  11 );
	mMultiplicityCut[1].push_back(  13 );  mMultiplicityCut[2].push_back(   7 );

	// Set same centrality bins 
	for(UInt_t ic=0; ic<mCentralityMin[0].size(); ic++){
		for(UInt_t im=1; im<mNMode; im++){
			mCentralityMin[im].push_back( mCentralityMin[0][ic] );
			mCentralityMax[im].push_back( mCentralityMax[0][ic] );
		}
	}
}

//____________________________________________________________________________________________________
void StCentrality::Init_ZrZr200GeV()
{
        /// Initialize Zr + Zr collisiona at 200 GeV
        // Copy from Cu+Cu 200 GeV
        //----------------------------------------------------------------------------------------------------
        mNpp = 2.43 ;
        mK   = 2.00 ;
        mX   = 0.13 ;
        mEfficiency  = 0.14 ; // Multiplicity dependent efficiency
        mTriggerBias = 1.00 ;

        // Define multiplicity cut from central to peripheral
        mMultiplicityCut[0].push_back( 441 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
        mMultiplicityCut[0].push_back( 375 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back( 10.0 );
        mMultiplicityCut[0].push_back( 317 ); mCentralityMin[0].push_back( 10.0 );  mCentralityMax[0].push_back( 15.0 );
        mMultiplicityCut[0].push_back( 266 ); mCentralityMin[0].push_back( 15.0 );  mCentralityMax[0].push_back( 20.0 );
        mMultiplicityCut[0].push_back( 221 ); mCentralityMin[0].push_back( 20.0 );  mCentralityMax[0].push_back( 25.0 );
        mMultiplicityCut[0].push_back( 182 ); mCentralityMin[0].push_back( 25.0 );  mCentralityMax[0].push_back( 30.0 );
        mMultiplicityCut[0].push_back( 148 ); mCentralityMin[0].push_back( 30.0 );  mCentralityMax[0].push_back( 35.0 );
        mMultiplicityCut[0].push_back( 118 ); mCentralityMin[0].push_back( 35.0 );  mCentralityMax[0].push_back( 40.0 );
        mMultiplicityCut[0].push_back(  93 ); mCentralityMin[0].push_back( 40.0 );  mCentralityMax[0].push_back( 45.0 );
        mMultiplicityCut[0].push_back(  72 ); mCentralityMin[0].push_back( 45.0 );  mCentralityMax[0].push_back( 50.0 );
        mMultiplicityCut[0].push_back(  55 ); mCentralityMin[0].push_back( 50.0 );  mCentralityMax[0].push_back( 55.0 );
        mMultiplicityCut[0].push_back(  41 ); mCentralityMin[0].push_back( 55.0 );  mCentralityMax[0].push_back( 60.0 );
        mMultiplicityCut[0].push_back(  30 ); mCentralityMin[0].push_back( 60.0 );  mCentralityMax[0].push_back( 65.0 );
        mMultiplicityCut[0].push_back(  21 ); mCentralityMin[0].push_back( 65.0 );  mCentralityMax[0].push_back( 70.0 );
        mMultiplicityCut[0].push_back(  15 ); mCentralityMin[0].push_back( 70.0 );  mCentralityMax[0].push_back( 75.0 );
        mMultiplicityCut[0].push_back(  10 ); mCentralityMin[0].push_back( 75.0 );  mCentralityMax[0].push_back( 80.0 );

	// -5%                                 +5%
        mMultiplicityCut[1].push_back( 445 );  mMultiplicityCut[2].push_back( 438 );
        mMultiplicityCut[1].push_back( 382 );  mMultiplicityCut[2].push_back( 369 );
        mMultiplicityCut[1].push_back( 325 );  mMultiplicityCut[2].push_back( 309 );
        mMultiplicityCut[1].push_back( 276 );  mMultiplicityCut[2].push_back( 257 );
        mMultiplicityCut[1].push_back( 232 );  mMultiplicityCut[2].push_back( 211 );
        mMultiplicityCut[1].push_back( 193 );  mMultiplicityCut[2].push_back( 171 );
        mMultiplicityCut[1].push_back( 159 );  mMultiplicityCut[2].push_back( 137 );
        mMultiplicityCut[1].push_back( 129 );  mMultiplicityCut[2].push_back( 108 );
        mMultiplicityCut[1].push_back( 104 );  mMultiplicityCut[2].push_back(  83 );
        mMultiplicityCut[1].push_back(  82 );  mMultiplicityCut[2].push_back(  63 );
        mMultiplicityCut[1].push_back(  64 );  mMultiplicityCut[2].push_back(  47 );
        mMultiplicityCut[1].push_back(  49 );  mMultiplicityCut[2].push_back(  34 );
        mMultiplicityCut[1].push_back(  37 );  mMultiplicityCut[2].push_back(  24 );
        mMultiplicityCut[1].push_back(  27 );  mMultiplicityCut[2].push_back(  16 );
        mMultiplicityCut[1].push_back(  19 );  mMultiplicityCut[2].push_back(  11 );
        mMultiplicityCut[1].push_back(  13 );  mMultiplicityCut[2].push_back(   7 );

        // Set same centrality bins 
        for(UInt_t ic=0; ic<mCentralityMin[0].size(); ic++){
                for(UInt_t im=1; im<mNMode; im++){
                        mCentralityMin[im].push_back( mCentralityMin[0][ic] );
                        mCentralityMax[im].push_back( mCentralityMax[0][ic] );
                }
        }
}


//____________________________________________________________________________________________________
void StCentrality::Init_UU200GeV()
{
	/// Initialize U + U collisiona at 200 GeV
	// Copy from Au+Au 200 GeV
	//----------------------------------------------------------------------------------------------------
	mNpp = 2.43 ;
	mK   = 2.00 ;
	mX   = 0.13 ;
	mEfficiency  = 0.14 ; // Multiplicity dependent efficiency
	mTriggerBias = 1.00 ;

	// Define multiplicity cut from central to peripheral
	mMultiplicityCut[0].push_back( 441 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	mMultiplicityCut[0].push_back( 375 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back( 10.0 );
	mMultiplicityCut[0].push_back( 317 ); mCentralityMin[0].push_back( 10.0 );  mCentralityMax[0].push_back( 15.0 );
	mMultiplicityCut[0].push_back( 266 ); mCentralityMin[0].push_back( 15.0 );  mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back( 221 ); mCentralityMin[0].push_back( 20.0 );  mCentralityMax[0].push_back( 25.0 );
	mMultiplicityCut[0].push_back( 182 ); mCentralityMin[0].push_back( 25.0 );  mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back( 148 ); mCentralityMin[0].push_back( 30.0 );  mCentralityMax[0].push_back( 35.0 );
	mMultiplicityCut[0].push_back( 118 ); mCentralityMin[0].push_back( 35.0 );  mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  93 ); mCentralityMin[0].push_back( 40.0 );  mCentralityMax[0].push_back( 45.0 );
	mMultiplicityCut[0].push_back(  72 ); mCentralityMin[0].push_back( 45.0 );  mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  55 ); mCentralityMin[0].push_back( 50.0 );  mCentralityMax[0].push_back( 55.0 );
	mMultiplicityCut[0].push_back(  41 ); mCentralityMin[0].push_back( 55.0 );  mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  30 ); mCentralityMin[0].push_back( 60.0 );  mCentralityMax[0].push_back( 65.0 );
	mMultiplicityCut[0].push_back(  21 ); mCentralityMin[0].push_back( 65.0 );  mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(  15 ); mCentralityMin[0].push_back( 70.0 );  mCentralityMax[0].push_back( 75.0 );
	mMultiplicityCut[0].push_back(  10 ); mCentralityMin[0].push_back( 75.0 );  mCentralityMax[0].push_back( 80.0 );

	// -5%                                 +5%
	mMultiplicityCut[1].push_back( 445 );  mMultiplicityCut[2].push_back( 438 );
	mMultiplicityCut[1].push_back( 382 );  mMultiplicityCut[2].push_back( 369 );
	mMultiplicityCut[1].push_back( 325 );  mMultiplicityCut[2].push_back( 309 );
	mMultiplicityCut[1].push_back( 276 );  mMultiplicityCut[2].push_back( 257 );
	mMultiplicityCut[1].push_back( 232 );  mMultiplicityCut[2].push_back( 211 );
	mMultiplicityCut[1].push_back( 193 );  mMultiplicityCut[2].push_back( 171 );
	mMultiplicityCut[1].push_back( 159 );  mMultiplicityCut[2].push_back( 137 );
	mMultiplicityCut[1].push_back( 129 );  mMultiplicityCut[2].push_back( 108 );
	mMultiplicityCut[1].push_back( 104 );  mMultiplicityCut[2].push_back(  83 );
	mMultiplicityCut[1].push_back(  82 );  mMultiplicityCut[2].push_back(  63 );
	mMultiplicityCut[1].push_back(  64 );  mMultiplicityCut[2].push_back(  47 );
	mMultiplicityCut[1].push_back(  49 );  mMultiplicityCut[2].push_back(  34 );
	mMultiplicityCut[1].push_back(  37 );  mMultiplicityCut[2].push_back(  24 );
	mMultiplicityCut[1].push_back(  27 );  mMultiplicityCut[2].push_back(  16 );
	mMultiplicityCut[1].push_back(  19 );  mMultiplicityCut[2].push_back(  11 );
	mMultiplicityCut[1].push_back(  13 );  mMultiplicityCut[2].push_back(   7 );

	// Set same centrality bins 
	for(UInt_t ic=0; ic<mCentralityMin[0].size(); ic++){
		for(UInt_t im=1; im<mNMode; im++){
			mCentralityMin[im].push_back( mCentralityMin[0][ic] );
			mCentralityMax[im].push_back( mCentralityMax[0][ic] );
		}
	}
}

//____________________________________________________________________________________________________
void StCentrality::Init_PbPb2760GeV()
{
	/// Initialize Pb + Pb collisiona at 2760 GeV
	// Copy from Au+Au 200 GeV
	//----------------------------------------------------------------------------------------------------
	mNpp = 2.43 ;
	mK   = 2.00 ;
	mX   = 0.13 ;
	mEfficiency  = 0.14 ; // Multiplicity dependent efficiency
	mTriggerBias = 1.00 ;

	// Define multiplicity cut from central to peripheral
	mMultiplicityCut[0].push_back( 441 ); mCentralityMin[0].push_back(  0.0 );  mCentralityMax[0].push_back(  5.0 );
	mMultiplicityCut[0].push_back( 375 ); mCentralityMin[0].push_back(  5.0 );  mCentralityMax[0].push_back( 10.0 );
	mMultiplicityCut[0].push_back( 317 ); mCentralityMin[0].push_back( 10.0 );  mCentralityMax[0].push_back( 15.0 );
	mMultiplicityCut[0].push_back( 266 ); mCentralityMin[0].push_back( 15.0 );  mCentralityMax[0].push_back( 20.0 );
	mMultiplicityCut[0].push_back( 221 ); mCentralityMin[0].push_back( 20.0 );  mCentralityMax[0].push_back( 25.0 );
	mMultiplicityCut[0].push_back( 182 ); mCentralityMin[0].push_back( 25.0 );  mCentralityMax[0].push_back( 30.0 );
	mMultiplicityCut[0].push_back( 148 ); mCentralityMin[0].push_back( 30.0 );  mCentralityMax[0].push_back( 35.0 );
	mMultiplicityCut[0].push_back( 118 ); mCentralityMin[0].push_back( 35.0 );  mCentralityMax[0].push_back( 40.0 );
	mMultiplicityCut[0].push_back(  93 ); mCentralityMin[0].push_back( 40.0 );  mCentralityMax[0].push_back( 45.0 );
	mMultiplicityCut[0].push_back(  72 ); mCentralityMin[0].push_back( 45.0 );  mCentralityMax[0].push_back( 50.0 );
	mMultiplicityCut[0].push_back(  55 ); mCentralityMin[0].push_back( 50.0 );  mCentralityMax[0].push_back( 55.0 );
	mMultiplicityCut[0].push_back(  41 ); mCentralityMin[0].push_back( 55.0 );  mCentralityMax[0].push_back( 60.0 );
	mMultiplicityCut[0].push_back(  30 ); mCentralityMin[0].push_back( 60.0 );  mCentralityMax[0].push_back( 65.0 );
	mMultiplicityCut[0].push_back(  21 ); mCentralityMin[0].push_back( 65.0 );  mCentralityMax[0].push_back( 70.0 );
	mMultiplicityCut[0].push_back(  15 ); mCentralityMin[0].push_back( 70.0 );  mCentralityMax[0].push_back( 75.0 );
	mMultiplicityCut[0].push_back(  10 ); mCentralityMin[0].push_back( 75.0 );  mCentralityMax[0].push_back( 80.0 );

	// -5%                                 +5%
	mMultiplicityCut[1].push_back( 445 );  mMultiplicityCut[2].push_back( 438 );
	mMultiplicityCut[1].push_back( 382 );  mMultiplicityCut[2].push_back( 369 );
	mMultiplicityCut[1].push_back( 325 );  mMultiplicityCut[2].push_back( 309 );
	mMultiplicityCut[1].push_back( 276 );  mMultiplicityCut[2].push_back( 257 );
	mMultiplicityCut[1].push_back( 232 );  mMultiplicityCut[2].push_back( 211 );
	mMultiplicityCut[1].push_back( 193 );  mMultiplicityCut[2].push_back( 171 );
	mMultiplicityCut[1].push_back( 159 );  mMultiplicityCut[2].push_back( 137 );
	mMultiplicityCut[1].push_back( 129 );  mMultiplicityCut[2].push_back( 108 );
	mMultiplicityCut[1].push_back( 104 );  mMultiplicityCut[2].push_back(  83 );
	mMultiplicityCut[1].push_back(  82 );  mMultiplicityCut[2].push_back(  63 );
	mMultiplicityCut[1].push_back(  64 );  mMultiplicityCut[2].push_back(  47 );
	mMultiplicityCut[1].push_back(  49 );  mMultiplicityCut[2].push_back(  34 );
	mMultiplicityCut[1].push_back(  37 );  mMultiplicityCut[2].push_back(  24 );
	mMultiplicityCut[1].push_back(  27 );  mMultiplicityCut[2].push_back(  16 );
	mMultiplicityCut[1].push_back(  19 );  mMultiplicityCut[2].push_back(  11 );
	mMultiplicityCut[1].push_back(  13 );  mMultiplicityCut[2].push_back(   7 );

	// Set same centrality bins 
	for(UInt_t ic=0; ic<mCentralityMin[0].size(); ic++){
		for(UInt_t im=1; im<mNMode; im++){
			mCentralityMin[im].push_back( mCentralityMin[0][ic] );
			mCentralityMax[im].push_back( mCentralityMax[0][ic] );
		}
	}
}

//____________________________________________________________________________________________________
Double_t StCentrality::GetReweighting(const UInt_t multiplicity) const
{
	// Get re-weighting correction
	// parameters depend on the incident energies

	// Check parameters. If all parameters are 0, return 1.0
	if( mParReweighting[0] == 0.0 && mParReweighting[1] == 0.0 ){
		Warning("StCentrality::GetReweighting", "No re-weighting correction implemented. return 1");
		return 1.0 ;
	}

	const Double_t x[] = {multiplicity};

	return Reweighting(x, mParReweighting) ;
}


