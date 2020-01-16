//------------------------------------------------------------------------------
// $Id: StRefMultCorr.cxx,v 1.5 2020/01/16 23:53:28 tnonaka Exp $
// $Log: StRefMultCorr.cxx,v $
// Revision 1.5  2020/01/16 23:53:28  tnonaka
// gRefmult for Run14 and Run16 added
//
// Revision 1.4  2019/10/03 15:42:26  tnonaka
// Some functions for pile-up rejection and trigger inefficiency corrections are added
//
// Revision 1.3  2019/07/11 03:31:45  tnonaka
// Some functions are added/replaced to read header files and to implement Vz dependent centrality definitions for 54.4 GeV RefMult.
//
//
//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
// StRefMultCor package has been moved to StRoot/StRefMultCorr 2019/02
//>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
//
// Revision 1.14  2015/05/22 06:52:05  hmasui
// Add grefmult for Run14 Au+Au 200 GeV
//
// Revision 1.13  2013/05/10 18:33:33  hmasui
// Add TOF tray mult, preliminary update for Run12 U+U
//
// Revision 1.12  2012/05/19 00:48:20  hmasui
// Update refmult3
//
// Revision 1.11  2012/05/14 00:40:25  hmasui
// Exit code if no valid run number found. Fix the initialization of mParameterIndex
//
// Revision 1.10  2012/05/09 22:26:46  hmasui
// Commented out option use in the print() function
//
// Revision 1.9  2012/05/08 03:19:49  hmasui
// Move parameters to Centrality_def_refmult.txt
//
// Revision 1.8  2012/04/23 21:29:37  hmasui
// Added isBadRun() function for outlier rejection, getBeginRun() and getEndRun() to obtain the run range for a given (energy,year)
//
// Revision 1.7  2011/11/30 00:25:07  hmasui
// Additional check for ifstream to avoid reading one extra line from input file
//
// Revision 1.6  2011/11/08 19:11:05  hmasui
// Add luminosity corrections for 200 GeV
//
// Revision 1.5  2011/10/11 19:35:20  hmasui
// Fix typo. Add z-vertex check in getWeight() function
//
// Revision 1.4  2011/10/10 21:30:37  hmasui
// Replaced hard coded parameters for z-vertex and weight corrections by input parameters from text file
//
//
// Revision 1.3  2011/08/12 20:28:07  hmasui
// Avoid varying corrected refmult in the same event by random number
//
// Revision 1.2  2011/08/11 23:51:10  hmasui
// Suppress cout in the setParameterIndex function. Use TError for error messages.
//
// Revision 1.1  2011/08/11 18:38:28  hmasui
// First version of Refmult correction class
//
//------------------------------------------------------------------------------

#include <algorithm>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>

#include "StRefMultCorr.h"
#include "TError.h"
#include "TRandom.h"
#include "TMath.h"

#include "Param.h"


ClassImp(StRefMultCorr)

	using namespace std ;

	namespace {
		typedef pair<Double_t, Int_t> keys;
	}

//______________________________________________________________________________
// Default constructor
	StRefMultCorr::StRefMultCorr(const TString name, const TString subname, const TString libname)
: mName(name), mSubName(subname), mLibName(libname)
{
	mRefMult = 0 ;
	mVz = -9999. ;
	mRefMult_corr = -1.0 ;

	cout << mSubName.Data() <<"  "<< mLibName.Data() << endl;

	// Clear all data members
	clear() ;

	readHeaderFile() ;
	readBadRunsFromHeaderFile() ;
}

//______________________________________________________________________________
// Default destructor
StRefMultCorr::~StRefMultCorr()
{
}

//______________________________________________________________________________
Int_t StRefMultCorr::getBeginRun(const Double_t energy, const Int_t year)
{
	keys key(std::make_pair(energy, year));

	// Make sure key exists
	multimap<keys, Int_t>::iterator iterCheck = mBeginRun.find(key);
	if ( iterCheck == mBeginRun.end() ) {
		Error("StRefMultCorr::getBeginRun", "can't find energy = %1.1f, year = %d", energy, year);
		return -1;
	}

	pair<multimap<keys, Int_t>::iterator, multimap<keys, Int_t>::iterator> iterRange = mBeginRun.equal_range(key);

	return (*(iterRange.first)).second ;
}

//______________________________________________________________________________
Int_t StRefMultCorr::getEndRun(const Double_t energy, const Int_t year)
{
	keys key(std::make_pair(energy, year));

	// Make sure key exists
	multimap<keys, Int_t>::iterator iterCheck = mEndRun.find(key);
	if ( iterCheck == mEndRun.end() ) {
		Error("StRefMultCorr::getEndRun", "can't find energy = %1.1f, year = %d", energy, year);
		return -1;
	}

	pair<multimap<keys, Int_t>::iterator, multimap<keys, Int_t>::iterator> iterRange = mEndRun.equal_range(key);
	multimap<keys, Int_t>::iterator iter = iterRange.second ;
	iter--;

	return (*iter).second ;
}

//______________________________________________________________________________
void StRefMultCorr::clear()
{
	// Clear all arrays, and set parameter index = -1

	mYear.clear() ;
	mStart_runId.clear() ;
	mStop_runId.clear() ;
	mStart_zvertex.clear() ;
	mStop_zvertex.clear() ;
	mNormalize_stop.clear() ;

	for(Int_t i=0; i<mNCentrality; i++) 
	{
		mCentrality_bins[i].clear() ;
	}
	
	mParameterIndex = -1 ;

	for(Int_t i=0;i<mNPar_z_vertex;i++) 
	{
		mPar_z_vertex[i].clear() ;
	}

	for(Int_t i=0;i<mNPar_weight;i++) 
	{
		mPar_weight[i].clear();
	}

	for(Int_t i=0;i<mNPar_luminosity;i++) 
	{
		mPar_luminosity[i].clear();
	}

	mBeginRun.clear() ;
	mEndRun.clear() ;
	mBadRun.clear() ;

	mnVzBinForWeight = 0 ;
	mVzEdgeForWeight.clear();
	mgRefMultTriggerCorrDiffVzScaleRatio.clear() ;
}


//______________________________________________________________________________
Bool_t StRefMultCorr::isBadRun(const Int_t RunId)
{
	// Return true if a given run id is bad run
	vector<Int_t>::iterator iter = std::find(mBadRun.begin(), mBadRun.end(), RunId);
#if 0
	if ( iter != mBadRun.end() ) {
		// QA
		cout << "StRefMultCorr::isBadRun  Find bad run = " << (*iter) << endl;
	}
#endif

	return ( iter != mBadRun.end() ) ;
}

//______________________________________________________________________________
void StRefMultCorr::initEvent(const UShort_t RefMult, const Double_t z, const Double_t zdcCoincidenceRate)
{
	// Set refmult, vz and corrected refmult if current (refmult,vz) are different from inputs
	// User must call this function event-by-event before 
	// calling any other public functions
	if ( mRefMult != RefMult || mVz != z || mZdcCoincidenceRate != zdcCoincidenceRate ) 
	{
		mRefMult            = RefMult ;
		mVz                 = z ;
		mZdcCoincidenceRate = zdcCoincidenceRate ;
		mRefMult_corr       = getRefMultCorr(mRefMult, mVz, mZdcCoincidenceRate) ;
	}
}

//______________________________________________________________________________
Bool_t StRefMultCorr::passnTofMatchRefmultCut(Double_t refmult, Double_t ntofmatch)
{
	if( mParameterIndex>=30 && mParameterIndex<=35 )//Run18 27 GeV MB, cut curve parameters are from fit
	{
		const Double_t min = 4.0;
		const Double_t max = 5.0;

		if(ntofmatch<=2)  return false;

		const double a0 = -0.704787625248525;
		const double a1 = 0.99026234637141;
		const double a2 = -0.000680713101607504;
		const double a3 = 2.77035215460421e-06;
		const double a4 = -4.04096185674966e-09;
		const double b0 = 2.52126730672253;
		const double b1 = 0.128066911940844;
		const double b2 = -0.000538959206681944;
		const double b3 = 1.21531743671716e-06;
		const double b4 = -1.01886685404478e-09;
		const double c0 = 4.79427731664144;
		const double c1 = 0.187601372159186;
		const double c2 = -0.000849856673886957;
		const double c3 = 1.9359155975421e-06;
		const double c4 = -1.61214724626684e-09;

		double refmultCenter = a0+a1*(ntofmatch)+a2*pow(ntofmatch,2)+a3*pow(ntofmatch,3)+a4*pow(ntofmatch,4);
		double refmultLower  = b0+b1*(ntofmatch)+b2*pow(ntofmatch,2)+b3*pow(ntofmatch,3)+b4*pow(ntofmatch,4);
		double refmultUpper  = c0+c1*(ntofmatch)+c2*pow(ntofmatch,2)+c3*pow(ntofmatch,3)+c4*pow(ntofmatch,4);

		double refmultCutMin = refmultCenter - min*refmultLower;
		double refmultCutMax = refmultCenter + max*refmultUpper;

		if( refmult>refmultCutMax || refmult<refmultCutMin ) return false;

		return true;
	}
	else
	{
		return true;
	}
}


//______________________________________________________________________________
Bool_t StRefMultCorr::isIndexOk() const
{
	// mParameterIndex not initialized (-1)
	if ( mParameterIndex == -1 ) 
	{
		Error("StRefMultCorr::isIndexOk", "mParameterIndex = -1. Call init(const Int_t RunId) function to initialize centrality bins, corrections");
		Error("StRefMultCorr::isIndexOk", "mParameterIndex = -1. or use valid run numbers defined in Centrality_def_%s.txt", mName.Data());
		Error("StRefMultCorr::isIndexOk", "mParameterIndex = -1. exit");
		cout << endl;
		// Stop the process if invalid run number found
		exit(0);
	}

	// Out of bounds
	if ( mParameterIndex >= (Int_t)mStart_runId.size() ) 
	{
		Error("StRefMultCorr::isIndexOk",
				Form("mParameterIndex = %d > max number of parameter set = %d. Make sure you put correct index for this energy",
					mParameterIndex, mStart_runId.size()));
		return kFALSE ;
	}

	return kTRUE ;
}

//______________________________________________________________________________
Bool_t StRefMultCorr::isZvertexOk() const
{
	// Primary z-vertex check
	return ( mVz > mStart_zvertex[mParameterIndex] && mVz < mStop_zvertex[mParameterIndex] ) ;
}

//______________________________________________________________________________
Bool_t StRefMultCorr::isRefMultOk() const
{
	// Invalid index
	if ( !isIndexOk() ) return kFALSE ;

	// select 0-80%
	return (mRefMult_corr > mCentrality_bins[0][mParameterIndex] && mRefMult_corr < mCentrality_bins[mNCentrality][mParameterIndex]);
}

//______________________________________________________________________________
Bool_t StRefMultCorr::isCentralityOk(const Int_t icent) const
{
	// Invalid centrality id
	if ( icent < -1 || icent >= mNCentrality+1 ) return kFALSE ;

	// Invalid index
	if ( !isIndexOk() ) return kFALSE ;

	// Special case
	// 1. 80-100% for icent=-1
	if ( icent == -1 ) return (mRefMult_corr <= mCentrality_bins[0][mParameterIndex]);

	// 2. icent = mNCentrality
	if ( icent == mNCentrality ) return (mRefMult_corr <= mCentrality_bins[mNCentrality][mParameterIndex]);

	const Bool_t isOK = (mRefMult_corr > mCentrality_bins[icent][mParameterIndex] && mRefMult_corr <= mCentrality_bins[icent+1][mParameterIndex]);
	//  if(isOK)
	//  {
	//    cout << "StRefMultCorr::isCentralityOk  refmultcorr = " << mRefMult_corr
	//      << "  min. bin = " << mCentrality_bins[icent][mParameterIndex]
	//      << "  max. bin = " << mCentrality_bins[icent+1][mParameterIndex]
	//      << endl;
	//  }
	return isOK ;
}

//______________________________________________________________________________
void StRefMultCorr::init(const Int_t RunId)
{
	// Reset mParameterIndex
	mParameterIndex = -1 ;

	// call setParameterIndex
	setParameterIndex(RunId) ;
}

//______________________________________________________________________________
Int_t StRefMultCorr::setParameterIndex(const Int_t RunId)
{
	// Determine the corresponding parameter set for the input RunId
	for(UInt_t npar = 0; npar < mStart_runId.size(); npar++)
	{
		if(RunId >= mStart_runId[npar] && RunId <= mStop_runId[npar])
		{
			mParameterIndex = npar ;
			//      cout << "StRefMultCorr::setParameterIndex  Parameter set = " << mParameterIndex << " for RUN " << RunId << endl;
			break ;
		}
	}
	
	// Multiple parameters/definitions for Run14/16 data
	// Set mParameterIndex by hand
	// For Run14 P16id production
	// For Run16 P16ij production
	if ( mName.CompareTo("grefmult", TString::kIgnoreCase) == 0 ){ 
		if ( mSubName.CompareTo("Run14_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 ){
			if(RunId/1000000==15 && mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0 ){
				mParameterIndex = 0;
				if(mVzEdgeForWeight.empty()){ 
				setVzForWeight( nWeightVzBin_Run14_P16id, 
						WeightVzEdgeMin_Run14_P16id,
						WeightVzEdgeMax_Run14_P16id );
				}
				if(mgRefMultTriggerCorrDiffVzScaleRatio.empty()){ 
				readScaleForWeight( nWeightgRefmultBin_Run14_P16id, 
						weight_VpdMB5ToVpdMB30_Run14_P16id );
				}
			}
		}
		else if ( mSubName.CompareTo("Run16_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 ){
			if(mLibName.CompareTo("P16ij", TString::kIgnoreCase) == 0){
				// prod.1
				if(RunId/1000>=17039&&RunId/1000<=17130){
					mParameterIndex = 4;
					if(mVzEdgeForWeight.empty()) 
					setVzForWeight( nWeightVzBin_Run16_P16ij_prod1, 
							WeightVzEdgeMin_Run16_P16ij_prod1,
							WeightVzEdgeMax_Run16_P16ij_prod1 );
					if(mgRefMultTriggerCorrDiffVzScaleRatio.empty()) 
					readScaleForWeight( nWeightgRefmultBin_Run16_P16ij_prod1, 
							weight_VpdMB5ToVpdMBnoVtx_Run16_P16ij_prod1 );
				}
				// prod.2
				else if(RunId/1000>=17169&&RunId/1000<=17179){
					mParameterIndex = 5;
					if(mVzEdgeForWeight.empty()) 
					setVzForWeight( nWeightVzBin_Run16_P16ij_prod2, 
							WeightVzEdgeMin_Run16_P16ij_prod2,
							WeightVzEdgeMax_Run16_P16ij_prod2 );
					if(mgRefMultTriggerCorrDiffVzScaleRatio.empty()) 
					readScaleForWeight( nWeightgRefmultBin_Run16_P16ij_prod2, 
							weight_VpdMB5ToVpdMBnoVtx_Run16_P16ij_prod2 );
				}
			}
		}
		else if ( mSubName.CompareTo("Run14_AuAu200_VpdMB30", TString::kIgnoreCase) == 0 ){
			if(mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0){
				mParameterIndex = 1;
			}
		}
		else if ( mSubName.CompareTo("Run14_AuAu200_VpdMBnoVtx_LowMid", TString::kIgnoreCase) == 0 ){
			if(mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0){
				mParameterIndex = 2;
			}
		}
		else if ( mSubName.CompareTo("Run14_AuAu200_VpdMBnoVtx_High", TString::kIgnoreCase) == 0 ){
			if(mLibName.CompareTo("P15ic", TString::kIgnoreCase) == 0){
				mParameterIndex = 3;
			}
		}
		else if ( mSubName.CompareTo("Run16_AuAu200_VpdMBnoVtx", TString::kIgnoreCase) == 0 ){
			if(mLibName.CompareTo("P16ij", TString::kIgnoreCase) == 0){
				mParameterIndex = 6;
			}
		}
		else{
			mParameterIndex = -1;
		}
	}	

//	cout <<"mParameterIndex = "<< mParameterIndex << endl;

	if(mParameterIndex == -1)
	{
		Error("StRefMultCorr::setParameterIndex", "Parameter set does not exist for RUN %d", RunId);
	}
	//else cout << "Parameter set = " << npar_set << endl;

	return mParameterIndex ;
}

//______________________________________________________________________________
Double_t StRefMultCorr::getRefMultCorr() const
{
	// Call initEvent() first
	return mRefMult_corr ;
}

//______________________________________________________________________________
Double_t StRefMultCorr::getRefMultCorr(
		const UShort_t RefMult, 
		const Double_t z,
		const Double_t zdcCoincidenceRate, 
		const UInt_t flag
		) const
{
	// Apply correction if parameter index & z-vertex are ok
	if (!isIndexOk() || !isZvertexOk()) return RefMult ;

	// Correction function for RefMult, takes into account z_vertex dependence

	// Luminosity corrections
	// 200 GeV only. correction = 1 for all the other energies for BES-I
	// the above statement may not true for BES-II, since the luminosity is much higher than BES-I, add by Zaochen
	// better to check the <Refmult> vs ZDCX to see whether they are flat or not, add by Zaochen
	const Double_t par0_lum = mPar_luminosity[0][mParameterIndex] ;
	const Double_t par1_lum = mPar_luminosity[1][mParameterIndex] ;
	Double_t correction_luminosity = (par0_lum==0.000) ? 1.0 : 1.0/(1.0 + par1_lum/par0_lum*zdcCoincidenceRate/1000.);

	// from Run14, P16id, for VpdMB5/VPDMB30/VPDMB-noVtx, use refMult at ZdcX=30, other is at ZdcX=0;  
	// -->changed by xlchen@lbl.gov, Run16 ~ 50kHz
        if( 
	   ( mSubName.CompareTo("Run14_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0 )
	|| ( mSubName.CompareTo("Run14_AuAu200_VpdMB30", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0 )
	|| ( mSubName.CompareTo("Run14_AuAu200_VpdMBnoVtx_LowMid", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P16id", TString::kIgnoreCase) == 0 )
	|| ( mSubName.CompareTo("Run14_AuAu200_VpdMBnoVtx_High", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P15ic", TString::kIgnoreCase) == 0 )
	|| ( mSubName.CompareTo("Run16_AuAu200_VpdMB5", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P16ij", TString::kIgnoreCase) == 0 )
	|| ( mSubName.CompareTo("Run16_AuAu200_VpdMBnoVtx", TString::kIgnoreCase) == 0 && mLibName.CompareTo("P16ij", TString::kIgnoreCase) == 0 )
	     ) {
            float zdcmean = 0;
            if(mYear[mParameterIndex] == 2014) zdcmean = 30.;
            if(mYear[mParameterIndex] == 2016) zdcmean = 50.;
            correction_luminosity = (par0_lum==0.0) ? correction_luminosity : correction_luminosity*(par0_lum+par1_lum*zdcmean)/par0_lum; 
        }


	// par0 to par5 define the parameters of a polynomial to parametrize z_vertex dependence of RefMult
	const Double_t par0 = mPar_z_vertex[0][mParameterIndex];
	const Double_t par1 = mPar_z_vertex[1][mParameterIndex];
	const Double_t par2 = mPar_z_vertex[2][mParameterIndex];
	const Double_t par3 = mPar_z_vertex[3][mParameterIndex];
	const Double_t par4 = mPar_z_vertex[4][mParameterIndex];
	const Double_t par5 = mPar_z_vertex[5][mParameterIndex];
	const Double_t par6 = mPar_z_vertex[6][mParameterIndex];
	const Double_t par7 = mPar_z_vertex[7][mParameterIndex]; // this parameter is usually 0, it takes care for an additional efficiency, usually difference between phase A and phase B parameter 0

	const Double_t  RefMult_ref = par0; // Reference mean RefMult at z=0
	const Double_t  RefMult_z   = par0 + par1*z + par2*z*z + par3*z*z*z + par4*z*z*z*z + par5*z*z*z*z*z + par6*z*z*z*z*z*z; // Parametrization of mean RefMult vs. z_vertex position
	Double_t  Hovno = 1.0; // Correction factor for RefMult, takes into account z_vertex dependence

	if(RefMult_z > 0.0)
	{
		Hovno = (RefMult_ref + par7)/RefMult_z;
	}

	// random sampling over bin width -> avoid peak structures in corrected distribution
	Double_t RefMult_d     = -9999.;
	if( mParameterIndex>=30 && mParameterIndex<=35 )
	{
		RefMult_d = (Double_t)(RefMult)+gRandom->Rndm()-0.5;
	}
	else
	{
		RefMult_d = (Double_t)(RefMult)+gRandom->Rndm();
	}
	
	Double_t RefMult_corr  = -9999. ;
	switch ( flag ) 
	{
		case 0: return RefMult_d*correction_luminosity;
		case 1: return RefMult_d*Hovno;
		case 2: return RefMult_d*Hovno*correction_luminosity;
		default:
				{
					Error("StRefMultCorr::getRefMultCorr", "invalid flag, flag=%d, should be 0,1 or 2", flag);
					return -9999.;
				}
	}
	//  cout << "Input RefMult = " << RefMult << ", input z = " << z << ", RefMult_corr = " << RefMult_corr << endl;
	return RefMult_corr ;
}

//______________________________________________________________________________
void StRefMultCorr::readScaleForWeight(const Char_t* input)
{
	ifstream fin(input) ;
	if(!fin) 
	{
		Error("StRefMultCorr::readScaleForWeight", "can't open %s", input);
		return;
	}

	// Users must set the vz bin size by setVzForWeight() (see below)
	if(mnVzBinForWeight==0) 
	{
		Error("StRefMultCorr::readScaleForWeight",
				"Please call setVzForWeight() to set vz bin size");
		return;
	}

	// Do not allow multiple calls
	if(!mgRefMultTriggerCorrDiffVzScaleRatio.empty()) 
	{
		Error("StRefMultCorr::readScaleForWeight",
				"scale factor has already set in the array");
		return;
	}

	cout << "StRefMultCorr::readScaleForWeight  Read scale factor ..."
		<< flush;
	
	while(fin) 
	{
		Double_t scale[mnVzBinForWeight] ;
		for(Int_t i=0; i<mnVzBinForWeight; i++) 
		{
			fin >> scale[i] ;
		}
		
		if(fin.eof()) break ;

		for(Int_t i=0; i<mnVzBinForWeight; i++) 
		{
			mgRefMultTriggerCorrDiffVzScaleRatio.push_back(scale[i]);
		}
	}
	cout << " [OK]" << endl;
}

// NEW version to read Vz dependent weights from header
// Implemented inside StRefMultCorr::setParameterIndex(RunId)
//______________________________________________________________________________
void StRefMultCorr::readScaleForWeight(const Int_t nRefmultbin, const Double_t *weight)
{

	// Users must set the vz bin size by setVzForWeight() (see below)
	if(mnVzBinForWeight==0) 
	{
		Error("StRefMultCorr::readScaleForWeight",
				"Please call setVzForWeight() to set vz bin size");
		return;
	}

	// Do not allow multiple calls
	if(!mgRefMultTriggerCorrDiffVzScaleRatio.empty()) 
	{
		Error("StRefMultCorr::readScaleForWeight",
				"scale factor has already set in the array");
		return;
	}

	cout << "StRefMultCorr::readScaleForWeight  Read scale factor ..."
		<< flush;
	

	for(Int_t i=0; i<nRefmultbin*mnVzBinForWeight; i++){
		mgRefMultTriggerCorrDiffVzScaleRatio.push_back(weight[i]);
	}


	cout << " [OK]" << endl;
}


// In NEW version, setVzForWeight() is implemented inside StRefMultCorr::setParameterIndex(RunId)
// It does not need to be called by users.
//______________________________________________________________________________
void StRefMultCorr::setVzForWeight(const Int_t nbin, const Double_t min, const Double_t max)
{
	// Do not allow multiple calls
	if(!mVzEdgeForWeight.empty()) 
	{
		Error("StRefMultCorr::setVzForWeight",
				"z-vertex range for weight has already been defined");
		return;
	}

	mnVzBinForWeight = nbin ;
	// calculate increment size
	const Double_t step = (max-min)/(Double_t)nbin;
	
	for(Int_t i=0; i<mnVzBinForWeight+1; i++) 
	{
		mVzEdgeForWeight.push_back( min + step*i );
	}
	
	// Debug
	for(Int_t i=0; i<mnVzBinForWeight; i++) 
	{
		cout << i << " " << step << " " << mVzEdgeForWeight[i]
			<< ", " << mVzEdgeForWeight[i+1] << endl;
	}
}

//______________________________________________________________________________
Double_t StRefMultCorr::getScaleForWeight() const
{
	// Special scale factor for global refmult in Run14 (Run16)
	// to account for the relative difference of VPDMB5 w.r.t VPDMB30 (VPDMBnoVtx) 

	// return 1 if mgRefMultTriggerCorrDiffVzScaleRatio array is empty
	if(mgRefMultTriggerCorrDiffVzScaleRatio.empty()) return 1.0 ;

	//  const Int_t nVzBins =6;
	//  Double_t VzEdge[nVzBins+1]={-6., -4., -2., 0., 2., 4., 6.};

	Double_t VPD5weight=1.0;
	for(Int_t j=0;j<mnVzBinForWeight;j++) 
	{
		if(mVz>mVzEdgeForWeight[j] && mVz<=mVzEdgeForWeight[j+1]) 
		{
			/*
			//refMultbin=mgRefMultTriggerCorrDiffVzScaleRatio_2[j]->FindBin(mRefMult_corr+1e-6);
			//VPD5weight=mgRefMultTriggerCorrDiffVzScaleRatio_2[j]->GetBinContent(refMultbin);
			const Int_t refMultbin=static_cast<Int_t>(mRefMult_corr);
			//VPD5weight=mgRefMultTriggerCorrDiffVzScaleRatio[j][refMultbin];
			VPD5weight=mgRefMultTriggerCorrDiffVzScaleRatio[refMultbin*mnVzBinForWeight + j];
			const Double_t tmpContent=VPD5weight;
			if(tmpContent==0 || (mRefMult_corr>500 && tmpContent<=0.65)) VPD5weight=1.15;//Just because the value of the weight is around 1.15
			if(mRefMult_corr>500 && tmpContent>=1.35) VPD5weight=1.15;//Remove those Too large weight factor,gRefmult>500
			// this weight and reweight should be careful, after reweight(most peripheral),Then weight(whole range)
			*/

			const Int_t refMultbin=static_cast<Int_t>(mRefMult_corr);
			VPD5weight=mgRefMultTriggerCorrDiffVzScaleRatio[refMultbin*mnVzBinForWeight + j];
			const Double_t tmpContent=VPD5weight;
			// 1) Ratios fluctuate too much at very high gRefmult due to low statistics
			// 2) Avoid some events with too high weight
			if(mRefMult_corr>550 && (tmpContent>3.0||tmpContent<0.3)) VPD5weight=1.0;
			// this weight and reweight should be careful, after reweight(most peripheral),Then weight(whole range)
		}
	}

	return 1.0/VPD5weight;
}

//______________________________________________________________________________
Double_t StRefMultCorr::getShapeWeight_SubVz2Center()
{
	if( mParameterIndex>=30 && mParameterIndex<=35 )//Run18 27 GeV MB
	{
		if(mRefMult_corr>=500.) return 1.0;// almost no Refmult>500 for this collision energy
		
		Int_t iRunPdIndex = mParameterIndex-30;
		Int_t iVzBinIndex = getVzWindowForVzDepCentDef();
		Int_t iRefmultBin = (Int_t)(mRefMult_corr/2.); //find the refmult bin matching to the Parameter bin, if binWidth=2
		//Int_t iRefmultBin = (Int_t)(mRefMult_corr);  //find the refmult bin matching to the Parameter bin, if binWidth=1
		
		if(iRunPdIndex<0||iRunPdIndex>5)  return 1.0;
		if(iVzBinIndex<0||iVzBinIndex>13) return 1.0;
		
		//----------------------------------------------------------
		//load the ShapeReweight factors in given RunPd and VzBin
		//----------------------------------------------------------
		vector<string> sParam_ShapeWeight = StringSplit(getParamX_ShapeWeight(iRunPdIndex,iVzBinIndex),',');
		if( iRefmultBin>=(Int_t)sParam_ShapeWeight.size() ) 
		{
			cout<<"ERROR: sParam_ShapeWeight is out of ranges!!!!!"<<endl;
			return 1.0;
		}
	
		//cout<<"sParam_ShapeWeight.size(): "<<sParam_ShapeWeight.size()<<endl;
		//for(UInt_t is=0; is<sParam_ShapeWeight.size(); is++) cout<<"sParam_ShapeWeight[is]: "<<sParam_ShapeWeight[is]<<endl;

		Double_t ShapeReweight = 1.0;

		Double_t tem_ShapeReweight = stod( sParam_ShapeWeight[iRefmultBin] );
		//prevent the crazy numbers for the large fluctuations
		if( tem_ShapeReweight<0.1 )
		{
			ShapeReweight = 0.1;
		}
		else if(tem_ShapeReweight>10)
		{
			ShapeReweight = 10.;
		}
		else if(tem_ShapeReweight>=0.1&&tem_ShapeReweight<=10.)
		{
			ShapeReweight = tem_ShapeReweight;
		}
		
		return (1./ShapeReweight);
	}
	else
	{
		return 1.0;
	}
}

//______________________________________________________________________________
Double_t StRefMultCorr::getWeight() //const
{
	Double_t Weight = 1.0;

	// Invalid index
	if( !isIndexOk() )   return Weight ;

	// Invalid z-vertex
	if( !isZvertexOk() ) return Weight ;

	const Double_t par0 =   mPar_weight[0][mParameterIndex];
	const Double_t par1 =   mPar_weight[1][mParameterIndex];
	const Double_t par2 =   mPar_weight[2][mParameterIndex];
	const Double_t par3 =   mPar_weight[3][mParameterIndex];
	const Double_t par4 =   mPar_weight[4][mParameterIndex];
	const Double_t A    =   mPar_weight[5][mParameterIndex];
	const Double_t par6 =   mPar_weight[6][mParameterIndex];//Add by guannan for run14
	const Double_t par7 =   mPar_weight[7][mParameterIndex];//Add by guannan for run14

	// Additional z-vetex dependent correction
	//const Double_t A = ((1.27/1.21))/(30.0*30.0); // Don't ask...
	//const Double_t A = (0.05/0.21)/(30.0*30.0); // Don't ask...
	//cout<<"--------------------"<<endl;
	//cout<<"par0: "<<par0<<endl;
	//cout<<"par1: "<<par1<<endl;
	//cout<<"par2: "<<par2<<endl;
	//cout<<"par3: "<<par3<<endl;
	//cout<<"par4: "<<par4<<endl;
	//cout<<"--------------------"<<endl;

	if(
			isRefMultOk() // 0-80%
			&& mRefMult_corr < mNormalize_stop[mParameterIndex] // re-weighting only apply up to normalization point
			&& mRefMult_corr != -(par3/par2) // avoid denominator = 0
	  )
	{
		Weight = 
			par0 
			+ par1/(par2*mRefMult_corr + par3) 
			+ par4*(par2*mRefMult_corr + par3) 
			+ par6/TMath::Power(par2*mRefMult_corr+par3 ,2) 
			+ par7*TMath::Power(par2*mRefMult_corr+par3 ,2); // Parametrization of MC/data RefMult ratio
		
		Weight = Weight + (Weight-1.0)*(A*mVz*mVz); // z-dependent weight correction
	}


	//------------for Run14 and Run16----------------
	// Special scale factor for global refmult depending on Vz window
	// for others, scale factor = 1
	const Double_t scale = getScaleForWeight() ; 
	Weight *= scale ;
	//------------for Run14 and Run16----------------

	//------------for Run18 27 GeV AuAu---------------
	const Double_t RefMult_ShapeWeight_SubVz2Center = getShapeWeight_SubVz2Center();
	//------------for Run18 27 GeV AuAu---------------
	
	//cout<<"RefMult_ShapeWeight_SubVz2Center: "<<setprecision(5)<<RefMult_ShapeWeight_SubVz2Center<<endl;

	Weight *= RefMult_ShapeWeight_SubVz2Center;

	return Weight ;
}
//______________________________________________________________________________
Int_t StRefMultCorr::getCentralityBin16() const
{
	Int_t CentBin16 = -1;

	// Invalid index
	if( !isIndexOk() ) return CentBin16 ;

	while( CentBin16 < mNCentrality && !isCentralityOk(CentBin16) )
	{
		CentBin16++;
	}

	// Vz dependent centrality definition for Vz<-27 and Vz>25
	// Run17 54.4 GeV, trigid = 580001
	if(mParameterIndex==28&&(mVz<-27||mVz>25))
	{
		CentBin16 = getCentralityBin16VzDep();
		// if(CentBin16==-1) cout <<"Vz="<< mVz <<" RefMult="<< mRefMult <<" RefMultCorr="<< mRefMult_corr << endl;
		// if(CentBin16==9999) cout << "Invalide number"<< endl; 
		// cout <<"Vz dependent centrality definition for Vz<-27 and Vz>25 ... Vz="<< mVz <<" RefMult="<< mRefMult <<" RefMultCorr="<< mRefMult_corr <<" iCent="<< CentBin16 << endl;
	}

	// return -1 if CentBin16 = 16 (very large refmult, refmult>5000)
	return (CentBin16==16) ? -1 : CentBin16;
}

//______________________________________________________________________________
Int_t StRefMultCorr::getCentralityBin9() const
{
	Int_t CentBin9 = -1;

	// Invalid index
	if ( !isIndexOk() ) return CentBin9 ;

	const Int_t  CentBin16 = getCentralityBin16(); // Centrality bin 16
	const Bool_t isCentralityOk = CentBin16 >= 0 && CentBin16 < mNCentrality ;

	// No centrality is defined
	if (!isCentralityOk) return CentBin9 ;

	// First handle the exceptions
	if(mRefMult_corr > mCentrality_bins[15][mParameterIndex] && mRefMult_corr <= mCentrality_bins[16][mParameterIndex])
	{
		CentBin9 = 8; // most central 5%
	}
	else if(mRefMult_corr > mCentrality_bins[14][mParameterIndex] && mRefMult_corr <= mCentrality_bins[15][mParameterIndex])
	{
		CentBin9 = 7; // most central 5-10%
	}
	else
	{
		CentBin9 = (Int_t)(0.5*CentBin16);
	}

	// Vz dependent centrality definition for Vz<-27 and Vz>25
	// Run17 54.4 GeV, trigid = 580001
	// cout << mParameterIndex << endl;
	if(mParameterIndex==28&&(mVz<-27||mVz>25))
	{
		CentBin9 = getCentralityBin9VzDep();
	}

	return CentBin9;
}

//______________________________________________________________________________
Int_t StRefMultCorr::getVzWindowForVzDepCentDef() const 
{
	Int_t iBinVz = -1;

	if(mParameterIndex==28)	//  54.4 GeV, RefMult, 580001
	{
		if(mVz>-30&&mVz<-29)      iBinVz = 0;
		else if(mVz>-29&&mVz<-27) iBinVz = 1;
		else if(mVz>25&&mVz<27)   iBinVz = 2;
		else if(mVz>27&&mVz<29)   iBinVz = 3;
		else if(mVz>29&&mVz<30)   iBinVz = 4;
		else iBinVz = -1;
	}
	else if(mParameterIndex>=30 &&mParameterIndex<=35)//Run18 27 GeV MB, 6 triggerIds
	{
		if(     mVz>=-70.&&mVz<-60.) iBinVz = 0;
		else if(mVz>=-60.&&mVz<-50.) iBinVz = 1;
		else if(mVz>=-50.&&mVz<-40.) iBinVz = 2;
		else if(mVz>=-40.&&mVz<-30.) iBinVz = 3;
		else if(mVz>=-30.&&mVz<-20.) iBinVz = 4;
		else if(mVz>=-20.&&mVz<-10.) iBinVz = 5;
		else if(mVz>=-10.&&mVz<0.0 ) iBinVz = 6;
		else if(mVz>=0.0 &&mVz<10. ) iBinVz = 7;
		else if(mVz>=10. &&mVz<20. ) iBinVz = 8;
		else if(mVz>=20. &&mVz<30. ) iBinVz = 9;
		else if(mVz>=30. &&mVz<40. ) iBinVz = 10;
		else if(mVz>=40. &&mVz<50. ) iBinVz = 11;
		else if(mVz>=50. &&mVz<60. ) iBinVz = 12;
		else if(mVz>=60. &&mVz<=70 ) iBinVz = 13;
		else iBinVz = -1;
	}
	else iBinVz = -1;
	
	return iBinVz;
}
//______________________________________________________________________________
Int_t StRefMultCorr::getCentralityBin9VzDep() const 
{
	const Int_t vzid = getVzWindowForVzDepCentDef();
	Int_t iCent = 9999;
	for(Int_t i=0; i<9; i++)
	{
		if(i==8)
		{
			if(mRefMult_corr>CentBin9_vzdep[vzid][i]&&mRefMult_corr<50000) iCent = i; 
		}
		else if(mRefMult_corr>CentBin9_vzdep[vzid][i]&&mRefMult_corr<CentBin9_vzdep[vzid][i+1]) iCent = i; 
	}
	// 80-100% for icent=-1
	if(mRefMult_corr>0&&mRefMult_corr<CentBin9_vzdep[vzid][0]) iCent = -1; 
	return (iCent==9999) ? -1 : iCent;
}

//______________________________________________________________________________
Int_t StRefMultCorr::getCentralityBin16VzDep() const 
{
	const Int_t vzid = getVzWindowForVzDepCentDef();
	Int_t iCent = 9999;
	for(Int_t i=0; i<16; i++)
	{
		if(i==15)
		{
			if(mRefMult_corr>CentBin16_vzdep[vzid][i]&&mRefMult_corr<50000) iCent = i; 
		}
		else if(mRefMult_corr>CentBin16_vzdep[vzid][i]&&mRefMult_corr<CentBin16_vzdep[vzid][i+1]) iCent = i; 
	}
	// 80-100% for icent=-1
	if(mRefMult_corr>0&&mRefMult_corr<CentBin16_vzdep[vzid][0]) iCent = -1; 
	return (iCent==9999) ? -1 : iCent;
}


//______________________________________________________________________________
const Int_t StRefMultCorr::getRefX() const
{
	if (      mName.CompareTo("grefmult", TString::kIgnoreCase) == 0 ) return 0; 
	else if ( mName.CompareTo("refmult",  TString::kIgnoreCase) == 0 ) return 1; 
	else if ( mName.CompareTo("refmult2", TString::kIgnoreCase) == 0 ) return 2; 
	else if ( mName.CompareTo("refmult3", TString::kIgnoreCase) == 0 ) return 3; 
	else if ( mName.CompareTo("refmult4", TString::kIgnoreCase) == 0 ) return 4; 
	else return 9999;
}

//______________________________________________________________________________
const Int_t StRefMultCorr::getNumberOfDatasets() const
{
	if (      mName.CompareTo("grefmult", TString::kIgnoreCase) == 0 ) return nID_gref; 
	else if ( mName.CompareTo("refmult",  TString::kIgnoreCase) == 0 ) return nID_ref1; 
	else if ( mName.CompareTo("refmult2", TString::kIgnoreCase) == 0 ) return nID_ref2; 
	else if ( mName.CompareTo("refmult3", TString::kIgnoreCase) == 0 ) return nID_ref3; 
	else if ( mName.CompareTo("refmult4", TString::kIgnoreCase) == 0 ) return nID_ref4; 
	else return 9999;
}

//______________________________________________________________________________
void StRefMultCorr::readHeaderFile()
{

	//vector<string> sParam_ShapeWeight = StringSplit(getParamX_ShapeWeight(1,1),',');
	//for(int ib=0;ib<sParam_ShapeWeight.size(); ib++) cout<<"sParam_ShapeWeight[i]: "<<sParam_ShapeWeight[ib]<<endl;


	const Int_t refX = getRefX();
	const Int_t nID =  getNumberOfDatasets();

	for(int iID=0; iID<nID; iID++)
	{
		//// Year, energy, run numbers, Vz cut
		Int_t year; Double_t energy;
		vector<string> sParam = StringSplit(getParamX(refX,iID,0),':'); 
		year = stoi(sParam[0]); 
		energy = stoi(sParam[1]); 
		vector<string> sRuns = StringSplit(sParam[2],',');
		
		Int_t startRunId=0, stopRunId=0;
		startRunId = stoi(sRuns[0]); 
		stopRunId = stoi(sRuns[1]); 
		
		Double_t startZvertex=-9999., stopZvertex=-9999. ;
		vector<string> sVz = StringSplit(sParam[3],',');
		startZvertex = stod(sVz[0]);
		stopZvertex = stod(sVz[1]);

		mYear.push_back(year);
		mBeginRun.insert(std::make_pair(std::make_pair(energy, year), startRunId));
		mEndRun.insert(  std::make_pair(std::make_pair(energy, year), stopRunId));
		mStart_runId.push_back( startRunId ) ;
		mStop_runId.push_back(  stopRunId ) ;
		mStart_zvertex.push_back( startZvertex ) ;
		mStop_zvertex.push_back(  stopZvertex ) ;

		//// Centrality definition
		vector<string> sParamCent = StringSplit(getParamX(refX,iID,1),','); 
		for(UInt_t i=0; i<sParamCent.size(); i++)
		{
			mCentrality_bins[i].push_back( stoi(sParamCent[i]) );
		}
		mCentrality_bins[mNCentrality].push_back( 5000 );

		//// Normalization range
		Double_t normalize_stop=-1.0 ;
		normalize_stop = stod(getParamX(refX,iID,2)) ;
		mNormalize_stop.push_back( normalize_stop );

		//// Acceptance correction
		vector<string> sParamVz = StringSplit(getParamX(refX,iID,3),','); 
		for(UInt_t i=0;i<mNPar_z_vertex;i++) 
		{
			Double_t val = -9999.; 
			if(i<sParamVz.size()) val = stod(sParamVz[i]);
			else                  val = 0.0;
			mPar_z_vertex[i].push_back( val );
		}

		//// Trigger inefficiency correction
		vector<string> sParamTrig = StringSplit(getParamX(refX,iID,4),','); 
		for(UInt_t i=0; i<mNPar_weight; i++) 
		{
			Double_t val = -9999.;
			if(i<sParamTrig.size()) val = stod(sParamTrig[i]);
			else                    val = 0.0;
			mPar_weight[i].push_back( val );
		}

		//// Luminosity correction
		vector<string> sParamLumi = StringSplit(getParamX(refX,iID,5),','); 
		for(UInt_t i=0; i<mNPar_luminosity; i++) 
		{
			Double_t val = -9999.;
			if(i<sParamLumi.size()) val = stod(sParamLumi[i]);
			else                    val = 0.0;
			mPar_luminosity[i].push_back( val );
		}

		//	cout << refX <<"  "<< iID <<"/"<< nID << endl;

	}

	cout <<"StRefMultCorr::readHeaderFile  [" << mName <<"] Correction parameters and centrality definitions have been successfully read."<< endl;

}

//______________________________________________________________________________
void StRefMultCorr::readBadRunsFromHeaderFile()
{
	for(Int_t i=0; i<nBadRun_refmult_2010; i++)
	{
		mBadRun.push_back(badrun_refmult_2010[i]);
	}
	
	cout<<"read in nBadRun_refmult_2010: "<<nBadRun_refmult_2010<<endl;

	for(Int_t i=0; i<nBadRun_refmult_2011; i++)
	{
		mBadRun.push_back(badrun_refmult_2011[i]);
	}
	
	cout<<"read in nBadRun_refmult_2011: "<<nBadRun_refmult_2011<<endl;

	for(Int_t i=0; i<nBadRun_grefmult_2014; i++)
	{
		mBadRun.push_back(badrun_grefmult_2014[i]);
	}
	
	cout<<"read in nBadRun_grefmult_2014: "<<nBadRun_grefmult_2014<<endl;

	for(Int_t i=0; i<nBadRun_grefmult_2016; i++)
	{
		mBadRun.push_back(badrun_grefmult_2016[i]);
	}
	
	cout<<"read in nBadRun_grefmult_2016: "<<nBadRun_grefmult_2016<<endl;
	
	for(Int_t i=0; i<nBadRun_refmult_2017; i++)
	{
		mBadRun.push_back(badrun_refmult_2017[i]);
	}
	
	cout<<"read in nBadRun_refmult_2017: "<<nBadRun_refmult_2017<<endl;
	
	for(Int_t i=0; i<nBadRun_refmult_2018; i++)
	{
		mBadRun.push_back(badrun_refmult_2018[i]);
	}
	
	cout<<"read in nBadRun_refmult_2018: "<<nBadRun_refmult_2018<<endl;
	
	//// notification only one
	if ( mName.CompareTo("grefmult", TString::kIgnoreCase) == 0 )
	{
		//cout << "StRefMultCorr::readBadRunsFromHeaderFile  Bad runs for year 2010, 2011 and 2017 have been read." << endl;
		cout << "StRefMultCorr::readBadRunsFromHeaderFile  Bad runs for year 2010, 2011, 2017 and 2018 have been read." << endl;
	}
}

//______________________________________________________________________________
void StRefMultCorr::print(const Option_t* option) const
{
	cout << "StRefMultCorr::print  Print input parameters for " << mName << " ========================================" << endl << endl;
	// Option switched off, can be used to specify parameters
	//  const TString opt(option);

	//  Int_t input_counter = 0;
	for(UInt_t id=0; id<mStart_runId.size(); id++) 
	{
		//cout << "Data line = " << input_counter << ", Start_runId = " << Start_runId[input_counter] << ", Stop_runId = " << Stop_runId[input_counter] << endl;
		//    const UInt_t id = mStart_runId.size()-1;

		// Removed line break
		cout << "  Index=" << id;
		cout << Form(" Run=[%8d, %8d]", mStart_runId[id], mStop_runId[id]);
		cout << Form(" z-vertex=[%1.1f, %1.1f]", mStart_zvertex[id], mStop_zvertex[id]);
		cout << ", Normalize_stop=" << mNormalize_stop[id];
		cout << endl;

		//    if(opt.IsWhitespace()){
		//      continue ;
		//    }

		cout << "Centrality:  ";
		
		for(Int_t i=0;i<mNCentrality;i++)
		{
			cout << Form("  >%2d%%", 80-5*i);
		}
		cout << endl;
		cout << "RefMult:     ";
		
		for(Int_t i=0;i<mNCentrality;i++)
		{
			//      cout << Form("StRefMultCorr::read  Centrality %3d-%3d %%, refmult > %4d", 75-5*i, 80-5*i, mCentrality_bins[i][id]) << endl;
			const TString tmp(">");
			const TString centrality = tmp + Form("%d", mCentrality_bins[i][id]);
			cout << Form("%6s", centrality.Data());
		}
		cout << endl;

		for(Int_t i=0;i<mNPar_z_vertex;i++) 
		{
			cout << "  mPar_z_vertex[" << i << "] = " << mPar_z_vertex[i][id];
		}
		cout << endl;
		
		for(Int_t i=0;i<mNPar_weight;i++) 
		{
			cout << "  mPar_weight[" << i << "] = " << mPar_weight[i][id];
		}
		cout << endl;
		
		for(Int_t i=0;i<mNPar_luminosity;i++) 
		{
			cout << "  mPar_luminosity[" << i << "] = " << mPar_luminosity[i][id];
		}
		cout << endl << endl;
	}
	cout << "=====================================================================================" << endl;
}

//______________________________________________________________________________
vector<string> StRefMultCorr::StringSplit( const string str, const char sep )
{
	vector<string> vstr;
	stringstream ss(str);
	string buffer;
	while( getline(ss,buffer,sep) )
	{
		vstr.push_back(buffer);
	}
	return vstr;
}

