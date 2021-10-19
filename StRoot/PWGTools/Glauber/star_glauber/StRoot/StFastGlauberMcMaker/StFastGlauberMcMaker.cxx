/******************************************************************************
 * $Id: StFastGlauberMcMaker.cxx,v 1.6 2014/10/21 01:03:38 hmasui Exp $
 * $Log: StFastGlauberMcMaker.cxx,v $
 * Revision 1.6  2014/10/21 01:03:38  hmasui
 * Add 14.5 GeV cross section
 *
 * Revision 1.5  2013/02/06 18:58:20  hmasui
 * Added 2.76 TeV cross section
 *
 * Revision 1.4  2013/02/05 22:49:50  hmasui
 * Added Pb and Cu nuclei
 *
 * Revision 1.3  2012/04/25 05:22:42  hmasui
 * Added deformation parameters, higher order participant eccentricity with r^2 weight
 *
 * Revision 1.2  2010/11/20 19:03:11  hmasui
 * Mode mode flag to StCentralityMaker. Use STAR logger rather than STD iostream
 *
 ******************************************************************************/

#include <assert.h>

#include "TF1.h"
#include "TF3.h"
#include "TH1.h"
#include "TGraph.h"
#include "TMath.h"
#include "TVector3.h"

#include "StMessMgr.h"

#include "StMessMgr.h"
#include "Nucleon.h"
#include "StCentralityMaker/StCentralityMaker.h"
#include "StCentralityMaker/StNegativeBinomial.h"
#include "StGlauberTree/StGlauberTree.h"
#include "StGlauberUtilities/StGlauberUtilities.h"
#include "StFastGlauberMcMaker.h"

using std::vector ;

ClassImp(StFastGlauberMcMaker)

	const UInt_t StFastGlauberMcMaker::mVersion = 2 ; /// Current version

	//____________________________________________________________________________________________________
	// Default constructor
StFastGlauberMcMaker::StFastGlauberMcMaker()
	: mEnergy(200), mOutputFileName("fastglaubermc.root"),
	mInelasticNNCrossSection(4.2)  // fm^2 --> 42 mb
{
	/// Initialize Au+Au collisions at 200 GeV
	const UInt_t A = 197 ;
	const Double_t R = 6.38 ;
	const Double_t d = 0.535 ;

	mMode = 0 ;
	mIsDeformed[0] = kFALSE ;
	mIsDeformed[1] = kFALSE ;

	Init(A, R, d, 0.0, 0.0, A, R, d, 0.0, 0.0);
}

//____________________________________________________________________________________________________
StFastGlauberMcMaker::StFastGlauberMcMaker(
		const TString outputFileName,
		const TString system,
		const Double_t energy,
		const TString type,
		const Bool_t isDeformed
		)
: mEnergy(energy), mOutputFileName(outputFileName),
	mInelasticNNCrossSection(GetInelasticNNCrossSection(mEnergy, type))
{
	// Set deformation. Spherical + Spherical or Deformed + Deformed collision
	// Use other constructor if Spherical + Deformed collision is needed
	mIsDeformed[0] = isDeformed ;
	mIsDeformed[1] = isDeformed ;

	// Initialize nuclei
	if ( system.CompareTo("auau", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker  Initialize AuAu collisions" << endm;
		InitAuAu(type);
	}
	else if ( system.CompareTo("smsm", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker  Initialize SmSm collisions" << endm;
		InitSmSm(type);
	}
	else if ( system.CompareTo("uu", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker  Initialize UU collisions" << endm;
		InitUU(type);
	}
	else if ( system.CompareTo("pbpb", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker  Initialize PbPb collisions" << endm;
		InitPbPb(type);
	}
	else if ( system.CompareTo("cucu", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker  Initialize CuCu collisions" << endm;
		InitCuCu(type);
	}
	else if ( system.CompareTo("zrzr_case1", TString::kIgnoreCase) == 0 ){
                LOG_INFO << "StFastGlauberMcMaker  Initialize ZrZr_Case1 collisions" << endm;
                InitZrZr(type,1);
        }
	else if ( system.CompareTo("ruru_case1", TString::kIgnoreCase) == 0 ){
                LOG_INFO << "StFastGlauberMcMaker  Initialize RuRu_Case1 collisions" << endm;
                InitRuRu(type,1); 
	}
	else if ( system.CompareTo("zrzr_case2", TString::kIgnoreCase) == 0 ){
                LOG_INFO << "StFastGlauberMcMaker  Initialize ZrZr_Case2 collisions" << endm;
                InitZrZr(type,2);
        }
        else if ( system.CompareTo("ruru_case2", TString::kIgnoreCase) == 0 ){
                LOG_INFO << "StFastGlauberMcMaker  Initialize RuRu_Case2 collisions" << endm;
                InitRuRu(type,2);
        }
	else if ( system.CompareTo("zrzr_case3", TString::kIgnoreCase) == 0 ){
                LOG_INFO << "StFastGlauberMcMaker  Initialize ZrZr_Case3 collisions" << endm;
                InitZrZr(type,3);
        }
        else if ( system.CompareTo("ruru_case3", TString::kIgnoreCase) == 0 ){
                LOG_INFO << "StFastGlauberMcMaker  Initialize RuRu_Case3 collisions" << endm;
                InitRuRu(type,3);
        }
	else{
		Error("StFastGlauberMcMaker", "Unknown system %s. abort", system.Data());
		assert(0);
	}
}

//____________________________________________________________________________________________________
StFastGlauberMcMaker::StFastGlauberMcMaker(
		const TString outputFileName, // Output filename
		const UInt_t massNumber,  // Mass number of nucleus
		const Double_t radius,    // Radius of nucleus
		const Double_t skinDepth, // Skin depth of nucleus
		const Double_t beta2, // 2nd order deformation parameter
		const Double_t beta4, // 4th order deformation parameter
		const Double_t inelasticCrossSection, // Inelastic NN cross section
		const Double_t energy  // sqrt(sNN)
		)
: mEnergy(energy), mOutputFileName(outputFileName),
	mInelasticNNCrossSection(inelasticCrossSection)
{
	mMode = 0 ;

	// Set deformation
	mIsDeformed[0] = kFALSE ;
	mIsDeformed[1] = kFALSE ;
	if(beta2 != 0.0 || beta4 != 0.0 ){
		mIsDeformed[0] = kTRUE ;
		mIsDeformed[1] = kTRUE ;
	}

	/// Initialize symmetric collisions
	Init(massNumber, radius, skinDepth, beta2, beta4, massNumber, radius, skinDepth, beta2, beta4) ;
}

//____________________________________________________________________________________________________
StFastGlauberMcMaker::StFastGlauberMcMaker(
		const TString outputFileName, // Output filename
		const UInt_t massNumberA,  // Mass number of nucleus for nucleus A
		const Double_t radiusA,    // Radius of nucleus for nucleus A
		const Double_t skinDepthA, // Skin depth of nucleus for nucleus A
		const Double_t beta2A,     // 2nd order deformation parameter for nucleus A
		const Double_t beta4A,     // 4th order deformation parameter for nucleus A
		const UInt_t massNumberB,  // Mass number of nucleus for nucleus B
		const Double_t radiusB,    // Radius of nucleus for nucleus B
		const Double_t skinDepthB, // Skin depth of nucleus for nucleus B
		const Double_t beta2B,     // 2nd order deformation parameter for nucleus B
		const Double_t beta4B,     // 4th order deformation parameter for nucleus B
		const Double_t inelasticCrossSection, // Inelastic NN cross section
		const Double_t energy  // sqrt(sNN)
		)
: mEnergy(energy), mOutputFileName(outputFileName),
	mInelasticNNCrossSection(inelasticCrossSection)
{
	mMode = 0 ;

	// Set deformation
	mIsDeformed[0] = kFALSE ;
	mIsDeformed[1] = kFALSE ;
	if( beta2A != 0.0 || beta4A != 0.0 ) mIsDeformed[0] = kTRUE ;
	if( beta2B != 0.0 || beta4B != 0.0 ) mIsDeformed[1] = kTRUE ;

	/// Initialize asymmetric collisions
	Init(massNumberA, radiusA, skinDepthA, beta2A, beta4A, massNumberB, radiusB, skinDepthB, beta2B, beta4B) ;
}

//____________________________________________________________________________________________________
// Default destructor
StFastGlauberMcMaker::~StFastGlauberMcMaker()
{
}

//____________________________________________________________________________________________________
void StFastGlauberMcMaker::SetRepulsionDistance(const Double_t repulsionDistance)
{
	mRepulsionDistance = repulsionDistance ;
	LOG_INFO << "StFastGlauberMcMaker::SetRepulsionDistance  Set repulsion distance between two nucleons, d = "
		<< mRepulsionDistance << " fm"
		<< endm;
}

//____________________________________________________________________________________________________
Int_t StFastGlauberMcMaker::Clear()
{
	mGlauberTree->Clear() ;

	for(UInt_t in=0;in<2;in++){
		for(UInt_t i=0;i<mNucleons[in].size();i++){
			mNucleons[in][i]->Reset() ;
		}
	}

	return 1 ;
}

//____________________________________________________________________________________________________
Int_t StFastGlauberMcMaker::InitTree()
{
	// Initialize Glauber tree in write mode
	mGlauberTree = new StGlauberTree(1);

	// Clear data members in tree
	Clear() ;

	// Initialize tree
	mGlauberTree->Open(mOutputFileName) ;

	// Histograms
	for(Int_t in=0;in<4;in++){
		TString nucleus("Projectile");
		if( in % 2 == 1 ) nucleus = "Target";

		TString title(Form("Woods-saxon density profile (%s)", nucleus.Data()));
		if(in>=2) title = Form("Woods-saxon density profile after repulsion (%s)", nucleus.Data());

		mhWoodsSaxon[in] = new TH1D(Form("hWoodsSaxon_%d", in), title, 400, 0, 20);
		mhWoodsSaxon[in]->SetXTitle("r (fm)");
	}

	// Sort output histograms/tree
	mGlauberTree->Sort();

	return 1;
}

//____________________________________________________________________________________________________
Int_t StFastGlauberMcMaker::Init(
		const UInt_t massNumberA,  // Mass number of nucleus for nucleus A
		const Double_t radiusA,    // Radius of nucleus for nucleus A
		const Double_t skinDepthA, // Skin depth of nucleus for nucleus A
		const Double_t beta2A,     // 2nd order deformation parameter for nucleus A
		const Double_t beta4A,     // 4th order deformation parameter for nucleus A
		const UInt_t massNumberB,  // Mass number of nucleus for nucleus B
		const Double_t radiusB,    // Radius of nucleus for nucleus B
		const Double_t skinDepthB, // Skin depth of nucleus for nucleus B
		const Double_t beta2B,     // 2nd order deformation parameter for nucleus B
		const Double_t beta4B,      // 4th order deformation parameter for nucleus B
		const TString type
		){
	LOG_INFO << "StFastGlauberMcMaker::Init  Inelastic N-N cross section = "
		<< mInelasticNNCrossSection * 10.0 << " mb"
		<< "  for sqrt(sNN) = " << mEnergy << " (GeV)"
		<< endm;

	mDebug = 0 ;

	mNeventsThrow  = 0 ; // Number of all events
	mNeventsAccept = 0 ; // Number of accepted events (Ncoll>0)

	// Centrlaity maker
	const Char_t* system(Form("%s%s_%dGeV", GetName(massNumberA), GetName(massNumberB), 
				static_cast<Int_t>(mEnergy))) ;
	LOG_INFO << "StFastGlauberMcMaker::init  Initialize system " << system << endm;
	mCentralityMaker = new StCentralityMaker(system);

	/// Repulsion distance is 0fm by default. Use SetRepulsionDistance(const Double_t) method to set finite value
	mRepulsionDistance = 0.0 ;

	mHardCoreSmearing = kFALSE ; /// Hard-core smearing, default is OFF
	mGaussianSmearing = kFALSE ; /// Gaussian smearing, default is OFF

	mCollisionProfile = mkHardCoreProfile  ; /// Default is hard-core collision
	if ( type.CompareTo("gauss", TString::kIgnoreCase) == 0 ){
		//DoGaussianCollision() ;
	}

	/// Hard-core smearing (only use if mHardCoreSmearing is true)
	const Double_t dmaxh = TMath::Sqrt(mInelasticNNCrossSection/TMath::Pi());
	mfHardCore = new TF3("fHardCore", GlauberUtilities::StepFunction, -dmaxh, dmaxh, -dmaxh, dmaxh, -dmaxh, dmaxh, 1);
	mfHardCore->SetParameter(0, mInelasticNNCrossSection);

	if(mDebug){
		LOG_INFO << "StFastGlauberMcMaker::Init  Initialize hard-core smearing function (will be used only if hard-core smearing is ON)"
			<< endm;
	}

	/// Gaussian smearing (only use if mGaussianSmearing is true)
	const Double_t sigma = 0.79/TMath::Sqrt(3.0) ;
	const Double_t dmaxg = sigma*5.0 ;
	mfGaussian = new TF3("fGaussian", GlauberUtilities::Gaussian, -dmaxg, dmaxg, -dmaxg, dmaxg, -dmaxg, dmaxg, 1);
	mfGaussian->SetParameter(0, sigma); // width

	if(mDebug){
		LOG_INFO << "StFastGlauberMcMaker::Init  Initialize gaussian smearing function (will be used only if gaussian smearing is ON)"
			<< endm;
	}

	for(UInt_t in=0; in<2; in++){
		if(mIsDeformed[in]){
			// Make sure deformation is finite
			const Bool_t isNoDeformation = (in==0) ? (beta2A==0.0 && beta4A==0.0) : (beta2B==0.0 && beta4B==0.0) ;
			if( isNoDeformation ){
				Error("StFastGlauberMcMaker::Init", "No deformation: beta2=beta4=0. abort");
				assert(0);
			}

			/// Initialize Woods-saxon density profile for deformed nuclei
			mfWoodsSaxon[in] = 0 ; // NULL pointer for spherical woods-saxon

			mfWoodsSaxon2D[in] = new TF2(Form("fWoodsSaxon2D_%d", in), GlauberUtilities::WoodsSaxon2D, 0, 20, -1.0, 1.0, 4);
			mfWoodsSaxon2D[in]->SetParName(0, "Radius");
			mfWoodsSaxon2D[in]->SetParName(1, "Skin depth");
			mfWoodsSaxon2D[in]->SetParName(2, "#beta_{2}");
			mfWoodsSaxon2D[in]->SetParName(3, "#beta_{4}");
			//      mfWoodsSaxon2D[in]->SetParName(4, "#beta_{6}");

			// Default number of sampling points are too small (probably for y-axis) in TF2
			// this causes the step-like structures if you get (r,theta) from TF2::GetRandom2()
			// --> Increase Npx, Npy by a factor of 2
			mfWoodsSaxon2D[in]->SetNpx(200);
			mfWoodsSaxon2D[in]->SetNpy(200);

			if( in == 0 ){
				mfWoodsSaxon2D[in]->SetParameter(0, radiusA) ;
				mfWoodsSaxon2D[in]->SetParameter(1, skinDepthA) ;
				mfWoodsSaxon2D[in]->SetParameter(2, beta2A) ;
				mfWoodsSaxon2D[in]->SetParameter(3, beta4A) ;
			}
			else{
				mfWoodsSaxon2D[in]->SetParameter(0, radiusB) ;
				mfWoodsSaxon2D[in]->SetParameter(1, skinDepthB) ;
				mfWoodsSaxon2D[in]->SetParameter(2, beta2B) ;
				mfWoodsSaxon2D[in]->SetParameter(3, beta4B) ;
			}

			LOG_INFO << "StFastGlauberMcMaker::Init  Initialize Deformed Woods-saxon density profile" << endm;
			for(Int_t ipar=0; ipar<mfWoodsSaxon2D[in]->GetNpar(); ipar++){
				LOG_INFO << "StFastGlauberMcMaker::Init  par[" << ipar << "] = " << mfWoodsSaxon2D[in]->GetParameter(ipar)
					<< ",  parName = " << mfWoodsSaxon2D[in]->GetParName(ipar)
					<< endm; 
			}
		}
		else{
			/// Initialize Woods-saxon density profile for spherical nuclei
			// Radius and skin depth
			mfWoodsSaxon2D[in] = 0 ; // NULL pointer for deformed woods-saxon

			mfWoodsSaxon[in] = new TF1(Form("fWoodsSaxon_%d", in), GlauberUtilities::WoodsSaxon, 0, 20, 2);
			mfWoodsSaxon[in]->SetParName(0, "Radius");
			mfWoodsSaxon[in]->SetParName(1, "Skin depth");
			if( in == 0 ){
				mfWoodsSaxon[in]->SetParameter(0, radiusA) ;
				mfWoodsSaxon[in]->SetParameter(1, skinDepthA) ;
			}
			else{
				mfWoodsSaxon[in]->SetParameter(0, radiusB) ;
				mfWoodsSaxon[in]->SetParameter(1, skinDepthB) ;
			}

			LOG_INFO << "StFastGlauberMcMaker::Init  Initialize Spherical Woods-saxon density profile" << endm;
			for(Int_t ipar=0; ipar<mfWoodsSaxon[in]->GetNpar(); ipar++){
				LOG_INFO << "StFastGlauberMcMaker::Init  par[" << ipar << "] = " << mfWoodsSaxon[in]->GetParameter(ipar)
					<< ",  parName = " << mfWoodsSaxon[in]->GetParName(ipar)
					<< endm; 
			}
		}

		/// Initialize nucleons
		mNucleons[in].clear() ;
		const UInt_t A = (in==0) ? massNumberA : massNumberB ;
		LOG_INFO << "StFastGlauberMcMaker::Init  Initialize " << A << " nucleons ..." << endm;

		for(UInt_t i=0;i<A;i++){
			mNucleons[in].push_back( new Nucleon() ) ;
		}
	}

	// Initialize output tree
	InitTree() ;

	return 1 ;
}

//____________________________________________________________________________________________________
Int_t StFastGlauberMcMaker::InitAuAu(const TString type)
{
	/// Initialize Au + Au collisions
	const UInt_t A   = 197 ;
	const Double_t R = 6.38 ;
	const Double_t d = 0.535 ;

	// Parameterization with hard-core smearing
	// from PRC79, 064904 (2009) T. Hirano et. al.
	//  const Double_t R = 6.42 ;
	//  const Double_t d = 0.544 ;
	Double_t RError = 0.0 ; // Absolute error on R
	Double_t dError = 0.0 ; // Absolute error on d

	// Deformation parameters
	Double_t beta2 = 0.0 ;
	Double_t beta4 = 0.0 ;
	//  Double_t beta6 = 0.0 ;

	// Deformation
	if( mIsDeformed[0] || mIsDeformed[1] ){
		LOG_INFO << "StFastGlauberMcMaker::InitAuAu  Set deformation for Au nuclei" << endm ;
		beta2 = -0.131 ; // Deformation parameter beta2
		beta4 = -0.031 ; // Deformation parameter beta4
		//    beta6 =  0.007 ; // Deformation parameter beta6
	}

	// Error on R and d is taken from Atomic Data and Nuclear Table 36, 495 (1987)
	// R = 6.38 +/- 0.06, d = 0.535 +/- 0.027
	//   --> take 2 sigma
	if ( type.CompareTo("large", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker::InitAuAu  Set large R, small d" << endm;
		RError = 0.12 ;
		dError = -0.054 ;
		// RError = 0.32 ;   //5%
		// dError = -0.0267 ;//5%
	}
	else if ( type.CompareTo("small", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker::InitAuAu  Set small R, large d" << endm;
		RError = -0.12 ;
		dError = 0.054 ;
		// RError = -0.32 ;  //5%
		// dError = 0.0267 ; //5%
	}
	else if ( type.CompareTo("gauss", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker::InitAuAu  Gaussian collision profile" << endm;
		DoGaussianCollision() ;
	}
	else if ( type.CompareTo("smallNpp", TString::kIgnoreCase) == 0 ){
		mMode = 1 ;
		LOG_INFO << "StFastGlauberMcMaker::InitAuAu  Use small npp, large x for multiplicity (mMode = "
			<< mMode << ")"
			<< endm;
	}
	else if ( type.CompareTo("largeNpp", TString::kIgnoreCase) == 0 ){
		mMode = 2 ;
		LOG_INFO << "StFastGlauberMcMaker::InitAuAu  Use large npp, small x for multiplicity (mMode = "
			<< mMode << ")"
			<< endm;
	}

	return Init(A, R+RError, d+dError, beta2, beta4, A, R+RError, d+dError, beta2, beta4, type);
}

//____________________________________________________________________________________________________
Int_t StFastGlauberMcMaker::InitSmSm(const TString type)
{
	/// Initialize Sm + Sm collisions
	//   - Sm144 (spherical, A=144, N=82, Z=62)
	//   - Sm154 (deformed,  A=154, N=92, Z=62)
	//
	//  Sm144
	//   R = 5.71 = 1.12*(144)^{1/3} - 0.86*(144)^{-1/3}
	//   d = 0.543 = 0.522 * 5.9387 / 5.71
	const UInt_t A   = (mIsDeformed) ? 154 : 144 ;
	//  const Double_t R = (mIsDeformed) ? 5.9387 : 5.71 ;
	//  const Double_t d = (mIsDeformed) ? 0.522 : 0.543 ;
	const Double_t R = (mIsDeformed) ? 5.9387 : 5.9387 ;
	const Double_t d = (mIsDeformed) ? 0.522 : 0.522 ;
	Double_t RError = 0.0 ; // Absolute error on R
	Double_t dError = 0.0 ; // Absolute error on d

	// Deformation parameters
	Double_t beta2 = 0.0 ;
	Double_t beta4 = 0.0 ;
	//  Double_t beta6 = 0.0 ;

	// Deformation
	if( mIsDeformed ){
		LOG_INFO << "StFastGlauberMcMaker::InitSmSm  Set deformation for Sm nuclei" << endm ;
		beta2 = 0.270 ; // Deformation parameter beta2
		beta4 = 0.113 ; // Deformation parameter beta4
		//    beta6 = -0.005 ; // Deformation parameter beta6
	}

	// Error on R and d is taken from Atomic Data and Nuclear Table 36, 495 (1987)
	// R = 5.9387 (no error, assign +/- 0.0050), d = 0.522 +/- 0.015 for Sm154
	// No R and d parameters for Sm144
	//   --> take 2 sigma
	if ( type.CompareTo("large", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker::InitSmSm  Set large R, small d" << endm;
		RError = 0.01 ;
		dError = -0.030 ;
	}
	else if ( type.CompareTo("small", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker::InitSmSm  Set small R, large d" << endm;
		RError = -0.01 ;
		dError = 0.030 ;
	}
	else if ( type.CompareTo("gauss", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker::InitSmSm  Gaussian collision profile" << endm;
		DoGaussianCollision() ;
	}
	else if ( type.CompareTo("smallNpp", TString::kIgnoreCase) == 0 ){
		mMode = 1 ;
		LOG_INFO << "StFastGlauberMcMaker::InitSmSm  Use small npp, large x for multiplicity (mMode = "
			<< mMode << ")"
			<< endm;
	}
	else if ( type.CompareTo("largeNpp", TString::kIgnoreCase) == 0 ){
		mMode = 2 ;
		LOG_INFO << "StFastGlauberMcMaker::InitSmSm  Use large npp, small x for multiplicity (mMode = "
			<< mMode << ")"
			<< endm;
	}

	return Init(A, R+RError, d+dError, beta2, beta4, A, R+RError, d+dError, beta2, beta4, type);
}

//____________________________________________________________________________________________________
Int_t StFastGlauberMcMaker::InitUU(const TString type)
{
	/// Initialize U + U collisions
	const UInt_t A   = 238 ;
	const Double_t R = 6.81 ;
	const Double_t d = 0.55 ;
	Double_t RError = 0.0 ; // Absolute error on R
	Double_t dError = 0.0 ; // Absolute error on d

	// Deformation parameters
	Double_t beta2 = 0.0 ;
	Double_t beta4 = 0.0 ;
	//  Double_t beta6 = 0.0 ;

	// Deformation
	if( mIsDeformed ){
		LOG_INFO << "StFastGlauberMcMaker::InitUU  Set deformation for U nuclei" << endm ;
		beta2 = 0.28  ; // Deformation parameter beta2
		beta4 = 0.093 ; // Deformation parameter beta4
		//    beta6 =  0.007 ; // Deformation parameter beta6
	}

	// Error on R and d is taken from Atomic Data and Nuclear Table 36, 495 (1987)
	// R = 6.8054, d = 0.605 +/- 0.016 (PRC13, 1083, 1976)
	// R = 6.874,  d = 0.556 (unpublished)
	// difference gives
	//  \Delta R = 0.0686 fm, \Delta d = 0.049 fm
	//   --> take 2 sigma
	if ( type.CompareTo("large", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker::InitUU  Set large R, small d" << endm;
		RError = 0.137 ;
		dError = -0.098 ;
	}
	else if ( type.CompareTo("small", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker::InitUU  Set small R, large d" << endm;
		RError = -0.137 ;
		dError = 0.098 ;
	}
	else if ( type.CompareTo("gauss", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker::InitUU  Gaussian collision profile" << endm;
		DoGaussianCollision() ;
	}
	else if ( type.CompareTo("smallNpp", TString::kIgnoreCase) == 0 ){
		mMode = 1 ;
		LOG_INFO << "StFastGlauberMcMaker::InitUU  Use small npp, large x for multiplicity (mMode = "
			<< mMode << ")"
			<< endm;
	}
	else if ( type.CompareTo("largeNpp", TString::kIgnoreCase) == 0 ){
		mMode = 2 ;
		LOG_INFO << "StFastGlauberMcMaker::InitUU  Use large npp, small x for multiplicity (mMode = "
			<< mMode << ")"
			<< endm;
	}

	return Init(A, R+RError, d+dError, beta2, beta4, A, R+RError, d+dError, beta2, beta4, type);
}

//____________________________________________________________________________________________________
Int_t StFastGlauberMcMaker::InitPbPb(const TString type)
{
	/// Initialize Pb + Pb collisions
	const UInt_t A   = 208 ;
	const Double_t R = 6.62 ;
	const Double_t d = 0.546 ;
	Double_t RError = 0.0 ; // Absolute error on R
	Double_t dError = 0.0 ; // Absolute error on d

	// Deformation parameters
	Double_t beta2 = 0.0 ;
	Double_t beta4 = 0.0 ;
	//  Double_t beta6 = 0.0 ;

	// Deformation
	if( mIsDeformed ){
		LOG_INFO << "StFastGlauberMcMaker::InitPbPb  Set deformation for Pb nuclei" << endl;
		beta2 = 0.0;
		beta4 = 0.0;
	}

	if ( type.CompareTo("large", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker::InitPbPb  Set large R, small d" << endm;
		RError = 0.120 ;
		dError = -0.020 ;
	}
	else if ( type.CompareTo("small", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker::InitPbPb  Set small R, large d" << endm;
		RError = -0.120 ;
		dError = 0.020 ;
	}
	else if ( type.CompareTo("gauss", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker::InitPbPb  Gaussian collision profile" << endm;
		DoGaussianCollision() ;
	}
	else if ( type.CompareTo("smallNpp", TString::kIgnoreCase) == 0 ){
		mMode = 1 ;
		LOG_INFO << "StFastGlauberMcMaker::InitPbPb  Use small npp, large x for multiplicity (mMode = "
			<< mMode << ")"
			<< endm;
	}
	else if ( type.CompareTo("largeNpp", TString::kIgnoreCase) == 0 ){
		mMode = 2 ;
		LOG_INFO << "StFastGlauberMcMaker::InitPbPb  Use large npp, small x for multiplicity (mMode = "
			<< mMode << ")"
			<< endm;
	}

	return Init(A, R+RError, d+dError, beta2, beta4, A, R+RError, d+dError, beta2, beta4, type);
}

//____________________________________________________________________________________________________
Int_t StFastGlauberMcMaker::InitCuCu(const TString type)
{
	/// Initialize Cu + Cu collisions
	const UInt_t A   = 63 ;
	const Double_t R = 4.19 ;
	const Double_t d = 0.60 ;
	Double_t RError = 0.0 ; // Absolute error on R
	Double_t dError = 0.0 ; // Absolute error on d

	// Deformation parameters
	Double_t beta2 = 0.0 ;
	Double_t beta4 = 0.0 ;
	//  Double_t beta6 = 0.0 ;

	// Deformation
	if( mIsDeformed ){
		LOG_INFO << "StFastGlauberMcMaker::InitCuCu  Set deformation for Cu nuclei" << endm ;
		beta2 = 0.162  ; // Deformation parameter beta2
		beta4 = -0.006 ; // Deformation parameter beta4
	}

	// Error on R and d is taken from Atomic Data and Nuclear Table 36, 495 (1987)
	if ( type.CompareTo("large", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker::InitCuCu  Set large R, small d" << endm;
		RError = 0.041 ;
		dError = -0.016 ;
	}
	else if ( type.CompareTo("small", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker::InitCuCu  Set small R, large d" << endm;
		RError = -0.041 ;
		dError = 0.016 ;
	}
	else if ( type.CompareTo("gauss", TString::kIgnoreCase) == 0 ){
		LOG_INFO << "StFastGlauberMcMaker::InitCuCu  Gaussian collision profile" << endm;
		DoGaussianCollision() ;
	}
	else if ( type.CompareTo("smallNpp", TString::kIgnoreCase) == 0 ){
		mMode = 1 ;
		LOG_INFO << "StFastGlauberMcMaker::InitCuCu  Use small npp, large x for multiplicity (mMode = "
			<< mMode << ")"
			<< endm;
	}
	else if ( type.CompareTo("largeNpp", TString::kIgnoreCase) == 0 ){
		mMode = 2 ;
		LOG_INFO << "StFastGlauberMcMaker::InitCuCu  Use large npp, small x for multiplicity (mMode = "
			<< mMode << ")"
			<< endm;
	}

	return Init(A, R+RError, d+dError, beta2, beta4, A, R+RError, d+dError, beta2, beta4, type);
}

//____________________________________________________________________________________________________
Int_t StFastGlauberMcMaker::InitZrZr(const TString type, int Case)
{
        /// Initialize Zr + Zr collisions
        const UInt_t A   = 96 ;
	Double_t R;
	Double_t d;
	if(Case==1){R = 5.02 ;  d = 0.46 ;}
	else if(Case==2){R = 5.02 ;  d = 0.46 ;}
        else if(Case==3){R = 4.965 ;  d = 0.556 ;}
	Double_t RError = 0.0 ; // Absolute error on R
        Double_t dError = 0.0 ; // Absolute error on d

        // Deformation parameters
        Double_t beta2 = 0.0 ;
        Double_t beta4 = 0.0 ;
        //  Double_t beta6 = 0.0 ;

        // Deformation parameters taken from https://journals.aps.org/prc/pdf/10.1103/PhysRevC.99.044908
        //                                  B Schenke, C Shen, P Tribedy. Phys. Rev. C 99, 044908 (2019)
	// Deformation
        if( mIsDeformed ){
                LOG_INFO << "StFastGlauberMcMaker::InitZrZr  Set deformation for Zr nuclei" << endm ;
                if(Case==1){beta2 = 0.08  ; beta4 = 0.0 ; }
		else if(Case==2){beta2 = 0.217  ; beta4 = 0.0 ; }
		else if(Case==3){beta2 = 0.0  ; beta4 = 0.0 ; }
        }

	if ( type.CompareTo("large", TString::kIgnoreCase) == 0 ){
                LOG_INFO << "StFastGlauberMcMaker::InitZrZr  Set large R, small d" << endm;
                RError = 0.0 ;
                dError = 0.0 ;
        }
        else if ( type.CompareTo("small", TString::kIgnoreCase) == 0 ){
                LOG_INFO << "StFastGlauberMcMaker::InitZrZr  Set small R, large d" << endm;
                RError = 0.0 ;
                dError = 0.0 ;
        }
        else if ( type.CompareTo("gauss", TString::kIgnoreCase) == 0 ){
                LOG_INFO << "StFastGlauberMcMaker::InitZrZr  Gaussian collision profile" << endm;
                DoGaussianCollision() ;
        }
	else if ( type.CompareTo("smallNpp", TString::kIgnoreCase) == 0 ){
                mMode = 1 ;
                LOG_INFO << "StFastGlauberMcMaker::InitZrZr  Use small npp, large x for multiplicity (mMode = "
                        << mMode << ")"
                        << endm;
        }
        else if ( type.CompareTo("largeNpp", TString::kIgnoreCase) == 0 ){
                mMode = 2 ;
                LOG_INFO << "StFastGlauberMcMaker::InitZrZr  Use large npp, small x for multiplicity (mMode = "
                        << mMode << ")"
                        << endm;
        }

        return Init(A, R+RError, d+dError, beta2, beta4, A, R+RError, d+dError, beta2, beta4, type);
}

//____________________________________________________________________________________________________
Int_t StFastGlauberMcMaker::InitRuRu(const TString type, int Case)
{
        /// Initialize Ru + Ru collisions
        const UInt_t A   = 96 ;
	Double_t R;
	Double_t d;
        if(Case==1){R = 5.085 ;  d = 0.46 ;}
        else if(Case==2){R = 5.085 ;  d = 0.46 ;}
        else if(Case==3){R = 5.067 ;  d = 0.500 ;}
	Double_t RError = 0.0 ; // Absolute error on R
        Double_t dError = 0.0 ; // Absolute error on d

        // Deformation parameters
        Double_t beta2 = 0.0 ;
        Double_t beta4 = 0.0 ;
        //  Double_t beta6 = 0.0 ;

        // Deformation parameters taken from https://journals.aps.org/prc/pdf/10.1103/PhysRevC.99.044908
        // 			            B Schenke, C Shen, P Tribedy. Phys. Rev. C 99, 044908 (2019)
        // Deformation
        if( mIsDeformed ){
                LOG_INFO << "StFastGlauberMcMaker::InitRuRu  Set deformation for Ru nuclei" << endm ;
        	if(Case==1){beta2 = 0.158  ; beta4 = 0.0 ; }
                else if(Case==2){beta2 = 0.053  ; beta4 = 0.0 ; }
                else if(Case==3){beta2 = 0.0  ; beta4 = 0.0 ; }
	}

        if ( type.CompareTo("large", TString::kIgnoreCase) == 0 ){
                LOG_INFO << "StFastGlauberMcMaker::InitRuRu  Set large R, small d" << endm;
                RError = 0.0 ;
                dError = 0.0 ;
        }
        else if ( type.CompareTo("small", TString::kIgnoreCase) == 0 ){
                LOG_INFO << "StFastGlauberMcMaker::InitRuRu  Set small R, large d" << endm;
                RError = 0.0 ;
                dError = 0.0 ;
        }
	else if ( type.CompareTo("gauss", TString::kIgnoreCase) == 0 ){
                LOG_INFO << "StFastGlauberMcMaker::InitRuRu  Gaussian collision profile" << endm;
                DoGaussianCollision() ;
        }
        else if ( type.CompareTo("smallNpp", TString::kIgnoreCase) == 0 ){
                mMode = 1 ;
                LOG_INFO << "StFastGlauberMcMaker::InitRuRu  Use small npp, large x for multiplicity (mMode = "
                        << mMode << ")"
                        << endm;
        }
        else if ( type.CompareTo("largeNpp", TString::kIgnoreCase) == 0 ){
                mMode = 2 ;
                LOG_INFO << "StFastGlauberMcMaker::InitRuRu  Use large npp, small x for multiplicity (mMode = "
                        << mMode << ")"
                        << endm;
        }

        return Init(A, R+RError, d+dError, beta2, beta4, A, R+RError, d+dError, beta2, beta4, type);
}

	


//____________________________________________________________________________________________________
Bool_t StFastGlauberMcMaker::IsCollision(Nucleon* nucleon0, Nucleon* nucleon1) const
{
	const Double_t dx     = nucleon0->GetX() - nucleon1->GetX() ;
	const Double_t dy     = nucleon0->GetY() - nucleon1->GetY() ;
	const Double_t dt     = TMath::Sqrt(dx*dx + dy*dy) ;
	const Double_t cutoff = TMath::Sqrt(mInelasticNNCrossSection/TMath::Pi());

	switch ( mCollisionProfile ){
		/// Hard-core collision profile (default)
		case mkHardCoreProfile:
			{
				return dt <= cutoff ;
			}

			/// Gaussian collision profile
		case mkGaussianProfile:
			{
				const Double_t sigma = cutoff ;
				const Double_t arg   = 0.5*dt/sigma ;
				return ( StGlauberUtilities::instance()->GetUniform() <= TMath::Exp(-arg*arg) );
			}

		default:
			{
				Error("StFastGlauberMcMaker::IsCollision", "No collision profile specified. see below");
				LOG_INFO << "  profile               description                         function" << endm;
				LOG_INFO << "  Hard-core profile    cut off distance  = sqrt(sigma/pi)   DoHardCoreCollision()" << endm;
				LOG_INFO << "  Gaussian profile     sigma of gaussian = sqrt(sigma/pi)   DoGaussianCollision()" << endm;
				LOG_INFO << endm;
				LOG_INFO << " d: transverse distrance between two nucleons (fm)" << endm;
				LOG_INFO << " sigma: Inelastic N-N cross section (fm^2)" << endm;
				assert(0);
			}
	}

	return kFALSE ;
}

//____________________________________________________________________________________________________
Int_t StFastGlauberMcMaker::Make()
{
	/// Clear all data members
	Clear() ;

	StGlauberUtilities* glauberUtilities = StGlauberUtilities::instance() ;

	/// Impact parameter
	const Double_t impactParameter = glauberUtilities->GetImpactParameter() ;
	mGlauberTree->SetB( impactParameter );

	//----------------------------------------------------------------------------------------------------
	/// 1. Generate nucleon positions (r,theta,phi)
	/// 2. Smearing nucleon positions if switch is ON (either DoHardCoreSmearing() or DoGaussianSmearing())
	/// 3. Distribute (r,theta,phi) with repulsion distance ds if ds != 0
	//----------------------------------------------------------------------------------------------------

	for(UInt_t in=0;in<2;in++){
		// Rotation angle (theta, phi) for deformed nuclei
		const Double_t Theta = (mIsDeformed[in]) ? glauberUtilities->GetTheta() : 0.0 ;
		const Double_t Phi   = (mIsDeformed[in]) ? glauberUtilities->GetPhi() : 0.0 ;
		mGlauberTree->SetTheta(in, Theta);
		mGlauberTree->SetPhi(in, Phi);

		/// Determine impact parameter for each nucleus
		const Double_t b = (in==0) ? -impactParameter/2.0 : impactParameter/2.0 ;

		UInt_t nNucleons = 0 ;
		while( nNucleons < mNucleons[in].size() ){
			Double_t r     = 0.0 ;
			Double_t theta = 0.0 ;
			Double_t phi   = 0.0 ;

			// Get (r,theta,phi) for nucleons
			GetRThetaPhi(in, r, theta, phi) ;

			mhWoodsSaxon[in]->Fill(r, 1.0/(r*r)) ;

			if( mRepulsionDistance == 0.0 ){
				/// No repulsion, just store (r,theta,phi)
				mNucleons[in][nNucleons]->Set(r, theta, phi, b, Theta, Phi, kTRUE) ;
				nNucleons++;
			}
			else{
				if(nNucleons==0){
					/// Store first nucleon, and increment total
					mNucleons[in][nNucleons]->Set(r, theta, phi, b, Theta, Phi, kTRUE) ;
					nNucleons++;
				}
				else{
					/// Check all the nucleons stored whether there are overlap each other
					/// defined by mRepulsionDistance. If any nucleons overlap, then discard
					/// current nucleon and try again
					Bool_t isOk = kFALSE ;
					do {
						const Double_t x = r*TMath::Sin(theta)*TMath::Cos(phi) ;
						const Double_t y = r*TMath::Sin(theta)*TMath::Sin(phi) ;
						const Double_t z = r*TMath::Cos(theta);

						Bool_t isOverlap = kFALSE ;
						for(UInt_t i=0; i<nNucleons; i++){
							const Double_t x0 = mNucleons[in][i]->GetX() ;
							const Double_t y0 = mNucleons[in][i]->GetY() ;
							const Double_t z0 = mNucleons[in][i]->GetZ() ;
							const Double_t dx = x - x0 ;
							const Double_t dy = y - y0 ;
							const Double_t dz = z - z0 ;

							// If any nucleons are found to be overlapped to the current one, stop the loop to save time
							if( TMath::Sqrt(dx*dx + dy*dy + dz*dz) <= mRepulsionDistance ){
								isOverlap = kTRUE ;
								break ;
							}
						}// Looping over other nucleons

						if(isOverlap){
							// There are overlap nucleons, try again
							isOk = kFALSE ;

							// Get (r,theta,phi) for nucleons
							GetRThetaPhi(in, r, theta, phi) ;
						}
						else{
							// if no nucleons are overlapped with the current one
							isOk = kTRUE ;
						}

					} while( !isOk ) ; // Find any overlaped nucleons ?

					// Add positions and increment total
					mNucleons[in][nNucleons]->Set(r, theta, phi, b, Theta, Phi, kTRUE) ;
					nNucleons++;
				}// Check overlap from 2nd nucleons
			}// mRepulsionDistance > 0
		}// Looping over all nucleons
	}// Nucleus loop

	//----------------------------------------------------------------------------------------------------
	/// Determine Number of collisions by looping over all pair of nucleons
	//----------------------------------------------------------------------------------------------------
	UInt_t Ncoll = 0 ;
	for(UInt_t in=0; in<mNucleons[0].size(); in++){
		Nucleon* nucleon0 = mNucleons[0][in] ;
		for(UInt_t jn=0; jn<mNucleons[1].size(); jn++){
			Nucleon* nucleon1 = mNucleons[1][jn] ;

			if(IsCollision(nucleon0, nucleon1)){
				nucleon0->IncrementNcoll() ;
				nucleon1->IncrementNcoll() ;

				Ncoll++;
			}
		}
	}

	//----------------------------------------------------------------------------------------------------
	/// Need at least one Ncoll
	//----------------------------------------------------------------------------------------------------
	if( Ncoll == 0 ) return 0 ;

	//----------------------------------------------------------------------------------------------------
	/// Determine Npart
	/// Calculate <x>, <y>, <x^2>, <y^2>, <xy>, ecc_{RP} and ecc_{PP}
	//----------------------------------------------------------------------------------------------------
	Double_t nSum[4] ;
	Double_t sumx[4] ;
	Double_t sumy[4] ;
	Double_t sumx2[4] ;
	Double_t sumy2[4] ;
	Double_t sumxy[4] ;

	for(Int_t i=0;i<4;i++){
		nSum[i] = 0.0 ;
		sumx[i] = 0.0 ;
		sumy[i] = 0.0 ;
		sumx2[i] = 0.0 ;
		sumy2[i] = 0.0 ;
		sumxy[i] = 0.0 ;
	}

	UInt_t Npart = 0 ;
	for(UInt_t in=0;in<2;in++){
		for(UInt_t i=0;i<mNucleons[in].size();i++){
			Nucleon* nucleon = mNucleons[in][i] ;
			const UInt_t ncoll = nucleon->GetNcoll() ;
			if( ncoll > 0 ){
				// Participant weight
				sumx[0]   += nucleon->GetXYZ("x");
				sumy[0]   += nucleon->GetXYZ("y");
				sumx2[0]  += nucleon->GetXYZ("xx");
				sumy2[0]  += nucleon->GetXYZ("yy");
				sumxy[0]  += nucleon->GetXYZ("xy");
				nSum[0]++ ; // Should be identical to Npart

				// Ncoll weight
				sumx[1]   += ncoll * nucleon->GetXYZ("x");
				sumy[1]   += ncoll * nucleon->GetXYZ("y");
				sumx2[1]  += ncoll * nucleon->GetXYZ("xx");
				sumy2[1]  += ncoll * nucleon->GetXYZ("yy");
				sumxy[1]  += ncoll * nucleon->GetXYZ("xy");
				nSum[1]   += ncoll ;

				// Multiplicity weight
				const Double_t mult = mCentralityMaker->GetNegativeBinomial()->GetTwoComponentMultiplicity(1.0, ncoll) ;
				nucleon->SetMultiplicity(mult);
				sumx[2]   += mult * nucleon->GetXYZ("x");
				sumy[2]   += mult * nucleon->GetXYZ("y");
				sumx2[2]  += mult * nucleon->GetXYZ("xx");
				sumy2[2]  += mult * nucleon->GetXYZ("yy");
				sumxy[2]  += mult * nucleon->GetXYZ("xy");
				nSum[2]   += mult ;

				nucleon->IncrementNpart() ;
				Npart++;
			}
			else{
				// Spectator
				sumx[3]   += nucleon->GetXYZ("x");
				sumy[3]   += nucleon->GetXYZ("y");
				sumx2[3]  += nucleon->GetXYZ("xx");
				sumy2[3]  += nucleon->GetXYZ("yy");
				sumxy[3]  += nucleon->GetXYZ("xy");

				nSum[3]++ ; // Should be identical to MassNumber - Npart
			}
		}// Nucleon loop
	}// Nucleus loop

	// Set multiplicity
	mGlauberTree->SetNpart(Npart) ;
	mGlauberTree->SetNcoll(Ncoll) ;

	// Get StNegativeBinomial with different (npp, x)
	const StNegativeBinomial* nbinomial = mCentralityMaker->GetNegativeBinomial() ;
	mGlauberTree->SetMultiplicity( nbinomial->GetMultiplicity(Npart, Ncoll) );

	// Get average and calculate eccentricity
	for(UInt_t i=0;i<4;i++){
		if(nSum[i]==0.0) continue ;

		sumx[i]  /= nSum[i] ;
		sumy[i]  /= nSum[i] ;
		sumx2[i] /= nSum[i] ;
		sumy2[i] /= nSum[i] ;
		sumxy[i] /= nSum[i] ;

		mGlauberTree->SetSumX(i, sumx[i]);
		mGlauberTree->SetSumY(i, sumy[i]);
		mGlauberTree->SetSumX2(i, sumx2[i]);
		mGlauberTree->SetSumY2(i, sumy2[i]);
		mGlauberTree->SetSumXY(i, sumxy[i]);

		// Eccentricity
		const Double_t sigmaX2  = sumx2[i] - sumx[i]*sumx[i] ;
		const Double_t sigmaY2  = sumy2[i] - sumy[i]*sumy[i] ;
		const Double_t sumSigma = sigmaX2 + sigmaY2 ;
		Double_t eccRP2 = -9999. ;
		Double_t eccPP2 = -9999. ;
		Double_t eccPP3 = -9999. ;
		Double_t eccPP4 = -9999. ;
		Double_t PP2 = -9999. ;
		Double_t PP3 = -9999. ;
		Double_t PP4 = -9999. ;

		if( sumSigma == 0.0 ){
			// Check denominator
			eccRP2 = -9999. ;
			eccPP2 = -9999. ;
			eccPP3 = -9999. ;
			eccPP4 = -9999. ;
			PP2 = -9999. ;
			PP3 = -9999. ;
			PP4 = -9999. ;
		}
		else{
			//----------------------------------------------------------------------------------------------------
			// Reaction plane eccentricity
			//----------------------------------------------------------------------------------------------------
			eccRP2 = (sigmaY2-sigmaX2)/sumSigma ;

			//----------------------------------------------------------------------------------------------------
			// The n-th order participant plane eccentricity
			//----------------------------------------------------------------------------------------------------
			for(Int_t io=0; io<3; io++){
				const Double_t order = io + 2.0 ;
				TGraph* qpart = GetParticipantEccentricity(order, sumx[i], sumy[i], nSum[i], i);
				if( io == 0 ) { PP2 = qpart->GetY()[2] ;  eccPP2 = qpart->GetY()[3] ; }
				if( io == 1 ) { PP3 = qpart->GetY()[2] ;  eccPP3 = qpart->GetY()[3] ; }
				if( io == 2 ) { PP4 = qpart->GetY()[2] ;  eccPP4 = qpart->GetY()[3] ; }
				delete qpart ;
			}
		}

		mGlauberTree->SetEccRP2(i, eccRP2);
		mGlauberTree->SetEccPP2(i, eccPP2);
		mGlauberTree->SetEccPP3(i, eccPP3);
		mGlauberTree->SetEccPP4(i, eccPP4);
		mGlauberTree->SetPP2(i, PP2);
		mGlauberTree->SetPP3(i, PP3);
		mGlauberTree->SetPP4(i, PP4);
	}

	return 1;
}

//____________________________________________________________________________________________________
Int_t StFastGlauberMcMaker::Run(const UInt_t nevents)
{
	LOG_INFO << "StFastGlauberMcMaker::Run  Run " << nevents << " events ..." << endm;

	while( mNeventsAccept < nevents ){
		if ( Make() == 1 ){
			// Debug
			if ( mNeventsAccept % 100 == 0 ){
				LOG_INFO << Form("StFastGlauberMcMaker::Run  (accept/throw, Npart, Ncoll, b) = (%5d/%5d, %4d, %4d, %1.2f fm)",
						mNeventsAccept, mNeventsThrow, mGlauberTree->GetNpart(), mGlauberTree->GetNcoll(),
						mGlauberTree->GetB())
					<< endm;
			}

			if(mDebug){
				LOG_INFO << Form("StFastGlauberMcMaker::Run  (accept/throw, Npart, Ncoll, b) = (%5d/%5d, %4d, %4d, %1.2f fm)",
						mNeventsAccept, mNeventsThrow, mGlauberTree->GetNpart(), mGlauberTree->GetNcoll(),
						mGlauberTree->GetB())
					<< endm;
			}

			// Fill tree
			mGlauberTree->Fill() ;

			mNeventsAccept++;
		}

		mNeventsThrow++;
	}

	return 1;
}

//____________________________________________________________________________________________________
Int_t StFastGlauberMcMaker::Finish()
{
	// Store header info.

	// Name of nucleus
	const UInt_t A[2] = {mNucleons[0].size(), mNucleons[0].size()};
	mGlauberTree->SetNameNucleusA(GetName(A[0]));
	mGlauberTree->SetNameNucleusB(GetName(A[1]));

	// Mass numbers
	mGlauberTree->SetMassNumberA(A[0]);
	mGlauberTree->SetMassNumberB(A[1]);

	// Radius
	mGlauberTree->SetRadiusA((mIsDeformed[0]) ? mfWoodsSaxon2D[0]->GetParameter(0) : mfWoodsSaxon[0]->GetParameter(0));
	mGlauberTree->SetRadiusB((mIsDeformed[1]) ? mfWoodsSaxon2D[1]->GetParameter(0) : mfWoodsSaxon[1]->GetParameter(0));

	// Skin depth
	mGlauberTree->SetSkinDepthA((mIsDeformed[0]) ? mfWoodsSaxon2D[0]->GetParameter(1) : mfWoodsSaxon[0]->GetParameter(1));
	mGlauberTree->SetSkinDepthB((mIsDeformed[1]) ? mfWoodsSaxon2D[1]->GetParameter(1) : mfWoodsSaxon[1]->GetParameter(1));

	// Deformation parameters
	if(mIsDeformed[0]){
		mGlauberTree->SetBeta2A(mfWoodsSaxon2D[0]->GetParameter(2));
		mGlauberTree->SetBeta4A(mfWoodsSaxon2D[0]->GetParameter(3));
	}
	if(mIsDeformed[1]){
		mGlauberTree->SetBeta2A(mfWoodsSaxon2D[1]->GetParameter(2));
		mGlauberTree->SetBeta4A(mfWoodsSaxon2D[1]->GetParameter(3));
	}

	// sigmaNN, sqrt(sNN)
	mGlauberTree->SetSigmaNN( mInelasticNNCrossSection * 10.0 ) ; // mb
	mGlauberTree->SetSqrtSNN( mEnergy ) ;

	// Repulsion distance
	mGlauberTree->SetRepulsionD(mRepulsionDistance);

	// Total cross section (mb) skip if no events have been processed
	Double_t totalX      = 0.0 ;
	Double_t totalXError = 0.0 ;
	const Double_t bmax  = StGlauberUtilities::instance()->GetMaximumImpactParameter() ;

	if( mNeventsAccept != 0 ){
		const Double_t scaleFactor = TMath::Pi() * bmax * bmax * 10.0 ;
		totalX      = (Double_t)mNeventsAccept / (Double_t)mNeventsThrow * scaleFactor ;
		totalXError = totalX * TMath::Sqrt(1.0/mNeventsAccept + 1.0/mNeventsThrow) ;
	}
	mGlauberTree->SetTotalXsec(totalX) ;
	mGlauberTree->SetTotalXsecError(totalXError) ;

	// Smearing options (0 or 1)
	//  UInt_t smearHardCore = (mHardCoreSmearing) ? 1 : 0 ;
	//  UInt_t smearGaussian = (mGaussianSmearing) ? 1 : 0 ;
	mGlauberTree->SetSmearHardCore(mHardCoreSmearing);
	mGlauberTree->SetSmearGaussian(mGaussianSmearing);

	// Collision profile options (0 or 1)
	UInt_t collisionHardCore = (mCollisionProfile==mkHardCoreProfile) ? 1 : 0 ;
	UInt_t collisionGaussian = (mCollisionProfile==mkGaussianProfile) ? 1 : 0 ;
	mGlauberTree->SetCollisionHardCore(collisionHardCore);
	mGlauberTree->SetCollisionGaussian(collisionGaussian);

	// Maximum impact parameter
	mGlauberTree->SetBMax(bmax);

	// Number of events
	mGlauberTree->SetNeventsAccept(mNeventsAccept);
	mGlauberTree->SetNeventsThrow(mNeventsThrow);

	// NBD parameters
	const StNegativeBinomial* nb = mCentralityMaker->GetNegativeBinomial() ;
	mGlauberTree->SetNpp( nb->GetNpp() );
	mGlauberTree->SetK( nb->GetK() );
	mGlauberTree->SetX( nb->GetX() );
	mGlauberTree->SetEfficiency( nb->GetEfficiency() );
	mGlauberTree->SetIsConstEfficiency( nb->IsConstEfficiency() );

	// version
	mGlauberTree->SetVersion(mVersion);
	mGlauberTree->FillHeader() ;

	LOG_INFO << "StFastGlauberMcMaker::Finish  Close ROOT file" << endm;
	mGlauberTree->Close();

	return 1;
}

//____________________________________________________________________________________________________
void StFastGlauberMcMaker::DoHardCoreSmearing()
{
	mHardCoreSmearing = kTRUE ;
	LOG_INFO << "StFastGlauberMcMaker::DoHardCoreSmearing  Hard-core smearing in (x,y,z) ";
	LOG_INFO << "by using the step function with inelastic nn cross section" << endm;

	/// Turn off Gaussian smearing
	mGaussianSmearing = kFALSE ;
	if(mDebug){
		LOG_INFO << "StFastGlauberMcMaker::DoHardCoreSmearing  Gaussian smearing turned off" << endm;
	}
}

//____________________________________________________________________________________________________
void StFastGlauberMcMaker::DoGaussianSmearing()
{
	mGaussianSmearing = kTRUE ;
	LOG_INFO << "StFastGlauberMcMaker::DoGaussianSmearing  gaussian smearing in (x,y,z) ";
	LOG_INFO << "by using the gaussian function with fixed sigma" << endm;

	/// Turn off Hard-core smearing
	mHardCoreSmearing = kFALSE ;
	if(mDebug){
		LOG_INFO << "StFastGlauberMcMaker::DoGaussianSmearing  hard-core smearing turned off" << endm;
	}
}

//____________________________________________________________________________________________________
void StFastGlauberMcMaker::DoHardCoreCollision()
{
	/// Hard-core collision
	mCollisionProfile = mkHardCoreProfile ;
	LOG_INFO << "StFastGlauberMcMaker::DoHardCoreCollision  hard-core n-n collision "
		<< "  cut off distance is sqrt(sigma/pi) = " << TMath::Sqrt(mInelasticNNCrossSection/TMath::Pi())
		<< endm;
}

//____________________________________________________________________________________________________
void StFastGlauberMcMaker::DoGaussianCollision()
{
	/// Gaussian profile collision
	mCollisionProfile = mkGaussianProfile ;
	LOG_INFO << "StFastGlauberMcMaker::DoGaussianCollision  gaussian profile n-n collision "
		<< "  sigma of gaussian is sqrt(sigma/pi) = " << TMath::Sqrt(mInelasticNNCrossSection/TMath::Pi())
		<< endm;
}

//----------------------------------------------------------------------------------------------------
// Utilities
//----------------------------------------------------------------------------------------------------

//____________________________________________________________________________________________________
const Char_t* StFastGlauberMcMaker::GetName(const UInt_t massNumber) const
{
	switch ( massNumber ){
		case 63: return "Cu";
		case 144: return "Sm";
		case 154: return "Sm";
		case 197: return "Au";
		case 208: return "Pb";
		case 238: return "U";
		//ZS The following should be changed to include Ru
		case 96:  return "Zr";
		default:
				  Error("StFastGlauberMcMaker::GetName", "can't find mass number = %d. Return white space", massNumber);
				  return "";
	}

	return "";
}

//____________________________________________________________________________________________________
Double_t StFastGlauberMcMaker::GetInelasticNNCrossSection(const Double_t energy, const TString type) const
{
	/// Cast to integer
	const UInt_t energyInt = static_cast<UInt_t>(energy) ;

	/// Assign error +/- 1mb if type is either largeXsec (+1mb) or smallXsec (-1mb)
	Double_t error = 0.0 ; // absolute error on cross section
	if( type.CompareTo("smallXsec", TString::kIgnoreCase) == 0 )      error = -0.1 ;
	else if( type.CompareTo("largeXsec", TString::kIgnoreCase) == 0 ) error = 0.1 ;

	Double_t sigmaNN = 0.0 ;
	switch ( energyInt ) {
		case 2760: sigmaNN = 6.4 + error ; break ; // 64 mb
		case 200: sigmaNN = 4.2 + error ; break ; // 42 mb
		case 62:  sigmaNN = 3.6 + error ; break ; // 36 mb
		case 39:  sigmaNN = 3.4 + error ; break ; // 34 mb
		case 27:  sigmaNN = 3.3 + error ; break ; // 33 mb
		case 19:  sigmaNN = 3.2 + error ; break ; // 32 mb
		case 14:  sigmaNN = 3.15 + error; break ; // 31.5 mb (for 14.5 GeV)
		case 11:  sigmaNN = 3.12 + error; break ; // 31.2 mb (for 11.5 GeV)
		case 7:   sigmaNN = 3.08 + error; break ; // 30.8 mb (for 7.7 GeV)
		default:
				  Error("StFastGlauberMcMaker::GetInelasticNNCrossSection", "No energy = %1.1f GeV is available. abort", energy);
				  assert(0);
	}

	if(mDebug){
		LOG_INFO << "StFastGlauberMcMaker::GetInelasticNNCrossSection  ";
		LOG_INFO << "Inelastic NN cross section = " << sigmaNN * 10 << " (mb) for sqrt(sNN) = " << energy
			<< endm;
	}

	return sigmaNN ;
}

//____________________________________________________________________________________________________
void StFastGlauberMcMaker::GetRThetaPhi(const UInt_t inucleus, Double_t& r, Double_t& theta, Double_t& phi) const
{
	if( mIsDeformed[inucleus] ){
		// Deformed nuclei
		Double_t cosTheta = -9999. ;

		// Random (r, cos(theta))
		mfWoodsSaxon2D[inucleus]->GetRandom2(r, cosTheta) ;
		theta = TMath::ACos(cosTheta) ; // convert into theta
	}
	else{
		// Spherical nuclei
		r     = mfWoodsSaxon[inucleus]->GetRandom() ;
		theta = StGlauberUtilities::instance()->GetTheta() ; // cos(theta) profile in 0 < theta < pi
	}
	phi = StGlauberUtilities::instance()->GetPhi() ; // flat phi in -pi < phi < pi

	// Smearing if the switch is ON
	Smearing(r, theta, phi) ;
}

//____________________________________________________________________________________________________
void StFastGlauberMcMaker::Smearing(Double_t& r, Double_t& theta, Double_t& phi) const
{
	// Return original if both smearing options are OFF
	if(!mHardCoreSmearing && !mGaussianSmearing) return;

	// Hard-core smearing
	if(mHardCoreSmearing){
		Double_t dx, dy, dz ; /// Smearing width
		mfHardCore->GetRandom3(dx, dy, dz) ;

		if(mDebug){
			LOG_INFO << Form("StFastGlauberMcMaker::Smearing  Do hard-core smearing : (dx,dy,dz)=(%1.3f,%1.3f,%1.3f) fm",
					dx,dy,dz) << endm; 
		}

		const Double_t x = r * TMath::Sin(theta) * TMath::Cos(phi) + dx ;
		const Double_t y = r * TMath::Sin(theta) * TMath::Sin(phi) + dy ;
		const Double_t z = r * TMath::Cos(theta) + dz ;

		r     = TMath::Sqrt(x*x + y*y + z*z) ;
		theta = TMath::ACos(z/r) ;
		phi   = TMath::ATan2(y, x) ;
		return;
	}

	// Gaussian smearing
	if(mGaussianSmearing){
		Double_t dx, dy, dz ; /// Smearing width
		mfGaussian->GetRandom3(dx, dy, dz) ;

		if(mDebug){
			LOG_INFO << Form("StFastGlauberMcMaker::Smearing  Do gaussian smearing : (dx,dy,dz)=(%1.3f,%1.3f,%1.3f) fm",
					dx,dy,dz) << endm; 
		}

		const Double_t x = r * TMath::Sin(theta) * TMath::Cos(phi) + dx ;
		const Double_t y = r * TMath::Sin(theta) * TMath::Sin(phi) + dy ;
		const Double_t z = r * TMath::Cos(theta) + dz ;

		r     = TMath::Sqrt(x*x + y*y + z*z) ;
		theta = TMath::ACos(z/r) ;
		phi   = TMath::ATan2(y, x) ;
		return;
	}
}

//____________________________________________________________________________________________________
TGraph* StFastGlauberMcMaker::GetParticipantEccentricity(const Double_t order, const Double_t sumx, const Double_t sumy,
		const Double_t sumw, const UInt_t weightId) const
{
	Double_t qx = 0.0 ;
	Double_t qy = 0.0 ;
	Double_t qw = 0.0 ;

	for(UInt_t in=0;in<2;in++){
		for(UInt_t j=0;j<mNucleons[in].size();j++){
			Nucleon* nucleon = mNucleons[in][j] ;
			const UInt_t ncoll = nucleon->GetNcoll() ;

			const Bool_t isOk = (weightId==3 && ncoll==0) // Spectator
				|| (weightId!=3 && ncoll > 0) ; // Participants

			if(!isOk) continue ;

			const Double_t x = nucleon->GetX() - sumx ;
			const Double_t y = nucleon->GetY() - sumy ;
			const Double_t z = nucleon->GetZ() ;

			const TVector3 vpart(x, y, z);
			const Double_t rt  = vpart.Perp() ;
			const Double_t phi = vpart.Phi() ;
			Double_t weight = 1.0 ;
			if( weightId == 1 ) weight = ncoll ;
			if( weightId == 2 ) weight = nucleon->GetMultiplicity() ; 
			if( weightId == 3 ) weight = 1.0 ;

			qx += -weight * rt*rt * TMath::Cos(order*phi) ; // -sign added to keep the same definition of participant plane
			qy +=  weight * rt*rt * TMath::Sin(order*phi) ;
			qw +=  weight * rt*rt ;
		}
	}

	qx /= sumw ;
	qy /= sumw ;
	qw /= sumw ;

	TGraph* g = new TGraph() ; // needs to be deleted later
	g->SetPoint(0, 0, qx);
	g->SetPoint(1, 1, qy);
	g->SetPoint(2, 2, TMath::ATan2(qy, qx));
	g->SetPoint(3, 3, TMath::Sqrt(qx*qx + qy*qy)/qw) ;

	return g ;
}

//____________________________________________________________________________________________________
void StFastGlauberMcMaker::DebugOn()
{
	mDebug = 1 ;
	LOG_INFO << "StFastGlauberMcMaker::DebugOn  Set debug mode, will see a bunch of debugging messages" << endm;
}

//____________________________________________________________________________________________________
void StFastGlauberMcMaker::Print(const TString option) const
{
	if( option.CompareTo("type", TString::kIgnoreCase) == 0 ){
		LOG_INFO << endm;
		LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm;
		LOG_INFO << "StFastGlauberMcMaker::Print  Current available types are:" << endm;
		LOG_INFO << "type               description" << endm;
		LOG_INFO << " default" << endm;
		LOG_INFO << " large              Large R(+2%), small d(-10%)" << endm;
		LOG_INFO << " small              Small R(-2%), large d(+10%)" << endm;
		LOG_INFO << " largeXsec          Large inelastic NN cross section (+1mb)" << endm;
		LOG_INFO << " smallXsec          Small inelastic NN cross section (-1mb)" << endm;
		LOG_INFO << " gauss              Use gaussian collision profile" << endm;
		LOG_INFO << " largeNpp           Use large Npp, small x" << endm;
		LOG_INFO << " smallNpp           Use small Npp, large x" << endm;
		LOG_INFO << "----------------------------------------------------------------------------------------------------" << endm;
		LOG_INFO << "NOTE:  Types below are not relevant for generating trees." << endm;
		LOG_INFO << "NOTE:  Only important for making histograms by StGlauberAnalysisMaker." << endm;
		LOG_INFO << "NOTE:  Users should use the default outputs and modify type to see " << endm;
		LOG_INFO << "NOTE:  the effect of different total cross section, for example" << endm;
		LOG_INFO << endm;
		LOG_INFO << " largeTotal         +5% total cross section" << endm;
		LOG_INFO << " smallTotal         -5% total cross section" << endm;
		LOG_INFO << " lowrw              +2(-2) sigma p0 (p1) parameter for re-weighting correction" << endm;
		LOG_INFO << " highrw             -2(+2) sigma p0 (p1) parameter for re-weighting correction" << endm;
		LOG_INFO << "#----------------------------------------------------------------------------------------------------" << endm;
		LOG_INFO << endm;
	}
}

//____________________________________________________________________________________________________
UInt_t StFastGlauberMcMaker::Version() const
{
	LOG_INFO << endm << endm;
	LOG_INFO << "StFastGlauberMcMaker::Version  Current StFastGlauberMcMaker version is " << mVersion << endm;
	Print("type");
	LOG_INFO << endm << endm;

	return mVersion ;
}

