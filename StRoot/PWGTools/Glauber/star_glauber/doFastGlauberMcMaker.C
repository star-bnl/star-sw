//****************************************************************************************************
//  Run Fast Monte Carlo Glauber simulation (StFastGlauberMcMaker)
//****************************************************************************************************
/****************************************************************************************************
 * $Id: doFastGlauberMcMaker.C,v 1.3 2012/04/25 05:29:34 hmasui Exp $
 * $Log: doFastGlauberMcMaker.C,v $
 * Revision 1.3  2012/04/25 05:29:34  hmasui
 * Added deformation flag
 *
 * Revision 1.2  2010/10/27 00:31:45  hmasui
 * Added loading relevant STAR libs
 *
 * Revision 1.1  2010/06/24 23:41:40  hmasui
 * Macro to run StFastGlauberMcMaker
 *
 ****************************************************************************************************/

//____________________________________________________________________________________________________
// Specify the system by TString, detailed parameters will be defined inside StFastGlauberMcMaker 
void doFastGlauberMcMaker
(
 const TString outputFileName = "fastmcglauber.root",
 const UInt_t nevents = 1000,
 const TString system = "AuAu",
 const Double_t energy = 200,
 const TString type = "default",
 const Bool_t isDeformed = kFALSE,
 const Double_t replusionDistance = 0.0
 )
{
	gBenchmark->Start("doFastGlauberMcMaker");

	gSystem->Load("St_base");
	gSystem->Load("StUtilities");
	gSystem->Load("StGlauberUtilities");
	gSystem->Load("StCentralityMaker");
	gSystem->Load("StGlauberTree");
	gSystem->Load("StFastGlauberMcMaker");

	StFastGlauberMcMaker* maker = new StFastGlauberMcMaker(outputFileName, system, energy, type, isDeformed);
	//  maker->DebugOn();

	// Print available types
	maker->Print("type");

	// Smearing option will be added in type later
	//  maker->DoHardCoreSmearing() ; // Hard-core smearing ON
	//  maker->DoGaussianSmearing() ; // Gaussian smearing ON

	// Repulsion option will be added in type later
	//  maker->SetRepulsionDistance(replusionDistance); // Repulsion distance between two nucleons

	// Collision profile were already added (default is hard-core)
	// specify "gauss" in type for Gaussian collision profile
	//  if( collisionProfile == 0 ) maker->DoHardCoreCollision() ;
	//  if( collisionProfile == 1 ) maker->DoGaussianCollision() ;

	// Run nevents
	maker->Run(nevents);

	// Close ROOT file etc
	maker->Finish();

	gBenchmark->Stop("doFastGlauberMcMaker");
	gBenchmark->Show("doFastGlauberMcMaker");
	gBenchmark->Reset();
}
//____________________________________________________________________________________________________
//  Specify all parameters by hand
void _doFastGlauberMcMaker
(
 const Char_t* outputFileName,
 const UInt_t nevents = 1000,
 const UInt_t A = 197,
 const Double_t R = 6.38,
 const Double_t d = 0.535,
 const Double_t beta2 = -0.131,
 const Double_t beta4 = -0.031,
 const Double_t sigma = 4.2,
 const Double_t energy = 200,
 const Double_t replusionDistance = 0.9,
 const UInt_t collisionProfile = 0 // 0=hard-core (default), 1=gaussian profile
 )
{
	gBenchmark->Start("doFastGlauberMcMaker");
	gSystem->Load("StGlauberUtilities");
	gSystem->Load("StCentralityMaker");
	gSystem->Load("StFastGlauberMcMaker");

	StFastGlauberMcMaker* maker = new StFastGlauberMcMaker(outputFileName, A, R, d, beta2, beta4, sigma, energy);

	// Hard-core smearing ON
	//  maker->DoHardCoreSmearing() ;

	// Gaussian smearing ON
	//  maker->DoGaussianSmearing() ;

	// Repulsion distance between two nucleons
	maker->SetRepulsionDistance(replusionDistance);

	// Collision profile
	if( collisionProfile == 0 ) maker->DoHardCoreCollision() ;
	if( collisionProfile == 1 ) maker->DoGaussianCollision() ;

	// Run nevents
	maker->Run(nevents);

	// Close ROOT file etc
	maker->Finish();

	gBenchmark->Stop("doFastGlauberMcMaker");
	gBenchmark->Show("doFastGlauberMcMaker");
	gBenchmark->Reset();
}
