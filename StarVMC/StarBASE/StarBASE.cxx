// $Id $

#include <assert.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TGeometry.h"
#include "TGeoManager.h"
#include "TObjectSet.h"
#include "StarBASE.h"
#include "Stiostream.h"
#include "StarMagField.h"
#include "StVMCApplication.h"
#include "StMCStack.h"
#include "StMessMgr.h"
#include "TVirtualMC.h"
#include "TGeant3TGeo.h"
#include "StMCInitApp.h"
#include "StMCStepping.h"
#include "StMCSimplePrimaryGenerator.h"
#include "TFile.h"
#include "TH2F.h"
#include "TVector3.h"
#include "TPaveText.h"

#include "TRandom3.h"

ClassImp(StarBASE);

//_____________________________________________________________________________
StarBASE::StarBASE(const char *name,const char *gy, int trig)
  :StMaker(name),fNTrig(trig),fGeo(gy)
{
  gSystem->AddIncludePath("${STAR}2/StarDb/VmcGeometry");
  mZ=0; mS=0;
}

//_____________________________________________________________________________
int StarBASE::Init() 
{

  StVMCApplication *app = new StVMCApplication(fGeo, "StVMC application");
  StMCInitApp *ini = new StMCInitApp();  
  app->SetInit(ini);
  app->Init();

  // StMCStepping is responsible for propagating tracks from one step
  // in the detector to the next.
  StMCStepping *steps = new StMCStepping(fGeo);
  mSteps=steps;
  app->SetStepping( steps );

  if (*SAttr("SteppingDebug")) {steps->SetDebug(IAttr("SteppingDebug"));}  

  mGenerator = ini->generator();

  return StMaker::Init();
}
//_____________________________________________________________________________
int StarBASE::InitRun  (int runumber)
{
  return kStOK;
}
//_____________________________________________________________________________

int StarBASE::Make()
{

  //
  // Setup fiducial volume in which to scan the detector
  //
  Float_t dphi = 0.0;
  Float_t deta = 0.0;
  Float_t min_phi, max_phi, min_eta, max_eta;

  Int_t nsample = 4;
 
  if ( *SAttr("eta_min") )       min_eta=DAttr("eta_min");
  if ( *SAttr("eta_max") )       max_eta=DAttr("eta_max");
  if ( *SAttr("deta")    )       deta   =DAttr("deta");

  if ( *SAttr("phi_min") )       min_phi=DAttr("phi_min");
  if ( *SAttr("phi_max") )       max_phi=DAttr("phi_max");
  if ( *SAttr("dphi")    )       dphi   =DAttr("dphi");

  if ( *SAttr("sample")  )       nsample=IAttr("sample");

  //  std::cout << Form("======================== nsample = %i ========================",nsample) << std::endl;
  std::cout << Form("StarBASE: %5.2f < eta < %5.2f    ",min_eta,max_eta) << std::endl;
  std::cout << Form("StarBASE: %5.2f < phi < %5.2f deg",min_phi,max_phi) << std::endl;
  std::cout << Form("StarBASE: deta = %5.3f    ", deta) << std::endl;
  std::cout << Form("StarBASE: dphi = %5.3f deg", dphi) << std::endl;

  assert( dphi > 0.0 && deta > 0.0 );

  TRandom3 *random = new TRandom3(1234);


  //
  // Scan the detector
  //
  Float_t phi, eta;
  for ( phi=min_phi+dphi/2; phi<max_phi; phi+=dphi ) 
    {

      for ( eta=min_eta+deta/2; eta<max_eta; eta+=deta )
	{

	  Double_t vertex=random->Gaus(mZ,mS); // smear by 1 cm in z 
	  Double_t zmin = (1.0-0.00001)*vertex;
	  Double_t zmax = (1.0+0.00001)*vertex;

	  if ( *SAttr("sample") ) 
	    {
	      mGenerator->SetGenerator( nsample, 48, 10, 10, eta-deta/2, eta+deta/2, phi-dphi/2, phi+dphi/2, zmin, zmax, "G" );
	    }
	  else 
	    {
	      mGenerator->SetGenerator( 1, 48, 10, 10, eta, eta, phi, phi, zmin, zmax, "G" );
	    }
	  TVirtualMC::GetMC()->ProcessEvent();

	  // Just once through
	  //	  return kStOK;

	}

    }
	
  return kStOK;

}


//_____________________________________________________________________________
int StarBASE::Finish()
{


  Float_t dphi = 0.0;
  Float_t deta = 0.0;
  Float_t eta_min, eta_max;
  Float_t phi_min, phi_max;

  Int_t nsample = 4;
 
  if ( *SAttr("eta_min") )       eta_min=DAttr("eta_min");
  if ( *SAttr("eta_max") )       eta_max=DAttr("eta_max");
  if ( *SAttr("deta")    )       deta   =DAttr("deta");

  if ( *SAttr("phi_min") )       phi_min=DAttr("phi_min");
  if ( *SAttr("phi_max") )       phi_max=DAttr("phi_max");
  if ( *SAttr("dphi")    )       dphi   =DAttr("dphi");

  if ( *SAttr("sample")  )       nsample=IAttr("sample");

  //  TPaveText *pave = new TPaveText(0.7, 0.6, 0.875, 0.875);
  TPaveText *pave = new TPaveText(395.0,200.0,795.0,400.0);
  pave -> AddText( Form("%5.3f < #eta < %5.3f",eta_min,eta_max) );
  pave -> AddText( Form("%5.1f#circ < #phi < %5.1f#circ",phi_min,phi_max) );
  pave -> AddText( Form("d#eta=%5.3f d#phi=%5.1f#circ",deta,dphi) );
  mSteps->h_radlen_rz->GetListOfFunctions()->Add(pave);
  mSteps->h_abslen_rz->GetListOfFunctions()->Add(pave);

  AddHist( mSteps->h_ncount_rz );
  AddHist( mSteps->h_radlen_rz );
  AddHist( mSteps->h_abslen_rz );

  for ( Int_t i=0;i<mSteps->mNumberOfVolumes+1;i++ )
    {
      if ( mSteps->hRadlenHist1D[i] ) {
	AddHist( mSteps->hRadlenHist1D[i] );
	AddHist( mSteps->hCountsHist1D[i] );
	AddHist( mSteps->hRadlenAccu1D[i] );

	AddHist( mSteps->hRadlenHist1D_z[i] );
	AddHist( mSteps->hCountsHist1D_z[i] );

	AddHist( mSteps->hRadlenHist1D_r[i] );
	AddHist( mSteps->hCountsHist1D_r[i] );

	AddHist( mSteps->hRadlenHist1D_phi[i] );
	AddHist( mSteps->hCountsHist1D_phi[i] );

	//	std::cout << mSteps->hRadlenAccu1D[i]->GetName() << std::endl;
	
	//	AddHist( mSteps->hEnterHist1D[i] );
	//	AddHist( mSteps->hExitHist1D[i] );
      }
    }

  return StMaker::Finish();
}

//________________________________________________________________________________
void StarBASE::SetDebug(int l) {
  StMaker::SetDebug(l);
}
