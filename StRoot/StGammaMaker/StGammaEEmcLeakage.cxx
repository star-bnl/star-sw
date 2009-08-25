#include "StGammaEEmcLeakage.h"
ClassImp(StGammaEEmcLeakage);

#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StGammaTower.h"

#include <TVector2.h>
#include <TFile.h>
#include <TH2F.h>
#include <TMarker.h>
#include <iostream>
#include <cassert>


StGammaEEmcLeakage *StGammaEEmcLeakage::sInstance = 0;

StGammaEEmcLeakage *StGammaEEmcLeakage::instance(){
  if ( !sInstance )
    {
      sInstance=new StGammaEEmcLeakage();
    }
  return sInstance;
}

StGammaEEmcLeakage::StGammaEEmcLeakage()
{
  //  const Char_t *fname = "share-shape1.root"; 
  const Char_t *fname = "/afs/rhic.bnl.gov/star/users/jwebb/2008/tower-leakage/eemc-shape.root";
  const Int_t numberOfEtabins=12;

  mFile=new TFile(fname);
  assert(mFile);

  mNumberOfEtabins=numberOfEtabins;
  for ( Int_t i=0;i<numberOfEtabins;i++ )
    {
      mEnergyFractions.push_back( (TH2F*)mFile->Get( Form("h12TC%02ifrac",i+1) ) );
      mEnergyFractions.back()->Print();
    }
  mEEmcGeom=new EEmcGeomSimple();
}

StGammaEEmcLeakage::~StGammaEEmcLeakage()
{
  mEnergyFractions.clear();
  mFile->Close();
  delete mFile;
}

Float_t StGammaEEmcLeakage::expectation( const TVector3 &gamma )
{

  const Float_t zplane = 288.0;

  TVector3 position = gamma;
  position.SetMagThetaPhi( zplane / position.CosTheta(),
			   position.Theta(),
			   position.Phi() );

  // Get the centroid of the requested tower (should be but not required to be the same vector)
  Int_t tSec, tSub, tEta;
  if ( !mEEmcGeom -> getTower( position, tSec, tSub, tEta ) ) return 0.0; // passed invalid tower

  TVector3 tTower = mEEmcGeom->getTowerCenter( (UInt_t)tSec, (UInt_t)tSub, (UInt_t)tEta );
  
  tTower.SetMagThetaPhi( zplane / tTower.CosTheta(),
			 tTower.Theta(),
			 tTower.Phi() );

  // 2D histogram which contains the fraction of the photon's energy
  TH2F *hFraction = mEnergyFractions[ tEta ];

  // angle between gamma vector and tower vector (may have botched the sign here... but it should
  // be symmetric (and we will enforce ths below!)
  Float_t phi = TVector2::Phi_mpi_pi( position.Phi() - tTower.Phi() );

  // the following distances should be measured in [cm]
  Float_t D_eta = position.Perp() * TMath::Cos( phi ) - tTower.Perp();
  Float_t D_phi = position.Perp() * TMath::Sin( phi );

  //  std::cout << "find bin D_phi, D_eta = " << D_phi << ", " << D_eta << std::endl;

  Int_t bin1 = hFraction -> FindBin( +D_phi, D_eta );
  Int_t bin2 = hFraction -> FindBin( -D_phi, D_eta );


  // return nothing on over/underflow
  Int_t nmax=hFraction->GetNbinsX();
  Int_t nmay=hFraction->GetNbinsY();
  nmax*=nmay;
  if ( bin1<=0 || bin2<=0 ) {
    std::cout << "bin <= 0" << std::endl;
    return 0.0; 
  }
  if ( bin1 > nmax || bin2 > nmax ) {
    std::cout << "bin > max" << std::endl;
    return 0.0;
  }

  Float_t frac1 = hFraction -> GetBinContent( bin1 );
  Float_t frac2 = hFraction -> GetBinContent( bin2 );

  Float_t f = (frac1+frac2)/2.0;

  if ( f < 0.5  ) 
    {
      std::cout << "=========================================================+" << std::endl;
      std::cout << "frac=" << f << std::endl;
      //      position.Print();
      //      tTower.Print();
      std::cout << "D_eta=" << D_eta<<" D_phi=" << D_phi << std::endl;
      std::cout << "bin1=" << bin1 << " bin2=" << bin2 << " frac1=" << frac1 << " frac2=" << frac2 << std::endl;

      //      std::cout << "gamma position is" << std::endl;
      //      position.Print();
      //      const Char_t *subs[]={"A","B","C","D","E","X"};
      //      std::cout << Form("tower containing photon returned as %02iT%s%02i",tSec+1,subs[tSub],tEta+1) << std::endl;

    }

  return f;

}


TCanvas *
StGammaEEmcLeakage::draw(  const TVector3 &gamma )
{

  const Float_t zplane = 288.0;
  TVector3 position = gamma;
  position.SetMagThetaPhi( zplane / position.CosTheta(),
			   position.Theta(),
			   position.Phi() );


  // Get the centroid of the requested tower (should be but not required to be the same vector)
  Int_t tSec, tSub, tEta;
  if ( !mEEmcGeom -> getTower( position, tSec, tSub, tEta ) ) return NULL; // passed invalid tower

  //  if ( !mEEmcGeom -> getTower( tower, tSec, tSub, tEta ) ) return NULL;
  TVector3 tTower = mEEmcGeom->getTowerCenter( (UInt_t)tSec, (UInt_t)tSub, (UInt_t)tEta );

  tTower.SetMagThetaPhi( zplane / tTower.CosTheta(),
			 tTower.Theta(),
			 tTower.Phi() );


  // 2D histogram which contains the fraction of the photon's energy
  TH2F *hFraction = mEnergyFractions[ tEta ];
  TCanvas *c = new TCanvas();
  hFraction->Draw("colz");

  // angle between gamma vector and tower vector (may have botched the sign here... but it should
  // be symmetric (and we will enforce ths below!)
  Float_t phi = TVector2::Phi_mpi_pi( gamma.Phi() - tTower.Phi() );

  // the following distances should be measured in [cm]
  Float_t D_eta = position.Perp() * TMath::Cos( phi ) - tTower.Perp();
  Float_t D_phi = position.Perp() * TMath::Sin( phi );

  TMarker *m=new TMarker(D_phi,D_eta,22);

  m->Draw();

  return c;

}
