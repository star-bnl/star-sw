/**
 * \class StEEmcPointFitMaker
 * \brief A class for finding EEMC points
 *
 *    This class find EEMC points using a fitting algorithm.  The fit 
 * is driven by the EEmcSectorFit class, which implements a simultaneous
 * fit to both the U and V smd planes.
 *
 *    The single-plane shower-shape is implemented in the function 
 * eeSinglePeak.  This shape is fairly standard, i.e. a double-gaussian 
 * with constrained  mean and constrainded widths.  
 * For this algorithm, the relative amplitudes of the two gaussians are
 * fixed.  The narrow-component gaussian has a relative amplitude of 0.8,
 * and the wide-component gaussian has a relative amplitude of 0.2.
 * Furthermore we constrain the width of the wide-component to be 3.5 
 * times the width of the narrow component.
 *
 *    In the two-plane simultaneous fit, we fit only three four parameters:
 *
 * 1. A total yield [nmips]
 *
 * 2. A common width [nstrips]
 *
 * 3. A mean U position 
 *
 * 4. A mean V position
 *
 * In order to start the fit to a new photon, we scan both SMD planes
 * for the largest residual energy deposit summed over five consecutive
 * smd strips (the residual energy deposit is simply the energy of the
 * strips in question minus the energy predicted by the fit to the current
 * number of photons).  We require that this maximum U,V pair appears
 * beneath an active tower (or a tower with a fail bit set).
 *
 * Once the maximum in each plane is found, we initialize the fit, using
 * the 5-strip energy, mean and widths to set the initial parameters of
 * the fit.  This procedure is repeated until 90% or more of the energy 
 * deposited in the smd is accounted for.  As each photon is discovered,
 * we try all permutations of one plane versus the other to determine if
 * a better association (although if more than 5 gammas are found, we do
 * not try all permutations as this begins to become too computationally 
 * intensive.)
 *
 * Once we have fit "all" points in an smd plane, we apply a cut --
 * any point which is w/in 3.5 times the width of the narrow component
 * gaussian must exceed 10% of the energy of the higher-energy point.
 * If it doesn't, then we assume that it is a large angle multiple scatter
 * or a bremstrahlung photon and remove it from consideration.
 *
 * \author Jason C. Webb
 *
 */

#include "StEEmcPointFitMaker.h"
#include "EEmcSectorFit.h" 
#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"
#include <algorithm>
#include "TString.h"
#include "StEEmcPool/StEEmcClusterMaker/StEEmcSmdCluster.h" 

ClassImp(StEEmcPointFitMaker);

// ----------------------------------------------------------------------------
StEEmcPointFitMaker::StEEmcPointFitMaker(const Char_t *n):StEEmcPointMaker(n)
{
  for ( Int_t sec=0;sec<12;sec++ )
    {
      mSectorFit[sec]=0;
    }

  mPermutations=true;

  mLimitFits=10;

}

// ----------------------------------------------------------------------------
Int_t StEEmcPointFitMaker::Init()
{
    return StEEmcPointMaker::Init();
}

// ----------------------------------------------------------------------------
Int_t StEEmcPointFitMaker::Make()
{
    mClusterId=10000; 
  /// Create a new sector fit maker 
  for ( Int_t sec=0;sec<12;sec++ )
    {
      mSectorFit[sec]=new EEmcSectorFit(10); 
      mSectorFit[sec]->doPermutations=mPermutations;
      //mSectorFit[sec]->SetPrintLevel(-1); 
    }

  /// Perform fits to SMD distributions in all 12 sectors
  for ( Int_t i=0;i<12;i++ ) 
    {
      FitSector(i); 
    }

  /// Do energy sharing.
  if ( mEnergyMode == 0 )
    shareEnergySmd();
  else if ( mEnergyMode == 1 )
    shareEnergySimple();


  return kStOK;
}
// ----------------------------------------------------------------------------
Int_t StEEmcPointFitMaker::FitSector(Int_t sector)
{

  EEmcSectorFit *fitter = mSectorFit[sector];

  /// How much energy is in the sector?  We will stop finding gammas
  /// when we have found 90% of it, or when we can no longer find a 
  /// good candidate in both planes
  Float_t etowers = mEEanalysis->energy(sector,0);
  Float_t eproject = etowers * 0.007 * 1000.0; // expected smd E [MeV]
  Float_t nproject = eproject / 1.3; // expected smd E [nmips]


  /// Fill histograms for u and v planes
  TString uname="h";uname+=(sector+1<10)?"0":"";uname+=sector+1;uname+="U";
  TString vname="h";vname+=(sector+1<10)?"0":"";vname+=sector+1;vname+="V";
  TH1F *hU=new TH1F(uname,"SMD-u energy distribution [nmips]",288,0.,288.);
  TH1F *hV=new TH1F(vname,"SMD-v energy distribution [nmips]",288,0.,288.);

  for ( Int_t hit=0;hit<mEEanalysis->numberOfHitStrips(sector,0);hit++ ) {
    StEEmcStrip strip=mEEanalysis->hitstrip(sector,0,hit);
    if ( strip.fail() ) continue;
    hU->SetBinContent( strip.index()+1, strip.energy() * 1000.0 / 1.3 );
  }
  for ( Int_t hit=0;hit<mEEanalysis->numberOfHitStrips(sector,1);hit++ ) {
    StEEmcStrip strip=mEEanalysis->hitstrip(sector,1,hit);
    if ( strip.fail() ) continue;
    hV->SetBinContent( strip.index()+1, strip.energy() * 1000.0 / 1.3 );
  }

  /// Give the sector fitter pointers to the histograms
  /// (note that hU and hV will be deleted with call to ~EEmcSectorFit
  mSectorFit[sector]->SetHistograms(hU,hV);


  /// we require at least 2 GeV of energy in the sector to continue
  if ( etowers < 1.0 ) return kStOK;
  

  Float_t nfit = 0.;
  Float_t nlast = 5.0;
  Int_t count = 0;

  while ( nfit < 0.90 * nproject && nlast >= 4.0 && count < mLimitFits ) {
    
    /// scan smd planes for maximum 5-strip residual
    Double_t res5umax=0.;
    Double_t res5vmax=0.;
    Double_t resu=0.;
    Double_t resv=0.;
    Double_t res5u=0.;
    Double_t res5v=0.;
    Int_t maxu=-1;
    Int_t maxv=-1;
    for ( Int_t i=0;i<288;i++ )
      {
	res5u=fitter->Residual(i,0,2);
	res5v=fitter->Residual(i,1,2);
	resu=fitter->Residual(i,0);
	resv=fitter->Residual(i,1);
	if ( res5u > res5umax ) {
	  res5umax=res5u;
	  maxu=i;
	}
	if ( res5v > res5vmax ) {
	  res5vmax=res5v;
	  maxv=i;
	}
      }

    /// punt if no suitable candidates were found
    if ( maxu<0 || maxv<0 ) {
      //      std::cout << "maxu || maxv < 0, abort" << std::endl;
      break;
    }
    if ( res5umax < 4.0 && res5vmax < 4.0 ) {
      //      std::cout << "Not enough signal, abort" << std::endl;
      break;
    }

    /// add a candidate photon at this point
    //  std::cout << "Adding candidate, now I expect some output!" << std::endl;
    fitter -> AddCandidate( (res5umax+res5vmax)/2., 0.85, (Double_t)maxu+0.5, (Double_t)maxv+0.5 );

    /// perform two plane simultaneous fit
    fitter -> Migrad();

    /// Get the parameters for the last candidate
    Double_t e,s,u,v;
    fitter->GetLastCandidate( e, s, u, v );
    nlast = e;

    /// refit all permutations (up to a point... cpu requirements
    /// get too large after 5! (!)
    if ( count && count < 5 ) fitter -> TryPermutations();

    /// Total energy in fit 
    nfit = 0.;
    for ( Int_t i=0;i<fitter->numberOfCandidates();i++ ){

      fitter->GetCandidate(i,e,s,u,v);
      nfit += e;

    }
    
    count++;
    if ( nlast < 4.0 ) break;

  }


  //$$$  std::cout << "found ncand=" << fitter->numberOfCandidates() << std::endl;


  /// array of flags telling us whether a given fit will be 
  /// spawned as a new point
  std::vector<Bool_t> drop( fitter->numberOfCandidates(), false );
  for ( Int_t ifit=0;ifit<fitter->numberOfCandidates()-1;ifit++ )
    {
      Double_t e1,s1,u1,v1;
      fitter->GetCandidate(ifit,e1,s1,u1,v1);
      for ( Int_t jfit=ifit+1;jfit<fitter->numberOfCandidates();jfit++ )
	{
	  Double_t e2,s2,u2,v2;
	  fitter->GetCandidate(jfit,e2,s2,u2,v2);

	  /// distance between gamma candidates
	  Float_t dist = TMath::Sqrt( (u1-u2)*(u1-u2) + (v1-v2)*(v1-v2) );

	  /// Is first gamma a fluctuation of second gamma?
	  if ( e1 < 0.15 * e2 ) {	    
	    Float_t min_dist = TMath::Abs(s1) * 3.5;
	    if ( dist < min_dist ) drop[ifit]=true;
	  }

	  /// Is second gamma a fluctuation of first gamma?
	  if ( e2 < 0.15 * e1 ) {
	    Float_t min_dist = TMath::Abs(s2) * 3.5;
	    if ( dist < min_dist ) drop[jfit]=true;
	  }

	}
    }



  /// Now we spawn points at the u,v crossing found by the fits
  /// Smd clusters should also be generated and inserted, to make
  /// embedding work...

  for ( Int_t ifit=0;ifit<fitter->numberOfCandidates();ifit++ )
    {

      /// Skip fits if they have been flagged
      if ( drop[ifit] ) continue;

      Double_t nmip,sigma,u,v;
      fitter->GetCandidate( ifit, nmip, sigma, u, v );
      StEEmcPoint point;
      point.sector( sector );
      point.sigma( sigma );
      point.u(u);
      point.v(v); 
      point.energy( (Float_t)(nmip * 1.3 / 0.007 / 1000.0) );
      TVector3 position = mEEsmd->getIntersection( sector, (Int_t)u, (Int_t)v );
      point.position( position );
      StEEmcTower *tower = mEEanalysis->tower(position,0);
      if ( !tower ) continue; /// should examine other permutations
      point.tower( *tower );
      /// residuals w/in +/- 30 cm
      point.residueU( (Float_t)fitter -> Residual( (Int_t)u, 0, 60 ) * 1.3 / 1000.0 );
      point.residueV( (Float_t)fitter -> Residual( (Int_t)v, 1, 60 ) * 1.3 / 1000.0 );
		      

      /// U AND V clusters
      StEEmcSmdCluster uc;
      uc.energy( (float)(nmip * 1.3 / 1000.0) );
      uc.mean( (float)u );
      uc.sigma( (float)sigma ); 
      uc.sector ( sector );
      uc.plane( 0 ); 
      uc.key( mClusterId++ ); 
      StEEmcSmdCluster vc;
      vc.energy( (float)(nmip * 1.3 / 1000.0) );
      vc.mean ( (float)v );
      vc.sigma( (float)sigma );
      vc.plane( 1 ); 
      vc.key( mClusterId++ ); 
      point.cluster( uc, 0 );
      point.cluster( vc, 1 ); 
      point.energy( (float)(nmip * 1.3 / 1000.0) );
      point.sector( sector ); 
     
      mPoints.push_back(point);

    }

  return kStOK;

}
// ----------------------------------------------------------------------------
void StEEmcPointFitMaker::Clear(Option_t *opts)
{
  /// Delete sector fits
  for ( Int_t i=0;i<12;i++ )
    if ( mSectorFit[i] ) delete mSectorFit[i];

  StEEmcPointMaker::Clear(opts);
}

// ----------------------------------------------------------------------------
void StEEmcPointFitMaker::print() const
{
    for ( Int_t i=0;i<12;i++ )
    {
	mSectorFit[i]->print(); 
    } 
    for ( UInt_t i=0;i<mPoints.size();i++ )
    {
	mPoints[i].print(); 
    } 
} 
