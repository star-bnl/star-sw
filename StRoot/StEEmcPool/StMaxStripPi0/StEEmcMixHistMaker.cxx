#include "StEEmcMixHistMaker.h"
#include "TLine.h"
#include <iostream>

ClassImp(StEEmcMixHistMaker);

// ----------------------------------------------------------------------------
StEEmcMixHistMaker::StEEmcMixHistMaker(const Char_t *name):StEEmcMixMaker(name)
{

  std::cout << "In constructor" << std::endl;

  mMin=0.1;
  mMax=0.18;

  std::cout << "..." << std::endl;

}

// ----------------------------------------------------------------------------
Int_t StEEmcMixHistMaker::Init()
{

  book(hMass,"hMass","Dipoint invariant mass spectrum",120,0.,1.2);  
  book(hEnergy,"hEnergy","Dipoint energy distribution",100,0.,40.);
  book(hPT,"hPT","Dipoint PT distribution",100,0.,20.);
  book(hZgg,"hZgg","Dipoint asymmetry",100,0.,1.);
  book(hPhigg,"hPhigg","Dipoint opening angle",100,0.,0.2);
  book(hU1,"hU1","U seed strip for higher energy gamma",288,0.,288.);
  book(hU2,"hU2","U seed strip for lower energy gamma",288,0.,288.);
  book(hV1,"hV1","V seed strip for higher energy gamma",288,0.,288.);
  book(hV2,"hV2","V seed strip for lower energy gamma",288,0.,288.);

  return StEEmcMixMaker::Init();
}

// ----------------------------------------------------------------------------
void StEEmcMixHistMaker::book( TH1F **h, const Char_t *name, const Char_t *title, Int_t nbin, Float_t min, Float_t max )
{

  const Char_t *n[]={"R","B","S"};
  const Char_t *t[]={", real events", ", mixed events", ", background subtracked"};
  const Int_t  col[]={1,2,4};

  for ( Int_t i=0;i<3;i++ )
    {
      h[i]=new TH1F(TString(name)+n[i], TString(title)+t[i], nbin, min, max );
      h[i]->Sumw2();
      h[i]->SetLineColor( col[i] );
    }

}

// ----------------------------------------------------------------------------
Int_t StEEmcMixHistMaker::Make()
{
  Int_t stat=StEEmcMixMaker::Make();
  if ( stat != kStOK ) return stat;

  /*
  std::cout << "candidates=" << mCandidates.size() << std::endl;
  std::cout << "background=" << mBackground.size() << std::endl;
  */

  /// cut events with > 2 points reconstructed for now
  if ( mCandidates.size() > 1 ) return kStOK;
    

  for ( UInt_t i=0; i<mCandidates.size(); i++ )
    {
      if ( accept(mCandidates[i] ) ) 
	fill( 0, mCandidates[i] );
    }

  for ( UInt_t i=0; i<mBackground.size(); i++ )
    {
      if ( accept(mBackground[i]) )
	fill( 1, mBackground[i] );
    }

  return kStOK;

}

// ----------------------------------------------------------------------------
void StEEmcMixHistMaker::fill( Int_t mode, StEEmcPair &pair )
{
  hMass[mode]->Fill( pair.mass() );
  if ( pair.mass() >= mMin && pair.mass() < mMax ) {
    hEnergy[mode]->Fill( pair.energy() );
    hPT[mode]->Fill( pair.momentum().Perp() );
    hZgg[mode]->Fill( pair.zgg() );
    hPhigg[mode]->Fill( pair.phigg() );
    hU1[mode]->Fill( pair.point(0).cluster(0).mean() );
    hU2[mode]->Fill( pair.point(1).cluster(0).mean() );
    hV1[mode]->Fill( pair.point(0).cluster(1).mean() );
    hV2[mode]->Fill( pair.point(1).cluster(1).mean() );
  }    			 
}

// ----------------------------------------------------------------------------
Int_t StEEmcMixHistMaker::Finish()
{
  
  /// First, normalize combinatoric background
  Float_t norm=1.0;
  //  hMass[0]->GetXaxis()->SetRangeUser(0.8,1.2);
  //  hMass[1]->GetXaxis()->SetRangeUser(0.8,1.2);

  Float_t a=hMass[0]->Integral(80,120);
  Float_t b=hMass[1]->Integral(80,120);
  if ( b > 0. ) norm = a/b;
  
  subtract(hMass,norm);
  subtract(hEnergy,norm);
  subtract(hPT,norm);
  subtract(hZgg,norm);
  subtract(hPhigg,norm);
  subtract(hU1,norm);
  subtract(hU2,norm);
  subtract(hV1,norm);
  subtract(hV2,norm);

  return kStOK;

}

void StEEmcMixHistMaker::subtract( TH1F **h, Float_t norm )
{
  h[1]->Scale(norm);
  h[2]->Add(h[0], 1.0);
  h[2]->Add(h[1],-1.0);
}

Bool_t StEEmcMixHistMaker::accept( StEEmcPair &pair )
{
  if ( pair.pt() < 2.0 ) return false;
  return true;
}
