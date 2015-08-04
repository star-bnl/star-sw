#include "StarAgmlChecker.h"
#include "TString.h"
#include "TH2F.h"
#include "TGeoManager.h"
#include <iostream>
#include "TMath.h"
#include "TStopwatch.h"

StarAgmlChecker::StarAgmlChecker( TGeoManager *m ) : TGeoChecker(m)
{

}

// Helper class to manager filling histograms
struct Hist_t {
  TString  _name;
  TH2F    *_hist;
  Hist_t( TH2F *h ){ _name=h->GetName(); _hist=h; }
  void  operator()( Double_t eta, Double_t phi, Double_t prop ){
    TString path = gGeoManager->GetPath();

    if ( path.Contains(_name) ) _hist->Fill( eta, phi, prop );


  };
};

std::vector<Hist_t>  filler;     

TObjectSet *StarAgmlChecker::MaterialPlot( const Char_t   *_top   ,
					   const Int_t     nEta   ,
					   const Double_t  mnEta  , 
					   const Double_t  mxEta  , 
					   const Int_t     nPhi   ,
					   const Double_t  mnPhi  , 
					   const Double_t  mxPhi  ,
					   const Double_t  rmin   ,
					   const Double_t  rmax   ,
					   const Double_t  zmin   ,
					   const Double_t  zmax   ,
					   const Option_t *_opts  )
{

  TString name = _top;  
  TString opts = _opts; opts.ToLower();

  // We will add in some useful options later... e.g. collect by nickname
  Bool_t set_top = opts.Contains("top");

  // Get the top level volume
  TGeoVolume *volume = gGeoManager->FindVolumeFast( name );
  if (set_top)         gGeoManager->SetTopVolume(volume);

  // Setup an iterator over its daughter nodes
  TGeoIterator next( volume ); 
  next.SetType( 1 );

  // Create a TObjectSet for this volume filled with a 2D histogram
  TH2F *hist = new TH2F(name,"radlen",nEta,mnEta,mxEta,nPhi,mnPhi,mxPhi);  
  hist->SetDirectory(0);
  TObjectSet *topSet = new TObjectSet(name, hist);

  filler.push_back( Hist_t(hist) );


  TGeoNode *node = 0;
  while( (node=(TGeoNode *)next()) )
    {

      // Get the current volume and find associated object set
      TGeoVolume *vol = node->GetVolume();
      TObjectSet *set = (TObjectSet *)topSet->Find(vol->GetName());
      if ( set ) continue;

      // If set wasn't found, create and hang on topSet
      hist = new TH2F(vol->GetName(),"radlen",nEta,mnEta,mxEta,nPhi,mnPhi,mxPhi);  
      hist->SetDirectory(0);
      set  = new TObjectSet(vol->GetName(),hist);
      filler.push_back(Hist_t(hist));

      // And shunt to the parent
      set->Shunt( topSet );

    }

  Fill( topSet, rmin, rmax, zmin, zmax );

  return topSet;

}


void StarAgmlChecker::Fill( TObjectSet *set, Double_t rmin, Double_t rmax, Double_t zmin, Double_t zmax )
{

  TH2F *hist = (TH2F *)set->GetObject();
  TString name = hist->GetName(); // name of the histogram/volume being probed

  Double_t the, eta, phi, step, matprop;//, x;
  Double_t start[3], dir[3];
  TGeoNode *startnode = 0, *endnode = 0;

  Int_t nphi = hist->GetNbinsY();
  Int_t neta = hist->GetNbinsX();
  Int_t ngen = 0;

  TStopwatch timer;
  timer.Reset();
  timer.Start();


  for ( Int_t iphi=1;iphi<nphi;iphi++ ) 
    for ( Int_t jeta=1;jeta<=neta;jeta++ ) 
      { 

	ngen++; // count total number of events
	//x = 0;  // reset radiation length counter
	eta  = hist->GetXaxis()->GetBinCenter(jeta);
	phi  = hist->GetYaxis()->GetBinCenter(iphi);    // in rad
	the  = 2.0 * TMath::ATan( TMath::Exp( -eta ) ); // in rad

	start[0] = start[1] = start[2] = 0.0; 
	dir  [0] = TMath::Sin(the) * TMath::Cos(phi);
	dir  [1] = TMath::Sin(the) * TMath::Sin(phi);
	dir  [2] = TMath::Cos(the);
	gGeoManager -> InitTrack( &start[0], &dir[0] );

	OpProgress( Form("MaterialPlot [%s]",name.Data()), ngen, nphi*neta, &timer );

	startnode = gGeoManager -> GetCurrentNode();
	if ( gGeoManager->IsOutside() ) startnode = 0;

	matprop = 0;
	if ( startnode ) matprop = startnode->GetVolume()->GetMaterial()->GetRadLen();        	 

	gGeoManager -> FindNextBoundary();	
	endnode = gGeoManager->Step();
	step    = gGeoManager->GetStep();

	Int_t nloop= 0;	
	while ( step < 1E10 ) 
	  {
	    nloop = 0;
	    while ( !gGeoManager->IsEntering() ) // take another step (mm size)
	      {
		nloop++;
		gGeoManager->SetStep(1E-3);
		step += 1E-3;
		endnode = gGeoManager->Step();
	      }

	    
	    const Double_t *p = gGeoManager->GetCurrentPoint();
	    const Double_t  R = TMath::Sqrt( p[0]*p[0]+p[1]*p[1] );	
	    const Double_t  Z = p[2];
	    
	    //
	    // Fill if material property is nonzero AND w/in the specified rmin,rmax range
	    //
	    if ( matprop > 0 && R > rmin && R < rmax && Z > zmin && Z < zmax ) //filler[0]( eta, phi, step/matprop );
	      {
		for ( UInt_t ii=0;ii<filler.size();ii++ ) filler[ii]( eta, phi, step/matprop );

	      }

	    // Done when leaving the world volume or when we exceed user specified max/min positions
	    if (endnode==0 && step>1E10) break;
	    if (R < rmin || R > rmax ) break;
	    if (Z < zmin || Z > zmax ) break;

	    // generate an extra step to cross boundary
	    startnode = endnode;
	    matprop = 0;
	    if ( startnode ) matprop = startnode->GetVolume()->GetMaterial()->GetRadLen();    

	    gGeoManager->FindNextBoundary();
            endnode = gGeoManager->Step();
            step = gGeoManager->GetStep();

	  }

	//	hist->Fill( eta, phi, x );
	
      }
					         
}
