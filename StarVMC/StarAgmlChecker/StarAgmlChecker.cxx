#include "StarAgmlChecker.h"
#include "TString.h"
#include "TH2F.h"
#include "TGeoManager.h"
#include <iostream>
#include "TMath.h"
#include "TStopwatch.h"
#include "TCanvas.h"
#include "TRandom.h" // should factor randomness out of this...

StarAgmlChecker::StarAgmlChecker( TGeoManager *m ) : TGeoChecker(m)
{

}
// ................................................................................................

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
  TGeoVolume *volume = gGeoManager->FindVolumeFast( name );  if (!volume) return 0;
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

  Double_t the, eta, phi, step, matprop, x;
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
	x = 0;  // reset radiation length counter
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
// ................................................................................................







void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoMatrix *matrix )
{
  CheckSum_t &sum = *_sum;
  for ( Int_t i=0;i<9;i++ ) sum += matrix->GetRotationMatrix()[i];
  for ( Int_t i=0;i<3;i++ ) sum += matrix->GetScale()[i];
  for ( Int_t i=0;i<3;i++ ) sum += matrix->GetTranslation()[i];
}

// 
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoNode *node )
{
  CheckSum_t &sum = *_sum;
  sum += node->IsOverlapping();
  Update( _sum, node -> GetMatrix() );
}

// 
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoMedium *medium )
{
  CheckSum_t &sum = *_sum;
  for ( Int_t i=0;i<20;i++ ) sum += medium->GetParam(i);
}

// Update checksum based on shape
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoShape *shape )
{
  if (shape->IsA() == TGeoBBox::Class()    ) Update( _sum, (TGeoBBox    *)shape ); 
  if (shape->IsA() == TGeoArb8::Class()    ) Update( _sum, (TGeoArb8    *)shape ); 
  if (shape->IsA() == TGeoTrap::Class()    ) Update( _sum, (TGeoTrap    *)shape ); 
  if (shape->IsA() == TGeoGtra::Class()    ) Update( _sum, (TGeoGtra    *)shape ); 
  if (shape->IsA() == TGeoCone::Class()    ) Update( _sum, (TGeoCone    *)shape ); 
  if (shape->IsA() == TGeoConeSeg::Class() ) Update( _sum, (TGeoConeSeg *)shape ); 
  if (shape->IsA() == TGeoTube::Class()    ) Update( _sum, (TGeoTube    *)shape ); 
  if (shape->IsA() == TGeoTubeSeg::Class() ) Update( _sum, (TGeoTubeSeg *)shape ); 
  if (shape->IsA() == TGeoCtub::Class()    ) Update( _sum, (TGeoCtub    *)shape ); 
  if (shape->IsA() == TGeoEltu::Class()    ) Update( _sum, (TGeoEltu    *)shape ); 
  if (shape->IsA() == TGeoHype::Class()    ) Update( _sum, (TGeoHype    *)shape ); 
  if (shape->IsA() == TGeoPara::Class()    ) Update( _sum, (TGeoPara    *)shape ); 
  if (shape->IsA() == TGeoPcon::Class()    ) Update( _sum, (TGeoPcon    *)shape ); 
  if (shape->IsA() == TGeoPgon::Class()    ) Update( _sum, (TGeoPgon    *)shape ); 
  if (shape->IsA() == TGeoSphere::Class()  ) Update( _sum, (TGeoSphere  *)shape ); 
  if (shape->IsA() == TGeoTorus::Class()   ) Update( _sum, (TGeoTorus   *)shape ); 
  if (shape->IsA() == TGeoTrd1::Class()    ) Update( _sum, (TGeoTrd1    *)shape ); 
  if (shape->IsA() == TGeoTrd2::Class()    ) Update( _sum, (TGeoTrd2    *)shape ); 
}

void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoBBox *shape ) { CheckSum_t &sum = *_sum; 
  sum += "bbox";
  sum += shape->GetDX();
  sum += shape->GetDY();
  sum += shape->GetDZ();
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoArb8 *shape ) { CheckSum_t &sum = *_sum;
  sum += "arb8";
  sum += shape->GetDz();
  for ( Int_t i = 0; i<16; i++ ) sum += shape->GetVertices()[i]; 
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoTrap *shape ) { CheckSum_t &sum = *_sum; 
  sum += "trap";
  Update(_sum, (TGeoArb8 *)shape );
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoGtra *shape ) { CheckSum_t &sum = *_sum; 
  sum += "gtra";
  Update(_sum, (TGeoArb8 *)shape );
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoCone *shape ) { CheckSum_t &sum = *_sum; 
  sum += "cone";
  sum += shape->GetDz();
  sum += shape->GetRmin1();
  sum += shape->GetRmax1();
  sum += shape->GetRmin2();
  sum += shape->GetRmax2();
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoConeSeg *shape ) { CheckSum_t &sum = *_sum; 
  sum += "cons";
  Update( _sum, (TGeoCone *)shape );
  sum += shape->GetPhi1();
  sum += shape->GetPhi2();
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoTube *shape ) {   CheckSum_t &sum = *_sum;  
  sum += "tube";
  //  TMD5 start = *_sum;
  sum += shape->GetRmin() + 1.0;
  sum += shape->GetRmax();
  sum += shape->GetDz();
  //  TMD5 finish = *_sum;
  //  std::cout << start.AsString() << " " << finish.AsString() << std::endl;
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoTubeSeg *shape ) { CheckSum_t &sum = *_sum; 
  sum += "tubs";
  Update( _sum, (TGeoTube *)shape );
  sum += shape->GetPhi1();
  sum += shape->GetPhi2();
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoCtub *shape ) { CheckSum_t &sum = *_sum; 
  sum += "ctub";
  Update( _sum, (TGeoTube *)shape );
  for ( Int_t i=0;i<3;i++ ) {
    sum += shape->GetNhigh()[i];
    sum += shape->GetNlow()[i];
  }
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoEltu *shape ) { CheckSum_t &sum = *_sum; 
  sum += "eltu";
  sum += shape->GetA();
  sum += shape->GetB();
  sum += shape->GetDz();
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoHype *shape ) { CheckSum_t &sum = *_sum; 
  sum += "hype";
  Update( _sum, (TGeoTube *)shape );
  sum += shape->GetStIn();
  sum += shape->GetStOut();
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoPara *shape ) { CheckSum_t &sum = *_sum;
  sum += "para";
  Update( _sum, (TGeoBBox *)shape );
  sum += shape->GetAlpha();
  sum += shape->GetPhi();
  sum += shape->GetTheta();
  sum += shape->GetX();
  sum += shape->GetY();
  sum += shape->GetZ();
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoPcon *shape ) { CheckSum_t &sum = *_sum;
  sum += "pcon";
  sum += shape->GetNz();
  sum += shape->GetPhi1();
  sum += shape->GetDphi();
  for ( Int_t iz = 0; iz< shape->GetNz(); iz++ )
    {
      sum += shape->GetZ(iz);
      sum += shape->GetRmin(iz);
      sum += shape->GetRmax(iz);
    }
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoPgon *shape ) { CheckSum_t &sum = *_sum; 
  sum += "pgon";
  Update( _sum, (TGeoPcon *)shape );
  sum += shape->GetNedges();
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoSphere *shape ) { CheckSum_t &sum = *_sum; 
  sum += "sphere";
  sum += shape -> GetRmin();
  sum += shape -> GetRmax();
  sum += shape -> GetPhi1();
  sum += shape -> GetPhi2();
  sum += shape -> GetTheta1();
  sum += shape -> GetTheta2();
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoTorus *shape ) { CheckSum_t &sum = *_sum; 
  sum += "torus";
  sum += shape -> GetRmin();
  sum += shape -> GetRmax();
  sum += shape -> GetR();
  sum += shape -> GetPhi1();
  sum += shape -> GetDphi();
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoTrd1 *shape ) { CheckSum_t &sum = *_sum;
  sum += "trd1";
  sum += shape -> GetDx1();
  sum += shape -> GetDx2();
  sum += shape -> GetDy();
  sum += shape -> GetDz();
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoTrd2 *shape ) { CheckSum_t &sum = *_sum; 
  sum += "trd2";
  sum += shape -> GetDx1();
  sum += shape -> GetDx2();
  sum += shape -> GetDy1();
  sum += shape -> GetDy2();
  sum += shape -> GetDz();
}


// Update checksums with material properties
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoMaterial *material )
{
  CheckSum_t   &sum = *_sum;  
  sum += material->GetA();
  sum += material->GetZ();
  sum += material->GetDensity();
  sum += material->GetRadLen();
  sum += material->GetIntLen();
  Bool_t mix = material->IsMixture();
  if ( mix )
    {
      TGeoMixture *mixture = (TGeoMixture *)material;
      Int_t nele = mixture -> GetNelements();
      sum += nele;
      for ( Int_t i=0;i<nele; i++ )
	{
	  TGeoElement *element = mixture->GetElement(i);
	  sum += element->A();
	  sum += element->Z();
	  if ( mixture->GetNmixt() )	  sum += mixture->GetNmixt()[i];
	  if ( mixture->GetWmixt() )      sum += mixture->GetWmixt()[i];
	  if ( mixture->GetZmixt() )      sum += mixture->GetZmixt()[i];
	}
    }
};

// Updates checksum for the specified volume.  
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoVolume *volume )
{
  CheckSum_t &sum = *_sum;
  TString name = volume->GetName();

  if ( !volume->IsAssembly() ) 
    { // Assemblies have no material, medium or shape...
      
      Update( _sum, volume->GetMaterial() );
      Update( _sum, volume->GetMedium() );
      Update( _sum, volume->GetShape() );      
    }
  
  // Loop over daughter nodes
  for ( Int_t i=0;i<volume->GetNdaughters(); i++ )
    {
      TGeoNode   *node = volume->GetNode(i);
      TGeoVolume *kid  = node->GetVolume();
      TGeoMatrix *mat  = node->GetMatrix();
      
      // Add the matrix
      Update( _sum, mat );  

      // First time visiting a daughter
      if ( mCheckSum[kid]==0 )
	{
	  Update( _sum, kid );
	}

      // Get the child's checksum and add it
      // to the running sum.
      TMD5 md5kid = CheckSum( kid->GetName() );
      sum += md5kid.AsString();

    }

  return;

}

TMD5 StarAgmlChecker::CheckSum( const Char_t *name )
{

  // Obtain the volume from the manager
  TGeoVolume *volume = gGeoManager->FindVolumeFast(name);

  // If it doesn't exist, return zero MD5 sum
  if ( !volume ) 
    {
      TMD5 result; 
      result.Final(); 
      return result;
    };

  // Lookup checksum for this volume.  If it exists, return a copy.
  if ( mCheckSum[volume] ) goto DONE;

  // We're processing a new volume.  Register a new checksum.
  mCheckSum[volume] = new CheckSum_t();

  // Update based on the volume parameters.
  Update( mCheckSum[volume], volume );

  // Print checksum
  //  std::cout << name << " " << volume << " " << (Char_t *)mCheckSum[volume] << std::endl;

 DONE:
  
  TMD5 result( *mCheckSum[volume] );
  result.Final();
  return result;

};
// ................................................................................................
TDataSet *StarAgmlChecker::CheckSet( const Char_t *name )
{
  TGeoVolume *top = gGeoManager->FindVolumeFast(name);
  if ( !top ) {
    return NULL; // should never happen...
  }

  TDataSet *set = new TDataSet(name);   set->SetTitle( CheckSum(name).AsString() );
  std::map< TString, Int_t > kids;
  
  for ( Int_t i=0;i<top->GetNdaughters(); i++ )
    {
      TGeoNode   *node = top->GetNode(i);
      TGeoVolume *kid  = node->GetVolume();
      if ( kids[kid->GetName()]++ ) continue; // skip if already in set
      set->Add( CheckSet(kid->GetName()) );   // recursively add daughters
      
    }
  return set;

};
