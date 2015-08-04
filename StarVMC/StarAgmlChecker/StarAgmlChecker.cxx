#include "StarAgmlChecker.h"
#include "TString.h"
#include "TH2F.h"
#include "TGeoManager.h"
#include <iostream>
#include "TMath.h"
#include "TStopwatch.h"
#include "TCanvas.h"
#include "TRandom.h" // should factor randomness out of this...
#include <cmath>
#include <limits>
#include <assert.h>

StarAgmlChecker::StarAgmlChecker( TGeoManager *m ) : TGeoChecker(m)
{

  //Skip("UPST");
  //  Skip("EXSG"); // Endcap SMD planes are bad w/ roundoff
  //  Skip("EXS1");
  //  Skip("EXS2");
  //  Skip("EXS3");
  //  Skip("EXS4");
  //  Skip("EXS5");
  //  Skip("EXS6");

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
// ................................................................................................


Double_t Normalize( Double_t f, Int_t &norm, Int_t &expo )
{
  Int_t i = f;
  if (!i) return f;

  Int_t    pwr  = TMath::Log10(    TMath::Abs(f) );
  Double_t fact = TMath::Power(10, pwr); expo=pwr ;

  f /= fact;
  i  = TMath::Nint(f);
  f /= i;

  norm = i;

  return f;  
};

Float_t Normalize( Float_t f, Int_t &norm, Int_t &expo )
{
  Int_t i = f;
  if (!i) return f;

  Int_t    pwr = TMath::Log10(    TMath::Abs(f) );
  Float_t fact = TMath::Power(10, pwr); expo=pwr;
  f /= fact;
  i  = TMath::Nint(f);
  f /= i;

  norm = i;

  return f;  
};


#define std__round __STD_ROUND__
#ifdef  std__round
Double_t __STD_ROUND__( Double_t x )
{
  if ( TMath::Abs(x) > std::numeric_limits<long long>::max() )
    {
      std::cout << "About to die... x = " << x << std::endl;
    }
  assert( TMath::Abs(x) < std::numeric_limits<long long>::max() );
  if ( x < 0 )      return (long long)(x-0.5);
  /* else */        return (long long)(x+0.5);  
}
#endif



Float_t Round( Float_t value, Float_t prec = 0.002 )
{ assert(0);
  Int_t   norm=0;
  Int_t   expo=0;
  value=Normalize(value,norm,expo);

  value /= prec; // divide precision by value
  prec  *= norm; // rescale precision by normalization
  prec  *= expo; // rescale by exponent

#ifdef std__round
  return std__round(value) * prec;
#else
  return std::round(value) * prec;
#endif

}
Double_t Round( Double_t value, Double_t prec = 0.0000250 )
{
  Int_t norm=0;
  Int_t expo=0;
  value=Normalize(value,norm,expo);

  value /= prec; // divide precision by value
  prec  *= norm; // rescale precision by normalization
  prec  *= expo; // rescale by exponent

#ifdef std__round
  return std__round(value) * prec;
#else
  return std::round(value) * prec;
#endif

}


// ................................................................................................



void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoMatrix *matrix )
{
  CheckSum_t &sum = *_sum;
  for ( Int_t i=0;i<9;i++ ) sum += Round(matrix->GetRotationMatrix()[i]);
  for ( Int_t i=0;i<3;i++ ) sum += Round(matrix->GetScale()[i]);
  for ( Int_t i=0;i<3;i++ ) sum += Round(matrix->GetTranslation()[i]);
}

// 
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoNode *node )
{
  CheckSum_t &sum = *_sum;
  sum += Int_t(node->IsOverlapping());
  Update( _sum, node -> GetMatrix() );
}

// 
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoMedium *medium )
{
  CheckSum_t &sum = *_sum;
  for ( Int_t i=0;i<20;i++ ) sum += Round(medium->GetParam(i));
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
  sum += Round(shape->GetDX());
  sum += Round(shape->GetDY());
  sum += Round(shape->GetDZ());
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoArb8 *shape ) { CheckSum_t &sum = *_sum;
  sum += "arb8";
  sum += Round(shape->GetDz());
  for ( Int_t i = 0; i<16; i++ ) sum += Round(shape->GetVertices()[i]);
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
  sum += Round(shape->GetDz());
  sum += Round(shape->GetRmin1());
  sum += Round(shape->GetRmax1());
  sum += Round(shape->GetRmin2());
  sum += Round(shape->GetRmax2());
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoConeSeg *shape ) { CheckSum_t &sum = *_sum; 
  sum += "cons";
  Update( _sum, (TGeoCone *)shape );
  sum += Round(shape->GetPhi1());
  sum += Round(shape->GetPhi2());
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoTube *shape ) {   CheckSum_t &sum = *_sum;  
  sum += "tube";
  //  TMD5 start = *_sum;
  sum += Round(shape->GetRmin() + 1.0);
  sum += Round(shape->GetRmax());
  sum += Round(shape->GetDz());
  //  TMD5 finish = *_sum;
  //  std::cout << start.AsString() << " " << finish.AsString() << std::endl;
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoTubeSeg *shape ) { CheckSum_t &sum = *_sum; 
  sum += "tubs";
  Update( _sum, (TGeoTube *)shape );
  sum += Round(shape->GetPhi1());
  sum += Round(shape->GetPhi2());
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoCtub *shape ) { CheckSum_t &sum = *_sum; 
  sum += "ctub";
  Update( _sum, (TGeoTube *)shape );
  for ( Int_t i=0;i<3;i++ ) {
    sum += Round(shape->GetNhigh()[i]);
    sum += Round(shape->GetNlow()[i]);
  }
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoEltu *shape ) { CheckSum_t &sum = *_sum; 
  sum += "eltu";
  sum += Round(shape->GetA());
  sum += Round(shape->GetB());
  sum += Round(shape->GetDz());
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoHype *shape ) { CheckSum_t &sum = *_sum; 
  sum += "hype";
  Update( _sum, (TGeoTube *)shape );
  sum += Round(shape->GetStIn());
  sum += Round(shape->GetStOut());
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoPara *shape ) { CheckSum_t &sum = *_sum;
  sum += "para";
  Update( _sum, (TGeoBBox *)shape );
  sum += Round(shape->GetAlpha());
  sum += Round(shape->GetPhi());
  sum += Round(shape->GetTheta());
  sum += Round(shape->GetX());
  sum += Round(shape->GetY());
  sum += Round(shape->GetZ());
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoPcon *shape ) { CheckSum_t &sum = *_sum;
  sum += "pcon";
  sum += Int_t(shape->GetNz());
  sum += Round(shape->GetPhi1());
  sum += Round(shape->GetDphi());
  for ( Int_t iz = 0; iz< shape->GetNz(); iz++ )
    {
      sum += Round(shape->GetZ(iz));
      sum += Round(shape->GetRmin(iz));
      sum += Round(shape->GetRmax(iz));
    }
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoPgon *shape ) { CheckSum_t &sum = *_sum; 
  sum += "pgon";
  Update( _sum, (TGeoPcon *)shape );
  sum += Int_t(shape->GetNedges());
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoSphere *shape ) { CheckSum_t &sum = *_sum; 
  sum += "sphere";
  sum += Round(shape -> GetRmin());
  sum += Round(shape -> GetRmax());
  sum += Round(shape -> GetPhi1());
  sum += Round(shape -> GetPhi2());
  sum += Round(shape -> GetTheta1());
  sum += Round(shape -> GetTheta2());
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoTorus *shape ) { CheckSum_t &sum = *_sum; 
  sum += "torus";
  sum += Round(shape -> GetRmin());
  sum += Round(shape -> GetRmax());
  sum += Round(shape -> GetR());
  sum += Round(shape -> GetPhi1());
  sum += Round(shape -> GetDphi());
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoTrd1 *shape ) { CheckSum_t &sum = *_sum;
  sum += "trd1";
  sum += Round(shape -> GetDx1());
  sum += Round(shape -> GetDx2());
  sum += Round(shape -> GetDy());
  sum += Round(shape -> GetDz());
}
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoTrd2 *shape ) { CheckSum_t &sum = *_sum; 
  sum += "trd2";
  sum += Round(shape -> GetDx1());
  sum += Round(shape -> GetDx2());
  sum += Round(shape -> GetDy1());
  sum += Round(shape -> GetDy2());
  sum += Round(shape -> GetDz());
}


// Update checksums with material properties
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoMaterial *material )
{
  CheckSum_t   &sum = *_sum;  
  sum += Round( material->GetA() );
  sum += Round( material->GetZ() );
  sum += Round( material->GetDensity() );
  sum += Round( material->GetRadLen() ); 
  sum += Round( material->GetIntLen() );
  Bool_t mix = material->IsMixture();
  sum += Int_t( mix );
  return; // because the above is sufficient
  /*  
  if ( mix )
    {
      TGeoMixture *mixture = (TGeoMixture *)material;
      Int_t nele = mixture -> GetNelements();
      sum += nele;
      for ( Int_t i=0;i<nele; i++ )
	{
	  TGeoElement *element = mixture->GetElement(i);
	  sum += shiftDecimal( element->A(), 5);
	  sum += shiftDecimal( element->Z(), 5);
	  if ( mixture->GetNmixt() )	  sum += shiftDecimal(mixture->GetNmixt()[i], 5);
	  if ( mixture->GetWmixt() )      sum += shiftDecimal(mixture->GetWmixt()[i], 5);
	  if ( mixture->GetZmixt() )      sum += shiftDecimal(mixture->GetZmixt()[i], 5);
	}
    }
  */
};

// Updates checksum for the specified volume.  
void StarAgmlChecker::Update( CheckSum_t *_sum, TGeoVolume *volume )
{
  CheckSum_t &sum = *_sum;
  TString name = volume->GetName();

  
   // If it is a sensitive volume, name could be significant...
  // if ( volume->GetMedium()->GetParam(0) ) sum += name;


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


      // Add the child's checksum.
      {
	TMD5 md5kid = CheckSum( kid->GetName() );
	Long64_t digest[2];
	md5kid.Final( (UChar_t *)&digest );
	sum += digest[0];
	sum += digest[1];
      }

      // Add in the matrix
      Update( _sum, mat );
      
      // Child's checksum is insufficient, as it will only be evaluated
      // once.  This does not pick up position-time paramters ala gposp.
      // So, for non-assembly volumes we evaluate the shape, medium and
      // material for the daughters as well.
      if ( !kid->IsAssembly() ) {
	TGeoShape    *shape = node->GetVolume()->GetShape();    // shape parameters from node
	TGeoMaterial *mate  = node->GetVolume()->GetMaterial(); // material parameters from node
	TGeoMedium   *med   = node->GetVolume()->GetMedium();   // medium parameters from node
	Update( _sum, shape );
	Update( _sum, mate );
	Update( _sum, med );
      }


    };

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

  TString Name = name;  
  for ( UInt_t ii=0;ii<mSkipList.size();ii++ )
    {
      if ( mSkipList[ii]==name ) return NULL;
    }

  TDataSet *set = new TDataSet(name);   set->SetTitle( CheckSum(name).AsString() );
  std::map< TString, Int_t > kids;
  
  for ( Int_t i=0;i<top->GetNdaughters(); i++ )
    {
      TGeoNode   *node = top->GetNode(i);
      TGeoVolume *kid  = node->GetVolume();
      if ( kids[kid->GetName()]++ ) continue; // skip if already in set
      TDataSet *ns = CheckSet( kid->GetName() );
      if (ns) set->Add( ns );   // recursively add daughters
      
    }
  return set;

};
