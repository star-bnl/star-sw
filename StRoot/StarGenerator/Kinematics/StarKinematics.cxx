#include "StarKinematics.h"
#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"
#include "StarGenerator/UTIL/StarParticleData.h"
#include "StarGenerator/UTIL/StarRandom.h"

#include <boost/algorithm/string.hpp>
#include <vector>
#include <string>
#include <algorithm>
#include <random>

#define random myrandom



// ----------------------------------------------------------------------------

StarParticleData &data = StarParticleData::instance();
StarRandom       &random = StarRandom::Instance();

static std::random_device _stl_rd;
static std::mt19937 _mt19937( _stl_rd() );

StarGenEvent *gEvent = 0;
StarGenEvent *gUser  = 0;

// ----------------------------------------------------------------------------
StarKinematics::StarKinematics( const Char_t *name ) : StarGenerator(name)
{ 
  mEvent = new StarGenEvent("kine"); gEvent = mEvent;
  mUser  = new StarGenEvent("user"); gUser  = mUser;
}
// ----------------------------------------------------------------------------
Int_t StarKinematics::PreGenerate()
{
  return kStOK;
}
// ----------------------------------------------------------------------------
Int_t StarKinematics::Generate()
{
  
  // Copy user event into mEvent
  for ( Int_t i=0;i<mUser->GetNumberOfParticles(); i++ )
    {
      mEvent -> AddParticle( (*mUser)[i] );
    }

  // And clear the user event
  mUser->Clear();

  return kStOK;
}
// ----------------------------------------------------------------------------
Int_t StarKinematics::PostGenerate()
{
  return kStOK;
}
// ----------------------------------------------------------------------------
StarGenParticle *StarKinematics::AddParticle()
{
  StarGenParticle *p = mUser->AddParticle();
  p->SetStatus( StarGenParticle::kFinal );
  return p;
}
// ----------------------------------------------------------------------------
StarGenParticle *StarKinematics::AddParticle( const Char_t *type )
{
  TParticlePDG *pdg = data(type); assert(pdg);
  Int_t id = pdg->PdgCode();
  StarGenParticle *p = AddParticle();
  p->SetStatus( StarGenParticle::kFinal );
  p->SetMass( pdg->Mass() );
  p->SetId( id );
  return p;
}	
// ----------------------------------------------------------------------------
void StarKinematics::Kine(Int_t ntrack, const Char_t *_type, Double_t ptlow, Double_t pthigh,
			  Double_t ylow, Double_t yhigh,
			  Double_t philow, Double_t phihigh )
{

  std::string type = _type;
  std::vector<std::string> types;
  boost::split( types, type,  [](char c){ return (c==' ' || c== ',');} );

  for ( Int_t i=0;i<ntrack;i++ )
    {
      
      std::shuffle( types.begin(), types.end(), _mt19937 );
      type = types[0];
      
      StarGenParticle *p = AddParticle(type.c_str());

      // Sample pt, eta, phi
      if ( 0 == IAttr("energy") ) { // uniform in pT
	double pt = random(ptlow,  pthigh);
	double y  = random(ylow,   yhigh );
	double phi= random(philow, phihigh );
	double m  = p->GetMass();

	double mt = pt;
	if ( IAttr("rapidity" ) ) { 
	  // switch from pseudorapidity to plain vanilla rapidity
	  mt = TMath::Sqrt( pt*pt + m*m );
	}
      
	double px = pt * TMath::Cos( phi );
	double py = pt * TMath::Sin( phi );
	double pz = mt * TMath::SinH( y ); 
	double E  = mt * TMath::CosH( y );

	p->SetPx( px );
	p->SetPy( py );
	p->SetPz( pz );
	p->SetEnergy( E );

	p->SetVx( 0. ); // put vertex at 0,0,0,0
	p->SetVy( 0. );
	p->SetVz( 0. );
	p->SetTof( 0. );
      }
      if ( IAttr("energy") ) { // uniform in energy.

	assert( 0==IAttr("rapidity") ); // only flat in eta, not rapidity

	double E  = random(ptlow, pthigh);
	double y  = random(ylow,   yhigh );   // y is eta here, not rapidity
	double phi= random(philow, phihigh );
	double m  = p->GetMass();
	double pmom  = TMath::Sqrt(E*E - m*m);
	double pt = 2.0 * pmom * TMath::Exp( -y ) / ( 1 + TMath::Exp( -2.0*y ) );

	double mt = pt;
	if ( IAttr("rapidity" ) ) { 
	  // switch from pseudorapidity to plain vanilla rapidity
	  mt = TMath::Sqrt( pt*pt + m*m );
	}
 
	double px = pt * TMath::Cos( phi );
	double py = pt * TMath::Sin( phi );
	double pz = mt * TMath::SinH( y ); 
	//double E  = mt * TMath::CosH( y );

	p->SetPx( px );
	p->SetPy( py );
	p->SetPz( pz );
	p->SetEnergy( E );

	p->SetVx( 1e-11); // put vertex at 0,0,0,0
	p->SetVy( 1e-11);
	p->SetVz( 1e-11);
	p->SetTof( 0. );
      }
	

    }

}
// ----------------------------------------------------------------------------
void StarKinematics::Dist( Int_t ntrack, const Char_t *_type, TF1 *ptFunc, TF1 *etaFunc, TF1 *phiFunc )
{

  std::string type = _type;
  std::vector<std::string> types;
  boost::split( types, type,  [](char c){ return (c==' ' || c== ',');} );

  for ( Int_t i=0; i<ntrack; i++ )
    {

      std::shuffle( types.begin(), types.end(), _mt19937 );
      type = types[0];
      
      StarGenParticle *p = AddParticle(type.c_str());
      
      Double_t pt  = ptFunc  -> GetRandom();
      Double_t y   = etaFunc -> GetRandom();
      Double_t phi = (phiFunc) ? phiFunc->GetRandom() : random( 0., TMath::TwoPi() );
      Double_t m   = p->GetMass();

      double mt = pt;
      if ( IAttr("rapidity" ) ) { 
	// switch from pseudorapidity to plain vanilla rapidity
	mt = TMath::Sqrt( pt*pt + m*m );
      }
      
      double px = pt * TMath::Cos( phi );
      double py = pt * TMath::Sin( phi );
      double pz = mt * TMath::SinH( y ); 
      double E  = mt * TMath::CosH( y );

      p->SetPx( px );
      p->SetPy( py );
      p->SetPz( pz );
      p->SetEnergy( E );

      p->SetVx( 0. ); // put vertex at 0,0,0,0
      p->SetVy( 0. );
      p->SetVz( 0. );
      p->SetTof( 0. );

    }
}
// ----------------------------------------------------------------------------
void StarKinematics::Dist( Int_t ntrack, const Char_t *_type, TH1 *ptFunc, TH1 *etaFunc, TH1 *phiFunc )
{
  std::string type = _type;
  std::vector<std::string> types;
  boost::split( types, type,  [](char c){ return (c==' ' || c== ',');} );

  for ( Int_t i=0; i<ntrack; i++ )
    {

      std::shuffle( types.begin(), types.end(), _mt19937 );
      type = types[0];
 
      StarGenParticle *p = AddParticle(type.c_str());
      
      Double_t pt  = ptFunc  -> GetRandom();
      Double_t y   = etaFunc -> GetRandom();
      Double_t phi = (phiFunc) ? phiFunc->GetRandom() : random( 0., TMath::TwoPi() );
      Double_t m   = p->GetMass();

      double mt = pt;
      if ( IAttr("rapidity" ) ) { 
	// switch from pseudorapidity to plain vanilla rapidity
	mt = TMath::Sqrt( pt*pt + m*m );
      }
      
      double px = pt * TMath::Cos( phi );
      double py = pt * TMath::Sin( phi );
      double pz = mt * TMath::SinH( y ); 
      double E  = mt * TMath::CosH( y );

      p->SetPx( px );
      p->SetPy( py );
      p->SetPz( pz );
      p->SetEnergy( E );

      p->SetVx( 0. ); // put vertex at 0,0,0,0
      p->SetVy( 0. );
      p->SetVz( 0. );
      p->SetTof( 0. );

    }
}
// ----------------------------------------------------------------------------
const double deg2rad = TMath::DegToRad();

void StarKinematics::Cosmic( int ntrack, const char* _type, double plow, double phigh, double radius, double zmin, double zmax, double dphi )
{
  std::string type = _type;
  std::vector<std::string> types;
  boost::split( types, type,  [](char c){ return (c==' ' || c== ',');} );

  for ( Int_t i=0; i<ntrack; i++ )
    {

      std::shuffle( types.begin(), types.end(), _mt19937 );
      type = types[0];
 
      StarGenParticle *p = AddParticle(type.c_str());

      // Generate a random vertex
      double zvertex = random( zmin, zmax );
      double phi     = random( 0.0, TMath::TwoPi() );
      double xvertex = radius * TMath::Cos(phi);
      double yvertex = radius * TMath::Sin(phi);

      xvertex *= 10; // cm --> mm per HEPEVT standard 
      yvertex *= 10; // cm --> mm 
      zvertex *= 10; // cm --> mm 

      // Initialize vertex X,Y ... to get unit vector pointing to beam line
      TVector3 vertex(xvertex,yvertex,0);

      // Unit vector pointing away from beamline
      TVector3 direct = vertex.Unit();

      // Sample momentum distribution
      double pmag    = random(plow,  phigh);

      // Momentum vector pointing in towards the beamline
      _momentum = -pmag * direct;

      // Now, randomize phi and theta by +/- dphi degrees about the momentum axis
             phi   = _momentum.Phi()   + deg2rad * random( -dphi, +dphi );
      double theta = _momentum.Theta() + deg2rad * random( -dphi, +dphi );

      _momentum.SetPhi(phi);
      _momentum.SetTheta(theta);            

      Double_t m   = p->GetMass();

      Double_t E2 = _momentum.Mag2() + m*m;
      Double_t E  = sqrt(E2);

      p->SetPx( _momentum.Px() );
      p->SetPy( _momentum.Py() );
      p->SetPz( _momentum.Pz() );
      p->SetEnergy( E );

      p->SetVx( xvertex ); 
      p->SetVy( yvertex );
      p->SetVz( zvertex );
      p->SetTof( 0. );

    }
}
// ----------------------------------------------------------------------------
