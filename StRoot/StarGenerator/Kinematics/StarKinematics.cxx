#include "StarKinematics.h"
#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"
#include "StarGenerator/UTIL/StarParticleData.h"
#include "StarGenerator/UTIL/StarRandom.h"

#include "TVector3.h"
#define random myrandom

// ----------------------------------------------------------------------------

StarParticleData &data = StarParticleData::instance();
StarRandom       &random = StarRandom::Instance();

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
void StarKinematics::Kine(Int_t ntrack, const Char_t *type, Double_t ptlow, Double_t pthigh,
			  Double_t ylow, Double_t yhigh,
			  Double_t philow, Double_t phihigh )
{

  for ( Int_t i=0;i<ntrack;i++ )
    {

      StarGenParticle *p = AddParticle(type);

      // Sample pt, eta, phi
      Double_t pt = random(ptlow,  pthigh);
      Double_t eta= random(ylow,   yhigh );
      Double_t phi= random(philow, phihigh );
      Double_t m  = p->GetMass();

      // Use TVector3 to get the momentum vector correct
      TVector3 momentum; {
	momentum.SetPtEtaPhi( pt, eta, phi );
      }

      Double_t E2 = momentum.Mag2() + m*m;
      Double_t E  = sqrt(E2);

      p->SetPx( momentum.Px() );
      p->SetPy( momentum.Py() );
      p->SetPz( momentum.Pz() );
      p->SetEnergy( E );

      p->SetVx( 0. ); // put vertex at 0,0,0,0
      p->SetVy( 0. );
      p->SetVz( 0. );
      p->SetTof( 0. );

    }

}
// ----------------------------------------------------------------------------
void StarKinematics::Dist( Int_t ntrack, const Char_t *type, TF1 *ptFunc, TF1 *etaFunc, TF1 *phiFunc )
{
  for ( Int_t i=0; i<ntrack; i++ )
    {

      StarGenParticle *p = AddParticle(type);
      
      Double_t pt  = ptFunc  -> GetRandom();
      Double_t eta = etaFunc -> GetRandom();
      Double_t phi = (phiFunc) ? phiFunc->GetRandom() : random( 0., TMath::TwoPi() );
      Double_t m   = p->GetMass();

      // Use TVector3 to get the momentum vector correct
      TVector3 momentum; {
	momentum.SetPtEtaPhi( pt, eta, phi );
      }

      Double_t E2 = momentum.Mag2() + m*m;
      Double_t E  = sqrt(E2);

      p->SetPx( momentum.Px() );
      p->SetPy( momentum.Py() );
      p->SetPz( momentum.Pz() );
      p->SetEnergy( E );

      p->SetVx( 0. ); // put vertex at 0,0,0,0
      p->SetVy( 0. );
      p->SetVz( 0. );
      p->SetTof( 0. );

    }
}
// ----------------------------------------------------------------------------
void StarKinematics::Dist( Int_t ntrack, const Char_t *type, TH1 *ptFunc, TH1 *etaFunc, TH1 *phiFunc )
{
  for ( Int_t i=0; i<ntrack; i++ )
    {

      StarGenParticle *p = AddParticle(type);
      
      Double_t pt  = ptFunc  -> GetRandom();
      Double_t eta = etaFunc -> GetRandom();
      Double_t phi = (phiFunc) ? phiFunc->GetRandom() : random( 0., TMath::TwoPi() );
      Double_t m   = p->GetMass();

      // Use TVector3 to get the momentum vector correct
      TVector3 momentum; {
	momentum.SetPtEtaPhi( pt, eta, phi );
      }

      Double_t E2 = momentum.Mag2() + m*m;
      Double_t E  = sqrt(E2);

      p->SetPx( momentum.Px() );
      p->SetPy( momentum.Py() );
      p->SetPz( momentum.Pz() );
      p->SetEnergy( E );

      p->SetVx( 0. ); // put vertex at 0,0,0,0
      p->SetVy( 0. );
      p->SetVz( 0. );
      p->SetTof( 0. );

    }
}
// ----------------------------------------------------------------------------
const double deg2rad = TMath::DegToRad();

void StarKinematics::Cosmic( int ntrack, const char* type, double plow, double phigh, double radius, double zmin, double zmax, double dphi )
{
  for ( Int_t i=0; i<ntrack; i++ )
    {

      StarGenParticle *p = AddParticle(type);

      // Generate a random vertex
      double zvertex = random( zmin, zmax );
      double phi     = random( 0.0, TMath::TwoPi() );
      double xvertex = radius * TMath::Cos(phi);
      double yvertex = radius * TMath::Sin(phi);

      // Initialize vertex X,Y ... to get unit vector pointing to beam line
      TVector3 vertex(xvertex,yvertex,0);

      // Unit vector pointing away from beamline
      TVector3 direct = vertex.Unit();

      // Sample momentum distribution
      double pmag    = random(plow,  phigh);

      // Momentum vector pointing in towards the beamline
      TVector3 momentum = -pmag * direct;

      // Now, randomize phi and theta by +/- dphi degrees about the momentum axis
             phi   = momentum.Phi()   + deg2rad * random( -dphi, +dphi );
      double theta = momentum.Theta() + deg2rad * random( -dphi, +dphi );

      momentum.SetPhi(phi);
      momentum.SetTheta(theta);            

      Double_t m   = p->GetMass();

      Double_t E2 = momentum.Mag2() + m*m;
      Double_t E  = sqrt(E2);

      p->SetPx( momentum.Px() );
      p->SetPy( momentum.Py() );
      p->SetPz( momentum.Pz() );
      p->SetEnergy( E );

      p->SetVx( xvertex ); 
      p->SetVy( yvertex );
      p->SetVz( zvertex );
      p->SetTof( 0. );

    }
}
// ----------------------------------------------------------------------------
