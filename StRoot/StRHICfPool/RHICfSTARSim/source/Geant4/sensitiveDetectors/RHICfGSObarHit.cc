#include "G4UnitsTable.hh"
#include "G4VVisManager.hh"
#include "G4ParticleTypes.hh"
#include "G4Circle.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4RotationMatrix.hh"

#include <iomanip>

#include "RHICfGSObarHit.hh"

/// allocator
G4Allocator<RHICfGSObarHit> RHICfGSObarHitAllocator;

//////////
/// Constructor and Destructor
RHICfGSObarHit::RHICfGSObarHit()
{
  tower=-1;
  belt=-1;
  xy=-1;
  bar=-1;
  edep_truth=0.;
  edep=0.;
}

RHICfGSObarHit::RHICfGSObarHit(G4int atower, G4int abelt, G4int axy, G4int abar, G4double aedep_truth, G4double aedep)
{
  tower=atower;
  belt=abelt;
  xy=axy;
  bar=abar;
  edep_truth=aedep_truth;
  edep=aedep;
}

RHICfGSObarHit::~RHICfGSObarHit()
{
}

//////////
/// Draw
void RHICfGSObarHit::Draw()
{
}

//////////
/// Print
void RHICfGSObarHit::Print()
{
  G4cout << "GSO plate Hit >>> Tower: " << tower
	 << " Belt: " << belt
	 << " XY: " << xy
	 << " Bar: " << bar
	 << " Energy(true):  " << edep_truth/CLHEP::MeV << " MeV"
	 << " Energy:  " << edep/CLHEP::MeV << " MeV"
	 << G4endl;
}
