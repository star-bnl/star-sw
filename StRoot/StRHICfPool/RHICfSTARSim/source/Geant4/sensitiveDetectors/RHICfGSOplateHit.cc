#include "G4UnitsTable.hh"
#include "G4VVisManager.hh"
#include "G4ParticleTypes.hh"
#include "G4Circle.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4RotationMatrix.hh"

#include <iomanip>

#include "RHICfGSOplateHit.hh"

/// allocator
G4Allocator<RHICfGSOplateHit> RHICfGSOplateHitAllocator;

//////////
/// Constructor and Destructor
RHICfGSOplateHit::RHICfGSOplateHit(): tower(-1), plate(-1), edep_truth(0.), edep(0.)
{
  tower=-1;
  plate=-1;
  edep_truth=0.;
  edep=0.;
}

RHICfGSOplateHit::RHICfGSOplateHit(G4int atower, G4int aplate, G4double aedep_truth, G4double aedep)
{
  tower=atower;
  plate=aplate;
  edep_truth=aedep_truth;
  edep=aedep;
}

RHICfGSOplateHit::~RHICfGSOplateHit()
{
}

//////////
/// Draw
void RHICfGSOplateHit::Draw()
{
}

//////////
/// Print
void RHICfGSOplateHit::Print()
{
  G4cout << "GSO plate Hit >>> Tower: " << tower
	 << " Plate: " << plate
	 << " Energy(truth):  " << edep_truth/CLHEP::MeV << " MeV"
	 << " Energy:  " << edep/CLHEP::MeV << " MeV"
	 << G4endl;
}
