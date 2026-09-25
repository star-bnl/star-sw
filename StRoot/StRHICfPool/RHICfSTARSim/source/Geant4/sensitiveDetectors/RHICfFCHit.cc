#include "G4UnitsTable.hh"
#include "G4VVisManager.hh"
#include "G4ParticleTypes.hh"
#include "G4Circle.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4RotationMatrix.hh"

#include <iomanip>

#include "RHICfFCHit.hh"

/// allocator
G4Allocator<RHICfFCHit> RHICfFCHitAllocator;

//////////
/// Constructor and Destructor
RHICfFCHit::RHICfFCHit(): tower(-1), edep(0.)
{
}

RHICfFCHit::RHICfFCHit(G4int atower, G4double aedep)
{
  tower=atower;
  edep=aedep;
}

RHICfFCHit::~RHICfFCHit()
{
}

//////////
/// Draw
void RHICfFCHit::Draw()
{
}

//////////
/// Print
void RHICfFCHit::Print()
{
  G4cout << "FC Hit >>> Channel:  " << tower << " "
	 << " Energy:  " << edep/CLHEP::MeV << " MeV"
	 << G4endl;
}
