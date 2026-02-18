#include "G4UnitsTable.hh"
#include "G4VVisManager.hh"
#include "G4ParticleTypes.hh"
#include "G4Circle.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4RotationMatrix.hh"

#include <iomanip>

#include "RHICfSMDHit.hh"

/// allocator
G4Allocator<RHICfSMDHit> RHICfSMDHitAllocator;

//////////
/// Constructor and Destructor
RHICfSMDHit::RHICfSMDHit(): xy(-1), smd(-1), edep(0.)
{
}

RHICfSMDHit::RHICfSMDHit(G4int axy, G4int asmd, G4double aedep)
{
  xy=axy;
  smd=asmd;
  edep=aedep;
}

RHICfSMDHit::~RHICfSMDHit()
{
}

//////////
/// Draw
void RHICfSMDHit::Draw()
{
}

//////////
/// Print
void RHICfSMDHit::Print()
{
  G4cout << "SMD Hit >>> Dir: " << xy
	 << " Channel:  " << smd << " "
	 << " Energy:  " << edep/CLHEP::MeV << " MeV"
	 << G4endl;
}
