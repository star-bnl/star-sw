#include "G4UnitsTable.hh"
#include "G4VVisManager.hh"
#include "G4ParticleTypes.hh"
#include "G4Circle.hh"
#include "G4Colour.hh"
#include "G4VisAttributes.hh"
#include "G4RotationMatrix.hh"

#include <iomanip>

#include "RHICfZDCHit.hh"

/// allocator
G4Allocator<RHICfZDCHit> RHICfZDCHitAllocator;

//////////
/// Constructor and Destructor
RHICfZDCHit::RHICfZDCHit(): module(-1), nphoton(-1), edep(0.)
{
  module=-1;
  nphoton=-1;
  edep=0.;
}

RHICfZDCHit::RHICfZDCHit(G4int amodule, G4int anphoton, G4double aedep)
{
  module=amodule;
  nphoton=anphoton;
  edep=aedep;
}

RHICfZDCHit::~RHICfZDCHit()
{
}

//////////
/// Draw
void RHICfZDCHit::Draw()
{
}

//////////
/// Print
void RHICfZDCHit::Print()
{
  G4cout << "ZDC Hit >>> Module: " << module
	 << " Nphoton:  " << nphoton << " "
	 << " Energy:  " << edep/CLHEP::MeV << " MeV"
	 << G4endl;
}
