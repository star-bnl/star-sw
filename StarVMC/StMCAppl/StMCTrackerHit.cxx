// $Id: StMCTrackerHit.cxx,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//
// Geant4 ExampleN02 adapted to Virtual Monte Carlo 
//
// Id: StMCTrackerHit.cc,v 1.7 2002/01/09 17:24:10 ranjard Exp 
// GEANT4 tag Name: geant4-04-00-patch-02 
//
//
// by Ivana Hrivnacova, 21.4.2002

#include "Riostream.h"

#include "StMCTrackerHit.h"

ClassImp(StMCTrackerHit)

using namespace std;

//_____________________________________________________________________________
StMCTrackerHit::StMCTrackerHit() 
  : fTrackID(-1),
    fChamberNb(-1),
    fEdep(0.),
    fPos()
{}

//_____________________________________________________________________________
StMCTrackerHit::~StMCTrackerHit() 
{}

/*
//_____________________________________________________________________________
void StMCTrackerHit::Draw()
{
  G4VVisManager* pVVisManager = G4VVisManager::GetConcreteInstance();
  if(pVVisManager)
  {
    G4Circle circle(pos);
    circle.SetScreenSize(0.04);
    circle.SetFillStyle(G4Circle::filled);
    G4Colour colour(1.,0.,0.);
    G4VisAttributes attribs(colour);
    circle.SetVisAttributes(attribs);
    pVVisManager->Draw(circle);
  }
}
*/

//_____________________________________________________________________________
void StMCTrackerHit::Print(const Option_t* opt) const
{
  /*  cout << "  trackID: " << fTrackID 
       << "  chamberNb: " << fChamberNb
       << "  energy deposit (keV): " << fEdep * 1.0e06
       << "  position (cm): (" 
       << fPos[0] << ", " << fPos[1] << ", " << fPos[2] << ")"
       << endl;
  */
}

