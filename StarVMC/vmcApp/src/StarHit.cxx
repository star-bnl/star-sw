// $Id: StarHit.cxx,v 1.1 2004/07/12 20:36:38 potekhin Exp $
//

#include <iostream.h>

#include "StarHit.h"

ClassImp(StarHit)

using namespace std;

//_____________________________________________________________________________
StarHit::StarHit() 
  : _trackID(-1),
    _volumeID(-1),
    _edep(0.),
    _pos()
{}

//_____________________________________________________________________________
StarHit::~StarHit() 
{}

/*
//_____________________________________________________________________________
void StarHit::Draw()
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
void StarHit::Print(const Option_t* opt) const
{
  cout << "  trackID: "   << _trackID 
       << "  volumeID: "  << _volumeID
       << "  energy deposit (keV): " << _edep * 1.0e06
       << "  position (cm): (" 
       << _pos[0] << ", " << _pos[1] << ", " << _pos[2] << ")"
       << endl;
}

