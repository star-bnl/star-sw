// $Id: StarHit.cxx,v 1.2 2004/07/13 19:12:04 potekhin Exp $
// $Log: StarHit.cxx,v $
// Revision 1.2  2004/07/13 19:12:04  potekhin
// Removed the G4 specific calls
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
StarHit::~StarHit() {}

/*
//_____________________________________________________________________________
void StarHit::Draw() {}
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

