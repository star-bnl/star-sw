// $Id: StarMCDisplay.cxx,v 1.2 2004/07/16 22:52:35 potekhin Exp $
// $Log: StarMCDisplay.cxx,v $
// Revision 1.2  2004/07/16 22:52:35  potekhin
// Incremental changes
//
// Revision 1.1  2004/07/13 19:03:51  potekhin
// Initial check in
//

#include <iostream.h>

#include <TROOT.h>
#include <TGeoManager.h>
#include "TPolyMarker3D.h"

#include "StarMCDisplay.h"
#include "StarHit.h"


ClassImp(StarMCDisplay)

//_____________________________________________________________________________
  StarMCDisplay::StarMCDisplay(): _c("StarMC","Test")
{
  _c.SetTheta(90.0);
}

//_____________________________________________________________________________
void StarMCDisplay::DrawVolume(void) {
  TGeoVolume* v = gGeoManager->GetTopVolume();
  v->Draw();
}

//_____________________________________________________________________________
void StarMCDisplay::Update(void) {
  _c.Update();
}

//_____________________________________________________________________________
void StarMCDisplay::DrawHits(TObjArray* h_) const {

  TPolyMarker3D* pm = new TPolyMarker3D(1000,20);
  pm->SetMarkerSize(1);
  pm->SetMarkerColor(2);

  // Now do individual hits
  TIterator*   it = h_->MakeIterator();
  StarHit*      h = (StarHit*) it->Next();

  while(h) {
    TVector3 pos = h->GetPos();
    pm->SetNextPoint(pos.X(),pos.Y(),pos.Z());
    h=(StarHit*) it->Next();
  }

  pm->Draw();

}
