// $Id: StMcEventInterface.cxx,v 1.3 2004/09/02 23:36:42 potekhin Exp $
// $Log: StMcEventInterface.cxx,v $
// Revision 1.3  2004/09/02 23:36:42  potekhin
// more code put in
//
// Revision 1.2  2004/07/16 22:57:53  potekhin
// Comments
//
// Revision 1.1  2004/07/13 19:08:01  potekhin
// Interface to StMcEvent
//

#include <TROOT.h>
#include <iostream.h>
#include "StMcEventInterface.h"
#include "StarHit.h"

#include "StThreeVectorF.hh"
#include "StMcTpcHit.hh"
#include "StMcTrack.hh"


ClassImp(StMcEventInterface)


//_______________________________________________________________________
void StMcEventInterface::FinishEventCB(TObjArray* hits_) {
  cout<<"Creating a StMcEvent"<<endl;

  StMcEvent* e = new StMcEvent();
  e->initToZero();
  cout<<"Init passed "<<e<<endl;


  // Now do individual hits
  TIterator*   it = hits_->MakeIterator();
  StarHit*      h = (StarHit*) it->Next();

  while(h) {
    TVector3 pos  = h->GetPos();
    Int_t trackID = h->GetTrackID();

    StThreeVectorF stPosition, stMomentum;


    //    cout << "trackID "<< trackID << "x " << pos.X() << " y " << pos.Y() << " z " << pos.Z() << endl;

    stPosition.setX(pos.X());
    stPosition.setY(pos.Y());
    stPosition.setZ(pos.Z());

    float energy = h->GetEdep();
    float dS(0);

    StMcTpcHit StH(stPosition, stMomentum, energy, dS, 0, 0, 0);

    //    pm->SetNextPoint(pos.X(),pos.Y(),pos.Z());

    h=(StarHit*) it->Next();
  }


  cout<<"finished StMcEvent"<<endl;
}
