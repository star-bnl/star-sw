// $Id: StMcEventInterface.cxx,v 1.2 2004/07/16 22:57:53 potekhin Exp $
// $Log: StMcEventInterface.cxx,v $
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

  //  StMcEvent* e = new StMcEvent();
  //   e->initToZero();


  // Now do individual hits
  //  TIterator*   it = hits_->MakeIterator();
  //  StarHit*      h = (StarHit*) it->Next();

  //  while(h) {
  //    TVector3 pos = h->GetPos();

    //    StThreeVectorF stPosition, stMomentum;

    //    stPosition.setX(pos.X());
    //    stPosition.setY(pos.Y());
    //    stPosition.setZ(pos.Z());

  //    float energy = h->GetEdep();
  //    float dS(0);

    //    StMcTpcHit StH(stPosition, stMomentum, energy, dS, 0, 0, 0);

    //    pm->SetNextPoint(pos.X(),pos.Y(),pos.Z());
    //    h=(StarHit*) it->Next();
  //  }


  cout<<"finished"<<endl;
}
