#include "StHelixModel.h"
#include "StPhysicalHelixD.hh"
#include "StPrimaryVertex.h"
#include "StEnumerations.h"
#include "StEventTypes.h"

#include "StPidAmpMaker/StPidAmpTrkVector.h"
#include "StPidAmpMaker/Infrastructure/StPidAmpTrk.hh"


void fillStPidAmpTrks4StandardStEvent(StEvent& event, StPidAmpTrkVector* trks,Int_t dedxMethod){


      StPrimaryVertex* primaryVtx=event.primaryVertex();


      if (primaryVtx){

  const StSPtrVecTrackNode& nodes=event.trackNodes();
   StDedxPidTraits* dedxPidTr;

   for (int i=0; i<int(nodes.size()); i++) {
          double rig;
          double dedx;
          int    charge;
          double pt;
          int    nhits;
          double dca;
    const  StTrack* theTrack=nodes[i]->track(global);
    charge=int((theTrack->geometry())->charge());
    const StSPtrVecTrackPidTraits& traits=theTrack->pidTraits();
       for (int itrait = 0; itrait < int(traits.size()); itrait++){
           dedxPidTr = 0;
           if (traits[itrait]->detector() == kTpcId) {
             //
             // tpc pid trait
             //
             StTrackPidTraits* thisTrait = traits[itrait];
             //
             // perform cast to make the pid trait a dedx trait
             //
             dedxPidTr = dynamic_cast<StDedxPidTraits*>(thisTrait);
           }
           if (dedxPidTr &&  dedxPidTr->method() == dedxMethod) {
           dedx=dedxPidTr->mean();
             nhits=dedxPidTr->numberOfPoints();
           }
       }
  const  StPhysicalHelixD& helix=theTrack->geometry()->helix();
    const StThreeVectorF& p=theTrack->geometry()->momentum();
    rig=double(p.mag()/charge);
    pt=double(p.perp());
   dca=helix.distance(primaryVtx->position());

   StPidAmpTrk* theAmpTrk=new StPidAmpTrk(rig, dedx, charge,pt, nhits, dca);
   trks->push_back(theAmpTrk);

   }
      }



}
