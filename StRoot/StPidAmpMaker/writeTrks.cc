/***************************************************************************
 *
 * $Id: writeTrks.cc,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             write StPidAmpTrks to the disk 
 *             for quick tune purpose
 ***************************************************************************
 *
 * $Log: writeTrks.cc,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#include <fstream.h>

#include "StEventTypes.h"
#include "StPhysicalHelixD.hh"


void writeTrks(StEvent& event){

  ofstream foutD;

  foutD.open("80evtsTrks.dat", ios::app);

  if (!foutD) {cout<<"Cannot open file.\n"; }//handle error

      StPrimaryVertex* primaryVtx=event.primaryVertex();

   const StSPtrVecTrackNode& nodes=event.trackNodes();
   StDedxPidTraits* dedxPidTr;

   for (int i=0; i<nodes.size(); i++) {
          double rig;
          double dedx;
          int    charge;
          double pt;
          int    nhits;
          double dca;



   StTrack* theTrack=nodes[i]->track(global);
    charge=int((theTrack->geometry())->charge());

   StSPtrVecTrackPidTraits& traits=theTrack->pidTraits();

       for (int itrait = 0; itrait < traits.size(); itrait++){
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

           if (dedxPidTr &&  dedxPidTr->method() == kTruncatedMeanId) {


             dedx=dedxPidTr->mean();
             nhits=dedxPidTr->numberOfPoints();
	   }
       }

   
    StPhysicalHelixD& helix=theTrack->geometry()->helix();
    const StThreeVectorF& p=theTrack->geometry()->momentum();
    rig=double(p.mag()/charge);
    pt=double(p.perp());
    dca=helix.distance(primaryVtx->position());

 if (dedxPidTr) foutD<<rig<<" "<<dedx<<" "<<charge<<" "<<pt<<" "<<nhits<<" "<<dca<<endl;
   }

   foutD.close();

}
