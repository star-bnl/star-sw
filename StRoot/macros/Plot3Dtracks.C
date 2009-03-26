#ifndef __CINT__
#include "StChain.h"
#include "StEvent.h"
#include "StTrack.h"
#include "StEventSummary.h"
#include "StuDraw3DEvent.h"
#include <stdlib.h>
#include <stdio.h>
#endif
void Plot3Dtracks(int eventNumber=22166, int trackId=425, const char *file="st_physics_8112087_raw_1020015.event.root"){
 // This example shpws how to use Draw3D class top draw  the 
 // track if one knows the track event nu,mber anad track id.
 
  gROOT->Macro("Load.C");
  gROOT->LoadMacro("bfc.C");
  bfc(0,"in,StEvent",file); 
  StEvent *event = 0;
  gEventDisplay->SetBkColor(kWhite);
  int MyEventId=eventNumber;
  int MyTrackId=trackId;
  printf(" Look up the file %s to find the track %d from the event %d \n"
       , file,MyTrackId,MyEventId);
  
  bool trackWasFound = false;
  bool eventWasFound = false;

  while ( !chain->MakeEvent() && !eventWasFound ) {
    // Access the StEvent pointer
    event  =  (StEvent *)chain->GetDataSet("StEvent");
    if (chain->GetEventNumber() == MyEventId) eventWasFound = true;  // select MyRunId
    printf(" ----------- %d  ----------\n", chain->GetEventNumber());
  }
  if (eventWasFound) {
     printf(" -++++++++++++++++++++++++++++++++ %p total tracks=%d\n",event
           ,event->summary()->numberOfTracks());
     const StSPtrVecTrackNode& theNodes = event->trackNodes();
     int sz = theNodes.size();
     printf(" -+++++++++++++++ %d +++++++++++++++++\n", sz);
     StTrack *track = 0;
     for (unsigned int i=0; i<sz; i++) {
        printf(" -track %d ---------- %d  ----------\n", i, theNodes[i]);
        track = theNodes[i]->track(global);
        printf(" -track ---------- %d  ----------\n", track->key());
        // select tracks
        trackWasFound  = (track && track->key() == MyTrackId);
      }
      if (trackWasFound) gEventDisplay->Track(*track);
      else {
          printf("ATTENTION: No track %d was found in %d event from file <%s>\n"
                , MyTrackId, MyEventId, file);
      }

  } else {
     printf("ATTENTION: No event %d was found from file <%s> !!!\n", MyEventId, file);
  }
}
