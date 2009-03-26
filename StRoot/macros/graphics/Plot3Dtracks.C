// $Id: Plot3Dtracks.C,v 1.1 2009/03/26 19:31:08 fine Exp $
// Author: Valeri Fine, 26.03.2009 (fine@bnl.gov)

void Plot3Dtracks(int eventNumber=22394, int trackId=425, const char *file="st_physics_8112087_raw_1020015.event.root"){
 // This example shows how to use Draw3D class to draw  the 
 // track if one knows the track "event number" and "track id".
 
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

  while (!eventWasFound && !chain->MakeEvent() ) {
     // Access the StEvent pointer
     event  =  (StEvent *)chain->GetDataSet("StEvent");
     if (chain->GetEventNumber() == MyEventId) { eventWasFound = true; break; }
     printf(" ----- Current event %d keep moving towards event %d\n"
         ,chain->GetEventNumber(), MyEventId);
  }
  if (eventWasFound) {
     // printf(" -++++++++++++++++++++++++++++++++ %p total tracks=%d\n",event
     //      ,event->summary()->numberOfTracks());
     const StSPtrVecTrackNode& theNodes = event->trackNodes();
     StTrack *track = 0; int sz = theNodes.size();
     for (unsigned int i=0; i<sz; i++) {
        track = theNodes[i]->track(global);
        printf(" - current track - %d  ---looking for %d \n", track->key(),MyTrackId);
        // select tracks
        if  (track && (track->key() == MyTrackId)) { trackWasFound  = true;  break; }
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
