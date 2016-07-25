void CheckdetectorInfo() {
  StEvent* pEvent = (StEvent*) chain->GetInputDS("StEvent");
  if (!pEvent) return;
  cout << "Event: Run "<< pEvent->runId() << " Event No: " << pEvent->id() << endl;
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node = 0;
  Int_t line = 0;
  for (unsigned int i=0; i < nTracks; i++) {
    node = trackNode[i]; if (!node) continue;
    StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(node->track(global));
    StDcaGeometry* dca    = gTrack->dcaGeometry();
    StPrimaryTrack *pTrack = 	static_cast<StPrimaryTrack*>(node->track(primary));
    for (int l = 0; l < 2; l++) {
      StTrack        *track = 0;
      if (l == global)  track = gTrack;
      if (l == primary) track = pTrack;
      if (track) {
	if (! track->detectorInfo()) {
	  cout << "detectorInfo is missing for track " << l << " ==================" << endl;
	} else {
	  cout << "detectorInfo exists for track " << l << " ==================" << endl;
	}
      }
    }  
  }
}

