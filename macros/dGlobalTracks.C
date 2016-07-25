void dGlobalTracks() {// display primary tracks
  new StuDraw3DEvent("TPC");
  StEvent *event = (StEvent *) chain->GetDataSet("StEvent");
  if (event) {
    StSPtrVecTrackNode& trackNode = event->trackNodes();
    UInt_t nTracks = trackNode.size();
    StGlobalTrack* gTrack = 0;
    StTrackNode *node = 0;
    for (UInt_t i=0; i < nTracks; i++) {
      node = trackNode[i]; if (!node) continue;
      gTrack = static_cast<StGlobalTrack*>(node->track(global));
      if (! gTrack) continue;
      if (gTrack->flag() < 0) continue;
      gEventDisplay->Track(*gTrack);
    }
  }
}
