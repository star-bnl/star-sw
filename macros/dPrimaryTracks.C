void dPrimaryTracks() {// display primary tracks
  new StuDraw3DEvent("TPC");
  StEvent *event = (StEvent *) chain->GetDataSet("StEvent");
  if (event) {
    gEventDisplay->Tracks(event,primary);
  }
}
