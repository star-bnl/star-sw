void ED() {
   StEvent *event = (StEvent*)chain->GetDataSet("StEvent");
   if (! event) return;
   delete gEventDisplay; // destroy the built-in display
   new StuDraw3DEvent(); // create our own one (with no detector geometry)
   gEventDisplay->Clear();
   Int_t hits = 0x1 | 0x2 | 4;
   if (hits & 0x1 ) gEventDisplay->Hits(event,1);
   if (hits & 0x2 ) gEventDisplay->EmcHits(event);
   //   if (hits & 0x4 ) gEventDisplay->FtpcHits(event,1);
   if (hits & 4) gEventDisplay->EmcHits(event,"bemc|eemc");
}
