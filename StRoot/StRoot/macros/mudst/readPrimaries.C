{{
  /* This is an example macro for reading selected branches from the microdst
   * The aim is to reduce I/O and speed up analysis
   * More specifically, in this example only primary tracks and 
   * the event information are read
   * The increase in reading speed is about a factor 2-3 from a simple test
   */
  gROOT->Macro("loadMuDst.C");

  TStopwatch time_all;
  mudst_mk=new StMuDstMaker(0,0,"/star/data42/reco/production62GeV/ReversedFullField/P04ie/2004/088/","st_physics_5088062_raw_4040003.MuDst.root");

  mudst_mk->SetStatus("*",0);             // First switch off all branches
  mudst_mk->SetStatus("MuEvent",1);       // We need StMuEvent for the chain
  mudst_mk->SetStatus("PrimaryTracks",1); // Only read primary tracks in addition

  mudst_mk->Init();

  TStopwatch time_loop;
  Int_t n_evt=500;
  for (Int_t i_evt=0; i_evt<n_evt; i_evt++) {
    mudst_mk->Make();
    if (mudst_mk->muDst()==0) {
      cout << "No event" << endl;
      continue;
    }
    StMuDst *mudst=mudst_mk->muDst();
    cout << "Event " << mudst->event()->eventId() << ": "
         << mudst->numberOfPrimaryTracks() << " primary tracks, " 
         << mudst->numberOfGlobalTracks() << " global tracks" << endl;
  }
  time_loop.Stop();
  time_all.Stop();
  cout << "Total time "; time_all.Print();
  cout << "loop "; time_loop.Print();
}}
