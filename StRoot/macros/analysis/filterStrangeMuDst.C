// $Id: filterStrangeMuDst.C,v 1.1 2000/04/05 20:27:58 genevb Exp $
// $Log: filterStrangeMuDst.C,v $
// Revision 1.1  2000/04/05 20:27:58  genevb
// Introduction of macro to filter strangeness micro DSTs
//
//
//======================================================
// owner:  Gene Van Buren, UCLA
// what it does:  uses StStrangeMuDstMaker to read a micro DST
//                and filter it to a sub-micro DST
//
//  - This example shows how one might select only those V0s
//    whose mass under the lambda hypothesis falls in a range
//    near that of that lambda.
//  - Another possibility is to select entire events, and the
//    lines commented out during the event loop with totalV0Pt
//    show how one would filter out those events whose total
//    Pt from V0's is greater than 350 GeV/c.
//======================================================


TStopwatch clock;

void load() {
  gSystem->Load("St_base");
  gSystem->Load("St_Tables");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StStrangeMuDstMaker");
}

void run() {

  // Set number of events to analyse
  const Int_t Nevents = 10000;   // go to EOF

  StChain chain("myChain");
  
  // The maker for the new micro DST must be constructed _before_ the 
  // maker to read the old micro DST. This is because the copying is
  // done during chain.Clear(), and the new maker's Clear() must be
  // called to do the copying before the old maker's Clear() is called,
  // erasing the event.
  
  StStrangeMuDstMaker strangeNewDst("strangeNewMuDst");
  strangeNewDst.DoV0();    // Selects V0 vertices for new micro-DST
  strangeNewDst.SetWrite("newEvMuDst.root","newV0MuDst.root"); //

  StStrangeMuDstMaker strangeOldDst("strangeOldMuDst");
  strangeOldDst.DoV0();    // Selects V0 vertices from old micro-DST
  strangeOldDst.SetRead(); // Sets "read" mode (using default file names)

  strangeNewDst.SubDst(&strangeOldDst);

  clock.Start(kTRUE);

  // Do init
  Int_t ierr = chain.Init();
  if( ierr ) { chain.Fatal(ierr,"on init"); return; }

  // Loop over events
  for( Int_t i=0; i<Nevents; i++ ) {
    if( chain.Make(i) ) break;

//    Float_t totalV0Pt = 0.;
    for( Int_t j=0; j<strangeOldDst.GetNV0(); j++ ) {
      StV0MuDst *v0m = strangeOldDst.GetV0(j);
      Float_t ml = v0m->massLambda();
      if ((ml>1.11) && (ml<1.12)) strangeNewDst.SelectV0(j);
//      totalV0Pt += v0m->ptV0();
    }
//    if (totalV0Pt > 350.) strangeNewDst.SelectEvent();

    if( i != Nevents) chain.Clear();
    printf("*** Finished processing event %d\n",i);
  }

  // Finish
  if( Nevents >= 1 ) {
    chain.Finish();
  }

  // Stop the stopwatch
  clock.Stop();
  clock.Print();
}

void filterStrangeMuDst() {
  load();
  run();
}
