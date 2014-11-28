// $Id: filterStrangeMuDst.C,v 2.1 2003/02/10 16:50:08 genevb Exp $
// $Log: filterStrangeMuDst.C,v $
// Revision 2.1  2003/02/10 16:50:08  genevb
// simple updates
//
// Revision 2.0  2000/06/09 22:12:56  genevb
// Updated for version 2 of Strangeness mico DST package
//
// Revision 1.2  2000/04/12 16:16:55  genevb
// Remove unnecessary library loads
//
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
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
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
  strangeNewDst.SetWrite("newEvMuDst.root"); //
  strangeNewDst.DoV0();    // Selects V0 vertices for new micro-DST
  strangeNewDst.DoMc();    // Keep MC info if it is available

  StStrangeMuDstMaker strangeOldDst("strangeOldMuDst");
  strangeOldDst.SetRead(); // Sets "read" mode (using default file names)
  // DoV0() and DoMc() are autmatically called for the old maker by the new.

  // Now we tell the new maker that it will create a sub-DST of the old one.
  strangeNewDst.SubDst(&strangeOldDst);

  // Next, any additional cuts that are being made should be added to
  // the cuts information in the new DST.
  strangeNewDst.Cuts().Add("Lambda mass","1.11 < mass < 1.12");
  // strangeNewDst.Cuts().Add("Large Pt total","Pt total > 350 GeV/c");

  clock.Start(kTRUE);

  // Do init
  Int_t ierr = chain.Init();
  if( ierr ) { chain.Fatal(ierr,"on init"); return; }

  // Loop over events
  for( Int_t i=0; i<Nevents; i++ ) {
    if( chain.Make() ) break;

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
