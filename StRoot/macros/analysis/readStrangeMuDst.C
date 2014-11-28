// $Id: readStrangeMuDst.C,v 2.0 2000/06/09 22:13:18 genevb Exp $
// $Log: readStrangeMuDst.C,v $
// Revision 2.0  2000/06/09 22:13:18  genevb
// Updated for version 2 of Strangeness mico DST package
//
// Revision 1.4  2000/04/12 16:16:55  genevb
// Remove unnecessary library loads
//
// Revision 1.3  2000/04/05 14:09:23  genevb
// Changed member functions again
//
// Revision 1.2  2000/03/29 20:58:58  genevb
// Modified StV0MuDst member functions
//
// Revision 1.1  2000/03/29 00:24:54  genevb
// Introduction of macro to use StStrangeMuDstMaker
//
//
//======================================================
// owner:  Gene Van Buren, UCLA
// what it does:  uses StStrangeMuDstMaker to read a micro DST
//                and draw some histograms from it
//======================================================


TStopwatch clock;

void load() {
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StStrangeMuDstMaker");
  
  // Create new canvas
  TCanvas* c1 =
    new TCanvas("c1","Getting Started with StV0MiniDst",0,0,600,600);

  c1->cd();
  p1 = new TPad("p1","1st Pad",0.01,0.51,0.49,0.99,10,0,0);
  p2 = new TPad("p2","2nd Pad",0.51,0.51,0.99,0.99,10,0,0);
  p3 = new TPad("p3","3rd Pad",0.01,0.01,0.49,0.49,10,0,0);
  p4 = new TPad("p4","4th Pad",0.51,0.01,0.99,0.49,10,0,0);

  c1->cd();
  p1->Draw();
  p2->Draw();
  p3->Draw();
  p4->Draw();
}

void run() {
  // Define some histograms
  hX = new TH1F("mX","X coordinate",100,-50,50);
  hY = new TH1F("mY","Y coordinate",100,-50,50);
  hZ = new TH1F("mZ","Z coordinate",100,-100,100);
  hMassLambda = new TH1F("mMassLambda","Lambda Mass",100,1.08,1.2);

  // Set number of events to analyse
  const Int_t Nevents = 10000;   // go to EOF

  StChain chain("myChain");
  StStrangeMuDstMaker strangeDst("strangeMuDst");
  strangeDst.DoV0();    // Selects V0 vertices for micro-DST
//  strangeDst.DoXi();    // Selects Xi vertices for micro-DST
//  strangeDst.DoMc();    // Reads in Monte Carlo information if available
  strangeDst.SetRead(); // Sets "read" mode (using default file names)

  clock.Start(kTRUE);

  // Do init
  Int_t ierr = chain.Init();
  if( ierr ) { chain.Fatal(ierr,"on init"); return; }

  gMessMgr->Info("Here are the cuts used to create this strangeness DST:");
  strangeDst.Cuts().List();

  // Loop over events
  for( Int_t i=0; i<Nevents; i++ ) {
    if( chain.Make(i) ) break;

    for( Int_t j=0; j<strangeDst.GetNV0(); j++ ) {
      StV0MuDst *v0m = strangeDst.GetV0(j);
      hX->Fill(v0m->decayVertexV0X());
      hY->Fill(v0m->decayVertexV0Y());
      hZ->Fill(v0m->decayVertexV0Z());
      hMassLambda->Fill(v0m->massLambda());
    }

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

  p1->cd();
  hX->Draw();
  p2->cd();
  hY->Draw();
  p3->cd();
  hZ->Draw();
  p4->cd();
  hMassLambda->Draw();
}

void readStrangeMuDst() {
  load();
  run();
}
