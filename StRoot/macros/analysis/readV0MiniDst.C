// Using StV0MiniDstMaker to read a v0 mini-DST
// Peter G. Jones, University of Birmingham, p.g.jones@bham.ac.uk
// $Id: readV0MiniDst.C,v 1.3 1999/08/03 02:37:54 genevb Exp $
// $Log: readV0MiniDst.C,v $
// Revision 1.3  1999/08/03 02:37:54  genevb
// StHFillObject now fills multiple histos simultaneously
//
// Revision 1.2  1999/07/30 15:07:31  genevb
// Take advantage of StHFillObject inheritance
//
// Revision 1.1  1999/07/13 12:51:02  jones
// Added macros for making and reading v0 mini-DSTs using StV0MiniDstMaker
//

int n_v0 = 0;
TOrdCollection *collection = 0;
TStopwatch clock;

void load() {
  gSystem->Load("St_base.so");
  gSystem->Load("St_Tables.so");
  gSystem->Load("StChain.so");
  gSystem->Load("StUtilities");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent.so");
  gSystem->Load("StV0MiniDstMaker.so");
  
  // Create and start new stopwatch
  clock = new TStopwatch;
  clock.Start(kTRUE);

  // Open V0 mini-DST
  TFile input("V0MiniDst.root");
  collection = (TOrdCollection *) input.Get("V0MiniDst;1");
  if( collection )
    printf("collection size %d\n",collection->GetSize());
  else
    printf("pointer to collection is zero\n");

  // Loop over collection and precalculate some useful quantities
  collection->ls("Update");

  // Stop the clock
  clock.Stop();
  clock.Print();

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
  hMassLambda = new TH1F("mMassLambda","Lambda Mass",100,1.08,1.12);

  clock.Start(kTRUE);

  // Loop over collection and fill histograms
  TString a1 = "mX:position[0] mY:position[1] mZ:position[2]";
  a1+=" mMassLambda:massLambda";
  collection->Draw(a1.Data());
  

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
