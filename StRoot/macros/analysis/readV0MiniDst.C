// $Id: readV0MiniDst.C,v 1.9 2000/04/12 16:16:55 genevb Exp $
// $Log: readV0MiniDst.C,v $
// Revision 1.9  2000/04/12 16:16:55  genevb
// Remove unnecessary library loads
//
// Revision 1.8  2000/01/05 22:18:07  genevb
// Put comments in order that QA wants
//
// Revision 1.7  1999/09/02 09:12:27  jones
// Changes reflect new I/O file handling
//
// Revision 1.6  1999/08/31 20:49:08  genevb
// StV0MiniDst class now depends on StAnalysisUtilities library
//
// Revision 1.5  1999/08/13 13:36:44  jones
// Modified to reflect new revision of StV0MiniDstMaker
//
// Revision 1.4  1999/08/10 21:26:35  genevb
// Spaces are no longer separaters for collection->Draw()
//
// Revision 1.3  1999/08/03 02:37:54  genevb
// StHFillObject now fills multiple histos simultaneously
//
// Revision 1.2  1999/07/30 15:07:31  genevb
// Take advantage of StHFillObject inheritance
//
// Revision 1.1  1999/07/13 12:51:02  jones
// Added macros for making and reading v0 mini-DSTs using StV0MiniDstMaker
//
//======================================================
// owner:  Peter G. Jones, University of Birmingham, p.g.jones@bham.ac.uk
// what it does:  uses StV0MiniDstMaker to read a V0 mini-DST
//                and draw some histograms from it
//======================================================


int n_v0 = 0;
TOrdCollection *collection = 0;
TStopwatch clock;

void load() {
  gSystem->Load("St_base");
  gSystem->Load("libgen_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libglobal_Tables");
  gSystem->Load("libtpc_Tables");
  gSystem->Load("StChain");
  gSystem->Load("StUtilities");
  gSystem->Load("StAnalysisUtilities");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StV0MiniDstMaker");
  
  // Create and start new stopwatch
  clock = new TStopwatch;
  clock.Start(kTRUE);

  // Open V0 micro-DST
  StV0MiniDstMaker* v0dst = new StV0MiniDstMaker("v0dst");

  v0dst->SetInputFile("V0MicroDst.root");
  v0dst->SetV0VertexType();
  collection = v0dst->Read(&n_v0);

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
  hMassLambda = new TH1F("mMassLambda","Lambda Mass",100,1.08,1.2);

  clock.Start(kTRUE);

  // Loop over collection and fill histograms

  // Example 1: Using Gene Van Buren's StHFillObject class
  TString a1 = "mX:decayVertexV0[0]; mY:decayVertexV0[1]; mZ:decayVertexV0[2];";
  collection->Draw(a1.Data());
  
  // Example 2: Or DIY
  for( Int_t i=0; i<n_v0; i++ ) {
    StV0MiniDst *v0m = (StV0MiniDst *) collection->At(i);
    hMassLambda->Fill(v0m->massLambda());
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
