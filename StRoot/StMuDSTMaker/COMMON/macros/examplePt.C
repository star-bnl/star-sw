// NOTE - chain needs to be declared global so for StHbtEventReader
//==========================================================================================
class StMuDstMaker;
StMuDstMaker* maker;

TH1D globalPt("globalPt","globalPt",100,0.,3.);
TH1D primaryPt("primaryPt","primaryPt",100,0.,3.);
TH1D l3Pt("l3Pt","l3Pt",100,0.,3.);

void examplePt() {
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");  // new addition 22jul99
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities"); 
  gSystem->Load("StMcEvent"); 
  gSystem->Load("StMcEventMaker"); 
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StMcAnalysisMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");
 
  cout << " loading done " << endl;
  
  StMuDebug::setLevel(0);  // switch of some debug output

  int counter=0;
  int iret=0;
  StMuTimer timer;
  timer.start();
  maker = new StMuDstMaker(0,0,"MuDST/central/ReversedFullField/runs/","","MuDst",10);   // set up maker in read mode
  cout << "time to load chain: " << timer.elapsedTime() <<endl;
  timer.reset();
  timer.start();
  while (!iret) {
    iret = maker->Make();  // read an event 
    
    StMuDst* mu = maker->muDst();
    int n= mu->globalTracks()->GetEntries();
    for (int l=0; l<n; l++) globalPt.Fill( mu->globalTracks(l)->pt() );   
    int n= mu->primaryTracks()->GetEntries();
    for (int l=0; l<n; l++) primaryPt.Fill( mu->primaryTracks(l)->pt() );   
    int n= mu->l3Tracks()->GetEntries();
    for (int l=0; l<n; l++) l3Pt.Fill( mu->l3Tracks(l)->pt() );   
    counter++;
  }
  cout << endl;
  cout << "time/event " << timer.elapsedTime()/counter <<endl;
  cout << " # of events:" << counter << endl;

  globalPt->Draw();
  primaryPt->Draw("same");
  l3Pt->Draw("same");

}



