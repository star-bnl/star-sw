void testMixer(char *file){

  gSystem->Load("MyEvent/MyEvent.so");
  gSystem->Load("gamma/analysis/lib/AnaCuts.so");
  gSystem->Load("gamma/analysis/lib/EventMixer.so");

  TFile *mFile=new TFile(file,"OPEN");
  TTree *myEventTree;
  myEventTree=(TTree*)mFile->Get("mEventTree");
  MyEvent *ev=new MyEvent();
  myEventTree->SetBranchAddress("branch",&ev);

  EventMixer *mix=new EventMixer("pp05");

  Int_t i=0;
  while(myEventTree->GetEntry(i) && i<10000){

    MyEvent *e_add=new MyEvent(*ev);
    mix->addEvent(e_add);

    i++;
  }

  TCanvas *c=new TCanvas();
  TH2F *h=new TH2F(*(TH2F*)mix->getMinvMB());
  h->Draw("colz");
  c->SaveAs("mix.eps");

}
