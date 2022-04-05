void combineRatiosView(char* file) {
  // viewer for files created by combineRatios

  TFile * tf = new TFile(file);
  
  const char* plotnames[] = {"DEtaDPhi","YtYt","EtaEta","PhiPhi"};
  const char* types[] = {"LS","US","CD","CI"};

  TH2F* hin[16];

  TString name;
  for(int i=0; i<4; i++) {
    for(int j=0; j<4; j++) {
      name=types[j]; name+=plotnames[i]; 
      hin[i*4+j] = (TH2F*)tf->Get(name);
      if(!hin[i*4+j]) cout << "ERROR with " << name << endl << flush;
    }
  }
  
  const char* drawopts[] = {"surf1","colz","colz","colz"};
  
  c1 = new TCanvas(file,file,900,900);
  c1->Divide(4,4);
  for(i=0; i<16; i++) {
    c1->cd(i+1); 
    hin[i]->Draw(drawopts[i/4]);
  }
  

}



    
