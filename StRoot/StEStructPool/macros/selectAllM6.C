void selectAllM6(const char* infile, char* outputdir = "./", int rescaleNev=0){
  // extracts files for cutbin mode 6 (z-vertex binning)
  // reads infile, writes final00.root to final09.root in outputdir

  cout << "  selectAllM6, input: " << infile << "\tout dir: " << outputdir << endl;
  gSystem->Load("StEStructPoolSupport.so");
  StEStructHAdd adder;
  TFile * tf=new TFile(infile);

  if(!tf){
    cout<<"error opening file "<<endl;
    return ;
  };

  int narray[1];
  int num=1;

  
  if(rescaleNev) {
    TH1D* hns = (TH1D*)tf->Get("NEventsSame");
    int nevs = hns->GetEntries();
    TH1D* hnm = (TH1D*)tf->Get("NEventsMixed");
    int nevm = hnm->GetEntries();
    cout << infile << " has " << nevs << " sibling events and " << nevm << " mixed events." << endl;

    TH1D* hcb = (TH1D*)tf->Get("hcb");    
    if(!hcb) { cout << "error loading hcb" << endl; return;}  
    TH1D* hcbs = (TH1D*)hcb->Clone("hcbs");
    TH1D* hcbm = (TH1D*)hcb->Clone("hcbm");
    hcbs->Scale( nevs / hcbs->Integral() );  
    hcbm->Scale( nevm / hcbm->Integral() );  

    cout << "Targets " << endl;
    for(int i=0; i<10; i++) cout << hcbs->GetBinContent(i+1) << "\t" << hcbm->GetBinContent(i+1) << endl;
    cout << "Totals:\t" << hcbs->Integral() << "\t" << hcbm->Integral() << endl;

    /*c1 = new TCanvas("sm6","sm6",600,300);
    c1->Divide(2,1);  
    c1->cd(1);  hcbs->Draw();
    c1->cd(2);  hcbm->Draw();
    */
  }   

  TString fname;
  for(int i=0; i<10; i++) {
    narray[0] = i;
    fname=outputdir; fname+="final0"; fname+=i; fname+=".root";
    cout << "writing " << fname << endl;
    adder.addCuts(fname,tf,narray,num);

    if(rescaleNev) {
      tfout = new TFile(fname,"UPDATE");
      TH1D* hs = (TH1D*)tfout->Get("NEventsSame");
      hs->SetEntries( hcbs->GetBinContent(i+1) );
      hs->Write();
      TH1D* hm = (TH1D*)tfout->Get("NEventsMixed");
      hm->SetEntries( hcbm->GetBinContent(i+1) );
      hm->Write();
      tfout->Close();
    }

  }


};


