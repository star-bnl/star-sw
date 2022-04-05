void selectAllM7(const char* infile, char* outputdir = "./", int rescale=0){
  // extracts files for cutbin mode 7 (cb2 * z-vertex binning)
  // reads infile, writes final00.root to final09.root in outputdir

  // only does 'all' for now:

  cout << "  selectAllM7, input: " << infile << "\tout dir: " << outputdir << endl;
  gSystem->Load("StEStructPoolSupport.so");
  StEStructHAdd adder;
  TFile * tf=new TFile(infile);

  if(!tf){
    cout<<"error opening file "<<endl;
    return ;
  };

  // 60 cutbins, 0-5 final00, 6-11 final01, ... for 'all'

  int narray[6];    
  int num=6;
  
  
  if(rescale) {
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

    double sib[10];
    double mix[10];
    double sibtot = 0;
    double mixtot = 0;
    
    for(int i=0; i<10; i++) {
      sib[i] = 0; 
      mix[i] = 0; 
      for(int j=0; j<6; j++) {
	sib[i] += hcbs->GetBinContent(i*6 + j + 1);
	mix[i] += hcbm->GetBinContent(i*6 + j + 1);
      }
      sibtot+=sib[i];
      mixtot+=mix[i];
    }
    
    cout << "Targets " << endl;
    for(int i=0; i<10; i++) cout << sib[i] << "\t" <<  mix[i] << endl;
    cout << "Totals:\t" << hcbs->Integral() << "\t" << hcbm->Integral() << endl;
    cout << "Totals:\t" << sibtot << "\t" << mixtot << endl;

    /*c1 = new TCanvas("sm6","sm6",600,300);
    c1->Divide(2,1);  
    c1->cd(1);  hcbs->Draw();
    c1->cd(2);  hcbm->Draw();
    */
  }   
  
  TString fname;
  for(int i=0; i<10; i++) {
    for(int j=0; j<6; j++) narray[j] = i*6 + j;
    fname=outputdir; fname+="final0"; fname+=i; fname+=".root";
    cout << "writing " << fname << endl;
    adder.addCuts(fname,tf,narray,num);
    
    if(rescale) {
      tfout = new TFile(fname,"UPDATE");
      TH1D* hs = (TH1D*)tfout->Get("NEventsSame");
      hs->SetEntries( sib[i] );
      hs->Write();
      TH1D* hm = (TH1D*)tfout->Get("NEventsMixed");
      hm->SetEntries( mix[i] );
      hm->Write();
      tfout->Close();
    }
    
  }
  
  
}

