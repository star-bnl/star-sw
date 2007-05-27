void centralityData(const char* infile, int writeAscii=0, int makeGif=0)  {
  
  // Display new centrality histograms, but not much else so far

  /* Files are:
  KEY: TH1F     hNEvent;1       dNevent/dNch - Centrality
  KEY: TH1F     hnt;1   dNevent/dNch - Ntrack
  KEY: TH1F     hvar;1  var bin
  KEY: TH1F     href;1  refMult
  KEY: TH1F     hnumPrim;1      Number of Good Primary Tracks
  KEY: TH1D     hctb;1  CTB Multiplicity
  KEY: TH1F     hnev14;1        centrality ^ 1/4
  KEY: TH1F     hnt14;1 Ntrack ^ 1/4
  KEY: TH1F     href14;1        refMult ^ 1/4
  KEY: TH1F     hnumPrim14;1    Primaries ^ 1/4
  KEY: TH1D     hctb14;1        CTB ^ 1/4
  KEY: TH1F     hmeanpt;1       mean pt
  KEY: TH1F     hmeanpt14;1     mean pt ^ 1/4
  */

  tf = new TFile(infile);

  TH1F* hin[6];
  TH1F* hin14[6];

  const char* names[] = {"hNEvent","hnt","href","hnumPrim","hctb","hmeanpt"};
  const int NUM = 6;
  TString name;
  int i;
  
  for(i=0; i<NUM; i++) {
    name = names[i];
    hin[i] = (TH1F*)tf->Get(name); 
    if(!hin[i]) {
      cout << " ERROR reading " << name << endl;
      tf->ls();
      return;
    }
    
    if (i==0) name = "hnev";
    name+="14";
    hin14[i] = (TH1F*)tf->Get(name);
  }


  name = infile; name += ": centrality";
  c1 = new TCanvas(name, name, 750, 500);
  c1->Divide(3,2);
  for(i=0; i<NUM; i++) {
    c1->cd(i+1);
    if(i!=5) {
      gPad->SetLogx();
      gPad->SetLogy();
    }
    hin[i]->Draw();
  }
  if(makeGif) c1->Print("cent.gif");

  name = infile; name += ": powerlaw";
  c2 = new TCanvas(name, name, 750, 500);
  c2->Divide(3,2);
  for(i=0; i<NUM; i++) {
    c2->cd(i+1);
    hin14[i]->Draw();
  }
  if(makeGif) c2->Print("cent-power.gif");



  if(writeAscii) {

    const char* fnames[] = {"nch","ntrack","refMult","numPrim","ctb","meanpt"};

    for(i=0; i<6; i++) {      
      name = fnames[i];  name+=".txt";
      cout << endl << name << endl;
      cout << "Binning:\t" << hin[i]->GetNbinsX() << " bins\tmin=" << hin[i]->GetXaxis()->GetXmin() <<"\tmax=" << hin[i]->GetXaxis()->GetXmax() << endl;
      ofstream out(name);
      if (i>3) for(int j=1; j<=hin[i]->GetNbinsX(); j++) out << hin[i]->GetBinCenter(j) << "\t" << hin[i]->GetBinContent(j) << endl;
      else for(int j=1; j<=hin[i]->GetNbinsX(); j++) out << (int)hin[i]->GetBinCenter(j) << "\t" << hin[i]->GetBinContent(j) << endl;
      out.close();
      out.clear();
    }
    for(i=0; i<6; i++) {
      name = fnames[i];  name+="14.txt";
      cout << endl << name << endl;
      cout << "Binning:\t" << hin14[i]->GetNbinsX() << " bins\tmin=" << hin14[i]->GetXaxis()->GetXmin() <<"\tmax=" << hin14[i]->GetXaxis()->GetXmax() << endl;
      ofstream out(name);
      for(int j=1; j<=hin14[i]->GetNbinsX(); j++) out << hin14[i]->GetBinCenter(j) << "\t" << hin14[i]->GetBinContent(j) << endl;
      out.close();
      out.clear();
    }





  }


}









