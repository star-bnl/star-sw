{

  TFile f("deviant.root");

  TF1 *f1 = new TF1("f1","[0]/([2]*2.516)*(exp(-0.5*((x-[1])/[2])*((x-[1])/[2])))+[3]",-4,4);

  f1->SetParameter(0,10);
  f1->SetParameter(1,0);
  f1->SetParameter(2,1);
  f1->SetParameter(3,10);


  f1->SetParName(0,"area");
  f1->SetParName(1,"mean");
  f1->SetParName(2,"width");
  f1->SetParName(3,"background");


  f.ls();
  c1 = new TCanvas("c1","pion deviant");
  c1.cd();
  YMtDeviantpiplus->Draw();
  YMtpiplus->Draw();
  YMtpiplus->Reset();
  PIDDeviantpiplus->Draw();
  c2 = new TCanvas("c2","pion dedx");
  c2.cd();
  dedxvsPpiplus->Draw();

  int NYbins = YMtDeviantpiplus->GetNbinsX();
  int NMtbins = YMtDeviantpiplus->GetNbinsY();
  const int Nbins = NYbins*NMtbins ;
  cout << NYbins << NMtbins << endl; 
  
  Stat_t stats[8];
  YMtDeviantpiplus->GetStats(stats);
  cout << "sum of weights " << stats[0] << endl;
  double sumFull = YMtDeviantpiplus->GetSumOfWeights();
  cout << "sum of weights " << sumFull << endl;

  char hlabel[100]; 

  c3 = new TCanvas("c3","pion deviant projections");
  c3.Divide(3,3);
  int plotbin = 0;
  int ibin = 0;
  for (Int_t iYSlice =1 ; iYSlice < NYbins+1; iYSlice++){ 
    int iYFirst = iYSlice ;
    int iYLast  = iYSlice + 1 ;
    for (Int_t iMtSlice =1 ; iMtSlice < NMtbins+1; iMtSlice++){ 
      int iMtFirst = iMtSlice ;
      int iMtLast  = iMtSlice + 1 ;

      sprintf(hlabel,"deviant%iYSlice%iMtSlice",iYSlice,iMtSlice);

      TH1D* temp = YMtDeviantpiplus->ProjectionZ(hlabel, iYFirst, iYLast,
					   iMtFirst, iMtLast,"E"); 

      double sum = temp->GetSumOfWeights();
      cout << "sum of weights " << sum << endl;
      if (sum > 10) {

       c3.cd(plotbin);
       plotbin++;
       if (plotbin > 9) plotbin=plotbin-9;
    
       temp->Fit("f1","s");
       Double_t chi2 = f1->GetChisquare();
       Int_t    ndeg = f1->GetNDF();
       if (ndeg > 0 ){
        cout << " chi2 per ndegf "<< chi2/ndeg << endl;
        if (chi2/ndeg < 2 && f1->GetParameter(1)< 0.2 
	  && fabs(f1->GetParameter(2)) < 1.5 ) {
	  double content = fabs(f1->GetParameter(0));
	  double err =  f1->GetParError(0);
	  YMtpiplus->SetCellContent(iYFirst,iMtFirst,content);
	  YMtpiplus->SetCellError(iYFirst,iMtFirst,err);
        }
       }
      cout << "mean: "<< f1->GetParameter(1) << endl;
      cout << "width: "<< f1->GetParameter(2) << endl;
      }
      ibin++;
    }
  }
  c4 = new TCanvas("c4","mt spectra");
  YMtpiplus->Draw();
  int iYmid = NYbins/2;
  c4.SetLogy();
  for (Int_t iYSlice = iYmid ; iYSlice < iYmid+1; iYSlice++){
    sprintf(hlabel,"mt%iYSlice",iYSlice);
    int iYFirst = iYSlice ;
    int iYLast  = iYSlice + 1 ;
    TH1D* temp = YMtpiplus->ProjectionY(hlabel, iYFirst, iYLast,"E");
    temp->Draw();
  } 
   
}









