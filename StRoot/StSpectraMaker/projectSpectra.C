{

  TFile f("piplus.root");

  TF1 *f1 = new TF1("f1","[0]/(abs([2])*2.516)*(exp(-0.5*((x-[1])/[2])*((x-[1])/[2])))+[3]",-4,4);

  f1->SetParameter(0,1);
  f1->SetParameter(1,0);
  f1->SetParameter(2,1);
  f1->SetParameter(3,0);


  f1->SetParName(0,"area");
  f1->SetParName(1,"mean");
  f1->SetParName(2,"width");
  f1->SetParName(3,"background");

  f.ls();

  c1 = new TCanvas("c1","pion deviant");
  c1.cd();  

//  YPtpiplus->Draw();
//  YPtpiplus->Reset();
 
  PIDDeviantpiplus->Draw();

  int NYbins = YPtDeviantWeightedpiplus->GetNbinsX();
  int NPtbins = YPtDeviantWeightedpiplus->GetNbinsY();
  const int Nbins = NYbins*NPtbins ;
cout << NYbins << " " <<NPtbins << endl; 
  
  Stat_t stats[8];
//  YPtDeviantWeightedpiplus->GetStats(stats);
//  cout << "sum of weights " << stats[0] << endl;

//  double sumFull = YPtDeviantWeightedpiplus->GetSumOfWeights();
//  cout << "sum of weights " << sumFull << endl;

  char hlabel[100]; 

  c2 = new TCanvas("c2","pion deviant projections");
  c2.Divide(3,3);

 

  int plotbin = 0;
  int ibin = 0;
  for (Int_t iYSlice =1 ; iYSlice < NYbins+1; iYSlice++){ 
    int iYFirst = iYSlice ;
    int iYLast  = iYSlice + 1 ;
    for (Int_t iPtSlice =1 ; iPtSlice < NPtbins+1; iPtSlice++){ 
      int iPtFirst = iPtSlice ;
      int iPtLast  = iPtSlice + 1 ;

      double jacobian; 
      double countsInBin = YPtCountspiplus->GetCellContent(iYSlice,iPtSlice);

      if (countsInBin > 0) {
	// this assumes the signal and background have similar 
	// pt distributions so you can get the jcabian from the
	// 2-D spectra
	jacobian = YPtJacobianpiplus->
	           GetCellContent(iYSlice,iPtSlice)/countsInBin;
      } else {
	//
	// should never need this, but would be better to find
	// the center of the pt bin and jacobian = 1./ptcenter
	//
        jacobian = 1.;
      }
      sprintf(hlabel,"weight%iYSlice%iPtSlice",iYSlice,iPtSlice);
      TH1D* weighted= YPtDeviantWeightedpiplus->ProjectionZ(hlabel, iYFirst, iYLast,
					   iPtFirst, iPtLast,"E");
 
      sprintf(hlabel,"counts%iYSlice%iPtSlice",iYSlice,iPtSlice);
      TH1D* counts= YPtDeviantCountspiplus->ProjectionZ(hlabel, iYFirst, iYLast,
					   iPtFirst, iPtLast,"E");
      double sum = counts->GetSumOfWeights();
      cout << "sum of counts " << sum << endl;
      if (sum > 50) {
	// fits more stable
       f1->SetParameter(0,1);
       f1->SetParameter(1,0);
       f1->SetParameter(2,1);
       f1->SetParameter(3,0);

       c2.cd(plotbin);
       plotbin++;
       if (plotbin > 9) plotbin=plotbin-9;
    
       weighted->Fit("f1","s");
       Double_t chi2 = f1->GetChisquare();
       Int_t    ndeg = f1->GetNDF();
       if (ndeg > 0 ){
        cout << " chi2 per ndegf "<< chi2/ndeg << endl;
        if (chi2/ndeg < 2 && f1->GetParameter(1)< 0.2 
	  && fabs(f1->GetParameter(2)) < 1.5 ) {
	  double content = fabs(f1->GetParameter(0)) * jacobian;
	  double err =  f1->GetParError(0) * jacobian ;
	  YPtpiplus->SetCellContent(iYFirst,iPtFirst,content);
	  YPtpiplus->SetCellError(iYFirst,iPtFirst,err);
        }
       }
      cout << "mean: "<< f1->GetParameter(1) << endl;
      cout << "width: "<< f1->GetParameter(2) << endl;
      }
      ibin++;
    }
  }

  c3 = new TCanvas("c3","pt spectra");
  c3.cd();
  YPtpiplus->Draw();
  int iYmid = NYbins/2;
  c3.SetLogy();
  for (Int_t iYSlice = iYmid ; iYSlice < iYmid+1; iYSlice++){
    sprintf(hlabel,"pt%iYSlice",iYSlice);
    int iYFirst = iYSlice ;
    int iYLast  = iYSlice + 1 ;
    TH1D* temp = YPtpiplus->ProjectionY(hlabel, iYFirst, iYLast,"E");
    temp->Draw();
  } 
  
}









