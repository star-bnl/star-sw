void combineRatios(char* dir, char* OutFilename, int num, int startnum = 0) {
  // Author: Michael Daugherity

  // combineRatios uses Support class to create charge-type correlations seperately for several input
  //   files, then combines them as a pair-weighted average.  The output file consists of 16 histograms
  //   with names such as CIDEtaDPhi.  This version does *not* do the necessary chi^2 minimization for YtYt.

  // Calling: reads dir/final0(startnum).root to dir/final0(startnum+num-1).root
  //          example: combineRatios(".","comb.root",10) reads final00.root to final09.root in current directory
  

  const int MAX=45;
  if(num>MAX) {
    cout << "OVERFLOW, increase MAX!!!" << endl;
    return;
  }
    
  TFile* tf[MAX];
  TString filename;

  for(int i=0; i<num; i++) {
    filename=dir; filename+="/"; filename+="final0"; filename+=i+startnum; filename+=".root";
    cout << "Loading "<< filename.Data() << endl;
    tf[i] = new TFile(filename.Data());
  }

  TH2D** plots[MAX];
  StEStructSupport ehelp(tf[0],0);

  tfout = new TFile(OutFilename,"RECREATE");
  const char* plotnames[] = {"DEtaDPhi","YtYt","EtaEta","PhiPhi"};  // Correlations to make
  const int NUMPLOTS = 4;

  TString name;
  double npairs[4][MAX+1];   // [0] = LS, [1] = US, [2] = CI, [3] = CD;
  const char* pairnames[] = {"SibppNDEtaDPhi","SibmmNDEtaDPhi","SibpmNDEtaDPhi","SibmpNDEtaDPhi"};
  double npairsCS[4];  // pair counts for pp,mm,pm,mm
  TH2D* htemp;


  for(int plot=0; plot<NUMPLOTS; plot++) { 
    name=plotnames[plot];
    
    for(i=0; i<num; i++) {  // fill plots[] with correlations
      ehelp.setTFile(tf[i]);
      
      //plots[i] = (TH2D**)ehelp.buildChargeTypeRFunctions(name);  
      plots[i] = (TH2D**)ehelp.buildNChargeTypes(name);  

      if(plot==0) {   // only fill pairs counts once	
	for(int j=0; j<4; j++) {  // get pair counts for norm
	  htemp = (TH2D*)tf[i]->Get(pairnames[j]);
	  if (!htemp) {
	    cout << "ERROR reading " << pairnames[j] << endl;
	    return;
	  }
	  npairsCS[j] = htemp->Integral();
	  //cout << npairsCS[j] << "\t";
	}
	npairs[0][i] = npairsCS[0] + npairsCS[1];  // LS = pp + mm
	npairs[1][i] = npairsCS[2] + npairsCS[3];  // US = pm + mp
	npairs[2][i] = npairs[0][i] + npairs[1][i];// CI = LS + US
	npairs[3][i] = npairs[2][i];  // CD = CI;
	if(i==0) { for(j=0; j<4; j++) npairs[j][MAX] = 0;}
	for(j=0; j<4; j++) npairs[j][MAX] += npairs[j][i];
      }
    }
    
    for (int type=0; type<4; type++) {      
      plots[0][type]->Scale(npairs[type][0]);
      TH2D* hout = (TH2D*)plots[0][type]->Clone();
      for(i=1; i<num; i++) {
	plots[i][type]->Scale(npairs[type][i]);
	hout->Add(plots[i][type]);
      }  
      hout->Scale(1.0/npairs[type][MAX]);
      tfout->cd();
      cout << "Writing " << hout->GetName() << endl;
      hout->Write();
    }
  }
  
  /*
  cout << "Pair Counts: LS, US, CI/CD" << endl;
  for(i=0; i<num; i++) {
    for(j=0; j<4; j++) cout << npairs[j][i] << "\t";
    cout << endl;
  }
  cout << "Totals" << endl;
  for(j=0; j<4; j++) cout << npairs[j][MAX] << "\t";
  cout << endl;
  */

  //tfout->ls();
  tfout->Close();

}



    
