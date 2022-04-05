// ********************************************************
// This code makes the necessary root histos for the 
// background subtraction and systematic uncertainty calculations
// Set charge == 1 for postive charges
// Set charge == -1 for negative charges
// Set charge == 0 for the sum of both
//
// Set two_or_four == 2 for 2 GeV wide bin histograms
// Set two_or_four == 4 for 4 GeV wide bin hisrograms
//
// Set |eta| boundries from 0 to 1 by default (bins are 0.01 wide in eta)
// Set etaLow  == 0   for |eta| = 0
// Set etaHigh == 100 for |eta| = 1
//
// This code uses the W MC sample to set the signal in the normalization 
// region.  It is less sensitive to low statistics fluctuations.
//
// ********************************************************

void makeBkgdFilesForRatio(int charge, int two_or_four, int etaLow=0, int etaHigh=100) {

  // ******************************************************
  // Read in the data set and all the histograms needed 
  // to do the actual calculation
  // ******************************************************

  string dataDir="/star/data01/pwg/stevens4/wAnalysis/xSecPaper/sl11b/data/";
  string dataName="run9setABCD.wana.hist.root";
  
  gStyle->SetOptDate(0);
  TFile *f1 = new TFile(Form("%s%s",dataDir,dataName));
  
  // get the signal w/ and w/o the EEMC in the algo
  if (charge == 1) {
    TH1F *signal = (TH1F*)((TH2F*)f1->Get("pos_muclustpTbal_wE_etaBin2"))->ProjectionY("signal_py",etaLow,etaHigh);
    TH1F *signal_wo_eemc = (TH1F*)((TH2F*)f1->Get("pos_muclustpTbal_noE_etaBin2"))->ProjectionY("signal_no_eemc_py",etaLow,etaHigh);
  } else if (charge == -1) {
    TH1F *signal = (TH1F*)((TH2F*)f1->Get("neg_muclustpTbal_wE_etaBin2"))->ProjectionY("signal_py",etaLow,etaHigh);
    TH1F *signal_wo_eemc = (TH1F*)((TH2F*)f1->Get("neg_muclustpTbal_noE_etaBin2"))->ProjectionY("signal_no_eemc_py",etaLow,etaHigh);
  } else if (charge == 0) { //not eta dependent yet
    TH1F *signal = (TH1F*)f1->Get("muclustPtBal");
    TH1F *signal_wo_eemc = (TH1F*)f1->Get("muclustPtBalnoE");
  }

  // ******************************************************
  // Read in all the MC data sets for background 
  // subtractions and studies
  // ******************************************************

  TFile *MC_fs[6];
  string mcDir="/star/u/stevens4/wAnalysis/efficXsec/outEmb/gainUp2/";
  string tpcHV="";
  //string tpcHV="HighTpcHV";

  MC_fs[0] = new TFile(Form("%sWplus%s.wana.hist.root",mcDir,tpcHV)); // W+ -> e++nu
  MC_fs[1] = new TFile(Form("%sWminus%s.wana.hist.root",mcDir,tpcHV)); // W- -> e-+nu
  MC_fs[2] = new TFile(Form("%sWtau%s.wana.hist.root",mcDir,tpcHV)); // W -> tau+nu
  MC_fs[3] = new TFile(Form("%sZany%s.wana.hist.root",mcDir,tpcHV)); // Z -> any
  MC_fs[4] = new TFile(Form("%sZe+e-Interf%s.wana.hist.root",mcDir,tpcHV)); // Z/gamma* -> e+ e-

  //get eta dependent signal and background from MC
  TH1F *MC_dists_raw[5][3];
  for (int i=0; i<5; i++) {
    if (charge == 1) {
      MC_dists_raw[i][0] = (TH1F*)((TH2F*)MC_fs[i]->Get("pos_muclustpTbal_wE_etaBin2"))->ProjectionY(Form("%d_0_py",i),etaLow,etaHigh);
      MC_dists_raw[i][1] = (TH1F*)((TH2F*)MC_fs[i]->Get("pos_muclustpTbal_noE_etaBin2"))->ProjectionY(Form("%d_1_py",i),etaLow,etaHigh);
      MC_dists_raw[i][2] = (TH1F*)((TH2F*)MC_fs[i]->Get("pos_muclustpTbal_back_etaBin2"))->ProjectionY(Form("%d_2_py",i),etaLow,etaHigh);
      //cout<<i<<" "<<MC_dists_raw[i][0]->Integral()<<endl;
    } else if (charge == -1) {
      MC_dists_raw[i][0] = (TH1F*)((TH2F*)MC_fs[i]->Get("neg_muclustpTbal_wE_etaBin2"))->ProjectionY(Form("%d_0_py",i),etaLow,etaHigh);
      MC_dists_raw[i][1] = (TH1F*)((TH2F*)MC_fs[i]->Get("neg_muclustpTbal_noE_etaBin2"))->ProjectionY(Form("%d_1_py",i),etaLow,etaHigh);
      MC_dists_raw[i][2] = (TH1F*)((TH2F*)MC_fs[i]->Get("neg_muclustpTbal_back_etaBin2"))->ProjectionY(Form("%d_2_py",i),etaLow,etaHigh);
    } else if (charge == 0) { //not eta dependent yet
      MC_dists_raw[i][0] = (TH1F*)MC_fs[i]->Get("muclustPtBal");
      MC_dists_raw[i][1] = (TH1F*)MC_fs[i]->Get("muclustPtBalnoE");
      MC_dists_raw[i][2] = (TH1F*)MC_fs[i]->Get("muclustPtBal_bckgrd");
    }
  }
  
  //pure lumi from pythia to scale MC
  float lumi[5] = {128.6,385.0,96.2,53.1,531.9};

  //scale MC to match lumi of the dataset
  float lumi_fact[6];
  for (int i=0; i<5; i++) {lumi_fact[i] = 13.18/lumi[i];}
  for (int i=0; i<5; i++) {
    for (int j=0; j<3; j++) {
      MC_dists_raw[i][j]->Scale(lumi_fact[i]);
      //cout<<MC_dists_raw[i][j]->Integral()<<endl;
    }
  }

  // Repack the MC histograms to mesh with the odd staggered binning that
  // is being used
  char str[200];
  TH1F *MC_dists_repack[5][3];
  for (int i=0; i<5; i++) {
    sprintf(str,"mcclustPtBal_%d",i);
    MC_dists_repack[i][0] = new TH1F(str,str,49,1.,99.);
    sprintf(str,"mcclustPtBalnoE_%d",i);
    MC_dists_repack[i][1] = new TH1F(str,str,49,1.,99.);
    sprintf(str,"mcclustPtBal_bckgrd_%d",i);
    MC_dists_repack[i][2] = new TH1F(str,str,49,1.,99.);
  }

  for (int i=0; i<5; i++) {
    for (int j=0; j<3; j++) {
      for (int k=1; k<=49; k++) {
        MC_dists_repack[i][j]->SetBinContent(k,MC_dists_raw[i][j]->GetBinContent(2*k)+MC_dists_raw[i][j]->GetBinContent(2*k+1));
      }
    }
  }

  // Make the EEMC background the real one of interest now
  for (int i=0; i<5; i++) {
    MC_dists_repack[i][1]->Add(MC_dists_repack[i][0],-1);
    MC_dists_raw[i][1]->Add(MC_dists_raw[i][0],-1.);
  }

  // **********************************************
  // Do some other stuff
  // **********************************************

  TH1F *eemc_bkgd = signal_wo_eemc->Clone();
  eemc_bkgd->Add(signal,-1.); //this is the '2nd endcap' bkgd
  eemc_bkgd->Add(MC_dists_raw[4][1],-1.);//remove known Z from '2nd endcap' bkgd

  TH1F *signal_final = signal->Clone();
  signal_final->Add(eemc_bkgd,-1); 

  //all histos rebinned by to match binning starting at 25 GeV
  TH1F *eemc_bkgd2 = new TH1F("eemc_bkgd2","eemc_bkgd2",49,1,99);
  TH1F *zsig_bkgd2 = new TH1F("zsig_bkgd2","zsig_bkgd2",49,1,99);
  TH1F *zeemc_bkgd2 = new TH1F("zeemc_bgkd2","zeemc_bkgd2",49,1,99);
  TH1F *zback_bkgd2 = new TH1F("zback_bkgd2","zback_bkgd2",49,1,99);

  TH1F *zanysig_bkgd2 = new TH1F("zanysig_bkgd2","zanysig_bkgd2",49,1,99);
  TH1F *zanyeemc_bkgd2 = new TH1F("zanyeemc_bgkd2","zanyeemc_bkgd2",49,1,99);
  TH1F *zanyback_bkgd2 = new TH1F("zanyback_bkgd2","zanyback_bkgd2",49,1,99);

  TH1F *zsig = MC_dists_raw[4][0]->Clone();
  TH1F *zeemc = MC_dists_raw[4][1]->Clone();
  TH1F *zback = MC_dists_raw[4][2]->Clone();

  //subtract off MC Z contribution to signal
  signal_final->Add(zsig,-1.);

  // First get the "nominal" QCD background shape (charge summed)
  TH1F *bkgd_shape1 = (TH1F*)((TH2F*)f1->Get("pos_muclustpTbal_back_etaBin2"))->ProjectionY("back1_py",etaLow,etaHigh);
  bkgd_shape1->Add((TH1F*)((TH2F*)f1->Get("neg_muclustpTbal_back_etaBin2"))->ProjectionY("back2_py",etaLow,etaHigh));

  //rebin background shape
  TH1F *bkgd_shape_nom = new TH1F("bkgd_shape","bkgd_shape",49,1,99);
  for (int i=1; i<=49; i++) {
    bkgd_shape_nom->SetBinContent(i,bkgd_shape1->GetBinContent(2*i)+
                                    bkgd_shape1->GetBinContent(2*i+1));
  }
  TH1F *bkgd_shape_nom2 = bkgd_shape_nom->Clone();

  TH1F *signal_final2 = new TH1F("signal_final2","signal_final2",49,1,99);
  signal_final2->SetLineColor(2);
  signal_final2->SetLineWidth(2.*signal_final2->GetLineWidth());
  TH1F *signal2 = new TH1F("signal2","signal2",49,1,99);
  for (int i=1; i<=49; i++) { //repack all distributions
    signal_final2->SetBinContent(i,signal_final->GetBinContent(2*i)+
                                   signal_final->GetBinContent(2*i+1));
    signal2->SetBinContent(i,signal->GetBinContent(2*i)+
                             signal->GetBinContent(2*i+1));
    eemc_bkgd2->SetBinContent(i,eemc_bkgd->GetBinContent(2*i)+
                                eemc_bkgd->GetBinContent(2*i+1));
    zsig_bkgd2->SetBinContent(i,zsig->GetBinContent(2*i)+
                                zsig->GetBinContent(2*i+1));
    zeemc_bkgd2->SetBinContent(i,zeemc->GetBinContent(2*i)+
                                 zeemc->GetBinContent(2*i+1));
    zback_bkgd2->SetBinContent(i,zback->GetBinContent(2*i)+
                                 zback->GetBinContent(2*i+1));
    zanysig_bkgd2->SetBinContent(i,MC_dists_raw[3][0]->GetBinContent(2*i)+
                                   MC_dists_raw[3][0]->GetBinContent(2*i+1));
    zanyeemc_bkgd2->SetBinContent(i,MC_dists_raw[3][1]->GetBinContent(2*i)+
                                    MC_dists_raw[3][1]->GetBinContent(2*i+1));
    zanyback_bkgd2->SetBinContent(i,MC_dists_raw[3][2]->GetBinContent(2*i)+
                                    MC_dists_raw[3][2]->GetBinContent(2*i+1));
  }

  TCanvas *can2 = new TCanvas("can2","can2",0,0,600,400);
  signal2->Draw();
  signal_final2->Draw("same");
  //can2->Print("signal_w_eemc.eps");
  //can2->Print("signal_w_eemc.png");


  // **********************************************
  // plot all signals and backgrounds from data and simu
  // **********************************************
  eemc_bkgd->SetLineColor(8);
  eemc_bkgd->SetLineWidth(2.*eemc_bkgd->GetLineWidth());
  bkgd_shape_nom2->SetLineColor(4);
  bkgd_shape_nom2->SetLineWidth(2.*bkgd_shape_nom2->GetLineWidth());

  TCanvas *can8 = new TCanvas("can8","can8",0,0,3000,1800);
  can8->Divide(5,3);
  for (int i=0; i<5; i++) {
    for (int j=0; j<3; j++) {
      can8->cd(5*(j)+i+1);
      //cout << 5*(j)+i+1 << endl;
      gPad->SetGridx(0);
      gPad->SetGridy(0);
      if (j == 0) {
        signal_final2->Draw();
      } else if (j == 1) {
        gPad->SetLogy();
        eemc_bkgd->Draw();
      } else if (j == 2) {
        gPad->SetLogy();
        bkgd_shape_nom2->Draw();
      }
      MC_dists_repack[i][j]->Draw("same");
    }
  }
  //can8->Print("phys_bkgds.eps");
  //can8->Print("phys_bkgds.png");

  for (int i=0; i<5; i++) {
    float signal_sum = 0.;
    float eemc_sum = 0.;
    float back_sum = 0.;
    for (int j=13; j<=49; j++) {
      signal_sum += MC_dists_repack[i][0]->GetBinContent(j);
      eemc_sum += MC_dists_repack[i][1]->GetBinContent(j);
      back_sum += MC_dists_repack[i][2]->GetBinContent(j);
    }
  }


  float signal_in_normMC[3];
  if (charge == 1) {
    signal_in_normMC[0]=MC_dists_repack[0][0]->Integral(8,8);
    signal_in_normMC[1]=MC_dists_repack[0][0]->Integral(9,9);
    signal_in_normMC[2]=MC_dists_repack[0][0]->Integral(10,10);
  }
  if (charge == -1) {
    signal_in_normMC[0]=MC_dists_repack[1][0]->Integral(8,8);
    signal_in_normMC[1]=MC_dists_repack[1][0]->Integral(9,9);
    signal_in_normMC[2]=MC_dists_repack[1][0]->Integral(10,10);
  }


  //Tau needs to be scale due to polarized tau decay (note from Carl)
  float taufrac=1.5;
  // subtract off the tau background
  signal_final2->Add(MC_dists_repack[2][0],-1.*taufrac);
  
  // ******************************************************
  // Do the iterative normalization of the W signal to
  // to the background for the nominal shape
  // ****************************************************** 

  // Now lets try a new background function
  // Fit the peak with a line from 23<ET<39
  TF1 *func1 = new TF1("func1","[0]+[1]*x",23,39);
  func1->SetParameter(0,0.);
  func1->SetParameter(1,0.);

  TCanvas *can4 = new TCanvas("can4","can4",0,0,600,400);
  signal_final2->Draw();

  // Calculate the background for the nominal signal
  // by doing the 20 iterations over the signal peak 
  float signal_in_norm[50];
  TH1F *bkgd_shape_unnorm[20];
  TH1F *signal_for_new[20];
  for (int i=0; i<20; i++) {
    bkgd_shape_unnorm[i] = (TH1F*)bkgd_shape_nom->Clone();
    signal_for_new[i] = (TH1F*)signal_final2->Clone();

    // subtract off the remaining Z signal from the background
    bkgd_shape_unnorm[i]->Add(zback_bkgd2,-1.);
    
    // calculate the W signal in the normalization bins
    signal_in_norm[8] = func1->Integral(15,17);
    signal_in_norm[9] = func1->Integral(17,19);
    signal_in_norm[10] = func1->Integral(19,21);
    //cout << "Integrals = " << signal_in_norm[8] << " " << signal_in_norm[9] << " " << signal_in_norm[10] << endl;
    for (int j=8; j<=10; j++) {
      if (signal_in_norm[j] < 0) {signal_in_norm[j] = 0.;}
    }

    //use MC for signal in normalization window
    signal_in_norm[8]=signal_in_normMC[0];
    signal_in_norm[9]=signal_in_normMC[1];
    signal_in_norm[10]=signal_in_normMC[2];

    // calculate the normalization factor using the 
    // signal subtraction normalization bins
    float normt = 0, normb = 0.;
    for (int k=8; k<10; k++) {
      if (bkgd_shape_unnorm[i]->GetBinContent(k) > 0) {
        normt += signal_final2->GetBinContent(k)-signal_in_norm[k];
        normb += bkgd_shape_unnorm[i]->GetBinContent(k);
      }
    }
    if (normb > 0 && normt > 0) {
      float norm = normt/normb;
      bkgd_shape_unnorm[i]->Scale(norm);
      bkgd_shape_unnorm[i]->Draw("same");
      //cout << "norm " << i << " = " << norm << endl;
    }
    else if(normb > 0 && normt < 0){
      float norm = 0;
      bkgd_shape_unnorm[i]->Scale(norm);
      bkgd_shape_unnorm[i]->Draw("same");
    }
  
    // With the new signal estimate, calculate the normalization
    // factor 
    for (int j=1; j<=49; j++) {
      if (bkgd_shape_unnorm[i]->GetBinContent(j) < 0) {bkgd_shape_unnorm[i]->SetBinContent(j,0.);}
    }
    signal_for_new[i]->Add(bkgd_shape_unnorm[i],-1.); 
    signal_for_new[i]->Fit(func1,"RQ");
  }
  //can4->Print("new_norm.eps");
  //can4->Print("new_norm.png");

  TH1F *signal_in_norm_region = new TH1F("signal_in_norm_region","signal_in_norm_region",49,1.,99.);
  
  signal_in_norm_region->SetBinContent(8,signal_in_norm[8]);
  signal_in_norm_region->SetBinContent(9,signal_in_norm[9]); 
 
  TH1F *new_bkgd = new TH1F("new_bkgd","new_bkgd",49,1.,99.);
  new_bkgd = (TH1F*)bkgd_shape_unnorm[19]->Clone();
  new_bkgd->SetName("new_bkgd");

  TCanvas *can5 = new TCanvas("can5","can5",0,0,600,400);
  signal_final2->Draw();
  new_bkgd->Draw("same");
  signal_in_norm_region->Draw("same");
  //can5->Print("signal_new.eps"); 
  //can5->Print("signal_new.png");

  // ******************************************************
  // Calculate all the 60 shapes for the background
  // systematic shape study
  // ******************************************************

  // get the various background shapes made by
  // using different parameters for sPtBal cut
  TH1F *bkgd_hists_from_file[21];
  TH1F *bkgd_hists_from_file2[21];
  char str[200];
  for (int i=0; i<=20; i++) {

    //use projections from eta dependent histos
    sprintf(str,"pos_failsPtBal_sPtBal_bin_%d_py",i);
    bkgd_hists_from_file[i] = (TH1F*)((TH2F*)f1->Get(Form("pos_failsPtBal_sPtBal_bin_%d_etaBin2",i)))->ProjectionY(str,etaLow,etaHigh);
    sprintf(str,"neg_failsPtBal_sPtBal_bin_%d_py",i);
    bkgd_hists_from_file2[i] = (TH1F*)((TH2F*)f1->Get(Form("neg_failsPtBal_sPtBal_bin_%d_etaBin2",i)))->ProjectionY(str,etaLow,etaHigh);
    bkgd_hists_from_file[i]->Add(bkgd_hists_from_file2[i]);
    
  }

  // Now do the rebinning
  TH1F *bkgd_hists1[21];
  TH1F *bkgd_hists2[21];
  TH1F *bkgd_hists3[21];
  for (int i=0; i<=20; i++) {
    sprintf(str,"bkgd_hist1_%d",i);
    bkgd_hists1[i] = new TH1F(str,str,49,1,99);
    sprintf(str,"bkgd_hist2_%d",i);
    bkgd_hists2[i] = new TH1F(str,str,49,1,99);
    sprintf(str,"bkgd_hist3_%d",i);
    bkgd_hists3[i] = new TH1F(str,str,49,1,99);
    for (int k=1; k<=49; k++) {
      bkgd_hists1[i]->SetBinContent(k,bkgd_hists_from_file[i]->GetBinContent(2*k)+bkgd_hists_from_file[i]->GetBinContent(2*k+1));
      bkgd_hists2[i]->SetBinContent(k,bkgd_hists_from_file[i]->GetBinContent(2*k)+bkgd_hists_from_file[i]->GetBinContent(2*k+1));
      bkgd_hists3[i]->SetBinContent(k,bkgd_hists_from_file[i]->GetBinContent(2*k)+bkgd_hists_from_file[i]->GetBinContent(2*k+1));
    }
  }

  // initiaize the iterative fit functions
  TF1 *func2 = new TF1("func2","[0]+[1]*x",23,39);
  func2->SetParameter(0,0.);
  func2->SetParameter(1,0.);
  TF1 *func3 = new TF1("func3","[0]+[1]*x",23,39);
  func3->SetParameter(0,0.);
  func3->SetParameter(1,0.);

  // Now loop over the the 60 possibilities (20 loops and 3 normalization regions)
  float final_sig_in_norm[21][3];
  float final_chisquare[21];
  float signal_in_norm1[50];
  float signal_in_norm2[50];
  float signal_in_norm3[50];
  TH1F *new_bkgd_hists1[21];
  TH1F *new_bkgd_hists2[21];
  TH1F *new_bkgd_hists3[21];
  TH1F *bkgd_shape_unnorm1[20];
  TH1F *bkgd_shape_unnorm2[20];
  TH1F *bkgd_shape_unnorm3[20];
  TH1F *signal_for_new1[20];
  TH1F *signal_for_new2[20];
  TH1F *signal_for_new3[20];
  for (int i=0; i<=20; i++) { //loop over possible sPtBalance cuts

    func1->SetParameter(0,0.);
    func1->SetParameter(1,0.);
    func2->SetParameter(0,0.);
    func2->SetParameter(1,0.);
    func3->SetParameter(0,0.);
    func3->SetParameter(1,0.);
    
    for (int l=0; l<20; l++) { //loop over 20 iterations
      bkgd_shape_unnorm1[l] = (TH1F*)bkgd_hists1[i]->Clone();
      bkgd_shape_unnorm2[l] = (TH1F*)bkgd_hists2[i]->Clone();
      bkgd_shape_unnorm3[l] = (TH1F*)bkgd_hists3[i]->Clone();
      signal_for_new1[l] = (TH1F*)signal_final2->Clone();
      signal_for_new2[l] = (TH1F*)signal_final2->Clone();
      signal_for_new3[l] = (TH1F*)signal_final2->Clone();
      
      // subtract off the Z contamination
      bkgd_shape_unnorm1[l]->Add(zback_bkgd2,-1.);
      bkgd_shape_unnorm2[l]->Add(zback_bkgd2,-1.);
      bkgd_shape_unnorm3[l]->Add(zback_bkgd2,-1.);
      
      // calculate the signal in the normalization bins
      signal_in_norm1[8] = func1->Integral(15,17);
      signal_in_norm1[9] = func1->Integral(17,19);
      signal_in_norm1[10] = func1->Integral(19,21);
      signal_in_norm2[8] = func2->Integral(15,17);
      signal_in_norm2[9] = func2->Integral(17,19);
      signal_in_norm2[10] = func2->Integral(19,21);
      signal_in_norm3[8] = func3->Integral(15,17);
      signal_in_norm3[9] = func3->Integral(17,19);
      signal_in_norm3[10] = func3->Integral(19,21);

      for (int m=8; m<=10; m++) {
	if (signal_in_norm1[m] < 0) {signal_in_norm1[m] = 0.;}
	if (signal_in_norm2[m] < 0) {signal_in_norm2[m] = 0.;}
	if (signal_in_norm3[m] < 0) {signal_in_norm3[m] = 0.;}
      }
   
      //use MC for signal in normalization window
      signal_in_norm1[8]=signal_in_normMC[0];
      signal_in_norm1[9]=signal_in_normMC[1];
      signal_in_norm1[10]=signal_in_normMC[2];
      signal_in_norm2[8]=signal_in_normMC[0];
      signal_in_norm2[9]=signal_in_normMC[1];
      signal_in_norm2[10]=signal_in_normMC[2];
      signal_in_norm3[8]=signal_in_normMC[0];
      signal_in_norm3[9]=signal_in_normMC[1];
      signal_in_norm3[10]=signal_in_normMC[2];

      // calculate the normalization factor for 1 bin
      float normt = 0, normb = 0.;
      for (int k=8; k<=8; k++) {
	if (bkgd_shape_unnorm1[l]->GetBinContent(k) > 0) {
	  normt += signal_final2->GetBinContent(k)-signal_in_norm1[k];
	  normb += bkgd_shape_unnorm1[l]->GetBinContent(k);
	}
      }
      if (normb > 0 && normt > 0) {
	float norm = normt/normb;
	bkgd_shape_unnorm1[l]->Scale(norm);
      }
      else if(normb > 0 && normt < 0){
	float norm = 0;
	bkgd_shape_unnorm1[l]->Scale(norm);
      }

      for (int m=1; m<=49; m++) {
	if (bkgd_shape_unnorm1[l]->GetBinContent(m) < 0) {bkgd_shape_unnorm1[l]->SetBinContent(m,0.);}
      }
      signal_for_new1[l]->Add(bkgd_shape_unnorm1[l],-1.);
      signal_for_new1[l]->Fit(func1,"RQ");
      
      // calculate the normalization factor for 2 bins
      normt = 0.; normb = 0.;
      for (int k=8; k<=9; k++) {
	if (bkgd_shape_unnorm2[l]->GetBinContent(k) > 0) {
	  normt += signal_final2->GetBinContent(k)-signal_in_norm2[k];
	  normb += bkgd_shape_unnorm2[l]->GetBinContent(k);
	}
      }
      if (normb > 0 && normt > 0) {
	float norm = normt/normb;
	bkgd_shape_unnorm2[l]->Scale(norm);
	//cout << "norm " << i << " " << l << " = " << norm << endl;
      }
      else if(normb > 0 && normt < 0){
	float norm = 0;
	bkgd_shape_unnorm2[l]->Scale(norm);
      }
      
      for (int m=1; m<=49; m++) {
	if (bkgd_shape_unnorm2[l]->GetBinContent(m) < 0) {bkgd_shape_unnorm2[l]->SetBinContent(m,0.);}
      }
      signal_for_new2[l]->Add(bkgd_shape_unnorm2[l],-1.);
      signal_for_new2[l]->Fit(func2,"RQ");
      
      // calculate the normalization factor for 3 bins
      normt = 0.; normb = 0.;
      for (int k=8; k<=10; k++) {
	if (bkgd_shape_unnorm3[l]->GetBinContent(k) > 0) {
	  normt += signal_final2->GetBinContent(k)-signal_in_norm3[k];
	  normb += bkgd_shape_unnorm3[l]->GetBinContent(k);
	}
      }
      if (normb > 0 && normt > 0) {
	float norm = normt/normb;
	bkgd_shape_unnorm3[l]->Scale(norm);
      }
      else if(normb > 0 && normt < 0){
	float norm = 0;
	bkgd_shape_unnorm3[l]->Scale(norm);
      }

      for (int m=1; m<=49; m++) {
	if (bkgd_shape_unnorm3[l]->GetBinContent(m) < 0) {bkgd_shape_unnorm3[l]->SetBinContent(m,0.);}
      }
      signal_for_new3[l]->Add(bkgd_shape_unnorm3[l],-1.);
      signal_for_new3[l]->Fit(func3,"RQ");
    } // end of for loop over l
    
    // Save the last iteration as the background histogram
    new_bkgd_hists1[i] = (TH1F*)bkgd_shape_unnorm1[19]->Clone();
    new_bkgd_hists2[i] = (TH1F*)bkgd_shape_unnorm2[19]->Clone();
    new_bkgd_hists3[i] = (TH1F*)bkgd_shape_unnorm3[19]->Clone();
    
  }

  // plot all the 60 histograms
  gStyle->SetTitleBorderSize(0);
  TCanvas *can6 = new TCanvas("can6","can6",0,0,600,400);
  signal_final2->SetStats(kFALSE);
  if (charge == 1) {
    signal_final2->SetTitle("W+ Background Shapes");
  } else if (charge == -1) {
    signal_final2->SetTitle("W- Background Shapes");
  }
  signal_final2->GetXaxis()->SetRangeUser(0.,70.);
  signal_final2->GetXaxis()->SetTitle("2x2 Cluster E_{T} (GeV)");
  signal_final2->Draw();
  for (int i=0; i<=20; i++) {
    new_bkgd_hists1[i]->Draw("same");
    new_bkgd_hists2[i]->Draw("same");
    new_bkgd_hists3[i]->Draw("same");
  }
  new_bkgd->SetLineColor(4);
  //new_bkgd->SetLineWidth(4.*new_bkgd->GetLineWidth());
  new_bkgd->Draw("same");
  if (charge == 1) {
    //can6->Print("Wplus_bkgd_shapes.eps");
    //can6->Print("Wplus_bkgd_shapes.png");
  } else if (charge == -1) {
    //can6->Print("Wminus_bkgd_shapes.eps");
    //can6->Print("Wminus_bkgd_shapes.png");
  }
 
  TH1F *chi2s = new TH1F("chi2s","chi2s",50,0.,10.);
  for (int i=0; i<=20; i++) {
    chi2s->Fill(final_chisquare[i]);
  }
 
  TCanvas *can7 = new TCanvas("can7","can7",0,0,600,400);
  chi2s->Draw();
  //can7->Print("chi2s.eps"); 
  //can7->Print("chi2s.png");

  // ************************************************
  // Now calculate all background numbers and their 
  // systematic uncertainties (and spit them out to histograms)
  // ************************************************

  // First get the simple numbers (backgrounds and their statistical uncertainties)
  TH1F *tauhist = MC_dists_repack[2][0]->Clone();
  tauhist->Scale(taufrac);
  float tau_norm = lumi_fact[2];
  float Z_norm = lumi_fact[4];
  float bkgd_sum = 0.;
  float signal_sum = 0.;
  float raw_sum = 0.;
  float QCD_sum = 0., tau_sum = 0., eemc_sum = 0.;
  float QCD_raw_sum = 0.;
  float Zany_bkgd_sum = 0.;
  float Zany_eemc_sum = 0.;
  float zsig_sum = 0., zeemc_sum = 0.,zback_sum = 0.;
  float zanysig_sum = 0., zanyeemc_sum = 0.,zanyback_sum = 0.;
  for (int i=13; i<=49; i++) { //get counts in ET>25 for diff contrib.
    bkgd_sum += new_bkgd->GetBinContent(i);
    bkgd_sum += tauhist->GetBinContent(i);
    bkgd_sum += eemc_bkgd2->GetBinContent(i);
    QCD_raw_sum += bkgd_shape_nom->GetBinContent(i);
    QCD_sum += new_bkgd->GetBinContent(i);
    tau_sum += tauhist->GetBinContent(i);
    eemc_sum += eemc_bkgd2->GetBinContent(i);
    signal_sum += signal_final2->GetBinContent(i);
    raw_sum += signal2->GetBinContent(i);
    zsig_sum += zsig_bkgd2->GetBinContent(i);
    zeemc_sum += zeemc_bkgd2->GetBinContent(i);
    zback_sum += zback_bkgd2->GetBinContent(i);
    zanysig_sum += zanysig_bkgd2->GetBinContent(i);
    zanyeemc_sum += zanyeemc_bkgd2->GetBinContent(i);
    zanyback_sum += zanyback_bkgd2->GetBinContent(i);
  }
  cout << "The total background for ET>25 is " << bkgd_sum+zsig_sum << endl;
  cout << "QCD = " << QCD_sum << ", tau = " << tau_sum << ", eemc = " << eemc_sum << ", and Z = " << zsig_sum << endl;
  cout << "Raw = " << raw_sum << endl;
  cout << "W Signal (w/o tau) = " << signal_sum-QCD_sum << endl;
  cout << "Z in sig = " << zsig_sum << endl;
  cout << "Z in eemc = " << zeemc_sum << endl;
  cout << "Z in back = " << zback_sum << endl;
  cout << "Zany in sig = " << zanysig_sum << endl;
  cout << "Zany in eemc = " << zanyeemc_sum << endl;
  cout << "Zany in back = " << zanyback_sum << endl;
  cout << "QCD raw in back = " << QCD_raw_sum << endl; 
  cout << "The QCD stat unc. is " << sqrt(norm*QCD_sum) << endl;
  float tau_stat = tau_norm*taufrac*sqrt(tau_sum/(tau_norm*taufrac));
  float tau_syst = 0.13*tau_sum;
  cout << "The tau stat unc. is " << tau_stat << " syst is " << tau_syst << " and total is " << sqrt(tau_stat*tau_stat + tau_syst*tau_syst) << endl;
  float eemc_stat = sqrt(eemc_sum);
  float eemc_syst = 0.13*zeemc_sum;
  cout << "The eemc stat unc. is " << eemc_stat << " syst is " << eemc_syst << " and total is " << sqrt(eemc_stat*eemc_stat + eemc_syst*eemc_syst) << endl;
  float Z_stat = Z_norm*sqrt(zsig_sum/Z_norm);
  float Z_syst = 0.13*zsig_sum;
  cout << "The Z stat unc. is " << Z_stat << " syst is " << Z_syst << " and total is " << sqrt(Z_stat*Z_stat + Z_syst*Z_syst) << endl;
    
  //for use with asymmetry analysis
  //cout << "f_tau = " << tau_sum/raw_sum << endl;
  //cout << "f_QCD = " << QCD_sum/raw_sum << endl;
  //cout << "f_EEMC = " << eemc_sum/raw_sum << endl;
  //cout << "f_Z = " << zsig_sum/raw_sum << endl;

  // Set up some histograms to hold all the errors that are calculated
  TH1F *raw_stat_err2 = new TH1F("raw_stat_err2","raw_stat_err2",49,1.,99.);
  TH1F *QCD_stat_err2 = new TH1F("QCD_stat_err2","QCD_stat_err2",49,1.,99.);
  TH1F *eemc_stat_err2 = new TH1F("eemc_stat_err2","eemc_stat_err2",49,1.,99.);
  TH1F *tau_stat_err2 = new TH1F("tau_stat_err2","tau_stat_err2",49,1.,99.);
  TH1F *QCD_syst_high_err = new TH1F("QCD_syst_high_err","QCD_syst_high_err",49,1.,99.);
  TH1F *QCD_syst_low_err = new TH1F("QCD_syst_low_err","QCD_syst_low_err",49,1.,99.);
  TH1F *zsig_stat_err2 = new TH1F("zsig_stat_err2","zsig_stat_err2",49,1.,99.);
  TH1F *zback_stat_err2 = new TH1F("zback_stat_err2","zback_stat_err2",49,1.,99.);
  TH1F *zeemc_stat_err2 = new TH1F("zeemc_stat_err2","zeemc_stat_err2",49,1.,99.);
 
 
  for (int i=1; i<=49; i++) {
    raw_stat_err2->SetBinContent(i,signal2->GetBinContent(i));
    QCD_stat_err2->SetBinContent(i,fabs(norm*new_bkgd->GetBinContent(i)));
    eemc_stat_err2->SetBinContent(i,max(eemc_bkgd2->GetBinContent(i),0)); //remove negative error
    tau_stat_err2->SetBinContent(i,tau_norm*taufrac*tauhist->GetBinContent(i));
    zsig_stat_err2->SetBinContent(i,Z_norm*zsig_bkgd2->GetBinContent(i));
    zback_stat_err2->SetBinContent(i,norm*norm*Z_norm*zback_bkgd2->GetBinContent(i));
    zeemc_stat_err2->SetBinContent(i,Z_norm*zeemc_bkgd2->GetBinContent(i));

    //cout << "Error " << i << " " << raw_stat_err2->GetBinCenter(i) << " " << raw_stat_err2->GetBinContent(i) << " " << QCD_stat_err2->GetBinContent(i) << " " << eemc_stat_err2->GetBinContent(i) << " " << tau_stat_err2->GetBinContent(i) << endl;
  }

  // Now go through all the 60 background shapes and find the
  // high and low in each ET bin to give the maximum extent uncertainty
  // for each ET bin (and also sum the low and high to give the
  // overall number used in the xsec analysis)  

  TH1F *low_bkgd = new_bkgd->Clone();
  low_bkgd->SetName("low_bkgd");
  TH1F *high_bkgd = new_bkgd->Clone();
  high_bkgd->SetName("high_bkgd");

  float low_sum = 0.;
  float high_sum = 0.;
  for (int i=7; i<=49; i++) {
    float high = 0.;
    float low = 10000.;
    for (int j=0; j<=20; j++) {
      if (new_bkgd_hists1[j]->GetBinContent(i) < low) {
	if (new_bkgd_hists1[j]->GetBinContent(i) >= 0) {
	  low = new_bkgd_hists1[j]->GetBinContent(i);
	}
      }
      if (new_bkgd_hists1[j]->GetBinContent(i) > high) {
	high = new_bkgd_hists1[j]->GetBinContent(i);
      }
      
      if (new_bkgd_hists2[j]->GetBinContent(i) < low) {
	if (new_bkgd_hists2[j]->GetBinContent(i) >= 0) {
	  low = new_bkgd_hists2[j]->GetBinContent(i);
	}
      }
      if (new_bkgd_hists2[j]->GetBinContent(i) > high) {
	high = new_bkgd_hists2[j]->GetBinContent(i);
      }
      
      if (new_bkgd_hists3[j]->GetBinContent(i) < low) {
	if (new_bkgd_hists3[j]->GetBinContent(i) >= 0) {
	  low = new_bkgd_hists3[j]->GetBinContent(i);
	}
      }
      if (new_bkgd_hists3[j]->GetBinContent(i) > high) {
	high = new_bkgd_hists3[j]->GetBinContent(i);
      }
      //cout << i << " low = " << low << " high = " << high << endl;
      //} // end of k-loop
    } // end of j-loop

    // calculate the sum
    low_bkgd->SetBinContent(i,0.);
    if ((low != 10000) && (new_bkgd->GetBinContent(i)-low > 0)) {
      if ((i >= 13)&&(i<=49)) {low_sum += low;}
      low_bkgd->SetBinContent(i,low);
    }
    if ((i >= 13)&&(i<=49)) {high_sum += high;} 
    high_bkgd->SetBinContent(i,high);
    //cout << i << " " << low_sum << " " << high_sum << endl;
    //cout << QCD_syst_low_err->GetBinCenter(i) << " low = " << low << " high = " << high << " nom = " << new_bkgd->GetBinContent(i) << endl;  
    // set the bin-by-bin error too
    if ((low != 10000) && (new_bkgd->GetBinContent(i)-low > 0)) {
      QCD_syst_low_err->SetBinContent(i,new_bkgd->GetBinContent(i)-low);
    } else {
      QCD_syst_low_err->SetBinContent(i,0.);
    }
    QCD_syst_high_err->SetBinContent(i,high-new_bkgd->GetBinContent(i));

  } // end of i=loop 

  cout << "QCD shape sys. unc. calc************" << endl;
  cout << "The QCD low sum = " << low_sum << endl;
  cout << "The QCD high sum = " << high_sum << endl; 

  cout << "The QCD low error = " << QCD_sum-low_sum << endl;
  cout << "The QCD high error = " << high_sum-QCD_sum << endl; 

  //totals for ET > 25
  float tot_stat = sqrt(tau_stat*tau_stat+eemc_stat*eemc_stat+Z_stat*Z_stat+norm*QCD_sum);
  cout << "total stat unc. is " << tot_stat << endl;
  float tot_syst_low = sqrt(tau_syst*tau_syst+eemc_syst*eemc_syst+Z_syst*Z_syst+(QCD_sum-low_sum)*(QCD_sum-low_sum));
  float tot_syst_high = sqrt(tau_syst*tau_syst+eemc_syst*eemc_syst+Z_syst*Z_syst+(QCD_sum-high_sum)*(QCD_sum-high_sum));
  cout << "total syst unc. is low: " << tot_syst_low << " and high: " << tot_syst_high << endl;
  cout << "total unc is low: " << sqrt(tot_syst_low*tot_syst_low+tot_stat*tot_stat) << " high: " << sqrt(tot_syst_high*tot_syst_high+tot_stat*tot_stat) <<endl;
 
  //final signal histo
  TH1F *signal_final3 = new TH1F("signal_final3","signal_final3",49,1.,99.);
  for (int i=1; i<=49; i++) {
    signal_final3->SetBinContent(i,signal_final2->GetBinContent(i));
  }
  signal_final3->Add(new_bkgd,-1.);


  //need to get 4 GeV wide bins starting at ET=25 to correct yields
#if 1
  cout<<"ET min, ET max, N_obs, N_obs-Nbkgd, QCD stat, EEMC stat, Tau stat, Zee stat, QCD syst+, QCD syst-, total stat, MC bkgd tot"<<endl;
  for (int i=7; i<=49; i=i+2) {
    cout.setf(ios::fixed);
    cout.precision(2);
    float Qcd_stat_err2=QCD_stat_err2->GetBinContent(i)+QCD_stat_err2->GetBinContent(i+1);
    float Eemc_stat_err2=eemc_stat_err2->GetBinContent(i)+eemc_stat_err2->GetBinContent(i+1);
    float Tau_stat_err2=tau_stat_err2->GetBinContent(i)+tau_stat_err2->GetBinContent(i+1);
    float Z_stat_err2=zsig_stat_err2->GetBinContent(i)+zsig_stat_err2->GetBinContent(i+1);
    cout << signal2->GetBinCenter(i)-1 << "," << signal2->GetBinCenter(i)+3 << "," << signal2->GetBinContent(i)+signal2->GetBinContent(i+1) << "," << signal_final3->GetBinContent(i)+signal_final3->GetBinContent(i+1) << "," << sqrt(Qcd_stat_err2) << "," << sqrt(Eemc_stat_err2) << "," << sqrt(Tau_stat_err2) << "," << sqrt(Z_stat_err2) << "," << QCD_syst_high_err->GetBinContent(i)+QCD_syst_high_err->GetBinContent(i+1) << "," << QCD_syst_low_err->GetBinContent(i)+QCD_syst_low_err->GetBinContent(i+1) ;
    //total statistical uncertainty
    cout<< "," <<sqrt(Tau_stat_err2+Z_stat_err2+Qcd_stat_err2+Eemc_stat_err2);
    //total MC syst uncertainty
    cout<< "," << zsig_bkgd2->GetBinContent(i)+zsig_bkgd2->GetBinContent(i+1) + tauhist->GetBinContent(i)+tauhist->GetBinContent(i+1)<<endl;
    
  }
#endif

  // ******************************************************
  // Write out the histograms of interest to a
  // file
  // ******************************************************

  if (charge == 1) {
    TFile *f2 = new TFile("bkgd_histos_pos_final.root","recreate");
  } else if (charge == -1) {
    TFile *f2 = new TFile("bkgd_histos_neg_final.root","recreate");
  } else if (charge == 0) {
    TFile *f2 = new TFile("bkgd_histos_sum.root","recreate");
  }

  f2->cd();

  if (two_or_four == 2) { 

    TH1F *signal_final3 = new TH1F("signal_final3","signal_final3",49,1.,99.);
    for (int i=1; i<=49; i++) {
      signal_final3->SetBinContent(i,signal_final2->GetBinContent(i));
    }
    signal_final3->Add(new_bkgd,-1.);

    tauhist->Write();
    zsig_bkgd2->Write();
    signal2->Write();
    signal_final2->Write(); 
    signal_final3->Write();
    eemc_bkgd2->Write(); 
    new_bkgd->Write();
    low_bkgd->Write();
    high_bkgd->Write();
    signal_in_norm_region->Write();

    raw_stat_err2->Write();
    QCD_stat_err2->Write();
    eemc_stat_err2->Write();
    tau_stat_err2->Write();
    QCD_syst_high_err->Write();
    QCD_syst_low_err->Write();
    zsig_stat_err2->Write();
    zback_stat_err2->Write();
    zeemc_stat_err2->Write();

#if 0
    for (int i=1; i<=49; i++) {
      cout.setf(ios::fixed);
      cout.precision(2);
      cout << " " << signal2->GetBinCenter(i) << " & " << signal2->GetBinContent(i) << " & " << signal_final3->GetBinContent(i) << " & " << QCD_stat_err2->GetBinContent(i) << " & " << eemc_stat_err2->GetBinContent(i) << " & " << tau_stat_err2->GetBinContent(i) << " & " << zsig_stat_err2->GetBinContent(i) << " & " << zback_stat_err2->GetBinContent(i) << " & " << QCD_syst_high_err->GetBinContent(i) << " & " << QCD_syst_low_err->GetBinContent(i) << " \\\\" << endl;
      
    }
#endif
  
    f2->Close();
 
  } else if (two_or_four == 4) {

    TH1F *tauhist_repack = new TH1F("tauhist_r","tauhist_r",24,3.,99.);
    TH1F *zsig_bkgd2_repack = new TH1F("zsig_bkgd2_r","zsig_bkgd2_r",24,3,99.);
    TH1F *signal2_repack = new TH1F("signal2_r","signal2_r",24,3.,99.);
    TH1F *signal_final2_repack = new TH1F("signal_final2_r","signal_final2_r",24,3.,99.);
    TH1F *signal_final3_repack = new TH1F("signal_final3_r","signal_final3_r",24,3.,99.);
    TH1F *eemc_bkgd2_repack = new TH1F("eemc_bkgd2_r","eemc_bkgd2_r",24,3.,99.);
    TH1F *new_bkgd_repack = new TH1F("new_bkgd_r","new_bkgd_r",24,3.,99.);
    TH1F *low_bkgd_repack = new TH1F("low_bkgd_r","low_bkgd_r",24,3.,99.);
    TH1F *high_bkgd_repack = new TH1F("high_bkgd_r","high_bkgd_r",24,3.,99.);
    TH1F *signal_in_norm_region_repack = new TH1F("signal_in_norm_region_r","signal_in_norm_region_r",24,3.,99.);

    TH1F *raw_stat_err2_repack = new TH1F("raw_stat_err2_r","raw_stat_err2_r",24,3.,99.);
    TH1F *QCD_stat_err2_repack = new TH1F("QCD_stat_err2_r","QCD_stat_err2_r",24,3.,99.);
    TH1F *eemc_stat_err2_repack = new TH1F("eemc_stat_err2_r","eemc_stat_err2_r",24,3.,99.);
    TH1F *tau_stat_err2_repack = new TH1F("tau_stat_err2_r","tau_stat_err2_r",24,3.,99.);
    TH1F *QCD_syst_high_err_repack = new TH1F("QCD_syst_high_err_r","QCD_syst_high_err_r",24,3.,99.);
    TH1F *QCD_syst_low_err_repack = new TH1F("QCD_syst_low_err_r","QCD_syst_low_err_r",24,3.,99.);
    TH1F *zsig_stat_err2_repack = new TH1F("zsig_stat_err2_r","zsig_stat_err2_r",24,3.,99.);
    TH1F *zback_stat_err2_repack = new TH1F("zback_stat_err2_r","zback_stat_err2_r",24,3.,99.);
    TH1F *zeemc_stat_err2_repack = new TH1F("zeemc_stat_err2_r","zeemc_stat_err2_r",24,3.,99.);

    for (int i=1; i<=24; i++) {
      tauhist_repack->SetBinContent(i,tauhist->GetBinContent(2*i)+
                                      tauhist->GetBinContent(2*i+1));
      zsig_bkgd2_repack->SetBinContent(i,zsig_bkgd2->GetBinContent(2*i)+
                                      zsig_bkgd2->GetBinContent(2*i+1));;
      signal2_repack->SetBinContent(i,signal2->GetBinContent(2*i)+
                                      signal2->GetBinContent(2*i+1));
      signal_final2_repack->SetBinContent(i,signal_final2->GetBinContent(2*i)+
                                            signal_final2->GetBinContent(2*i+1));
      signal_final3_repack->SetBinContent(i,signal_final3->GetBinContent(2*i)+
                                            signal_final3->GetBinContent(2*i+1));
      eemc_bkgd2_repack->SetBinContent(i,eemc_bkgd2->GetBinContent(2*i)+
                                         eemc_bkgd2->GetBinContent(2*i+1));
      new_bkgd_repack->SetBinContent(i,new_bkgd->GetBinContent(2*i)+
                                       new_bkgd->GetBinContent(2*i+1));
      low_bkgd_repack->SetBinContent(i,low_bkgd->GetBinContent(2*i)+
                                       low_bkgd->GetBinContent(2*i+1));
      high_bkgd_repack->SetBinContent(i,high_bkgd->GetBinContent(2*i)+
                                        high_bkgd->GetBinContent(2*i+1));
      signal_in_norm_region_repack->SetBinContent(i,signal_in_norm_region->GetBinContent(2*i)+signal_in_norm_region->GetBinContent(2*i+1));

      raw_stat_err2_repack->SetBinContent(i,raw_stat_err2->GetBinContent(2*i)+
                                            raw_stat_err2->GetBinContent(2*i+1));
      QCD_stat_err2_repack->SetBinContent(i,QCD_stat_err2->GetBinContent(2*i)+
                                          QCD_stat_err2->GetBinContent(2*i+1));
      eemc_stat_err2_repack->SetBinContent(i,eemc_stat_err2->GetBinContent(2*i)+
                                             eemc_stat_err2->GetBinContent(2*i+1));
      tau_stat_err2_repack->SetBinContent(i,tau_stat_err2->GetBinContent(2*i)+
                                            tau_stat_err2->GetBinContent(2*i+1));
      QCD_syst_high_err_repack->SetBinContent(i,QCD_syst_high_err->GetBinContent(2*i)+QCD_syst_high_err->GetBinContent(2*i+1));
      QCD_syst_low_err_repack->SetBinContent(i,QCD_syst_low_err->GetBinContent(2*i)+QCD_syst_low_err->GetBinContent(2*i+1));
      zsig_stat_err2_repack->SetBinContent(i,zsig_stat_err2->GetBinContent(2*i)+
                                             zback_stat_err2->GetBinContent(2*i+1));
      zback_stat_err2_repack->SetBinContent(i,zback_stat_err2->GetBinContent(2*i)+
                                             zback_stat_err2->GetBinContent(2*i+1));
      zeemc_stat_err2_repack->SetBinContent(i,zeemc_stat_err2->GetBinContent(2*i)+
                                             zeemc_stat_err2->GetBinContent(2*i+1));
    }

    tauhist_repack->Write();
    zsig_bkgd2_repack->Write();
    signal2_repack->Write();
    signal_final2_repack->Write();
    signal_final3_repack->Write();
    eemc_bkgd2_repack->Write();
    new_bkgd_repack->Write();
    low_bkgd_repack->Write();
    high_bkgd_repack->Write();
    signal_in_norm_region_repack->Write();

    raw_stat_err2_repack->Write();
    QCD_stat_err2_repack->Write();
    eemc_stat_err2_repack->Write();
    tau_stat_err2_repack->Write();
    QCD_syst_high_err_repack->Write();
    QCD_syst_low_err_repack->Write();
    zsig_stat_err2_repack->Write();
    zback_stat_err2_repack->Write();
    zeemc_stat_err2_repack->Write();

#if 0
    for (int i=1; i<=24; i++) {
      cout.setf(ios::fixed);
      cout.precision(2);
      cout << " " << signal2_repack->GetBinCenter(i) << " & " << signal2_repack->GetBinContent(i) << " & " << signal_final3_repack->GetBinContent(i) << " & " << QCD_stat_err2_repack->GetBinContent(i) << " & " << eemc_stat_err2_repack->GetBinContent(i) << " & " << tau_stat_err2_repack->GetBinContent(i) << " & " << zsig_stat_err2_repack->GetBinContent(i) << " & " << zback_stat_err2_repack->GetBinContent(i) << " & " << QCD_syst_high_err_repack->GetBinContent(i) << " & " << QCD_syst_low_err_repack->GetBinContent(i) << " \\\\" << endl;
    }
#endif

    f2->Close();

  }

  
} // end of macro
