// ********************************************************
// This code makes the necessary root histos for the 
// background subtraction and systematic uncertainty calculations
// Set charge == 1 for postive charges
// Set charge == -1 for negative charges
// Set charge == 0 for the sum of both
//
// Set two_or_four == 2 for 2 GeV wide bin histograms
// Set two_or_four == 4 for 4 GeV wide bin hisrograms
// ********************************************************

void makeBkgdFiles(int charge, int two_or_four) {

  // ******************************************************
  // Read in the data set and all the histograms needed 
  // to do the actual calculation
  // ******************************************************

  gStyle->SetOptDate(0);
  TFile *f1 = new TFile("run9setP1234.wana.hist.root");
 
  // hybrid2
  //TFile *f1 = new TFile("run9setP1234.wana.hist.root");
  //TFile *f1 = new TFile("noZTag/run9setP1234.wana.hist.root");

  // get the signal and missing endcap backgrounds
  if (charge == 1) {
    TH1F *signal = (TH1F*)f1->Get("pos_muclustpTbal_wE");
    TH1F *signal_wo_eemc = (TH1F*)f1->Get("pos_muclustpTbal_noE");
  } else if (charge == -1) {
    TH1F *signal = (TH1F*)f1->Get("neg_muclustpTbal_wE");
    TH1F *signal_wo_eemc = (TH1F*)f1->Get("neg_muclustpTbal_noE");
  } else if (charge == 0) {
    TH1F *signal = (TH1F*)f1->Get("muclustPtBal");
    TH1F *signal_wo_eemc = (TH1F*)f1->Get("muclustPtBalnoE");
  }

  // ******************************************************
  // Read in all the MC data sets for background 
  // subtractions and studies
  // ******************************************************

  TFile *MC_fs[6];

  /*MC_fs[1] = new TFile("rcf10010.wana.hist.root"); // W+ -> e++nu
  MC_fs[0] = new TFile("rcf10011.wana.hist.root"); // W- -> e-+nu
  MC_fs[2] = new TFile("rcf10012.wana.hist.root"); // W -> tau+nu
  MC_fs[3] = new TFile("rcf10014.wana.hist.root"); // W -> any
  MC_fs[4] = new TFile("rcf10015.wana.hist.root"); // Z -> any
*/


  /*MC_fs[0] = new TFile("rck10010.wana.hist.root"); // W+ -> e++nu
  MC_fs[1] = new TFile("rck10011.wana.hist.root"); // W- -> e-+nu
  MC_fs[2] = new TFile("rck10012.wana.hist.root"); // W -> tau+nu
  MC_fs[3] = new TFile("rck10017.wana.hist.root"); // W -> any
  MC_fs[4] = new TFile("rck10018.wana.hist.root"); // Z -> any
*/

  MC_fs[0] = new TFile("rcn10010.wana.hist.root"); // W+ -> e++nu
  MC_fs[1] = new TFile("rcn10011.wana.hist.root"); // W- -> e-+nu
  MC_fs[2] = new TFile("rcn10012.wana.hist.root"); // W -> tau+nu
  MC_fs[3] = new TFile("rcn10017.wana.hist.root"); // W -> any
  MC_fs[4] = new TFile("rcn10018.wana.hist.root"); // Z -> any


/*
  MC_fs[0] = new TFile("noZTag/rck10010.wana.hist.root"); // W+ -> e++nu
  MC_fs[1] = new TFile("noZTag/rck10011.wana.hist.root"); // W- -> e-+nu
  MC_fs[2] = new TFile("noZTag/rck10012.wana.hist.root"); // W -> tau+nu
  MC_fs[3] = new TFile("noZTag/rck10017.wana.hist.root"); // W -> any
  MC_fs[4] = new TFile("noZTag/rck10018.wana.hist.root"); // Z -> any
*/

  // Because of a screw up between the RFF and FF in the simulation
  // and reconstruction the charge sign of the MC needs to be inverted
  TH1F *MC_dists_raw[5][3];
  for (int i=0; i<5; i++) {
    if (charge == 1) {
      MC_dists_raw[i][0] = (TH1F*)MC_fs[i]->Get("pos_muclustpTbal_wE");
      MC_dists_raw[i][1] = (TH1F*)MC_fs[i]->Get("pos_muclustpTbal_noE");
      //MC_dists_raw[i][2] = (TH1F*)MC_fs[i]->Get("pos_muclustpTbal_bckgrd");
    } else if (charge == -1) {
      MC_dists_raw[i][0] = (TH1F*)MC_fs[i]->Get("neg_muclustpTbal_wE");
      MC_dists_raw[i][1] = (TH1F*)MC_fs[i]->Get("neg_muclustpTbal_noE");
      ///MC_dists_raw[i][2] = (TH1F*)MC_fs[i]->Get("neg_muclustpTbal_bckgrd");
    } else if (charge == 0) {
      MC_dists_raw[i][0] = (TH1F*)MC_fs[i]->Get("muclustPtBal");
      MC_dists_raw[i][1] = (TH1F*)MC_fs[i]->Get("muclustPtBalnoE");
      MC_dists_raw[i][2] = (TH1F*)MC_fs[i]->Get("muclustPtBal_bckgrd");
    }
    MC_dists_raw[i][2] = (TH1F*)MC_fs[i]->Get("muclustPtBal_bckgrd");

  }

  // Old lumi estimates
  //float lumi[5] = {116.5,318.5,76.9,1220.,45.6}; // old MC
  //float lumi[5] = {30.,63.,61.,14.7,33.};

  // w/ Z and W xsec scaled to RHICBOS from PYTHiA
  //float lumi[5] = {30./(124./98.7),63./(41./32.9),61./((124.+41.)/(98.7+32.9)),14.7/((124+41)/(98.7+32.9)),33./(10./8.23)};

  // w/ Z and W xsec scaled to RHICBOS from PYTHiA - new MC
  float lumi[5] = {30.4/(124./98.7),31.4/(41./32.9),30.3/((124.+41.)/(98.7+32.9)),30.8/((124+41)/(98.7+32.9)),33.4/(10./8.23)};

  float lumi_fact[6];
  for (int i=0; i<5; i++) {lumi_fact[i] = 12.0/lumi[i];}

  for (int i=0; i<5; i++) {
    for (int j=0; j<3; j++) {
      MC_dists_raw[i][j]->Scale(lumi_fact[i]);
    }
  }


  // Repack the histograms to mesh with the odd staggered binning that
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
        MC_dists_repack[i][j]->SetBinContent(k,MC_dists_raw[i][j]->GetBinContent(2*k)+
                                               MC_dists_raw[i][j]->GetBinContent(2*k+1));
      }
    }
  }

  // Make the EEMC background the real one of interest now
  for (int i=0; i<5; i++) {
    MC_dists_repack[i][1]->Add(MC_dists_repack[i][0],-1);
    MC_dists_raw[i][1]->Add(MC_dists_raw[i][0],-1.);
  }

  // **********************************************
  // Do some otherstuff
  // **********************************************

  TH1F *eemc_bkgd = signal_wo_eemc->Clone();
  eemc_bkgd->Add(signal,-1.);
  TH1F *signal_final = signal->Clone();
  signal_final->Add(eemc_bkgd,-1);

  TH1F *eemc_bkgd2 = new TH1F("eemc_bkgd2","eemc_bkgd2",49,1,99);
  TH1F *zsig_bkgd2 = new TH1F("zsig_bkgd2","zsig_bkgd2",49,1,99);
  TH1F *zeemc_bkgd2 = new TH1F("zeemc_bgkd2","zeemc_bkgd2",49,1,99);
  TH1F *zback_bkgd2 = new TH1F("zback_bkgd2","zback_bkgd2",49,1,99);

  TH1F *wanysig_bkgd2 = new TH1F("wanysig_bkgd2","wanysig_bkgd2",49,1,99);
  TH1F *wanyeemc_bkgd2 = new TH1F("wanyeemc_bgkd2","wanyeemc_bkgd2",49,1,99);
  TH1F *wanyback_bkgd2 = new TH1F("wanyback_bkgd2","wanyback_bkgd2",49,1,99);

  TH1F *zsig = MC_dists_raw[4][0]->Clone();
  TH1F *zeemc = MC_dists_raw[4][1]->Clone();
  TH1F *zback = MC_dists_raw[4][2]->Clone();

  signal_final->Add(zsig,-1.);

  // First get the "nominal" background shape
  TH1F *bkgd_shape1 = (TH1F*)f1->Get("muclustPtBal_bckgrd");

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
  for (int i=1; i<=49; i++) {
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
    wanysig_bkgd2->SetBinContent(i,MC_dists_raw[3][0]->GetBinContent(2*i)+
                                   MC_dists_raw[3][0]->GetBinContent(2*i+1));
    wanyeemc_bkgd2->SetBinContent(i,MC_dists_raw[3][1]->GetBinContent(2*i)+
                                    MC_dists_raw[3][1]->GetBinContent(2*i+1));
    wanyback_bkgd2->SetBinContent(i,MC_dists_raw[3][2]->GetBinContent(2*i)+
                                    MC_dists_raw[3][2]->GetBinContent(2*i+1));
  }

  TCanvas *can2 = new TCanvas("can2","can2",0,0,600,400);
  signal2->Draw();
  signal_final2->Draw("same");
  //can2->Print("signal_w_eemc.eps");
  //can2->Print("signal_w_eemc.png");


  // **********************************************
  // plot all the business
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

  //Tau "background" needs no further scaling
  float taufrac=1.5;
  // calculate a fractional scale factor for the charge separated
  // background due to tau->e
  //float tauplus_xsec = 9.8e-8;
  //float tauminus_xsec = 3.1e-8;
  //float taufrac;
  //if (charge == 1) {
  //  taufrac = tauplus_xsec/(tauplus_xsec+tauminus_xsec);
  //} else if (charge == -1) {
  //  taufrac = tauminus_xsec/(tauplus_xsec+tauminus_xsec);
  //} else if (charge == 0) {
  // taufrac = 1.;
  //}


  // ******************************************************
  // Do the iterative normalization of the W signal to
  // to the background for the nominal shape
  // ****************************************************** 

  // subtract off the tau background
  signal_final2->Add(MC_dists_repack[2][0],-1.*taufrac);

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

    // subtract off the Z signal from the background
    bkgd_shape_unnorm[i]->Add(zback_bkgd2,-1.);
    
    // calculate the W signal in the normalization bins
    signal_in_norm[8] = func1->Integral(15,17);
    signal_in_norm[9] = func1->Integral(17,19);
    signal_in_norm[10] = func1->Integral(19,21);
    //cout << "Integrals = " << signal_in_norm[8] << " " << signal_in_norm[9] << " " << signal_in_norm[10] << endl;
    for (int j=8; j<=10; j++) {
      if (signal_in_norm[j] < 0) {signal_in_norm[j] = 0.;}
    }

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
  // Calculate all the 1200 shapes for the background
  // systematic shape study
  // ******************************************************

  // get the various background shapes made by
  // using different cuts for the awayside backgrounds
  TH2F *bkgd_hists_from_file[21];
  TH2F *bkgd_hists_from_file2[21];
  char str[200];
  for (int i=0; i<=20; i++) {

//    if (charge == 1) {
//      sprintf(str,"pos_failAwaySide_Awayside_pt_bin_%d",i);
//      bkgd_hists_from_file[i] = (TH2F*)f1->Get(str);
//    } else if (charge == -1) {
//      sprintf(str,"neg_failAwaySide_Awayside_pt_bin_%d",i);
//      bkgd_hists_from_file[i] = (TH2F*)f1->Get(str);
//    } else if (charge == 0) {
      sprintf(str,"pos_failAwaySide_Awayside_pt_bin_%d",i);
      bkgd_hists_from_file[i] = (TH2F*)f1->Get(str);
      sprintf(str,"neg_failAwaySide_Awayside_pt_bin_%d",i);
      bkgd_hists_from_file2[i] = (TH2F*)f1->Get(str);
      bkgd_hists_from_file[i]->Add(bkgd_hists_from_file2[i]);
//    }
  }

  // Now do the rebinning
  TH1F *bkgd_hists1[21][21];
  TH1F *bkgd_hists2[21][21];
  TH1F *bkgd_hists3[21][21];
  int jval=0;
  for (int i=0; i<=20; i++) {
    int j=jval;
//    for (int j=0; j<20; j++) {
      sprintf(str,"bkgd_hist1_%d_%d",i,j);
      bkgd_hists1[i][j] = new TH1F(str,str,49,1,99);
      sprintf(str,"bkgd_hist2_%d_%d",i,j);
      bkgd_hists2[i][j] = new TH1F(str,str,49,1,99);
      sprintf(str,"bkgd_hist3_%d_%d",i,j);
      bkgd_hists3[i][j] = new TH1F(str,str,49,1,99);
      for (int k=1; k<=49; k++) {
        bkgd_hists1[i][j]->SetBinContent(k,bkgd_hists_from_file[0]->GetBinContent(2*k,i+1)+bkgd_hists_from_file[0]->GetBinContent(2*k+1,i+1));
        bkgd_hists2[i][j]->SetBinContent(k,bkgd_hists_from_file[0]->GetBinContent(2*k,i+1)+bkgd_hists_from_file[0]->GetBinContent(2*k+1,i+1));
        bkgd_hists3[i][j]->SetBinContent(k,bkgd_hists_from_file[0]->GetBinContent(2*k,i+1)+bkgd_hists_from_file[0]->GetBinContent(2*k+1,i+1));
      }
   // }
  }



  // initiaize the iterative fit functions
  TF1 *func2 = new TF1("func2","[0]+[1]*x",23,39);
  func2->SetParameter(0,0.);
  func2->SetParameter(1,0.);
  TF1 *func3 = new TF1("func3","[0]+[1]*x",23,39);
  func3->SetParameter(0,0.);
  func3->SetParameter(1,0.);

  // Now loop over the the 1200 possibilities (400 loops and 3 normalization
  // regions)
  float final_sig_in_norm[21][21][3];
  float final_chisquare[21][21];
  float signal_in_norm1[50];
  float signal_in_norm2[50];
  float signal_in_norm3[50];
  TH1F *new_bkgd_hists1[21][21];
  TH1F *new_bkgd_hists2[21][21];
  TH1F *new_bkgd_hists3[21][21];
  TH1F *bkgd_shape_unnorm1[20];
  TH1F *bkgd_shape_unnorm2[20];
  TH1F *bkgd_shape_unnorm3[20];
  TH1F *signal_for_new1[20];
  TH1F *signal_for_new2[20];
  TH1F *signal_for_new3[20];
  for (int i=0; i<=20; i++) {
    int j=jval;
//    for (int j=0; j<20; j++) {

      func1->SetParameter(0,0.);
      func1->SetParameter(1,0.);
      func2->SetParameter(0,0.);
      func2->SetParameter(1,0.);
      func3->SetParameter(0,0.);
      func3->SetParameter(1,0.);

      for (int l=0; l<20; l++) {
        bkgd_shape_unnorm1[l] = (TH1F*)bkgd_hists1[i][j]->Clone();
        bkgd_shape_unnorm2[l] = (TH1F*)bkgd_hists2[i][j]->Clone();
        bkgd_shape_unnorm3[l] = (TH1F*)bkgd_hists3[i][j]->Clone();
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
        for (int m=1; m<=49; m++) {
          if (bkgd_shape_unnorm3[l]->GetBinContent(m) < 0) {bkgd_shape_unnorm3[l]->SetBinContent(m,0.);}
        }
        signal_for_new3[l]->Add(bkgd_shape_unnorm3[l],-1.);
        signal_for_new3[l]->Fit(func3,"RQ");
      } // end of for loop over l

      // Save the last iteration as the background histogram
      new_bkgd_hists1[i][j] = (TH1F*)bkgd_shape_unnorm1[19]->Clone();
      new_bkgd_hists2[i][j] = (TH1F*)bkgd_shape_unnorm2[19]->Clone();
      new_bkgd_hists3[i][j] = (TH1F*)bkgd_shape_unnorm3[19]->Clone();

    //}
  }

  // plot all the 1200 histograms
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
    int j=jval;
    //for (int j=0; j<20; j++) {
      new_bkgd_hists1[i][j]->Draw("same");
      new_bkgd_hists2[i][j]->Draw("same");
      new_bkgd_hists3[i][j]->Draw("same");
    //}
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
    int j=jval;
    //for (int j=0; j<20; j++) {
      chi2s->Fill(final_chisquare[i][j]);
    //}
  }
 
  TCanvas *can7 = new TCanvas("can7","can7",0,0,600,400);
  chi2s->Draw();
  //can7->Print("chi2s.eps"); 
  //can7->Print("chi2s.png");

  // ************************************************
  // Now calculate the all background numbers and their 
  // systematic uncertainties (and spit them out to histograms)
  // ************************************************

  // First get the simple numbers (backgrounds and their 
  // statistical uncertainties)
  TH1F *tauhist = MC_dists_repack[2][0]->Clone();
  //TH1F *zsig = MC_dists_repack[3][0]->Clone(); 
  //TH1F *zeemc = MC_dists_repack[3][1]->Clone();
  //TH1F *zback = MC_dists_repack[3][2]->Clone();
  tauhist->Scale(taufrac);
  float tau_norm = lumi_fact[2];
  float Z_norm = lumi_fact[4];
  float bkgd_sum = 0.;
  float signal_sum = 0.;
  float raw_sum = 0.;
  float QCD_sum = 0., tau_sum = 0., eemc_sum = 0.;
  float QCD_raw_sum = 0.;
  float Wany_bkgd_sum = 0.;
  float Wany_eemc_sum = 0.;
  float zsig_sum = 0., zeemc_sum = 0.,zback_sum = 0.;
  float wanysig_sum = 0., wanyeemc_sum = 0.,wanyback_sum = 0.;
  for (int i=13; i<=26; i++) {
    //cout << i << " " << new_bkgd->GetBinCenter(i) << " " << new_bkgd->GetBinContent(i) << " " << tauhist->GetBinContent(i) << " " << eemc_bkgd2->GetBinContent(i) << endl;
    bkgd_sum += new_bkgd->GetBinContent(i);
    //bkgd_sum += tauhist->GetBinContent(i);
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
    wanysig_sum += wanysig_bkgd2->GetBinContent(i);
    wanyeemc_sum += wanyeemc_bkgd2->GetBinContent(i);
    wanyback_sum += wanyback_bkgd2->GetBinContent(i);
 
  }
  cout << "The total background for ET>25 is " << bkgd_sum+zsig_sum << endl;
  cout << "QCD = " << QCD_sum << ", tau = " << tau_sum << ", eemc = " << eemc_sum << ", and Z = " << zsig_sum << endl;
  cout << "Raw = " << raw_sum << endl;
  cout << "W Signal (w/o tau) = " << signal_sum-QCD_sum << endl;
  cout << "Z in sig = " << zsig_sum << endl;
  cout << "Z in eemc = " << zeemc_sum << endl;
  cout << "Z in back = " << zback_sum << endl;
  cout << "Wany in sig = " << wanysig_sum << endl;
  cout << "Wany in eemc = " << wanyeemc_sum << endl;
  cout << "Wany in back = " << wanyback_sum << endl;
  cout << "QCD raw in back = " << QCD_raw_sum << endl; 
  cout << "The QCD stat unc. is " << norm*sqrt(QCD_sum/norm) << endl;
  float tau_stat = tau_norm*taufrac*sqrt(tau_sum/(tau_norm*taufrac));
  cout << "The tau stat unc. is " << tau_stat << endl;
  float eemc_stat = sqrt(eemc_sum);
  cout << "The eemc stat unc. is " << eemc_stat << endl;
  float Z_stat = Z_norm*sqrt(zsig_sum/Z_norm);
  cout << "The Z stat unc. is " << Z_stat << endl;
  float tot_stat = sqrt(tau_stat*tau_stat+eemc_stat*eemc_stat+Z_stat*Z_stat);
  cout << "tau+eemc+Z stat unc. is " << sqrt(tau_stat*tau_stat+eemc_stat*eemc_stat+Z_stat*Z_stat) << endl;
  
  cout << "f_tau = " << tau_sum/raw_sum << endl;
  cout << "f_QCD = " << QCD_sum/raw_sum << endl;
  cout << "f_EEMC = " << eemc_sum/raw_sum << endl;
  cout << "f_Z = " << zsig_sum/raw_sum << endl;

  // Set up some histograms to hold all the errors that
  // are calculated
  TH1F *raw_stat_err2 = new TH1F("raw_stat_err2","raw_stat_err2",49,1.,99.);
  TH1F *QCD_stat_err2 = new TH1F("QCD_stat_err2","QCD_stat_err2",49,1.,99.);
  TH1F *eemc_stat_err2 = new TH1F("eemc_stat_err2","eemc_stat_err2",49,1.,99.);
  TH1F *tau_stat_err2 = new TH1F("tau_stat_err2","tau_stat_err2",49,1.,99.);
  TH1F *QCD_syst_high_err = new TH1F("QCD_syst_high_err","QCD_syst_high_err",49,1.,99.);
  TH1F *QCD_syst_low_err = new TH1F("QCD_syst_low_err","QCD_syst_low_err",49,1.,99.);
  TH1F *zsig_stat_err2 = new TH1F("zsig_stat_err2","zsig_stat_err2",49,1.,99.);
  TH1F *zback_stat_err2 = new TH1F("zback_stat_err2","zback_stat_err2",49,1.,99.);
 
 
  for (int i=1; i<=26; i++) {
    raw_stat_err2->SetBinContent(i,signal2->GetBinContent(i));
    QCD_stat_err2->SetBinContent(i,fabs(norm*new_bkgd->GetBinContent(i)));
    eemc_stat_err2->SetBinContent(i,eemc_bkgd2->GetBinContent(i));
    tau_stat_err2->SetBinContent(i,tau_norm*taufrac*tauhist->GetBinContent(i));
    zsig_stat_err2->SetBinContent(i,Z_norm*zsig_bkgd2->GetBinContent(i));
    zback_stat_err2->SetBinContent(i,norm*norm*Z_norm*zback_bkgd2->GetBinContent(i));
    //cout << "Error " << i << " " << raw_stat_err2->GetBinCenter(i) << " " << raw_stat_err2->GetBinContent(i) << " " << QCD_stat_err2->GetBinContent(i) << " " << eemc_stat_err2->GetBinContent(i) << " " << tau_stat_err2->GetBinContent(i) << endl;
  }

  // Now go through all the 1200 background shapes and find the
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
      int k=jval;
      //for (int k=0; k<20; k++) {
        if (new_bkgd_hists1[j][k]->GetBinContent(i) < low) {
          if (new_bkgd_hists1[j][k]->GetBinContent(i) >= 0) {
            low = new_bkgd_hists1[j][k]->GetBinContent(i);
          }
        }
        if (new_bkgd_hists1[j][k]->GetBinContent(i) > high) {
          high = new_bkgd_hists1[j][k]->GetBinContent(i);
        }

        if (new_bkgd_hists2[j][k]->GetBinContent(i) < low) {
          if (new_bkgd_hists2[j][k]->GetBinContent(i) >= 0) {
            low = new_bkgd_hists2[j][k]->GetBinContent(i);
          }
        }
        if (new_bkgd_hists2[j][k]->GetBinContent(i) > high) {
          high = new_bkgd_hists2[j][k]->GetBinContent(i);
        }

        if (new_bkgd_hists3[j][k]->GetBinContent(i) < low) {
          if (new_bkgd_hists3[j][k]->GetBinContent(i) >= 0) {
            low = new_bkgd_hists3[j][k]->GetBinContent(i);
          }
        }
        if (new_bkgd_hists3[j][k]->GetBinContent(i) > high) {
          high = new_bkgd_hists3[j][k]->GetBinContent(i);
        }
        //cout << i << " low = " << low << " high = " << high << endl;
      //} // end of k-loop
    } // end of j-loop

    // calculate the sum
    low_bkgd->SetBinContent(i,0.);
    if ((low != 10000) && (new_bkgd->GetBinContent(i)-low > 0)) {
      if ((i >= 13)&&(i<=26)) {low_sum += low;}
      low_bkgd->SetBinContent(i,low);
    }
    if ((i >= 13)&&(i<=26)) {high_sum += high;} 
    high_bkgd->SetBinContent(i,high);
    cout << i << " " << low_sum << " " << high_sum << endl;
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

  cout << "Total low error = " << sqrt((QCD_sum-low_sum)*(QCD_sum-low_sum)+tot_stat*tot_stat) << endl;
  cout << "Total high error = " << sqrt((high_sum-QCD_sum)*(high_sum-QCD_sum)+tot_stat*tot_stat) << endl;

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
    zsig->Write();
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

    for (int i=1; i<=49; i++) {
      cout.setf(ios::fixed);
      cout.precision(2);
      cout << " " << signal2->GetBinCenter(i) << " & " << signal2->GetBinContent(i) << " & " << signal_final3->GetBinContent(i) << " & " << QCD_stat_err2->GetBinContent(i) << " & " << eemc_stat_err2->GetBinContent(i) << " & " << tau_stat_err2->GetBinContent(i) << " & " << zsig_stat_err2->GetBinContent(i) << " & " << zback_stat_err2->GetBinContent(i) << " & " << QCD_syst_high_err->GetBinContent(i) << " & " << QCD_syst_low_err->GetBinContent(i) << " \\\\" << endl;
      
    }
  
    f2->Close();
 
  } else if (two_or_four == 4) {

    TH1F *signal_final3 = new TH1F("signal_final3","signal_final3",49,1.,99.);
    for (int i=1; i<=49; i++) {
      signal_final3->SetBinContent(i,signal_final2->GetBinContent(i));
    }
    signal_final3->Add(new_bkgd,-1.);

    TH1F *tauhist_repack = new TH1F("tauhist_r","tauhist_r",24,3.,99.);
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

    for (int i=1; i<=24; i++) {
      tauhist_repack->SetBinContent(i,tauhist->GetBinContent(2*i)+
                                      tauhist->GetBinContent(2*i+1));;
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
    }

    tauhist_repack->Write();
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

    for (int i=1; i<=24; i++) {
      cout.setf(ios::fixed);
      cout.precision(2);
      cout << " " << signal2_repack->GetBinCenter(i) << " & " << signal2_repack->GetBinContent(i) << " & " << signal_final3_repack->GetBinContent(i) << " & " << QCD_stat_err2_repack->GetBinContent(i) << " & " << eemc_stat_err2_repack->GetBinContent(i) << " & " << tau_stat_err2_repack->GetBinContent(i) << " & " << zsig_stat_err2_repack->GetBinContent(i) << " & " << zback_stat_err2_repack->GetBinContent(i) << " & " << QCD_syst_high_err_repack->GetBinContent(i) << " & " << QCD_syst_low_err_repack->GetBinContent(i) << " \\\\" << endl;
    }

    f2->Close();

  }

  
} // end of macro
