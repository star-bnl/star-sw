// This macro takes as its input, the output of the
// the makeBkgdFiles.C macro and the MC files for W+ and W-
// and compares/plots the data and MC expectations

void plotMCDataComp_2GeV() {

  TFile *f1 = new TFile("bkgd_histos_pos_output4.root");
  TFile *f2 = new TFile("bkgd_histos_neg_output4.root");
  TFile *f3 = new TFile("rcf10010_atMCscale.wana.hist.root");
  TFile *f4 = new TFile("rcf10011_atMCscale.wana.hist.root");

  TH1F *pos_data = (TH1F*)f1->Get("signal_final3");
  TH1F *neg_data = (TH1F*)f2->Get("signal_final3");
  TH1F *pos_mc   = (TH1F*)f3->Get("muclustPtBal");
  TH1F *neg_mc   = (TH1F*)f4->Get("muclustPtBal");
  TH1F *pos_data_repack = new TH1F("pos_data_repack","pos_data_repack",49,1.,99.);
  TH1F *neg_data_repack = new TH1F("neg_data_repack","neg_data_repack",49,1.,99.);

  // repack the data to 2 GeV wide bins
  for (int i=1; i<=49; i++) {
    pos_data_repack->SetBinContent(i,pos_data->GetBinContent(i));
    neg_data_repack->SetBinContent(i,neg_data->GetBinContent(i));
  }
  
  TH1F *pos_mc_repack = new TH1F("pos_mc_repack","pos_mc_repack",49,1.,99.);
  TH1F *neg_mc_repack = new TH1F("neg_mc_repack","neg_mc_repack",49,1.,99.);
  for (int i=1; i<=49; i++) {
    pos_mc_repack->SetBinContent(i,pos_mc->GetBinContent(2*i-1)+
                                   pos_mc->GetBinContent(2*i));
    neg_mc_repack->SetBinContent(i,neg_mc->GetBinContent(2*i-1)+
                                   neg_mc->GetBinContent(2*i));
  } 

  // Find MC normalization factors to scale by
  // and scale the MC
  float pos_data_cnt = 0., pos_mc_cnt = 0.;
  float neg_data_cnt = 0., neg_mc_cnt = 0.;
  for (int i=13; i<=49; i++) {
    cout << i << " " << pos_data_repack->GetBinContent(i) << endl;
    pos_data_cnt += pos_data_repack->GetBinContent(i);
    neg_data_cnt += neg_data_repack->GetBinContent(i);
    pos_mc_cnt   += pos_mc_repack->GetBinContent(i);
    neg_mc_cnt   += neg_mc_repack->GetBinContent(i);
  }
  pos_mc_repack->Scale(pos_data_cnt/pos_mc_cnt);
  neg_mc_repack->Scale(neg_data_cnt/neg_mc_cnt);
  cout << "Pos Data = " << pos_data_cnt << " and MC = " << pos_mc_cnt << endl;
  cout << "Neg Data = " << neg_data_cnt << " and MC = " << neg_mc_cnt << endl;
  cout << "Norm ratio = " << pos_data_cnt/pos_mc_cnt << endl;

  // *******************************************************
  // Grab the statistical and QCD bkgd shape uncertainties
  // *******************************************************
 
  // Get all the necessary histograms for the calculation
  // of all the uncertainties
  TH1F *pos_raw_stat_err2 = (TH1F*)f1->Get("raw_stat_err2");
  TH1F *pos_QCD_stat_err2 = (TH1F*)f1->Get("QCD_stat_err2");
  TH1F *pos_eemc_stat_err2 = (TH1F*)f1->Get("eemc_stat_err2");
  TH1F *pos_tau_stat_err2 = (TH1F*)f1->Get("tau_stat_err2");
  TH1F *pos_QCD_syst_low_err = (TH1F*)f1->Get("QCD_syst_low_err");
  TH1F *pos_QCD_syst_high_err = (TH1F*)f1->Get("QCD_syst_high_err");

  TH1F *neg_raw_stat_err2 = (TH1F*)f2->Get("raw_stat_err2");
  TH1F *neg_QCD_stat_err2 = (TH1F*)f2->Get("QCD_stat_err2");
  TH1F *neg_eemc_stat_err2 = (TH1F*)f2->Get("eemc_stat_err2");
  TH1F *neg_tau_stat_err2 = (TH1F*)f2->Get("tau_stat_err2");
  TH1F *neg_QCD_syst_low_err = (TH1F*)f2->Get("QCD_syst_low_err");
  TH1F *neg_QCD_syst_high_err = (TH1F*)f2->Get("QCD_syst_high_err");

  float pos_high[49], pos_low[49];
  float neg_high[49], neg_low[49];
  float bin_cent[49], pos_bin_val[49], neg_bin_val[49];
  float ex[49];
  float err;

  // calculate the statistical uncertainty and the background
  // shape systematic uncertainty 
  for (int i=1; i<=49; i++) {
    ex[i-1] = 2.0; 

    err = 0.;
    err += pos_raw_stat_err2->GetBinContent(i);
    err += pos_QCD_stat_err2->GetBinContent(i);
    err += pos_eemc_stat_err2->GetBinContent(i);
    err += pos_tau_stat_err2->GetBinContent(i);
    pos_data_repack->SetBinError(i,sqrt(err));
    
    err = 0.;
    err += neg_raw_stat_err2->GetBinContent(i);
    err += neg_QCD_stat_err2->GetBinContent(i);
    err += neg_eemc_stat_err2->GetBinContent(i);
    err += neg_tau_stat_err2->GetBinContent(i);
    neg_data_repack->SetBinError(i,sqrt(err));

    bin_cent[i-1] = pos_data_repack->GetBinCenter(i);
    pos_bin_val[i-1] = pos_data_repack->GetBinContent(i);
    neg_bin_val[i-1] = neg_data_repack->GetBinContent(i);

    // The QCD background shape systematic errors are
    // put in their arrays
    pos_high[i-1] = pos_QCD_syst_high_err->GetBinContent(i);
    neg_high[i-1] = neg_QCD_syst_high_err->GetBinContent(i);
    pos_low[i-1]  = pos_QCD_syst_low_err->GetBinContent(i);
    neg_low[i-1]  = neg_QCD_syst_low_err->GetBinContent(i);
  }

  // *******************************************************
  // Calculate an energy scale systematic
  // *******************************************************

  TFile *f5 = new TFile("bkgd_histos_pos_energy_up.root");
  TFile *f6 = new TFile("bkgd_histos_pos_energy_down.root");
  TFile *f7 = new TFile("bkgd_histos_neg_energy_up.root");
  TFile *f8 = new TFile("bkgd_histos_neg_energy_down.root");

  TH1F *pos_data_en_up   = (TH1F*)f5->Get("signal_final3");
  TH1F *pos_data_en_down = (TH1F*)f6->Get("signal_final3");
  TH1F *neg_data_en_up   = (TH1F*)f7->Get("signal_final3");
  TH1F *neg_data_en_down = (TH1F*)f8->Get("signal_final3");

  TH1F *pos_data_en_up_repack = new TH1F("pos_data_en_up_repack","pos_data_en_up_repack",49,1.,99.);
  TH1F *pos_data_en_down_repack = new TH1F("pos_data_en_down_repack","pos_data_en_down_repack",49,1.,99.);
  TH1F *neg_data_en_up_repack = new TH1F("neg_data_en_up_repack","neg_data_en_up_repack",49,1.,99.);
  TH1F *neg_data_en_down_repack = new TH1F("neg_data_en_down_repack","neg_data_en_down_repack",49,1.,99.);

  for (int i=1; i<=49; i++) {
    pos_data_en_up_repack->SetBinContent(i,pos_data_en_up->GetBinContent(i));
    pos_data_en_down_repack->SetBinContent(i,pos_data_en_down->GetBinContent(i));
    neg_data_en_up_repack->SetBinContent(i,neg_data_en_up->GetBinContent(i));
    neg_data_en_down_repack->SetBinContent(i,neg_data_en_down->GetBinContent(i));
  }

  for (int i=1; i<=49; i++) {
    cout << "W+ -> " << pos_data_en_up_repack->GetBinContent(i) << " " << pos_data_repack->GetBinContent(i) << " " << pos_data_en_down_repack->GetBinContent(i) << endl;
  }
  for (int i=1; i<=49; i++) {
    cout << "W- -> " << neg_data_en_up_repack->GetBinContent(i) << " " << neg_data_repack->GetBinContent(i) << " " << neg_data_en_down_repack->GetBinContent(i) << endl;
  }

  // Now loop over all the bins of the three histograms (energy low, 
  // normal, energy high) and in each bin find the lowest and the
  // highest and then call that the systematic (normally it will be
  // the two energy scaled data sets) ... and do it for both charges
  float pos_data_ensys_low[49];
  float pos_data_ensys_high[49];
  float neg_data_ensys_low[49];
  float neg_data_ensys_high[49];
  for (int i=1; i<=49; i++) {

    // for positive charge
    float low = 10000.;
    if (pos_data_en_up_repack->GetBinContent(i) < low) {
      low = pos_data_en_up_repack->GetBinContent(i);
    }
    if (pos_data_repack->GetBinContent(i) < low) {
      low = pos_data_repack->GetBinContent(i);
    }
    if (pos_data_en_down_repack->GetBinContent(i) < low) {
      low = pos_data_en_down_repack->GetBinContent(i);
    }
    pos_data_ensys_low[i-1] = fabs(low-pos_data_repack->GetBinContent(i));

    float high = 0.;
    if (pos_data_en_up_repack->GetBinContent(i) > high) {
      high = pos_data_en_up_repack->GetBinContent(i);
    }
    if (pos_data_repack->GetBinContent(i) > high) {
      high = pos_data_repack->GetBinContent(i);
    }
    if (pos_data_en_down_repack->GetBinContent(i) > high) {
      high = pos_data_en_down_repack->GetBinContent(i);
    }
    pos_data_ensys_high[i-1] = fabs(high-pos_data_repack->GetBinContent(i));

    // for negative charge
    low = 10000.;
    if (neg_data_en_up_repack->GetBinContent(i) < low) {
      low = neg_data_en_up_repack->GetBinContent(i);    }
    if (neg_data_repack->GetBinContent(i) < low) {
      low = neg_data_repack->GetBinContent(i);
    }
    if (neg_data_en_down_repack->GetBinContent(i) < low) {
      low = neg_data_en_down_repack->GetBinContent(i);
    }
    neg_data_ensys_low[i-1] = fabs(low-neg_data_repack->GetBinContent(i));
    high = 0.;
    if (neg_data_en_up_repack->GetBinContent(i) > high) {      high = neg_data_en_up_repack->GetBinContent(i);
    }
    if (neg_data_repack->GetBinContent(i) > high) {
      high = neg_data_repack->GetBinContent(i);
    }
    if (neg_data_en_down_repack->GetBinContent(i) > high) {
      high = neg_data_en_down_repack->GetBinContent(i);
    }
    neg_data_ensys_high[i-1] = fabs(high-neg_data_repack->GetBinContent(i));

    cout << i << " " << pos_data_ensys_low[i-1] << " " << pos_data_ensys_high[i-1] << 
" " << neg_data_ensys_low[i-1] << " " << neg_data_ensys_high[i-1] << endl;
 
    // convention is switched (add to the QCD background systematic)
    // assume fully correlated (probably not true)
    pos_high[i-1] += pos_data_ensys_low[i-1];
    pos_low[i-1]  += pos_data_ensys_high[i-1];
    neg_high[i-1] += neg_data_ensys_low[i-1];
    neg_low[i-1]  += neg_data_ensys_high[i-1];
  }

  TGraphAsymmErrors *pos_syst_err_box = new TGraphAsymmErrors(49,bin_cent,pos_bin_val,ex,ex,pos_high,pos_low);
  TGraphAsymmErrors *neg_syst_err_box = new TGraphAsymmErrors(49,bin_cent,neg_bin_val,ex,ex,neg_high,neg_low);

  // ******************************************************
  // Calculate the MC to Data ratios now (even though they
  // are not plotted)
  // ******************************************************

  TH1F *pos_ratio = pos_data_repack->Clone();
  TH1F *neg_ratio = neg_data_repack->Clone();
  float pos_ratio_syst_high[49], pos_ratio_syst_low[49];
  float neg_ratio_syst_high[49], neg_ratio_syst_low[49];
  float pos_ratio_bin_val[49], neg_ratio_bin_val[49];

  for (int i=1; i<=49; i++) {
    if ((pos_mc_repack->GetBinContent(i) > 0)  && (pos_data_repack->GetBinContent(i) > 0)) {
      pos_ratio->SetBinContent(i,pos_data_repack->GetBinContent(i)/
                                 pos_mc_repack->GetBinContent(i));
      pos_ratio->SetBinError(i,pos_data_repack->GetBinError(i)/pos_mc_repack->GetBinContent(i));
      pos_ratio_bin_val[i-1] = pos_data_repack->GetBinContent(i)/
                               pos_mc_repack->GetBinContent(i);
      pos_ratio_syst_high[i-1] = (pos_data_repack->GetBinContent(i)+pos_high[i-1])/pos_mc_repack->GetBinContent(i);
      pos_ratio_syst_low[i-1] = (pos_data_repack->GetBinContent(i)-pos_low[i-1])/pos_mc_repack->GetBinContent(i);
    } else {
      pos_ratio->SetBinContent(i,0.);
      pos_ratio->SetBinError(i,0.);
      pos_ratio_syst_high[i-1] = 0.;
      pos_ratio_syst_low[i-1] = 0.;
      pos_ratio_bin_val[i-1] = 0.;
    }

    if ((neg_mc_repack->GetBinContent(i) > 0) && (neg_data_repack->GetBinContent(i) > 0)) {
      neg_ratio->SetBinContent(i,neg_data_repack->GetBinContent(i)/
                                 neg_mc_repack->GetBinContent(i));
      neg_ratio->SetBinError(i,neg_data_repack->GetBinError(i)/neg_mc_repack->GetBinContent(i));
      neg_ratio_bin_val[i-1] = neg_data_repack->GetBinContent(i)/
                             neg_mc_repack->GetBinContent(i);
      neg_ratio_syst_high[i-1] = (neg_data_repack->GetBinContent(i)+neg_high[i-1])/neg_mc_repack->GetBinContent(i);
      neg_ratio_syst_low[i-1] = (neg_data_repack->GetBinContent(i)-neg_low[i-1])/neg_mc_repack->GetBinContent(i);
    } else {
      neg_ratio->SetBinContent(i,0.);
      neg_ratio->SetBinError(i,0.);
      neg_ratio_syst_high[i-1] = 0.;
      neg_ratio_syst_low[i-1] = 0.;
      neg_ratio_bin_val[i-1] = 0.;
    }
  }

  for (int i=0; i<49; i++) {
    pos_ratio_syst_high[i] -= pos_ratio_bin_val[i];
    pos_ratio_syst_low[i] = pos_ratio_bin_val[i]-pos_ratio_syst_low[i];
    neg_ratio_syst_high[i] -= neg_ratio_bin_val[i];
    neg_ratio_syst_low[i] = neg_ratio_bin_val[i]-neg_ratio_syst_low[i];
  }
  TGraphAsymmErrors *pos_ratio_syst_err_box = new TGraphAsymmErrors(49,bin_cent,pos_ratio_bin_val,ex,ex,pos_ratio_syst_high,pos_ratio_syst_low);
  TGraphAsymmErrors *neg_ratio_syst_err_box = new TGraphAsymmErrors(49,bin_cent,neg_ratio_bin_val,ex,ex,neg_ratio_syst_high,neg_ratio_syst_low);

  // ******************************************************
  // Plots versus ET (no longer the ratios)
  // ******************************************************
  gStyle->SetTitleBorderSize(0);
  gStyle->SetOptDate(0);
  TCanvas *can1 = new TCanvas("can1","can1",0,0,500,500);

  gPad->SetBorderMode(0);
  gPad->SetGridx(0);
  gPad->SetGridy(0);
  pos_mc_repack->GetYaxis()->SetRangeUser(-10.,100.);
  pos_mc_repack->GetXaxis()->SetRangeUser(0.,70.);
  pos_mc_repack->SetLineColor(2);
  pos_mc_repack->SetStats(kFALSE);
  pos_mc_repack->GetYaxis()->SetTitle("Counts");
  pos_mc_repack->GetYaxis()->SetTitleOffset(1.2);
  pos_mc_repack->GetXaxis()->SetTitle("EMC Cluster E_{T} (GeV)");
  pos_mc_repack->SetTitle("");
  pos_mc_repack->SetLineWidth(3.*pos_mc_repack->GetLineWidth());
  pos_data_repack->SetLineWidth(2.*pos_data_repack->GetLineWidth());
  pos_mc_repack->Draw();
  pos_syst_err_box->SetFillColor(14);
  pos_syst_err_box->Draw("e2same");
  pos_mc_repack->Draw("same");
  pos_data_repack->Draw("same");
  TLegend *leg1 = new TLegend(0.5,0.7,0.88,0.88);
  leg1->SetFillColor(0);
  leg1->SetBorderSize(0);
  leg1->AddEntry(pos_mc_repack,"MC Norm. to Signal","l");
  leg1->AddEntry(pos_data_repack,"Signal","le");
  leg1->AddEntry(pos_syst_err_box,"Syst. Unc.","f");
  leg1->Draw("same");
  TLatex *lat1 = new TLatex();
  lat1->SetNDC();
  lat1->DrawLatex(0.14,0.84,"p+p#rightarrow W^{+}#rightarrow e^{+}+#nu");
  lat1->DrawLatex(0.14,0.74,"Run9 STAR");
  lat1->DrawLatex(0.14,0.69,"Preliminary");
  lat1->DrawLatex(0.14,0.64,"#sqrt{s}=500 GeV");
  can1->Print("data_mc_ET_comp_output4_W+_2GeV.eps");

  TCanvas *can2 = new TCanvas("can2","can2",0,0,500,500);
  gPad->SetBorderMode(0); 
  gPad->SetGridx(0);
  gPad->SetGridy(0);
  neg_mc_repack->GetYaxis()->SetRangeUser(-5.,50.);
  neg_mc_repack->GetXaxis()->SetRangeUser(0.,70.);
  neg_mc_repack->SetLineColor(2); 
  neg_mc_repack->SetStats(kFALSE);
  neg_mc_repack->GetYaxis()->SetTitle("Counts");
  neg_mc_repack->GetYaxis()->SetTitleOffset(1.2);
  neg_mc_repack->GetXaxis()->SetTitle("EMC Cluster E_{T} (GeV)");
  neg_mc_repack->SetTitle("");
  neg_mc_repack->SetLineWidth(3.*neg_mc_repack->GetLineWidth());
  neg_data_repack->SetLineWidth(2.*neg_data_repack->GetLineWidth());
  neg_mc_repack->Draw();
  neg_syst_err_box->SetFillColor(14);
  neg_syst_err_box->Draw("e2same");
  neg_mc_repack->Draw("same");
  neg_data_repack->Draw("same"); 
  leg1->Draw("same");
  lat1->DrawLatex(0.14,0.84,"p+p#rightarrow W^{-}#rightarrow e^{-}+#bar{#nu}");
  lat1->DrawLatex(0.14,0.74,"Run9 STAR");
  lat1->DrawLatex(0.14,0.69,"Preliminary");
  lat1->DrawLatex(0.14,0.64,"#sqrt{s}=500 GeV");
  can2->Print("data_mc_ET_comp_output4_W-_2GeV.eps");

} // end of macro
