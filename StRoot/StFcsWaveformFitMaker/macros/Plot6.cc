/*
  Author:David Kapukchyan
  @[August 26, 2022]
  > First instance

  A macro to plot the results from testing the StFcsWaveformFitMaker methods using StFcsWaveformFitMaker set to test level 6. The "prefix" is to set the output save file names prefix.

  How to determine the sum8 vs. Fit scale factor.
  1. Run Wff using setTest(6) on a sufficiently large data sample, setFileOutFileName(), and setEnergySumScale(1,1,1)
  2. Take output root file and run this macro giving it the file name and a prefix for the saved image files
  3. Look at prefix_sum8vfit.png to check what values to scale by. slope of fitted line is the value to use
  4. Run Wff on same data except setEnergySumScale(slopecal,slopehcal,slopepres);
  5. Run this macro again and and check same plot. Slopes should be close to 1?

*/

//#include "Rtools.h"
//#include "HistColl2.h"

void Plot6(std::string filename="test.root",std::string prefix="test")
{
  gStyle->SetOptStat(111111);
  gStyle->SetOptDate(0);

  TFile* infile = TFile::Open(filename.c_str());
  if( infile==0 || infile->IsZombie() ){ std::cout << "ERROR:Unable to open: " << filename << std::endl; return 0; }
  else{ std::cout << "Opened file:"<<infile << " Named:"<<infile->GetName() << std::endl; }

  TH1F* npeaks[7] = {0};
  TH1F* npeaksfiltered[7] = {0};
  TH1F* res0[7] = {0};
  TH1F* res0zoom[7] = {0};
  TH1F* sum8res0[7] = {0};
  TH1F* sum8res0zoom[7] = {0};
  TH1F* fitres0[7] = {0};
  TH1F* fitres0zoom[7] = {0};
  TH2F* sum8vfit[7] = {0};
  for( UInt_t i=0; i<7; ++i ){
    char name[100];
    
    sprintf(name,"H1_NPeaks_%d",i);
    npeaks[i] = (TH1F*)infile->Get(name);
    npeaks[i]->SetTitle(";number of peaks");
    
    sprintf(name,"H1_NPeaksFiltered_%d",i);
    npeaksfiltered[i] = (TH1F*)infile->Get(name);
    npeaksfiltered[i]->SetTitle(";number of peaks");

    sprintf(name,"H1_Res0_%d",i);
    res0[i] = (TH1F*)infile->Get(name);
    res0[i]->SetTitle(";ADC sum");
    
    sprintf(name,"H1_Res0Zoom_%d",i);
    res0zoom[i] = (TH1F*)infile->Get(name);
    res0zoom[i]->SetTitle(";ADC sum");
    
    sprintf(name,"H1_Sum8Res0_%d",i);
    sum8res0[i] = (TH1F*)infile->Get(name);
    sum8res0[i]->SetTitle(";ADC sum");
    
    sprintf(name,"H1_Sum8Res0Zoom_%d",i);
    sum8res0zoom[i] = (TH1F*)infile->Get(name);
    sum8res0zoom[i]->SetTitle(";ADC sum");
    
    sprintf(name,"H1_FitRes0_%d",i);
    fitres0[i] = (TH1F*)infile->Get(name);
    fitres0[i]->SetTitle(";ADC sum");
    
    sprintf(name,"H1_FitRes0Zoom_%d",i);
    fitres0zoom[i] = (TH1F*)infile->Get(name);
    fitres0zoom[i]->SetTitle(";ADC sum");

    sprintf(name,"H2_Sum8vFit_%d",i);
    sum8vfit[i] = (TH2F*)infile->Get(name);
    sum8vfit[i]->SetTitle(";Sum8;FitSum");

    res0[i]        ->SetLineColor(kBlack);
    sum8res0[i]    ->SetLineColor(kBlue);
    fitres0[i]     ->SetLineColor(kGreen);
    res0zoom[i]    ->SetLineColor(kBlack);
    sum8res0zoom[i]->SetLineColor(kBlue);
    fitres0zoom[i] ->SetLineColor(kGreen);
    res0[i]        ->SetStats(0);
    sum8res0[i]    ->SetStats(0);
    fitres0[i]     ->SetStats(0);
    res0zoom[i]    ->SetStats(0);
    sum8res0zoom[i]->SetStats(0);
    fitres0zoom[i] ->SetStats(0);
  }
  res0[6]->SetTitle("");
  sum8res0[6]->SetTitle("");
  fitres0[6]->SetTitle("");
  res0zoom[6]->SetTitle("");
  sum8res0zoom[6]->SetTitle("");
  fitres0zoom[6]->SetTitle("");
  
  //Rtools::HistColl1F* npeaks = new Rtools::HistColl1F(infile,"NPeaks",";number of peaks");
  //Rtools::HistColl1F* npeaksfiltered = new Rtools::HistColl1F(infile,"NPeaksFiltered",";number of peaks");
  //Rtools::HistColl1F* res0 = new Rtools::HistColl1F(infile,"Res0",";ADC sum");
  //Rtools::HistColl1F* res0zoom = new Rtools::HistColl1F(infile,"Res0Zoom", ";ADC sum");
  //Rtools::HistColl1F* sum8res0 = new Rtools::HistColl1F(infile,"Sum8Res0", ";ADC sum");
  //Rtools::HistColl1F* sum8res0zoom = new Rtools::HistColl1F(infile,"Sum8Res0Zoom",";ADC sum");
  //Rtools::HistColl1F* fitres0 = new Rtools::HistColl1F(infile,"FitRes0",";ADC sum");
  //Rtools::HistColl1F* fitres0zoom = new Rtools::HistColl1F(infile,"FitRes0Zoom",";ADC sum");

  //Rtools::HistColl2F* sum8vfit = new Rtools::HistColl2F(infile,"Sum8vFit",";Sum8;FitSum");
  //sum8vfit->Print();

  TH1F* timefit = (TH1F*)infile->Get("H1_TimeFitPulse");
  TH1F* measuretime = (TH1F*)infile->Get("FitTime");

  int width = 1280;
  int height = 960;

  //Rtools::DrawTools* dt0 = new Rtools::DrawTools("dt0","",width,height);
  TCanvas* canv = new TCanvas("canv","",width,height);
  npeaks[6]->SetTitle("");
  npeaks[6]->SetStats(0);
  npeaks[6]->GetXaxis()->SetTitle("Found number of peaks");
  npeaks[6]->Draw("hist e");
  canv->SaveAs( (prefix+"_Npeaks.png").c_str() );

  canv->Clear();
  npeaksfiltered[6]->Draw("hist e");
  canv->SaveAs( (prefix+"_NpeaksFiltered.png").c_str() );

  canv->Clear();
  canv->Divide(3,2);
  for( UInt_t i=0; i<6; ++i ){
    canv->cd(i+1);
    npeaks[i]->Draw("hist e");
  }
  canv->SaveAs( (prefix+"_detNpeaks.png").c_str() );

  canv->Clear();
  canv->Divide(3,2);
  for( UInt_t i=0; i<6; ++i ){
    canv->cd(i+1);
    npeaksfiltered[i]->Draw("hist e");
  }
  canv->SaveAs( (prefix+"_detNpeaksFiltered.png").c_str() );

  //Draw sum8 result, fitted result, and final result on top of each other
  canv->Clear();
  //TPad* pad = dt0->DrawCanv(width,height);
  canv->Divide(2,1);
  canv->cd(1)->SetLogy(1);
  res0[6]->Draw("hist e");
  res0[6]->GetXaxis()->SetTitle("Signal Integral");
  sum8res0[6]->Draw("hist e sames");
  fitres0[6]->Draw("hist e sames");
  canv->cd(2)->SetLogy(1);
  res0zoom[6]->Draw("hist e");
  res0zoom[6]->GetXaxis()->SetTitle("Signal Integral");
  sum8res0zoom[6]->Draw("hist e sames");
  fitres0zoom[6]->Draw("hist e sames");
  TLegend* legres = 0;//new TLegend(0.5,0.5,0.99,0.99,"Sum Comparison","nbNDC");
  //Rtools::AddHistStats(legres,res0->At(6));
  //Rtools::AddHistStats(legres,sum8res0->At(6));
  //Rtools::AddHistStats(legres,fitres0->At(6));
  //legres->Draw();
  canv->SaveAs( (prefix+"_res0.png").c_str() );

  canv->Clear();
  canv->Divide(3,2);
  for( UInt_t i=0; i<6; ++i ){
    canv->cd(i+1)->SetLogy(1);
    res0[i]->Draw("hist e");
    sum8res0[i]->Draw("hist e same");
    fitres0[i]->Draw("hist e same");
  }
  canv->SaveAs( (prefix+"_detres0.png").c_str() );

  canv->Clear();
  canv->Divide(3,2);
  for( UInt_t i=0; i<6; ++i ){
    canv->cd(i+1)->SetLogy(1);
    res0zoom[i]->Draw("hist e");
    sum8res0zoom[i]->Draw("hist e same");
    fitres0zoom[i]->Draw("hist e same");
  }
  canv->SaveAs( (prefix+"_detres0zoom.png").c_str() );
  
  //Draw just sum8 and fitted result on top of each other
  canv->Clear();
  canv->Divide(2,1);
  canv->cd(1)->SetLogy(1);
  sum8res0[6]->Draw("hist e");
  sum8res0[6]->GetXaxis()->SetTitle("Signal Integral");
  fitres0[6]->Draw("hist e same");
  fitres0[6]->GetXaxis()->SetTitle("Signal Integral");
  canv->cd(2)->SetLogy(1);
  sum8res0zoom[6]->Draw("hist e");
  sum8res0zoom[6]->GetXaxis()->SetTitle("Signal Integral");
  fitres0zoom[6]->Draw("hist e same");
  fitres0zoom[6]->GetXaxis()->SetTitle("Signal Integral");
  TLegend* legres2 = 0;//new TLegend(0.5,0.5,0.99,0.99,"Comparing Sum Methods","nbNDC");
  //Rtools::AddHistStats(legres2,sum8res0->At(6));
  //Rtools::AddHistStats(legres2,fitres0->At(6));
  //legres2->Draw();
  canv->SaveAs( (prefix+"_s8fit.png").c_str() );

  canv->Clear();
  TF1* linfit[7] = {0};
  linfit[6] = new TF1("linfit","[0]+[1]*x",0,2000);
  canv->SetLogz(1);
  sum8vfit[6]->Fit(linfit[6]);
  sum8vfit[6]->Draw("colz");
  canv->SaveAs( (prefix+"_sum8vfit.png").c_str() );

  canv->Clear();
  canv->Divide(3,2);
  for( UInt_t i=0; i<6; ++i ){
    linfit[i] = new TF1("linfit","[0]+[1]*x",0,2000);
    canv->cd(i+1)->SetLogz(1);
    sum8vfit[i]->Fit(linfit[i]);
    sum8vfit[i]->Draw("colz");
  }
  canv->SaveAs( (prefix+"_detsum8vfit.png").c_str() );


  canv->Clear();
  timefit->SetLineColor(kBlue);
  timefit->GetXaxis()->SetRangeUser(0,100);
  timefit->GetXaxis()->SetTitle("time (ms)");
  timefit->Draw("hist e");
  double timeintegral = timefit->Integral(1,timefit->GetNbinsX());
  gPad->Update();//h1 should be on the current pad otherwise won't work
  TPaveStats* TPS_timefit = timefit->FindObject("stats");
  TPS_timefit->SetName("timefitstats");
  //TPaveStats* TPS_timefit = Rtools::GetStats(timefit,111111);
  //std::cout<< "|TPS:"<<TPS_timefit<< "|TPS_Name:"<<TPS_timefit->GetName() << "|S:"<< TPS_timefit->GetListOfLines()->GetEntries() << std::endl;
  timefit->SetStats(0);
  std::stringstream ss_tinttext;
  ss_tinttext << "Integral = " << timeintegral;
  TLatex* ttimeint = new TLatex(0,0, ss_tinttext.str().c_str() );
  //ttimeint->SetTextFont(42);
  ttimeint->SetTextSize(0.03);
  TPS_timefit->GetListOfLines()->Add(ttimeint);
  TPS_timefit->Draw();
  canv->SaveAs( (prefix+"_timefit.png").c_str() );

  if( measuretime!=0 ){
    canv->Clear();
    canv->SetLogy();
    measuretime->SetLineColor(kBlue);
    //measuretime->GetXaxis()->SetRangeUser(0,100);
    measuretime->GetXaxis()->SetTitle("time (ms)");
    measuretime->Draw("hist e");
    double totaltime = measuretime->Integral(1,measuretime->GetNbinsX());
    gPad->Update();//h1 should be on the current pad otherwise won't work
    TPaveStats* TPS_measuretime = measuretime->FindObject("stats");
    TPS_measuretime->SetName("measuretimestats");
    measuretime->SetStats(0);
    std::stringstream ss_minttext;
    ss_minttext << "Integral = " << totaltime;
    TLatex* mtimeint = new TLatex(0,0, ss_minttext.str().c_str() );
    //mtimeint->SetTextFont(42);
    mtimeint->SetTextSize(0.03);
    TPS_measuretime->GetListOfLines()->Add(mtimeint);
    TPS_measuretime->Draw();
    canv->SaveAs( (prefix+"_measuretime.png").c_str() );
  }

  /*
  delete npeaks;
  delete npeaksfiltered;
  delete res0;
  delete res0zoom;
  delete sum8res0;
  delete sum8res0zoom;
  delete fitres0;
  delete fitres0zoom;
  delete sum8vfit;
  delete timefit;
  delete measuretime;
  delete legres;
  delete legres2;
  */
  infile->Close();
  delete infile;
  delete canv;
  
}

