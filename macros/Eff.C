//________________________________________________________________________________
void Eff0() {
  //canvas only needed for this documentation
  TCanvas* c1 = new TCanvas("example","",600,400);
  c1->SetFillStyle(1001);
  c1->SetFillColor(kWhite);
  
  //create one-dimensional TEfficiency object with fixed bin size
  TEfficiency* pEff = new TEfficiency("eff0","my efficiency;x;#epsilon",20,0,10);
  TRandom3 rand;
  
  bool bPassed;
  double x;
  for(int i=0; i<10000; ++i)
    {
      //simulate events with variable under investigation
      x = rand.Uniform(10);
      //check selection: bPassed = DoesEventPassSelection(x)
      bPassed = rand.Rndm() < TMath::Gaus(x,5,4);
      pEff->Fill(bPassed,x);
    }
  
  pEff->Draw("AP");
}
//________________________________________________________________________________
void Eff1(){
  //canvas only needed for the documentation
  TCanvas* c1 = new TCanvas("c1","",600,400);
  c1->Divide(2);
  c1->SetFillStyle(1001);
  c1->SetFillColor(kWhite);

  //create one-dimensional TEfficiency object with fixed bin size
  TEfficiency* pEff = new TEfficiency("eff1","different confidence levels;x;#epsilon",20,0,10);
  TRandom3 rand;
  
  bool bPassed;
  double x;
  for(int i=0; i<1000; ++i)
    {
      //simulate events with variable under investigation
      x = rand.Uniform(10);
      //check selection: bPassed = DoesEventPassSelection(x)
      bPassed = rand.Rndm() < TMath::Gaus(x,5,4);
      pEff->Fill(bPassed,x);
    }
  
  //set style attributes
  pEff->SetFillStyle(3004);
  pEff->SetFillColor(kRed);
  
  //copy current TEfficiency object and set new confidence level
  TEfficiency* pCopy = new TEfficiency(*pEff);
  pCopy->SetConfidenceLevel(0.683);
  
  //set style attributes
  pCopy->SetFillStyle(3005);
  pCopy->SetFillColor(kBlue);
  
  c1->cd(1);
  
  //add legend
  TLegend* leg1 = new TLegend(0.3,0.1,0.7,0.5);
  leg1->AddEntry(pEff,"95%","F");
  leg1->AddEntry(pCopy,"68.3%","F");
  
  pEff->Draw("A4");
  pCopy->Draw("same4");
  leg1->Draw("same");
  
  //use same confidence level but different statistic methods
  TEfficiency* pEff2 = new TEfficiency(*pEff);
  TEfficiency* pCopy2 = new TEfficiency(*pEff);
  
  pEff2->SetStatisticOption(TEfficiency::kFNormal);
  pCopy2->SetStatisticOption(TEfficiency::kFAC);
  
  pEff2->SetTitle("different statistic options;x;#epsilon");
  
  //set style attributes
  pCopy2->SetFillStyle(3005);
  pCopy2->SetFillColor(kBlue);
  
  c1->cd(2);
  
  //add legend
  TLegend* leg2 = new TLegend(0.3,0.1,0.7,0.5);
  leg2->AddEntry(pEff2,"kFNormal","F");
  leg2->AddEntry(pCopy2,"kFAC","F");
  
  pEff2->Draw("a4");
  pCopy2->Draw("same4");
  leg2->Draw("same");
  
  //only for this documentation
  c1->cd(0);
}
//________________________________________________________________________________
void Eff2() {
  //canvas only needed for the documentation
  TCanvas* c1 = new TCanvas("c2","",600,400);
  c1->SetFillStyle(1001);
  c1->SetFillColor(kWhite);
  
  //create different beta distributions
  TF1* f1 = new TF1("f1","TMath::BetaDist(x,1,1)",0,1);
  f1->SetLineColor(kBlue);
  TF1* f2 = new TF1("f2","TMath::BetaDist(x,0.5,0.5)",0,1);
  f2->SetLineColor(kRed);
  TF1* f3 = new TF1("f3","TMath::BetaDist(x,1,5)",0,1);
  f3->SetLineColor(kGreen+3);
  f3->SetTitle("Beta distributions as priors;#epsilon;P(#epsilon)");
  TF1* f4 = new TF1("f4","TMath::BetaDist(x,4,3)",0,1);
  f4->SetLineColor(kViolet);
  
  //add legend
  TLegend* leg = new TLegend(0.25,0.5,0.85,0.89);
  leg->SetFillColor(kWhite);
  leg->SetFillStyle(1001);
  leg->AddEntry(f1,"a=1, b=1","L");
  leg->AddEntry(f2,"a=0.5, b=0.5","L");
  leg->AddEntry(f3,"a=1, b=5","L");
  leg->AddEntry(f4,"a=4, b=3","L");
  
  f3->Draw();
  f1->Draw("same");
  f2->Draw("Same");
  f4->Draw("same");
  leg->Draw("same");
  
  //only for this documentation
}
//________________________________________________________________________________
void Eff3(){
  //canvas only needed for this documentation
  TCanvas* c1 = new TCanvas("example3","",600,400);
  c1->SetFillStyle(1001);
  c1->SetFillColor(kWhite);
  
  //create one-dimensional TEfficiency object with fixed bin size
  TEfficiency* pEff = new TEfficiency("eff3","my efficiency;x;#epsilon",20,0,10);
  TRandom3 rand;
  
  bool bPassed;
  double x;
  for(int i=0; i<10000; ++i)
    {
      //simulate events with variable under investigation
      x = rand.Uniform(10);
      //check selection: bPassed = DoesEventPassSelection(x)
      bPassed = rand.Rndm() < TMath::Gaus(x,5,4);
      pEff->Fill(bPassed,x);
    }
  
  //create a function for fitting and do the fit
  TF1* f1 = new TF1("f1","gaus",0,10);
  f1->SetParameters(1,5,2);
  pEff->Fit(f1);
  
  //create a threshold function
  TF1* f2 = new TF1("thres","0.8",0,10);
  f2->SetLineColor(kRed);
  //add it to the list of functions
  //use add first because the parameters of the last function will be displayed
  pEff->GetListOfFunctions()->AddFirst(f2);
  
  pEff->Draw("AP");
  
  //only for this documentation
}
//________________________________________________________________________________
void Eff() {
  Eff0();
  Eff1();
  Eff2();
  Eff3();
}
