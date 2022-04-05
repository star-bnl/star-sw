void getSpectrumDAU(const char *pid)
{
  gStyle->SetOptStat(0);
  gStyle->SetOptFit(0);

  gStyle->SetErrorX(0);

  Bool_t subtractNEUTRONS=kTRUE;

  //neutron data:
  //hadron data for Levy function, nucl-ex/0601033:
  //-->  d^2N/(2*pi*pT*N*dpT*dy) = B/((1+((mT - m0)/nT))^n)
  // {p-dAu; pbar-dAu; p-pp; pbar-pp} and m0 = m_neutron = 1.0 GeV.
  Double_t B[]={0.3,0.23,0.072,0.061};//->[0]
  //Double_t eB[]={0.01,0.01,0.005,0.005};
  Double_t T[]={0.205,0.215,0.179,0.173};//->[1]
  //Double_t eT[]={0.004,0.005,0.006,0.006};
  Double_t n[]={11.00,12.55,10.87,10.49};//->[2]
  //Double_t en[]={0.29,0.41,0.43,0.40};
  TF1 *f_antineutron=new TF1("f_antineutron","[0]/pow((1.+(sqrt(x*x+1.) - 1.)/([1]*[2])),[2])",0.,15.);
  f_antineutron->SetParameters(B[1],T[1],n[1]);

  //get direct gammas pQCD:
  ifstream pQCDphotons("./datapoints/pQCD_Werner/rhic_cteq6_gamma_inv_sc1.dat");
  Float_t ppx[100];
  Float_t ppy[100];
  Int_t iii=0;
  cout<<"pqcd photons:"<<endl;
  while(iii<28){
    if(!pQCDphotons.good()) break;
    Float_t dummy=0.;
    pQCDphotons>>ppx[iii]>>dummy>>dummy>>ppy[iii];
    ppy[iii]*=1.e-09*7.5/42.;//convert to mb * Nbin / sigma_inel
    iii++;
  }
  TGraph *g_dirgamma=new TGraph(iii,ppx,ppy);
  //get direct gammas pQCD: scale 0.5*pT
  ifstream pQCDphotons05("./datapoints/pQCD_Werner/rhic_cteq6_gamma_inv_sc05.dat");
  Float_t ppx05[100];
  Float_t ppy05[100];
  Int_t iii05=0;
  while(iii05<28){
    if(!pQCDphotons05.good()) break;
    Float_t dummy=0.;
    pQCDphotons05>>ppx05[iii05]>>dummy>>dummy>>ppy05[iii05];
    ppy05[iii05]*=1.e-09*7.5/42.;//convert to mb
    iii05++;
  }
  TGraph *g_dirgamma05=new TGraph(iii05,ppx05,ppy05);
  //get direct gammas pQCD: scale 2*pT
  ifstream pQCDphotons2("./datapoints/pQCD_Werner/rhic_cteq6_gamma_inv_sc2.dat");
  Float_t ppx2[100];
  Float_t ppy2[100];
  Int_t iii2=0;
  while(iii2<28){
    if(!pQCDphotons2.good()) break;
    Float_t dummy=0.;
    pQCDphotons2>>ppx2[iii2]>>dummy>>dummy>>ppy2[iii2];
    ppy2[iii2]*=1.e-09*7.5/42.;//convert to mb  
    iii2++;
  }
  TGraph *g_dirgamma2=new TGraph(iii2,ppx2,ppy2);

  //get phenix pions in dau
  ifstream phenix("./datapoints/phenix.dat");
  Float_t phex[100];
  Float_t phey[100];
  Float_t ephex[100];
  Float_t ephey[100];
  Int_t iphe=0;
  while(iphe<18){
    if(!phenix.good()) break;
    phenix>>phex[iphe]>>phey[iphe]>>ephey[iphe];
    ephex[iphe]=0.;
    iphe++;
  }
  TGraphErrors *phenix_dau=new TGraphErrors(iphe,phex,phey,ephex,ephey);
  phenix_dau->SetMarkerStyle(24);
  phenix_dau->SetName("phenix_dau");

  //get pions KKP  scale pT
  ifstream pQCDpions("./datapoints/pQCD_Werner/klaus_pi0inv_200_kkp_1.dat");
  Float_t pionx[100];
  Float_t piony[100];
  Int_t ipion=0;
  while(ipion<28){
    if(!pQCDpions.good()) break;
    pQCDpions>>pionx[ipion]>>piony[ipion];
    ipion++;
  }
  TGraphErrors *kkp=new TGraphErrors(ipion,pionx,piony);
  kkp->SetLineColor(54);
  kkp->SetName("kkp");

  //get pions KKP  scale 0.5*pT
  ifstream pQCDpions05("./datapoints/pQCD_Werner/klaus_pi0inv_200_kkp_05.dat");
  Float_t pionx05[100];
  Float_t piony05[100];
  Int_t ipion05=0;
  while(ipion05<28){
    if(!pQCDpions05.good()) break;
    pQCDpions05>>pionx05[ipion05]>>piony05[ipion05];
    ipion05++;
  }
  TGraphErrors *kkp05=new TGraphErrors(ipion05,pionx05,piony05);
  kkp05->SetLineStyle(2);
  kkp05->SetLineColor(54);
  kkp05->SetName("kkp05");

  //get pions KKP  scale 2*pT
  ifstream pQCDpions2("./datapoints/pQCD_Werner/klaus_pi0inv_200_kkp_2.dat");
  Float_t pionx2[100];
  Float_t piony2[100];
  Int_t ipion2=0;
  while(ipion2<28){
    if(!pQCDpions2.good()) break;
    pQCDpions2>>pionx2[ipion2]>>piony2[ipion2];
    ipion2++;
  }
  TGraphErrors *kkp2=new TGraphErrors(ipion2,pionx2,piony2);
  kkp2->SetLineStyle(2);
  kkp2->SetLineColor(54);
  kkp2->SetName("kkp2");

  TFile *f_decaybg=new TFile("~/MyDecay/gammaDecayDAUSum.root","OPEN");
  TH1F *h_decaybg=(TH1F*)f_decaybg->Get("gamma");
  TH1F *h_decaypion=(TH1F*)f_decaybg->Get("gamma_pion");
  TF1 *fit_decay=new TF1("fit_decay","[0]/pow(x,[1])+[2]",.3,15.);
  TF1 *fit_piondecay=new TF1(*fit_decay);
  fit_decay->SetParameters(1.,1.,.5);
  fit_piondecay->SetParameters(1.,1.,.5);
  h_decaybg->Fit(fit_decay,"R0");
  h_decaypion->Fit(fit_piondecay,"R0Q");

  TCanvas *c_test=new TCanvas();
  h_decaybg->Draw();
  fit_decay->Draw("same");
  c_test->SaveAs("test.eps");

  //take ratio gamma_direct/pion and divide by gamma/pi and then +1:
  for(Int_t i=0;i<iii;i++){
    ppy[i]=ppy[i]/piony[i];
    ppy[i]=ppy[i]/fit_decay->Eval(ppx[i]);
    ppy[i]+=1.;
    ppy05[i]=ppy05[i]/piony05[i];
    ppy05[i]=ppy05[i]/fit_decay->Eval(ppx05[i]);
    ppy05[i]+=1.;
    ppy2[i]=ppy2[i]/piony2[i];
    ppy2[i]=ppy2[i]/fit_decay->Eval(ppx2[i]);
    ppy2[i]+=1.;
  }
  TGraphErrors *g_photonpqcd=new TGraphErrors(iii,ppx,ppy);
  g_photonpqcd->SetName("g_photonpqcd");
  TGraphErrors *g_photonpqcd05=new TGraphErrors(iii05,ppx05,ppy05);
  g_photonpqcd05->SetName("g_photonpqcd05");
  TGraphErrors *g_photonpqcd2=new TGraphErrors(iii2,ppx2,ppy2);
  g_photonpqcd2->SetName("g_photonpqcd2");

  //set outputfiles
  TString dir("/star/u/russcher/gamma/analysis/output/dAu/");
  dir.Append(pid);
  dir.Append("/");
  dir.Append(pid);
  TString psout=dir;
  TString psout_eta=dir;
  TString eFile=dir;
  TString eFileGamma=dir;
  TString pi0File=dir;
  TString nbarFile=dir;
   
  eFile.Append("pion_eff.root");
  eFileGamma.Append("gamma_eff.root");
  pi0File.Append("pi0_dAu.root");
  nbarFile.Append("antineutron_eff.root");
  psout_eta.Append("/dev/null");
  psout.Append("invmassplots.ps");

  //load *.so
  gSystem->Load("$HOME/MyEvent/MyEvent");
  gSystem->Load("$HOME/gamma/analysis/lib/AnaCuts");
  gSystem->Load("$HOME/gamma/analysis/lib/EventMixer");
  gSystem->Load("$HOME/gamma/analysis/lib/Pi0Analysis");


  AnaCuts *cuts=new AnaCuts("dAu");

  //get inv mass hists
  TFile f(pi0File.Data(),"OPEN");
  TH2F *h_mb=new TH2F(*h_minvMB);
  TH2F *h_ht1=new TH2F(*h_minvHT1);
  TH2F *h_ht2=new TH2F(*h_minvHT2);
  TH1F *h_ev=new TH1F(*h_events);

  //get anti-neutron
  if(0){
    TFile *file_nbar=new TFile(nbarFile,"OPEN");
    TH1F *h_nbarEffMB=new TH1F(*h_effMB);
    h_nbarEffMB->Sumw2();
    TH1F *h_nbarEffHT1=new TH1F(*h_effHT1);
    h_nbarEffHT1->Sumw2();
    TH1F *h_nbarEffHT2=new TH1F(*h_effHT2);
    h_nbarEffHT2->Sumw2();
  }
  //get prescales
  int trigger=0;
  Int_t numberOfMB=0;
  Int_t numberOfHT1=0;
  Int_t numberOfHT2=0;
  for(Int_t i=1;i<=h_ev->GetNbinsX();i++)
    {
      trigger=(Int_t)h_ev->GetBinCenter(i);
      if(trigger&1) numberOfMB+=(Int_t)h_ev->GetBinContent(i);
      if(trigger&2) numberOfHT1+=(Int_t)h_ev->GetBinContent(i);
      if(trigger&4) numberOfHT2+=(Int_t)h_ev->GetBinContent(i);
    }

  cout<<"number of mb: "<<numberOfMB<<endl;
  numberOfMB/=0.93;
  cout<<"nmb after 93% vertex eff.: "<<numberOfMB<<endl;

  Float_t psMB=383.;
  Float_t psHT1=9.65;
  Float_t psHT2=1.;

  //get efficiencies+acceptance
  TFile g(eFile.Data(),"OPEN");
  TH1F *h_emb=new TH1F(*h_effMB);
  TH1F *h_eht1=new TH1F(*h_effHT1);
  TH1F *h_eht2=new TH1F(*h_effHT2);

  //corrections, all multiplicative, in case of
  //pions:
  TF1 *pion_cpv_corrMB=new TF1("pion_cpv_corrMB","1./(1.-0.01*(0.4+0.05*x))",0.,15.);
  TF1 *pion_cpv_corrHT1=new TF1("pion_cpv_corrHT1","1./(1.-0.01*(0.4+0.07*x))",0.,15.);
  TF1 *pion_cpv_corrHT2=new TF1("pion_cpv_corrHT2","1./(1.-0.01*(0.5+0.06*x))",0.,15.);
  //photons:
  TF1 *gamma_cpv_corrMB=new TF1("gamma_cpv_corrMB","1./(1.-0.01*(4.1+0.4*x))",0.,15.);
  TF1 *gamma_cpv_corrHT1=new TF1("gamma_cpv_corrHT1","1./(1.-0.01*(3.4+0.5*x))",0.,15.);
  TF1 *gamma_cpv_corrHT2=new TF1("gamma_cpv_corrHT2","1./(1.-0.01*(4.9+0.3*x))",0.,15.);
  TF1 *gamma_cont_corrMB=new TF1("gamma_cont_corrMB","0.98",0.,15.);
  TF1 *gamma_cont_corrHT1=new TF1("gamma_cont_corrHT1","0.98",0.,15.);
  TF1 *gamma_cont_corrHT2=new TF1("gamma_cont_corrHT2","0.965",0.,15.);

  //missing material
  //pions:
  TF1 *pion_conv_corrMB=new TF1("pion_conv_corrMB","1.15",0.,15.);
  TF1 *pion_conv_corrHT1=new TF1("pion_conv_corrHT1","1.15",0.,15.);
  TF1 *pion_conv_corrHT2=new TF1("pion_conv_corrHT2","1.15",0.,15.);
  //photons:
  TF1 *gamma_conv_corrMB=new TF1("gamma_conv_corrMB","1.08",0.,15.);
  TF1 *gamma_conv_corrHT1=new TF1("gamma_conv_corrHT1","1.08",0.,15.);
  TF1 *gamma_conv_corrHT2=new TF1("gamma_conv_corrHT2","1.08",0.,15.);

  //bin corrections
  TFile binf("~/BinWidth/bincorrectionsDAU.root","OPEN"); 
  TH1F *h_binmb=new TH1F(*h4mb);
  TH1F *h_binht1=new TH1F(*h4ht1);
  TH1F *h_binht2=new TH1F(*h4ht2);
  h_binmb->Sumw2();
  h_binht1->Sumw2();
  h_binht2->Sumw2();
  for(Int_t i=1;i<=h_binmb->GetNbinsX();i++) h_binmb->SetBinError(i,0);
  for(Int_t i=1;i<=h_binht1->GetNbinsX();i++) h_binht1->SetBinError(i,0);
  for(Int_t i=1;i<=h_binht2->GetNbinsX();i++) h_binht2->SetBinError(i,0);

  //get yield
  Pi0Analysis *pi0=new Pi0Analysis(psout.Data(),psout_eta.Data(),"dAu");
  pi0->init("/dev/null");
  TH1F *pionYieldMB=new TH1F(*pi0->getYield(h_mb,"mb"));
  TH1F *pionYieldHT1=new TH1F(*pi0->getYield(h_ht1,"ht1"));
  TH1F *pionYieldHT2=new TH1F(*pi0->getYield(h_ht2,"ht2"));
  pi0->storeCanvases((dir+"canvases.root").Data());

  cout<<"***************************************"<<endl;
  cout<<"got yield, dividing by rapidity bite!!!"<<endl;
  float dy_gamma=cuts->rapidityMaxCUT - cuts->rapidityMinCUT;
  float dy_pion=cuts->rapPionMaxCUT - cuts->rapPionMinCUT;
  cout<<"***************************************"<<endl;
  cout<<endl;
  cout<<"    pion bite is "<<dy_pion<<endl;
  cout<<"    gamma bite is "<<dy_gamma<<endl;
  cout<<endl;
  cout<<"***************************************"<<endl;

  pionYieldMB->Scale(1./dy_pion);
  pionYieldHT1->Scale(1./dy_pion);
  pionYieldHT2->Scale(1./dy_pion);

  //set yield to zero
  /*
  for(Int_t i=0;i<pionYieldHT2->GetNbinsX();i++)
    {
      if(i<1)
	{
	  pionYieldMB->SetBinContent(i+1,0);
	  pionYieldMB->SetBinError(i+1,0);
	}
      if(i<4)
        {
          pionYieldHT1->SetBinContent(i+1,0);
          pionYieldHT1->SetBinError(i+1,0);
	}
      if(i>10)
	{
	  pionYieldHT1->SetBinContent(i+1,0);
          pionYieldHT1->SetBinError(i+1,0);
	}
      if(i<6) 
	{
	  pionYieldHT2->SetBinContent(i+1,0);
          pionYieldHT2->SetBinError(i+1,0);
	}
    }
  */

  //set colors:
  pionYieldMB->SetMarkerStyle(8);
  pionYieldMB->SetMarkerSize(1.0);
  pionYieldHT1->SetMarkerStyle(8);
  pionYieldHT1->SetMarkerSize(1.0); 
  pionYieldHT1->SetMarkerColor(4);
  pionYieldHT2->SetMarkerStyle(8);
  pionYieldHT2->SetMarkerSize(1.0);
  pionYieldHT2->SetMarkerColor(2);
  
  TF1 *scale=new TF1("scale","x",0.,15.);

  pionYieldMB->SetNameTitle("pionYieldMB","corrected yield MB");
  pionYieldMB->Divide(h_emb);
  pionYieldMB->Scale(psMB/(psMB*numberOfMB*2.*TMath::Pi()));
  pionYieldMB->Divide(scale);
  pionYieldMB->Multiply(h_binmb);
  pionYieldMB->Multiply(pion_cpv_corrMB);
  pionYieldMB->Multiply(pion_conv_corrMB);

  pionYieldHT1->SetNameTitle("pionYieldHT1","corrected yield HT1");
  pionYieldHT1->Divide(h_eht1);
  pionYieldHT1->Scale(psHT1/(psMB*numberOfMB*2.*TMath::Pi()));
  pionYieldHT1->Divide(scale);
  pionYieldHT1->Multiply(h_binht1);
  pionYieldHT1->Multiply(pion_cpv_corrHT1);
  pionYieldHT1->Multiply(pion_conv_corrHT1);

  pionYieldHT2->SetNameTitle("pionYieldHT2","corrected yield HT2");
  pionYieldHT2->Divide(h_eht2);
  pionYieldHT2->Scale(psHT2/(psMB*numberOfMB*2.*TMath::Pi()));
  pionYieldHT2->Divide(scale);
  pionYieldHT2->Multiply(h_binht2);
  pionYieldHT2->Multiply(pion_cpv_corrHT2);
  pionYieldHT2->Multiply(pion_conv_corrHT2);

  //create pion yield for double ratio:
  TH1F *pionYieldMBratio=new TH1F(*pionYieldMB);
  TH1F *pionYieldHT1ratio=new TH1F(*pionYieldHT1);
  TH1F *pionYieldHT2ratio=new TH1F(*pionYieldHT2);
  
  TH1F *pionXsMB=new TH1F(*pionYieldMB);
  TH1F *pionXsHT1=new TH1F(*pionYieldHT1);
  TH1F *pionXsHT2=new TH1F(*pionYieldHT2);

  TH1F *pionXsMBnoErr=new TH1F(*pionXsMB);
  TH1F *pionXsHT1noErr=new TH1F(*pionXsHT1);
  TH1F *pionXsHT2noErr=new TH1F(*pionXsHT2);
  for(int i=1;i<=pionXsMBnoErr->GetNbinsX();i++){
    pionXsMBnoErr->SetBinError(i,0.);
  }
  for(int i=1;i<=pionXsHT1noErr->GetNbinsX();i++){
    pionXsHT1noErr->SetBinError(i,0.);
  }
  for(int i=1;i<=pionXsHT2noErr->GetNbinsX();i++){
    pionXsHT2noErr->SetBinError(i,0.);
  }
  
  TGraphErrors *g_pionXsMB=new TGraphErrors(pionXsMB);
  g_pionXsMB->SetName("g_pionXsMB");
  removeThesePoints(g_pionXsMB,1); 

  TGraphErrors *g_pionXsHT1=new TGraphErrors(pionXsHT1);
  g_pionXsHT1->SetName("g_pionXsHT1");
  removeThesePoints(g_pionXsHT1,2);

  TGraphErrors *g_pionXsHT2=new TGraphErrors(pionXsHT2);
  g_pionXsHT2->SetName("g_pionXsHT2");
  removeThesePoints(g_pionXsHT2,3);

  if(0){
    cout<<endl<<"yield: x  y  ex  ey"<<endl;
    cout<<"minbias"<<endl;
    printPoints(g_pionXsMB);
    cout<<endl<<"hightower-1"<<endl;
    printPoints(g_pionXsHT1);
    cout<<endl<<"hightower-2"<<endl;
    printPoints(g_pionXsHT2);
    cout<<endl;
  }
  

  TMultiGraph *m_pions_fit=new TMultiGraph();
  m_pions_fit->SetName("m_pions_fit");
  m_pions_fit->SetMinimum(1.0e-10);
  m_pions_fit->SetMaximum(.1);


  m_pions_fit->Add(g_pionXsMB);
  m_pions_fit->Add(g_pionXsHT1);
  m_pions_fit->Add(g_pionXsHT2);

  TF1 *fitQCD=new TF1("fitQCD",sumpqcd,1.,15.,6);
  fitQCD->SetParameters(100.,-9.,1.,-8.5,1.,6.);
  fitQCD->FixParameter(4,2.);
  m_pions_fit->Fit(fitQCD,"R");
  
  bool inclPhenix=true;
  bool inclSasha=false;

  TCanvas *compare=new TCanvas("compare","compare;p_{T}:xsec (mb)",600,750);
  compare->cd();

  TPad *padt=new TPad("padt","",0.0,0.3,1.,1.0);
  padt->SetBottomMargin(0.001);
  padt->SetLeftMargin(0.15);
  TPad *padb=new TPad("padb","",0.0,0.0,1.,0.3);
  padb->SetTopMargin(0.001);
  padb->SetBottomMargin(0.25);
  padb->SetLeftMargin(0.15);

  padt->Draw();
  padt->cd();
  gPad->SetLogy();

  TMultiGraph *m_pions=new TMultiGraph();
  m_pions->SetName("m_pions");

  if(inclSasha){
    //
  }
  if(inclPhenix){
    m_pions->Add(phenix_dau);
  }

  g_pionXsMB->Print();
  cout<<endl<<endl;
  g_pionXsHT1->Print();
  cout<<endl<<endl;
  g_pionXsHT2->Print();
  cout<<endl;


  m_pions->Add(g_pionXsMB);
  m_pions->Add(g_pionXsHT1);
  m_pions->Add(g_pionXsHT2);

  m_pions->SetMinimum(5.0e-10);
  m_pions->SetMaximum(.099);
  m_pions->SetTitle(";p_{T} (GeV/c);invariant yield");
  m_pions->Draw("ap");

  //fitQCD->Draw("same");

  TLegend *leg=new TLegend(.5,.5,.85,.85);

  if(inclPhenix) leg->AddEntry(phenix_dau,"PHENIX d+Au","p");
  if(inclSasha) leg->AddEntry(sasha_dau,"sasha's d+Au","p");
  leg->AddEntry(g_pionXsMB,"d+Au minimum bias","p");
  leg->AddEntry(g_pionXsHT1,"hightower 1","p");
  leg->AddEntry(g_pionXsHT2,"hightower 2","p");

  leg->SetFillColor(0);
  leg->Draw();

  compare->cd();
  padb->Draw();
  padb->cd();

  TGraphErrors *phenix_dau2=new TGraphErrors(*phenix_dau);
  divideGraphWithFunction(phenix_dau2,fitQCD);
  TGraphErrors *g_pionXsMBcopy=new TGraphErrors(*g_pionXsMB);
  divideGraphWithFunction(g_pionXsMBcopy,fitQCD);
  TGraphErrors *g_pionXsHT1copy=new TGraphErrors(*g_pionXsHT1);
  divideGraphWithFunction(g_pionXsHT1copy,fitQCD);
  TGraphErrors *g_pionXsHT2copy=new TGraphErrors(*g_pionXsHT2);
  divideGraphWithFunction(g_pionXsHT2copy,fitQCD);

  TMultiGraph *m_pions2=new TMultiGraph();
  //if(inclSasha) m_pions2->Add(sasha_dau2);
  if(inclPhenix) m_pions2->Add(phenix_dau2);
  m_pions2->Add(g_pionXsMBcopy);
  m_pions2->Add(g_pionXsHT1copy);
  m_pions2->Add(g_pionXsHT2copy);
  m_pions2->SetMinimum(0.01);
  m_pions2->SetMaximum(1.99);
  m_pions2->Draw("ap");

  compare->SaveAs((dir+"pionyield_dau.eps").Data());
  compare->SaveAs((dir+"pionyield_dau.root").Data());


  //********************************************
  //   Get double ratio:
  //********************************************

  //pion decay photon eff:
  TFile gg(eFile.Data(),"OPEN");  
  TH1F *effGammaMB=new TH1F(*h_effDaughtersMB);
  TH1F *effGammaHT1=new TH1F(*h_effDaughtersHT1);
  TH1F *effGammaHT2=new TH1F(*h_effDaughtersHT2);
  
  //single photon eff:
  TFile gg_single(eFileGamma.Data(),"OPEN");
  TH1F *effGammaSingleMB=new TH1F(*h_effMB);
  TH1F *effGammaSingleHT1=new TH1F(*h_effHT1);
  TH1F *effGammaSingleHT2=new TH1F(*h_effHT2);

  //raw neutral clusters:
  TFile ff(pi0File.Data(),"OPEN");
  TH1F *gammaYieldMB=new TH1F(*h_gammaMB);
  TH1F *gammaYieldHT1=new TH1F(*h_gammaHT1);
  TH1F *gammaYieldHT2=new TH1F(*h_gammaHT2);

  //divide rap. bite:
  gammaYieldMB->Scale(1./dy_gamma);
  gammaYieldHT1->Scale(1./dy_gamma);
  gammaYieldHT2->Scale(1./dy_gamma);


  for(Int_t i=1;i<=gammaYieldMB->GetNbinsX();i++){
    gammaYieldMB->SetBinContent(i,gammaYieldMB->GetBinContent(i)/gammaYieldMB->GetXaxis()->GetBinWidth(i));
    gammaYieldMB->SetBinError(i,gammaYieldMB->GetBinError(i)/gammaYieldMB->GetXaxis()->GetBinWidth(i));
  }
  gammaYieldMB->Scale(psMB/(psMB*numberOfMB*2.*TMath::Pi()));
  gammaYieldMB->Divide(scale);// ../pT
  gammaYieldMB->Multiply(h_binmb);
  gammaYieldMB->Divide(effGammaMB);
  gammaYieldMB->Multiply(gamma_cpv_corrMB);
  gammaYieldMB->Multiply(gamma_cont_corrMB);
  gammaYieldMB->Multiply(gamma_conv_corrMB);

  gammaYieldMB->SetMarkerStyle(8);
  gammaYieldMB->SetMarkerSize(1.);
  TGraphErrors *g_inclPhotonsMB=new TGraphErrors(gammaYieldMB);


  for(Int_t i=1;i<=gammaYieldHT1->GetNbinsX();i++){
    gammaYieldHT1->SetBinContent(i,gammaYieldHT1->GetBinContent(i)/gammaYieldHT1->GetXaxis()->GetBinWidth(i));
    gammaYieldHT1->SetBinError(i,gammaYieldHT1->GetBinError(i)/gammaYieldHT1->GetXaxis()->GetBinWidth(i));
  }
  gammaYieldHT1->Scale(psHT1/(psMB*numberOfMB*2.*TMath::Pi()));
  gammaYieldHT1->Divide(scale);// ../pT
  gammaYieldHT1->Multiply(h_binht1);
  gammaYieldHT1->Divide(effGammaHT1);
  gammaYieldHT1->Multiply(gamma_cpv_corrHT1);
  gammaYieldHT1->Multiply(gamma_cont_corrHT1);
  gammaYieldHT1->Multiply(gamma_conv_corrHT1);

  gammaYieldHT1->SetMarkerStyle(8);
  gammaYieldHT1->SetMarkerSize(1.);
  gammaYieldHT1->SetMarkerColor(4);
  TGraphErrors *g_inclPhotonsHT1=new TGraphErrors(gammaYieldHT1);


  for(Int_t i=1;i<=gammaYieldHT2->GetNbinsX();i++){
    gammaYieldHT2->SetBinContent(i,gammaYieldHT2->GetBinContent(i)/gammaYieldHT2->GetXaxis()->GetBinWidth(i));
    gammaYieldHT2->SetBinError(i,gammaYieldHT2->GetBinError(i)/gammaYieldHT2->GetXaxis()->GetBinWidth(i));
  }
  gammaYieldHT2->Scale(psHT2/(psMB*numberOfMB*2.*TMath::Pi()));
  gammaYieldHT2->Divide(scale);// ../pT
  gammaYieldHT2->Multiply(h_binht2);
  gammaYieldHT2->Divide(effGammaHT2);
  gammaYieldHT2->Multiply(gamma_cpv_corrHT2);
  gammaYieldHT2->Multiply(gamma_cont_corrHT2);
  gammaYieldHT2->Multiply(gamma_conv_corrHT2);

  gammaYieldHT2->SetMarkerStyle(8);
  gammaYieldHT2->SetMarkerSize(1.);
  gammaYieldHT2->SetMarkerColor(2);
  TGraphErrors *g_inclPhotonsHT2=new TGraphErrors(gammaYieldHT2);


  removeThesePoints(g_inclPhotonsMB,1);
  removeThesePoints(g_inclPhotonsHT1,2);
  removeThesePoints(g_inclPhotonsHT2,3);


  TMultiGraph *m_incl=new TMultiGraph();
  m_incl->SetTitle("inclusive photon invariant yield 0<y<1;p_{T} (GeV/c);#frac{1}{2#piNp_{T}} #frac{d^{2}N}{dydp_{T}}");
  m_incl->Add(g_inclPhotonsMB);
  m_incl->Add(g_inclPhotonsHT1);
  m_incl->Add(g_inclPhotonsHT2);

  m_incl->Fit(fitQCD,"R0");

  m_incl->SetMinimum(1.e-11);
  m_incl->SetMaximum(10.);
  TCanvas *c_incl=new TCanvas("c_incl","c_incl",600,400);
  gPad->SetLogy();
  m_incl->Draw("ap");
  c_incl->SaveAs((dir+"inclPhotonYield.eps").Data());
  c_incl->SaveAs((dir+"inclPhotonYield.root").Data());



  //get ratio:
  TH1F *gammaYieldMBratio=new TH1F(*gammaYieldMB);
  gammaYieldMBratio->SetName("gammaYieldMBratio");
  TH1F *gammaYieldHT1ratio=new TH1F(*gammaYieldHT1);
  gammaYieldHT1ratio->SetName("gammaYieldHT1ratio");
  TH1F *gammaYieldHT2ratio=new TH1F(*gammaYieldHT2);
  gammaYieldHT2ratio->SetName("gammaYieldHT2ratio");

  gammaYieldMBratio->Divide(pionYieldMBratio);
  gammaYieldHT1ratio->Divide(pionYieldHT1ratio);
  gammaYieldHT2ratio->Divide(pionYieldHT2ratio);

  //correct gamma over pion ratio, using two efficiencies:
  getRatio(gammaYieldMBratio,effGammaMB,effGammaSingleMB,fit_piondecay);
  getRatio(gammaYieldHT1ratio,effGammaHT1,effGammaSingleHT1,fit_piondecay);
  getRatio(gammaYieldHT2ratio,effGammaHT2,effGammaSingleHT2,fit_piondecay);

  TH1F *gammaYieldMBratio_incl=new TH1F(*gammaYieldMBratio);
  TH1F *gammaYieldHT1ratio_incl=new TH1F(*gammaYieldHT1ratio);
  TH1F *gammaYieldHT2ratio_incl=new TH1F(*gammaYieldHT2ratio);


  TH1F *gammaYieldMBratioNoErr=new TH1F(*gammaYieldMBratio);
  TH1F *gammaYieldHT1ratioNoErr=new TH1F(*gammaYieldHT1ratio);
  TH1F *gammaYieldHT2ratioNoErr=new TH1F(*gammaYieldHT2ratio);
  for(int i=1;i<=gammaYieldMBratioNoErr->GetNbinsX();i++){
    gammaYieldMBratioNoErr->SetBinError(i,0.);
  }
  for(int i=1;i<=gammaYieldHT1ratioNoErr->GetNbinsX();i++){
    gammaYieldHT1ratioNoErr->SetBinError(i,0.);
  }
  for(int i=1;i<=gammaYieldHT2ratioNoErr->GetNbinsX();i++){
    gammaYieldHT2ratioNoErr->SetBinError(i,0.);
  }
    
  TGraphErrors *g_ratioMB=new TGraphErrors(gammaYieldMBratio);
  g_ratioMB->SetName("g_ratioMB");
  TGraphErrors *g_ratioHT1=new TGraphErrors(gammaYieldHT1ratio);
  g_ratioHT1->SetName("g_ratioHT1");
  TGraphErrors *g_ratioHT2=new TGraphErrors(gammaYieldHT2ratio);
  g_ratioHT2->SetName("g_ratioHT2");


  removeThesePoints(g_ratioMB,1);
  removeThesePoints(g_ratioHT1,2);
  removeThesePoints(g_ratioHT2,3);

  TCanvas *c_ratio=new TCanvas("c_ratio","c_ratio",400,200);

  TMultiGraph *m_ratio=new TMultiGraph("m_ratio","d+Au 2003;p_{T};#gamma/#pi^{0}");
  m_ratio->Add(g_ratioMB);
  m_ratio->Add(g_ratioHT1);
  m_ratio->Add(g_ratioHT2);

  m_ratio->Draw("ap");
  m_ratio->SetMinimum(.0);
  m_ratio->SetMaximum(2.);

  TLegend *leg3=new TLegend(.35,.65,.65,.85);
  leg3->AddEntry(g_ratioMB,"minimum bias","p");
  leg3->AddEntry(g_ratioHT1,"hightower 1","p");
  leg3->AddEntry(g_ratioHT2,"hightower 2","p");
  leg3->AddEntry(fit_decay,"decay background (total)","l");
  leg3->AddEntry(fit_piondecay,"decay background (#pi^{0})","l");
  leg3->SetFillColor(0);
  leg3->Draw("same");

  fit_decay->SetLineColor(13);
  fit_decay->SetLineWidth(1);
  fit_decay->SetLineColor(1);
  fit_decay->Draw("same");
  fit_piondecay->SetLineColor(13);
  fit_piondecay->SetLineWidth(1);
  fit_piondecay->SetLineStyle(2);
  fit_piondecay->SetLineColor(1);
  fit_piondecay->Draw("same");


  c_ratio->SaveAs((dir+"gammaOverPion.eps").Data());
  c_ratio->SaveAs((dir+"gammaOverPion.root").Data());


  //create fully corrected incl. photons:
  gammaYieldMBratio_incl->Multiply(pionYieldMBratio);
  gammaYieldHT1ratio_incl->Multiply(pionYieldHT1ratio);
  gammaYieldHT2ratio_incl->Multiply(pionYieldHT2ratio);
  
  TGraphErrors *g_incl_corrMB=new TGraphErrors(gammaYieldMBratio_incl);
  TGraphErrors *g_incl_corrHT1=new TGraphErrors(gammaYieldHT1ratio_incl);
  TGraphErrors *g_incl_corrHT2=new TGraphErrors(gammaYieldHT2ratio_incl);

  TCanvas *c_incl_corr=new TCanvas("c_incl_corr","c_incl_corr",400,300);
  gPad->SetLogy();
  TMultiGraph *m_incl_corr=new TMultiGraph();
  m_incl_corr->Add(g_incl_corrMB);
  m_incl_corr->Add(g_incl_corrHT1);
  m_incl_corr->Add(g_incl_corrHT2);

  m_incl_corr->SetMinimum(1.e-11);
  m_incl_corr->SetMaximum(1.);

  m_incl_corr->Draw("apX");
  c_incl_corr->SaveAs((dir+"inclPhotonYieldCorr.eps").Data());
  c_incl_corr->SaveAs((dir+"inclPhotonYieldCorr.root").Data());

  TCanvas *c_doubleratio=new TCanvas("c_doubleratio","c_doubleratio",400,300);
  gStyle->SetOptStat(0);
  c_doubleratio->cd(1);

  TH1F *gammaYieldMBdoubleratio=new TH1F(*gammaYieldMBratio);
  TH1F *gammaYieldHT1doubleratio=new TH1F(*gammaYieldHT1ratio);
  TH1F *gammaYieldHT2doubleratio=new TH1F(*gammaYieldHT2ratio);

  gammaYieldMBdoubleratio->Divide(fit_decay);
  gammaYieldHT1doubleratio->Divide(fit_decay);
  gammaYieldHT2doubleratio->Divide(fit_decay);

  TGraphErrors *g_doubleRatioMB=new TGraphErrors(gammaYieldMBdoubleratio);
  g_doubleRatioMB->SetName("g_doubleRatioMB");
  g_doubleRatioMB->SetMarkerStyle(8);
  TGraphErrors *g_doubleRatioHT1=new TGraphErrors(gammaYieldHT1doubleratio);
  g_doubleRatioHT1->SetName("g_doubleRatioHT1");
  g_doubleRatioHT1->SetMarkerStyle(8);
  TGraphErrors *g_doubleRatioHT2=new TGraphErrors(gammaYieldHT2doubleratio);
  g_doubleRatioHT2->SetName("g_doubleRatioHT2");
  g_doubleRatioHT2->SetMarkerStyle(8);

  removeThesePoints(g_doubleRatioMB,1);
  removeThesePoints(g_doubleRatioHT1,2);
  removeThesePoints(g_doubleRatioHT2,3);

  TMultiGraph *m_doubleratio=new TMultiGraph();
  m_doubleratio->SetMinimum(.5);
  m_doubleratio->SetMaximum(2.75);

  cout<<endl;
  g_doubleRatioHT1->Print();
  cout<<endl<<endl;
  g_doubleRatioHT2->Print();
  cout<<endl;

  //m_doubleratio->Add(g_doubleRatioMB,"p");
  m_doubleratio->Add(g_doubleRatioHT1,"p");
  m_doubleratio->Add(g_doubleRatioHT2,"p");

  g_photonpqcd->SetLineWidth(2);
  g_photonpqcd->SetLineColor(2);
  g_photonpqcd05->SetLineWidth(2);
  g_photonpqcd05->SetLineColor(2);
  g_photonpqcd05->SetLineStyle(2);
  g_photonpqcd2->SetLineWidth(2);
  g_photonpqcd2->SetLineColor(2);
  g_photonpqcd2->SetLineStyle(2);

  m_doubleratio->Add(g_photonpqcd,"c");
  m_doubleratio->Add(g_photonpqcd05,"c");
  m_doubleratio->Add(g_photonpqcd2,"c");

  //appropriate fit to photon pqcd result
  //TF1 *fitGamma2=new TF1("fitGamma2","1.+[0]*pow(x,[1])",2.,15.);

  m_doubleratio->Draw("a");
  
  m_doubleratio->GetXaxis()->SetTitle("p_{T} (GeV/c)");
  m_doubleratio->GetYaxis()->SetTitle("1 + #gamma_{dir}/#gamma_{incl}");
  m_doubleratio->GetXaxis()->SetRangeUser(2.,16.);
  

  TLegend *leg5=new TLegend(.15,.6,.6,.8);
  leg5->AddEntry(g_doubleRatioHT1,"hightower-1","p");
  leg5->AddEntry(g_doubleRatioHT2,"hightower-2","p");
  leg5->AddEntry(g_photonpqcd,"NLO (CTEQ6+KKP) #mu=p_{T}","l");
  leg5->AddEntry(g_photonpqcd05,"#mu=2p_{T}, #mu=p_{T}/2","l");
  leg5->SetFillColor(0);
  leg5->Draw("same");

  c_doubleratio->cd(0);  
  c_doubleratio->SaveAs((dir+"gammaDoubleRatio.eps").Data());
  c_doubleratio->SaveAs((dir+"gammaDoubleRatio.root").Data());


  TCanvas *c_dirphoton=new TCanvas("c_dirphoton","c_dirphoton",400,300);
  gStyle->SetOptStat(0);
  c_dirphoton->cd(1);

  TH1F *dirphotonYieldMB=new TH1F(*gammaYieldMBdoubleratio);
  TH1F *dirphotonYieldHT1=new TH1F(*gammaYieldHT1doubleratio);
  TH1F *dirphotonYieldHT2=new TH1F(*gammaYieldHT2doubleratio);

  TH1F *dirphotonYieldMBnoErr=new TH1F(*dirphotonYieldMB);
  for(int i=1;i<=dirphotonYieldMBnoErr->GetNbinsX();i++){
    dirphotonYieldMBnoErr->SetBinError(i,0.);
  }
  TH1F *dirphotonYieldHT1noErr=new TH1F(*dirphotonYieldHT1);
  for(int i=1;i<=dirphotonYieldHT1noErr->GetNbinsX();i++){
    dirphotonYieldHT1noErr->SetBinError(i,0.);
  }
  TH1F *dirphotonYieldHT2noErr=new TH1F(*dirphotonYieldHT2);
  for(int i=1;i<=dirphotonYieldHT1noErr->GetNbinsX();i++){
    dirphotonYieldHT2noErr->SetBinError(i,0.);
  }

  TF1 *f_unity=new TF1("f_unity","1.",0.,15.);
  dirphotonYieldMB->Add(f_unity,-1.);
  dirphotonYieldHT1->Add(f_unity,-1.);
  dirphotonYieldHT2->Add(f_unity,-1.);


  dirphotonYieldMB->Divide(dirphotonYieldMBnoErr);
  dirphotonYieldHT1->Divide(dirphotonYieldHT1noErr);
  dirphotonYieldHT2->Divide(dirphotonYieldHT2noErr);
  dirphotonYieldMB->Multiply(gammaYieldMBratioNoErr);
  dirphotonYieldHT1->Multiply(gammaYieldHT1ratioNoErr);
  dirphotonYieldHT2->Multiply(gammaYieldHT2ratioNoErr);
  dirphotonYieldMB->Multiply(pionXsMBnoErr);
  dirphotonYieldHT1->Multiply(pionXsHT1noErr);
  dirphotonYieldHT2->Multiply(pionXsHT2noErr);


  TGraphErrors *g_dirphotonMB=new TGraphErrors(dirphotonYieldMB);
  g_dirphotonMB->SetName("g_dirphotonMB");
  g_dirphotonMB->SetMarkerStyle(8);
  TGraphErrors *g_dirphotonHT1=new TGraphErrors(dirphotonYieldHT1);
  g_dirphotonHT1->SetName("g_dirphotonHT1");
  g_dirphotonHT1->SetMarkerStyle(8);
  TGraphErrors *g_dirphotonHT2=new TGraphErrors(dirphotonYieldHT2);
  g_dirphotonHT2->SetName("g_dirphotonHT2");
  g_dirphotonHT2->SetMarkerStyle(8);


  removeThesePoints(g_dirphotonMB,1);
  removeThesePoints(g_dirphotonHT1,2);
  removeThesePoints(g_dirphotonHT2,3);

  gPad->SetLogy();

  TMultiGraph *m_dirphoton=new TMultiGraph();
  m_dirphoton->SetName("m_dirphoton");
  m_dirphoton->SetMinimum(1.0e-11);
  m_dirphoton->SetMaximum(0.1);

  m_dirphoton->Add(g_dirgamma,"c");
  m_dirphoton->Add(g_dirgamma05,"c");
  m_dirphoton->Add(g_dirgamma2,"c");

  cout<<"direct photons:"<<endl;
  g_dirphotonHT1->Print();
  cout<<endl;
  g_dirphotonHT2->Print();
  cout<<endl;

  m_dirphoton->Add(g_dirphotonHT1,"p");
  m_dirphoton->Add(g_dirphotonHT2,"p");

  m_dirphoton->Draw("a");

  m_dirphoton->GetXaxis()->SetTitle("p_{T} (GeV/c)");
  m_dirphoton->GetYaxis()->SetTitle("1 - R^{-1}");
  m_dirphoton->GetXaxis()->SetRangeUser(2.,16.);


  TLegend *leg5=new TLegend(.15,.6,.6,.8);
  leg5->AddEntry(g_dirphotonHT1,"hightower-1","p");
  leg5->AddEntry(g_dirphotonHT2,"hightower-2","p");
  leg5->SetFillColor(0);
  leg5->Draw("same");

  c_dirphoton->cd(0);
  c_dirphoton->SaveAs((dir+"gammaDirPhoton.eps").Data());
  c_dirphoton->SaveAs((dir+"gammaDirPhoton.root").Data());





  return;
}
