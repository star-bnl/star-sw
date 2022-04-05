int blueCol=4;
int yellCol=kYellow;
int stage=3;
float yMax=175; 
float etMax=70;

void plWJacob(int charge=0) {
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(1000000);

  TString iPath="/star/data05/scratch/stevens4/wAnalysisOut/apsXsec/data/";
  TString core="run9setABCD";

  TFile *fd = TFile::Open(iPath+core+".wana.hist.root"); 
 
  assert(fd->IsOpen());
  
  if(charge==0){
    //charge summed histos 
    hwE=(TH1F*)fd->Get("muclustPtBal"); assert(hwE);
    hnE=(TH1F*)fd->Get("muclustPtBalnoE"); assert(hnE);
    hQCD=(TH1F*)fd->Get("muclustPtBal_bckgrd"); assert(hQCD);
  }
  else if(charge==1){
    //charge summed histos 
    hwE=(TH1F*)fd->Get("pubclustPtBalP"); assert(hwE);
    hnE=(TH1F*)fd->Get("pubclustPtBalnoEP"); assert(hnE);
    hQCD=(TH1F*)fd->Get("pubclustPtBal_bckgrdP"); assert(hQCD);
  }
  else if(charge==-1){
    //charge summed histos 
    hwE=(TH1F*)fd->Get("pubclustPtBalN"); assert(hwE);
    hnE=(TH1F*)fd->Get("pubclustPtBalnoEN"); assert(hnE);
    hQCD=(TH1F*)fd->Get("pubclustPtBal_bckgrdN"); assert(hQCD);
  }

  // plot raw spectra ..........  
  c=new TCanvas();
  c->Divide(2,3);
  c->cd(2);
  hwE->Draw();  
  c->cd(4);
  hnE->Draw();  // hnE->SetAxisRange(0.,etMax);

  //.. compute Intermediate results
  TAxis* axX=hwE->GetXaxis();
  float x1=axX->GetXmin()+1.0;
  float x2=axX->GetXmax()+1.0;
  int   nbX=axX->GetNbins();

  // assumption: output histos will have 100 bins [1,101] GeV

  // those 3 have errors =sqrt(N)
  hA=new TH1F("hA","W candidates (input)",nbX,x1,x2);
  hAA=new TH1F("hAA","W candidates- 2nd Endcap",nbX,x1,x2);

  hB=new TH1F("hB","rejected by including Endcap",nbX,x1,x2);
  hC=new TH1F("hC","rejected by inverting  cut",nbX,x1,x2);

 // .......copy & subtract input spectra, subtract 2nd endcap
  float sumA=0,sumB=0,sumC=0; // over ET<20
  for(int i=1;i<=nbX-1;i++) {
    float y1=hwE->GetBinContent(i+1);
    float y2=hnE->GetBinContent(i+1);
    float y3=hQCD->GetBinContent(i+1);
    //  printf("%d , %.1f,  %.0f, %.0f, %.0f \n",i,hA->GetBinCenter(i),y1,y2,y3);
    // compute yields & comulative variance 
    float yA=y1;
    float yB=y2-y1; // Endcap background, no adding errors in quadrature
    float yC=y3; // rejected by away ET
    hA->SetBinContent(i,yA); // W candidates
    hB->SetBinContent(i,yB);  // endcapBCG
    hC->SetBinContent(i,yC);  // away BCG
  }
  
  hC->Sumw2();   hAA->Sumw2();
  // subtract 2nd endcap
  hAA->Add(hA,1.); hAA->Add(hB,-1.);

  c->cd(1); hA->Draw();//   hB->SetFillColor(8);

  c->cd(3); hB->Draw();   hB->SetFillColor(8);


  c->cd(5); hC->Draw("he");
  hC->SetLineColor(kRed);
  hC->SetLineWidth(2);

  hA->Rebin();   hAA->Rebin();  hB->Rebin();  hC->Rebin(); 
  nbX/=2;
  hA->SetLineWidth(2.);  hA->SetAxisRange(0.,etMax); 
  hB->SetAxisRange(0.,etMax);
  hC->SetAxisRange(0.,etMax);

  float sumA=0, sumC=0; // over ET =[15,21] GeV, delET=6 GeV
  for(int i=1;i<=nbX;i++) {// decides on backg norm. range
    float y1=hAA->GetBinContent(i);
    float y2=hC->GetBinContent(i);
    //    printf("%d %f %f    \n",i,y1,y2);
    if(i>=8 && i<=9) { sumA+=y1; sumC+=y2;}
  }
  float fact=sumA/sumC;
  printf("sumA=%.1f sumC=%.1f fac=%.3f\n",sumA, sumC, fact);
  hC->Scale(fact);

  //..........................................
  //..........................................
  // ..... "Preliminary" Jacobian peak
  //..........................................
  //..........................................
  TString tt="W2009-prelimB"; tt+=stage;

  c=new TCanvas(tt,tt,600,600);
  gStyle->SetOptStat(0); 
  c->SetFillColor(kWhite);
  c->SetLeftMargin(0.15);
  c->SetFrameBorderMode(0);



  // input W spectrum
  hA3=(TH1F*) hA->Clone();
  hA3->SetTitle("PT Balance Cut; EMC cluster #font[72]{E_{T}} (GeV)       ;Counts"); 
  hA3->GetYaxis()->SetTitleOffset(1.5);
  hA3->SetAxisRange(7,69); hA3->SetMinimum(-39); hA3->SetMaximum(yMax);
  hA3->Draw("h"); hA3->SetLineWidth(2);
  ln=new TLine(8,0,70,0); ln->Draw();
  printf("raw sum=%.0f\n",hA3->Integral());
  hA3->SetTitle(iPath+core);

  // pure Ws
  hDD=(TH1F*) hAA->Clone(); 
  hDD->Add(hC,-1.);  // subtracted scaled away ET 
  hDD->SetMarkerStyle(8); 
  hDD2=(TH1F*) hDD->Clone(); 
  hDD2->SetMarkerStyle(8); hDD2->SetMarkerSize(1);
  hDD->SetFillColor(yellCol);  //hDD2->SetLineWidth(2);
  
  if( stage>2) { hDD->Draw("same h");  hDD2->Draw("same h"); }

  // away backg+ Endcap backg
  hCB=(TH1F*) hC->Clone();
  hCB->Add(hB,1.);
  hCB->SetMarkerStyle(8); hCB->SetMarkerSize(1); hCB->SetMarkerColor(blueCol);
  hCB->SetLineColor(blueCol);  hCB->SetLineWidth(2);
  
  hCB->SetAxisRange(14.5,65);
  if(stage>1) hCB->Draw("p e h same");

  // draw inut again w/ dashed style
  hA3b=(TH1F*) hA3->Clone();
  hA3b->SetLineStyle(2);
  hA3b->Draw("same");   hA3b->SetAxisRange(15,18);


  lg=new TLegend(0.32,0.64,0.66,0.80); // top
  TDatime dt;lg->SetHeader(dt.AsString());
  //lg->SetHeader("     cluster |#eta|_{ }<_{ }1");
  if(charge==0) 
    lg->AddEntry(hA,"#font[52]{W} candidates (charge summed)","l");
  else if(charge==1)
    lg->AddEntry(hA,"#font[52]{W+} candidates ","l");
  else 
    lg->AddEntry(hA,"#font[52]{W-} candidates ","l");
  lg->AddEntry(hCB,"QCD backg. est.","lfep");
  lg->AddEntry(hDD,"Backg. subtr. #font[52]{W}s","lfep");
  lg->SetFillColor(kWhite);
  lg->SetLineColor(kWhite);
  lg->Draw();

  TLatex *lat1 = new TLatex(0.32,0.84,"STAR   #font[72]{#vec{p}_{ }+_{ }#vec{p}}   #sqrt{#font[72]{s}}_{ }=_{ }500 GeV");
  lat1->SetNDC();  lat1->Draw("same");


  tx=new TText(14,20,"software threshold"); tx->SetTextAngle(90); 
  tx->SetTextSize(0.03);//tx->SetTextColor(kMagenta);
  tx->Draw();

  //....... Joe's additional variables
  // Calcate the total relative error squared for the cross section
  // which is roughly (S+2B)/S^2 
  // and for the AL which is roughly (S+B?)/S^2

  float tot_xsec_err_num = 0.;
  float tot_xsec_err_den = 0.;
  float tot_al_err_num = 0.;
  float tot_al_err_den = 0.;
  for (int i=11; i<=hA->GetNbinsX(); i++) {// defines ET range for x-section
    tot_xsec_err_num += hA->GetBinError(i)*hA->GetBinError(i);
    tot_xsec_err_num += hB->GetBinError(i)*hB->GetBinError(i);
    tot_xsec_err_num += hC->GetBinError(i)*hC->GetBinError(i);
    tot_xsec_err_den += hDD->GetBinContent(i);
    // cout << i << " " << hA->GetBinContent(i) << endl;
  }

  for (int i=13; i<=hA->GetNbinsX(); i++) { // defines ET range for AL
    tot_al_err_num += hA->GetBinError(i)*hA->GetBinError(i);
    tot_al_err_den += hDD->GetBinContent(i); 
  }
  
  float tot_xsec_rel_err = sqrt(tot_xsec_err_num)/tot_xsec_err_den;
  float tot_al_err = sqrt(tot_al_err_num)/tot_al_err_den;
  printf(" Joe measures: xsec_rel_err=%.3f  AL_err=%.3f\n", tot_xsec_rel_err, tot_al_err);

  char text[100];
  sprintf(text,"Error estim:  xSec_rel=%.4f    AL=%.3f", tot_xsec_rel_err, tot_al_err);
  tx=new TText(21,-18,text); tx->Draw(); tx->SetTextSize(0.03);

  //...... end Joe' calculation

  return; // not print
  for(int i=1;i<=nbX;i++) {
    printf("%d , %.1f,",i,hA->GetBinCenter(i));
    printf("   %.1f, %.1f, ", hA->GetBinContent(i), hA->GetBinError(i));
    printf("   %.1f, %.1f, ", hCB->GetBinContent(i), hCB->GetBinError(i));
    printf("   %.1f, %.1f\n ", hDD->GetBinContent(i), hDD->GetBinError(i));
  }

}
