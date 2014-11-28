const int mxTheo=6, mxQ=2; // iQ=0=W+, iQ=1=W-
const int mxD=3, mxEr=4; // stat, +sys, -sys, lumi
float theoA[mxQ][mxTheo]={{83.3,80.5,75.4}{19.3,18.8,17.7}};
float theoErr[2*mxQ][mxTheo]={{0.,0.,4.3}{0.,0.,1.}{0.,0.,3.9}{0.,0.,0.9}};
float errA[mxQ][mxEr]={{3.,10.,-13.,14.}{2.,3.,-4.,4.}};
float crossA[mxQ]={61,17};
TString theoN[mxTheo]={"#splitline{     RHICBOS}{CTEQ5M Resum.}","#splitline{     RHICBOS}{MRST02 Resum.}","#splitline{deFlorian, et al.}{  MRST02 NLO}"};
TH1F *hBase;
TGraphAsymmErrors *grT[mxQ][mxTheo]; //theory graph
TGraphAsymmErrors *grD[mxQ][mxD]; //theory graph*grEx[mxQ][mx]; //theory graph 
TLegend *leg;


void plWxsec_APS() {
  gStyle->SetOptStat(0); 
  gStyle->SetOptDate(0);
  initTheory();
  initData();

  //make legend
  leg=new TLegend(0.47,0.48,0.88,0.35);
  leg->SetNColumns(2);
  //leg->SetColumnSeparation(0.01);
  leg->SetEntrySeparation(0.01);
  leg->SetMargin(0.2);
  leg->SetFillColor(0);
  leg->SetTextSize(0.023);
  leg->AddEntry(grD[0][0]," STAR Data","pl");
  leg->AddEntry(grT[0][2],theoN[2],"p");
  leg->AddEntry(grD[0][2]," Lumi. Uncert.","f");
  leg->AddEntry(grT[0][1],theoN[1],"p");
  leg->AddEntry(grT[0][2],"#splitline{  Theoretical}{ Scale Uncert.}","f");
  leg->AddEntry(grT[0][0],theoN[0],"p");
  
  c=new TCanvas("aa","bb",550,700);
  c->SetTopMargin(0.025); c->SetBottomMargin(0.05);
  gPad->SetGridy(false); gPad->SetGridx(false);
  initHisto();
  hBase->GetXaxis()->SetTickLength(0);
  hBase->GetXaxis()->SetLabelColor(0);
  hBase->GetYaxis()->SetTitleOffset(1.2);
  hBase->SetMinimum(0); hBase->SetMaximum(99.9);
  hBase->Draw();
  for(int iq=0; iq<mxQ;iq++) {
    drawTheo(iq);
    drawData(iq);
    //break;
  }

  leg->Draw("same");

  //insert text
  TLatex *lat0 = new TLatex(0.13,0.945,"Run 9 STAR Preliminary  #font[52]{p+p}  #sqrt{#font[72]{s}}_{ }=_{ }500 GeV");
  lat0->SetNDC(); lat0->SetTextSize(0.04); lat0->Draw("same");
  TLatex *lat1 = new TLatex(0.18,0.915,"Kinematic acceptance: |#eta_{#font[52]{e}}| < 1 and #font[52]{E_{T}^{e}} > 25 GeV");
  lat1->SetNDC(); lat1->SetTextSize(0.03); lat1->Draw("same");
  TLatex *lat2 = new TLatex(0.6,0.86,"#sigma(#font[52]{W^{#scale[1.2]{#pm}} #rightarrow e^{#scale[1.2]{#pm}} + #nu_{e}})");
  lat2->SetNDC(); lat2->SetTextSize(0.042); lat2->Draw("same");
  TLatex *lat3 = new TLatex(0.7,0.59,"#font[52]{W^{+}}");
  lat3->SetNDC(); lat3->SetTextSize(0.09); lat3->Draw("same");
  TLatex *lat4 = new TLatex(0.7,0.18,"#font[52]{W^{#scale[1.2]{-}}}");
  lat4->SetNDC(); lat4->SetTextSize(0.09); lat4->Draw("same");
  

}

//------------------------------
void initHisto() {
  char  *txt0="h";
  hBase=new TH1F(txt0, "; ; #sigma  (pb)", 100, 0,0.8);
}

//----------------------
void   initTheory() {
  
  int color[3]={kBlack,kGreen+3,kBlue+1};
  int marker[3]={26,28,4};

  for(int iq=0; iq<mxQ;iq++) 
    for(int t=0;t<mxTheo; t++) {
      int tb=t;
      if(t>2) tb=t-3;
      TGraphAsymmErrors *gr=new   TGraphAsymmErrors;
      gr->SetMarkerColor(color[tb]);
      gr->SetLineWidth(2);
      gr->SetLineColor(0);   
      gr->SetMarkerSize(1.0);
      if(t>2) gr->SetMarkerSize(0.8);
      gr->SetMarkerStyle(marker[tb]);
      gr->SetPoint(0,(tb+3)*0.1,theoA[iq][tb]);
      
      
      if(t==2) {
	gr->SetPointError(0,0.01,0.01,theoErr[iq+2][tb],theoErr[iq][tb]);
	gr->SetFillColor(kRed+2);
	gr->SetFillStyle(3001);
      }
      grT[iq][t]=gr;
    }
  
}


//----------------------
void   initData() {
  int iq=0;
  for(int iq=0; iq<mxQ;iq++) 
    for(int id=0;id<mxD; id++) {
      if(id>2) break;
      TGraphAsymmErrors *gr=new   TGraphAsymmErrors;
      if(id==0)gr->SetMarkerStyle(21);
      else gr->SetMarkerStyle(1);
      gr->SetMarkerColor(kRed+id);
      gr->SetMarkerSize(1.5);
      gr->SetLineWidth(2);
      gr->SetLineColor(kBlack);
     
      if(id<2) gr->SetPoint(0,0.15,crossA[iq]);
      else gr->SetPoint(0,0.1,crossA[iq]);
      
      if(id==0) 
      	gr->SetPointError(0,0,0,errA[iq][id],errA[iq][id]);
      if(id==1)  {
	float a=errA[iq][0]; // stat
	float b=errA[iq][1]; // +sys
	float c=errA[iq][2]; // -sys
	float d=errA[iq][3]; //  lumi
	float yu=sqrt(a*a+b*b);//+d*d);
	float yd=sqrt(a*a+c*c);//+d*d);
	gr->SetPointError(0,0,0,yd,yu);
      }
      if(id==2){
      	gr->SetPointError(0,0.015,0.015,errA[iq][3],errA[iq][3]);
      	gr->SetFillColor(13);
	gr->SetLineColor(0);
      }
      grD[iq][id]=gr;
      
    }

}



//---------------------
void   drawTheo(int iq) {

  for(int t=mxTheo-1;t>=0; t--) {
    if(t==2) {
      grT[iq][t]->Draw("2 same");
      grT[iq][t]->Draw("p X");
    }
    else grT[iq][t]->Draw("p");
    if(t<3) grT[iq][t]->Print();
  }
  
}



//---------------------
void   drawData(int iq) {

  for(int id=mxD-1;id>=0; id--) {
    if(id==0) grD[iq][id]->Draw("p e same");
    else if(id==1) grD[iq][id]->Draw("p e same");
    else if(id==2) grD[iq][id]->Draw("2 same");
    grD[iq][id]->Print();
  }
  
  
}
