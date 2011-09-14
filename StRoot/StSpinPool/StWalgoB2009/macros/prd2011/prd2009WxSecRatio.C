const int mxB=2; //eta bin 
const int mxErr=3; // stat, +sys, -sys
float errA[mxB][mxErr]={{0.7,0.3,-0.3},{0.5,0.2,-0.2}};
float ratioA[mxB]={4.3,2.9};

const int mxTh=2; //number theory points first is MSTW second is CTEQ
float theory[mxTh][mxB]={{4.790,2.949},{5.044,3.071}};
float theoryErr[mxTh][mxB]={{0.430,0.236},{0.404,0.227}};
//TString theoN[mxTh]={"#splitline{     RHICBOS}{CTEQ5M Resum.}","#splitline{     RHICBOS}{MRST02 Resum.}","#splitline{deFlorian, et al.}{  MRST02 NLO}"};
TH1F *hBase;
TGraphAsymmErrors *grD;
TGraphAsymmErrors *grT[mxTh]; 
TMultiGraph *mg;

bool colorPlot;

void prd2009WxSecRatio(bool x=false, float canvasScale=1.0) {
  
  colorPlot=x;
  
  gStyle->SetOptStat(0); 
  gStyle->SetOptDate(0);

  mg = new TMultiGraph();
  initTheory();
  initData();

  c=new TCanvas("aa","bb",500*canvasScale,300*canvasScale);
  c->SetTopMargin(0.05); c->SetBottomMargin(0.13);
  c->SetLeftMargin(0.085); c->SetRightMargin(0.02);
  gPad->SetGridy(false); gPad->SetGridx(false);
  if(!colorPlot) c->SetGrayscale();
  initHisto();
  hBase->GetXaxis()->SetRangeUser(0.,1.);
  hBase->GetYaxis()->SetLabelColor(0);
  hBase->GetYaxis()->SetTitleOffset(0.6);
  hBase->GetYaxis()->SetTitleSize(0.06);
  hBase->GetYaxis()->CenterTitle();
  hBase->GetXaxis()->SetLabelColor(0);
  hBase->GetXaxis()->SetTitleOffset(0.88);
  hBase->GetXaxis()->SetTitleSize(0.06);
  hBase->GetXaxis()->CenterTitle();
  hBase->Draw();
  drawTheo();
  gPad->Modified();
  mg->SetMinimum(0.001); mg->SetMaximum(5.8);
  mg->Draw("2 same"); 
  mg->GetXaxis()->SetLimits(0.,1.);
  mg->GetXaxis()->SetNdivisions(5);
  mg->GetXaxis()->SetLabelSize(0.05);
  mg->GetXaxis()->SetRangeUser(0.0,1.0);
  mg->GetYaxis()->SetTitleOffset(2.0); 
  mg->GetYaxis()->SetLabelSize(0.05);

  drawData();
  
  //make legend
  TLegend *leg;
  leg=new TLegend(0.15,0.25,0.5,0.5);
  leg->SetMargin(0.2);
  leg->SetFillColor(kWhite);
  leg->SetLineColor(kWhite);
  leg->SetTextSize(0.04);
  leg->AddEntry(grD," STAR 2009 Data","pl");
  leg->AddEntry(grT[0]," FEWZ NLO MSTW 2008","f");
  leg->AddEntry(grT[1]," FEWZ NLO CTEQ 6.6 ","f");
  leg->Draw("same");

  //insert text
  //TLatex *lat0 = new TLatex(0.13,0.945,"Run 9 STAR #font[52]{p+p}  #sqrt{#font[72]{s}}_{ }=_{ }500 GeV");
  //lat0->SetNDC(); lat0->SetTextSize(0.04); lat0->Draw("same");
  
  if(colorPlot) {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/xSecRatioColor.png");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/xSecRatioColor.eps");
  }
  else {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/xSecRatioBW.png");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/xSecRatioBW.eps");
  }

}

//------------------------------
void initHisto() {
  char  *txt0="h";
  hBase=new TH1F(txt0, ";|#eta_{e}|; R_{W} ",2,0.,1.);
}


//----------------------
void   initData() {
  float binCenter[mxB]={0.25,0.75};
  TGraphAsymmErrors *gr=new TGraphAsymmErrors;

  for(int id=0;id<mxB; id++) {
    gr->SetMarkerStyle(20);
    gr->SetMarkerColor(1);
    gr->SetMarkerSize(1);
    gr->SetLineWidth(1);
    gr->SetLineColor(1);
    gr->SetPoint(id,binCenter[id],ratioA[id]);
    float a=errA[id][0]; // stat
    float b=errA[id][1]; // +sys
    float c=errA[id][2]; // -sys
    float yu=sqrt(a*a+b*b);
    float yd=sqrt(a*a+c*c);
    gr->SetPointError(id,0.25,0.25,yd,yu);
  }
  grD=gr;
}


//---------------------
void   drawData() {
  cout<<"drawing data"<<endl;
  grD->Draw("p e z same");
}


//put theory points here 
//----------------------
void   initTheory() {
  int color[mxTh]={kBlue,kGreen};
  for(int itheo=0; itheo<mxTh; itheo++){
    float binCenter[mxB]={0.25,0.75};
    TGraphAsymmErrors *gr=new TGraphAsymmErrors;
    for(int id=0;id<mxB; id++) {
      gr->SetFillColor(color[itheo]);
      if(itheo==0) gr->SetFillStyle(3545);
      else if(itheo==1) gr->SetFillStyle(3554);
      gr->SetPoint(id,binCenter[id],theory[itheo][id]);
      gr->SetPointError(id,0.25,0.25,theoryErr[itheo][id],theoryErr[itheo][id]);
    }
    grT[itheo]=gr;
  }
}

//---------------------
void   drawTheo() {
  for(int itheo=0;itheo<mxTh;itheo++) {
    mg->Add(grT[itheo]);
    //grT[itheo]->Print();
  }
}

