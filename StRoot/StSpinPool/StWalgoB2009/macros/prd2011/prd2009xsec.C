//init W info and data points
const int mxQ=2; // iQ=0=W+, iQ=1=W-
const int mxD=4, mxEr=4; // stat, +sys, -sys, lumi
float errA[mxD][mxQ][mxEr]={{{5.95,5.93,-5.97.,15.09}{3.46,2.97,-3.07,4.09}},{{21.2,3.4,10.3,21.6}{12.1,10.1,8.2,4.8}},{{260,480,480,690}{220,330,330,470}},{{60,170,170,640}{50,140,140,450}}};
float crossA[mxD][mxQ]={{116.05,42.36},{144.1,31.7},{6270,4230},{5860,4090}};
//order of x-sec and err is STAR(e), Phenix(e), Atlas(l), CMS(l), CDF(l,1.8), CDF(l,1.96), D0(l,1.8), D0(l,1.96), UA1(l), UA2(e)

const int mxColl=2; // pp and ppbar
const int mxPt=80; //number of points for curve
float theory[mxColl][mxQ][mxPt];
TH1F *hBase;
TGraphAsymmErrors *grD[mxQ][mxD]; //theory graph*grEx[mxQ][mx];
TGraph *grT[mxQ][mxColl]; 


//init Z info and data points
const int mxDZ=3, mxErZ=4; // stat, +sys, -sys, lumi
float errZ[mxDZ][mxErZ]={{2.14,0.50,-0.50,1.01},{60,50,50,90},{40,60,60,110}};
float crossZ[mxDZ]={7.73,820,930};
float theoryZ[mxColl][mxPt];
TH1F *hBaseZ;
TGraphAsymmErrors *grDZ[mxD];
TGraph *grTZ[mxColl];

bool colorPlot;

void prd2009xsec(bool x=false, float canvasScale=1.0){

  theoryPoints();

  colorPlot=x; 

  //canvas for plotting
  TCanvas *c=new TCanvas("aa","bb",500*canvasScale,500*canvasScale);

  TPad *cT = new TPad("padT","padT",0,0.5,1.,1.); cT->Draw();
  TPad *cB = new TPad("padB","padB",0,0,1.,0.5); cB->Draw();
  cT->cd();
  prd2009Wxsec();
  cB->cd();
  prd2009Zxsec();

  if(colorPlot) {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/xsecBRcolor.png");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/color/xsecBRcolor.eps");
  }
  else {
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/xsecBRbw.png");
    c->Print("/star/u/stevens4/wAnalysis/xSecPaper/plots/bw/xsecBRbw.eps");
  }

  return;
}

void prd2009Wxsec() {
  gStyle->SetOptStat(0); 
  gStyle->SetOptDate(0);
  initData();
  initTheory();

  //set up pad
  gPad->SetTopMargin(0.05); gPad->SetBottomMargin(0.0);
  gPad->SetLeftMargin(0.1); gPad->SetRightMargin(0.03);
  gPad->SetGridy(false); gPad->SetGridx(false);
  gPad->SetLogy(); gPad->SetLogx();
  initHisto();
  hBase->GetYaxis()->SetTitleOffset(0.6);
  hBase->GetYaxis()->CenterTitle();
  hBase->GetYaxis()->SetTitleSize(0.07);
  hBase->GetYaxis()->SetLabelSize(0.06);
  hBase->SetMinimum(8.0); hBase->SetMaximum(2e4);
  
  hBase->Draw();
  for(int iq=0; iq<mxQ;iq++) {
    drawTheo(iq);
    drawData(iq);
  }

  //make legend
  TLegend *leg;
  leg=new TLegend(0.7,0.2,0.95,0.5);
  leg->SetNColumns(2);
  //leg->SetColumnSeparation(0.01);
  leg->SetEntrySeparation(0.01);
  leg->SetMargin(0.2);
  leg->SetFillColor(kWhite);
  leg->SetLineColor(kWhite);
  leg->SetTextSize(0.05);
  leg->AddEntry(grD[0][0]," UA1","pl");
  leg->AddEntry(grD[0][0]," STAR","pl");
  leg->AddEntry(grD[0][0]," UA2","pl");
  leg->AddEntry(grD[0][1]," Phenix","pl");
  leg->AddEntry(grD[0][0]," CDF","pl");
  leg->AddEntry(grD[0][2]," Atlas","pl");
  leg->AddEntry(grD[0][0]," D0","pl");
  leg->AddEntry(grD[0][3]," CMS","pl");
  leg->Draw("same");

  return;

}

//------------------------------
void initHisto() {
  char  *txt0="hW";
  hBase=new TH1F(txt0, "; #sqrt{s}  (GeV); #sigma_{W} #upoint BR(W #rightarrow l #nu)  (pb)   ", 100, 350,12000);
}


//----------------------
void   initData() {
  float offset[mxD]={-10,10,-250,250};
  float style[mxD][mxQ]={{29,30},{22,26},{20,24},{21,25}};
  float color[mxD]={kRed,kBlue,kGreen,kOrange};
  float size[mxD]={1.2,1.0,1.0,1.0};
  float roots[mxD]={500,500,7000,7000};
  for(int iq=0; iq<mxQ;iq++) 
    for(int id=0;id<mxD; id++) {
      TGraphAsymmErrors *gr=new TGraphAsymmErrors;
      gr->SetMarkerStyle(style[id][iq]);
      if(colorPlot) gr->SetMarkerColor(color[id]);
      gr->SetMarkerSize(size[id]);
      gr->SetLineWidth(1);
      gr->SetPoint(0,roots[id]+offset[id],crossA[id][iq]);
      float a=errA[id][iq][0]; // stat
      float b=errA[id][iq][1]; // +sys
      float c=errA[id][iq][2]; // -sys
      float d=errA[id][iq][3]; //  lumi
      float yu=sqrt(a*a+b*b+d*d);
      float yd=sqrt(a*a+c*c+d*d);
      gr->SetPointError(0,0,0,yd,yu);
      grD[iq][id]=gr;
    }
}


//---------------------
void   drawData(int iq) {
  for(int id=mxD-1;id>=0; id--) {
    grD[iq][id]->Draw("e p z same");
    //grD[iq][id]->Print();
  }
}

//----------------------
void   initTheory() {
  float xPt[mxPt];
  for(int j=0; j<mxPt; j++)
    xPt[j]=100+j*100;

  int style[mxColl][mxQ]={{1,7},{3,3}};
  for(int icoll=0; icoll<mxColl; icoll++){
    for(int iq=0; iq<mxQ;iq++) {
      TGraph *gr=new TGraph;
      //gr->SetMarkerColor(color[tb]);
      gr->SetLineWidth(1);
      gr->SetLineStyle(style[icoll][iq]);   
      gr->SetMarkerSize(0);
      for(int i=0; i<mxPt; i++){
        if(icoll==0) gr->SetPoint(i,xPt[i],theory[icoll][iq][i]);
        else gr->SetPoint(i,xPt[i],2.*theory[icoll][iq][i]); 
      }
      grT[iq][icoll]=gr;
    }
  }  
}

//---------------------
void   drawTheo(int iq) {
  for(int id=0;id<mxColl;id++) {
    if(id==1 && iq==1) continue;
    grT[iq][id]->Draw("p c same");
    //cout<<"draw W theory "<<id<<" "<<iq<<endl;
    //grT[iq][id]->Print();
  }
}


//plot Z x-sec
void prd2009Zxsec() {

  initDataZ();
  initTheoryZ();

  //set up pad
  gPad->SetTopMargin(0.0); gPad->SetBottomMargin(0.15);
  gPad->SetLeftMargin(0.1); gPad->SetRightMargin(0.03);
  gPad->SetGridy(false); gPad->SetGridx(false);
  gPad->SetLogy(); gPad->SetLogx();
  initHistoZ();
  hBaseZ->GetXaxis()->SetTitleOffset(0.9);
  hBaseZ->GetXaxis()->CenterTitle();
  hBaseZ->GetXaxis()->SetTitleSize(0.07);
  hBaseZ->GetXaxis()->SetLabelSize(0.06);
  hBaseZ->GetYaxis()->SetTitleOffset(0.6);
  hBaseZ->GetYaxis()->CenterTitle();
  hBaseZ->GetYaxis()->SetTitleSize(0.07);
  hBaseZ->GetYaxis()->SetLabelSize(0.06);
  
  hBaseZ->SetMinimum(1.0); hBaseZ->SetMaximum(2500);
  hBaseZ->Draw();
  drawTheoZ();
  drawDataZ();

  //make legend
  
  TLegend *leg;
  leg=new TLegend(0.6,0.3,0.95,0.6);
  leg->SetNColumns(2);
  leg->SetEntrySeparation(0.01);
  leg->SetMargin(0.2);
  leg->SetFillColor(kWhite);
  leg->SetLineColor(kWhite);
  leg->SetTextSize(0.05);
  leg->SetHeader("  FEWZ and MSTW08 NLO PDFs");
  leg->AddEntry(grT[0][0]," p+p #rightarrow W^{+}","l");
  leg->AddEntry(grTZ[0]," p+p #rightarrow Z/#gamma*","l");
  leg->AddEntry(grT[1][0]," p+p #rightarrow W^{-}","l");
  leg->AddEntry(grTZ[1]," p+#bar{p} #rightarrow Z/#gamma*","l");
  leg->AddEntry(grT[0][1]," p+#bar{p} #rightarrow W^{+/-}","l");  
  leg->Draw("same");

  return;
}

//------------------------------
void initHistoZ() {
  char  *txt0="hZ";
  hBaseZ=new TH1F(txt0, "; #sqrt{s}  (GeV); #sigma_{Z/#gamma*} #upoint BR(Z/#gamma* #rightarrow ll) (pb)   ", 100, 350,12000);
}


//----------------------
void   initDataZ() {
  float offset[mxDZ]={0,-250,250};
  float style[mxDZ]={29,20,21};
  float color[mxDZ]={kRed,kGreen,kOrange};
  float roots[mxDZ]={500,7000,7000};
  for(int id=0;id<mxDZ; id++) {
    TGraphAsymmErrors *gr=new TGraphAsymmErrors;
    gr->SetMarkerStyle(style[id]);
    if(colorPlot) gr->SetMarkerColor(color[id]);
    gr->SetMarkerSize(1.0);
    gr->SetLineWidth(1);
    gr->SetPoint(0,roots[id]+offset[id],crossZ[id]);
    float a=errZ[id][0]; // stat
    float b=errZ[id][1]; // +sys
    float c=errZ[id][2]; // -sys
    float d=errZ[id][3]; //  lumi
    float yu=sqrt(a*a+b*b+d*d);
    float yd=sqrt(a*a+c*c+d*d);
    gr->SetPointError(0,0,0,yd,yu);
    grDZ[id]=gr;
  }
}


//---------------------
void   drawDataZ() {
  for(int id=mxDZ-1;id>=0; id--) {
    grDZ[id]->Draw("p e z same");
    //grDZ[id]->Print();
  }
}


//----------------------
void   initTheoryZ() {
  float xPt[mxPt];
  for(int j=0; j<mxPt; j++)
    xPt[j]=100+j*100;

  int style[mxColl]={1,3};
  for(int icoll=0; icoll<mxColl; icoll++){
    TGraph *gr=new TGraph;
    //gr->SetMarkerColor(color[tb]);
    gr->SetLineWidth(1);
    gr->SetLineStyle(style[icoll]);   
    gr->SetMarkerSize(0);
    for(int i=0; i<mxPt; i++){
      if(icoll==0) gr->SetPoint(i,xPt[i],theoryZ[icoll][i]);
      else gr->SetPoint(i,xPt[i],2.*theoryZ[icoll][i]); 
      grTZ[icoll]=gr;
    }
  }  
}

//---------------------
void   drawTheoZ() {
  for(int id=0;id<mxColl;id++) {
    grTZ[id]->Draw("p c same");
    //grTZ[id]->Print();
  }
}


//---------------------
void theoryPoints() {
  //read in theory points from csv viles
  string path="/star/u/stevens4/wAnalysis/xSecPaper/plots/xSecBR/";
  string names[6]={"ppWplusRoots.csv","ppWminusRoots.csv","ppbarWplusRoots.csv","ppbarWminusRoots.csv","ppZroots.csv","ppbarZroots.csv"};
  int coll[6]={0,0,1,1,0,1};
  int q[6]={0,1,0,1,2,2};
  for(int ifile=0; ifile<6; ifile++){
    ifstream file(Form("%s%s",path.data(),names[ifile].data()));
    string line;
    
    int i=0;
    while(file.good()){
      getline (file,line);
      if(line == ""){ //blank last line ie. end of file
	break;
      }
      if(q[ifile]<2) theory[coll[ifile]][q[ifile]][i]=atof(line.data());
      else theoryZ[coll[ifile]][i]=atof(line.data());
      i++;
    }
  }
  
}
