TFile *outH, *inpH;

const int  totC=4;
char *tCutL[totC]={ "Pt16", "PtLMH", "EtaBF",  "Qpn"};

const int mxAmp=4;
char *ampL[mxAmp]={"a1:P",  "-b1:Q",  "c0:PQ",  "c2:PQ"};

const int mxGr=200;
TGraphErrors * grL[mxGr];
int nGr=0;
TString  wrkDir="final/fixMe/";

endPlot(char *path="") {
  wrkDir="final/";
  wrkDir+=path;
  //wrkDir="final/default-H/"; 
  //wrkDir="final/maxEta1.4-H/"; 
  //wrkDir="final/nPrim5_20-H/";
  //wrkDir="final/zVert50-H/";
  //wrkDir="final/highPol-H/";


//wrkDir="final/swapPatt-H/";
  // wrkDir="final/shiftPatt-H/";
    //wrkDir="final/pT1_3-H/";
  
  //wrkDir="final/posCharge-H/";
 

  gStyle->SetOptStat(1111);
  
  createTgr(wrkDir+"endVer1.hist.root");
 
  //.................. access   TGraphs with averages
  TString fname=wrkDir+"asyVer1.hist.root";
  inpH=new TFile(fname);
  assert(inpH->IsOpen());
  //inpH->ls();

  const int  mxC2=6, mxC3=3, mxC4=4, mxC5=2;
  char  *cut2L[mxC2]={ "Pt1",  "Pt2",  "Pt3",  "Pt4",  "Pt5",  "Pt6"};
  float cut2XM[mxC2]={ 0.7,1.5,2.5,3.5,4.5,5.5};
  float cut2XH[mxC2]={ 0.3,.5,.5,.5,.5,.5};
  sortCoef(cut2L, mxC2,"Pt16", cut2XM, cut2XH );
  
  char  *cut3L[mxC3]={  "PtL",  "PtM",  "PtH"};
  float cut3XM[mxC3]={ 0.55, 0.85, 2.};
  float cut3XH[mxC3]={ 0.15, 0.15, 1.};
  sortCoef(cut3L, mxC3,"PtLMH", cut3XM, cut3XH );

  char  *cut4L[mxC4]={  "EtaBB",  "EtaBc",  "EtaFc",  "EtaFF" };
  float cut4XM[mxC4]={ -.75, -.25, .25, .75};
  float cut4XH[mxC4]={ .25,.25,.25.,.25};
  // float cut4XM[mxC4]={ -.95, -.25, .25, .95}; // maxEta=1.4
  //float cut4XH[mxC4]={ .45,.25,.25.,.45}; // maxEta=1.4
  sortCoef(cut4L, mxC4,"EtaBF", cut4XM, cut4XH );

  char  *cut5L[mxC5]={    "Qneg", "Qpos"};
  float cut5XM[mxC5]={ -1, 1}; 
  float cut5XH[mxC5]={ 0., 0.};
  sortCoef(cut5L, mxC5,"Qpn", cut5XM, cut5XH );

  // add for blue & yellow
  TGraphErrors *gr;
  gr=appendTG("a1:P*Pt16","-b1:Q*Pt16"); gr->SetName("A_n*Pt16");
  gr=appendTG("a1:P*PtLMH","-b1:Q*PtLMH");gr->SetName("A_n*PtLMH");
  gr=appendTG("a1:P*Qpn","-b1:Q*Qpn");gr->SetName("A_n*Qpn");
  gr=appendTG("a1:P*EtaBF","-b1:Q*EtaBF");gr->SetName("A_n*EtaBF");


  fetchTG("c0:PQ*Pt16")->SetName("A_s*Pt16");
  fetchTG("c0:PQ*PtLMH")->SetName("A_s*PtLMH");
  fetchTG("c0:PQ*Qpn")->SetName("A_s*Qpn");
  takeModuleEta("c0:PQ*EtaBF","A_s*EtaBF");

  fetchTG("c2:PQ*Pt16")->SetName("A_d*Pt16");
  fetchTG("c2:PQ*PtLMH")->SetName("A_d*PtLMH");
  fetchTG("c2:PQ*Qpn")->SetName("A_d*Qpn");
  takeModuleEta("c2:PQ*EtaBF","A_d*EtaBF");

  int pl=3;
  plotObs("A_n",pl);
  plotObs("A_s",pl);
  plotObs("A_d",pl);

  // save histo+TGraph's
  outH->cd();
  int i;
  for(i=0;i<nGr;i++) {// add TGrpahs only
    if(outH->Get(grL[i]->GetName())) continue;
    grL[i]->Write();
  }

 

  // outH->ls();
  outH->Write();

  return;
}


//==========================================
//==========================================
TGraphErrors * fetchTG(TString name) {
  //  printf("fetch-->'%s'\n",name.Data());
  TGraphErrors *gr=0;     
  int i;
  for(i=0;i<nGr && gr==0;i++) {
      if(!name.Contains(grL[i]->GetName())) continue;
      gr=grL[i];
    }
  assert(gr);
  return gr;
}


//==========================================
//==========================================
void createTgr(TString fname) {
  outH=new TFile(fname,"RECREATE");
  assert(outH->IsOpen());
  printf("save outH -->%s\n", fname.Data());

  char *ampTit[mxAmp]={"An", "AnYell", "A#Sigma", "A#Delta"};
  int  ampCol[mxAmp]={kBlue, kYellow, kGreen, kMagenta};

  int ic,iam;    
  for (ic=0;ic<totC;ic++) { 
    for (iam=0;iam<mxAmp;iam++) {
      char name[100];
      sprintf(name,"%s*%s",ampL[iam],tCutL[ic]);
      //printf("ic=%d iam=%d name=%s=\n",ic,iam,name);
      TGraphErrors *gr =new  TGraphErrors;
      gr->SetName(name);
      gr->SetTitle(ampTit[iam]);
      gr->SetMarkerColor(ampCol[iam]);
      gr->SetMarkerSize(0.8);
      gr->SetMarkerStyle(21);
      assert(nGr<mxGr);
      grL[nGr++]=gr;   
    }
  }
}


//==========================================
//==========================================
TGraphErrors * appendTG(char *basket, char *apple, float eps=0.){

  TGraphErrors *gr1=fetchTG(basket);
  TGraphErrors *gr2=fetchTG(apple);
  int i;
  for(i=0;i<gr2->GetN();i++) {
    double erX=gr2->GetErrorX(i);
    double erY=gr2->GetErrorY(i);
    double x,y;
    gr2->GetPoint(i,x,y);

    int n=gr1->GetN();
    gr1->SetPoint(n,x+eps,y);
    gr1->SetPointError(n,erX,erY);
  }
  //gr1->Print();
  return gr1;
}

//==========================================
//==========================================
takeModuleEta(char *inp, char *out) {
  TGraphErrors *gr= (TGraphErrors *)fetchTG(inp)->Clone();
  gr->SetName(out);
  int i;
  //  gr->Print();
  //printf(" do reflection\n");
  for(i=0;i<gr->GetN();i++) {
    double x,y;
    gr->GetPoint(i,x,y);
    if(x>0) continue;
    gr->SetPoint(i,-x,y);
  }
  // gr->Print();

  assert(nGr<mxGr);
  grL[nGr++]=gr;
 
}

//==========================================
//==========================================
void sortCoef(char **cutL, int nC, char *tCut, float *xMean, float *xErr){

  printf("sortCoef(%s)\n",tCut);
  int iam=0;

  for(iam=0;iam<mxAmp;iam++) {
    TString outG=ampL[iam];
    outG+="*";
    outG+=tCut;
    TGraphErrors * grOut=fetchTG(outG);  
    
    int ic;
    for(ic=0;ic<nC;ic++){
      TString inpG=ampL[iam];
      inpG+="*";
      inpG+=cutL[ic];
      printf("%d '%s' -->%s xM=%f xEr=%f\n",ic,cutL[ic],inpG.Data(),xMean[ic], xErr[ic]);
      TGraphErrors * gr=(TGraphErrors *)inpH->Get(inpG);
      assert(gr);
      TF1 *ff=gr->GetFunction("pol0");
      if(ff==0) continue; // no fit was made
      float val=ff->GetParameter(0);
      float err=ff->GetParError(0);
      int n=grOut->GetN();
      float x=xMean[ic];
      if(strstr(inpG.Data(),"-b1:Q*Eta")) x=-x;
      grOut->SetPoint(n,x,val);
      grOut->SetPointError(n,xErr[ic],err);
    }
  }
}


//==========================================
//==========================================
void plotObs(char *obs="c0:PQ",  int flag=0) {
  printf("plot Obs -->%s\n",obs);
  char *tCutX[totC]={"pT (GeV/c), 1 GeV/c bins", " pT (GeV/c), rebinned","pseudorapidity", "charge"}; 

  c=new TCanvas(obs,obs,600,500);
  c->Divide(2,2);

  int it;
  for (it=0;it<totC;it++) {
    char name[100];
    sprintf(name,"%s*%s",obs,tCutL[it]);
    TGraphErrors * gr=fetchTG(name);
    int n=gr->GetN();
    printf("\nit=%d  name='%s', N=%d\n",it,name,n);
    // gr->Print();
    if(n<=0) continue;
    c->cd(1+it);
    gr->Draw("AP");
    gr->Fit("pol0");

    TAxis *ax=gr->GetXaxis();
    ax->SetTitle(tCutX[it]);

    drawFitError(gr);  // draw +/- sigma of the fit
    c->Update();
    modifyLabels();
    c->Update();

    // draw +/- sigma of the fit
    TF1 *ff=gr->GetFunction("pol0");
    assert(ff);
    TString aaa="avr"; aaa+=name;
    ff->SetName(aaa);

    if(strstr(obs,"A_n")) {// draw also yellow An
      sprintf(name,"%s*%s","-b1:Q",tCutL[it]);
      TGraphErrors * gr=fetchTG(name);  
      if(gr->GetN()<=0) continue;
      gr->Draw("P");
    } else if(strstr(tCutL[it],"EtaBF")) {// draw once more positive eta for A_s & A_d
      if(strstr(obs,"A_s")) sprintf(name,"c0:PQ*EtaBF");
      if(strstr(obs,"A_d")) sprintf(name,"c2:PQ*EtaBF");
      TGraphErrors * gr=fetchTG(name);  
      if(gr->GetN()<=0) continue;
      gr->Draw("P");
      gr->SetMarkerColor(kBlack); 
      gr->SetMarkerStyle(25);
    }

    // junk
    float val=ff->GetParameter(0);
    float err=ff->GetParError(0);

    float chi2ndf=0;
    if (ff->GetNDF()>0) 
     chi2ndf=ff->GetChisquare()/ff->GetNDF();

    float xSig=fabs(val)/err;
    char cKey='N';
    if(xSig>2) cKey='Y';
     
    printf("## , %s , %s ,  %f ,  %f , %f %s\n",
    	  tCutL[it], obs, val,err, chi2ndf,ff->GetName());
  }

  if(flag<=0) return;
  TString outFig=wrkDir+obs;
  if(flag&1) c->Print(outFig+".ps");
  if(flag&2) c->Print(outFig+".gif");

}

//==========================================
//==========================================
void   modifyLabels(){
  TPaveStats *st1  =( TPaveStats *)gPad->GetPrimitive("stats");
  st1->SetX1NDC(0.4);
  
  st1  =( TPaveStats *)gPad->GetPrimitive("title");
  st1->SetX2NDC(0.2);
  st1->SetY1NDC(.8);
  
}

//==========================================
//==========================================
void  drawFitError( TGraphErrors * gr) {

  int col=kRed;
  TF1 *ff=gr->GetFunction("pol0");
  assert(ff);
  
  ff->SetLineColor(col);
  ff->SetParName(0,"avr");
  
  float val=ff->GetParameter(0);
  float err=ff->GetParError(0);
  float upY=val+err, dwY=val-err;
  
  TAxis *ax=gr->GetXaxis();
  float x1=ax->GetXmin();
  float x2=ax->GetXmax();
  
  TLine *ln0=new TLine(x1,0.,x2,0.);
  float xlen=x2-x1;
  x1+=0.1*xlen;
  x2-=0.1*xlen;
  TLine *upL=new TLine(x1,upY,x2,upY);
  TLine *dwL=new TLine(x1,dwY,x2,dwY);
  upL->SetLineStyle(3);
  dwL->SetLineStyle(3);
  
  upL->SetLineColor(col);
  dwL->SetLineColor(col);
  
  upL->Draw();
  dwL->Draw();
  ln0->Draw();
}





