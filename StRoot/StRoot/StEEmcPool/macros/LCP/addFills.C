TFile *outH;
const int mxR=3;
const int mxC=16;// was 16
char *cutL[mxC]={"All",  "Pt1",  "Pt2",  "Pt3",  "Pt4",  "Pt5",  "Pt6",  "PtL",  "PtM",  "PtH",  "EtaBB",  "EtaBc",  "EtaFc",  "EtaFF",  "Qpos",  "Qneg"};

const int mxAmp=9;
char *ampL[mxAmp]={"a1:P",  "-b1:Q",  "c0:PQ",  "c2:PQ",  "a0",  "a2",  "b0",  "b2",  "c1"};

const int mxGr=200;
TGraphErrors * grL[mxGr];
int nGr=0;

// logic of Lum & pol
float minPol=0.07; // was 0.07
float minPol2=0.01; // was 0.01


addFills( TString outPath="defaultB-H/") {
  TString inpDir="/star/data04/sim/balewski/LcpRun2/maxEta1.0/";
  
  char *fillL="  F2258  F2266 F2161 ";
  //  fillL=" F2053  F2075  F2076  F2083  F2095  F2102  F2105  F2110  F2116  F2127  F2132  F2134  F2135  F2136  F2147  F2153  F2161 ";

   fillL=" F2053  F2075  F2076  F2083  F2095  F2102  F2105  F2110  F2116  F2127  F2132  F2134  F2135  F2136  F2147  F2153  F2161  F2162  F2175  F2178  F2181  F2185  F2187  F2192  F2193  F2196  F2201  F2207  F2208  F2212  F2216  F2222  F2235  F2246  F2251  F2257  F2258  F2266  F2269  F2275  F2277  F2281  F2289  F2290  F2301  F2303  F2304";
  
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(1111);
  gROOT->LoadMacro("getPol.C");

  createHistTgr("final/"+outPath+"asyVer1.hist.root");

  char *fill=strtok(fillL," "); // init 'strtok'
  int nFill=0;
  do {
    //.................. access R1,2,3  histos
    TString fname=inpDir+outPath+"r"+fill+".hist.root";
    TFile * inpH=new TFile(fname);
    assert(inpH->IsOpen());
    if(!inpH->IsOpen()) {
      printf("#fail %s does not open, skip\n",fname.Data());
      continue;
    }
    //inpH->ls();
    nFill++;
    //printf("\nprocess fill %02d '%s' \n",nFill,fill);
    int xFill=atoi(fill+1);
    assert(xFill);
    sortCoef(inpH,xFill);
    
  }while(fill=strtok(0," ")); 
  
  //nicePlot(); return;
  // plotTG("Pt1",3); return;

  int icut;
  for(icut=0;icut<mxC;icut++) {     
      char *cut=cutL[icut];
    plotTG(cut,3,"final/"+outPath);
  }
  
  // save histo+TGraph's
  outH->cd();
  int i;
  for(i=0;i<nGr;i++) {// add TGrpahs only
    if(outH->Get(grL[i]->GetName())) continue;
    grL[i]->Write();
  }

  //  outH->ls();
  outH->Write();
  return;
}


//==========================================
//==========================================
TGraphErrors * fetchTG(TString name) {
  //printf("fetch-->'%s'\n",name.Data());
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
void sortCoef(TFile *inp, int xFill){
  assert(inp->IsOpen());

  float P,Q,eP,eQ;
  getPol(xFill,P,Q,eP,eQ);
  float PQ, ePQ;
  PQ=P*Q;
  ePQ=PQ*sqrt(eP*eP/P/P +eQ*eQ/Q/Q);
  //printf("%d  P*Q=%f /- %f\n",xFill,PQ,ePQ);
    
  int cLum=true, cP=true, cQ=true,cPQ=true;
  if(xFill<2189) cLum=false;
  if(P<minPol) cP=false;
  if(Q<minPol) cQ=false;
  if(PQ<minPol2) cPQ=false;
  printf("   logic cL=%d  cP=%d  cQ=%d cPQ=%d\n",cLum,cP,cQ,cPQ);


  int icut;
  for(icut=0;icut<mxC;icut++) {     
  //  for(icut=0;icut<2;icut++) {     
    char *cut=cutL[icut];
    // fetch histos with fitted ratios
    TH1F * h[mxR];
    int k;
    for(k=0;k<mxR;k++) {
      char  name[100];
      sprintf(name,"r%d*%s",k+1,cut);
      //printf("nn2=%s=\n",name);
      h[k]=(TH1F *)inp->Get(name);
      //assert(h[k]);
    }
    
    // extract fit params and add to Tgraph's
    int ir;
    for(ir=0;ir<mxR;ir++) {
      if( h[ir]==0) continue;
      TF1 *ff=h[ir]->GetFunction("fCos012");
      assert(ff);
      
      const int mxPar=3; // out
      int ip;
      for(ip=0;ip<mxPar;ip++) {
	TString grName;
	float pol=1, ePol=0;
	
	// manipulate with fite coef depending on observable
	
	if(ir==0 && ip==0 )         { grName="a0"; 
	} else if (ir==0 && ip==1 && cP ) { grName="a1:P";
	pol=P;
	ePol=eP;
	} else if (ir==0 && ip==2 ) { grName="a2";
	} else if (ir==1 && ip==0 ) { grName="b0";
	} else if (ir==1 && ip==1 && cQ ) { grName="-b1:Q";
	pol=-Q;
	ePol=eQ;
	} else if (ir==1 && ip==2 ) { grName="b2";
	} else if (ir==2 && ip==0 && cPQ && cLum) { grName="c0:PQ";
	pol=PQ;
	ePol=ePQ;
	} else if (ir==2 && ip==1 ) { grName="c1";
	} else if (ir==2 && ip==2 & cPQ ) { grName="c2:PQ";
	pol=PQ;
	ePol=ePQ;
	} else {
	  continue;
	}
	
	// add result to the proper tgraph
	grName+="*";
	grName+=cut;
	TGraphErrors * gr=fetchTG(grName);  
	// printf("ir=%d ip=%d name=%s=%p\n",ir,ip,grName.Data(),gr);
	double val=ff->GetParameter(ip);
	double err=ff->GetParError(ip);
	storeVal(gr,xFill, val,err,pol,ePol); 
	
      }
    
      // accumulate chi2
      double chi2=ff->GetChisquare();
      double ndf=ff->GetNDF();
      //printf("ir%d %f %f \n",ir,chi2,ndf);
      char name[100];
      sprintf(name,"chi2R%d*%s",ir+1,cut);
      TH1F *ho=(TH1F*)fetchTG(name);  
      assert(ho);
      ho->Fill(chi2/ndf);
    }// end of loop over ir
  }// end of loop over cuts    
    
}


//==========================================
//==========================================
void createHistTgr(TString fname) {
  outH=new TFile(fname,"RECREATE");
  assert(outH->IsOpen());
  printf("save outH -->%s\n", fname.Data());
  
  // create TGraphis with fit amplitudes

  char *ampTit[mxAmp]={"An, ~ cos(#phi), Blue","An, ~ cos(#phi), Yellow","A#Sigma, no #phi ","A#Delta,  ~ cos(2#phi)",
		       "a0 -instrumental",   "a2  -instrumental",  
		       "b0 -instrumental",  "b2 -instrumental",  
		       "c1 -instrumental"};

  int ampCol[mxAmp]={kBlue,kYellow,kGreen,kMagenta,kBlack,
		     kBlack,kBlack,kBlack,kBlack};
  int ic,iam=0;
  for (ic=0;ic<mxC;ic++) { 
    for (iam=0;iam<mxAmp;iam++) {
      char name[100];
      sprintf(name,"%s*%s",ampL[iam],cutL[ic]);
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
    int ir=0;
    for(ir=0;ir<mxR;ir++) {
      char name[100], tit[200];
      sprintf(name,"chi2R%d*%s",ir+1,cutL[ic]);
      sprintf(tit,"chi2 for R%d(#Phi), LCP cut=%s",ir+1,cutL[ic]);
      //printf("ic=%d ir=%d name=%s=\n",ic,ir,name);
      TH1F *h=new TH1F(name, tit,20,0,4);
      assert(nGr<mxGr);
      grL[nGr++]=(TGraphErrors*)h;
    }
  }

}

//==========================================
//==========================================
void plotTG(char *cut, int flag=0, TString plotDir="./") {
  printf("plot cut -->%s\n",cut);
  
  c=new TCanvas(cut,cut,900,700);
  c->Divide(4,3);
  int ampPan[mxAmp]={2,6,9,11,1,3,5,7,10};
  int chi2Pan[mxR]={4,8,12};

  int iam=0;
  for (iam=0;iam<mxAmp;iam++) {
    char name[100];
    sprintf(name,"%s*%s",ampL[iam],cut);
    TGraphErrors * gr=fetchTG(name);  
    int n=gr->GetN();
    printf("\niam=%d  name='%s', N=%d\n",iam,name,n);
    // gr->Print();
    if(n<=0) continue;
    c->cd(ampPan[iam]);
    gr->Draw("AP");
    gr->Fit("pol0");

    // draw +/- sigma of the fit
    TF1 *ff=gr->GetFunction("pol0");
    assert(ff);
    float val=ff->GetParameter(0);
    float err=ff->GetParError(0);

    float chi2ndf=0;
    if (ff->GetNDF()>0) 
     chi2ndf=ff->GetChisquare()/ff->GetNDF();

    drawFitError(gr);  // draw +/- sigma of the fit
    c->Update();
    modifyLabels();
    
    float xSig=fabs(val)/err;
    char cKey='N';
    if(xSig>2) cKey='Y';
     
     printf("#ampl= %s , cut= %s , nFill= %d , <y>= %f , sig<y>= %f , xSig= %.1f , nonZero= %c\n",
	   ampL[iam], cut,gr->GetN(), val,err, xSig,cKey);
    printf("## , %s , %s ,  %f ,  %f , %f\n",
    	  ampL[iam], cut, val,err, chi2ndf);
  }

  int ir=0;
  for(ir=0;ir<mxR;ir++) {
    char name[100];
    sprintf(name,"chi2R%d*%s",ir+1,cut);
    TH1F *ho=(TH1F*)fetchTG(name);  
    assert(ho);
    c->cd(chi2Pan[ir]);
    ho->Draw();
    printf("%s nE=%d\n",ho->GetName(),ho->GetEntries());
    c->Update();
    modifyLabels();
  }

  if(flag<=0) return;
  TString outFig=plotDir+"asy";
  outFig+=cut;
  if(flag&1) c->Print(outFig+".ps");
  if(flag&2) c->Print(outFig+".gif");


}

//--------------------------------------------------------
//--------------------------------------------------------

void   storeVal(TGraphErrors *gr, float x, float y,float ey,float fac, float efac=0){
  assert(gr);

  int n=gr->GetN();
  double val=y/fac;

  double e1=ey/y;
  double e2=efac/fac;
  double err=fabs(val)*sqrt(e1*e1+e2*e2);
  // printf("%f %f %f %f\n",e1,e2,ey/fabs(fac),err);
  gr->SetPoint(n,x,val);
  gr->SetPointError(n,.0,err);

  //  printf("add: %s %f=fac  y=%f ey=%f val=%f err=%f\n",gr->GetName(),fac,y,ey,val,err);
}


//==========================================
//==========================================
void nicePlot(){
  
  c=new TCanvas();

 TGraphErrors * gr=fetchTG("a1:P*All");  
 //TGraphErrors * gr=fetchTG("c2:PQ*All");  
 int n=gr->GetN();
  printf("\n  name='%s', N=%d\n",gr->GetName(),n);
  assert(n>0);
  // gr->Print();
  gr->Draw("AP");
  gr->Fit("pol0");
  
  drawFitError(gr);  // draw +/- sigma of the fit
 
  return;
   c->Update();
  TPaveStats *st1  =( TPaveStats *)gPad->GetPrimitive("stats");
  st1->SetX1NDC(0.4);
  st1->SetX2NDC(1.);
  st1->SetY1NDC(.7);
  st1  =( TPaveStats *)gPad->GetPrimitive("title");
  st1->SetX2NDC(.4);
  st1->SetY1NDC(.8);
  c->Update();

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
  ax->SetTitle("RHIC FILL \#");
  //printf("x1=%f x2=%f\n",x1,x2);
  
  TLine *ln0=new TLine(x1,0.,x2,0.);
  x1+=2;
  x2-=2;
  TLine *upL=new TLine(x1,upY,x2,upY);
  TLine *dwL=new TLine(x1,dwY,x2,dwY);
  upL->SetLineStyle(3);
  dwL->SetLineStyle(3);

  upL->SetLineColor(col);
  dwL->SetLineColor(col);
  
  upL->Draw();
  dwL->Draw();
  ln0->Draw();
  
  float xSig=fabs(val)/err;
  char cKey='N';
  if(xSig>2) cKey='Y';
  printf(" nData= %d , <y>= %f , sig<y>= %f , xSig= %.1f , nonZero= %c\n",
	 gr->GetN(), val,err, xSig,cKey);

}



//==========================================
//==========================================
void   modifyLabels(){
  TPaveStats *st1  =( TPaveStats *)gPad->GetPrimitive("stats");
  st1->SetX1NDC(0.3);
  st1->SetX2NDC(1.);
  st1->SetY1NDC(.8);
  return;
  st1  =( TPaveStats *)gPad->GetPrimitive("title");
  st1->SetX1NDC(.6);
  st1->SetX2NDC(1.0);
  st1->SetY1NDC(.1);
  st1->SetY1NDC(.3);
  

}



