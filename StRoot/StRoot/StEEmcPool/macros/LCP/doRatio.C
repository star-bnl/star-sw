TFile *outH, *inpH;
const int mxR=3;


doRatio(TString fill="F2201x", TString wrkDir="./") {
  // wrkDir="/star/data04/sim/balewski/LcpRun2/maxEta1.4/";
  // wrkDir="./wrkLcp";

  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(0);

  TString fname=wrkDir+"/"+fill+".hist.root";
  
  inpH=new TFile(fname);
  
  if(!inpH->IsOpen()) {
    printf(" %s  not exits \n",fname.Data()); 
    return;
  }
  printf(" %s  opened \n",fname.Data()); 

  // inpH->ls();

  
  fname.ReplaceAll("F","rF");
  outH=new TFile(fname,"recreate");
  if(outH->IsZombie()==kTRUE) {
    printf("WARN file %s not created\n",fname.Data());
    return;
  } 

  // just trig func expansion
  double  PI=3.141592654;
  TF1 *ff;
  ff = new TF1("fCos012",Cos012,-PI ,PI,3);
  ff->SetParameters(0,0,0);  ff->SetParNames("a0","a1","a2");

  //ff = new TF1("fCos012",Cos012,-PI ,PI,4);
  //ff->SetParameters(0,0,0,0);  ff->SetParNames("a0","a1","a2","phi0");

  ff->SetLineColor(kGreen);
  
  char *cut="xxx";
  TH1F *hr[mxR];
  char *cutL="All   Pt1      Pt2      Pt3      Pt4      Pt5      Pt6      PtL      PtM      PtH      EtaBB    EtaBc    EtaFc    EtaFF    Qpos     Qneg";

  //char *cutL="All ";
  
  char *cut=strtok(cutL," ");

  do {
    int err=calcRatio(cut,hr);
    if(err) continue;
    fitRatio(hr);
    plotRatio(fill,hr);
  }  while(cut=strtok(0," "));
  outH->Write();
  outH->ls();

}

//--------------------------------------------------------
//--------------------------------------------------------
//--------------------------------------------------------
void  plotRatio(TString fill1,  TH1F **hr){
  c=new TCanvas(fill1,fill1);
  c->Divide(2,2);
  int k;

  TLine *ln1=new TLine(-3.14,0.,3.14,0.);
  
  TGraphErrors *gr=new  TGraphErrors;
    
  for(k=0;k<mxR;k++) {
    c->cd(k+1);
    hr[k]-> Draw(); 
    ln1->Draw();

    TF1 *ff=hr[k]->GetFunction("fCos012");
    int i;
    int npar=3;
    for(i=0;i<npar;i++) {
      double val=ff->GetParameter(i);
      double err=ff->GetParError(i);
      int n=gr->GetN();
      gr->SetPoint(n,i+1+5*k,val);
      gr->SetPointError(n,.0,err);
    }
  }

  c->cd(4);
  gr->Draw("AP");
  
  gr->SetMarkerSize(1.3);
  gr->SetMarkerColor(4);
  gr->SetMarkerStyle(21);
  gr->SetTitle("Fit params:  #2=Ay*P   #7=-Ay*Q  #11=A#Sigma*P*Q   #13=A#Delta*P*Q");

  TAxis *ax;
  ax=gr->GetXaxis();
  float x1=0,x2=15;
  ax->SetLimits(x1,x2);
  TLine *ln0=new TLine(x1,0.,x2,0.);
  ln0->SetLineColor(kBlue);
  ln0->Draw();

  gr->Print();
  
  
}

//--------------------------------------------------------
//--------------------------------------------------------
//--------------------------------------------------------
void  fitRatio( TH1F **hr){
  c=new TCanvas("aa","aa",50,100);
  
  int k;
  for(k=0;k<mxR;k++) {
    hr[k]-> Fit("fCos012");
    TF1 *ff=hr[k]->GetFunction("fCos012");
    assert(ff);
    if(k==0) ff->SetParNames("a0","a1","a2");
    if(k==1) ff->SetParNames("b0","b1","b2");
    if(k==2) ff->SetParNames("c0","c1","c2");
  }
}

//--------------------------------------------------------
//--------------------------------------------------------
//--------------------------------------------------------
int  calcRatio( char *cut,  TH1F **hr){
  int minContent=4; // abort ratio if any bin has less 
  printf("doRatio for cut='%s'\n",cut); 
 
  const int  mxPol=4;
  char *cpolBY[mxPol]={"B+Y+","B+Y-","B-Y+","B-Y-"};

  TH1F * h[mxPol];

  // fetch proper 1D histo
  int k;
  double sum1=0;
  double min=10000;
  for(k=0;k<mxPol;k++) {
    char name[100];
    sprintf(name,"Phi%2s%s",cpolBY[k],cut);
    h[k]=(TH1F *)inpH->Get(name);
    assert(h[k]);
    sum1+=h[k]->Integral();
    printf("%s int=%f min=%f\n",name,h[k]->Integral(),h[k]->GetMinimum());
    if(h[k]->GetMinimum()<minContent) return -1;
 }
  
  TAxis *phiAx=h[0]->GetXaxis();
  
  // create hitos with ratio
  outH->cd();
  int k;

  for(k=0;k<mxR;k++) {
    char name[100];
    sprintf(name,"r%d*%s",k+1,cut);
    char tit2[100]={"aaa bbb"};
    sprintf(tit2,"R%d(#phi), LCP cut=%s, Neve=%.0f",k+1,cut,sum1);
    hr[k]=new TH1F(name,tit2,phiAx->GetNbins(),phiAx->GetXmin(), phiAx->GetXmax());
    hr[k]->Sumw2();
  }

  // do ratio bin by bin
  double sum=0;
  int iph;
  for(iph=1;iph<=phiAx->GetNbins();iph++) {// bins are counted from 1 !
    float m[mxPol], v[mxPol];
 
    //assumption: ==={k}== Y.B=={++,+-,-+,--};  Y=>Q=-Z   B=>P=+Z
    for(k=0;k<mxPol;k++){
      m[k]=h[k]->GetBinContent(iph);
      v[k]=h[k]->GetBinError(iph);
      v[k]*=v[k]; // now it is variance
    }
    
    for(int ir=0;ir<mxR;ir++) {
      TH1F * h3=hr[ir];
      assert(h3);
      double s,s1,s2,r=900.,vs1,vs2;
      switch(ir) {
      case 0:
	s1 =m[0]+m[2];      s2=m[1]+m[3];  // AN*P
	vs1=v[0]+v[2];     vs2=v[1]+v[3]; 
	break; 
      case 1:
	s1 =m[0]+m[1];      s2=m[2]+m[3];  // -AN*Q
	vs1=v[0]+v[1];     vs2=v[2]+v[3]; 
	break; 
      case 2:
	s1 =m[0]+m[3];      s2=m[1]+m[2];  // ... *P*Q
	vs1=v[0]+v[3];     vs2=v[1]+v[2]; 
	break; 
      default:
	printf("wrong ir=%d\n",ir); assert(1==2);
      }
      
      s=s1+s2;
      if(s1<=0 || s2<=0) { 
        printf("iph=%d s1=%f s2=%f s=%f   \n",iph,s1,s2,s);
        assert(2==3);
      }

      r=(s1-s2)/s;  
      //if(ir==0)      printf("iph=%d s1=%f s2=%f ir=%d %d\n",iph,s1,s2,ir,sum); 
      double vr = 4*( s2*s2*vs1 + s1*s1*vs2 )/s/s/s/s; //accurate
      
      h3->SetBinContent(iph,r); 
      h3->SetBinError(iph,sqrt(vr)); 
      if(ir==0) sum+=s;    
    }// end of loop over ir
  }// end of loop over phi

  printf("sum=%f sum1=%f r=%f\n",sum,sum1,sum/sum1);

  return 0;
}



//  c1->Print(fname.ReplaceAll("tree.root","bx.ps"));




//--------------------------------------------------------
//--------------------------------------------------------
//--------------------------------------------------------
Double_t Cos012(Double_t *x, Double_t *par)
{
  float  PI=3.141592654;

  Float_t phi =x[0];
  Float_t A0=par[0];
  Float_t A1=par[1];
  Float_t A2=par[2];
  Float_t phi0=0; //par[3]/180*3.1416;

  Double_t  f =A0 +A1*cos(phi-phi0) + A2*cos(2*(phi-phi0));
  return f;
}


