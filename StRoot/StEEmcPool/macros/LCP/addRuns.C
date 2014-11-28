const int mxBx=120, mxPol=4;

char *cpolBY[mxPol]={"B+Y+","B+Y-","B-Y+","B-Y-"};
char bXselection[mxBx];
int polPattBY[mxBx];
double Lum[mxPol],Lum2[mxPol];
TFile  *outH=0;
TString cutL=" ";


//-------------------------------------
//-------------------------------------
void  updatePattern(char *run) {
  int irun=atoi(run+1);
  assert(irun>0);
  printf("Update pattern for run=%d\n",irun);

  char * polPattY,* polPattB,*dum;
  
  char *  //yellow
    selPatt="a.........k....BBBBBa.........k....pppppa.........k....YYYYY";
  dum      ="|         |         |         |         |         |        |";
  dum      ="1         11        21        31        41        51       60";   

  if(irun<3017036) { // (STAR) Pattern1  
    polPattY= "--++--++--++--++--++--++--++--++--++--++--++--++--++--+aaaaa";
    polPattB= "-+-+-+-+-+-+-+-aaaaa-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+";
    dum     = "|         |         |         |         |         |        |";
    dum     = "1         11        21        31        41        51       60";   
  } else { // (STAR) Pattern2  with 3 unpolarized bXings
    polPattY= "0-+-+-+-+-+-+-+-+-+-0+-+-+-+-+-+-+-+-+-+0-+-+-+-+-+-+-+aaaaa";
    polPattB= "0++--++--++--++aaaaa0--++--++--++--++--+0+--++--++--++--++--";
    dum     = "|         |         |         |         |         |        |";
    dum     = "1         11        21        31        41        51       60";   
  }

  int i;
  for(i=0;i<60;i++) {
    bXselection[2*i]=selPatt[i];
    bXselection[2*i+1]='i'; // ignore
  }
  
  for(i=0;i<60;i++) {
    int k=2*i;
    polPattBY[k]=-2; //default
    if     (polPattB[i]=='+' && polPattY[i]=='+' ) polPattBY[k]=0;
    else if(polPattB[i]=='+' && polPattY[i]=='-' ) polPattBY[k]=1;
    else if(polPattB[i]=='-' && polPattY[i]=='+' ) polPattBY[k]=2;
    else if(polPattB[i]=='-' && polPattY[i]=='-' ) polPattBY[k]=3;
  }

}



//-------------------------------------
//        M A I N 
//-------------------------------------
addRuns(TString fill=0, TString runL=0, TString wrkDir=0) {
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(1111111);
  
  //fill="F2201x";  runL="R2362128 R777";
  //wrkDir="/star/data04/sim/balewski/LcpRun2/setEta10/";

  // create output histo
  TString outF=wrkDir+fill+".hist.root";
  //TString outF=fill+".hist.root";
  outH=new TFile(outF,"RECREATE");
  assert(outH->IsOpen());
  printf("save outH -->%s\n", outF.Data());


  // ----- access scaler data
  TString inpScal="JoannaScal/scalVsRun.hist.root";
  scal=new TFile(inpScal);
  assert(scal->IsOpen());  

  char *run=strtok(runL.Data()," "); // init 'strtok'

  float sumIn=0;
  int nRun=0, nLum=0; // counts runs & scaler data for them
  do { // loop over runs
     printf("\n\n process run %02d '%s' \n",nRun,run);
    updatePattern(run);

    //.................. access LCP histos
    TString lcpHist=wrkDir+run+".hist.root";
    TFile * lcp=new TFile(lcpHist);
     assert(lcp->IsOpen());
    if(!lcp->IsOpen()) {
      printf("#fail %s does not open, skip\n",lcpHist.Data());
      continue;
    }
    nRun++;


    // ................  access scale data (if exist)
    TH1F *hSc=(TH1F*)scal->Get(run);
    if(hSc) { 
      printf(" use absolute normalization\n");
      sumScal(hSc); 
      calcNorm(run);
      nLum++;
    }  else { // do not scal by lumin
      printf(" NO Scaler data, --> no renormalization\n");
      for(int i=0;i<mxPol;i++) {Lum[i]=Lum2[i]=1.;}
    }
    
    //lcp->ls();

    TList *top = lcp->GetListOfKeys();
    TIter iter(top);
    TObject*ob=0;
    // ........... loop over histo with LCP
    while( ob=iter())     {
      char *name=ob->GetName();
      if(strstr(name,"PhiBx")==0) continue;
      // if(strstr(name,"PhiBxPt1")==0 && strstr(name,"PhiBxAll")==0) continue;

      TH2F *hIn=(TH2F*)lcp->Get(name);
      printf("  %10s integral=%.1f\n",name, hIn->Integral());
      if(strstr(name,"PhiBxAll"))sumIn+=hIn->Integral();
      
      if(nRun==1) createHisto1D(hIn); 
      TH1F * h[mxPol];
      TString cut=name+5;
      fetch1D(cut,h);
      accumulateLcp(hIn,h); 

    } // end of loop over histos in one run
    lcp->Close();
    delete lcp;
    
    
 } while(run=strtok(0," "));  // advance by one nam

  printf("acumulation done nRun=%d nLum=%d\n", nRun, nLum);
  assert(nLum==0 || nRun==nLum);

  if(nLum==0) autoLum("All");
  printf("left2=%s=\n",cutL.Data());
  printf("#fill %s , inp= %d ,  ",fill.Data(),sumIn);
  float sumAll=summaryLcp();
  printf(" AllEff= %.2f\n",sumAll/sumIn );
  outH->Write();

  //printLcp("PhiBxAll");


  //  printLcp("PhiBxPtM");

  // outH->ls();
  (outH->Get("PhiB+Y-All"))->Draw();
  TH1F*h1=(TH1F*) outH->Get("PhiB+Y+All");
  h1->Draw("same"); h1->SetLineColor(kRed);
  

  // hsc->Draw();

}



//-------------------------------------
//-------------------------------------
int bx2pol(int bx1) {
  if(bXselection[bx1-1]!='.') return -3;
  int pol= polPattBY[bx1-1];
  assert(pol<mxPol);
  return pol;
}

//-------------------------------------
//-------------------------------------
void   createHisto1D(TH2F *hIn){
  TString name=hIn->GetName();
  TString cut=name.Data()+5;
  cutL+=" "+cut;
  printf("create outHist for %s\n",name.Data());
  TAxis *phiAx=hIn->GetYaxis();

  outH->cd();
  int k;
  for(k=0;k<mxPol;k++) {
    TString name1="Phi";
    name1+=cpolBY[k];
    name1+=cut;
    char tit2[100]={"aaa bbb"};
    sprintf(tit2,"LCP vs. Phi/rad, pol=%s, cut=%s",cpolBY[k],cut.Data());
    printf("k=%d %s '%s'\n",k,name1.Data(), tit2);
    TH1F *h=new TH1F(name1,tit2,phiAx->GetNbins(),phiAx->GetXmin(), phiAx->GetXmax());
    h->Sumw2(); // be carefull, not use Fill(x,w) ! 
  }
}


//-------------------------------------
//-------------------------------------
void   accumulateLcp(TH2F *hIn, TH1F**h){
  TString name=hIn->GetName();
  //  printf("accu histo %s\n",name.Data());

  // fetch proper 1D histo
  int k;
  for(k=0;k<mxPol;k++) {
    TString name1=hIn->GetName();
    name1.ReplaceAll("Bx",cpolBY[k]);    
    h[k]=(TH1F *)outH->Get(name1);
    assert(h[k]);
  }

  assert(hIn);
  TAxis *X=hIn->GetXaxis();  
  int nx=X->GetNbins();
  TAxis *Yax=hIn->GetYaxis();
  assert(Yax);
  int ny=Yax->GetNbins();
  int ix,iy;
  
  double nSel=0;
  for(iy=1; iy<=ny; iy++) {
    float phi=Yax->GetBinCenter(iy);
    //intf("iy=%d phi/deg=%f\n", iy,phi/3.1416*180);
    for(ix=1;ix<=nx;ix++) {
      int pol=bx2pol(ix);
      if(pol<0) continue;
      double val=hIn->GetBinContent(ix,iy);
      if (val==0) continue;
      nSel+=val;
      TH1F * h1=h[pol];
      h1->AddBinContent(iy,val/Lum[pol]);
      // do error propagation by hand
      double err=h1->GetBinError(iy);
      h1->SetBinError(iy,sqrt(err*err+val/Lum2[pol]));
      // printf("add %d %f %d\n",pol,val, h[pol]->Integral());
    }
  } 

  // cross checks

  return;
  printf("%s --> %.1f nSel=%.1f\n",hIn->GetName(), hIn->Integral(),nSel);
  for(k=0;k<mxPol;k++) {
    TH1F *h1=h[k];
    printf("%s --> %.0f\n",h1->GetName(), h1->Integral());
  }
 
}


//-------------------------------------
//-------------------------------------
void   printLcp(char *cut){
  printf("print histo %s, format: kPhi  polBY ++ +-  -+ --\n#tot: ",cut);
  
  TH1F * h[mxPol];

  // fetch proper 1D histo
  int k;
  double sum=0;
  for(k=0;k<mxPol;k++) {
    TString name1=cut;
    name1.ReplaceAll("Bx",cpolBY[k]);    
    h[k]=(TH1F *)outH->Get(name1);
    assert(h[k]);
    sum+=h[k]->Integral();
    printf(" %.1f ", h[k]->Integral());
  }

  printf("\n#rel: ",cut);
  for(k=0;k<mxPol;k++) {
    printf(" %.3f ", h[k]->Integral()*4/sum);
  }
  printf("\n");
  TAxis *X=h[0]->GetXaxis();  
  int nx=X->GetNbins();
  int ix;
  
  for(ix=1; ix<=nx; ix++) {
    printf("%2d ",ix);
    for(k=0;k<mxPol;k++) {
      float val=h[k]->GetBinContent(ix);
      float err=h[k]->GetBinError(ix);
      printf("%8.1f +/- %5.1f,  ",val,err);
    }
    printf("\n");
  } 
}

//-------------------------------------
//-------------------------------------
void   fetch1D(TString cut,TH1F **h) {// fetch proper 1D histo
  int k;
  for(k=0;k<mxPol;k++) {
    TString name1="Phi";
    name1+=cpolBY[k];
    name1+=cut;
    h[k]=(TH1F *)outH->Get(name1);
    assert(h[k]);
  }
}

//-------------------------------------
//-------------------------------------
void sumScal(TH1F *h) {
  memset(Lum,0,sizeof(Lum)); // clear normalization
  memset(Lum2,0,sizeof(Lum2)); 

  assert(h);
  TAxis *X=h->GetXaxis();  
  int nx=X->GetNbins();
  int ix;
  int nBx=0;
  for(ix=1;ix<=nx;ix++) {
    int pol=bx2pol(ix);
    if(pol<0) continue;
    assert(pol<mxPol);
    Lum[pol]+=h->GetBinContent(ix);
    nBx++;
    // printf("%3d %c %2d  %8.1f %d \n",ix,bXselection[ix-1],polPattBY[ix-1], h->GetBinContent(ix),nBx);
  }
  
  int i;
  printf("Lum: ");
  for(i=0;i<mxPol;i++)
    printf("%d ",Lum[i]);
 
  printf(" usedBx=%d\n",nBx);

}
  
//-------------------------------------
//-------------------------------------
void calcNorm(char *name){
  int i;
  float sum=0;
  printf("\nyield ");
  for(i=0;i<mxPol;i++){
    sum+=Lum[i];
    printf("%d ",Lum[i]);
  }
  
  int id=atoi(name+1);
  //float day=(id-3000000.)/1000.;

  printf("\n#norm %s %d  ",name , id);
  sum/=4.;
  // renormalize
  for(i=0;i<mxPol;i++){
    Lum[i]/=sum;
    Lum2[i]=Lum[i]*Lum[i];
    printf("%.4f ",Lum[i]);
  }
  printf("\n");
}



//-------------------------------------
//-------------------------------------
void autoLum( char *cut0) {
  printf("autoLuminosiy using cut=%s\n",cut0);
  
  TH1F * h[mxPol];
  fetch1D(cut0,h);
  int k;
  for(k=0;k<mxPol;k++) {
    Lum[k]=h[k]->Integral();
  }// end of loop over pol  
  
  calcNorm("AutoLum7777");

  // scale all histos for all cuts 
  TString cut1=" "+cutL;
  char *cut=strtok(cut1.Data()," "); // init 'strtok'
  do {
    TH1F * h[mxPol];
    fetch1D(cut,h);
    int k;
    for(k=0;k<mxPol;k++) {
      h[k]->Scale(1./Lum[k]);
    }// end of loop over pol  
  } while(cut=strtok(0," "));  // advance by one nam
}  


//-------------------------------------
//-------------------------------------
float summaryLcp(){
  TString cut1=cutL;
  //printf("summary for cutL=%s\n",cut1.Data());
  char *cut=strtok(cut1.Data()," "); // init 'strtok'
  int sumAll=-1;
  do {
    // printf("cut:  %s\n",cut);
    float sum[mxPol];
    float sum0=0;
    TH1F * h[mxPol];
    fetch1D(cut,h);
    int k;
    for(k=0;k<mxPol;k++) {
      // printf("  %s %.1f -->",name1.Data(),h->Integral());
      sum[k]=h[k]->Integral();
      sum0+=sum[k];
    }// end of loop over pol  
    
    printf("%s= %.1f , ",cut,sum0);
    if(strstr(cut,"All"))sumAll=sum0;

  } while(cut=strtok(0," "));  // advance by one nam
  
  return sumAll;
}







  
