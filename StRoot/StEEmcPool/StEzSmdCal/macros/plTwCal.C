// compares gains from various calibrations for fixed eta bin
const int mxEta=12, mxPhi=60, mxVer=5;
float g[mxVer][mxEta][mxPhi];
float eg[mxVer][mxEta][mxPhi];
TH1F *hX[mxEta];
int sec1=1, sec2=12;
int iphi1=5*(sec1-1);
int iphi2=5*(sec2);

float slFac[mxEta]={-2.26,-2.64,-2.88,-2.92,-3.00,-3.36,
		    -3.60,-3.64,-3.98,-4.30,-4.44,-4.41}; 
float iGain[mxEta];
enum { gIdeal=0, gAu200=1, gAu62=2, gFinal=3, gSlpp=4}; 

plTwCal(int keta=0 ){
  memset(g,0,sizeof(g));
  memset(eg,0,sizeof(eg));
  gStyle->SetOptStat(0);
  initHist();
  TLine *ln1=new TLine(0,0,60,60); ln1->SetLineColor(kGreen);ln1->SetLineWidth(1);
  TLine *lnV=new TLine(1,0,1,20); lnV->SetLineColor(kMagenta);lnV->SetLineWidth(1);

  //  readG(gIdeal,"../iterIdeal/");
  // readG(gFinal,"/star/u/balewski/WWW/tmp-iter12-mc/",'T');
  //..... patch for per/post gains
  // readG(gFinal,"/star/u/balewski/WWW-E/calibration/run5/absMipCal/iter6-mc-sec1to3/",'R');
  readG(gFinal,"/star/u/wissink/WWW/cal2006/tablesTowers/",'T');
  // plGainsRaw(gFinal,'T');
  plAllGains(gFinal);

  return;  
  
  //  writeG("./",gSlpp); return;// be carefull
  
  if(!keta) {
    c=new TCanvas("aa","aa",600,800);   c->Divide(3,4);
    c2=new TCanvas("bb","bb",600,800);  c2->Divide(3,4);
  } else {
    c=new TCanvas("aa","aa",400,400);   c->Divide(1,1);
    c2=new TCanvas("bb","bb",300,300);  c2->Divide(1,1);
  }

  int ieta=1;
  for(ieta=0;ieta<12;ieta++) {
    if(!keta) c->cd(ieta+1);
    else {
      if(keta!=ieta+1) continue;
      c->cd(1);
    }
    //doCorr1(gSlpp,gFinal,ieta);
    ln1->Draw();
    drawIdeal(ieta);
    gPad->SetGrid(1,1); 
    // continue;
    if(!keta) c2->cd(ieta+1);
    else c2->cd(1);
    hX[ieta]->Fit("gaus","WQ"); lnV->Draw();
    //if(ieta>1) break;
    prGainChange(gSlpp,ieta);

  }
}


//========================
//========================
int readG( int iv, char *path, char cT='T') {
  char fname[200];

  // use one common file for input
  sprintf(fname,"%sgains%c-allSect.dat",path,cT);
  printf("reading gains from %s\n",fname);    
  FILE *fd=fopen(fname, "r"); assert(fd);
  
  bool isSlope= strstr(path,"iter4xx")!=0;

  int sec=0;  
  for(sec=sec1;sec<=sec2;sec++)   {
    //if(isMipUxV && (sec<5 || sec>8) ) continue;
    
#if 0
    char fname[200];
    sprintf(fname,"%sgains%02dtower.dat",path,sec);
    if(isSlope)sprintf(fname,"%ssect%d.txt",path,sec);    
    printf("reading gains from %s\n",fname);    
    FILE *fd=fopen(fname, "r"); assert(fd);
#endif

    const int mx=1000;
    char buf[mx], name[100];
    float gx, egx;
    
    while(1) {
      char * ret=fgets(buf,mx,fd);
      if(ret==0) break;
      if(buf[0]=='#') continue;
      if(strlen(buf)<=1) continue;
      //printf("=%s=\n",buf);
      int  n=sscanf(buf,"%s %f %f",name,&gx, &egx);
      assert(n==3);
      assert(name[2]==cT);
      int isec=atoi(name)-1;
      int isub=name[3]-'A';
      int ieta=atoi(name+4)-1;
     
      int iphi=5*isec+isub;
      printf("%s %d %d %d %d\n",name, isec,isub,ieta,iphi);
      if(gx==0) continue;
      if(isSlope&&0) {// not used any more
	float r=fabs(egx/gx);
	gx=slFac[ieta]/gx; 
	egx=r*gx;
      }
      
      g[iv][ieta][iphi]=gx;
      eg[iv][ieta][iphi]=egx;
    }// loop over towers
  }// loop over sectors

}

  
//========================
//========================
int  plAllGains(int v1) {
  TGraphErrors *gr=new TGraphErrors;
  gr->SetMarkerStyle(21);
  gr->SetMarkerColor(kRed);
  gr->SetMarkerSize(0.5);
  char tit[200];
  int ieta=4;

  sprintf(tit,"2006 EEMC tower gains from MIPs w/ UxV, day89; eta bin; gain (ch/GeV)");
  //sprintf(tit,"Reco EEMC tower gains from M-C, SF=5%; eta bin; gain (ch/GeV)");
  
  gr->SetTitle(tit);
  // line below added - sww 3/8/07
  TString tt="T"; tt+="-shower";
  h=new TH2F(tt,tit,10,0.91,13.1,10,0,60);

  // line below added - sww 3/8/07
  c=new TCanvas(tt,tt,900,600);
  int iphi;

  for(ieta=0; ieta<mxEta;ieta++)
  for(iphi=iphi1;iphi<iphi2;iphi++) {
    int sec=1+(iphi/5);
    char sub='A'+iphi%5;

    float g1=g[v1][ieta][iphi];
    float eg1=eg[v1][ieta][iphi];
    float x=ieta+1.+iphi/60.;
    int n=gr->GetN();
    gr->SetPoint(n,x,g1);
    gr->SetPointError(n,0,eg1);
  }                                         
  //  gr->Print();

  h->Draw();  gr->Draw("P");
  //gr->Draw("PA");

  for(ieta=0; ieta<mxEta;ieta++) drawIdeal(ieta,1.1, 1);
  gPad->SetGridy(0);
  gPad->SetGridx(0);
  // line below added - sww 3/8/07
  c->Print(tt+".gif");
}


  
//========================
//========================
int  plGainsRaw(int v1, char cT='X') {
  TGraphErrors *gr=new TGraphErrors;
  gr->SetMarkerStyle(8);
  gr->SetMarkerColor(kRed);
  gr->SetMarkerSize(0.5);
  char tit[200];
  int ieta=4;

  sprintf(tit,"Reconstructed %c-shower gains for M-C , INPUT=23000; eta bin; gain (ch/GeV)",cT);
  
  gr->SetTitle(tit);
  TString tt=cT; tt+="-shower";
  h=new TH2F(tt,tit,10,0.91,13.1,10,0,40000);

  c=new TCanvas(tt,tt,500,400);
  int iphi;

  for(ieta=0; ieta<mxEta;ieta++)
  for(iphi=iphi1;iphi<iphi2;iphi++) {
    int sec=1+(iphi/5);
    char sub='A'+iphi%5;

    float g1=g[v1][ieta][iphi];
    float eg1=eg[v1][ieta][iphi];
    if(g1<=10) continue;
    float x=ieta+1.+iphi/60.;
    int n=gr->GetN();
    gr->SetPoint(n,x,g1);
    gr->SetPointError(n,0,eg1);
  }                                         
  //  gr->Print();

  h->Draw();  gr->Draw("P");
  //gr->Draw("PA");
 
  gr->Fit("pol1");

  ln=new TLine(0,23000,13,23000);
  ln->SetLineColor(kRed);
  ln->SetLineStyle(2);
  ln->Draw();

  gPad->SetGridy(0);
  gPad->SetGridx(0);
  c->Print(tt+".gif");
}

//========================
//========================
int writeG(char *path, int iv) {
  
  int sec=0;
  
  for(sec=sec1;sec<=sec2;sec++)   {
    if(drop58 && sec>=5 && sec<=8) continue;
    char fname[200];
    sprintf(fname,"%sgains%02dtower.dat",path,sec);
    printf("write gains from %s\n",fname);   
    FILE *fd=fopen(fname, "w"); assert(fd);
    fprintf(fd,"# gains from slopes pp200 from iter4, scaled to average from sector5-8, sector %d\n# name, gain, error\n",sec);
    int ieta,isub;
    for(isub=0;isub<5;isub++)
      for(ieta=0;ieta<12;ieta++) {
	int iphi=5*(sec-1)+isub;
	fprintf(fd,"%02dT%c%02d %.2f %.2f\n",sec,'A'+isub,ieta+1,g[iv][ieta][iphi],eg[iv][ieta][iphi]);
	//fprintf(fd,"%02dT%c%02d %.2f %.2f\n",sec,'A'+isub,ieta+1,iGain[ieta],0);
      }
    fclose(fd);
  }// loop over sectors
}


//========================
//========================
int doCorr1(int v1, int v2 , int ieta=11) {
  TGraphErrors *gr=new TGraphErrors;
  gr->SetMarkerStyle(8);
  gr->SetMarkerColor(kRed);
  gr->SetMarkerSize(0.5);
  char tit[200];
  sprintf(tit,"etaBin = %d",ieta+1);
  gr->SetTitle(tit);
  h=new TH2F(tit,tit,10,0,50,10,0,50);

  int iphi;
  for(iphi=iphi1;iphi<iphi2;iphi++) {
    int sec=1+(iphi/5);
    char sub='A'+iphi%5;
    if(drop58 && sec>=5 && sec<=8) continue;

    float g1=g[v1][ieta][iphi];
    float g2=g[v2][ieta][iphi];
    float eg1=eg[v1][ieta][iphi];
    float eg2=eg[v2][ieta][iphi];
    if(g1==0 || g2==0) continue;
    int n=gr->GetN();
    gr->SetPoint(n,g1,g2);
    gr->SetPointError(n,eg1,eg2);
    float x=g2/g1;
    hX[ieta]->Fill(x);
    // pick outleyers

    if(fabs(x-1.)>0.3)
      printf("** ");
    else 
      printf("   ");

    printf("%02dT%c%02d %f %f %f\n",sec,sub,ieta+1,g1,g2,x);

  }                                         
  //  gr->Print();
  h->Draw();  gr->Draw("P");
  //gr->Draw("PA");

}
  
//========================
//========================
int prGainChange(int v1, int ieta=11) {

  int nH=0, nL=0, nOK=0,nD=0, nTot=0;
  float eps=0.1;
  int iphi;
  printf("\n  HV tower  gain  erGain ratio (big) , eta bin=%d, ideal gain(ch/GeV)=%.2f\n",ieta+1, iGain[ieta]);


   for(iphi=iphi1;iphi<iphi2;iphi++) {
    int sec=1+(iphi/5);
    char sub='A'+iphi%5;
    //printf("%d %d %c\n", iphi,sec, sub);
    if(drop58 && sec>=5 && sec<=8) continue;
    nTot++;

    float g1=g[v1][ieta][iphi];
    float eg1=eg[v1][ieta][iphi];
    float r=g1/ iGain[ieta];
    // pick outleyers
    
    if(g1<=0) { printf("# xx "); nD++; }
    elseif(r>1+eps) { printf("# +  "); nH++; }
    elseif(r<1-eps) { printf("# -  "); nL++; }
    else { printf("# OK "); nOK++; }

    printf("%02dT%c%02d %4.1f   %.1f   %.3f",sec,sub,ieta+1,g1,eg1,r);

    if(fabs(r-1.)>0.3)
      printf(" ** \n");
    else 
      printf("    \n");

  }                                         
   printf("nH=%d nL=%d nOK=%d nD=%d nTot=%d\n",nH,nL,nOK,nD,nTot);
   printf("<tr><th> x <td>%d <td>%d <td>%d <td>%d \n",nH,nL,nOK,nD);

}
  
//===========================
//===========================
initHist() {
  const float maxADC=4095;
  const float maxEt=60;
  const float eta[mxEta]={1.95,1.855,1.765,1.675,1.59,1.51,1.435,1.365,1.3,1.235,1.17,1.115};

  char tit[200], tit2[200];
  int ieta;
  for(ieta=0;ieta<12;ieta++) {
    sprintf(tit,"x%d",ieta+1);
    sprintf(tit2,"MIPgain*slope , etaBin=%d",ieta+1);
    hX[ieta]=new TH1F(tit,tit2,20,.5,1.5);
    iGain[ieta]=maxADC/maxEt/cosh(eta[ieta]);
    printf("%.3f ", iGain[ieta]);
  }
    printf(" <- ideal gains\n");
}


//=================================
void drawIdeal(int ieta, float fac=1.1, int flag=0) {
  float x1=0, x2=60;
  if(flag) { x1=ieta+1; x2=x1+1; } 
  TLine *ln0 =new TLine(x1,iGain[ieta],x2,iGain[ieta]); ln0->SetLineColor(kBlue);ln0->SetLineWidth(1);    ln0->Draw();
   if(flag)  ln0->SetLineWidth(2);

   ln0 =new TLine(x1,iGain[ieta]*fac,x2,iGain[ieta]*fac); ln0->SetLineColor(kBlue);ln0->SetLineWidth(1);    ln0->Draw();ln0->SetLineStyle(2);

   ln0 =new TLine(x1,iGain[ieta]/fac,x2,iGain[ieta]/fac); ln0->SetLineColor(kBlue);ln0->SetLineWidth(1);    ln0->Draw();ln0->SetLineStyle(2);


}

