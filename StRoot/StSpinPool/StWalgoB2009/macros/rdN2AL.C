// converts raw inputs yields in to physics asymetries
// no float, only double
// version 1.1

/* mapping of spin4 to STAR pol  */   
enum { ka=10,  /* STAR pol B+ Y +  */
       kb=9,   /* STAR pol B+ Y -  */
       kc=6,   /* STAR pol B- Y +  */
       kd=5,   /* STAR pol B- Y -  */
       mxSS=4, mxQ=2
}; 
double pol1=0.381; // bernd=0.35.8; // blue
double pol2=0.402; // bernd=0.36.3; // yellow
double betaP=1.08, betaN=1.16; //  beta=(S+B)/S for W+ & W-, tau not included
// betaP=1., betaN=1.;// only for QCD background
 
int kA[mxSS]={ka,kb,kc,kd};
TH1F *hData[mxQ];
TH1F *hAsy[mxQ];
TH1F *hLum=new TH1F("hLum","hLum", 50,0.5,50.5); // for lumi monitor and 

void rdN2AL(TString inpCore="run9setP1234") {
  TString  iPath="/star/data05/scratch/balewski/2009-Wana-SL09g-a3/data/";
  //  iPath="sortJan29XXX/moveConeEt/";  // use 'Aspin4' as signal: ET[13,20]
  //iPath="sortJan29CNI/moveConeRevMiss/";  // use 'Aspin4' as signal: ET[13,20]

  TString  oPath="out/";
  
  for(int iq=0;iq<mxQ;iq++) {
    char *tt1="hDataP";
    if(iq==1) tt1="hDataN";
    hData[iq]=new TH1F(tt1,tt1,30,0.5,30.5);
    tt1="hAsyP"; char *tt2="Positive charge";
    if(iq==1){ tt1="hAsyN"; tt2="Negative charge";}
    hAsy[iq]=new TH1F(tt1,tt2,7,0,7);
    
    //  hR->GetXaxis()->SetTitleOffset(0.4);  hR->GetXaxis()->SetLabelSize(0.06);   hR->SetMinimum(0.8);
    hAsy[iq]->GetXaxis()->SetLabelSize(0.065);
    // h->SetLineColor(kBlue);
    hAsy[iq]->SetLineWidth(2);
    char *key[]={"AL blue","AL yell","AL avr","ALL","null test","AL*  "};
    for(int i=0;i<6;i++) hAsy[iq]->Fill(key[i],0.); // preset the order of keys
    hAsy[iq]->SetMinimum(-0.95);    hAsy[iq]->SetMaximum(0.95);
  }

  TString fullOutName=oPath+inpCore+".wasy.hist.root";

  TString fullInpName=iPath;  fullInpName+=inpCore;
  fullInpName+=".wana.hist.root";
  fd=new TFile(fullInpName);
  if(! fd->IsOpen()) {
    printf("EROR: input histo file not found, quit\n",fullInpName.Data());
    return;
  } else {
    printf("Opened: %s\n",fullInpName.Data());
  }

  getLumi( fd);
  //  hLum->Draw(); gPad->SetLogy();
  
  for(int iq=0;iq<mxQ;iq++) {// Q=+, -
    printf("........processing iQ=%d .....\n",iq);
    getSignal(fd,iq,"AspinY2"); //Aspin2=default,  select here B,C for East/West
    computeAsy(iq);
  }
  fd->Close();

  fdo=new TFile(fullOutName,"recreate"); assert(fdo->IsOpen());
  hLum->Write();
  hData[0]->Write();
  hData[1]->Write();
  hAsy[0]->Write();
  hAsy[1]->Write();

  //........ nice plot with summary
  can=new TCanvas(inpCore, inpCore,500,370);
  gStyle->SetOptStat(0);
  TPad *c=makeTitle(can,(iPath+inpCore).Data());
  c->SetFillColor(kWhite); 
  c->Divide(2,1);
  ln=new TLine(0,0,7,0); ln->SetLineStyle(2);
  
  for(int iq=0;iq<mxQ;iq++) {// Q=+, -
    c->cd(1+iq);  hAsy[iq]->Draw(); ln->Draw();
    gPad->SetFillStyle(0); 
    TString tt=hAsy[iq]->GetTitle(); tt+=", unpol yield=";
    float sum=hData[iq]->GetBinContent(5); tt+=(int)sum;
    hAsy[iq]->SetTitle(tt);
   
    // add special markers for 3 physcis observables
    int myMark[mxQ][5]={{21,28,25},{20,28,24}};
    for(int kk=2; kk<=4;kk++) {
      hx=(TH1*) hAsy[iq]->Clone();
      hx->Draw("same");   hx->SetMarkerSize(2);
      hx->SetAxisRange(kk,kk);
      
      hx->SetMarkerStyle(myMark[iq][kk-2]); 
    }

 }
 
}

//**********************************************************
//.................
void getLumi( TFile *fd) {
  TH1F *h1=fd->Get("AspinY1"); assert(h1);// h1->Draw();

  double sum=0;
  for( int k=1; k<=4; k++ ){
    double y= h1->GetBinContent(kA[k-1]+1);
    sum+=y;
    hLum->SetBinContent(k,y);
  }
  hLum->SetBinContent(5,sum);

  printf(" lum total=%.0f\n", sum);
  sum/=4.;
  for( int k=1; k<=4; k++ ){
    double y= hLum->GetBinContent(k);
    printf("k=%d %.0f %.3f +/- %.3f \n",k,y, y/sum, 1/sqrt(y));  
    hLum->SetBinContent(k+10,y/sum);
  }
  hLum->SetBinContent(6,pol1);  
  hLum->SetBinContent(7,pol2);
  
  hLum->SetBinContent(8,betaP); 
  hLum->SetBinContent(9,betaN);

  // fd->ls();
}

//**********************************************************

//.................
void getSignal( TFile *fd, int iq, char *hcore) {
  TString hname=hcore;
  if(iq==0) hname+="_P";
  if(iq==1) hname+="_N";
  TH1F *h1=fd->Get(hname); assert(h1); //h1->Draw();
  
  double sum=0, sumL=0;
  for( int k=1; k<=4; k++ ){
    double y= h1->GetBinContent(kA[k-1]+1);
    printf("k=%d yield=%.0f\n",k,y);
    sum+=y;
    hData[iq]->SetBinContent(k,y);
    double L= hLum->GetBinContent(k+10);
    double yL=y/L;
    double syL=sqrt(y)/L;
    sumL+=yL;
    hData[iq]->SetBinContent(k+10,yL);
    hData[iq]->SetBinError(k+10,syL);
  }
  hData[iq]->SetBinContent(5,sum);
  hData[iq]->SetBinContent(15,sumL);

  //  hData[iq]->Draw("e text");
}

//**********************************************************
//---------------------
void computeAsy(int iq) {
  TH1F *hD= hData[iq];
  TH1F *hA= hAsy[iq];

  double P1=hLum->GetBinContent(6);
  double P2=hLum->GetBinContent(7);
  double beta=hLum->GetBinContent(8+iq);


  double Ma=hD->GetBinContent(11), sMa=hD->GetBinError(11), VMa=sMa*sMa;
  double Mb=hD->GetBinContent(12), sMb=hD->GetBinError(12), VMb=sMb*sMb;
  double Mc=hD->GetBinContent(13), sMc=hD->GetBinError(13), VMc=sMc*sMc;
  double Md=hD->GetBinContent(14), sMd=hD->GetBinError(14), VMd=sMd*sMd;
  
  printf(" yield total=%.0f  S+B/S=%.2f\n", Ma+Mb+Mc+Md,beta);
  //.....................  AL for beam 1
  double eps, sEps;
  
  doEps_I( Ma+ Mb,  Mc+ Md,
	  VMa+VMb, VMc+VMd, eps, sEps  );
  eps*=beta;  sEps*=beta;   // background correction 
  double AL=eps/P1, sAL=sEps/P1;
  hD->SetBinContent(21,eps);   hA->SetBinContent(1,AL);
  hD->SetBinError  (21,sEps);  hA->SetBinError  (1,sAL);
  // printf("eps1 %f %f %f\n", eps, sEps, 1./sqrt(Ma+Mb+Mc+Md));
  printf("AL(1) %.3f %.3f nSig=%.2f\n", AL, sAL, AL/sAL);

  //......................  AL for beam 2
  doEps_I( Ma+ Mc,  Mb +Md,
	   VMa+VMc, VMb+VMd, eps, sEps  ); 
  
  eps*=beta;  sEps*=beta;   // background correction
  AL=eps/P1; sAL=sEps/P1;

  hD->SetBinContent(22,eps);   hA->SetBinContent(2,AL);
  hD->SetBinError  (22,sEps);  hA->SetBinError  (2,sAL);
  // printf("eps2 %f %f %f\n", eps, sEps, 1./sqrt(Ma+Mb+Mc+Md));
  printf("AL(2) %.3f %.3f nSig=%.2f\n", AL, sAL, AL/sAL);

 //......................  AL for beam 1+2
  doEps_II( Ma,   Md,  Mb +Mc,
	    VMa, VMd, VMb+VMc, eps, sEps  ); 
  eps*=beta;  sEps*=beta;   // background correction
  AL=2.*eps/(P1+P2); sAL=2*sEps/(P1+P2);

  hD->SetBinContent(23,eps);   hA->SetBinContent(3,AL);
  hD->SetBinError  (23,sEps);  hA->SetBinError  (3,sAL);
  // printf("eps3 %f %f %f\n", eps, sEps, 1./sqrt(Ma+Mb+Mc+Md)/sqrt(2.));
  printf("AL(1+2) %.3f %.3f nSig=%.2f\n", AL, sAL, AL/sAL);

  //......................  ALL -regular
  doEps_I( Ma+ Md,  Mb +Mc,
	  VMa+VMd, VMb+VMc, eps, sEps  ); 
  eps*=beta;  sEps*=beta;   // background correction
  double ALL=eps/P1/P2, sALL=sEps/P1/P2;

  hD->SetBinContent(24,eps);   hA->SetBinContent(4,ALL);
  hD->SetBinError  (24,sEps);  hA->SetBinError  (4,sALL);
  // printf("eps4 %f %f %f\n", eps, sEps, 1./sqrt(Ma+Mb+Mc+Md));
  printf("ALL(reg) %.2f %.2f nSig=%.1f\n", ALL, sALL, ALL/sALL);

  //......................  ALL : PV barr = null test
  doEps_I( Mb,  Mc,
	  VMb, VMc, eps, sEps  ); 

  hD->SetBinContent(25,eps);    hA->SetBinContent(5,eps);  
  hD->SetBinError  (25,sEps);   hA->SetBinError  (5,sEps); 
  printf("eps5= Null %.3f %.3f nSig=%.2f\n", eps, sEps,  eps/ sEps );
  
  //...................... PV  ALL, both beams
  doEps_I( Ma ,  Md,
	  VMa,  VMd, eps, sEps  ); 
  eps*=beta;  sEps*=beta;   // background correction 
  AL=eps/(P1+P2); sAL=sEps/(P1+P2);

  hD->SetBinContent(26,eps);   hA->SetBinContent(6,AL);
  hD->SetBinError  (26,sEps);  hA->SetBinError  (6,sAL);
  // printf("eps6 %f %f %f\n", eps, sEps, 1./sqrt(Ma+Mb+Mc+Md));
  printf("PV ALL(eps6) %.3f %.3f nSig=%.1f\n", AL, sAL, AL/sAL);

}

//------------
void doEps_I( double a, double b, double va, double vb,
	      double &eps, double &sEps) {
  double sum=a+b;
  eps= (a-b)/sum;
  double xx=b*b*va+ a*a*vb;
  sEps= sqrt( 4 * xx/sum/sum/sum/sum);
}

//------------
void doEps_II( double a, double b,double c, double Va, double Vb, double Vc, 
	       double &eps, double &sEps) {
  double sum=a+b+c;
  eps= (a-b)/sum;
  double vEps=(4*a*c*Vb + c*c*(Va + Vb) +  b*b*(4*Va + Vc) + 
	       a*a*(4*Vb + Vc) + b*(4*c*Va - 2*a*Vc))/sum/sum/sum/sum;
  sEps= sqrt(vEps);
}



//------------------------
Void splitPadX(float x, TPad **cL, TPad **cR) {
  (*cL) = new TPad("padL", "apdL",0.0,0.,x,0.95);
  (*cL)->Draw();    
  (*cR) = new TPad("padL", "apdL",x+0.005,0.,1.0,0.95);
  (*cR)->Draw();
}

//------------------------
TPad *makeTitle(TCanvas *c,char *core) {
  c->Range(0,0,1,1);
  TPad *pad0 = new TPad("pad0", "apd0",0.0,0.95,1.,1.);
  pad0->Draw();
  pad0->cd();

  TPaveText *pt = new TPaveText(0,0.,1,1,"br");
  pt->Draw();
  TDatime dt;
  TString txt2=core;
  
  txt2+=",  ";
  txt2+=dt.AsString();
  pt->AddText(txt2);
  txt2="--";
  pt->AddText(txt2);

  c->cd();
  pad = new TPad("pad1", "apd1",0.0,0.0,1,.95);
  pad->Draw();
  return pad;
}
