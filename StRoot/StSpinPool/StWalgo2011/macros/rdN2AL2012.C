/* converts raw inputs yields in to physics asymetries vs. polBeam-eta
   version 2.2

INPUT:
  - BGmodel(year) as root hists
  - Wyield+lumis(year) as root histo (STAR data or toy MC)
  - polarization(year) as input params

Computation is done in 2 steps:
1) in loop over starPhys-eta bins 
AL,ALL,EpsNull are computed  for both beams * background and stored in tempHistos bins [1-8]

2) SSA, DSA from tempHistos are combined according to polBeam-eta index [10-20] and store in final histos

OUTPUT : final histos are saved vs.  polBeam eta float.

Now WB & WE are used with the following hardcoded rules:
bins 1-4 : only WB
bin 5 : WB+WE
bin 6,7 : only WE
bin 8 :  only WB
*/

/* mapping of spin4-index to helicity at STAR  */   
enum { ka=10,  /* STAR pol B+ Y +  */
       kb=9,   /* STAR pol B+ Y -  */
       kc=6,   /* STAR pol B- Y +  */
       kd=5,   /* STAR pol B- Y -  */
       mxSS=4, 
       mxEta=8, /* # of used STAR eta bins */
       mxQ=2   /*0=W+, 1=W- */
};
 
int kA[mxSS]={ka,kb,kc,kd};
int minYieldPerSpin=1; // cut-off to drop whole eta bin due to not reliable error propagation

// working histos to store AL, ALL from single beams vs. starEtaBin
TH1F * hAL1[mxQ],  *hAL2[mxQ],  *hALL[mxQ],  *hEpsNull[mxQ],  *hNused[mxQ], *hBeta[mxQ];
// final histos to store AL from both beams vs. polBeamEtaBin
TH1F * hALfin[mxQ],* hLumi;
double SumYieldPerSpin[mxQ][mxEta][mxSS]; // only for nice printing at the end, not saved

TFile *fdWAL=0;
TFile *fdBG=0;
TFile *fdData=0;
int addEndcap=1; // include WE events in AL computation
int skipWminus=0; // used '1' for testing

//=================================
void rdN2AL2012(TString inpCore="run12toySetB", TString bgYear="2012", double pol1=0.5,double pol2=0.99 ) {
  //  inpCore="8.15.12/run9long";pol1=0.376;  pol2=0.395 ;
  //inpCore="../8.24.12/run12long";pol1=0.55;  pol2=0.57 ;
  inpCore="../results_August28/8.29.12/run12long";pol1=0.549;  pol2=0.571;
  TString bgName="8.15.12/bkgdBetaModel_"+bgYear+".hist.root";
 
  //  fdWAL=new TFile("WALModel.hist.root"); assert(fdWAL->IsOpen()); // activate to plot on final page

  gROOT->LoadMacro("setWALmodel.C");

  fdBG=new TFile(bgName); assert(fdBG->IsOpen());
  //fdBG->ls();

  TString dataName=inpCore+".wana.hist.root";
  fdData=new TFile(dataName); assert(fdData->IsOpen());
  printf("Opened %s\n",fdData->GetName());
  //fdData->ls();

  memset(SumYieldPerSpin,0,sizeof(SumYieldPerSpin)); // just in case
  //......... access & store lumis  (works: )
  TH1F *hAbsLum=(TH1F *)fdData->Get("Eta8/AEta8spinY1"); assert(hAbsLum); //hAbsLum->Draw();
  hLumi=newSpin4Histo("relLumi","renormalized relative lums");// just monitor
  double sum=0;
  for(int is=0; is<mxSS; is++){
    int bin=kA[is]+1;
    double y= hAbsLum->GetBinContent(bin);
    sum+=y;
    hLumi->SetBinContent(bin,y);
    //printf("y=%.0f\n",y);
  }
  hLumi->Scale(mxSS/sum);
  for(int is=0; is<mxSS; is++)
    hLumi->SetBinError(kA[is]+1,1./2/sqrt(sum));

  double rLumi[mxSS],sLumi[mxSS];
  for(int is=0; is<mxSS; is++) {
    rLumi[is]= hLumi->GetBinContent(kA[is]+1);
    sLumi[is]= hLumi->GetBinError(kA[is]+1);
  }
  //hLumi->Fit("pol0");
  printf("renorm lumis: %.4f %.4f %.4f %.4f  QCD eve count=%.1e\n", rLumi[0],rLumi[1],rLumi[2],rLumi[3],sum);
  printf(" have errors: %.4f %.4f %.4f %.4f  1/2/sqrt(N)=%.1e\n", sLumi[0],sLumi[1],sLumi[2],sLumi[3],1./2/sqrt(sum));
 

  TH1F *h; TGraphErrors *gr;

  for( int iq=0; iq<mxQ; iq++) {
    if(skipWminus && iq==1) continue; // tmp work only with + charge
    TString Q="P"; if (iq) Q="N";

    hNused[iq]=newEtaBinHisto("Nused_"+Q, " Lum-norm. events, Q="+Q,"EVENTS");  

    hAL1[iq]=h=newEtaBinHisto("AL1_"+Q, "beam 1 AL (starEta)  , Q="+Q, "A_L");  
    h->SetMarkerStyle(22+iq*4); // tringleUp 22:26
    h->SetMarkerSize(1.3);

    hAL2[iq]=h=newEtaBinHisto("AL2_"+Q, " beam 2 AL (starEta) , Q="+Q, "A_L");
    h->SetMarkerStyle(21+iq*4); // square 21:25

    hALL[iq]=h=newEtaBinHisto("ALL_"+Q, " beam 1*2 ALL, Q="+Q, "A_LL"); 
    h->SetMarkerStyle(23+iq*9); // tringleDown 23:32
    h->SetMarkerSize(1.3);

    hEpsNull[iq]=h=newEtaBinHisto("EpsNull_"+Q, "EpsNull  , Q="+Q, "Eps_null");
    h->SetMarkerStyle(21+iq*4); // square 21:25

    hALfin[iq]=h=newEtaBinHisto("finAL_"+Q, "AL (polBeamEta)  , Q="+Q, "A_L");  
    h->SetMarkerStyle(21+iq*4); // square 21:25
    
    //....... access background    
    //    TH1F *hAlpha=(TH1F *)fdBG->Get("alpha"+Q+"_"+bgYear); assert(hAlpha);
    hBeta[iq]=(TH1F *)fdBG->Get("beta"+Q+"_"+bgYear); assert(hBeta);  
  

    printf("\n\n ==========   PHASE_I =========== Q=%s\n",Q.Data());
    //   compute AL, ALL for each beam separately    
    
    //.... ADD loop over starPhysEta bins here ......
    for( int starPhysEtaBin=1;   starPhysEtaBin<=mxEta;  starPhysEtaBin++) {
      printf("\n------------ processing Q=%s ------- starEtaBin=%d  ------\n",Q.Data(),starPhysEtaBin);
      // if(starPhysEtaBin!=8) continue;
      //..... access spin sorted yields ..... 
      TH1F *hData=(TH1F *)fdData->Get(Form("Eta%d/AEta%dspinY2_",starPhysEtaBin,starPhysEtaBin)+Q); assert(hData); 
      TH1F *hDataE=(TH1F *)fdData->Get(Form("Eta%d/AEta%dspinEY2_",starPhysEtaBin,starPhysEtaBin)+Q); assert(hDataE); 

      //hData->Draw();     
      if(addEndcap && ( starPhysEtaBin==5 )) hData->Add(hDataE);
      if( starPhysEtaBin==7 ||  starPhysEtaBin==6) {
	if (addEndcap ) hData=hDataE;
	else hData->Reset();
      }
      // eta histo : AEta7spinELepEta
      // Endcap yiled : AEta7spinEY2_N

      double beta =hBeta[iq] ->GetBinContent(starPhysEtaBin);
      double betaErr=hBeta[iq] ->GetBinError(starPhysEtaBin);

      //tmp-start
      if(beta<0.1) {printf("changed beta WARN\n"); beta=0.9;}
      double alpha1=0, alpha2=0; 
      double alphaErr1=0, alphaErr2=0;
      //tmp-end
            
      double M[mxSS]; //yields  corrected for lumi
      double VM[mxSS]; // variances corrected for lumi
      //...... access Walgo yields
      printf("raw Wyields iQ=%d  starEtaBin%d:  ",iq,starPhysEtaBin);
      double sum=0;
      int skipBin=0;
      for(int is=0; is<mxSS; is++) {
	double val= hData->GetBinContent(kA[is]+1); // raw yield , events
	if (val<minYieldPerSpin) skipBin++;
	M[is]=val/ rLumi[is];
	VM[is]=val/ rLumi[is]/ rLumi[is];
	sum+=val;
	SumYieldPerSpin[iq][starPhysEtaBin-1][is]=M[is]; 
	//printf(" M[%d]=%.1f V=%.1f; ",is,M[is], VM[is]);
	printf("  %.0f  ",val);
      }
      
      printf("  sumN=%.1f  skipBin=%d\n",sum,skipBin);
      if(skipBin) { printf(" SKIP this starEtaBin due to low stats\n"); continue;}
      printf("alpha1: %.4f +/- %.4f, ",alpha1, alphaErr1);
      printf("alpha2: %.4f +/- %.4f, ",alpha2, alphaErr2);
      printf("beta: %.4f +/- %.4f\n",beta,betaErr);
      
      hNused[iq]->SetBinContent(starPhysEtaBin,sum); 

      double A,sA; // physics asymmetry + error
      double eps, sEps; //raw  asymmetry + error
 
      if(starPhysEtaBin!=8) {// do sth else for full-barrel
	printf(".....................  AL for beam 1 \n");
	computeAL(pol1, alpha1,alphaErr1, beta,betaErr,
		  M[0]+ M[1],   M[2]+ M[3],
		  VM[0]+VM[1],  VM[2]+VM[3], A, sA);
	hAL1[iq]->SetBinContent(starPhysEtaBin,A);    
	hAL1[iq]->SetBinError  (starPhysEtaBin,sA); 
		
	printf(".....................  AL for beam 2 \n");
	computeAL(pol2, alpha2,alphaErr2, beta,betaErr,
		  M[0]+ M[2],   M[1]+ M[3],
		  VM[0]+VM[2],  VM[1]+VM[3], A, sA);
	hAL2[iq]->SetBinContent(starPhysEtaBin,A);    
	hAL2[iq]->SetBinError  (starPhysEtaBin,sA); 
      } else { // special case for full barrel
	printf(".....................  AL for beam 1+2 over Barrel \n");
	doEps_II(  M[0],   M[3],  M[1]+ M[2], 
		  VM[0],  VM[3], VM[1]+VM[2],   eps, sEps);
	printf("epsALsym= %.3f $\\pm$ %.3f nSig=%.2f \n", eps, sEps,  eps/ sEps);
	// use alpha from beam 1 only, should be identical for this case
	doPolBckgCorr(eps, sEps,(pol1+pol2)/2., alpha1, alphaErr1, beta, betaErr, A, sA); 
	printf("ALsym=%.3f +/- %.3f nSig=%.2f\n", A, sA, A/sA);
	hAL1[iq]->SetBinContent(starPhysEtaBin,A);    
	hAL1[iq]->SetBinError  (starPhysEtaBin,sA); 	
      }
      
      printf(".....................  ALL for beam 1*2 \n");
      // (sic!) - the same formula is used for AL() and ALL()
      computeAL(pol1*pol2, 0. , 0., beta,betaErr, 
	 	 M[0]+ M[3],   M[1]+ M[2],
		VM[0]+VM[3],  VM[1]+VM[2], A, sA);
      
      hALL[iq]->SetBinContent(starPhysEtaBin,A);    
      hALL[iq]->SetBinError  (starPhysEtaBin,sA); 
      
      
      printf(".....................  EpsNULL  \n");
      doEps_II(  M[1],   M[2],  M[0]+ M[3], 
		VM[1],  VM[2], VM[0]+VM[3],   eps, sEps);
      printf("epsNull_not= %.3f $\\pm$ %.3f nSig=%.2f \n", eps, sEps,  eps/ sEps);
      hEpsNull[iq]->SetBinContent(starPhysEtaBin,eps);    
      hEpsNull[iq]->SetBinError  (starPhysEtaBin,sEps); 
           
    } // ----- end of starEtaBins loop

    // now SSA, DSA are computed per beam , merging of results is done below 
    //     combine AL, ALL from 2 beams in to single value  

    printf("\n\n ==========   PHASE_II =========== Q=%s\n",Q.Data());
    
    // work on polBeam-eta slices
    int k1,k2;
    for (int k=10;k <=20; k++) { // index polBeamEtaBin
      printf("\n........ processing iQ=%d ..... polBeamEtaBin=%d  ....\n",iq,k);
      k1=k-12; // for beam 1
      k2=17-k; // for beam 2
      if(k==20) { k1=8; k2=-999; } // special case: barrel Ws 

      double  N1=0, N2=0;
      if(k1>0) 	N1 = hNused[iq]->GetBinContent(k1);      
      if(k2>0)  N2 = hNused[iq]->GetBinContent(k2);
      hNused[iq]->SetBinContent(k,N1+N2);
      if(N1+N2<=0) continue;  // skip if neither beam have any contribution

      printf(".....................  AL from both  beams \n");
      doOneBinAverage(k1,k2, hAL1[iq], hAL2[iq],k, hALfin[iq]);
      if(k>=15) {
	printf(".....................  ALL from both  beams \n");
	doOneBinAverage(k1,k2, hALL[iq], hALL[iq],k, hALL[iq]);
 	printf(".....................  EpsNull from both  beams \n");
	doOneBinAverage(k1,k2, hEpsNull[iq], hEpsNull[iq],k, hEpsNull[iq]);
      }
    } // end of loop over polBeamEtaBins

    // hALfin[iq]->Fit("pol0","","R",10.5,18.5);
  }// ---- end of 2nd  + - charge loop
  
  // return;
  TString outName=inpCore+".wasy.hist.root";
  TFile *fdOut=new TFile(outName,"recreate"); assert(fdOut->IsOpen());
  //-------- final plots and printouts 
  for( int iq=0; iq<mxQ; iq++) {
    if(skipWminus && iq==1) continue; // tmp work only with + charge
  
    makeNiceSummaryPlotPerBeam(inpCore, iq);
    makeNiceSummaryPlotSumBeam(inpCore, iq);
    printNiceTable(inpCore, iq, pol1, pol2);
    hNused[iq]->Write();
    hEpsNull[iq]->Write();
    hALL[iq]->Write();
    hBeta[iq]->Write();
    hALfin[iq]->Write();
    saveEtaSpectra(iq);
    
  }
  hLumi->Write();
  hAbsLum->Write();
  
}




//-----------------------------------
void  doOneBinAverage( int k1, int k2, TH1F *h1,  TH1F *h2,int k, TH1F *hfin) {
  double A1=0, A2=0, sA1=0, sA2=0, N1=0, N2=0;
  double A1=0, A2=0, sA1=0, sA2=0;
  if(k1>0) {
    A1 = h1->GetBinContent(k1);
    sA1= h1->GetBinError(k1);
  }
  if(k2>0) {
    A2 = h2->GetBinContent(k2);
    sA2= h2->GetBinError(k2);
  }
  double A=999,sA=0.01; // asymmetry + error
  averageTwoValues(A1,sA1, A2,sA2, A,sA); // if one is missing it gets 0-weight = OK
  printf(" k=%d k1=%d k2=%d  -> A=%.3f  +/-%.3f\n",k,k1,k2,A,sA);
  hfin->SetBinContent(k,A);      
  hfin->SetBinError(k,sA);  
}


//-----------------------------------
void  averageTwoValues(double x, double sx,double y, double sy , double &A, double &sA) {
  printf("X=%.3f sX=%.3f; Y=%.3f sY=%.3f\n",x,sx,y,sy); 
  if( sx<=0.) { A=y; sA=sy; return; }  
  if( sy<=0.) { A=x; sA=sx; return; }
  double wx=1./sx/sx;
  double wy=1./sy/sy;
  double wsum=wx+wy;
  wx/=wsum; wy/=wsum;
  A=wx*x+wy*y;
  sA=sqrt(wx*wx*sx*sx +wy*wy*sy*sy);
   printf("wx=%f wy=%f --> A=%f\n",wx,wy,A);
}



//-----------------------------------
void   computeAL(double pol, double alpha,double alphaErr, double beta,double betaErr, 
		 double Ma, double Mb,double  VMa, double VMb, double &A, double &sA) {
  printf("computeAL:  pol=%.3f  Ma+Mb=%.1f\n",pol, Ma+Mb);
  printf("alpha: %.4f +/- %.4f  , ",alpha, alphaErr);
  printf("beta: %.4f +/- %.4f\n",beta,betaErr);
  printf("Ma=%.1f Va=%.1f; ",Ma, VMa);  printf(" Mb=%.1f Vb=%.1f\n",Mb, VMb);

  double eps, sEps;
  doEps_I( Ma,  Mb, VMa, VMb , eps, sEps  );
  printf("eps= %.3f $\\pm$ %.3f nSig=%.2f  sig(eps)*sqrt(Ma+Mb)=%.3f\n", eps, sEps,  eps/ sEps , sEps*sqrt(Ma+Mb));

  doPolBckgCorr(eps, sEps, pol, alpha, alphaErr, beta, betaErr, A, sA);
  printf("A=%.3f +/- %.3f nSig=%.2f\n", A, sA, A/sA);

}

//-----------------------------
void doEps_I( double a, double b, double va, double vb,
              double &eps, double &sEps) {
  printf("doEps_I: Ma=%f Mb=%f\n",a,b);
  double sum=a+b;
  eps= (a-b)/sum;
  double xx=b*b*va+ a*a*vb;
  sEps= sqrt( 4 * xx/sum/sum/sum/sum);
}



//------------
void doEps_II( double a, double b,double c, double Va, double Vb, double Vc,
               double &eps, double &sEps) {
  printf("doEps_II: Ma=%f Mb=%f  Mc=%f\n",a,b,c);
  // printf("doEps_II: Va=%f Vb=%f  Vc=%f\n",Va,Vb,Vc);
  double sum=a+b+c;
  eps= (a-b)/sum;
  double vEps=(4*a*c*Vb + c*c*(Va + Vb) +  b*b*(4*Va + Vc) +
               a*a*(4*Vb + Vc) + b*(4*c*Va - 2*a*Vc))/sum/sum/sum/sum;
  sEps= sqrt(vEps);
}



//------------------------------
void doPolBckgCorr( double eps, double sEps,double P, 
		    double a, double sa, double b, double sb,
		    double &AL, double &sAL) {
  // printf("doPolBckgCorr eps=%f P=%f a=%f b=%f\n",eps,P,a,b);
  AL=(eps/P-a)/b;
  double v1=sEps*sEps/P/P;
  double v2=sa*sa;
  double v3=AL*AL*sb*sb;
  sAL=sqrt(v1+v2+v3)/b;
  //printf("v1=%f v2=%f v3=%f  sEps/P=%f sqrt(v1)=%f sAL=%f\n",v1,v2,v3,sEps/P, sqrt(v1),sAL);
}

//------------------------
void splitPadX(float x, TPad **cL, TPad **cR) {
  (*cL) = new TPad("padL", "apdL",0.0,0.,x,0.95);
  (*cL)->Draw();
  (*cR) = new TPad("padL", "apdL",x+0.005,0.,1.0,0.95);
  (*cR)->Draw();
}
 
//------------------------
void splitPadY(float y, TPad **cU, TPad **cD) {
  (*cU) = new TPad("padD", "apdD",0,y+0.005,1.0,1.);
  (*cU)->Draw();
  (*cD) = new TPad("padU", "apdU",0.0,0.,1.,y);
  (*cD)->Draw();

  /* use case:    
     TPad *cU,*cD;   splitPadY(0.4,&cU,&cD);    cU->cd(); h->Draw()   
  */
}


//------------------------
TPad *makeTitle(TCanvas *c,TString core) {
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



//======================================
//======================================
//======================================
void  makeNiceSummaryPlotPerBeam(TString inpCore, int iq) {
  TString Q="P"; if (iq) Q="N";
 //........ nice plot with summary
  TString titC1=inpCore+"one Q="+Q;
  can=new TCanvas(titC1, titC1,600,720);
  gStyle->SetOptStat(0);
  TPad *c=makeTitle(can,inpCore+"  Q="+Q+", perBeam");
  // c->SetFillColor(kWhite);
  c->Divide(2,2);
  ln=new TLine(0,0,10,0); ln->SetLineStyle(1);ln->SetLineColor(kBlue);

  float  yMx=1.05, xMx=8.4;
  bxEE=new TBox(6.5,-yMx,xMx,yMx); 
  bxEE->SetFillColor(15);  bxEE->SetFillStyle(3944); 
 
     
  TH1F *hX[4]= {hAL1[iq],hALL[iq],hAL2[iq],hEpsNull[iq]};
  for(int ih=0;ih<4;ih++) {
    if(ih==3) yMx/=3.;
    c->cd(1+ih);  
    hX[ih]->Draw();   hX[ih]->SetAxisRange(0,xMx);
    hX[ih]->SetMinimum(-yMx);      hX[ih]->SetMaximum(yMx);
    TAxis *ax=hX[ih]->GetXaxis();
    ax->SetTitle("STAR #eta bins    "); ax->SetTitleSize(0.06); ax->SetTitleOffset(0.8);
    ax->SetLabelSize(0.05);
    ln->Draw(); 
    bxEE->Draw();
    
    if(ih!=2) {
      ar=new TArrow(0.6,-yMx*.9,6.3,-yMx*.9, 0.025);      ar->Draw(); ar->SetLineColor(8);
      tx1=new TLatex(0.8,-yMx*.8,"-0.9"); tx1->Draw(); tx1->SetTextColor(8);
      tx1=new TLatex(5.5,-yMx*.8,"+1.3"); tx1->Draw();tx1->SetTextColor(8);
      tx1=new TLatex(8.1,yMx*.8,"Barrel"); tx1->Draw();tx1->SetTextAngle(90);
      tx1=new TLatex(7.1,yMx*.74,"+Endcap"); tx1->Draw();tx1->SetTextAngle(90);
    } else {
      ar=new TArrow(0.6,-yMx*.9,6.3,-yMx*.9, 0.025,"<");      ar->Draw(); ar->SetLineColor(8);
      tx1=new TLatex(0.8,-yMx*.8,"+0.9"); tx1->Draw();tx1->SetTextColor(8);
      tx1=new TLatex(5.5,-yMx*.8,"-1.3"); tx1->Draw();tx1->SetTextColor(8);
      tx1=new TLatex(7.1,yMx*.75,"-Endcap"); tx1->Draw();tx1->SetTextAngle(90);
    }
   
    tx1=new TLatex(3.1,-yMx*.8,"polBeam   #eta"); tx1->Draw();tx1->SetTextColor(8);
    
    gPad->SetGridy();
  }
}




//======================================
//======================================
//======================================
void  makeNiceSummaryPlotSumBeam(TString inpCore, int iq) {

  TString Q="P"; if (iq) Q="N";
  ln=new TLine(9,0,21,0); ln->SetLineStyle(1);ln->SetLineColor(kBlue);
  //........ nice plot with summary
  TString titC1=inpCore+"sum Q="+Q;
  can=new TCanvas(titC1, titC1,700,700);
  gStyle->SetOptStat(0);
  TPad *c=makeTitle(can,inpCore+"  Q="+Q+", sumBeam");
  c->cd();
  TPad *cL,*cR;   splitPadX(0.5,&cL,&cR);  

  //------- AL plot ------- 
  TH1F *h=hALfin[iq];
  TAxis *ax=h->GetXaxis();
  ax->SetTitle("polBeam #eta bins    "); ax->SetTitleSize(0.06); ax->SetTitleOffset(0.73);
  ax->SetLabelSize(0.05);

  cL->cd();
  float  yMx=1.05, x1=9.5, x2=20.4;
  bxEE=new TBox(18.5,-yMx,x2,yMx);  bxEE->SetFillColor(15);  bxEE->SetFillStyle(3944); 
  bxEF=new TBox(x1,-yMx,10.5,yMx);  bxEF->SetFillColor(15);  bxEF->SetFillStyle(3944); 

  h->Draw(); h->SetAxisRange(x1,x2);
  h->SetMinimum(-yMx);      h->SetMaximum(yMx);

  bxEE->Draw();  bxEF->Draw(); ln->Draw(); 
  ar=new TArrow(10.6,-yMx*.85,18.3,-yMx*.85, 0.025);  ar->Draw(); ar->SetLineColor(8);
  tx1=new TLatex(10.8,-yMx*.8,"-1.3"); tx1->Draw(); tx1->SetTextColor(8);
  tx1=new TLatex(17.1,-yMx*.8,"+1.3"); tx1->Draw();tx1->SetTextColor(8);
  tx1=new TLatex(12.5,-yMx*.8,"polBeam   #eta"); tx1->Draw();tx1->SetTextColor(8);
  tx1=new TLatex(20.1,yMx*.8,"Barrel"); tx1->Draw();tx1->SetTextAngle(90);
  tx1=new TLatex(19.1,yMx*.74,"+Endcap"); tx1->Draw();tx1->SetTextAngle(90);
  tx1=new TLatex(10.1,yMx*.75,"-Endcap"); tx1->Draw();tx1->SetTextAngle(90);

  gPad->SetGridy();

  if(fdWAL) {// overlay model
    TH1F *hWAL=fdWAL->Get("modW_AL_"+Q); assert(hWAL);
    hWAL->Draw("same");
  }

  cR->cd(); cR->Divide(1,2);
  
  //------- ALL, Anull plot ------- 
  cR->cd(1);
  TPad *cRL,*cRR;   splitPadX(0.5,&cRL,&cRR);
  x1=14.5;
  for(int jj=0;jj<2;jj++) {
    h=(TH1F*)hALL[iq]->Clone(); 
    cRL->cd();
    if(jj==1) { // changes for A_NULL
      h=(TH1F*)hEpsNull[iq]->Clone();
      cRR->cd();
      yMx*=0.3;  
    }
    gPad->SetLeftMargin(0.20);
    ax=h->GetXaxis();
    float txSz=0.08;    
    ax->SetTitle("polBeam #eta bins    "); 
    ax->SetTitleSize(txSz); ax->SetTitleOffset(0.65);
    ax->SetLabelSize(txSz); ax->SetLabelOffset(-0.01);
    ax=h->GetYaxis();
    ax->SetLabelSize(txSz); //ax->SetLabelOffset(-0.01);
    ax->SetTitleSize(txSz); ax->SetTitleOffset(0.99);
    h->Draw(); h->SetAxisRange(x1,x2);
    // h->SetNdivisions(6,"y");
    h->SetMinimum(-yMx);      h->SetMaximum(yMx);
    ln->Draw();
    
    bxEE->Draw();
    
    ar=new TArrow(14.6,yMx*1.05,18.3,yMx*1.05, 0.025);  ar->Draw(); ar->SetLineColor(8);
    tx1=new TLatex(14.8,yMx*.85,"0"); tx1->Draw(); tx1->SetTextColor(8);tx1->SetTextSize(txSz);
    tx1=new TLatex(17.1,yMx*.85,"+1.3"); tx1->Draw();tx1->SetTextColor(8);tx1->SetTextSize(txSz);
    
    tx1=new TLatex(20.1,yMx*.8,"Barrel"); tx1->Draw();tx1->SetTextAngle(90);tx1->SetTextSize(txSz);
    tx1=new TLatex(19.1,yMx*.74,"Endcap"); tx1->Draw();tx1->SetTextAngle(90);tx1->SetTextSize(txSz);
    gPad->SetGridy();
    
    if(jj==0 && fdWAL) {// overlay model
      TH1F *hWALL=fdWAL->Get("modW_ALL_"+Q); assert(hWALL);
      hWALL->Draw("same");
    }
    
  }
  //------- yield plot ------- 
  cR->cd(2);
  gPad->SetLeftMargin(0.15);
  h=hNused[iq];
  h->Draw(); h->SetAxisRange(0,8.4);
  ax=h->GetXaxis();
  float txSz=0.06;    
  ax->SetTitle("STAR #eta bins                         "); 
  ax->SetTitleSize(txSz); ax->SetTitleOffset(0.9);
  ax->SetLabelSize(txSz); ax->SetLabelOffset(0.01);
  ax=h->GetYaxis();
  ax->SetLabelSize(txSz); 
  ax->SetTitleSize(txSz); ax->SetTitleOffset(0.85);
  h->Draw("h text");  h->SetMarkerSize(3.); //<-- larger text font
  h->SetFillColor(kBlue);
  h->SetMinimum(0.8);
  bxEG=new TBox(6.5,0.1,8.5,1e6);  bxEG->SetFillColor(15);  bxEG->SetFillStyle(3944); 
  bxEG->Draw();
  gPad->SetGridy();
  gPad->SetLogy();

  ar=new TArrow(0.6,1.3,6.3,1.3, 0.025);      ar->Draw(); ar->SetLineColor(8);
  tx1=new TLatex(0.8,2,"-0.9"); tx1->Draw(); tx1->SetTextColor(8);
  tx1=new TLatex(5.5,2,"+1.3"); tx1->Draw();tx1->SetTextColor(8);
  tx1=new TLatex(8.1,3,"Barrel"); tx1->Draw();tx1->SetTextAngle(90); tx1->SetTextColor(8);
  tx1=new TLatex(7.1,3,"Endcap"); tx1->Draw();tx1->SetTextAngle(90); tx1->SetTextColor(8);
  tx1=new TLatex(3.1,2,"STAR  #eta"); tx1->Draw();tx1->SetTextColor(8); tx1->SetTextColor(8);

}


//======================================
//======================================
//======================================
void  printNiceTable(TString inpCore, int iq,  double pol1=0.5,double pol2=0.5 ) {
  TString Q="P"; if (iq) Q="N";

  printf("\n******* W(eta) summary for charge=%s  INPUT=%s *********\n",Q.Data(), inpCore.Data());

  printf("lumi-corrections:  ");
  for(int is=0; is<mxSS; is++)printf("%.3f, ",hLumi->GetBinContent(kA[is]+1));

  printf(" applied\nstar-bin, sum , yield ++ +- -+ --  ,  1/sqrt(sum), beta\n");
  for( int starPhysEtaBin=1;   starPhysEtaBin<=8;  starPhysEtaBin++) {
    double val=hNused[iq]->GetBinContent(starPhysEtaBin);
    if(val<=0 ) val=0.1;
    printf("%d  %6.0f, ", starPhysEtaBin,val);
    for(int is=0; is<mxSS; is++) printf("%5.0f  ",SumYieldPerSpin[iq][starPhysEtaBin-1][is]);
    printf(", %.3f,  %.2f\n",  1./sqrt(val),  hBeta[iq]->GetBinContent(starPhysEtaBin));
  }

  printf("Spin results: pol1=%.2f  pol2=%.2f\npolBeam-bin, events, *** AL ***,sig*sqrt(M)\n",pol1, pol2);
  for (int k=10;k <=20; k++) { // index polBeamEtaBin
    double eve=hNused[iq]->GetBinContent(k);
    val=hALfin[iq]->GetBinContent(k);
    float err=hALfin[iq]->GetBinError(k);
    printf("%d  %6.0f   %.3f +/- %.3f  nSig=%.1f ,  %.2f\n",k, eve,val,err,fabs(val)/err ,  err*sqrt(eve));
  }

  printf("polBeam-bin, events,*** ALL *** ,sig*sqrt(M)\n");
  for (int k=15;k <=20; k++) { // index polBeamEtaBin
    double eve=hNused[iq]->GetBinContent(k);
    val=hALfin[iq]->GetBinContent(k);
    val=hALL[iq]->GetBinContent(k);
    err=hALL[iq]->GetBinError(k);
    printf("%d  %6.0f   %.3f +/- %.3f  nSig=%.1f ,  %.2f\n",k, eve,val,err,fabs(val)/err ,  err*sqrt(eve));
  }
  printf("polBeam-bin, events, *** NULL ***  \n");
  for (int k=15;k <=20; k++) { // index polBeamEtaBin
    double eve=hNused[iq]->GetBinContent(k);
    val=hEpsNull[iq]->GetBinContent(k);
    err=hEpsNull[iq]->GetBinError(k);
    err=hALL[iq]->GetBinError(k);
    printf("%d  %6.0f   %.3f +/- %.3f  nSig=%.1f \n",k, eve,val,err,fabs(val)/err );
  }

  printf("\******* end ************** charge=%s  ********\n",Q.Data());
}

//========================================
void saveEtaSpectra(int iq) {
  TString Q="P"; if (iq) Q="N";
  c3=new TCanvas("etaYiled_"+Q,"etaYield_"+Q); 
  c3->Divide(3,3); //gStyle->SetOptStat(1111); 

  TGraphErrors *gr= new TGraphErrors;
  gr->SetName( "finEta_"+Q);
  gr->SetTitle("final W Q=P+N  yield ;  average STAR  #eta #\pm bin rms; events");
  
  //..... access eta spectra ....
  float txSz=0.06;    
  double yyMx=70.;
  if(iq) yyMx=26;
 for( int starPhysEtaBin=1;   starPhysEtaBin<=mxEta;  starPhysEtaBin++) {

   TH1F *hEta=(TH1F *)fdData->Get(Form("Eta%d/AEta%dspinLepEta_",starPhysEtaBin,starPhysEtaBin)+Q); assert(hEta); 
   TH1F *hEtaE=(TH1F *)fdData->Get(Form("Eta%d/AEta%dspinELepEta_",starPhysEtaBin,starPhysEtaBin)+Q); assert(hEtaE); 
   hEtaE->SetFillColor(24);

   hEta->Rebin(4);hEtaE->Rebin(4);
   c3->cd(starPhysEtaBin);      gPad->SetBottomMargin(0.15);
   if( starPhysEtaBin==6 ||  starPhysEtaBin ==7) hEta=hEtaE;
   hEta->Draw(); // order matters!!!	
   if( starPhysEtaBin==5) { hEta->Add(hEtaE);hEtaE->Draw("same"); } 
   float eveEta= hEta->GetEntries();
   float avrEta= hEta->GetMean();
   float rmsEta= hEta->GetRMS();
   printf("etaBin=%d  eve=%d  Eta=%.3f +/- %.3f\n",starPhysEtaBin,eveEta, avrEta, rmsEta); 
   ttx=new TLatex(-1,yyMx*0.85,Form("%.0f eve,  #eta= %.2f",eveEta,avrEta));
   ttx->Draw();ttx->SetTextSize(1.5*txSz);ttx->SetTextColor(kBlue);
   gr->SetPoint(starPhysEtaBin-1,avrEta,eveEta);
   gr->SetPointError(starPhysEtaBin-1,rmsEta,sqrt(eveEta));
   hEta->SetMaximum(yyMx);
   hEta->SetLineWidth(2);
   ax=hEta->GetXaxis();
  
   ax->SetTitleSize(txSz); ax->SetTitleOffset(0.9);
   ax->SetLabelSize(txSz); ax->SetLabelOffset(0.01);
   ax=hEta->GetYaxis();
   ax->SetLabelSize(txSz); 
   ax->SetTitleSize(txSz); ax->SetTitleOffset(0.85); 
   
  gPad->SetGridy();
   hEta->Write() ;    hEta->SetAxisRange(-1.1,1.6);
 }

 // show yield vs. float-eta
 c3->cd(9); 	gr->Draw("A P");
 gPad->SetLogy();
 //gr->Print();  
 gr->Write();
 
}
