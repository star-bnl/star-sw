// simulates population of spin dependent W yields, including background
// version 2.0
/* mapping of spin4-index to helicity at STAR  */   
enum { ka=10,  /* STAR pol B+ Y +  */
       kb=9,   /* STAR pol B+ Y -  */
       kc=6,   /* STAR pol B- Y +  */
       kd=5,   /* STAR pol B- Y -  */
       mxSS=4, 
       mxEta=8, /* # of used STAR eta bins */
       mxQ=2   /*0=W+, 1=W- */
}; 

int kA[mxSS]={ka,kb,kc,kd}; // mapping of  of valid spin4 values to blue & yellow polarization

TRandom3 *rnd=0;

//=================================
void genWalgoYields(int nEve=6e2, double pol1=0.5,double pol2=0.99 , double lumiSpread=0.1){
  gROOT->LoadMacro("setWALmodel.C");
  TString outF="run12toySetB";

  rnd=new TRandom3();
  //rnd->SetSeed(1234); // fix random sequence
  rnd->SetSeed(0); // job-unique random sequence

  // create output histos
  TH1F * hLumi=newSpin4Histo("AEta8spinY1","relative lums from toy MC");

  TH1F * hWyield[mxQ][mxEta];
  for (int iq=0;iq<mxQ;iq++) {
    TString Q="P"; if (iq) Q="N";
    for(int k=0;k<mxEta;k++) {
      int etaBin=k+1;
      TString cEta=Form("Eta%d",k+1);
      hWyield[iq][k]=newSpin4Histo("A"+cEta+"spinY2_"+Q,"charge="+Q+", thrown W+BG events from toy MC, starEtaBin="+cEta);
      //printf("k=%d iq=%d\n",k,iq);
    }
  }
  
  //------- generate lumi  model -------
  double lumi4[mxSS], prob4[mxSS];
  for(int is=0; is<mxSS; is++) {
    float val=1.+rnd->Uniform(lumiSpread);
    lumi4[is]=val;
    hLumi->SetBinContent(kA[is]+1,val);
  }
  TString core="2012";
  TString bgName="8.15.12/bkgdBetaModel_"+core+".hist.root";
  TFile *fdBG=new TFile(bgName); assert(fdBG->IsOpen());
  TFile *fdWAL=new TFile("WALModel.hist.root"); assert(fdWAL->IsOpen());

  printf(" generate W algo yields, nEve=%d  year=%s pol1=%.2f, pol2=%.2f  lumiSpread=%.2f\n",nEve,core.Data(), pol1, pol2,lumiSpread);
  
  //fdWAL->ls(); //  fdBG->ls();

  for( int iq=0; iq<mxQ; iq++) {
    TString Q="P"; if (iq) Q="N";
    // if(iq==1) continue; // tmp work only with + charge
    
    // Ws
    TH1F *hWAL=fdWAL->Get("modW_AL_"+Q); assert(hWAL);
    TH1F *hWALL=fdWAL->Get("modW_ALL_"+Q); assert(hWALL);
    // background
    // TH1F *hAlpha=fdBG->Get("alpha"+Q+"_"+core); assert(hAlpha);
    TH1F *hBeta=fdBG->Get("beta"+Q+"_"+core); assert(hBeta);
    
    // loopp over eta bins mapped to different eta bins
    for( int starPhysEtaBin=1;   starPhysEtaBin<=6;  starPhysEtaBin++) {
      int nThrow=nEve/6 *(1.-iq*0.3);
      if (starPhysEtaBin>=5)  nThrow/=5;
      printf("\n\n........ processing Q=%s ..... starEtaBin=%d  ....nThrow=%d\n",Q.Data(),starPhysEtaBin,nThrow);
      
      //map starEta on to polBeam eta
      int polBeam1EtaBin=12+starPhysEtaBin; 
      int polBeam2EtaBin=17-starPhysEtaBin; 
      printf("starPhysEtaBin=%d --> polBeam1=%d polBeam2=%d\n",starPhysEtaBin,polBeam1EtaBin,polBeam2EtaBin);
      
      // compute probabilities ....
      double WAL1=hWAL->GetBinContent(polBeam1EtaBin);
      double WAL2=hWAL->GetBinContent(polBeam2EtaBin);
      double WALL=hWALL->GetBinContent(polBeam1EtaBin); // does not matter which etaBin - it is even-function
 
      //      double alpha1=hAlpha->GetBinContent(polBeam1EtaBin);
      // double alpha2=hAlpha->GetBinContent(polBeam2EtaBin);
      double beta=hBeta->GetBinContent(starPhysEtaBin);
      
      // tmp
      double alpha1=0, alpha2=0;
      // beta=starPhysEtaBin/10.;

      //------- generate pol x-section model ------
      getProbOneEta( pol1, pol2,WAL1, WAL2, WALL, alpha1,alpha2, beta, lumi4, prob4);
      
      //---------- generate events  ---------
      for( int ieve=0; ieve<nThrow; ieve++) {
	int spin4=throwSpin4( prob4 );
	int kBin=starPhysEtaBin-1;
	//printf("eve=%d got spin4=%d kBin=%d\n",ieve,spin4,kBin);
	hWyield[iq][kBin]->Fill(spin4);
	
      }
    }//..... end of loop over eta bins

    // compute yields for B-only and E-only bins, no overlap assumed
    // Endcap :   starPhysEtaBin7=5+6
    hWyield[iq][7-1]->Add( hWyield[iq][6-1]);
    hWyield[iq][7-1]->Add( hWyield[iq][5-1]);
    for(int jj=0;jj<4;jj++)
      hWyield[iq][8-1]->Add( hWyield[iq][jj]);

  }//....... end of loop over + - charge
  
  TFile *fdOut=new TFile(outF+".wana.hist.root","recreate"); assert(fdOut->IsOpen());
  printf("saving histo w/ events to %s\n",fdOut->GetName());
  // loopp over all eta bins 
    for(int k=0;k<mxEta;k++) {
    TString cEta=Form("Eta%d",k+1);
    fdOut->mkdir(cEta);
    fdOut->cd(cEta);
    for (int iq=0;iq<mxQ;iq++) hWyield[iq][k]->Write();
    //fdOut->ls();
    fdOut->cd();
  }
  
  fdOut->cd("Eta8");
  hLumi->Write();

  //fdOut->ls();
  fdOut->Close();

}


//=====================
int throwSpin4(double *prob4) { // assigne event to one of 4 spin states
  double x=rnd->Uniform();
  for(int is=0; is<mxSS; is++)
    if(x<prob4[is]) break;
  assert(is<mxSS); // probability should not exeed 2.
  int spin4=kA[is];
  return spin4;
}

//=====================
void getProbOneEta(double P1, double P2, double WAL1, double WAL2, double WALL,  
                   double alpha1, double alpha2, double beta,  double *lumi, 
                   double *prob ) {  // output
  printf("getProbOneEta INPUT: pol1=%.2f, pol2=%.2f, W: AL1=%g AL2=%g  ALL=%g \n",P1,P2,WAL1,WAL2,WALL);
  printf("input lumis: ");  for(int is=0;is<mxSS; is++) printf("%8.3f ", lumi[is]);printf("\n");
  if(beta<0.1) {printf("changed beta WARN\n"); beta=0.9;}
  printf("input BG: beta=%.3f  alpha1=%.3f  alpha2=%.3f\n",beta,alpha1, alpha2);
 
  double ppW , pnW, npW, nnW ; // spin dependent x-sections, relative
  
  ppW=lumi[0]*(1. +(WAL1*beta +alpha1)*P1 +( WAL2*beta +alpha2)*P2 +WALL*beta*P1*P2 );
  pnW=lumi[1]*(1. +(WAL1*beta +alpha1)*P1 -( WAL2*beta +alpha2)*P2 -WALL*beta*P1*P2 );
  npW=lumi[2]*(1. -(WAL1*beta +alpha1)*P1 +( WAL2*beta +alpha2)*P2 -WALL*beta*P1*P2 );
  nnW=lumi[3]*(1. -(WAL1*beta +alpha1)*P1 -( WAL2*beta +alpha2)*P2 +WALL*beta*P1*P2 );
  
  // get cumulative probabilities after folding in lumi asymmetries
  prob[0]=  0.   +ppW;   
  prob[1]=prob[0]+pnW;
  prob[2]=prob[1]+npW;
  prob[3]=prob[2]+nnW;
  
  for(int is=0;is<mxSS; is++) prob[is]/=prob[3]; // renormalize probabilities
  
  printf("computed probs:%8.3f ", prob[0]);  for(int is=1;is<mxSS; is++) printf("%8.3f ", prob[is]-prob[is-1]); printf("\n");
  
 // Returns relative probabilities for 4 spin states for 'signal' yields
 // (including  pol & unpol backgrounds)
}
