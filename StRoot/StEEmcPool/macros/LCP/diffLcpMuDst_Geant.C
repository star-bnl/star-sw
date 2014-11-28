TChain *chainG = new TChain("G-LCP"); // Geant
TChain *chainR = new TChain("T-LCP"); // muDst
float ptR,ptG,phiG,phiR,etaG,etaR;

int idG,idR;
int nPrimR,qR;
float vzR;

TH1F *hr[4];
TH1F *h1[4];
TH2F *h2[4];

TFile *fd=0;

diffLcpMuDst_Geant(int cut=2){
  // f=TFile("./MC7777,tree.root");
  // f->ls();
  // return;
  //TH1F *h=f->Get("hpx");
  // h->Draw();

  TString histF="mcLcp_cut";
  histF+=cut;
  histF+=".hist.root";
  initHisto(histF);

  TString fItem="recoEffStudy/G2032.tree.root";
  TString evePath="/star/data04/sim/balewski/LcpRun2/MCrcf1200/";

  int subSet;
  for(subSet=2032;subSet<=2167;subSet++) {
    fItem=evePath+"G";
    fItem+=subSet;
    fItem+=".tree.root";
    chainG->AddFile(fItem, -1);
    fItem.ReplaceAll("G","R");
    chainR->AddFile(fItem, -1);
  }

  

#if 0
  fItem=evePath+"G2033.tree.root";
  chainG->AddFile(fItem, -1);
  fItem.ReplaceAll("G","R");
  chainR->AddFile(fItem, -1);
#endif

  chainG->ls();
  chainR->ls();

  chainR->SetBranchAddress("nPrim",&nPrimR);  
  chainR->SetBranchAddress("vz",&vzR);  
  chainR->SetBranchAddress("q",&qR);  

  chainG->SetBranchAddress("pt",&ptG);  
  chainR->SetBranchAddress("pt",&ptR);  

  chainG->SetBranchAddress("id",&idG);  
  chainR->SetBranchAddress("id",&idR);  

  chainG->SetBranchAddress("phi",&phiG);  
  chainR->SetBranchAddress("phi",&phiR);  

  chainG->SetBranchAddress("eta",&etaG);  
  chainR->SetBranchAddress("eta",&etaR);  

  int N=chainG->GetEntries();
  printf("N=%d\n",N);
  system("date");
  int k;
  for(k=0;k<N;k++) {
    int ret=-1;
    ret=chainG->GetEntry(k);  
    assert(ret);
    ret=chainR->GetEntry(k);  
    assert(ret);
    assert(idG==idR);
    // if(k>30) break;
    if(nPrimR<=2) continue;
    if(fabs(vzR)>100.) continue;
    if(ptR>5.) continue;
    if(k%1000==0)
     printf("%d  ptG=%f ptR=%f  phi %f=%f  eta %f=%f\n",k,ptG,ptR,phiG,phiR,etaG,etaR);

    //printf("nPrim=%d vz=%f q=%d\n",nPrimR,vzR,qR);

  
    if(cut==1 & ptR>=1) continue;
    else if(cut==2 && (ptR<1. || ptR>=2)) continue; 
    else  if(cut==3 && (ptR<2. || ptR>=3)) continue; 
    else  if(cut==4 && (ptR<3. || ptR>=4)) continue; 

    else  if(cut==11 && (etaR<0. || etaR>0.5)) continue; 
    else  if(cut==12 && (etaR<0.5 || etaR>1.0)) continue; 
    
    else  if(cut==21 &qR!=1) continue; 
    else  if(cut==22 &qR!=-1) continue; 
    
    fillHisto();

 }

  system("date");
 gStyle->SetPalette(1,0);

  gStyle->SetOptStat(1111111);
  c=new TCanvas();
  c->Divide(2,2);

  int i;
  for(i=1;i<4;i++){
    c->cd(i+1);
    hr[i]->Draw();
    //h2[i]->Draw("colz");  gPad->SetLogz();
  }
  
  fd->Write();
  fd->ls();
}

//====================================
//====================================
initHisto(TString fname){
  fd=new TFile(fname,"recreate");
  assert(fd->IsOpen());
  int npt=20;
  float ptMax=5; 
  h1[0]=new TH1F("GvR","type of reco event; 0=noG*noR  1=G*noR 2=noG*R 3=G*R",5,-0.5,4.5);

  hr[1]=new TH1F("PhiR","Reco phi/deg",100,0,400);
  hr[2]=new TH1F("EtaR","Reco eta",100,-2,2);
  hr[3]=new TH1F("PtR","Reco pT/(GeV/c)",100,0,ptMax);

  h1[1]=new TH1F("dPhi","diff phi ; G-R phi/deg",100,-200,200);
  h1[2]=new TH1F("dEta","diff eta ; G-R eta",100,-2,2);
  h1[3]=new TH1F("dPt","diff pt ; G-R  pT/(GeV/c)",100,-2,2);

  h2[0]=0;
 
  h2[1]=new TH2F("dPhiPT","diff phi; G-R diff phi/deg; G pT/GeV/c",50,-200,200,npt,0,ptMax);
  h2[2]=new TH2F("dEtaPT","diff eta; G-R diff eta; pTG/GeV/c",50,-2,2,npt,0,ptMax);
  h2[3]=new TH2F("dPtPT","diff pT;  G-R diff pT/GeV/c; pTG/GeV/c",50,-1,1,npt,0,ptMax);
}

//====================================
//====================================
fillHisto(){
  int mType=0;
  if(ptG>0.3) mType+=1;
  if(ptR>0.3) mType+=2;
  h1[0]->Fill(mType);

  if(mType!=3) continue;



  //.... only both Lcp exist
  float phi1=phiG/3.1416*180;
  if(phi1<0) phi1+=360;
  float phi2=phiR/3.1416*180;
  if(phi2<0) phi2+=360;
  float dPhi=phi1-phi2;
  if(dPhi>180) dPhi-=360;
  if(dPhi<-180) dPhi+=360;

  hr[1]->Fill(phi1);
  hr[2]->Fill(etaR);
  hr[3]->Fill(ptR);
  
  h1[1]->Fill(dPhi);
  h1[2]->Fill(etaG-etaR);
  h1[3]->Fill(ptG-ptR);
  
  h2[1]->Fill(dPhi,ptG);
  h2[2]->Fill(etaG-etaR,ptG);
  h2[3]->Fill(ptG-ptR,ptG);
  

}
