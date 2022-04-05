// projects TTree in to histos using cuts (if any)
// former rdLcpTree()

proLcpTree( int cut=0, TString run="R3010006", int maxEve=10000,
	    TString wrkDir="/star/data04/sim/balewski/LcpRun2/maxEta1.0/",
	    TString outPath="fixMe/"
	    ){
  printf("#cut %d -->%s \n",cut,outPath.Data(),run.Data());
    
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(1111111);
 
  printf("#run %s\n",run.Data());
  
  
  TChain *chain = new TChain("T-LCP","dummName");
  
  TString item=wrkDir+"/tree/"+run;
  item=item+".tree.root";
  printf("#tree %s\n", item.Data());
  int ret=chain->AddFile(item,-1);
  printf("chain ret %d \n",ret);
  
 
  // ============================  DEFINE  OUTPUT HISTO 
  const float Pi=3.1416; 
  TString outF=wrkDir+outPath+run+".hist.root";

  hFile=new TFile(outF,"RECREATE");
  assert(hFile->IsOpen());

  int i;
  TH1F *h1[8];
  TH2F *h2L[100]; int nh2L=0;

  //...........................  QA histo 
  h1[0]=new TH1F("bx","rates vs. bXing",129,-0.5,128.5);
  h1[1]=new TH1F("pt","Leading Particle pT (GeV/c)",100,0.,10.);
  //h1[1]=new TH1F("pt","Leading Particle pT (GeV/c)",135,0.025,6.775); // Olga's definition
  h1[2]=new TH1F("Zv","Z vertex/cm",100,-250.,250.);
  h1[3]=new TH1F("nPrim","No.of valid prim TPC tracks ",50,-0.5,49.5);
  h1[4]=new TH1F("cosm", "Mike's Cosmics  Rejector return value",5,-0.5,4.5);
  h1[5]=new TH1F("phi","LCP phi/rad",100,-3.15.,3.15);
  h1[6]=new TH1F("eta","LCP Eta",100,2.,2.);
  h1[7]=new TH1F("q","LCP charge",10,-2.,2.);
  float bxF=-0.5, bxL=119.5;
  int nbx=120;
  int nphi=16;
  TH2F *h2= new TH2F("PhiBxAll","LCP Phi/rad vs. bXing[0-119]",nbx,bxF,bxL,nphi,-Pi,Pi);

  /* nomanclautre for LCP cuts:
    pTL,M,H for pt in[0.4,0.7,1.0,2.0]
    pT1,2,3,4,5,6 for pt in[0.4,1,2,3,4,5,6]
    etaBB,Bc,Fc,FF  for eta in [low,-0.5,0.,0.5,high]
    q+,- for charge + or -
  */

  //.................. histo for    PT 1,2,3,4,
  const int mxPt=6;
  TH2F *h2pT[mxPt];
  for(i=0;i<mxPt;i++) {
    char tit1[100], tit2[1000];
    sprintf(tit1,"PhiBxPt%d",i+1);
    sprintf(tit2,"LCP Phi/rad vs. bXing[0-119], cut=Pt%d",i+1);
    h2pT[i]= new TH2F(tit1,tit2,nbx,bxF,bxL,nphi,-Pi,Pi);
    h2L[nh2L++]=h2pT[i];
    //printf("-%s-%s-\n",tit1,tit2);
  }

  //..................   histo for   PT L,M,H
  const int mxRt=3;
  TH2F *h2rT[mxRt];
  char coreRt[mxRt]={'L','M','H'};
  for(i=0;i<mxRt;i++) {
    char tit1[100], tit2[1000];
    sprintf(tit1,"PhiBxPt%c",coreRt[i]);
    sprintf(tit2,"LCP Phi/rad vs. bXing[0-119], cut=Pt%c",coreRt[i]);
    h2rT[i]= new TH2F(tit1,tit2,nbx,bxF,bxL,nphi,-Pi,Pi);
    h2L[nh2L++]=h2rT[i];
  }

  //..................   histo for   Eta selection
  const int mxEta=4;
  TH2F *h2eta[mxEta];
  char *coreEta[mxEta]={"BB","Bc","Fc","FF"};
  for(i=0;i<mxEta;i++) {
    char tit1[100], tit2[1000];
    sprintf(tit1,"PhiBxEta%s",coreEta[i]);
    sprintf(tit2,"LCP Phi/rad vs. bXing[0-119], cut=Eta%s",coreEta[i]);
    h2eta[i]= new TH2F(tit1,tit2,nbx,bxF,bxL,nphi,-Pi,Pi);
    h2L[nh2L++]=h2eta[i];
  }

  //..................  histo for    charge selection
  const int mxq=2;
  TH2F *h2q[mxq];
  char *coreq[mxq]={"neg","pos"};
  for(i=0;i<mxq;i++) {
    char tit1[100], tit2[1000];
    sprintf(tit1,"PhiBxQ%s",coreq[i]);
    sprintf(tit2,"LCP Phi/rad vs. bXing[0-119], cut=Q%s",coreq[i]);
    h2q[i]= new TH2F(tit1,tit2,nbx,bxF,bxL,nphi,-Pi,Pi);
    h2L[nh2L++]=h2q[i];
  }


  //========================== HISTO CREATED

  int N=chain->GetEntries();
  if(maxEve<=0) maxEve=N;

  printf("#sort %d of %d\n",maxEve,N);
  //  hFile->ls();

  // associate varibales with  the tree
  float vz,pt,phi,eta;
  int nPrim,bx,cosm,id,nFit,charge;
  chain->SetBranchAddress("bx120",&bx);  
  chain->SetBranchAddress("vz",&vz);  
  chain->SetBranchAddress("id",&id);  
  chain->SetBranchAddress("pt",&pt);  
  chain->SetBranchAddress("phi",&phi);  
  chain->SetBranchAddress("eta",&eta);  
  chain->SetBranchAddress("q",&charge);  
  chain->SetBranchAddress("nPrim",&nPrim);  
  chain->SetBranchAddress("nFit",&nFit);  
  chain->SetBranchAddress("cosm",&cosm);  

  system("date");
  printf("\n::::::::::::::: L O O P   O V E R   E V E N T S  :::::\n");
  int k;
  int nLcp=0;
  for(k=0;k<N;k++) {
    if(k>=maxEve) break;
    int ret=chain->GetEntry(k);  
    assert(ret);
    if(k%1000==0)printf("%d %d %d %f id=%d nFit=%d\n",k,ret,nPrim  ,vz,id,nFit);
    // new final cuts
    if(nPrim<3 ) continue;
    if( pt>5. ) continue;

    if(cut==1 && (nPrim<5 || nPrim>20) ) continue;
    if(cut==2 && fabs(vz) >50) continue;
    if(cut==3 && (pt<1. || pt>3.) ) continue;
    if(cut==4 && charge<0. ) continue;

    h1[3]->Fill(nPrim);
    h1[0]->Fill(bx);
    h1[2]->Fill(vz);
    h1[4]->Fill(cosm);
    if(nFit<=0) continue; // LCP must exist
    nLcp++;
    h1[1]->Fill(pt);
    h1[5]->Fill(phi);
    h1[6]->Fill(eta);
    h1[7]->Fill(charge);
    h2->Fill(bx,phi);

    // .............. cuts on LCP 
    //... Pt case
    int i=(int)pt;
    if(i>=0 && i<mxPt)  h2pT[i]->Fill(bx,phi); 
    i=-1;
    if(pt<0.7) i=0;
    else if(pt<1.0) i=1;
    else if(pt<3.0) i=2;
    if(i>=0)  h2rT[i]->Fill(bx,phi);
    
    //... Eta case
    if(eta<-0.5) i=0;
    else if(eta<0.) i=1;
    else if(eta<0.5) i=2;
    else i=3;
    h2eta[i]->Fill(bx,phi);
    
    //... charge case
    i=0;
    if(charge>0) i=1;
    h2q[i]->Fill(bx,phi);

  }
  system("date");
  printf("nLcp=%d\n",nLcp);
  
  hFile->Write();// Save all objects in this file
  
  //................... print out 2D-histo
  
  TAxis *X=h2->GetXaxis();
  TAxis *Y=h2->GetYaxis();
  int nx=X->GetNbins();
  int ny=Y->GetNbins();
  printf("#title %s\n", h2->GetTitle());
  printf("#X-axis  %f, %f %d %s \n", X->GetXmin(), X->GetXmax(),nx);
  printf("#Y-axis  %f, %f %d %s \n", Y->GetXmin(), Y->GetXmax(),ny);
  printf("#Integral(valid)= %.1f \n",h2->Integral());

  for(i=0;i<nh2L;i++) {
    TH2F *hx= h2L[i];
    printf("#Integral(%s)= %.1f \n",hx->GetName(),hx->Integral());
  }
  
  c=new TCanvas();
  c->Divide(2,3);
  c->cd(1); h1[1]->Draw(); gPad->SetLogy();
  c->cd(2); h1[5]->Draw();
  c->cd(3); h1[6]->Draw();
  c->cd(4); h2->Draw("colz");
  c->cd(5); h1[2]->Draw();
  c->cd(6); h1[7]->Draw();

  TString outPS=outF.ReplaceAll("hist.root","pro.ps");

  c->Print(outPS);


}
