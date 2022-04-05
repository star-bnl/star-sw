TCanvas *can=0;
const float PI=2*acos(0);
/* to fix/change colors of lines embedded in histos do:
root [5] TLine* ln = (TLine*)muWET->GetListOfFunctions()->At(0)
root [6] ln->SetLineColor(kRed)
root [7] muWET->Draw()         
*/

// Endcap reco algo

//=================================================
void plEana(  int page=-1,int pl=2, char *core0="sumR12P13ib", char *iPath="/star/u/jlzhang/run12-dev/", char *oPath="/star/u/jlzhang/run12-dev/movies/", int isMC=0, char *etaBin="Eta7"){ //1=gif, 2=ps, 3=both
    
  cout<<iPath<<core0<<endl;

  if(page<=-1) {
    doAll(core0,iPath,isMC,oPath,etaBin);
    return;
  }
   
/*
cat mcSetD1*W*ps | ps2pdf - ~/WWW/tmp/all-W.pdf
*/


  char *nameA[]={"muEStatEve","muEStatTrk"}; //pg 1
  char *nameB[]={"muEVRf","muEZv","muENV","muEbX48"};//pg 2
  char *nameC[]={"muEbX7","muEbX7v"};//pg 3
  char *nameD[]={"muEDsm1","muEDsm2","muEDsm3","muEDsm4"};//pg 4
  char *nameE[]={"muETrNfit","muETrFitFrac","muETrch2","muETrRxyIn","muETrRxyOut","muETrch2b"};//pg 5
  char *nameF[]={"muETr2D1","muETrPt1","muETrPt1N","muETrPt1Pr","muETrPt1NPr"};//pg 6
  char *nameG[]={"muEeXY","muEmaxAdc","muEtotAdc","muEclAdcPt","muEclET"};//pg 7
  char *nameH[]={"muEclET24","muEclE242D","muEclET24R"};//pg 8
  
  char *nameJ[]={"muEdist1","muEdist2","muEdist3","muEdist4"};//pg 9
  char *nameK[]={"muETEMCjetETR","muETjetEMCjet2D","muETEMCjetET"};//pg 10
  char *nameL[]={"muEEMCjetET","muEclEMCjetE2D","muETjetET"};//pg 11
  char *nameM[]={"muETwayET","muEBwayET","muEEclETPt","muEEwayET"};//pg 12
  
  char *nameW[]={"muETotwayET2D","muEsPtBalance_clust","muE_WET","muE_W2D1"};//pg 13
  char *namePB[]={"muEsPtBalance_clust", "muEsPtBalance_awayTot","muE_Weta","muE_WXY"};// pg 14 -Pt-Balance plots
  char *nameN[]={"muETrdEdX","muE_Wdedx"}; //pg 16 -> pg 15 pg21 -> pg16
  char *nameO[]={"muE_WglDcaSP","muE_WglDcaSN"}; // pg 17
  
  char *nameP[]={"muE_ETlive0","muE_ETlive1","muE_ETlive2","muE_ETlive3","muE_Wcar1","muE_Wcar2","muE_Wcar3"}; // pg 18
  
  //add histograms for q/pt plots etc.
  char *nameR2[]={"muEchRecPNg","muEchRecPNp"};// pg 19
  char *nameR3[]={"muEchRecHypCorrPNg" ,"muEchRecHypCorrPNp"};// pg 20

  char *nameSMD[]={"muE_UoffStr","muE_VoffStr"}; // pg21 -> pg 16

  TString spinPre='A';
  char *nameS1[]={"spinEStatEve","spinEs4mon","spinEbX48","spinEbX7","spinEbX48c","spinEbX7c"};// pg 23
  char *nameS5[]={"spinE_ET_P","spinE_ET_N","spinEQpT","spinEQpT2"};// pg 24
  char *nameS6[]={"spinEQpT_hits","spinEQpT_hitF","spinEHitsFit_Frac"};// pg 25
  char *nameS2[]={"spinEY0","spinEY1","spinEY2_P","spinEY2_N"};// pg 26
  char *nameS3[]={,"spinEY3_P","spinEY3_N","spinEY4_P","spinEY4_N"};// pg 27
  char *nameS4[]={"spinEY5_P","spinEY5_N","spinELepEta_P","spinELepEta_N"};// pg 28

  char *nameEsmd1[]={"muEsmdNhit","muEsmdEne","muEsmdRatioUV","muEclustET_esmdNhit","muEclustET_esmdEne","muEclustET_esmdRatio"};//pg 29
  char *nameEsmd2[]={"muEclustET_esmdEneSum7","muEsPtBalance_clustPassSMD","muEsPtBalance_clustFailSMD","muEsPtBalance_esmdRatio_ET25","muEsPtBalance2_clustPassSMD","muEsPtBalance2_clustFailSMD"};//pg 30

  //use  Page 30-42 TPC sectors per cut, 2 pages per cut

  gStyle->SetOptFit(1);
  TString fullInpName=iPath;  fullInpName+=core0;
  fullInpName+=".wana.hist.root";  
  fd=new TFile(fullInpName);
  if(! fd->IsOpen()) {
    printf("EROR: input histo file not found, quit\n",fullInpName.Data());
    return;
  } else {
    printf("Opened: %s\n",fullInpName.Data());
  }

  //switch to TDirectory for eta binning
  if(fd->Get("muEStatEve")==0) {
    cout<<"Switching to etaBin="<<etaBin<<" now have to use gDirectory"<<endl;
    spinPre+=etaBin;
    if(!fd->cd(etaBin)) {
      cout<<"Missing TDirectory of interest, no plots!"<<endl;
      return;
    }
  }

  if(page==1||page==13){ 
   //fd->ls(); 
   h0=(TH1*)gDirectory->Get("muEStatEve"); assert(h0);
   printf("%s: ",h0->GetName());
   for(int k=1;k<=16;k++) printf("%.0f, ",h0->GetBinContent(k));
   printf("\n");
 }
 // if(page>=23 && page<=25 && gDirectory->Get("AspinStatEve")==0) return; // skip spin plots if maker was not used

  //skip tpc plots if using tree reader code
  if( ((page>=2 && page<=6) || (page>=31 && page<=43)) && !fd->cd("tpc")) return;
  fd->cd(); 
  gStyle->SetPalette(1,0);
  gStyle->SetOptStat(0);
  char padTit[1000];
  sprintf(padTit,"%s",core0);

 switch (page) {

 case 1:{   
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(1,2);gStyle->SetOptStat(0);
    char **nameX=nameA;
    for(int i=0;i<2;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==0) h->Draw("h text");
    }
    c->GetPad(1)->SetLogy();
    c->GetPad(2)->SetLogy();
 } break;//--------------------------------------

 case 2:{   
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(1110);
    char **nameX=nameB;
    for(int i=0;i<4;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==1) h->Fit("gaus","","hR",-50,50);
    }
    c->GetPad(1)->SetLogy();
    c->GetPad(3)->SetLogy();
 } break;//--------------------------------------


 case 3:{  // efficiency vs. bXing, only for data 
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(1,2);gStyle->SetOptStat(10);
    char **nameX=nameC;
    TH1F *hA[2];
    c->cd(1);
    for(int i=0;i<2;i++) {
      printf("->%s<\n",nameX[i]);
      hA[i]=(TH1F*)gDirectory->Get(nameX[i]);  assert(hA[i]);
      if(i==0)  hA[i]->Draw();
      else  hA[i]->Draw("same");
      
    }
    // c->GetPad(1)->SetLogy();
    
    hA[0]->SetFillColor(0); 

    TH1 *hEf=(TH1F*) hA[1]->Clone(); 
    hEf->SetTitle("Vertex effi vs. bXing");
    hEf->SetName("muVefbx");
    hEf->SetFillColor(0);     hEf->SetLineColor(kMagenta);
    hEf->Divide(hA[0]);
    c->cd(2);
    hEf->Draw(); hEf->SetMaximum(1.1);
    hEf->Fit("pol0","","Rh",50,100);
    
 } break;//--------------------------------------

 case 4:{   
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(1001111);
    char **nameX=nameD;
    for(int i=0;i<4;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==3) {
	  h->SetFillColor(kBlue);
	  c->cd(3);
	  h->Draw("same");
      }
    }
    c->GetPad(1)->SetLogy();
    c->GetPad(2)->SetLogy();
    c->GetPad(3)->SetLogy();
 } break;//--------------------------------------

 case 5:{    sprintf(padTit,"Track selection cuts, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(3,2);gStyle->SetOptStat(110);
    char **nameX=nameE;
    for(int i=0;i<6;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==5) h->Draw("colz");
    }
    //c->GetPad(1)->SetLogy();
 } break;//--------------------------------------




 case 6:{    sprintf(padTit,"Selected high PT tracks, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c0=makeTitle(can,padTit,page);
    TPad *cL,*cR;   splitPadX(0.4,&cL,&cR);
    cR->cd();
    cR->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameF;
    TH1F *h1,*h2;
    for(int i=0;i<5;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      if(i==0) { cL->cd();  
	h->Draw("colz");
	for( float eta=-0.8; eta<.6; eta+=1.4) // print sectors IDs
	  for(float x=-PI-.1; x<PI; x+=PI/6) {
	    int sec=tpcSec(x, eta);;
	    char txt[100];
	    sprintf(txt,"sec %d",sec);
	    tx=new TText(eta,x,txt); tx->Draw();
	  }
      }
      
      if(i==1 || i==3) {cR->cd(1+i/2); h1=h; h->Draw(); h->SetMinimum(.1);}
      if(i==4) h->SetFillColor(9);
      if(i==2 || i==4) {  h2=h; h->Draw("same");  cR->cd(2+i/2);
	h=(TH1F*) h2->Clone(); h->SetTitle("Ratio Negtive/all");h->Divide(h1); h->Draw();
	h->SetMinimum(0.2);	h->SetMaximum(0.7);
      }
      if(i>0) h->SetAxisRange(0,50);
    }
    cR->GetPad(1)->SetLogy();
    cR->GetPad(2)->SetLogy();
 } break;//--------------------------------------

 case 7:{    sprintf(padTit,"ETOW response, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(3,2);gStyle->SetOptStat(1000110);
    char **nameX=nameG;
    for(int i=0;i<5;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==3 || i==0) h->Draw("colz");
      if(i==3) h->SetMaximum(0.6* h->GetMaximum());
    }
    c->GetPad(2)->SetLogy();    
    c->GetPad(3)->SetLogy();    
    // c->GetPad(4)->SetLogz();    
    c->GetPad(5)->SetLogy();    
 } break;//--------------------------------------

 case 8:{    sprintf(padTit,"2x1 / 4x4 cluster isolation cut, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameH;
    for(int i=0;i<3;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==1  ) h->Draw("colz");
    }
   c->GetPad(1)->SetLogy();       
   c->GetPad(2)->SetLogz();       
 } break;//--------------------------------------

 case 9:{     sprintf(padTit,"3D distance between track & cluster, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameJ;
    for(int i=0;i<4;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); 
      if(i==3) h->Draw();
      else h->Draw("colz");
    }
 } break;//--------------------------------------

 case 10:{    sprintf(padTit,"separted near jet in BOW & TPC, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameL;
    for(int i=0;i<3;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==1  ) h->Draw("colz");
    }
   c->GetPad(1)->SetLogy();       
   c->GetPad(2)->SetLogz();       
   c->GetPad(3)->SetLogy();       
 } break;//--------------------------------------


 case 11:{    sprintf(padTit,"TPC+BTOW near jet ET, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameK;
    for(int i=0;i<3;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==1  ) h->Draw("colz");
    }
 
   c->GetPad(2)->SetLogz();       
   
 } break;//--------------------------------------

 case 12:{    sprintf(padTit,"away ET veto, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameM;
    for(int i=0;i<4;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==2)	h->SetAxisRange(0,60);
      if(i==2  ){
	h->Draw("colz");
      }
    }
   c->GetPad(1)->SetLogy();       
   c->GetPad(2)->SetLogy();       
   c->GetPad(4)->SetLogy();       

 } break;//--------------------------------------


 case 13:{    sprintf(padTit,"best W selection, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameW;
    for(int i=0;i<4;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      if(i==3) { // draw on previous
	float sum=h->GetEntries();
	sprintf(txt,"%.0f eve >thres",sum);
	tx=new TText(30,52,txt); tx->Draw();
      }

      c->cd(i+1); h->Draw();
      if(i<3)	h->SetAxisRange(0,60);   
      if(i!=2 ) h->Draw("colz");
      if(i==0 )	h->SetAxisRange(0,60,"y");
      if(i==1 )	{h->SetAxisRange(0,60,"x");h->SetAxisRange(-40,60,"y");}
      if(i==2 ) {
	h->Draw("eh");  	h->SetAxisRange(0,60);
	h->SetFillColor(8);
	//	if(strstr("run9",core0)>=0) h->SetMaximum(80);
      }
    }
   
 } break;//--------------------------------------
   
 case 14: {   sprintf(padTit,"pT-Balance plots (out of order)  %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=namePB;
    for(int i=0;i<4;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); 
      if(i==2) {h->Rebin(8); h->GetXaxis()->SetRangeUser(0.5,2.0);}
      if(i<2) h->Draw("colz");
      else  h->Draw();
      if(i==3) h->Draw("colz");
    }
   
 } break;//--------------------------------------

case 15:{    sprintf(padTit,"TPC dEdx for all & W tracks, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,1);gStyle->SetOptStat(10);
    char **nameX=nameN;
    for(int i=0;i<2;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw("colz");
    }
    c->GetPad(1)->SetLogz();   
 } break;//--------------------------------------

 case 16:{    sprintf(padTit,"TRack-SMD peak offset , %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(1,2);gStyle->SetOptStat(10);
    char **nameX=nameSMD;
    for(int i=0;i<2;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1);  h->Draw("colz");
      for(int sec=1;sec<=12;sec++) {
	float phiC= (3-sec)*30;
	if(phiC<-179) phiC+=360;
	float phiL= (phiC-15.)/180*3.1416;
	//printf("sec=%d phiC/deg=%.0d  phiL/rad=%.2f\n",sec,phiC, phiL);
	ln=new TLine(phiL,-7,phiL,7); ln->Draw(); ln->SetLineColor(8);
	tx=new TText(phiL+0.05,5,Form("sec %d",sec)); tx->Draw();
      }
    }
 } break;//--------------------------------------

case 17:{    sprintf(padTit,"TPC global DCA to Vertex for W tracks, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameO;
    for(int i=0;i<2;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw("colz");
      h->SetAxisRange(0,60);      h->SetAxisRange(-2.,2.,"y");
    }
 } break;//--------------------------------------



 case 18:{    sprintf(padTit,"electron candidate ET vs. condition, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    gStyle->SetOptStat(10);
    char **nameX=nameP;
    c->cd(); TPad *cL,*cR;   splitPadX(0.5,&cL,&cR);
    cL->cd(); cR->Divide(1,3);
    for(int i=0;i<7;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      if(i==0) { h->Draw(); h->SetAxisRange(0,60);}
      if(i>0 && i<4)   h->Draw("same");
      if(i==1) h->SetFillColor(kBlue);
      if(i==2) h->SetFillColor(8);
      if(i==3) h->SetFillColor(kRed);
      if(i==4) { cR->cd(1); h->Draw(); h->SetAxisRange(0,5); }
      if(i==5) { cR->cd(2); h->Draw();}
      if(i==6) { cR->cd(3); h->Draw(); h->Fit("gaus","","Rh",-100,80);	h->GetXaxis()->SetTitle("Z (cm)");}
      if(i>3) {
	h->GetXaxis()->SetTitleOffset(0.9);
	h->GetXaxis()->SetLabelSize(0.06);  h->GetXaxis()->SetTitleSize(0.05); h->SetMinimum(0.8);}
      
    }
    cL->GetPad(0)->SetLogy();
    cR->GetPad(1)->SetLogy();

 } break;//--------------------------------------

 case 19:{    sprintf(padTit,"charge separation, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,1);gStyle->SetOptStat(110110);
    ln=new TLine(0,0,80,0); ln->SetLineColor(kMagenta);
    char **nameX=nameR2;
    for(int i=0;i<2;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH2*)gDirectory->Get(nameX[i]);  assert(h); h->Rebin2D(3,3);
      c->cd(i+1);  h->Draw("colz");
      h->SetAxisRange(0,70); ln->Draw();
    }
 } break;//--------------------------------------
    
 case 20:{    sprintf(padTit,"Charge Separation Hyperbola Corrected, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,1);gStyle->SetOptStat(110110);
    ln=new TLine(0,0,70,0); ln->SetLineColor(kMagenta);
    char **nameX=nameR3;
    for(int i=0;i<2;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH2*)gDirectory->Get(nameX[i]);  assert(h); h->Rebin2D(3,3);
      c->cd(i+1);  h->Draw("colz");
      h->SetAxisRange(0,70); ln->Draw();
    }
 } break;//--------------------------------------

 case 23:{    sprintf(padTit,"bXing & spin QA, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,spinPre+padTit,page);
    c->Divide(2,3);gStyle->SetOptStat(1000010);
    char **nameX=nameS1;
    for(int i=0;i<6;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(spinPre+nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==1) { h->Draw("colz");}
    }
    c->GetPad(1)->SetLogy();
    
 } break;//--------------------------------------


 case 24:{    sprintf(padTit,"Final Endcap Ws for spin analysis, %s",core0);
   can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,spinPre+padTit,page);
   c->Divide(2,2);gStyle->SetOptStat(10);
   char **nameX=nameS5;
   for(int i=0;i<4;i++) {
     printf("->%s<\n",nameX[i]);
     h=(TH1*)gDirectory->Get(spinPre+nameX[i]);  assert(h);
     c->cd(i+1);  h->Draw();
     if(i==2) { hx=(TH1*) h->Clone(); h->SetFillColor(9); hx->SetFillColor(46);
       hx->SetAxisRange(0,1); hx->Draw("same");
     }
     
     if(i==3) { h->Draw("colz"); ((TH2F*)h)->Rebin2D(1,2);}
     if(i<2) h->Rebin();
     if(i<2||i==3) h->SetAxisRange(0,70);
     gPad->SetGrid(0,0);
   }
 } break;//--------------------------------------
 case 25:{  sprintf(padTit,"Charge separation vs. track quality, %s", core0);
   can=new TCanvas("aa","aa",800,600); TPad *c=makeTitle(can,spinPre+padTit,page);
   c->Divide(2,2);gStyle->SetOptStat(10);
   char **nameX=nameS6;
   for(int i=0;i<3;i++) {
	 printf("->%s<\n",nameX[i]);
     h=(TH1*)gDirectory->Get(spinPre+nameX[i]);  assert(h);  
	 c->cd(i+1);  h->Draw("colz"); ((TH2F*)h)->Rebin2D(2,2);
	 gPad->SetGrid(0,0);
   }
		 }break;//-----------------------------------

 case 26:
 case 27:
   {    sprintf(padTit,"spin sorting: lumi & Ws, %s",core0);
     char **nameX=nameS2;
     if(page==26) { nameX=nameS3;sprintf(padTit,"spin sorting: QCD background, %s",core0);}
     can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,spinPre+padTit,page);
     c->Divide(2,2);gStyle->SetOptStat(1000010);

    for(int i=0;i<4;i++) {
      printf("->%s<\n",nameX[i]);
      cout<<spinPre+nameX[i]<<endl;
      h=(TH1*)gDirectory->Get(spinPre+nameX[i]);  assert(h);
      c->cd(i+1); h->Draw("h  text");
    }
 } break;//--------------------------------------
 
  case 28:{    sprintf(padTit,"charge & ET vs. spin state, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,spinPre+padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameS4;
    for(int i=0;i<4;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(spinPre+nameX[i]);  assert(h);
      c->cd(i+1);  h->Draw("colz");
      if(i>1) h->Draw();
    }
 } break;//--------------------------------------

 case 29:{    sprintf(padTit,"ESMD 1, %s",core0);
     can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
     c->Divide(3,2);gStyle->SetOptStat(10);
     char **nameX=nameEsmd1;
     for(int i=0;i<6;i++) {
       printf("->%s<\n",nameX[i]);
       h=(TH2F*)gDirectory->Get(nameX[i]);  assert(h);
       if(i==1) h->Rebin2D(4,4);
       if(i==3) h->Rebin2D(4,2);
       if(i==4) h->Rebin2D(4,10);
       if(i==5) h->Rebin2D(4,2); 
       if(i>2) h->GetXaxis()->SetRangeUser(0,70);
     
       c->cd(i+1);  h->Draw("colz");
     }
 } break;//--------------------------------------

 case 30:{    sprintf(padTit,"ESMD 2, %s",core0);
     can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
     c->Divide(3,2);gStyle->SetOptStat(10);
     char **nameX=nameEsmd2;
     for(int i=0;i<6;i++) {
       printf("->%s<\n",nameX[i]);
       h=(TH2F*)gDirectory->Get(nameX[i]);  assert(h);
       if(i==0) {
	 h->Rebin2D(4,10);
	 h->GetXaxis()->SetRangeUser(0,70);
       }
       if(i!=0 && i!=3) {
	 h->GetXaxis()->SetRangeUser(0,70);
	 h->GetYaxis()->SetRangeUser(-60,60);
       }
       c->cd(i+1);  h->Draw("colz");
     }
 } break;//--------------------------------------   

 
 case 31: // TPC stats
 case 32: 
 case 33: // TPC nFitPts
 case 34:
 case 35: // TPC nFit/nPossibe
 case 36:
 case 37: // TPC 1st hit R
 case 38:
 case 39: // TPC last hit R
 case 40: 
 case 41: // TPC dE/dX
 case 42:
   {    
   fd->cd("tpc");
   int iew=(page-31)%2; // East-West
   int iCut=(page-31)/2;
   int sec1=1, sec2=12; if(iew) {sec1=13, sec2=24; return;}
   char *titA[]={"stats","nFitPoints", " nFit/nPossible","1st hit Rxy","last hit Rxy","dE/dX"};
   char *titB[]={"Stat","TrNfit","TrFitFrac","TrRxyIn","TrRxyOut","TrdEdX"};
   sprintf(padTit,"TPC %s   sectors[%d,%d], %s",titA[iCut],sec1,sec2,core0);
   can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
   c->Divide(4,3);gStyle->SetOptStat(10);
   for(int i=0;i<12;i++) {
     char name[100];
     sprintf(name,"secEemcTr%d_%s",i+sec1,titB[iCut]);
     printf("->%s<\n",name);
      h=(TH1*)gDirectory->Get(name);  assert(h);
      c->cd(i+1);
      if(iCut==5) 
	h->Draw("colz");
      else
	h->Draw();
      int col=30+i+sec1;
      h->SetFillColor(col);

      
   }
 } break;//--------------------------------------

 case 43:{    sprintf(padTit,"TPC accepted tracks, %s",core0);
    fd->cd();
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,1);gStyle->SetOptStat(10);
    h2=muETr2D1; // cumulative
    c->cd(1); h2->Draw("colz");
    c->cd(2);
  
    fd->cd("tpc");
    for(int i=0;i<24;i++) {
      char name[100];
      sprintf(name,"secEemcTr%d_Tr2D1",i+1);
      printf("->%s<\n",name);
      h=(TH1*)gDirectory->Get(name);  assert(h); h->SetLineColor(30+i);
      if(i==0) h->Draw("box");
      else h->Draw("box same");
    }

 } break;//--------------------------------------

 default:
     printf("page=%d NOT defined\n",page);
     return;
 }

 char text[100];
 sprintf(text,"%s%s_page%03d",oPath,core0,page);
 TString tit=text;
 can->SetTitle(tit);
 can->SetName(tit);

 
 if(pl&1) can->Print(tit+".gif");
 if(pl&2) can->Print(tit+spinPre+".ps");
 
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
int tpcSec(float phiRad, float etaDet){ // finds TPC sector for hit(phi,eta) 
  int sec=0;
  float phi=phiRad/PI*180; // now in degrees
  if (etaDet>0) { // West TPC
    float x=75-phi;
    while(x<0) x+=360;
    sec=1+(int)( x/30.);
  } else {
    float x=phi-105;
    while(x<0) x+=360;
    sec=13+(int)( x/30.);
  }
  // printf("phi/deg=%.1f, x=%.1f\n",phi,x);
  return sec;
}

//------------------------
TPad *makeTitle(TCanvas *c,char *core, int page) {
  c->Range(0,0,1,1);
  TPad *pad0 = new TPad("pad0", "apd0",0.0,0.95,1.,1.);
  pad0->Draw();
  pad0->cd();

  TPaveText *pt = new TPaveText(0,0.,1,1,"br");
  pt->Draw();
  TDatime dt;
  TString txt2=core;
  txt2+=", page=";
  txt2+=page;
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

//============================
void doAll(char *core0="", char *iPath="", int isMC=0, char* oPath="", char* etaBin=""){
  for(int i=1;i<=28;i++)  { 
//    if( i==20) continue; //remove 20 for sign flip!
    if( isMC && i==3) continue;
    if( isMC &&i==4) continue;
    if( isMC && i>=20) continue;

    plEana(i,2,core0,iPath,oPath,isMC,etaBin);
  }

  // ESMD QA
  //plEana(21,2,core0,iPath,oPath,isMC,etaBin);
  for(int i = 29; i<=30; i++) plEana(i,2,core0,iPath,oPath,isMC,etaBin);
  
  // TPC by sector:
  for(int i=31;i<=43;i++)  plEana(i,2,core0,iPath,oPath,isMC,etaBin);

}

//============================
void doAllMC(char *core0="", char *iPath=""){
  for(int i=1;i<=22;i++){ //was 23
    //if(i==2) continue;
    if(i==3) continue;
    if(i==4) continue;
    if(i==15) continue;
    if(i==20 || i==22) continue;
    plEana(i,2,core0,iPath);
  }
  
  // TPC by sector:
  for(int i=31;i<=43;i++)  plEana(i,2,core0,iPath);
}


// $Log: plEana.C,v $
// Revision 1.16  2013/06/14 21:09:09  jlzhang
// add histo Q/pT vs. nHitsFit and Q/pT vs. nHitsPos
//
// Revision 1.15  2012/09/21 21:14:08  balewski
// plane/sectord dependent Z-location for ESMD implemented in matching of TPC track to ESMD shower.
// I'm done
//
// Revision 1.14  2012/09/21 16:59:13  balewski
// added ESMD peak adjustement - partialy finished
//
// Revision 1.13  2012/08/28 14:28:48  stevens4
// updates to movie makers
//
// Revision 1.12  2012/08/07 21:06:56  stevens4
// update to tree analysis to produce independent histos in a TDirectory for each eta-bin
//
// Revision 1.11  2012/07/06 20:45:19  stevens4
// *** empty log message ***
//
// Revision 1.10  2012/07/05 20:13:33  balewski
// *** empty log message ***
//
// Revision 1.9  2012/06/29 19:13:36  stevens4
// Include Jan's edits (previously removed with addition of TPC directory)
//
// Revision 1.8  2012/06/25 20:58:09  stevens4
// add directory for tpc histos
//
// Revision 1.6  2012/06/22 18:23:36  balewski
// *** empty log message ***
//
// Revision 1.5  2012/06/22 18:14:48  balewski
// removed doAllMC
//
// Revision 1.4  2012/06/22 17:36:57  stevens4
// *** empty log message ***
//
// Revision 1.3  2011/02/15 17:34:11  stevens4
// update plotting macros
//
// Revision 1.1  2011/02/10 20:33:34  balewski
// start
//
