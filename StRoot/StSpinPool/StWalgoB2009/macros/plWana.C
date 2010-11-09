TCanvas *can=0;
const float PI=2*acos(0);
TString spinPre='A';
/* to fix/change colors of lines embedded in histos do:
root [5] TLine* ln = (TLine*)muWET->GetListOfFunctions()->At(0)
root [6] ln->SetLineColor(kRed)
root [7] muWET->Draw()         
*/

//=================================================
plWana(  int page=13,int pl=0, char *core0="R10096140", char *iPath="", char *oPath=""){ //1=gif, 2=ps, 3=both
  //iPath="./";
  //iPath="/star/data05/scratch/stevens4/wAnalysis";
  //iPath="/star/data05/scratch/balewski/2009-Wana-SL09g-march15-PRL/data/";
  iPath="/star/data05/scratch/balewski/2009-Wana-SL10j-aW/data/";
  //core0="R10090018";
  //core0="run9setABCD";
  core0="sum";
  //core0="mcSetD1_ppWprod";
  //core0="mcSetD2_ppQCD10_inf_filter_tot";
  // core0="mcSetD1_ppZprod";

  if(page==0) {
    doAll();
    return;
  }
  if(page==-1) {
    doAllMC();
    return;
  }
   
/*
cat mcSetD1*W*ps | ps2pdf - ~/WWW/tmp/all-W.pdf
*/


  char *nameA[]={"muStatEve","muStatTrk","muStatBtow"}; //pg 1
  char *nameB[]={"muVRf","muZv","muNV","mubX48"};//pg 2
  char *nameC[]={"mubX7","mubX7v"};//pg 3
  char *nameD[]={"muDsm1","muDsm2","muDsm3","muDsm4"};//pg 4
  char *nameE[]={"muTrNfit","muTrFitFrac","muTrch2","muTrRxyIn","muTrRxyOut","muTrch2b"};//pg 5
  char *nameF[]={"muTr2D1","muTrPt1","muTrPt1N","muTrPt1Pr","muTrPt1NPr"};//pg 6
  char *nameG[]={"muBmaxAdc","muBtotAdc","muBclAdcPt","muBclET"};//pg 7
  char *nameH[]={"muBclET24","muBclE242D","muBclET24R"};//pg 8
  
  char *nameJ[]={"muBdist1","muBdist2","muBdist3","muBdist4"};//pg 9
  char *nameK[]={"muBjetETR","muTjetBjet2D","muTBjetET"};//pg 10
  char *nameL[]={"muBjetET","muBclEjetE2D","muTjetET"};//pg 11
  char *nameM[]={"muTwayET","muBwayET","muBclETPt","muEwayET"};//pg 12
  
  char *nameW[]={"muTotwayET2D","musPtBalance_clust","muWET","muW2D1"};//pg 13
  char *namePB[]={"musPtBalance_clust", "musPtBalance_awayTot","muWeta"};// pg 14 -Pt-Balance plots
  char *nameB1[]={"muSEadc1","muSPadc1"}; // pg 15 BSMD spectra
  char *nameN[]={"muTrdEdX","muWdedx"}; //pg 16
  char *nameO[]={"muWglDca","muWglDcaSP","muWglDcaSN"}; // pg 17
  
  char *nameP[]={"muETlive0","muETlive1", "muETlive2","muETlive3","muWcar1","muWcar2","muWcar3"}; // pg 18
  
  char *nameQ[]={"pubJoe1","pubJoe2","pubJoe3","pubJoe4","pubJoe5","pubJoe6","pubJoe7","pubJoe8"};// pg 19

  char *nameR1[]={"pubStatEve","pubCrR","pubWET","pubchEtaCP","pubchEtaCN"};// pg 20
  char *nameR2[]={"pubchRecPNg","pubchRecPNp"};// pg 21
  char *nameR3[]={"pubchWETPg"  ,"pubchWETPp","pubchCFP0" ,"pubchWETNp","pubchWETNg" ,"pubchCFN0"};// pg 22

  char *nameS1[]={"spinStatEve","spins4mon","spinbX48","spinbX7","spinbX48c","spinbX7c"};// pg 23
  char *nameS5[]={"spinET_P","spinET_N","spinQpT","spinQpT2"};// pg 24
  char *nameS2[]={"spinY0","spinY1","spinY2_P","spinY2_N"};// pg 25
  char *nameS3[]={,"spinY3_P","spinY3_N","spinY4_P","spinY4_N"};// pg 26
  char *nameS4[]={"spinY5_P","spinY5_N","spinLepEta"};// pg 27

  //use  Page 30 -42 TPC sectors per cut, 2 pages per cut

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
  if(page==1){ 
   fd->ls(); 
   h0=(TH1*)fd->Get("muStatEve"); assert(h0);
   printf("%s: ",h0->GetName());
   for(int k=1;k<=10;k++) printf("%.0f, ",h0->GetBinContent(k));
   printf("\n");
 }
  if(page>=23 && page<=25 && fd->Get("AspinStatEve")==0) return; // skip spin plots if maker was not used

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
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
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
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
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
      hA[i]=(TH1F*)fd->Get(nameX[i]);  assert(hA[i]);
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
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
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
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
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
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
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

 case 7:{    sprintf(padTit,"BTOW response, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(1000110);
    char **nameX=nameG;
    for(int i=0;i<4;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==2 ) h->Draw("colz");
      if(i==2) h->SetMaximum(0.6* h->GetMaximum());
    }
    c->GetPad(1)->SetLogy();    
    c->GetPad(2)->SetLogy();    
    c->GetPad(3)->SetLogz();    
    c->GetPad(4)->SetLogy();    
 } break;//--------------------------------------

 case 8:{    sprintf(padTit,"2x2 / 4x4 cluster isolation cut, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameH;
    for(int i=0;i<3;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
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
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
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
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
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
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
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
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
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
    can=new TCanvas("aa","aa",900,800);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameW;
    for(int i=0;i<4;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      if(i==3) { // draw on previous
	float sum=h->GetEntries();
	sprintf(txt,"%.0f eve >thres",sum);
	tx=new TText(30,55,txt); tx->Draw();
      }

      c->cd(i+1); h->Draw();
      if(i<3)	h->SetAxisRange(0,60);   
      if(i!=2 ) h->Draw("colz");
      if(i==0 )	h->SetAxisRange(0,60,"y");
      if(i==1 )	{h->SetAxisRange(0,60,"x");h->SetAxisRange(-40,60,"y");}
      if(i==2 ) {
	h->Draw("eh");  	h->SetAxisRange(0,60);
	//	if(strstr("run9",core0)>=0) h->SetMaximum(80);
      }
    }
   
 } break;//--------------------------------------
   
 case 14: {   sprintf(padTit,"pT-Balance plots (out of order)  %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=namePB;
    for(int i=0;i<3;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); 
      if(i<2) h->Draw("colz");
      else  h->Draw();
      if(i==2) h->Rebin(4);
    }
   
 } break;//--------------------------------------


 case 15:{    sprintf(padTit,"BSMD raw spectra, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(1,2);gStyle->SetOptStat(10);
    char **nameX=nameB1;
    for(int i=0;i<2;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
    }
   c->GetPad(1)->SetLogy();       
   c->GetPad(2)->SetLogy();       
   
 } break;//--------------------------------------

case 16:{    sprintf(padTit,"TPC dEdx for all & W tracks, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,1);gStyle->SetOptStat(10);
    char **nameX=nameN;
    for(int i=0;i<2;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw("colz");
    }
    c->GetPad(1)->SetLogz();   
 } break;//--------------------------------------

case 17:{    sprintf(padTit,"TPC global DCA to Vertex for W tracks, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameO;
    for(int i=0;i<3;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
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
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
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

case 19:{    sprintf(padTit,"Background study for Joe, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(3,3);gStyle->SetOptStat(10);
    char **nameX=nameQ;
    for(int i=0;i<8;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1);  h->Draw();
      h->SetFillColor(30+i*5);
      if(i==7) h->Draw("colz");
    }
    c->GetPad(1)->SetLogy();
 } break;//--------------------------------------


 case 20:{    sprintf(padTit,"pub-maker misc, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameR1;
    for(int i=0;i<3;i++) {
        printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      if(i==4) {
	h->Draw("same e");
	break;
      }
      c->cd(i+1); h->Draw();
      if(i==1)  h->Draw("colz");
      if(i==0) h->Draw("h text");
    }
    c->GetPad(2)->SetLogz();          
    c->GetPad(1)->SetLogy();
 } break;//--------------------------------------

 
 case 21:{    sprintf(padTit,"charge separation, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,1);gStyle->SetOptStat(10);
    char **nameX=nameR2;
    for(int i=0;i<2;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1);  h->Draw("colz");
    }
 } break;//--------------------------------------

 
 case 22:{    sprintf(padTit,"charge separation, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(3,2);gStyle->SetOptStat(10);
    char **nameX=nameR3;
    for(int i=0;i<6;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1);  h->Draw();
    }
 } break;//--------------------------------------



 case 23:{    sprintf(padTit,"bXing & spin QA, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,spinPre+padTit,page);
    c->Divide(2,3);gStyle->SetOptStat(1000010);
    char **nameX=nameS1;
    for(int i=0;i<6;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(spinPre+nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==1) { h->Draw("colz");}
    }
    c->GetPad(1)->SetLogy();
    
 } break;//--------------------------------------


 case 24:{    sprintf(padTit,"Final Ws for spin analysis, %s",core0);
   can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,spinPre+padTit,page);
   c->Divide(2,2);gStyle->SetOptStat(10);
   char **nameX=nameS5;
   for(int i=0;i<4;i++) {
     printf("->%s<\n",nameX[i]);
     h=(TH1*)fd->Get(spinPre+nameX[i]);  assert(h);
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
   

 
 case 25:
 case 26:
   {    sprintf(padTit,"spin sorting: lumi & Ws, %s",core0);
     char **nameX=nameS2;
     if(page==26) { nameX=nameS3;sprintf(padTit,"spin sorting: QCD background, %s",core0);}
     can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,spinPre+padTit,page);
     c->Divide(2,2);gStyle->SetOptStat(1000010);

    for(int i=0;i<4;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(spinPre+nameX[i]);  assert(h);
      c->cd(i+1); h->Draw("h  text");
    }
 } break;//--------------------------------------
 
  case 27:{    sprintf(padTit,"charge & ET vs. spin state, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,spinPre+padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameS4;
    for(int i=0;i<3;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(spinPre+nameX[i]);  assert(h);
      c->cd(i+1);  h->Draw("colz");
      if(i==2) h->Draw();
    }
 } break;//--------------------------------------



 case 30: // TPC stats 
 case 31: 
 case 32: // TPC nFitPts 
 case 33: 
 case 34: // TPC nFit/nPossibe
 case 35: 
 case 36: // TPC 1st hit R
 case 37: 
 case 38: // TPC last hit R
 case 39: 
 case 40: // TPC dE/dX
 case 41: 
   {    
   int iew=(page-30)%2; // East-West
   int iCut=(page-30)/2;
   int sec1=1, sec2=12; if(iew) sec1=13, sec2=24;
   char *titA[]={"stats","nFitPoints", " nFit/nPossible","1st hit Rxy","last hit Rxy","dE/dX"};
   char *titB[]={"Stat","TrNfit","TrFitFrac","TrRxyIn","TrRxyOut","TrdEdX"};
   sprintf(padTit,"TPC %s   sectors[%d,%d], %s",titA[iCut],sec1,sec2,core0);
   can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
   c->Divide(4,3);gStyle->SetOptStat(10);
   for(int i=0;i<12;i++) {
     char name[100];
     sprintf(name,"sec%d_%s",i+sec1,titB[iCut]);
     printf("->%s<\n",name);
      h=(TH1*)fd->Get(name);  assert(h);
      c->cd(i+1);
      if(iCut==5) 
	h->Draw("colz");
      else
	h->Draw();
      int col=30+i+sec1;
      h->SetFillColor(col);

      
   }
 } break;//--------------------------------------

 case 42:{    sprintf(padTit,"TPC accepted tracks, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,1);gStyle->SetOptStat(10);
    h2=muTr2D1; // cumulative
    c->cd(1); h2->Draw("colz");
    c->cd(2);
  
    for(int i=0;i<24;i++) {
      char name[100];
      sprintf(name,"sec%d_Tr2D1",i+1);
      printf("->%s<\n",name);
      h=(TH1*)fd->Get(name);  assert(h); h->SetLineColor(30+i);
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
  printf("phi/deg=%.1f, x=%.1f\n",phi,x);
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
void doAll(){
  for(int i=1;i<=27;i++)  { 
    plWana(i,2);
  }
  // TPC by sector:
  for(int i=30;i<=42;i++)  plWana(i,2);

}

//============================
void doAllMC(){
 for(int i=1;i<=23;i++){
   if(i==2) continue;
   if(i==3) continue;
   if(i==4) continue;
   plWana(i,2);
 }
  // TPC by sector:
 for(int i=30;i<=42;i++)  plWana(i,2);
}


// $Log: plWana.C,v $
// Revision 1.17  2010/11/09 23:00:56  balewski
// added chi2/dof for East & West TPC separately
//
// Revision 1.16  2010/03/22 16:11:44  balewski
// better computation of AL(QCD)
//
// Revision 1.15  2010/03/20 18:38:43  balewski
// *** empty log message ***
//
// Revision 1.14  2010/03/18 16:52:19  balewski
// corrected sPtBalance for no-endcap
//
// Revision 1.13  2010/03/15 17:05:51  balewski
// cleanup, used for W AL sort March 15, 2010
//
// Revision 1.12  2010/03/14 22:50:34  balewski
// *** empty log message ***
//
// Revision 1.11  2010/02/04 03:48:25  balewski
// add ET for lumi monitor
//
// Revision 1.10  2010/01/30 02:02:56  balewski
// manore tunes
//
// Revision 1.9  2010/01/28 20:10:08  balewski
// added eta dependent spin sorting
//
// Revision 1.8  2010/01/27 22:12:26  balewski
// spin code matched to x-section code
//
// Revision 1.7  2010/01/26 19:28:07  balewski
// added pt-balanc eplots
//
// Revision 1.6  2010/01/21 00:15:30  balewski
// added sector & run  dependent TPC cuts on Rin, Rout
//
// Revision 1.4  2010/01/10 03:01:39  balewski
// cleanup & nicer histos
//
// Revision 1.3  2009/12/30 18:37:08  balewski
// code tagged in the form close to that used for the Fall 2009 DNP preliminary Jacobian peak
//
// Revision 1.2  2009/12/08 16:53:01  balewski
// *** empty log message ***
//
// Revision 1.1  2009/11/23 23:00:20  balewski
// code moved spin-pool
//
