TCanvas *can=0;

//=================================================
plWana(  int page=4,int pl=0, char *core0="R10096140", char *iPath="", char *oPath=""){ //1=gif, 2=ps, 3=both
  //  iPath="/star/u/stevens4/wAnalysis/";
  //iPath="/star/data05/scratch/stevens4/wAnalysis";
  //iPath="/star/data05/scratch/balewski/2009-Wana-out6/data/";
  //core0="R10097000";
  //core0="run9setABCD";
  core0="run9setP1234";
  //core0="run9setC";
  //core0="mcSetD1_ppWprod";
  //core0="mcSetD2_ppQCD10_inf_filter";
  //core0="mcSetD1_ppZprod";
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


cat R10097000*ps | ps2pdf - all.pdf
cp all.pdf ~/WWW/tmp/all-F10505x.pdf

cat run9setABCD*ps | ps2pdf - all.pdf
cp all.pdf ~/WWW/tmp/all-run9.pdf

*/


  char *nameA[]={"muStatEve","muStatTrk","muStatBtow"}; //pg 1
  char *nameB[]={"muVRf","muZv","muNV","mubX48"};//pg 2
  char *nameC[]={"mubX7","mubX7v"};//pg 3
  char *nameD[]={"muDsm1","muDsm2","muDsm3","muDsm4"};//pg 4
  char *nameE[]={"muTrNfit","muTrFitFrac","muTrRxyIn","muTrRxyOut"};//pg 5
  char *nameF[]={"muTr2D1","muTrPt1","muTrPt1N","muTrPt1Pr","muTrPt1NPr"};//pg 6
  char *nameG[]={"muBmaxAdc","muBtotAdc","muBclAdcPt","muBclET"};//pg 7
  char *nameH[]={"muBclET24","muBclE242D","muBclET24R"};//pg 8
  
  char *nameJ[]={"muBdist1","muBdist2","muBdist3","muBdist4"};//pg 9
  char *nameK[]={"muBjetETR","muTjetBjet2D","muTBjetET"};//pg 10
  char *nameL[]={"muBjetET","muBclEjetE2D","muTjetET"};//pg 11
  char *nameM[]={"muTwayET","muBwayET","muBclETPt","muEwayET"};//pg 12
  
  char *nameW[]={"muTotwayET2D","muAwayTotEt","muWET","muW2D1"};//pg 13
  // pg 14 -free
  char *nameB1[]={"muSEadc1","muSPadc1"}; // pg 15 BSMD spectra
  char *nameN[]={"muTrdEdX","muWdedx"}; //pg 16
  char *nameO[]={"muWglDca","muWglDcaSP","muWglDcaSN"}; // pg 17
  
  char *nameP[]={"muETlive0","muETlive1", "muETlive2","muETlive3","muWcar1","muWcar2","muWcar3"}; // pg 18
  
  char *nameQ[]={"pubJoe1","pubJoe2","pubJoe3","pubJoe4","pubJoe5","pubJoe6","pubJoe7","pubJoe8"};// pg 19

  char *nameR1[]={"pubStatEve","pubCrR","pubWET","pubchEtaCP","pubchEtaCN"};// pg 20
  char *nameR2[]={"pubchRecPNg","pubchRecPNp"};// pg 21
  char *nameR3[]={"pubchWETPg"  ,"pubchWETPp","pubchCFP0" ,"pubchWETNp","pubchWETNg" ,"pubchCFN0"};// pg 22
  char *nameS1[]={"spinStatEve","spins4mon","spinbX48","spinbX7","spinbX48c","spinbX7c"};// pg 23
  char *nameS2[]={"spinY0","spinY1","spinY2_P","spinY2_N","spinY3_P","spinY2_N"};// pg 24
  char *nameS3[]={"spinY4_P","spinY4_N"};// pg 25



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
      char txt[100];
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
      char txt[100];
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
      char txt[100];
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
      char txt[100];
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
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameE;
    for(int i=0;i<4;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
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
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      if(i==0) { cL->cd();  
	h->Draw("colz");
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
      char txt[100];
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
      char txt[100];
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
      char txt[100];
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
      char txt[100];
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
      char txt[100];
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
      char txt[100];
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
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameW;
    for(int i=0;i<4;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      if(i==3) {
	float sum=h->GetEntries();
	sprintf(txt,"%.0f eve >thres",sum);
	tx=new TText(30,62,txt); tx->Draw();
      }

      c->cd(i+1); h->Draw();
      if(i<3)	h->SetAxisRange(0,60);   
      if(i==0 || i==3 ) h->Draw("colz");
      if(i==0 )	h->SetAxisRange(0,60,"y");
      if(i==2 ) {
	h->Draw("eh");  	h->SetAxisRange(0,60);
	//	if(strstr("run9",core0)>=0) h->SetMaximum(80);
      }
    }
   
 } break;//--------------------------------------

 case 14:{   //free 

 } break;//--------------------------------------

 case 15:{    sprintf(padTit,"BSMD raw spectra, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(1,2);gStyle->SetOptStat(10);
    char **nameX=nameB1;
    for(int i=0;i<2;i++) {
      char txt[100];
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
      char txt[100];
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
      char txt[100];
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
      char txt[100];
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
      char txt[100];
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
      char txt[100];
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
      char txt[100];
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
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1);  h->Draw();
    }
 } break;//--------------------------------------



 case 23:{    sprintf(padTit,"bXing /spin QA, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,3);gStyle->SetOptStat(1000010);
    char **nameX=nameS1;
    for(int i=0;i<6;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==1) { h->Draw("colz");}
    }
    c->GetPad(1)->SetLogy();

 } break;//--------------------------------------
 
 case 24:{    sprintf(padTit,"spin sorting: QCD events, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(1000010);
    char **nameX=nameS2;
    for(int i=0;i<4;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw("h  text");
    }
 } break;//--------------------------------------
 
  case 25:{    sprintf(padTit,"charge & ET vs. spin state, %s",core0);
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,1);gStyle->SetOptStat(10);
    char **nameX=nameS3;
    for(int i=0;i<2;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1);  h->Draw("colz");
    }
 } break;//--------------------------------------


 default:
     printf("page=%d NOT defined\n",page);

 }

 char text[100];
 sprintf(text,"%s%s_page%03d",oPath,core0,page);
 TString tit=text;
 can->SetTitle(tit);
 can->SetName(tit);

 
 if(pl&1) can->Print(tit+".gif");
 if(pl&2) can->Print(tit+".ps");
 
}

//------------------------
void splitPadX(float x, TPad **cL, TPad **cR) {
  (*cL) = new TPad("padL", "apdL",0.0,0.,x,0.95);
  (*cL)->Draw();    
  (*cR) = new TPad("padL", "apdL",x+0.005,0.,1.0,0.95);
  (*cR)->Draw();
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
 for(int i=1;i<=25;i++)  {
   if(i==14) continue;
  plWana(i,2);
 }
}

//============================
void doAllMC(){
 for(int i=1;i<=18;i++){
   if(i==2) continue;
   if(i==3) continue;
   if(i==4) continue;
   if(i==14) continue;
   plWana(i,2);
 }
}


// $Log: plWana.C,v $
// Revision 1.1  2009/11/23 23:00:20  balewski
// code moved spin-pool
//
