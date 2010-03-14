TCanvas *can=0;

//=================================================
plZana(  int page=3,int pl=0, char *core0="R10096140", char *iPath="", char *oPath=""){ //1=gif, 2=ps, 3=both
  iPath="./";
  iPath="/star/data05/scratch/balewski/2009-Wana-SL09g-feb24/data/";
  //iPath="/star/data05/scratch/stevens4/wAnalysisOut/apsXsec/rcfMC/";
  core0="run9setP1234";
  //core0="rcf10014";;
  //core0="mcSetD1_ppWprod";
  //core0="mcSetD2_ppQCD10_inf_filter";
  //core0="mcSetD1_ppZprod";

  if(page==0) {
    doAll();
    return;
  }
  

  char *nameA[]={"_Z_EventType"}; //pg 1
  char *nameB[]={"_Z_et1iso","_Z_et1val","_Z_et1frac","_Z_et2iso","_Z_et2val","_Z_et2frac"}; //pg 2
  char *nameC[]={"_Z_phi12","_Z_ZmassLike","_Z_chRecPNp","_Z_ZmassUnlike"};
  char *nameD[]={"muEne_Deta","_Z_Ene_Deta"};// pg 4

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
   h0=(TH1*)fd->Get("_Z_EventType"); assert(h0);
   printf("%s: ",h0->GetName());
   for(int k=1;k<=14;k++) printf("%.0f, ",h0->GetBinContent(k));
   printf("\n");
 }
 gStyle->SetPalette(1,0);
 gStyle->SetOptStat(0);
 char padTit[1000];
 sprintf(padTit,"%s",core0);

 switch (page) {

 case 1:{   
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(1,1);gStyle->SetOptStat(0);
    char **nameX=nameA;
    for(int i=0;i<1;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==0) h->Draw("h text");
    }
    c->GetPad(1)->SetLogy();
   
 } break;//--------------------------------------

 case 2:{   
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(3,2);gStyle->SetOptStat(1110);
    char **nameX=nameB;
    for(int i=0;i<6;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
    }
   c->GetPad(2)->SetLogy();
   
 } break;//--------------------------------------


 case 3:{  
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    //can->SetGrayscale();
    c->Divide(2,2);gStyle->SetOptStat(10);
    char **nameX=nameC;
    for(int i=0;i<4;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==2) {
	h2=(TH2F*) h;
	h2->Rebin2D(2,2);h2->SetMaximum(3);
	h2->Draw("box"); h2->SetFillColor(kBlack);
	h3=(TH2F*)pubchRecPNp;	h3->Rebin2D(2,2);
	h3->Draw("colz same");
	h2->Draw("box same");  
      }
      if(i==3)  {
	//h->SetFillColor(kYellow);
	h->SetAxisRange(0,130);
	h->Fit("gaus","","RH",75.,115.);
      }
    }
 
        
 } break;//--------------------------------------

 case 4:{   
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(1,2);gStyle->SetOptStat(1110);
    char **nameX=nameD;
    for(int i=0;i<2;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h2=(TH2F*)fd->Get(nameX[i]);  assert(h2);
      h2->Rebin2D(2,2);
      c->cd(i+1); h2->Draw("colz");	
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
 for(int i=1;i<=4;i++)  {
  plZana(i,2);
 }
}




// $Log: plZana.C,v $
// Revision 1.8  2010/03/14 22:50:34  balewski
// *** empty log message ***
//
// Revision 1.7  2010/02/04 03:48:25  balewski
// add ET for lumi monitor
//
// Revision 1.6  2010/01/10 03:01:39  balewski
// cleanup & nicer histos
//
// Revision 1.5  2010/01/06 14:11:17  balewski
// one Z-plot added
//
// Revision 1.4  2010/01/06 05:21:59  balewski
// cleanup
//
// Revision 1.3  2010/01/06 04:22:18  balewski
// added Q/PT plot for Zs, more cleanup
//
// Revision 1.2  2010/01/05 03:23:02  balewski
// change logic for filling btow status tables, added printout to Z-code
//
// Revision 1.1  2010/01/04 05:12:02  balewski
// added 4x4 cut to Z-algo, cleanup
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
