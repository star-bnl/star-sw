TCanvas *can=0;
TString ver='B';

//=================================================
plWjj(  int page=2,int pl=0, char *core0="R10096140", char *iPath="", char *oPath=""){ //1=gif, 2=ps, 3=both
  iPath="./";
  //iPath="/star/data05/scratch/balewski/2009-Wana-SL09g-s1/data/";
  core0="sumRun9_may1";
  //  core0="sumW";
  core0="rcn10017_10_500evts";// Z->any
  //core0="rcm10019_65_1000evts";// QCD ^^^10
  //core0="rcn10020_13_300evts";// QCD ^^^5
  //core0="rcn10017_35_500evts";// W->any

  if(page==0) {
    doAll();
    return;
  }
  

  char *nameA[]={"WjjStatEve"}; //pg 1
  char *nameB[]={"Wjj_J1","Wjj_J2","Wjj_K1","Wjj_K6","Wjj_K5","Wjj_K3"};//pg2
  char *nameC[]={"Wjj_3J","Wjj_K4","Wjj_K2","Wjjphi12"};//pg3
  char *nameD[]={"Wjj_K1","Wjj_P1","Wjj_P2","Wjj_P4","Wjj_P3"};//pg4
  
  
  //C2??


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
   h0=(TH1*)fd->Get(ver+"WjjStatEve"); assert(h0);
   printf("%s: ",h0->GetName());
   for(int k=1;k<=h0->GetXaxis()->GetNbins();k++) printf("%.0f, ",h0->GetBinContent(k));
   printf("\n");
 }
 gStyle->SetPalette(1,0);
 gStyle->SetOptStat(0);
 char padTit[1000];
 sprintf(padTit,"%s %s",ver.Data(),core0);

 switch (page) {

 case 1:{   
    can=new TCanvas("aa","aa",800,400);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(1,1);gStyle->SetOptStat(0);
    char **nameX=nameA;
    for(int i=0;i<1;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(ver+nameX[i]);  assert(h);
      c->cd(i+1); h->Draw(); gPad->SetTopMargin(0.2);
      if(i==0) h->Draw("h text");
    }
    c->GetPad(1)->SetLogy();
   
 } break;//--------------------------------------

 case 2:{   
    can=new TCanvas("aa","aa",900,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(3,2);gStyle->SetOptStat(1110);
    char **nameX=nameB;
    for(int i=0;i<6;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(ver+nameX[i]);  assert(h);
      c->cd(i+1); h->Draw("colz");
      if(i==2) h->Draw();
      if(i==2){  h->SetAxisRange(0,180); h->SetMinimum(0.9);}// h->Fit("gaus","","RH",60,110);}
      if(i>=4) ((TH2F*)h)->Rebin2D();
      c->GetPad(i+1)->SetRightMargin(0.15);
    }

    if(strstr(core0,"sumR")) c->GetPad(3)->SetLogy();
   
 } break;//--------------------------------------


 case 3:{   
    can=new TCanvas("aa","aa",700,500);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(3,2);gStyle->SetOptStat(1110);
    char **nameX=nameC;
    for(int i=0;i<4;i++) {
      printf("->%s<\n",nameX[i]);
      h=(TH1*)fd->Get(ver+nameX[i]);  assert(h);
      c->cd(i+1);
      h->Draw("colz");
      if(i==3) h->Draw();
    }
 } break;//--------------------------------------

   case 4:{   
     can=new TCanvas("aa","aa",700,500);    TPad *c=makeTitle(can,padTit,page);
     c->Divide(3,2);gStyle->SetOptStat(1110);
     char **nameX=nameD;
     for(int i=0;i<5;i++) {
       printf("->%s<\n",nameX[i]);
       h=(TH1*)fd->Get(ver+nameX[i]);  assert(h);
       c->cd(i+1);
       h->Draw();
       if(i==3) h->Draw("colz");
     }
   } break;//--------------------------------------
   



 default:
     printf("page=%d NOT defined\n",page);

 }

 char text[100];
 sprintf(text,"%s%s_page%03d_%s",oPath,core0,page,ver.Data());
 TString tit=text;
 can->SetTitle(tit);
 can->SetName(tit);

 
 if(pl&1) can->Print(tit+".gif");
 if(pl&2) can->Print(tit+".ps");
 
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




// $Log: plWjj.C,v $
// Revision 1.2  2010/06/25 15:42:22  balewski
// *** empty log message ***
//
// Revision 1.1  2010/04/16 01:20:18  balewski
// start
//
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
