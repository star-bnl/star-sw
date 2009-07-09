/*
cat outPS/*ps | ps2pdf - all.pdf
cp all.pdf ~/WWW/tmp/xP.pdf
*/

TCanvas *can=0;

void doAll(){
  for(int i=10;i<=19;i++)  plPPV3D(i,2); 
  plPPV3D(1,2); 
}

//=================================================
void plPPVtrack4beamLine(  int page=1,int pl=0, char *core0="st_W_10097106_raw_5180001", char *path="./"){ //pl=1=gif, 2=ps, 3=both

  gStyle->SetOptFit(1);
  TString fullInpName=path;  
  fullInpName+=core0; fullInpName+=".ppv.hist";  
  fullInpName+=".root";  
  fd=new TFile(fullInpName);
  if(! fd->IsOpen()) {
    printf("EROR: input histo file not found, quit\n",fullInpName.Data());
    return;
  } else {
    printf("Opened: %s\n",fullInpName.Data());
  }
 if(page==1){ 
   fd->ls();
   printf("v3D_myStat: ");
   for(int k=1;k<10;k++) printf("%.0f, ",v3D_myStat->GetBinContent(k));
   printf("\n");
 }
 gStyle->SetPalette(1,0);
 gStyle->SetOptStat(0);
 char padTit[1000];
 sprintf(padTit,"%s",core0);
 //sprintf(padTit,"%s","run 10079030");

 char *nameA[]={"inPt","inSigY","myStat","inSigZ","inTr","eve0YX"};

 switch (page) {

 case 1:{   
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(3,2);gStyle->SetOptStat(1001111);

    char **nameX=nameA;
    for(int i=0;i<6;i++) {
      char txt[100];

      sprintf(txt,"v3D_%s",nameX[i]);
      printf("->%s<\n",txt);
      h=(TH1*)fd->Get(txt);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==1) h->SetAxisRange(0,10.);
      if(i==2) h->SetMinimum(0.7);
    }
    c->GetPad(1)->SetLogy();
   

    break;
 }//--------------------------------------
 case 2:{   
    can=new TCanvas("aa","aa",900,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(3,2);gStyle->SetOptStat(1001111);
    for(int i=0;i<6;i++) {
      char txt[100];
      sprintf(txt,"v3D_chi%d",i+1);
      printf("->%s<\n",txt);
      h=(TH1*)fd->Get(txt);  assert(h);
      c->cd(i+1); h->Draw();
      if(i>0) {
	h->Draw("colz");
      }
    }
    break;
 }//--------------------------------------

 case 10:
 case 11:
 case 12:
 case 13:
 case 14:
 case 15:
 case 16:
 case 17:
 case 18:
 case 19:
   {   
    can=new TCanvas("aa","aa",500,750);    TPad *c=makeTitle(can,padTit,page);
    int nP=3;
    c->Divide(2,nP);gStyle->SetOptStat(0);
    int eve0=(page-10)*nP;
    eve0+=2;
    for(int i=0;i<nP;i++) {
      char txt[100];
      sprintf(txt,"v3D_eve%dYX",eve0+i);
      printf("->%s<\n",txt);
      h=(TH1*)fd->Get(txt);  assert(h);
      c->cd(2*i+1); h->Draw();
      sprintf(txt,"v3D_eve%dYZ",eve0+i);
      printf("->%s<\n",txt);
      h=(TH1*)fd->Get(txt);  assert(h);
      c->cd(2*i+2); h->Draw();
    }
    break;
 }//--------------------------------------



 default:
     printf("page=%d NOT defined\n",page);

 }

 char text[100];
 sprintf(text,"outPS/%s_page%03d",core0,page);
 TString tit=text;
 can->SetTitle(tit);
 can->SetName(tit);
 //  c->Clear();
 
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
