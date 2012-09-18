TCanvas *can=0;

//=================================================
plZana(  int page=0,int pl=2,  char *core0="run12long", char *iPath="../9.10.12/", char *oPath="./out/z/"){ //1=gif, 2=ps, 3=both
  //core0="jba322";
  if(page==0) {
    doAll(core0,iPath);
    return;
  }
  

  char *nameA[]={"_Z_EventType"}; //pg 1
  char *nameB[]={"_Z_et1iso","_Z_et1val","_Z_et1frac","_Z_et2iso","_Z_et2val","_Z_et2frac"}; //pg 2
  char *nameC[]={"_Z_phi12","_Z_ZmassLike","_Z_chRecPNp","_Z_ZmassUnlike"};
  char *nameD[]={"muEne_Deta","_Z_Ene_Deta"};// pg 4

  char *nameE[]={"_Z_Endcap_EventType","_Z_Y2","_Z_etaZ"}; //pg 5
  char *nameF[]={"_Z_Eet1iso","_Z_Eet1val","_Z_Eet1frac","_Z_Eet2iso","_Z_Eet2val","_Z_Eet2frac"}; //pg 6
  char *nameG[]={"_Z_Ephi12","_Z_ELike_chRecPNp","_Z_E_ZmassLike","_Z_Eeta12","_Z_EUnlike_chRecPNp","_Z_E_ZmassUnlike"} //pg7

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
  if(fd->Get("muStatEve")==0)
    fd->cd("Z");

  if(page==1){ 
    //fd->ls(); 
    h0=(TH1*)gDirectory->Get("_Z_EventType"); assert(h0);
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
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
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
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
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
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==2) {
	h2=(TH2F*) h;
	h2->Rebin2D(2,2); //h2->SetMaximum(3);
	h2->Draw("colz");
	//h2->Draw("box"); h2->SetFillColor(kBlack);
	//h3=(TH2F*)pubchRecPNp;	h3->Rebin2D(2,2);
	//h3->Draw("colz same");
	//h2->Draw("box same");  
      }
      if(i==3)  {
	//h->SetFillColor(kYellow);
	//h->SetMaximum(4);
        h->SetAxisRange(0,130);
	//h->Fit("gaus","","RH",75.,115.);
      }
    }
 
        
 } break;//--------------------------------------

 case 4:{   
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(1,2);gStyle->SetOptStat(1110);
    char **nameX=nameD;
    for(int i=0;i<2;i++) {
      char txt[100];
      if(i==0) continue;
      printf("->%s<\n",nameX[i]);
      h2=(TH2F*)gDirectory->Get(nameX[i]);  assert(h2);
      h2->Rebin2D(2,2);
      c->cd(i+1); h2->Draw("colz");	
    }
 
 } break;//--------------------------------------

 case 5:{   
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(2,2);gStyle->SetOptStat(0);
    char **nameX=nameE;
    for(int i=0;i<3;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  //assert(h);
      if(h==0) continue;
      c->cd(i+1); h->Draw();
      if(i<2) {  h->Draw("h text");
	h->SetMarkerSize(2); // for bigger text
      }
      if(i==2) { h->Rebin(8); h->Draw(); }
      
    }
    c->GetPad(1)->SetLogy();
   
 } break;//--------------------------------------

 case 6:{   
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    c->Divide(3,2);gStyle->SetOptStat(1110);
    char **nameX=nameF;
    for(int i=0;i<6;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
    }
   c->GetPad(2)->SetLogy();
   
 } break;//--------------------------------------
   
 case 7:{  
    can=new TCanvas("aa","aa",800,600);    TPad *c=makeTitle(can,padTit,page);
    //can->SetGrayscale();
    c->Divide(3,2);gStyle->SetOptStat(10);
    char **nameX=nameG;
    for(int i=0;i<6;i++) {
      char txt[100];
      printf("->%s<\n",nameX[i]);
      h=(TH1*)gDirectory->Get(nameX[i]);  assert(h);
      c->cd(i+1); h->Draw();
      if(i==3) h->Draw("colz");
      if(i==1 || i==4) {
	h2=(TH2F*) h;
	h2->Rebin2D(2,2); //h2->SetMaximum(3);
	h2->Draw("colz");
	//h2->Draw("box"); h2->SetFillColor(kBlack);
	//h3=(TH2F*)pubchRecPNp;	h3->Rebin2D(2,2);
	//h3->Draw("colz same");
	//h2->Draw("box same");  
      }
      //if(i==3)  {
	//h->SetFillColor(kYellow);
	//h->SetMaximum(4);
        //h->SetAxisRange(0,130);
	//h->Fit("gaus","","RH",75.,115.);
      //}
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
void doAll(char *core0="", char *iPath=""){
 for(int i=1;i<=7;i++)  {
  plZana(i,2,core0,iPath);
 }
}




// $Log: plZana.C,v $
// Revision 1.6  2012/09/18 15:46:00  balewski
// added etaZ to movie
//
// Revision 1.5  2012/09/14 21:02:31  balewski
// *lumi-maker re-written to accumulate alternative rel lumi monitors,
// * added spin sorting to Zs
//
// Revision 1.4  2012/08/28 14:28:49  stevens4
// updates to movie makers
//
// Revision 1.3  2012/08/07 21:06:56  stevens4
// update to tree analysis to produce independent histos in a TDirectory for each eta-bin
//
// Revision 1.2  2012/06/22 17:36:57  stevens4
// *** empty log message ***
//
// Revision 1.1  2011/02/22 21:38:05  balewski
// First 100 Ws in run 11
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
