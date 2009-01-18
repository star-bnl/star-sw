#include <stdio.h>
#include <assert.h>

#include <TPad.h>
#include <TH2.h>
#include <TF1.h>
#include <TEnv.h>

#include <TPaveStats.h>
#include <TStyle.h> // for gPad
#include <TROOT.h> // for gROOT
#include <TSystem.h>
#include <TMath.h>

const int mxh=64;
static TH1 *hr[mxh];
static TH1 *h[mxh];
static  TList *hCleanUp=new TList;


#include "EEqaPresenter.h"
TStyle *defStyle=0; 
TStyle *ee1Style=0;


//--------------------------------------
//--------------------------------------
void eePlotInit() {
  defStyle=gROOT->GetStyle("Default");
  ee1Style= new TStyle("ee1STyle","ee1Style (general, minimal Stat)");
  ee1Style->SetOptStat(11); 
  ee1Style->SetPalette(1);
  //  ee1Style->cd();
  ee1Style->SetTextSize(0.09);
}

//--------------------------------------
//--------------------------------------
void GetHisto(FileType &fd,char *name, int i) {
  // this is very silly trick to avoid memory leak in the online version

    hr[i]=(TH1 *)fd.Get(name,hr[i]);

    if(hr[i]==0) return;
    h[i]=(TH1*) hr[i]->Clone();
    hCleanUp->Add(h[i]);
}

//--------------------------------------
//--------------------------------------
void 
eePlot(int page, int panel,FileType fd, TPad *cc, const Char_t *eemcTwMaskFilename){
  static int first=1;
  static EemcTwMask *twMask=0;
  if(first) { 
  bool twMaskFound=false;
  twMask =new EemcTwMask;
  eePlotInit(); 
//  const char *maskFile = gEnv->GetValue("OnLine.eemcMask","eemcTwMask.dat");
  twMaskFound=useTwMask(eemcTwMaskFilename, twMask); 

  first=0;
  if(!twMaskFound) { delete twMask; twMask=0;}
  }  
  
  ee1Style->cd(); // use my default style
  hCleanUp->Delete();
  cc->Clear();
  
  if(page==10) {
    switch(panel) {
    case 1: eeJpQa(fd, cc, twMask); break;
    case 2: eeDaqCorr(fd, cc,1); break;
    case 3: eeFreq(fd, cc, twMask); break;
    case 4: eeDaqTwCr(fd, cc, twMask); break;
    case 5: eeDaqTwHot(fd, cc, twMask); break;
    case 6: eeDaqTwHit(fd, cc); break;
    default:  plNone(cc); break;
    } 
  } else if (page==11) {
    switch(panel) {
    case 1: eeDaqCorr(fd, cc,2); break;
    case 2: eeDaqMapmtStat(fd, cc); break;
    case 3: eeDaqMapmtCr(fd, cc,64); break;
    case 4: eeDaqMapmtCr(fd, cc,72); break;
    case 5: eeDaqMapmtCr(fd, cc,80); break;
    case 6: eeDaqMapmtCr(fd, cc,88); break;
    case 7: eeDaqMapmtCr(fd, cc,96); break;
    case 8: eeDaqMapmtCr(fd, cc,104); break;
    case 9:  eeDaqSmdA(fd, cc,"SmdA",'U'); break;
    case 10: eeDaqSmdA(fd, cc,"SmdA",'V'); break;
    case 11: eeDaqSmdA(fd, cc,"HSmd",'U'); break;
    case 12: eeDaqSmdA(fd, cc,"HSmd",'V'); break;
    default:  plNone(cc); break;
      }
  } else if (page==12) {
    switch(panel) {
    case 1: eeTrigHanks(fd, cc); break;  
    case 2: eeTrigDsm0(fd, cc,"HT"); break;
    case 3: eeTrigDsm0(fd, cc,"TP"); break;
    case 4: eeTrigDsm1(fd, cc,"HT"); break;
    case 5: eeTrigDsm1(fd, cc,"TP"); break;
    case 6: eeTrigDsm2HT(fd, cc); break;
    case 7: eeTrigJPsum(fd, cc,"_sum"); break;
    case 8: eeTrigJPfreq(fd, cc); break;
    case 9: eeTrigAdjJPsum(fd, cc,"_sum"); break;
    case 10: eeTrigAdjJPcor(fd, cc,"_cor"); break;
    case 11: eeTrigEtot(fd, cc); break;
    default:  plNone(cc); break;
    }
    
  }
#ifdef IN_PANITKIN 
//Sergey does not like it  gSystem->Sleep(5000);   
#endif
  //  defStyle->cd(); // retun to default style
 printf("JB panel=%d page=%d done\n",panel,page);
 
}

//--------------------------------------
//--------------------------------------
void plNone( TPad *c){
  static TText txt(0.05,.5,"tempo disabled, Jan");
  c->Clear();
  txt.Draw();
}
  
//--------------------------------------
//--------------------------------------
void eeJpQa(FileType fd, TPad *c0, EemcTwMask *m) { // out
  c0->Clear();
  c0->cd(0);

  TPad* c = new TPad("pad2", "apd2",0.0,0.1,1.,1.);  
  c->Draw();  c->cd();
  c->Divide(2,2);
  char *name1[]={"JPpedZoom","JPtotCor","JPtotFreq","JPpedHot"};
  char *name2[]={"JPpedZoom","JPsumTh3","JPtotFreq","xx"}; 
  //printf("m=%p\n",m);
  char **name=name1;
  if(m==0) name=name2; // dirty trick, JB
  int i;
  for(i=0;i<4;i++) {
    // printf("%d %s\n",i,name[i]);
    GetHisto(fd,name[i],i);
    if( h[i]==0) continue;
    c->cd(1+i);
    if(i==0) 
      h[i]->Draw("colz");
    else
      h[i]->Draw("b");
    if(i==0) gPad->SetLogz();
  }
  // extra steps
  if(h[2]) {
    eeJpQaMinMax(h[2]);
  }
  if(h[1]) {
    eeJpQaMinMax(h[1]);
  }
  
  if(m==0) return;
  if(h[3] && h[1]) {// start counting hot towers
    TH1* H4jpHot=h[3];
    H4jpHot->Reset(); // should be here, but online works w/o it 
    
    int cr;
    char tit[100];
    for(cr=1;cr<6;cr++) {
      sprintf(tit,"cr%dHot",cr);
      GetHisto(fd,tit,4+cr);
      TH1F * hx=( TH1F *)h[4+cr];    
      if(hx->Integral()<=50 ) continue;
      if(hx->GetRMS()<=2.)    continue;
      hx->Fit("pol0","0Q");
      TF1 * ff=hx->GetFunction("pol0");
      // assert(ff);
      float yM=ff->GetParameter(0);
      //  printf("fit(%s) yM=%f\n",hx->GetName(),yM);
      
      int nb=hx->GetNbinsX();
      int k;
      for(k=1;k<=nb;k++){
	if(hx->GetBinContent(k)<10*yM) continue;
	if(m->crCh[cr-1][k-1]) continue; // ignore masked channels
	printf(" hot cr=%d ch=%d val=%f\n",cr,k-1,hx->GetBinContent(k));
	H4jpHot->Fill(cr);
      }
    }
  }
  // write text on the left 
  c0->cd(0);
  TPad *c3 = new TPad("pad3", "apd3",0.,0.,1.,.1);
  c3->Draw();
  c3->cd();
  m->txtH->Draw();
  
}

//--------------------------------------
//--------------------------------------
void eeDaqCorr(FileType fd, TPad *c, int es) { // out

  static  TPad *c2 =0;
  static  TPad *c3 =0;

  //  if(c2) delete c2; // to avoid memory leak, breaks in online

  char *nameT[]={"ETowHealth","ETowHeadCorr","ETowOFF","ETowN256","ETowOFFid","ETowGhost","ETowCorrBit"};
  char *nameE[]={"ESmdHealth","ESmdHeadCorr","ESmdOFF","ESmdN256","ESmdOFFid","ESmdGhost","ESmdCorrBit"};

  char **name=nameT;
  float y1=0.3;
  int n1=6;  
  if(es==2) {
    name=nameE;
    y1=0.45;
    n1=4;
  }

  int i;
  c->cd(0);
  c2 = new TPad("pad2", "apd2",0.0,y1+0.01,1.,1.);  
  c2->Draw();  c2->cd();
  c2->Divide(2,n1/2);
  
  for(i=0;i<n1;i++) {
    // printf("%d %s\n",i,name[i]);
    GetHisto(fd,name[i],i);
    if( h[i]==0) continue;
    c2->cd(1+i);
    h[i]->Draw();
    //    gPad->SetLogy(0);
    //  if(h[i]->Integral()>0 ) gPad->SetLogy();
    // if(i<2) gPad->SetLogx();
  }

  if(es==2) {
    c->cd(0);
    float y2=y1*0.7;
    c2 = new TPad("pad2", "apd2",0.0,y2,1.,y1);  
    y1=y2;
    c2->Draw();  c2->cd();
    i=4;
    GetHisto(fd,name[i],i);
    h[i]->Draw(); 
    if(h[i]->Integral()>0 ) gPad->SetLogy();
 
  }


  c2->cd(5);
  gPad->SetGridx();

  c->cd(0);
  c3 = new TPad("pad3", "apd3",0.0,0.,1.,y1);
  c3->Draw();
  c3->cd();
  i=6;
  GetHisto(fd,name[i],i);
  h[i]->Draw(); gPad->SetGridx();

}


//--------------------------------------
//--------------------------------------

void eeDaqTwCr(FileType fd, TPad *c, EemcTwMask *m) { 
  // raw tower crates 1-6
  //  ee2Style->cd(); 

  char tit[100];
  c->Divide(3,2);
  int i;
  for(i=0;i<6;i++) {
    sprintf(tit,"cr%d",i+1);
    GetHisto(fd,tit,i);
    c->cd(i+1);
    gPad->SetLogz(0);
    h[i]->Draw("colz"); 
    h[i]->SetAxisRange(0,500);
    if(h[i]->Integral()>0 )gPad->SetLogz();
    TGraphErrors *gr=  m->crG2+i;
    if(gr->GetN()>0) gr->Draw("P");
  }

}

//--------------------------------------
//--------------------------------------

void eeFreq(FileType fd, TPad *c, EemcTwMask *m) {
  const int nh=4;
  char *name[nh]={"TowHits","Pre1Hits","Pre2Hits","PostHits"};
  int i;
  c->Divide(1,4);
  for(i=0;i<nh;i++) {
    GetHisto(fd,name[i],i);
    //printf("i=%d =%s= p=%p\n",i,name[i],h[i]);
    if(h[i]==0) continue;
    // printf("name=%s=\n",h[i]->GetName());
    c->cd(1+i);
    gPad->SetLogz(0);
    h[i]->Draw("colz");
    gPad->SetGrid();
    if(h[i]->Integral()>0 ) gPad->SetLogz();
    if(i==0 && m->phiG.GetN()>0){ // show hot towers
      m->phiG.Draw("P");
    }
  }
  if(h[0]) addJPphiLimits(h[0]); 
 
}

//--------------------------------------
//--------------------------------------
void eeDaqTwHit(FileType fd, TPad *c) {
  const int nh=4;

  char *name[nh]={"HTow","HPre1","HPre2","HPost"};

  int i;

  c->Divide(2,2);
  
  for(i=0;i<nh;i++) {
    GetHisto(fd,name[i],i);
    c->cd(1+i);
    gPad->SetLogy(0);
    h[i]->Draw();
    if(h[i]->Integral()>0 ) gPad->SetLogy();
  }
}


//--------------------------------------
//--------------------------------------
void eeMany1D(FileType fd, TPad *c, char *core, int nh, int nx, int ny) {
  int linLog=1; 
  char tit[100];
  c->Divide(nx,ny);
  
  int i;

  for(i=0;i<nh;i++) {
    sprintf(tit,"%s%d",core,i+1);
    GetHisto(fd,tit,i);

    c->cd(i+1);
    h[i]->Draw();
    gPad->SetLogy(0);    
    if (h[i]->Integral()>0 && linLog==1) gPad->SetLogy();
  }
}



//--------------------------------------
//--------------------------------------

void eeDaqTwHot(FileType fd, TPad *c, EemcTwMask *m) { 
  const int ncr=6; // raw tower crates 1-6
  int i;
  float ymax=2;
  
  char tit[100];
  c->Divide(1,ncr);
  for(i=0;i<ncr;i++) {
    sprintf(tit,"cr%dHot",i+1);
    GetHisto(fd,tit,i);

    c->cd(i+1);
    gPad->SetLogy(0);
    gPad->SetGridx();
    h[i]->Draw("b");
    
    TH1F * hx=( TH1F *)h[i];    
    if(hx->Integral()<=10 ) continue;
    if(hx->GetRMS()<=2.)    continue;
    if(hx->Integral()>1) gPad->SetLogy();
    hx->Fit("pol0");
    TF1 * ff=hx->GetFunction("pol0");
    float yM=ff->GetParameter(0);
    if(ymax<yM) ymax=yM;

    TGraph *gr=  m->crG+i;
    if(gr->GetN()>0) gr->Draw("P");
 }

  int j;
  for(j=0;j<6;j++) {
    if(h[j]==0) continue;
    h[j]->SetMaximum(ymax*20.);
  }

}

//--------------------------------------
//--------------------------------------

void eeDaqMapmtCr(FileType fd, TPad *c,int cr1) {
  //raw  mapmt crates 84-91 or 92-99 
  int cr;
  char tit[100];
  c->Divide(4,2);
  for(cr=cr1;cr<=cr1+7;cr++) {
    int i=cr-cr1;
    sprintf(tit,"cr%d",cr);
    GetHisto(fd,tit,i);
    c->cd(cr-cr1+1);
    gPad->SetLogz(0);

    h[i]->Draw("colz"); 
    h[i]->SetAxisRange(0,1000);
    if(h[i]->Integral()>0 ) gPad->SetLogz();
  }

}

//--------------------------------------
//--------------------------------------

void eeDaqSmdA(FileType fd, TPad *c, char *core,char uv){
 
  char tit[100];
  if( strstr(core,"SmdA")) 
    c->Divide(2,6);
  else
   c->Divide(3,4);

  int sec;
  int i=0;
  for(sec=1;sec<=12;sec++) {
    sprintf(tit,"%s%d%c",core,sec,uv);
    GetHisto(fd,tit,i);
    // printf("i=%d =%s= p=%p\n",i,tit,h[i]);
    if( h[i]==0) continue;    
    c->cd(i+1);
    h[i]->Draw();
    gPad->SetLogy(0);
    if (h[i]->Integral()>0 ) gPad->SetLogy();
    i++;

  }
}

//--------------------------------------
//--------------------------------------
void eeDaqMapmtStat(FileType fd, TPad *c) {
  static TH2F *h2=0;
  int i=0;
  c->Divide(1,2);
  GetHisto(fd,"MAPMHits",i);
  if(h[i]==0) return;
  h2=(TH2F *) h[i]->Clone();

  c->cd(1);
  h2->Draw("colz");
  gPad->SetGrid();
  h2->SetAxisRange(63,87);
  h2->SetXTitle("Crate ID     12S1=64,  1S1=68,  2S1=72,  3S1=76,  4S1=80,  5S1=84"); 
  gPad->SetLogz(0);    
  if (h2->Integral()>0) gPad->SetLogz();
  

  c->cd(2);
  h2=(TH2F *) h[i];
  h2->Draw("colz");
  gPad->SetGrid();
  h2->SetAxisRange(88,120);
  h2->SetXTitle("Crate ID     6S1=88,  7S1=92,  8S1=96,  9S1=100, 10S1=104,  11S1=108");
  gPad->SetLogz(0);    
  if (h[i]->Integral()>0) gPad->SetLogz();

}



//--------------------------------------
//--------------------------------------
void eeTrigHanks(FileType fd, TPad *c ) {

  char *name[2]={"dsm0inJPall_HT","dsm0inJPall_TP"};
  c->Divide(1,2);
 
  int i;
  for(i=0;i<2;i++) {
    GetHisto(fd,name[i],i);
    // printf("aaa%d %s %p\n",i,name[i],h[i]);
    c->cd(1+i);
    gPad->SetLogz(0);

    h[i]->Draw("colz");
    if( h[i]->Integral()>0 ) gPad->SetLogz();
  }

}


//--------------------------------------
//--------------------------------------
void eeTrigDsm0(FileType fd, TPad *c, char *mode ) {
  char tit[100];
  c->Divide(2,3);
  
  float ymax=0;
  int j;

  for(j=0;j<6;j++) {
    sprintf(tit,"dsm0inJP%d_%s",j+1,mode);
    c->cd(j+1);
    GetHisto(fd,tit,j);
    gPad->SetLogz(0);
    h[j]->Draw("colz");
    if(ymax<h[j]->GetMaximum()) ymax=h[j]->GetMaximum();

    if ( h[j]->Integral()>0 ) gPad->SetLogz();
  }
  
  for(j=0;j<6;j++) {
    if(h[j]==0) continue;
    h[j]->SetMaximum(ymax);
  }
}

//--------------------------------------
//--------------------------------------
void eeTrigDsm1(FileType fd, TPad *c, char *mode ) {
  char tit[100];
  char *core="dsm1HJP";
  int j;
   
  if(mode[0]=='H') 
    c->Divide(2,6);
  else
    c->Divide(4,3);

  float ymax=0;

  for(j=0;j<12;j++) { 

    sprintf(tit,"%s%d_%s",core,j+1,mode);
    GetHisto(fd,tit,j);
    c->cd(j+1);
    h[j]->Draw("colz");
    gPad->SetLogz(0);
    if(ymax<h[j]->GetMaximum()) ymax=h[j]->GetMaximum();

    if ( h[j]->Integral()>0 ) gPad->SetLogz();
    if(mode[0]=='H') gPad->SetGridx();
 }


  for(j=0;j<12;j++) {
    if(h[j]==0) continue;
    h[j]->SetMaximum(ymax);
  }
  
}
//--------------------------------------
//--------------------------------------
void eeTrigDsm2HT(FileType fd, TPad *c ) {

  char *name[3]={"dsm2Half1_HTTP","dsm2Half2_HTTP","dsm3_HTTP"};
  c->Divide(2,2);
 
  int i;
  for(i=0;i<3;i++) {
    GetHisto(fd,name[i],i);
    c->cd(1+i);
    gPad->SetLogz(0);

    h[i]->Draw("colz");
    if( h[i]->Integral()>0 ) gPad->SetLogz();
  }

}
  


//--------------------------------------
//--------------------------------------
void eeTrigJPsum(FileType fd, TPad *c, char *mode ) {

  char tit[100];
  char newtitle[500];
 
  c->Divide(2,3);

  char *core="JP";
  int j;
  for(j=0;j<6;j++) { 
    sprintf(tit,"%s%d%s",core,j+1,mode);
    //printf("%s %d\n",tit,j);
    GetHisto(fd,tit,j); 
    c->cd(j+1);
    int maxbin=h[j]->GetMaximumBin()-1;
    //printf("max bin = %d\n",maxbin);
    const char *title=h[j]->GetTitle();
    sprintf(newtitle,"%s    ped= %d\n",title,maxbin);
    //printf("%s",newtitle);
    h[j]->SetTitle(newtitle);
    gPad->SetLogy(0);
    h[j]->GetXaxis()->SetRange(1,200); //for p-p commnet out and use full range for Au-Au
    h[j]->Draw();
    if(h[j]->Integral()>0 ) gPad->SetLogy();
  }
  
}


//--------------------------------------
//--------------------------------------
void eeTrigJPfreq(FileType fd, TPad *c) {

  char tit[100];
 
  c->Divide(2,3);
  char *core="JPsumTh";
  int j;
  for(j=0;j<4;j++) { 
    sprintf(tit,"%s%d",core,j);
    //printf("%s %d\n",tit,j);
    GetHisto(fd,tit,j); 
    //printf("h=%p\n",h[j]);
    c->cd(j+1);
    h[j]->Draw();
    h[j]->SetMinimum(0.);
  }


  for(j=0;j<2;j++) { 
    sprintf(tit,"dsm2Half%d_Etot",j+1);
    GetHisto(fd,tit,j); 
    c->cd(j+5);
    h[j]->Draw();
  }

#if 0 // out 2006+
  GetHisto(fd,"JPadjTh",5); 
  c->cd(5);
  h[5]->Draw();
#endif
  
}

//--------------------------------------
//--------------------------------------
void  eeTrigAdjJPsum(FileType fd, TPad *c, char *mode ) {

  char tit[100];
 
  c->Divide(2,3);

  char *core="JP";
  int j;
  for(j=0;j<6;j++) { 
    sprintf(tit,"%s%d%d%s",core,j+1,((j+1)%6)+1,mode);
    //printf("%s %d\n",tit,j);
    GetHisto(fd,tit,j); 
    c->cd(j+1);
    gPad->SetLogy(0);
    // h[j]->GetXaxis()->SetRange(19,69); //for p-p commnet out and use full range for Au-Au
    h[j]->Draw();
    if(h[j]->Integral()>0 ) gPad->SetLogy();
  }
  
}


//--------------------------------------
//--------------------------------------
void eeTrigEtot(FileType fd, TPad *c ) {

  char *nameA[3]={"dsm2E_etot","dsm2B_etot","dsm2BE_etot"};
  c->Divide(1,3);
 
  int i;
  int k=0;
  char name[20];
  for(i=0;i<3;i++) {
    c->cd(1+i);
    gPad->SetLogy(0);
    int ii;
    for(ii=0;ii<=1;ii++) {  
      sprintf(name,"%s%d",nameA[i],ii);
      GetHisto(fd,name,k);
      if(ii==0 ) {
	h[k]->Draw();
	TString tt=h[k]->GetTitle();
	tt+="  COLORS:   bit=0 BLUE , bit=1 RED";
	h[k]->SetTitle(tt);
      } else h[k]->Draw("same");
      if( ii==0&&h[k]->Integral()>0 ) gPad->SetLogy();
      k++;
    }
  }
  //  return;

  // lock maxY
  float ymax=0;
  int j;
  for(j=0;j<6;j++) {
    if(h[j]==0) continue;
    if(ymax< h[j]->GetMaximum() ) ymax= h[j]->GetMaximum();
  }
  
  for(j=0;j<6;j++) {
    if(h[j]==0) continue;
    h[j]->SetMaximum(ymax*1.1);
  }
}
  
//--------------------------------------
//--------------------------------------
void  eeTrigAdjJPcor(FileType fd, TPad *c, char *mode ) {

  char tit[100];
 
  c->Divide(3,2);

  char *core="JP";
  int j;
  for(j=0;j<6;j++) { 
    sprintf(tit,"%s%d%d%s",core,j+1,((j+1)%6)+1,mode);
    //printf("%s %d\n",tit,j);
    GetHisto(fd,tit,j); 
    c->cd(j+1);
    gPad->SetLogz(0);
    h[j]->Draw("colz");
    if ( h[j]->Integral()>0) gPad->SetLogz();
  }
  
}

//--------------------------------------
//--------------------------------------
bool  useTwMask(const char *fname, EemcTwMask *m) {
  const int mx=1000;
  char buf[mx];
  
  printf("EEqaPresenter::useTwMask(\'%s') ...\n",fname);
  
  FILE * fd=fopen(fname,"r");
  int nok=0;
  int nM=1;
  m->txtH=new TPaveText(0,0.,1,1);
  TString myTxt="ETOW masked (cr-ch-name): ";
  if(fd==0) goto abandon;
  while (1) {
    char * ret=fgets(buf,mx,fd);
    if(ret==0) break ; //EOF 
    if(buf[0]=='#') continue; // skip comment
    if(buf[0]=='\n') continue; // skip empty lines
    // printf("uu=%s=%d\n",buf,ret);
    char name[100]; 
    int cr,ch;
    int sec,isub,jeta,jphi;
    int  n=sscanf(buf,"%d %d %s",&cr, &ch, name);
    if(n!=3)  goto abandon;
    if(cr<1 || cr>m->nCr || ch<0 || ch>=m->nCh)  goto abandon;
    if(name[2]!='T')   goto abandon;
    sec=atoi(name);
    isub=name[3]-'A';
    jeta=atoi(name+4);
    jphi=(sec-1)*5+isub;
    // printf("%s sec=%d isub=%d jeta=%d, jphi=%d\n",name, sec,isub,jeta,jphi);

    m->crCh[cr-1][ch]=1;
    int jj=m->crG[cr-1].GetN();
    m->crG[cr-1].SetPoint(jj,ch,10);
    m->crG2[cr-1].SetPoint(jj,250,ch);
    m->crG2[cr-1].SetPointError(jj,250,0.);

    jj=m->phiG.GetN();
    m->phiG.SetPoint(jj,jphi,jeta);
    char tt[100];
    sprintf(tt,"%d-%d-%s,  ",cr,ch,name);  
    myTxt+=tt;
    nok++;
    nM++;
    printf("mask ETOW cr=%d ch=%d =%s=\n",cr,ch,name);
    if(nM%4==0) {
       m->txtH->AddText(myTxt);
       myTxt=" ";
    }
  }
  m->nMask=nok;
  m->txtH->AddText(myTxt);  m->txtH->AddText("--");
  printf(" got %d masked towers\n",nok); 

  int i;
  for(i=0;i<6;i++){
    m->crG[i].SetMarkerStyle(23);
    m->crG[i].SetMarkerColor(kRed);
    m->crG2[i].SetMarkerStyle(1);
    m->crG2[i].SetLineColor(kRed);
  }
  
  m->phiG.SetMarkerStyle(24);
 
  return 1;
 abandon: // any error has happened (this is a new approach for me, JB)
  m->clear();
  m->txtH->AddText("List of ETOW hot towers not found");  m->txtH->AddText("--");
  printf(" EEqaPresenter::useTwMask() FAILED\n");
  return 0;
}


//--------------------------------------
//--------------------------------------
void addJPphiLimits(TH1 *h){
  TList *Lx= h->GetListOfFunctions(); 
  if(Lx->GetSize()>0) return; // do it just once
  int i;
  for(i=0;i<=5;i++) { // boundarioes for JP
    float x1=2.5+i*10;
    TLine *ln=new TLine(x1,-0.2,x1,12.5); 
    Lx->Add(ln);
    int jpid=i+2;
    if(jpid==7)jpid=1;
    TString aa="Jet Patch "; aa+=jpid;
    TText *tt=new TText(x1+1,12.6,aa);
    tt->SetTextSize(0.08);
    Lx->Add(tt);
  }
 
}

//--------------------------------------
//--------------------------------------
void eeJpQaMinMax(TH1 *hh) {
    hh->SetAxisRange(0.5,6.4);
    hh->SetMinimum(0.);
    int ib= hh->GetMaximumBin();
    float yMax=hh->GetBinContent(ib);
    ib= hh->GetMinimumBin();
    float yMin=hh->GetBinContent(ib);
    float r=0,er=999;
    if(yMin<=0) yMin=1;
    if(yMax<=0) yMax=1;    
    r=yMin/yMax;
    er=r*TMath::Sqrt(1/yMax + 1/yMin);
    printf("JP min/max=%.2f +/- %.2f  (min=%.0f max=%.0f) \"%s\"\n",r,er,yMin, yMax,hh->GetTitle());
    printf("#JP %.2f %.2f %.0f %.0f :%s\n",r,er,yMin, yMax,hh->GetTitle());
}
