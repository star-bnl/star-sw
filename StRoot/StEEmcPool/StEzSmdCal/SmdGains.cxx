// $Id: SmdGains.cxx,v 1.2 2004/09/14 19:38:43 balewski Exp $
 
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <TClonesArray.h>
#include <TObjArray.h> 
#include <TString.h> 
#include <TCanvas.h> 
#include <TGraphErrors.h> 
#include <TLine.h> 
#include <TF1.h> 
#include <TH1.h> 
#include <TH2.h> 
#include <TFile.h> 

#include "SmdGains.h"


ClassImp(SmdGains)

//--------------------------------------------------
//--------------------------------------------------
SmdGains::SmdGains(){
  HList=0;
  sectID=0;
  fdIn=0;
  c1=c2=0;
  gnCorFn=0;
  planeUV='X';
  memset(hA,0,sizeof(hA));
  memset(grA,0,sizeof(grA));

  idealMipEne=1.3; // (MeV) in 7mm of plastic
  // limits for expo fit
  adcMin=40;
  adcMax=100;
  minSum=50;
  maxRelEr=0.2; // maximal relative error at any stage of calculations
  minMipEne=0.3;// (MeV) lower/upper thres for Landau Ene fit
  maxMipEne=4;
}

//--------------------------------------------------
//--------------------------------------------------
void SmdGains::init(){
  int i;
  assert(HList);
  
  char tt1[100];
  sprintf(tt1,"%02d%c",sectID,planeUV);
  plCore=tt1;

  //............... histos ............
  hA[0]=new TH1F("sum"+plCore,plCore+" Integral from raw spectra; strip ID; total counts",290,0.5,290.5);
   hA[1]=new TH1F("Lm"+plCore,plCore+" Mean of Landau fit to average MIP ene (2strip); MPV (MeV)",30,.5,2);
  hA[2]=new TH1F("Lw"+plCore,plCore+" Width of Landau fit to average MIP ene (2strip); Width=sigma (MeV)",20,0,1);
  hA[3]=new TH1F("fgc"+plCore,plCore+" Final Gain Correction",100,0.5,1.5);
  hA[4]=new TH1F("fegc"+plCore,plCore+" Error of Final Gain Correction",50,0.,0.5);

 
  for(i=0;i<mxH;i++) 
    if(hA[i]) HList->Add(hA[i]);
 
  //................ TGRaphs
  TGraphErrors*  gr=new TGraphErrors;
  gr->SetMarkerStyle(21);
  gr->SetName("gc"+plCore); // final gains 
  gr->SetTitle(plCore+ " calculated gain correction ; strip ID; gain corr");
  grA[0]=gr;
  
  gr=new TGraphErrors;
  gr->SetMarkerStyle(24);
  gr->SetName("mpvN"+plCore); // MPV of Landau fit
  gr->SetTitle(plCore+ " MPV of Landau fit to N 2-strips; strip ID; MIP energy (MeV)");
  grA[1]=gr;

  gr=new TGraphErrors;  gr->SetMarkerStyle(5);
  gr->SetName("mpvS"+plCore); // MPV of Landau fit
  gr->SetTitle(plCore+ " MPV of Landau fit to single strips; strip ID; MIP energy (MeV)");
  grA[2]=gr;

  
  for(i=0;i<mxH;i++) 
    if(grA[i]) HList->Add(grA[i]);
  
  //............... other initializations ...........
  printf("cuts for %s : adcMin=%d ,adcMax=%d minSum=%d maxRelEr=%f\n",plCore.Data(),adcMin,adcMax,minSum, maxRelEr);
  c1=new TCanvas("aa","aa",300,400);

  for(i=0;i<mxS;i++) str[i].id=i+1;

}

//--------------------------------------------------
//--------------------------------------------------
TFile* SmdGains::open(TString fn) {
  fdIn=new TFile(fn);
  assert(fdIn->IsOpen());
  return fdIn;
}



//-------------------------------------------------
//-------------------------------------------------
void SmdGains::doGainCorr(int str1, int str2, int ns){

  TGraphErrors  *gr= grA[1];
  printf("doGainCorr() for %s, average over %d-strips\n",gr->GetName(),ns);
  TString nn=gr->GetName();
  nn="C"+nn;

  c2=new TCanvas(nn,nn,300,400);
  c2->Divide(4,4);
  int i,k;
  for(i=str1,k=1; i<mxS; i+=ns,k++) {
    if(i>=str2) break;
    c2->cd(k);
    avrRelNGain(i,ns);
  }
  c2->cd(15);  hA[1]->Draw();
  c2->cd(16);  hA[2]->Draw();


}

//-----------------------------------------
//-----------------------------------------
void SmdGains::doOneStripEne(int str1, int str2,char *shpFunc){
  char tit[100];
  int ns=str2-str1+1;
  assert(str1>0 && ns>0);
  sprintf(tit,"%02d%c%03d+%d",sectID,planeUV,str1,ns);
  printf("doOneStripEne() for %s\n",tit);

  int i;
  int nOK=0, nTot=0;
  for(i=str1;i<=str2;i++) {
    float rerr=oneStripEne(i);
    nTot++;
    if(fabs(rerr)< maxRelEr) nOK++;
  }
  printf("doOneMipEne summary nOK=%d of nTot=%d\n",nOK,nTot);  
  // return;
  TGraphErrors  *gr= grA[2];
  c1->Divide(1,1);

  TString nn=gr->GetName();
  nn="C"+nn;
  c1->SetName(nn);  c1->SetTitle(nn); 
  c1->cd(1);
  gr->Draw("AP");
  
  gr->Fit(shpFunc);
  TList *Lx=gr->GetListOfFunctions();    assert(Lx);
  TF1* f=gr->GetFunction(shpFunc); assert(f);
  f->SetLineColor(kGreen);
  f ->SetLineWidth(2);
  
  TLine *ln=new TLine(1,idealMipEne/2., mxS,idealMipEne/2.); 
  ln->SetLineColor(kBlue); Lx->Add(ln);
  
}


//-----------------------------------------
//-----------------------------------------
float SmdGains:: oneStripEne( int str1, int pl){
  //  MIP ene from one strip
  // save only of error not too large
  char tit[100];
  assert(str1>0 && str1<=mxS);
  sprintf(tit,"d%s%03d",plCore.Data(),str1);
  TH1F* hs= (TH1F*)fdIn->Get(tit); assert(hs);

   HList->Add(hs);
   // hs->Draw();
   
  float sum=hs->Integral();
  printf("%s --> sum=%.1f ",hs->GetName(),sum );

  if(sum<50) {printf(" ignored-1\n"); return 999;}
  if(sum>200)
    hs->Rebin(2);//was 4
  else
    hs->Rebin(3);//was 6
  hs->Fit("landau","RQ","", minMipEne/2., maxMipEne);
  TF1* f=hs->GetFunction("landau");  assert(f);
  f->SetLineColor(kRed);
  f->SetLineWidth(2);

  double *par=f->GetParameters();
  double *epar=f->GetParErrors();
  float rerr=0;
  if(par[1]>0) rerr=fabs(epar[1]/par[1]);
  hs->SetAxisRange(0,1.2* maxMipEne);

  printf("MPV=%.2f +/- %.2f (%.1f%c) \n",par[1],epar[1],rerr*100.,37);

  StripG *s=&str[str1-1];
  if(rerr>maxRelEr || par[1]<0 ||rerr<0.01) {
    printf("    ignored-2\n");
    s->flag+=16;
    return 888;
  }
  s->mpv1=par[1];
  s->empv1=epar[1];
  //s->print();

  TGraphErrors*  gr=grA[2];
  int n=gr->GetN();
  gr->SetPoint(n,str1,par[1]);
  gr->SetPointError(n,0.,epar[1]);

  if(pl) {
   TString nn=tit; nn="C"+nn;  c1=new TCanvas(nn,nn,400,300);  c1->Clear();
   hs->Draw();
  }

  return rerr;
}


//-----------------------------------------
//-----------------------------------------
void SmdGains:: avrRelNGain( int str1,int ns){
  // sum MIP ene (from 2 strips) of ns strips

  assert(str1>0 && ns>0);

  int i;
  TGraphErrors  *grg= grA[0];
  TGraphErrors  *gr= grA[2];

  //........... find abs gain
  float mpEne, empEne;
  avrMipNEne( str1,ns,mpEne, empEne);

  //.......... calc average single-strip energy
  float sw=0,syw=0;
  for(i=str1;i<str1+ns;i++) {
    if(i>mxS) break;    
    StripG *s=&str[i-1];
    //s->print();
    if(s->empv1 <=0) continue;
    float w=1/s->empv1/s->empv1;
    sw+=w;
    syw+=w*s->mpv1;
  }
  
  float avr1Ene=1;
  if(sw<=0) {
    printf("WARN : str1=%d +%d no single-strip peaks for the group, set to 1\n",str1,ns);
  } else {
    avr1Ene=syw/sw;
  }
  printf("str1=%d ns=%d  avr1Ene=%f  avrMipEne=%f +/- %f\n",str1,ns,avr1Ene,mpEne,empEne);

  TList *Lx=gr->GetListOfFunctions();    assert(Lx);
  TLine * ln=new TLine(str1,avr1Ene,str1+ns,avr1Ene);
  ln->SetLineColor(kRed); 
  ln->SetLineWidth(2); 
  Lx->Add(ln);


  //.......... use both to predict total gain correction

  for(i=str1;i<str1+ns;i++) {
    if(i>mxS) break;    
    StripG *s=&str[i-1];
    //    s->print();
    float agc= mpEne/idealMipEne;
    float er2=empEne/mpEne/agc;
    if(agc<=0) {
      agc=1;
      er2=0;
    }

    float rgc=1;
    float er1=maxRelEr;
    //tmp
    if(0&&s->empv1>0) {
      rgc=s->mpv1/avr1Ene;
      er1=s->empv1/s->mpv1/rgc;
    }

    float gc=agc*rgc;
    // error caclulation
    float egc=gc*sqrt(er1*er1+ er2*er2);
    char tag=' ';
    if(fabs(1-gc) <egc) tag='*';

    s->gc=gc;
    s->egc=egc; 
    float rer=100.*s->egc/s->gc;
    
    int n=grg->GetN();
    grg->SetPoint(n,s->id,s->gc);
    grg->SetPointError(n,0,s->egc);
    
    hA[3]->Fill(s->gc);
    hA[4]->Fill(s->egc);

    printf("%s%d agc=%.2f rgc=%.3f -->gc=%.3f +/- %.3f (%.1f%c) %c\n",plCore.Data(),i,agc, rgc,gc,egc,rer,37,tag);
    // printf("#2 %s%03d %.3f %.3f (%.1f%c) %c \n",plCore.Data(),s->id,s->gc,s->egc,rer,37,tag);
  }

}


//-----------------------------------------
//-----------------------------------------
void SmdGains:: avrMipNEne( int str1,int ns, float &mpv, float &empv){
  // sum MIP ene (from 2 strips) of ns strips
  char tit[100];
  assert(str1>0 && ns>0);
  int i;
  mpv=empv=0; // clear output

  TH1F *hs=0;
  for(i=str1;i<str1+ns;i++) {
    if(i>mxS) break;
    sprintf(tit,"e%s%03d",plCore.Data(),i);
    TH1F* h= (TH1F*)fdIn->Get(tit); assert(h);
    HList->Add(h);
    //  c1->cd(k+1);
    //  h->Draw(); gPad->SetLogy(0);
    
    if(hs==0)
      hs=(TH1F*) h->Clone();
    else
      hs->Add(h);
  }

  sprintf(tit,"avr%s%03d",plCore.Data(),str1);
  hs->SetName(tit);
  TString nn=hs->GetTitle(); nn="avr"+nn;
  hs->SetTitle(nn);
  hs->Draw();
  HList->Add(hs);
  float sum=hs->Integral();
  //  hs->Rebin();
  if(sum<150)  hs->Rebin();

  //printf("%s --> sum=%.1f\n",hs->GetName(),sum );
  if(sum<50) return ;

  hs->Fit("landau","RQ","", minMipEne, maxMipEne);
  TF1* f=hs->GetFunction("landau");
  assert(f);
  f->SetLineColor(kRed);
  f->SetLineWidth(2);
  double *par=f->GetParameters();
  double *epar=f->GetParErrors();
  mpv=par[1];
  empv=epar[1];
  float rerr=0;
  
  hA[1]->Fill(mpv) ;
  hA[2]->Fill(par[2]) ;

  if(par[1]>0) rerr=empv/mpv;
  hs->SetAxisRange(0,5.);
  float mean=hs->GetMean();
  printf("%s MPV=%.2f +/- %.2f (%.1f%c) r=%.2f m=%.2f \n",hs->GetName(),mpv,empv,rerr*100.,37,mean/mpv,mean);
  
  if(rerr>maxRelEr)  {
    for(i=str1;i<str1+ns;i++) {
      if(i>mxS) break;    
      str[i-1].flag+=8;
    }
  }

  float x=str1+ns/2.; 
  TGraphErrors*  gr=grA[1];
  int n=gr->GetN();
  gr->SetPoint(n,x,mpv);
  gr->SetPointError(n,ns/2.,empv);

  return ;
}


//-----------------------------------------
//-----------------------------------------
void SmdGains:: plTGraph( char *shpFunc,int ig){
  // sum MIP ene (from 2 strips) of k strips

  TGraphErrors  *gr= grA[ig];
  assert(gr);
  printf("plot %s\n",gr->GetName());
  TString nn=gr->GetName();
  nn="CC"+nn;  
  c2=new TCanvas(nn,nn,300,400);
  gr->Draw("AP");
  gr->Fit(shpFunc);

  gnCorFn=gr->GetFunction(shpFunc); assert(gnCorFn);
  gnCorFn->SetLineColor(kRed);
  gnCorFn ->SetLineWidth(2);

  if(ig==1 ) {
    TList *Lx;  TLine *ln;
    Lx=gr->GetListOfFunctions();    assert(Lx);
    ln=new TLine(1,idealMipEne, mxS,idealMipEne); ln->SetLineColor(kBlue); Lx->Add(ln);
  }
 }
  
//-----------------------------------------
//-----------------------------------------
void SmdGains:: plFGC(){
  TString nn="fgc"+plCore;
  c2=new TCanvas(nn,nn,500,400);

  c2->Divide(2,2);
  c2->cd(1);  hA[1]->Draw();
  c2->cd(2);  hA[2]->Draw();
  c2->cd(3);  hA[3]->Draw();
  c2->cd(4);  hA[4]->Draw();

 }
  

//-----------------------------------------
//-----------------------------------------

void SmdGains::fitSlopes(int str1, int str2) {
  char tit[100];
  int ns=str2-str1+1;
  assert(str1>0 && ns>0);
  sprintf(tit,"%02d%c%03d+%d",sectID,planeUV,str1,ns);
  printf("fitSlopes() %s\n",tit);
  
  if(c1) {
    c1->Divide(3,3);
    c1->SetName(tit);
    c1->SetTitle(tit);
  }
  
  int k=0;
  int i;
  TList *Lx;  TLine *ln;
  for(i=str1;i<=str2;i++,k++) {
    if(k>mxS) break;
    StripG *s=&str[i-1];
    sprintf(tit,"a%s%03d",plCore.Data(),i);
    TH1F* h= (TH1F*)fdIn->Get(tit); assert(h);
    // h->Rebin(4);
    HList->Add(h);
    c1->cd(k+1);
    h->SetAxisRange(adcMin,adcMax);
    float sum=h->Integral();
    h->SetAxisRange(-25,1.5*adcMax);
    //printf(" %s sum=%f\n",h->GetName(),sum);
    
    hA[0]->Fill(i,sum);
    s->sum1=(int)sum;
    h->Draw(); h->SetLineColor(kBlue);gPad->SetLogy();
    h->SetMinimum(.9);    h->SetMaximum(10000);
    //if(sum<4*minSum)  h->Rebin();

    Lx=h->GetListOfFunctions();    assert(Lx);
    ln=new TLine(adcMin,0,adcMin,30000); ln->SetLineColor(kMagenta); Lx->Add(ln);
    ln=new TLine(adcMax,0,adcMax,30000);  ln->SetLineColor(kMagenta);  Lx->Add(ln);
    if(sum<minSum) { s->flag+=1; continue;}
    h->Fit("expo","RQ","",adcMin,adcMax);
    TF1* f=h->GetFunction("expo");
    f->SetLineColor(kRed);
    f->SetLineWidth(2);
    double *par=f->GetParameters();
    double *epar=f->GetParErrors();

    s->sl=par[1];
    s->esl=epar[1];
    if(epar[1]>maxRelEr *fabs(par[1])) {
      s->flag+=2;
    }
    
  }
}



//-------------------------------------------------
//-------------------------------------------------
void SmdGains:: doSlopesOnly(float fac){
  printf("working on gains from slopes only, fac=%f\n",fac);
  TGraphErrors*  gr=grA[0];
  int i;
  // ...... realtive gains
  for(i=0;i<mxS;i++) {
    StripG *s=str+i;
    if(s->sl<0){ // calculate is possible
      s->gc=-fac/s->sl;
      s->egc=s->esl*s->gc/fabs(s->sl);
    }
    if(s->egc>maxRelEr *s->gc) {
      s->flag+=4;
      s->gc=s->egc=0;
    }

    int n=gr->GetN();
    gr->SetPoint(n,s->id,s->gc);
    gr->SetPointError(n,0,s->egc);
  }
  
}

//-------------------------------------------------
//-------------------------------------------------
void SmdGains:: finish(int k) {
  int i;
  for(i=0;i<mxS;i++) {
    StripG *s=str+i;
    s->print();
  }
  
  c1->Clear();
  c1->Divide(1,2);
  c1->cd(1);  hA[0]->Draw();
  c1->cd(2);  grA[0]->Draw("PA");
  grA[0]->SetMinimum(0.);
  
}

//-------------------------------------------------
//-------------------------------------------------
void SmdGains::saveGains(FILE *fd) {
  if(fd==0) fd=stdout;
  int i,nBad=0;
  for(i=0;i<mxS;i++) {
    StripG *s=str+i;
    float rer=-1;
    if(s->gc>0) 
      rer=100.*s->egc/s->gc;
    else
      nBad++;
    fprintf(fd,"%s%03d %.1f %.1f (%.1f%c) sum1=%d flag=0x%0x mpv1=%.1f %.1f\n",plCore.Data(),s->id,s->gc,s->egc,rer,37,s->sum1,s->flag,s->mpv1,s->empv1);
  }
  fprintf(fd,"#nBad =%d\n",nBad);
}

//-------------------------------------------------
//-------------------------------------------------
void SmdGains::saveHisto(char *fname){
  TString outName;
  if(fname){
    outName=fname;
  } else {
    outName="smd"+plCore;
  }
  outName+=".hist.root";
  TFile f( outName,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),outName.Data());
  HList->Write();
  f.Close();
}

//=============================================
//=============================================
//=============================================
//=============================================

StripG::StripG(){
clear();
}

//-------------------------
void StripG::clear(){
  id=0;
  sl=esl=999;
  sum1=0; 
  gc=egc=0;
  flag=0;
  mpv1=empv1=0;
}

//-------------------------
void StripG::print(){
  if(flag) printf("*");
  if (flag==0) 
    printf("strip id=%d sl=%f esl=%f gc=%f egc=%f sum1=%d mpv1=%f empv1=%f\n",id,sl,esl,gc,egc,sum1,mpv1,empv1);
  else
    printf("strip id=%d  --- sum1=%d\n",id,sum1);
}

/*****************************************************************
 * $Log: SmdGains.cxx,v $
 * Revision 1.2  2004/09/14 19:38:43  balewski
 * new version, SMD calib is now too complicated
 *
 * Revision 1.1  2004/09/11 04:57:34  balewski
 * cleanup
 *
 *
 *
 ********************************************************************/


