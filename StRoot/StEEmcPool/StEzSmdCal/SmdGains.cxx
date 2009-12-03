// $Id: SmdGains.cxx,v 1.11 2009/12/03 22:35:03 ogrebeny Exp $
 
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
#include "TMath.h"
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
  // sww 1/17/07 - change adcMax from 100 -> 140
  adcMin=40;  adcMax=140; // SMD
   minSum=50;
  // sww 1/17/07 - change maxRelEr from 0.4 -> 0.5
  maxRelEr=0.5; // maximal relative error at any stage of calculations
  minMipEne=0.4;// (MeV) lower/upper thres for Landau Ene fit
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

 
  for(i=0;i<mxH;i++) 
    if(hA[i]) HList->Add(hA[i]);
 
  //................ TGRaphs
  TGraphErrors*  gr=new TGraphErrors;

  grA[0]=0;
  
  gr=new TGraphErrors;
  gr->SetMarkerStyle(24);
  gr->SetName("mpvN"+plCore); // MPV of Landau fit
  gr->SetTitle(plCore+ " MPV of Landau fit to N 2-strips; strip ID; MIP energy (MeV)");
  grA[1]=gr;


  for(i=0;i<mxH;i++) 
    if(grA[i]) HList->Add(grA[i]);
  
  //............... other initializations ...........
  printf("cuts for %s : adcMin=%d ,adcMax=%d minSum=%d maxRelEr=%f\n",plCore.Data(),adcMin,adcMax,minSum, maxRelEr);

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
void SmdGains::doGainCorr(int str1, int str2, int ns, int pl){

  TGraphErrors  *gr= grA[1];
  printf("doGainCorr() for %s, average over %d-strips pl=%d\n",gr->GetName(),ns,pl);
  TString nn=gr->GetName();

  if(pl==0) {
    c2=new TCanvas(nn,nn,300,400);
  } else {
    c2=new TCanvas(nn,nn,600,800);
  }

  c2->Divide(5,6);
  int i,k;
  for(i=str1,k=1; i<mxS; i+=ns,k++) {
    if(i>=str2) break;
    c2->cd(k);
     avrMipNEne(i,ns);
  }
  c2->cd(29);  hA[1]->Draw();
  c2->cd(30);  hA[2]->Draw();

    if( pl&1) c2->Print(nn+".ps");
  // if( pl&2) c2->Print(nn+".gif");

}




//-----------------------------------------
//-----------------------------------------
void SmdGains:: avrMipNEne( int str1,int ns){
  // sum MIP ene (from 2 strips) of ns strips
  char tit[100];
  assert(str1>0 && ns>0);
  int i;

  TH1F *hs=0;
  for(i=str1;i<str1+ns;i++) {
    if(i>mxS) break;
    sprintf(tit,"e%s%03d",plCore.Data(),i);
    TH1F* h= (TH1F*)fdIn->Get(tit); assert(h);
    HList->Add(h);
    //  c1->cd(k+1);
    //  h->Draw(); gPad->SetLogy(0);
    
    if(hs==0) {
      hs=(TH1F*) h->Clone();
    } else {
      hs->Add(h);
    }
  }

  sprintf(tit,"avr%s%03d",plCore.Data(),str1);
  hs->SetName(tit);
  TString nn=hs->GetTitle(); nn="avr"+nn;
  hs->SetTitle(nn);
  hs->Draw();
  HList->Add(hs);
  float sum=hs->Integral();
  hs->Rebin(3);
  if(sum<150)  hs->Rebin();

  // printf("%s --> sum=%.1f\n",hs->GetName(),sum );
  if(sum<50) return ;

  hs->Fit("landau","RQ","", minMipEne, maxMipEne);
  TF1* f=hs->GetFunction("landau");
  assert(f);
  f->SetLineColor(kRed);
  f->SetLineWidth(1);
  double *par=f->GetParameters();
  double *epar=f->GetParErrors();
  float   mpv=par[1];
  float   empv=epar[1];
  float rerr=0;
  
  hA[1]->Fill(mpv) ;
  hA[2]->Fill(par[2]) ;

  if(par[1]>0) rerr=empv/mpv;
  hs->SetAxisRange(0,5.);
 
  printf("%s MPV=%.2f +/- %.2f (%.1f%c) \n",hs->GetName(),mpv,empv,rerr*100.,37);
  
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
void SmdGains:: plTGraph(const Char_t *shpFunc,int ig, int pl){
  // sum MIP ene (from 2 strips) of k strips

  TGraphErrors  *gr= grA[ig];
  assert(gr);
  printf("plot %s\n",gr->GetName());
  TString nn=gr->GetName();
  nn="C"+nn;  
  c2=new TCanvas(nn,nn,300,250);
  gr->Draw("AP");
  gr->Fit(shpFunc);
  gr->SetMinimum(0.7);
  gr->SetMaximum(1.8);

  gnCorFn=gr->GetFunction(shpFunc); assert(gnCorFn);
  gnCorFn->SetLineColor(kRed);
  gnCorFn ->SetLineWidth(2);

  if(ig==1 ) {
    TList *Lx;  TLine *ln;
    Lx=gr->GetListOfFunctions();    assert(Lx);
    ln=new TLine(1,idealMipEne, mxS,idealMipEne); ln->SetLineColor(kBlue); Lx->Add(ln);
  }


  if( pl&1) c2->Print(nn+".ps");
  if( pl&2) c2->Print(nn+".gif");

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

void SmdGains::fitSlopesSmd(int str1, int str2, int pl) {
  char tit[100];
  int ns=str2-str1+1;
  assert(str1>0 && ns>0);
  sprintf(tit,"%02d%c%03d+%d",sectID,planeUV,str1,ns);
  printf("fitSlopes() %s\n",tit);
  TString ttC=tit;

  TLine *ln0=new TLine(0,0,0,5000); // memory leak
  ln0->SetLineColor(kGreen);

  if(c1==0) c1=new TCanvas("aa1","aa1",800,700);// big
  c1->Clear();
  if(pl) {
    c1->Divide(5,6);
  } else {
    c1->Divide(2,2);
  }
  c1->SetName(ttC);
  c1->SetTitle(ttC);
  
  int k=0;
  int i;
  TList *Lx;  TLine *ln;
  for(i=str1;i<=str2;i++,k++) {
    if(k>mxS) break;
    StripG *s=&str[i-1];
    sprintf(tit,"a%s%03d",plCore.Data(),i);
    TH1F* h= (TH1F*)fdIn->Get(tit); assert(h);
    h->Rebin(2);
    HList->Add(h);
    c1->cd(k+1);
    h->SetAxisRange(adcMin,adcMax);
    float sum=h->Integral();
    h->SetAxisRange(-20,1.5*adcMax);
    //printf(" %s sum=%f\n",h->GetName(),sum);
    
    hA[0]->Fill(i,sum);
    s->sum1=(int)sum;
    h->Draw(); h->SetLineColor(kBlue);gPad->SetLogy();
    gPad->SetGridx(0);      gPad->SetGridy(0);
    float ymax=6000-str1*10;    h->SetMaximum(ymax);
    float ymin=100.9-str1/2.7;   
   // sww 1/17/07 change test from 30 ->300
    if(i<=300) ymin=0.9;
    h->SetMinimum(ymin);    

    Lx=h->GetListOfFunctions();    assert(Lx);
    ln=new TLine(adcMin,0,adcMin,30000); ln->SetLineColor(kMagenta);ln->SetLineStyle(2);
    Lx->Add(ln);
    ln=new TLine(adcMax,0,adcMax,30000);  ln->SetLineColor(kMagenta); ln->SetLineStyle(2);
    Lx->Add(ln);
    if(sum<minSum) { s->flag+=1; continue;}
    h->Fit("expo","RQ","",adcMin,adcMax);

    // ....exceptions in 2005 data, day 49
    if(strstr(h->GetName(),"a02V269")) h->Fit("expo","RQ","",60,130);
    if(strstr(h->GetName(),"a06U077")) h->Fit("expo","RQ","",80,150); // was wrong [800,150]
       //... end
    ln0->Draw();
    TF1* f=h->GetFunction("expo");
    f->SetLineColor(kRed);
    f->SetLineWidth(1);
    double *par=f->GetParameters();
    double *epar=f->GetParErrors();

    s->sl=par[1];
    s->esl=epar[1];
    if(epar[1]>maxRelEr *TMath::Abs(par[1])) {
      s->flag+=2;
    } 
  }

  if(pl&1) c1->Print(ttC+".ps");
  if(pl&2) c1->Print(ttC+".gif");
 
}


//-----------------------------------------
//-----------------------------------------

void SmdGains::fitSlopesTile(int eta1, int nEta, char cT, int pl) {
  char tit[100];
  assert(eta1>0 && nEta>0);
  sprintf(tit,"%02d%cA-E%02d+%d",sectID,cT,eta1,nEta);
  printf("fitSlopes() %s\n",tit);
  TString ttC=tit;
  float xLow=-50;

  if(cT=='T') { adcMin=15;  adcMax=45; } // towers
  if(c1==0) c1=new TCanvas("aa2","aa2",800,700);// big

  c1->Clear();
  if(pl)
    c1->Divide(5,6);
  else
    c1->Divide(2,2);
  c1->SetName(ttC);
  c1->SetTitle(ttC);
  
  int k=0;
  int i;
  TList *Lx;  TLine *ln;
  for(i=eta1;i<eta1+nEta;i++) {
    char sub='A';
    for(sub='A';sub<='E';sub++) {
      k++;
      sprintf(tit,"a%02d%c%c%02d",sectID,cT,sub,i); 
      TH1F* h= (TH1F*)fdIn->Get(tit); assert(h);
      // h->Rebin(2);
      HList->Add(h);
      c1->cd(k);
      h->SetAxisRange(adcMin,adcMax);
      float sum=h->Integral();
      h->SetAxisRange(xLow,1.5*adcMax);
      if(cT=='T') h->SetAxisRange(-20,100);
      // printf(" %s sum=%f\n",h->GetName(),sum);
      
      h->Draw(); h->SetLineColor(kBlue);
      gPad->SetLogy();
      gPad->SetGridx(0);      gPad->SetGridy(0);
      h->SetMinimum(.9);    h->SetMaximum(9000);
      //if(sum<4*minSum)  h->Rebin();
      
      Lx=h->GetListOfFunctions();    assert(Lx);
      ln=new TLine(adcMin,0,adcMin,30000); ln->SetLineColor(kMagenta); Lx->Add(ln);
      ln=new TLine(adcMax,0,adcMax,30000);  ln->SetLineColor(kMagenta);  Lx->Add(ln); 
      float sl=0,esl=0;
      if(sum>minSum) { //do fit
	h->Fit("expo","RQ","",adcMin,adcMax);
	TF1* f=h->GetFunction("expo");
	f->SetLineColor(kRed);
	f->SetLineWidth(1);
	double *par=f->GetParameters();
	double *epar=f->GetParErrors();
	sl=par[1];
	esl=epar[1];
      }

      printf("# %s %.4f %.4f %f\n",h->GetName(),sl,esl,sum);

    }
  }

  if(pl&1) c1->Print(ttC+".ps");
  if(pl&2) c1->Print(ttC+".gif");
 
}



//-------------------------------------------------
//-------------------------------------------------
void SmdGains:: doSlopesOnly(float fac){
  printf("working on gains from slopes only, fac=%f\n",fac);

  int i;
  // ...... relative gains
  for(i=0;i<mxS;i++) {
    StripG *s=str+i;
    if(s->sl<0){ // calculate is possible
      s->gc=-fac/s->sl;
      s->egc=s->esl*s->gc/TMath::Abs(s->sl);
    }
    if(s->egc>maxRelEr *s->gc) {
      s->flag+=4;
      s->gc=s->egc=0;
    }

  }
  
}

#if 0
//-------------------------------------------------
//-------------------------------------------------
void SmdGains:: finish(int k) {
  int i;
  for(i=0;i<mxS;i++) {
    StripG *s=str+i;
    s->print();
  }
  
  
}

#endif

//-------------------------------------------------
//-------------------------------------------------
void SmdGains::saveGains(FILE *fd) {
  if(fd==0) fd=stdout;
  int i,nBad=0;
  for(i=0;i<mxS;i++) {
    StripG *s=str+i;
    float rer=-1;
    if(s->gc>0) {
      rer=100.*s->egc/s->gc;
    } else {
      nBad++;
    }
    fprintf(fd,"%s%03d %.1f %.1f (%.1f%c) sum1=%d flag=0x%0x mpv1=%.1f %.1f\n",plCore.Data(),s->id,s->gc,s->egc,rer,37,s->sum1,s->flag,s->mpv1,s->empv1);
  }
  fprintf(fd,"#nBad =%d\n",nBad);
}

//-------------------------------------------------
//-------------------------------------------------
void SmdGains::saveHisto(const Char_t *fname){
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
  if (flag==0) {
    printf("strip id=%d sl=%f esl=%f gc=%f egc=%f sum1=%d mpv1=%f empv1=%f\n",id,sl,esl,gc,egc,sum1,mpv1,empv1);
  } else {
    printf("strip id=%d  --- sum1=%d\n",id,sum1);
  }
}

/*****************************************************************
 * $Log: SmdGains.cxx,v $
 * Revision 1.11  2009/12/03 22:35:03  ogrebeny
 * Fixed compiler warnings, mostly char* -> const char*
 *
 * Revision 1.10  2009/01/26 14:37:42  fisyak
 * Add missing (in ROOT 5.22) includes
 *
 * Revision 1.9  2007/08/21 13:10:04  balewski
 * final, used in 2006 offline calibration by soScott
 *
 *
 * VS: ----------------------------------------------------------------------
 *
 * Revision 1.7  2005/09/29 13:57:57  balewski
 * after SMD gains were rescaled
 *
 * Revision 1.6  2005/08/09 18:46:31  balewski
 * after smd calib in 2005
 *
 * Revision 1.5  2004/11/02 21:29:08  balewski
 * ange hist range
 *
 * Revision 1.4  2004/10/08 14:34:50  balewski
 * as used for PQRUV calib for pp200, 2004
 *
 * Revision 1.3  2004/09/22 00:45:52  balewski
 * ready for calib of smd
 *
 * Revision 1.2  2004/09/14 19:38:43  balewski
 * new version, SMD calib is now too complicated
 *
 * Revision 1.1  2004/09/11 04:57:34  balewski
 * cleanup
 *
 *
 *
 ********************************************************************/


