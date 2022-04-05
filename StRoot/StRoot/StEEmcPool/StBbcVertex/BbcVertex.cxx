// $Id: BbcVertex.cxx,v 1.3 2007/07/12 19:25:36 fisyak Exp $
 
#include <assert.h>
#include <stdlib.h>

#include <TClonesArray.h>
#include <TObjArray.h> 
#include <TH1.h> 
#include <TH2.h> 

#include "BbcVertex.h"
#include "BbcHex.h"
#include "TMath.h"
ClassImp(BbcVertex)

//--------------------------------------------------
//--------------------------------------------------
BbcVertex::BbcVertex(){

  nInpEve=0;
  HList=0;
  memset(hA,0, sizeof(hA));
  unixTime0=0;
  cm2tdcCh=-999;
  
  int iew,ih;
  char cEW[mxEW]={'E','W'};
  for(iew=0;iew<mxEW;iew++) {
    for(ih=0;ih<mxHex;ih++) {
      char name[100];
      sprintf(name,"bbc%c%02d",cEW[iew],ih+1);
      BbcHex *x=new BbcHex(ih,name);
      x->setCalib(0,0,0,0);
      x->clear();
      hex[iew][ih]=x;
    }
  }

}

//--------------------------------------------------
//--------------------------------------------------
BbcVertex::~BbcVertex() {/* noop */}


//-------------------------------------------------
//-------------------------------------------------
void BbcVertex::clear(){
  unixTime=0;
  int iew,ih;
  for(iew=0;iew<mxEW;iew++) 
    for(ih=0;ih<mxHex;ih++) 
      hex[iew][ih]->clear();

  onlTdiff=-999;
  onlTdiff=-997;
  zTpc=-998;
}


//-------------------------------------------------
//-------------------------------------------------
void BbcVertex::initRun(int runID){
  printf(" BbcVertex::initRun(%d),  cm2tdcCh=%f\n",runID, cm2tdcCh);
  assert(TMath::Abs(cm2tdcCh)<20);

}

//-------------------------------------------------
//-------------------------------------------------
void BbcVertex::init(){
  int i;
  hA[0]=new TH1F ("onlvd","zTpc - Z(BBC) ; (cm)",50,-100,100);
  hA[1]=(TH1F*)new TH2F ("onlvv","BBC  zTPC vs. onlTdiff;  (TDC ch); Z vert (cm)",50,150,350,50,-200,200);
  hA[2]=(TH1F*)new TH2F ("onlvd2","BBC  (onlZbbc-zTPC)/cm2ch vs. onlTdiff;  (TDC ch); #D Z vert (TDC ch)",50,150,350,50,-20,20);

  for(i=0;i<3;i++) hA[i]->SetLineColor(kGreen);


 hA[7]=(TH1F*)new TH2F ("vd2","BBC  (oflZbbc-zTPC)/cm2ch vs. oflTdiff+256;  (TDC ch); #D Z vert (TDC ch)",50,150,350,50,-20,20);

  hA[8]=(TH1F*)new TH2F ("vk","BBC iW vs. iE",16,.5,16.5,16,0.5,16.5);
  hA[9]=(TH1F*)new TH2F ("va","BBC adcW vs. adcE",156,-0.5,155.5,156,-0.5,155.5);
  hA[10]=(TH1F*)new TH2F ("vt","BBC tdcW vs. tdcE",128,-0.5,255.5,128,-0.5,255.5);
  hA[11]=(TH1F*)new TH2F ("vv","BBC  zTPC vs. oflTdiff+256;  (TDC ch); Z vert (cm)",50,150,350,50,-200,200);

  hA[12]=new TH1F ("vd","zTpc - Z(BBC) ; (cm)",50,-100,100);
  hA[13]=(TH1F*)new TH2F ("vE","BBC  zTpc - Z(BBC) vs. adcE ; ADC; (cm)",120,0.5,120.5,50,-100,100);
  hA[14]=(TH1F*)new TH2F ("vW","BBC  zTpc - Z(BBC) vs. adcW ; ADC; (cm)",120,0.5,120.5,50,-100,100);

  hA[15]=(TH1F*)new TH2F ("vU","TPC vertex Z  vs run time ; minutes; cm ",120,0,60, 50,-200,200);
  hA[16]=(TH1F*)new TH2F ("vD","Ztpc - Zbbc  vs run time ; minutes; cm ",120,0,60, 50,-200,200);

  hA[17]=new TH1F ("ts","sum of T_E + T_W ; raw chan",100,0,300);


  if(HList) {
    for(i=0;i<=17;i++) {
      if(hA[i]==0) continue;
      HList->Add(hA[i]);
    }  
  }

  char cEW[mxEW]={'E','W'};
  int iew,ih;
  for(iew=0;iew<mxEW;iew++) {
    for(ih=0;ih<mxHex;ih++) {
      char tt1[100],tt2[100];
      sprintf(tt1,"w%c%02d",cEW[iew],ih+1);
      sprintf(tt2,"%s , dZ vs. ADC ; ADC(chan) ;#Delta Z (cm)",tt1);
      TH2F *h=new TH2F (tt1,tt2,60,0.5,120.5,25,-100,100);
      hC[iew][ih]=h;
      HList->Add(h);
    }
  }

  clear();  
}
//-------------------------------------------------
//-------------------------------------------------
void BbcVertex::finish(){

  printf("\n  BbcVertex::finish() nInpEve=%d\n",nInpEve);
 
   
}

//-------------------------------------------------
//-------------------------------------------------
int  BbcVertex:: findTime(BbcHex **D){
  int i;
  float tMax=0;
  int iT=-1;
  for(i=0;i<mxHex;i++) {
    // printf("i=%d a=%d t=%d iT=%d tMin=%d\n",i,D[i]->adc,D[i]->tdc,iT,tMin);
    if(D[i]->adc<5) continue;
    if(D[i]->tdc>245) continue;
    if(D[i]->tof<tMax) continue;
    tMax=D[i]->tof;
    iT=i;
   }
  //  printf("minT=%d iT=%d\n",tMin,iT);
  assert(iT>=0);
  return iT;
}


//-------------------------------------------------
//-------------------------------------------------
void  BbcVertex:: export2NN(BbcHex **D){
  int key[mxHex];
  memset(key,0,sizeof(key));
  int k=0, nk=3;

  for(k=0;k<nk;k++) {
    int i;
    float tMax=0;
    int iT=-1;
    for(i=0;i<mxHex;i++) {
      // if(k==0) printf("i=%d a=%d t=%d \n",i,D[i]->adc,D[i]->tdc);
      if(key[i]) continue; 
      if(D[i]->adc<5) continue;
      if(D[i]->tdc>245) continue;
      if(D[i]->tof<tMax) continue;
      tMax=D[i]->tof;
      iT=i;
    }
    if(iT>=0) {
      key[iT]=100+k;
      printf("%d %d %d ",iT,D[iT]->adc,D[iT]->tdc);
    } else {
      printf("-1 0 0 ");
    } 
    // printf("Tmax=%.1f iT=%d\n",tMax,iT);
  }

}


//-------------------------------------------------
//-------------------------------------------------
void BbcVertex::print(){
  printf("\n  BbcVertex::print()\n  dump:\n");
}

//-------------------------------------------------
//-------------------------------------------------
void BbcVertex:: doVertex(){
  if(zTpc>200) return;
  //printf("TPC: Z=%f\n",zTpc); 
  //................ online vertex info
  float onlZ=-(onlTdiff-256)*cm2tdcCh; // now in cm
  float onlDz=onlZ-zTpc; // in cm
  float onlTcorr=onlDz/cm2tdcCh; // in TDC chan
  //printf("onl: tDiff=%f Z=%f Dz=%f Tcorr=%f\n", onlTdiff,onlZ,onlDz,onlTcorr);

  hA[0]->Fill(onlDz);
  ( (TH2F*) hA[1])->Fill(onlTdiff,zTpc);
  ( (TH2F*) hA[2])->Fill(onlTdiff,onlTcorr);


  //............... offline vertex info
  printf("## ");
  export2NN(hex[0]);
  export2NN(hex[1]);
  printf(" %f\n",zTpc);
  return; //tmp  

  int iE=findTime(hex[0]);
  int iW=findTime(hex[1]);
  int kE=iE+1;
  int kW=iW+1;

  float tE=hex[0][iE]->tof;
  float tW=hex[1][iW]->tof;
  float aE=hex[0][iE]->adc;
  float aW=hex[1][iW]->adc;
  float oflTdiff=256.+(tE-tW); 
    
  float oflZ=-(oflTdiff-256)*cm2tdcCh; // now in cm
  float oflDz=oflZ-zTpc; // in cm
  float oflTcorr=oflDz/cm2tdcCh; // in TDC chan
  //printf("ofl: tE=%f tW=%f\n", tE, tW);
  //printf("ofl: tDiff=%f Z=%f Dz=%f Tcorr=%f\n", oflTdiff,oflZ,oflDz,oflTcorr);

#if 0 
  float diff=onlTdiff -oflTdiff;
  if(TMath::Abs(diff)>0.01)
    printf("DIFIDIF: Z=%f  onlTd=%f oflTd=%f  difT=%f\n",zTpc,onlTdiff,oflTdiff,diff);
#endif
  

  //  if(iW!=0) return;
  //if(kE!=3&& kE!=4) return;
  //  if(kW==9) return; // skip one not calibrated
  //  if(kW!=1&& kW!=3&& kW!=6&& kW!=8&& kW!=9&& kW!=13&& kW!=15) return;
  //hex[0][iE]->print();  hex[1][iW]->print();
  // printf(" tDif onl=%d ofl=%f dd=%f\n",b.tDif,tDif, b.tDif-tDif);
  
  ( (TH2F*) hA[8])->Fill(kE,kW);
  ( (TH2F*) hA[9])->Fill(aE,aW);
  ( (TH2F*) hA[10])->Fill(tE,tW);
  ( (TH2F*) hA[11])->Fill(oflTdiff,zTpc);
  ( (TH2F*) hA[7])->Fill(oflTdiff,oflTcorr);
  hA[12]->Fill(oflDz);

  float time1= unixTime-unixTime0;
  time1/=60; // converte to minutes
  ( (TH2F*) hA[15])->Fill(time1,zTpc);
  ( (TH2F*) hA[16])->Fill(time1,oflDz);
  hA[17]->Fill(tE+tW);
  
  calibWalk(hex[1][iW],hex[0][iE],oflDz,hC[0]); // doEast
  calibWalk(hex[0][iE],hex[1][iW],oflDz,hC[1]); // doWest
 
  // old stuff
  ( (TH2F*) hA[13])->Fill(aE,oflDz);
  ( (TH2F*) hA[14])->Fill(aW,oflDz);


if(0) {//new stuff
  int iew,ih;
  int adcMin=5;
  int nT[mxEW];
  for(iew=0;iew<mxEW;iew++){
    nT[iew]=0;
    for(ih=0;ih<mxHex;ih++) {
     if( hex[iew][ih]->adc<adcMin) continue;
     nT[iew]++;
     hex[iew][ih]->print();
    }
  }
  printf("nT-E=%d nT-W=%d\n\n",nT[0],nT[1]);

 }
 
}


//-------------------------------------------------
//-------------------------------------------------
void BbcVertex::calibWalk( BbcHex *x, BbcHex *y, float dz, TH2F **h){
  // int id=0;
  // if (x ->id!=id) return;
  //  //  if( x->adc<25) return;
  // printf(" %d+%d %d %d\n", x->id,y->id, unixTime, unixTime-unixTime0);
 
   assert(y->id>=0 && y->id<mxHex); 
   h[y->id]->Fill(y->adc,dz);
}


//-------------------------------------------------
//-------------------------------------------------
void BbcVertex::readCalib(char *fname) {
  printf("readCalib('%s')\n",fname);
  FILE *fd=fopen(fname,"r");
  //printf("file opened fd=%p %s\n",fd,fname);
  if(fd==0) {
      printf("file %s was not open\n",fname);
      printf(" fix it, (JB)\n\n");
      assert(1==0);
  }
  
  int nd=0,nl=0;;
  while (1  ) {
    const int mx=1000;
    char buf[mx];  
    char * ret=fgets(buf,mx,fd);
    if(ret==0) break;// EOF
    nl++;
    if(buf[0]=='#') continue; // commented out record 
    char name[100];
    float a,b,c,d;
    int n=sscanf(buf,"%s %f %f %f %f",name,&a,&b,&c,&d);
    if(n!=5) {
      printf("faulty %d line in %s\n",nl,fname);
      printf("n=%d buf='%s'\n fix it, (JB)\n\n",n,buf);
      assert(1==2);
    }
    nd++;

    // find this PMT 
    int iew,ih;
    int ok=0;
    for(iew=0;iew<mxEW;iew++) 
      for(ih=0;ih<mxHex;ih++){ 
	if(strstr(hex[iew][ih]->name,name)==0) continue;
	ok=1;
	hex[iew][ih]->setCalib(a,b,c,d);
	break;
      }
    if(ok!=1) {
      printf("faulty BbcPMT name=%s in %d line in %s\n",name,nl,fname);
      printf("n=%d buf='%s'\n fix it, (JB)\n\n",n,buf);
      assert(1==3);
    }
  }

  fclose(fd);  
  printf("BbcVertex::readCalib() done, found %d valid records\n",nd);
  return;
}


/*******************************************************
 * $Log: BbcVertex.cxx,v $
 * Revision 1.3  2007/07/12 19:25:36  fisyak
 * Add includes for TMath for ROOT 5.16
 *
 * Revision 1.2  2004/12/04 05:07:38  balewski
 * export to NN
 *
 * Revision 1.1  2004/08/31 03:44:13  balewski
 * first
 *
 * Revision 1.6  2004/08/26 04:39:40  balewski
 *
 */ 


