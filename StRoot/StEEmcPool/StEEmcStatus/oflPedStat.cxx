#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
  
#include <TFile.h>
#include <TString.h>
#include <TF1.h>
#include <TH1F.h>
#include <TCanvas.h>

#include "oflPedStat.h"

ClassImp(oflPedStat)
//=========================================
//=========================================
  oflPedStat::oflPedStat() {
  //not using constructor at this time
  
}


//=========================================
//=========================================
int 
oflPedStat::initRun(int index,TFile *f,TFile *fraw,FILE *fpout,FILE *fplog, int rNum)
{
  mindex = index;
  mfd=f;
  mfdraw=fraw;
  mfpout=fpout;
  mfplog=fplog;
  mrNum=rNum;
  
  return 0;
}


//=========================================
//=========================================
int 
oflPedStat::procDetector(char *name,float maxPedPos, float minPedPos, float maxSig, float minSig, float deadentries, int msk)
{   
  int nfail=0;
  int nstat=0;
  int nStuckBit=0;
  
  //fail edge strips of mapmt
  if(!strcmp(name+4,"288"))
    {     fprintf(mfplog,"%s is an edge strip\n",name);
    nfail=1;
    fprintf(mfpout,"%s 0x%04x 0x%04x\n",name,nfail,nstat);
    return -1;
    }
  if((strncmp(name,"a03U",4)==0 || strncmp(name,"a09U",4)==0) && atoi(name+4)>283 )
    {     fprintf(mfplog,"%s is an edge strip\n",name);
    nfail=1;
    fprintf(mfpout,"%s 0x%04x 0x%04x\n",name,nfail,nstat);
    return -1;
    }
  if((strncmp(name,"a04V",4)==0 || strncmp(name,"a10V",4)==0) &&  atoi(name+4)> 283 )
    {     fprintf(mfplog,"%s is an edge strip\n",name);
    nfail=1;
    fprintf(mfpout,"%s 0x%04x 0x%04x\n",name,nfail,nstat);
    return -1;
    }
  
  //call histograms and fits
  TH1F *h = (TH1F*)mfd ->Get(name);
  TH1F *hraw = (TH1F*)mfdraw ->Get(name);
  if(h==0 || hraw==0){
    fprintf(mfplog,"individual channel %s  not found\n",name);
    nfail=1;
    fprintf(mfpout,"%s 0x%04x 0x%04x\n",name,nfail,nstat);
    return -1;   
  }
  TF1 *fn = h->GetFunction("fPeak");
  if(fn==0){
    nfail=1;
    fprintf(mfplog,"%s has no fit parameters\n",name);
    fprintf(mfpout,"%s 0x%04x 0x%04x\n",name,nfail,nstat);
    return -1;
  }
  
  //sort by crate/chan
  char title[100],crate[4],txt[3]; int chan=-1;
  sprintf(title,"%s",h->GetTitle()); //sprintf(title2,"%s",h->GetTitle());
  if(strncmp(title+35,"tube=",5)==0 && name[3]!='T') {
    sprintf(txt,"%c%c%c",title[29],title[30],title[31]); chan=atoi(txt); 
    sprintf(crate,"%c%c%c%c",title[40],title[41],title[42],title[43]);
  }
  //printf("name=%s crate=%s chan=%d\n",name,crate,chan);
  
  
  // place hardcoded known problem channels here 
  // for example, a hot tower known to be bad for a whole year can be set here
  
  if(!strcmp(name,"a12TC05")) //tube has high ped always set status flag
    {
      fprintf(mfplog,"%s masked out of trigger for whole run\n",name);
      nstat+=12;
    }
  

  int entries=(int)h->GetEntries();
  int integral=(int)h->Integral();
  //printf("entries %d integral %d\n",entries,integral);
  
  //daq file # entries is wrong since it's done with weighting
  if(entries != integral)
    entries = integral;
  
  int nb=h->GetNbinsX();
  
  float *x = (float*)malloc(nb*sizeof(float));
  x =(float*) h->GetArray();
  
  float ymax=0;
  int i,jfind=-1;
  float sum = 0.0;
  for(i=0;i<nb;i++)
    {  if (ymax < x[i])
      { ymax=x[i];
      jfind=i;
      }
    sum+=x[i];
    }
  if (ymax == sum)
    { fprintf(mfplog,"%s fails- all entries %.0f in one channel\n",name,ymax);
    nfail=1;
    fprintf(mfpout,"%s 0x%04x 0x%04x\n",name,nfail,nstat);
    return -1;
    }
  if(sum < 0.90*entries)
    {
      fprintf(mfplog,"%s fails needed events - %.0f of %d\n",name,sum,entries);
      nfail=1;
      fprintf(mfpout,"%s 0x%04x 0x%04x\n",name,nfail,nstat);
      return -1;
    }
  
  float x0=h->GetBinCenter(jfind);
  float ped = fn->GetParameter(1);
  float sig = fn->GetParameter(2);
  
  //overflow test (for dead tower with counts in overflow bin)
  double overflow=0;
  double zerobinContent=0;
  int zerobin=h->GetXaxis()->FindBin(0.0);
  zerobinContent = h->GetBinContent(zerobin);   
  
  if(name[3]=='T'){ //test for towers
    if(zerobinContent > 0.001*entries) {
      nfail=1;
      printf("%s fails due to ADC=0 counts = %f\n",name,zerobinContent);
      fprintf(mfplog,"%s has to many ADC=0 counts\n",name);
      fprintf(mfpout,"%s 0x%04x 0x%04x\n",name,nfail,nstat);
      return -1;
    }
    overflow = hraw->Integral(ped+100,5000);
    if(overflow>0.001*entries) {
      nfail=1;
      printf("%s fails due to overflow counts = %f\n",name,overflow);
      fprintf(mfplog,"%s has to many overflow counts\n",name);
      fprintf(mfpout,"%s 0x%04x 0x%04x\n",name,nfail,nstat);
      return -1;
    }
  }
  else { //test for mapmt's
    overflow = hraw->Integral(ped+200,5000);
    if(overflow>0.01*entries) {
      nfail=1;
      printf("%s fails due to overflow counts = %f\n",name,overflow);
      fprintf(mfplog,"%s has to many overflow counts\n",name);
      fprintf(mfpout,"%s 0x%04x 0x%04x\n",name,nfail,nstat);
      return -1;
    }
  }
  
  //stuck bit test
  int adc;
  int nonzerohits=0;
  int bitcompare=1+2+4+8;
  int biton=bitcompare;
  int bitoff=0;
  
  adc = (int)ped + 1;
  for (int k=0; k < (hraw->GetNbinsX()); k++)
    {
      if (hraw ->GetBinContent(k)>0)
	{
	  bitoff = bitoff | adc;
	  biton = biton & adc;
	  nonzerohits += 1;
	}
      adc += 1;
    }
  
  if (name[3]=='T') { //only check for stuck bits on towers
    if ( (bitoff & bitcompare) != bitcompare && nonzerohits >= 8)
      {
	printf("%s bit stuck off \n",name);
	fprintf(mfplog,"%s has stuck bits\n",name);
	nstat+=2;
	nStuckBit=1;
      }
    else if(biton && nonzerohits >= 5 && bitoff>70) //not dead towers
      {
	printf("%s bit stuck on \n",name);
	fprintf(mfplog,"%s has stuck bits\n",name);
	nstat+=2;
	nStuckBit=1;
      }
  }
  //make sure we catch known stuck bits 
  if((!strcmp(name,"a04TB07")||!strcmp(name,"a06TA07")||!strcmp(name,"a08TB07")) && nStuckBit==0){
    printf("%s missed stuck bit, but hardcoded fix\n",name);
    fprintf(mfplog,"%s missed stuck bit, but hardcoded\n",name);
    nstat+=2;
    nStuckBit=1;
  }
  
  //other ped QA
  if(fabs(x0-ped)> 1.5 && !nStuckBit){
    fprintf(mfplog,"%s fails, diff of x0=%.0f and ped =%.1f > 1.5\n",name,x0,ped);
    nfail=1;
  }
  if(ped <= 0.0){
    fprintf(mfplog,"%s fails, ped <= 0\n",name);
    nfail=1;
  }
  if(ped >0 && ped < minPedPos){
    fprintf(mfplog,"%s fails,%.4f, ped too low\n",name,ped);
    nfail=1;
  }  
  if(ped > maxPedPos) {
    fprintf(mfplog,"%s,%.4f, ped too high\n",name,ped);
    nstat+=16; 
  }
  
  if (sig < minSig && !nStuckBit){
    fprintf(mfplog,"%s, %.4f, sig too low\n",name,sig);
    nstat+=2; 
  }
  // wide ped set status bit 2^7
  if (sig > maxSig){
    fprintf(mfplog,"%s, %.4f, sig too high\n",name,sig);
    nstat+=128;
  }
  
  //dead channel test
  int start5sig=jfind -(int)(5.0*sig);
  int end5sig = jfind +(int)(5.0*sig);
  float sum2=0.0;
  for(i=start5sig; i<=end5sig; i++)
    {   sum2+=x[i];
    }
  if(sum2 >= deadentries*entries && !nStuckBit){
    fprintf(mfplog,"%s ped=%.3f x0=%.0f sig=%.3f sum2=%.0f start5sig=%d end5sig=%d jfind=%d\n",name,ped,x0,sig,sum2,start5sig,end5sig,jfind);	     
    fprintf(mfplog,"%s is a dead channel\n",name);
    nfail=1;
    nstat+=1;
  }
  
  //write to errs file
  if(nfail > 0 || nstat > 0)
    {  fprintf(mfpout,"%s 0x%04x 0x%04x\n",name,nfail,nstat);}
  
  
  return 0;
}



