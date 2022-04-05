#include<stdio.h>

#include <TH1.h>

#include "SpyJPfreq.h"
#include "TMath.h"
//------------------------------
//------------------------------
SpyJPfreq:: SpyJPfreq(){ //none

};

//------------------------------
//------------------------------
bool
SpyJPfreq::sense(FILE *fd) {

  const float maxR=0.5;

  int ib= h->GetMaximumBin();
  float yMax=h->GetBinContent(ib);
  ib= h->GetMinimumBin();
  float yMin=h->GetBinContent(ib);
  float r=0,er=999;
  if(yMin<=0) yMin=1;
  if(yMax<=0) yMax=1;
  
  r=yMin/yMax;
  er=r*TMath::Sqrt(1/yMax + 1/yMin);
  
  bool isBad=( r+er <maxR ) && ( yMax+yMin >150);
  
  fprintf(fd,"\nSpyJPfreq: min/max=%.3f +/- %.3f (Nmin=%.0f Nmax=%.0f)\n",r,er,yMin, yMax);
  if(isBad) 
    fprintf(fd,"          EEMC JP ratio <%.1f     I S   B A D  !!\n",maxR);
  
  return isBad;

}
 
