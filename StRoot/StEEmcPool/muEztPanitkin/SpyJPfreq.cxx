#include<stdio.h>

#include <TH1.h>

#include "SpyJPfreq.h"

//------------------------------
//------------------------------
SpyJPfreq:: SpyJPfreq(){ //none

};

//------------------------------
//------------------------------
bool
SpyJPfreq::sense(FILE *fd) {

  const float maxR=1.7;

  int ib= h->GetMaximumBin();
  float yMax=h->GetBinContent(ib);
  ib= h->GetMinimumBin();
  float yMin=h->GetBinContent(ib);
  float r=0,er=999;
  if(yMin<=0) yMin=1;
  if(yMax<=0) yMax=1;
  
  r=yMax/yMin;
  er=r*sqrt(1/yMax + 1/yMin);
  
  bool isBad=(r-er) >maxR;
  
  fprintf(fd,"\nSpyJPfreq: max/min=%.2f +/- %.2f ",r,er);
  if(isBad) 
    fprintf(fd," >%.1f    I S   B A D     (Nmax=%.0f Nmin=%.0f)\n",maxR,yMax, yMin);
  else
    fprintf(fd,"  ~OK  (Nmax=%.0f Nmin=%.0f)\n",yMax, yMin);
  return isBad;

}
 
