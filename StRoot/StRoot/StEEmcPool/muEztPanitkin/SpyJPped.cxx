#include<stdio.h>

#include <TH1.h>

#include "SpyJPped.h"
#include "TMath.h"
//------------------------------
//------------------------------
SpyJPped:: SpyJPped(){ //none

};

//------------------------------
//------------------------------
bool
SpyJPped::sense(FILE *fd) {

  const float maxDiff=2.5; // in channals between taget position 
  int mxB=h->GetMaximumBin();
  float xMax=h->GetBinCenter(mxB);
  bool isBad=TMath::Abs(xMax-45)>maxDiff;
  // fprintf(fd,"\n SpyJPped: %s  scanned =%s=  xMax=%f isBad=%d\n",text.Data(),h->GetTitle(),xMax,isBad);
  
  if(isBad) {
    fprintf(fd,"\nSpyJPpedestal: %s  xMax=%.0f not at 45 \n",h->GetName(),xMax);
  }
  return isBad;

}
 
