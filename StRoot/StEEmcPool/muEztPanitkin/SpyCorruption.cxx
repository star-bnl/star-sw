#include<stdio.h>

#include <TH1.h>

#include "SpyCorruption.h"

//------------------------------
//------------------------------
SpyCorruption:: SpyCorruption(){ //none
  lastSum=0;
};

//------------------------------
//------------------------------
bool
SpyCorruption::sense(FILE *fd) {
  int sumCorr=(int)h->Integral();
  bool isBad=sumCorr>lastSum;
  //fprintf(fd,"\n SpyCorruption: %s  scanned\n",text.Data());
  if(isBad) {
    fprintf(fd,"\nSpyCorruption: %s  total=%d eve,\n    details (type=value):",text.Data(),sumCorr);
    int i;  
    int k=0;
    for(i=1;i<=h->GetNbinsX();i++) {
      fprintf(fd,"%d=%.0f, ",i,h->GetBinContent(i));
      if(h->GetBinContent(i) <=0)   continue; 
      k++;
    } 
    fprintf(fd," --> nTypes=%d\n",k);
    fprintf(fd," for definition see:\n @  http://www.star.bnl.gov/STAR/eemc/software/panitkin2005/\n");
  }
  lastSum=sumCorr;
  return isBad;
}
 
