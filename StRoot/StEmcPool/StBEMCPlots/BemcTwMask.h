#ifndef BemcTwMask_H
#define BemcTwMask_H

#include <TGraph.h>
#include <TGraphErrors.h>
#include <TPaveText.h>


class BemcTwMask {

 public:
  enum {nPl=4,nCh=1200};
  Char_t crCh[nPl][nCh];
  TGraph crG[nPl];
  TGraphErrors crG2[nPl];
  TPaveText *txtH;
  int nMask;
  void clear() {
    nMask=0;
    memset(crCh,0,sizeof(crCh));
    if(txtH) txtH->Clear();
  }      
  
  BemcTwMask() {
    txtH=0;
    clear();
  }
};


bool useBtowMask(const Char_t *fname, BemcTwMask *m);

#endif
