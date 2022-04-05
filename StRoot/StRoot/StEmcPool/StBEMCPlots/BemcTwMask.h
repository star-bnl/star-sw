#ifndef BemcTwMask_H
#define BemcTwMask_H

#include <TGraph.h>
#include <TGraphErrors.h>
#include <TPaveText.h>

class BemcTwMask {
public:
  BemcTwMask();
  ~BemcTwMask();

  void clear();

  enum {nPl=4,nCh=1200};
  Char_t crCh[nPl][nCh];
  TGraph crG[nPl];
  TGraphErrors crG2[nPl];
  TPaveText *txtH;
  int nMask;
};

bool useBtowMask(const Char_t *fname, BemcTwMask *m);

#endif
