#ifndef EemcTwMask_H
#define EemcTwMask_H

#include <TGraph.h>
#include <TGraphErrors.h>
#include <TPaveText.h>

class EemcTwMask {
public:
    EemcTwMask();
    ~EemcTwMask();

    void clear();

    enum {nCr = 6, nCh = 120, nPhi = 60, nEta = 12};
    Char_t crCh[nCr][nCh];
    TGraph crG[nCr], phiG;
    TGraphErrors crG2[nCr];
    TPaveText *txtH; // 1=knownHot
    int nMask;
};

bool useTwMask(const Char_t *fname, EemcTwMask *m);

#endif
