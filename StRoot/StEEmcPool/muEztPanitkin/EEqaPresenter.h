#ifndef EEqaPresenter_H
#define EEqaPresenter_H

#include "GenericFile.h"
typedef GenericFile FileType;
class TPad ;
class EemcTwMask;

void plNone(TPad *c);

void eePlot(int page, int panel,FileType fd, TPad *c, const Char_t *eemcTwMaskFilename);

void eeJpQa(FileType fd, TPad *c,EemcTwMask *m);
void eeDaqCorr(FileType fd, TPad *c, int es);
void eeFreq(FileType fd, TPad *c,EemcTwMask *m);
void eeDaqTwCr(FileType fd, TPad *c, EemcTwMask *m); 
void eeDaqTwHot(FileType fd, TPad *c, EemcTwMask *m);
void eeDaqTwHit(FileType fd, TPad *c);
void eeDaqMapmtCr(FileType fd, TPad *c, int);
void eeDaqMapmtStat(FileType fd, TPad *c);

void eeMany1D(FileType fd, TPad *c, const Char_t *core, int nh, int nx, int ny);
void eeDaqSmdA(FileType fd, TPad *cc,const Char_t *, Char_t uv);
void eeEmuVsSimu(FileType fd, TPad *c);

void eeTrigHanks(FileType fd, TPad *c);
void eeTrigDsm0(FileType fd, TPad *c,const Char_t *);
void eeTrigDsm1(FileType fd, TPad *c, const Char_t *);
void eeTrigDsm2HT(FileType fd, TPad *c);

void eeTrigJPsum(FileType fd, TPad *c, const Char_t *);
void eeTrigJPfreq(FileType fd, TPad *c);
void eeTrigAdjJPsum(FileType fd, TPad *c, const Char_t *);
void eeTrigAdjJPcor(FileType fd, TPad *c, const Char_t *);
void eeTrigEtot(FileType fd, TPad *c);

//utility
void addJPphiLimits(TH1 *h);
void eeJpQaMinMax(TH1 *hh);

//--
#endif
