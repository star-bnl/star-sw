#include <TGraph.h>
#include <TGraphErrors.h>
#include <TPaveText.h>
 
class TPad ;
#ifdef IN_PANITKIN 
  class GenericFile;
  typedef GenericFile  FileType;
#else
  class TFile;
 typedef TFile  FileType;
#endif

class EemcTwMask {
 public:
    enum {nCr=6, nCh=120, nPhi=60,nEta=12};
    char crCh[nCr][nCh];
    TGraph crG[nCr], phiG;
    TGraphErrors crG2[nCr];
    TPaveText *txtH;// 1=knownHot
    int nMask;
    void clear() {
      nMask=0;
      memset(crCh,0,sizeof(crCh));
      if(txtH) txtH->Clear();
   }      

    EemcTwMask() {
      txtH=0;
      clear();
    }
};


void plNone(TPad *c);

void eePlot(int page, int panel,FileType *fd, TPad *c);

void eeJpQa(FileType *fd, TPad *c,EemcTwMask *m);
void eeDaqCorr(FileType *fd, TPad *c, int es);
void eeFreq(FileType *fd, TPad *c,EemcTwMask *m);
void eeDaqTwCr(FileType *fd, TPad *c, EemcTwMask *m); 
void eeDaqTwHot(FileType *fd, TPad *c, EemcTwMask *m);
void eeDaqTwHit(FileType *fd, TPad *c);
void eeDaqMapmtCr(FileType *fd, TPad *c, int);
void eeDaqMapmtStat(FileType *fd, TPad *c);

void eeMany1D(FileType *fd, TPad *c, char *core, int nh, int nx, int ny);
void eeDaqSmdA(FileType *fd, TPad *cc,char *, char uv);

void eeTrigHanks(FileType *fd, TPad *c);
void eeTrigDsm0(FileType *fd, TPad *c,char *);
void eeTrigDsm1(FileType *fd, TPad *c, char *);
void eeTrigDsm2HT(FileType *fd, TPad *c);

void eeTrigJPsum(FileType *fd, TPad *c, char *);
void eeTrigJPfreq(FileType *fd, TPad *c);
void eeTrigAdjJPsum(FileType *fd, TPad *c, char *);
void eeTrigAdjJPcor(FileType *fd, TPad *c, char *);
void eeTrigEtot(FileType *fd, TPad *c);

bool useTwMask(char *fname, EemcTwMask *m);

//utility
void addJPphiLimits(TH1 *h);
void eeJpQaMinMax(TH1 *hh);

//--
