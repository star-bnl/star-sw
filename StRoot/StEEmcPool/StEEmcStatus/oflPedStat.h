

#ifndef oflPedStat_HH
#define oflPedStat_HH

#include "TObject.h"
#include "TH1F.h"
#include "TFile.h"

class oflPedStat :public TObject {
 public:
  oflPedStat();
  int procDetector(char *name,float maxPedPos, float minPedPos, float maxSig, float minSig, float deadentries, int msk);
  int initRun(int index,TFile *f,TFile *fraw,FILE *fpout,FILE *fplog, int rNum);
  TFile *mfd;
  TFile *mfdraw;
  int mrNum;
  int mindex;
  FILE *mfpout;
  FILE *mfplog;

  ClassDef(oflPedStat,1)
    
    };
    
#endif
