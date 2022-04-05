#ifndef __STESTRUCTAUTOFIT_HH
#define __STESTRUCTAUTOFIT_HH

#include "TROOT.h"
#include "TH2D.h"

class StEStructAutoFit {

 public:
 
 //Double_t pow(Double_t x, int k);
 //Double_t funAway(Double_t *x, Double_t *par);
 //Double_t funNear(Double_t *x, Double_t *par);
 //Double_t fun2(Double_t *x, Double_t *par);
 double* autofit8Par(double* best, TH2D* plot, int type, double* allchisq=NULL);
 double* autofit8Parv3(double* best, TH2D* plot, int type, double* allchisq=NULL);
 double* autofit9Par(double* best, TH2D* plot, int type, double* allchiaq=NULL);
 double* autofit11Par(double* best, TH2D* plot, int type, double* allchisq=NULL);
 double* autofit12Par(double* best, TH2D* plot, int type, double* allchisq=NULL);
 double* autofit11Par2G(double* best, TH2D* plot, int type, double yt, double* allchisq=NULL);
 double* autofit11Parv3(double* best, TH2D* plot, int type, double* allchisq=NULL);

 StEStructAutoFit() {};
 ~StEStructAutoFit() {};

 ClassDef(StEStructAutoFit,1)
 
};

#endif
