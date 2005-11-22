#ifndef __STESTRUCTFITTER_HH
#define __STESTRUCTFITTER_HH

#include "TROOT.h"
#include "TH2F.h"

class StEStructFitter {


 double mpi;
 double m2pi;

 double mmean; // for 1d fitted bck.
 double msigma;// for 1d fitted bck.

 TH2F** mhists;

 static StEStructFitter* mInstance;

 StEStructFitter();

 public:

 static StEStructFitter* Instance();
 virtual ~StEStructFitter();

 double detadphiFit(double* x, double* par);
 double seconddetadphiFit(double* x, double* par);
 double softCD(double* x, double* par);
 double secondSoftCD(double* x, double* par);
 double hardCI(double* x, double* par);
 double secondHardCI(double* x, double* par);
 double hardCICosine(double* x, double* par);
 double secondHardCICosine(double* x, double* par);
 double detadphiSS(double* x, double* par);
 double seconddetadphiSS(double* x, double* par);

 double softLS(double* x, double* par);
 double secondSoftLS(double* x, double* par);
 double softUS(double* x, double* par);
 double syt(double* x, double* par);


 double mcComponents(double* x, double* par);

 // 2D gaussian on dytsyt ...

 double dytGsytG(double* x, double* par);

 void setMeanSigma(double mean, double sigma);

 double DoubleE(double* x, double* par);
 double secondDoubleE(double* x, double* par);

 ClassDef(StEStructFitter,1)

};

inline void StEStructFitter::setMeanSigma(double mean, double sigma){
  mmean=mean;
  msigma=sigma;
}

#endif
