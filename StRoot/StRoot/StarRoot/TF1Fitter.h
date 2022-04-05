#ifndef TH1Fitter_H
#define TH1Fitter_H 1
#include "TH1.h"
#include "TMath.h"
#include "TF1.h"
#include "TLegend.h"
#include "TCanvas.h"
#include "TNamed.h"

//_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
class TF1Fitter : public TF1
{
public:
TF1Fitter(const char *name="",int nPars=0);
    // methods
          void SetHist(TH1 *hist);
virtual   void Init()=0 ;
virtual double operator()(const double* x, const double* params = 0);
virtual double operator()(double x, double y = 0, double z = 0, double t = 0) const
  {return TF1::operator()(x,y,z,t);}
virtual   void Copy(TObject& f1) const;
virtual double EvalPar(const double *x,const double *par);
virtual   void Draw (const char *opt="same");
protected:
TH1 *fTH1;
double fMean;
double fSigm;

private:

  typedef Double_t(*DummyFuncPtr_t)(Double_t *, Double_t *) ;

ClassDef(TF1Fitter,0)
};

//_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
class TF1GausFitter : public TF1Fitter
{
public:
    TF1GausFitter(const char *name="Gaus"):TF1Fitter(name,3) {}
    // methods
virtual   void Init();

virtual double EvalPar(const double *x,const double *par);
ClassDef(TF1GausFitter,0)
};

//_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
class TF1TwoGausFitter : public TF1Fitter
{
public:
    TF1TwoGausFitter(const char *name="TwoGaus"):TF1Fitter(name,6) {}
    // methods
virtual   void Init();

virtual double EvalPar(const double *x,const double *par);
ClassDef(TF1TwoGausFitter,0)
};
//_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
class TF1GausPol2Fitter : public TF1Fitter
{
public:
    TF1GausPol2Fitter(const char *name="GausPol2"):TF1Fitter(name,6) {}
    // methods
virtual   void Init();

virtual double EvalPar(const double *x,const double *par);
ClassDef(TF1GausPol2Fitter,0)
};















#if 0














class TF1Gaus : public TF1
{
public:
    TF1Gaus(const char *name="Gaus"):TF1(name,(void*)0,0,0,3) {}
    // methods

virtual double EvalPar(const double *x,const double *par)
{
  double p1 = 1./(par[1]*par[1]);
  return par[2]*exp(-0.5*p1*(x[0]-par[0])*(x[0]-par[0]));
}
KlassDef(TF1Gaus,0)
};
   

//_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
class TF1TwoGaus : public TF1
{
public:
    TF1TwoGaus(const char *name="TwoGaus"):TF1(name,(void*)1,0,0,6) {}
    // methods
virtual double EvalPar(const double *x,const double *par)
{
  double p1 = 1./(par[1]*par[1]);
  double p4 = 1./(par[1]+par[4])*par[1]+par[4]);
  return par[2]*exp(-0.5*p1*(x[0]-par[0])*(x[0]-par[0]))
        +par[5]*exp(-0.5*p4*(x[0]-par[3])*(x[0]-par[3]))
}
KlassDef(TF1Gaus,0)
};













//_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
class TF1AuxFitter: public TF1
{
public:
    TF1AuxFitter(int nPars,int nTF,TF1 **tfList):TF1("AuxH1Fitter",(void*)1,0,0,nPars) 
    { mNTF=nTF; mTFList = tfList;}
    // methods
    TF1AuxFitter(){};
virtual void  Copy(TObject& f1) const;
virtual double EvalPar(const double *x,const double *par);
protected:
  int mNTF;
  TF1 **mTFList;

KlassDef(TF1AuxFitter,0)
};
//_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
class TH1Fitter: public TNamed
{
public:
   TH1Fitter(TH1 *th,const char *name="");
  ~TH1Fitter()		{delete mTF1;}
   void SetRange(double xMin,double xMax) {mXmin=xMin; mXmax=xMax;}
   TF1 *GetTF1();
   void Set(TH1 *th);
   void Add(TF1 *fcn);
virtual double Fit (const char *opt="IRV");
virtual   void Draw(const char *opt="");
    int GetNPars() const 	{ return mNPars;}
double *GetPars()      		{ return mPars ;}



protected:

protected:
char mBeg[0];
TH1 *mTH1;		// TH1 pointer
int  mNPars; 		// number of parameters
int  mNTF1s; 		// number of functors
TF1 *mTF1;		//Sum of TF1 list
TF1 *mTF1s[10];	//List of functors
double mXmin;
double mXmax;
double mPars[100];
char mEnd[0];
KlassDef(TH1Fitter,0)
};


//_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
class TwoGausFitter: public TH1Fitter 
{
public:
   TwoGausFitter(TH1 *th,const char *name="TwoGauss");
  ~TwoGausFitter(){;}
void Prep();
double Fit(const char *opt);


KlassDef(TwoGausFitter,0)
};
#endif //0
#endif
