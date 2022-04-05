// @(#)root/base:$Name:  $:$Id: TNumDeriv.h,v 1.2 2007/10/24 22:45:01 perev Exp $
// Author: Victor Perev   05/07/03

#ifndef ROOT_TNumDeriv
#define ROOT_TNumDeriv
#include <math.h>
#include "TNamed.h"

class TNumDeriv : public TNamed {
public:
           TNumDeriv(const char *name):TNamed(name,"") {fStep=0.01;fIArg=0;fOutLim=0;};
  virtual ~TNumDeriv(){}
  virtual Double_t   Fcn(Double_t  add=0.)=0;  	//Fcn,  must  be overloaded 
          Double_t  DFcn(Double_t  add=0.);  	//dFcn/dPar         
          void      SetOutLimit(int lim=1) 	{fOutLim=lim	;}
          void      SetStep(Double_t step)	{fStep  =step	;}
          void      SetIArg(Int_t iarg)		{fIArg  =iarg	;}
          Int_t     GetIArg()          		{return fIArg	;}

	  Double_t  GetStep()			{return fStep	;}
  static  Double_t  Tiny();
  static  Double_t  Epsilon();

private:
double numericalDerivative(  double x ,double &delta, double &error);

protected:
Double_t fStep;  
Int_t    fIArg;
Int_t    fOutLim;
static Double_t fgTiny;
static Double_t fgEpsilon;
ClassDef(TNumDeriv,0)
};



class TNumDeriv1Test : public TNumDeriv {
public:
           TNumDeriv1Test(double x) :TNumDeriv("DerivTest") {fX=x;SetStep(1e-3);}
  virtual ~TNumDeriv1Test(){};
//  virtual Double_t   Fcn(Double_t  arg){return sin((fX+arg));}  	//Fcn 
    virtual Double_t   Fcn(Double_t  arg){return pow(fX+arg,3);}  	//Fcn 
//
  double fX;
ClassDef(TNumDeriv1Test,0)
};

class TNumDeriv2Test : public TNumDeriv {
public:
           TNumDeriv2Test(double x) :TNumDeriv("DerivTest") {fDT=new TNumDeriv1Test(x);fDT->SetStep(0.1);}
  virtual ~TNumDeriv2Test() {delete fDT;};
  virtual Double_t   Fcn(Double_t  arg){return fDT->DFcn(arg);}  	//Fcn 
//
TNumDeriv1Test *fDT;
ClassDef(TNumDeriv2Test,0)
};


#ifdef __CINT__
#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;
#pragma link C++ class TNumDeriv;
#pragma link C++ class TNumDeriv1Test;
#pragma link C++ class TNumDeriv2Test;
#endif


#endif //ROOT_TNumDeriv












  
