// $Id: StMultiH1F.h,v 1.1 2000/07/26 22:00:28 lansdell Exp $
// $Log: StMultiH1F.h,v $
// Revision 1.1  2000/07/26 22:00:28  lansdell
// new multi-hist class for superimposing the x-projections of y-bins (of a TH2F histogram) into one TH1F histogram
//

#ifndef ClassStMultiH1F
#define ClassStMultiH1F

#include "TH2.h"

class StMultiH1F : public TH2F {
 public:
  StMultiH1F();
  StMultiH1F(const char *name,const char *title,Int_t nbinsx,Axis_t xlow,
	     Axis_t xup,Int_t nbinsy,Axis_t ylow,Axis_t yup);
  StMultiH1F(const char *name,const char *title,Int_t nbinsx,Double_t *xbins,
	     Int_t nbinsy,Axis_t ylow,Axis_t yup);
  StMultiH1F(const char *name,const char *title,Int_t nbinsx,Axis_t xlow,
	     Axis_t xup,Int_t nbinsy,Double_t *ybins);
  StMultiH1F(const char *name,const char *title,Int_t nbinsx,Double_t *xbins,
	     Int_t nbinsy,Double_t *ybins);
  StMultiH1F(const char *name,const char *title,Int_t nbinsx,Float_t  *xbins,
	     Int_t nbinsy,Float_t  *ybins);
  virtual ~StMultiH1F() {}
  virtual void Draw(Option_t *option="");

  ClassDef(StMultiH1F,1)
};

#endif
