/*!
  \class StMultiH2F
  
  StMultiH2F allows multiple similar TH2F histograms to be
  easily plotted on one graph

*/

#ifndef ClassStMultiH2F
#define ClassStMultiH2F

#include "TH3.h"
#include "TH2.h"

class StMultiH2F : public TH3F {
 public:
  StMultiH2F();
  StMultiH2F(const char *name,const char *title,Int_t nbinsx,Axis_t xlow,
	     Axis_t xup,Int_t nbinsy,Axis_t ylow,Axis_t yup,Int_t nbinsz);
  virtual ~StMultiH2F() {}
  virtual        void Draw(Option_t *option="");
  // Probably only the "box" and "cont" options are reasonable here

  virtual        void SetNames(Int_t   zbin, const char* name)
                              { names[zbin] = name; }
  virtual        void SetNames(Float_t zbin, const char* name)
                              { SetNames((Int_t) zbin, name); }
  virtual const char* GetNames(Int_t   zbin) const
                              { return names[zbin].Data(); }
  virtual const char* GetNames(Float_t zbin) const
                              { return GetNames((Int_t) zbin); }
  // Overload the Rebin() function to allow naming of z bins with TH3F pointer
  virtual        TH1* Rebin(Int_t ngroup, const char* newname)
                              { SetNames(ngroup, newname); return 0; }
  virtual        TH1* Rebin(Int_t ngroup, const char* newname, const Double_t* xbins)
                              { SetNames(ngroup, newname); return 0; }
  virtual        void SavePrimitive(std::ostream& out, Option_t* option = "");
 protected:
  TString names[10];
  virtual       TH2D* XYProjection(const char* name, Int_t zbin=-1);
  ClassDef(StMultiH2F,1)
};

#endif

// $Id: StMultiH2F.h,v 1.3 2012/06/11 15:05:34 fisyak Exp $
// $Log: StMultiH2F.h,v $
// Revision 1.3  2012/06/11 15:05:34  fisyak
// std namespace
//
// Revision 1.2  2008/07/09 20:52:38  genevb
// Implement SavePrimitive functions
//
// Revision 1.1  2007/03/13 16:22:32  genevb
// Introduce StMultiH2F class
//
//
