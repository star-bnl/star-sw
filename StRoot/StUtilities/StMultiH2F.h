/*!
  \class StMultiH2F
  
  StMultiH2F allows multiple similar TH2F histograms to be
  easily plotted on one graph

*/

#ifndef ClassStMultiH2F
#define ClassStMultiH2F

#include "TH3.h"
#include "TH2.h"
class TString;
#define StMultiH2FMaxBins 10
// change in MaxBins requires change in ClassDef version

class StMultiH2F : public TH3F {
 public:
  StMultiH2F();
  StMultiH2F(const char *name,const char *title,Int_t nbinsx,Axis_t xlow,
	     Axis_t xup,Int_t nbinsy,Axis_t ylow,Axis_t yup,Int_t nbinsz);
  virtual ~StMultiH2F();
  virtual        void Draw(Option_t *option="");
  // Probably only the "box" and "cont" options are reasonable here
  virtual        void Draw3F(Option_t *option="") { TH3F::Draw(option); } // access inherited

  virtual        void SetNames(Int_t   zbin, const char* name)
                              { if (zbin<StMultiH2FMaxBins) names[zbin] = name; }
  virtual        void SetNames(Float_t zbin, const char* name)
                              { SetNames((Int_t) zbin, name); }
  virtual const char* GetNames(Int_t   zbin) const
                              { return (zbin<StMultiH2FMaxBins ? names[zbin].Data() : 0); }
  virtual const char* GetNames(Float_t zbin) const
                              { return GetNames((Int_t) zbin); }
  // Overload the Rebin() function to allow naming of z bins with TH3F pointer
  virtual        TH1* Rebin(Int_t ngroup, const char* newname)
                              { SetNames(ngroup, newname); return 0; }
  virtual        TH1* Rebin(Int_t ngroup, const char* newname, const Double_t* xbins)
                              { SetNames(ngroup, newname); return 0; }
  virtual        void SavePrimitive(std::ostream& out, Option_t* option = "");
  virtual       TH2D* GetSubHist(Int_t zbin) { return (subHists ? subHists[zbin] : 0); }
 protected:
  TString names[StMultiH2FMaxBins];
  virtual       TH2D* XYProjection(const char* name, Int_t zbin=-1);
  TH2D** subHists; //!
  ClassDef(StMultiH2F,2)
};

#endif

// $Id: StMultiH2F.h,v 1.6 2016/05/27 18:02:41 genevb Exp $
// $Log: StMultiH2F.h,v $
// Revision 1.6  2016/05/27 18:02:41  genevb
// Garbage collection (Coverity), remove unnecessary ROOT types
//
// Revision 1.5  2013/11/22 16:48:39  genevb
// Access to inherited Draw() functions
//
// Revision 1.4  2013/11/21 22:22:48  genevb
// Protect against array out-of-bounds, use inherited axis handles
//
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
