/*!
  \class StMultiH1F
  
  StMultiH1F allows multiple similar TH1F histograms to be
  easily plotted on one graph

*/

#ifndef ClassStMultiH1F
#define ClassStMultiH1F

#include "TH2.h"
class TString;
#define StMultiH1FMaxBins 10
// change in MaxBins requires change in ClassDef version

class StMultiH1F : public TH2F {
 public:
  StMultiH1F();
  StMultiH1F(const char *name,const char *title,Int_t nbinsx,Axis_t xlow,
	     Axis_t xup,Int_t nbinsy);
  StMultiH1F(const char *name,const char *title,Int_t nbinsx,Double_t *xbins,
	     Int_t nbinsy);
  virtual ~StMultiH1F();
  virtual        void Draw(Option_t *option="");
  virtual        void Draw2F(Option_t *option="") { TH2F::Draw(option); } // access inherited
  virtual        void SetNames(Int_t   ybin, const char* name)
                              { if (ybin<StMultiH1FMaxBins) names[ybin] = name; }
  virtual        void SetNames(Float_t ybin, const char* name)
                              { SetNames((Int_t) ybin, name); }
  virtual const char* GetNames(Int_t   ybin) const
                              { return (ybin<StMultiH1FMaxBins ? names[ybin].Data() : 0); }
  virtual const char* GetNames(Float_t ybin) const
                              { return GetNames((Int_t) ybin); }
  // Overload the Rebin() function to allow naming of y bins with TH2F pointer
  virtual        TH1* Rebin(Int_t ngroup, const char* newname)
                              { SetNames(ngroup, newname); return 0; }
  virtual        TH1* Rebin(Int_t ngroup, const char* newname, const Double_t* xbins)
                              { SetNames(ngroup, newname); return 0; }
  // Overload the SetBarOffset() function to allow offsetting of y bins vertically
  virtual        void SetBarOffset(Float_t offset);
  virtual    Double_t GetNonZeroMinimum() const;
  virtual    Double_t GetNonZeroMaximum() const;
  virtual        void SavePrimitive(std::ostream& out, Option_t* option = "");
  virtual       TH1F* GetSubHist(Int_t ybin) { return (subHists ? subHists[ybin] : 0); }
  virtual       TH1F* GetAHist() { return aHist; }

  /// Saves subhistograms (TH1F) along with this composite object
  virtual       Int_t Write(const char *name=0, Int_t option=0, Int_t bufsize=0);

 protected:
  TString names[StMultiH1FMaxBins];
  Float_t fMOffset;
  virtual       TH1F* XProjection(const char* name, Int_t ybin=-1);
  TH1F** subHists; //!
  TH1F*  aHist;    //!
  ClassDef(StMultiH1F,2)
};

#endif

// $Id: StMultiH1F.h,v 1.12 2018/05/03 16:04:58 smirnovd Exp $
// $Log: StMultiH1F.h,v $
// Revision 1.12  2018/05/03 16:04:58  smirnovd
// Override Write() to save sub histograms in StMultiH1F
//
// Revision 1.11  2016/05/27 18:02:41  genevb
// Garbage collection (Coverity), remove unnecessary ROOT types
//
// Revision 1.10  2013/11/22 16:48:39  genevb
// Access to inherited Draw() functions
//
// Revision 1.9  2013/11/21 22:22:48  genevb
// Protect against array out-of-bounds, use inherited axis handles
//
// Revision 1.8  2012/06/11 15:05:34  fisyak
// std namespace
//
// Revision 1.7  2008/07/09 20:52:38  genevb
// Implement SavePrimitive functions
//
// Revision 1.6  2006/08/10 20:46:24  genevb
// additional Rebin() interface for TH1.h vers. 1.79, ROOT 5.13
//
// Revision 1.5  2002/04/23 01:59:16  genevb
// New offset abilities
//
// Revision 1.4  2000/09/05 19:54:46  genevb
// Erroneous delete in destructor removed
//
// Revision 1.3  2000/08/28 19:21:05  genevb
// Improved projection code
//
// Revision 1.2  2000/08/25 15:46:42  genevb
// Added stats box, legend names
//
// Revision 1.1  2000/07/26 22:00:28  lansdell
// new multi-hist class for superimposing the x-projections of y-bins (of a TH2F histogram) into one TH1F histogram
//

