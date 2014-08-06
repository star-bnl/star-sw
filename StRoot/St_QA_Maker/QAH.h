// $Id: QAH.h,v 2.5 2014/08/06 11:43:52 jeromel Exp $ 
// $Log: QAH.h,v $
// Revision 2.5  2014/08/06 11:43:52  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 2.4  2007/03/13 18:44:07  genevb
// Added StMultiH2F support
//
// Revision 2.3  2004/12/13 15:52:36  genevb
// Numerous updates: PMD, primtrk, FPD, QAShift lists
//
// Revision 2.2  2001/04/28 22:05:12  genevb
// Added EMC histograms
//
// Revision 2.1  2000/08/25 16:04:09  genevb
// Introduction of files
//
//
///////////////////////////////////////////////////////////////////////////
//                                                                       //
//  QAH class for instantiating QA Histograms                            //
//                                                                       //
///////////////////////////////////////////////////////////////////////////

#ifndef STAR_StQAH
#define STAR_StQAH

#include "TString.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
class StMaker;

class QAH {

// ************************ Public Functions *****************************
 public:

  QAH() {}
  virtual       ~QAH() {}
  static StMaker* maker;        // maker to which histograms belong
  static TString preString;     // string to prepend to names/titles

  // methods for 1d-hists
  static TH1F* H1F(const Text_t* name, const Text_t* title,
              Int_t nbinsx, Axis_t xlow, Axis_t xup);
  static TH1F* H1F(const Text_t* name, const Text_t* title,
              Int_t nbinsx, const Double_t* xbins);

  // methods for 2d-hists
  static TH2F* H2F(const Text_t* name, const Text_t* title,
              Int_t nbinsx, Axis_t xlow, Axis_t xup,
              Int_t nbinsy, Axis_t ylow, Axis_t yup);
  static TH2F* H2F(const Text_t* name, const Text_t* title,
              Int_t nbinsx, const Double_t* xbins,
              Int_t nbinsy, Axis_t ylow, Axis_t yup);

  // methods for multi 1d-hists
  static TH2F* MH1F(const Text_t* name, const Text_t* title,
              Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy);
  // several similar multi 1d-hists
  static void MMH1F(TH2F** histp, Int_t nhist, const Text_t* name, const Text_t* title,
		    Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy, Int_t first=0);
  // methods for multi 2d-hists
  static TH3F* MH2F(const Text_t* name, const Text_t* title,
              Int_t nbinsx, Axis_t xlow, Axis_t xup, Int_t nbinsy, Axis_t ylow, Axis_t yup,
              Int_t nbinsz);


// the following is a ROOT macro  that is needed in all ROOT code
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: QAH.h,v 2.5 2014/08/06 11:43:52 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}


 protected:
  static TString QAHistName;    // character string for each hist name
  static TString QAHistTitle;   // character string for each hist title
  static const char* NameIt(const char* name); // method for naming histograms
  static const char* TitleIt(const char* name);// method for titling histograms

  ClassDef(QAH,0)
};


#endif
    







