/**********************************************************************
 *
 * $Id: StEStructSupport.h,v 1.1 2004/07/01 00:37:17 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description: Simple helper class for calculating
 *              delta-rho, delta-rho/rho, and delta-rho/sqrt(rho)
 *              plus some other goodies
 *
 ***********************************************************************/
#ifndef __STESTRUCTSUPPORT__H
#define __STESTRUCTSUPPORT__H

#include "TROOT.h"

class TH1;
class TH2;
class TH1D;
class TH2F;
class TH1F;
class TFile;

class StEStructSupport : public TObject {

  TFile* mtf;
  float mNbar;
  int   mbgMode;
  char* mtmpString;

  bool  goodName(const char* name); // test if name is one of ours
  char* getFrontName(int itype); 
  const char* getTypeName(int itype);
  const char* getChargeSignName(int ics);
  char* prepend(const char* name, const char* s1);
  char* swapIn(const char* name, const char* s1, const char* s2);

  StEStructSupport(){};   

public:

  StEStructSupport(TFile* tf, int bgmode, float nbar=1.);
  virtual ~StEStructSupport();
  void setTFile(TFile* tf);
  void setNBar(float nbar);
  void setBGMode( int mode);

  TH1** getHists(const char* name);
  float* getNorms(TH1** histArray);
  TH1** getLocalClones(const char* name);

  // ++, +-, --, ++ - --
  TH1** buildCommonRatios(const char* name);
  TH1** buildCommonCFunctions(const char* name);
  TH1** buildCommonRFunctions(const char* name);
  TH1** buildCommon(const char* name, int opt=0);

  // LS, US, CD, CI
  TH1** buildChargeTypeRatios(const char* name);
  TH1** buildChargeTypeCFunctions(const char* name);
  TH1** buildChargeTypeRFunctions(const char* name);
  TH1** buildChargeTypes(const char* name, int opt=0);
  
  void scaleBackGround(TH1* sib, TH1* mix);
  TH1* getSqrt(TH1* h);
  void fixDEta(TH2** h, int numHists); // correct triangle in hists with DEta

  // helper for writing ascii dump of set a of histograms to file=fname
  void writeAscii(TH1** h, int numHists, const char* fname);

  ClassDef(StEStructSupport,1)
};

inline void StEStructSupport::setTFile(TFile* tf){ mtf=tf; };
inline void StEStructSupport::setNBar(float nbar){ mNbar=nbar;};
inline void StEStructSupport::setBGMode(int mode){ mbgMode=mode; };


#endif

/***********************************************************************
 *
 * $Log: StEStructSupport.h,v $
 * Revision 1.1  2004/07/01 00:37:17  porter
 * new code previously my StEStructHelper. Takes hists from correltation
 * pass and builds final ressults.  Also the StEStructHAdd.h is a simple
 * replacemnt for my sumyt.C macro which could be expanded later as needed.
 *
 *
 *
 *********************************************************************/



