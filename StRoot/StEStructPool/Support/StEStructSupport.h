/**********************************************************************
 *
 * $Id: StEStructSupport.h,v 1.7 2006/10/02 22:26:53 prindle Exp $
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
  float* mnpairs; //! for normalization comparing different cuts 
  int   mbgMode;
  char* mtmpString;
  bool  mapplyDEtaFix;

  bool  goodName(const char* name); // test if name is one of ours
  char* getFrontName(int itype); 
  const char* getTypeName(int itype);
  const char* getChargeSignName(int ics);
  char* prepend(const char* name, const char* s1);
  char* swapIn(const char* name, const char* s1, const char* s2);

  StEStructSupport(){};   

public:

  StEStructSupport(TFile* tf, int bgmode, float* npairs=0, float nbar=1.);
  virtual ~StEStructSupport();
  void setTFile(TFile* tf);
  void setNBar(float nbar);
  void setBGMode( int mode);
  void setApplyDEtaFix();
  void unsetApplyDEtaFix(); // default
  bool applyDEtaFix();

  TH1** getHists(const char* name);
  float* getNorms(TH1** histArray);
  TH1** getLocalClones(const char* name);
  TH1** getPtHists(const char* name);
  TH1** getPtClones(const char* name);

  // ++, +-, -+, --
  TH1** buildCommonRatios(const char* name);
  TH1** buildCommonCFunctions(const char* name);
  TH1** buildCommonRFunctions(const char* name);
  TH1** buildCommon(const char* name, int opt=0);

  TH1** buildNCommon(const char* name);
  TH1** buildPtCommon(const char* name, int opt=0, int subtract=0);

  // LS, US, CD, CI
  TH1** buildChargeTypeRatios(const char* name);
  TH1** buildChargeTypeCFunctions(const char* name);
  TH1** buildChargeTypeRFunctions(const char* name);
  TH1** buildChargeTypes(const char* name, int opt, float* sf=0);

  TH1** buildNChargeTypes(const char* name);
  TH1** buildPtChargeTypes(const char* name, int opt=0, int subtract=0);

  
  void scaleBackGround(TH1* sib, TH1* mix, float sf=0);
  TH1* getSqrt(TH1* h);
  void fixDEta(TH2** h, int numHists); // correct triangle in hists with DEta

  // helper for writing ascii dump of set a of histograms to file=fname
  void writeAscii(TH1** h, int numHists, const char* fname, int optErrors);

  ClassDef(StEStructSupport,1)
};

inline void StEStructSupport::setTFile(TFile* tf){ mtf=tf; };
inline void StEStructSupport::setNBar(float nbar){ mNbar=nbar;};
inline void StEStructSupport::setBGMode(int mode){ mbgMode=mode; };
inline void StEStructSupport::setApplyDEtaFix()  { mapplyDEtaFix=true; };
inline void StEStructSupport::unsetApplyDEtaFix(){ mapplyDEtaFix=false; };
inline bool StEStructSupport::applyDEtaFix()     { return mapplyDEtaFix; };


#endif

/***********************************************************************
 *
 * $Log: StEStructSupport.h,v $
 * Revision 1.7  2006/10/02 22:26:53  prindle
 * Hadd now symmetrizes histograms while adding them, so output is usable
 * in Support as before. Need to load library for Correlation so we know
 * how many bins there are.
 * Added  alternative versions of methods to calculate Delta\sigma^2.
 * Important for pt correlations where we need proper normalization before
 * subtracting mixed reference.
 *
 * Revision 1.6  2006/04/06 01:09:50  prindle
 *   Calculating pt for each cut bin caused changes in HAdd.
 * The splitting of +- into +- and -+ caused changes in Support.
 *
 * Revision 1.5  2006/04/04 22:14:10  porter
 * fixdeta is now NOT default but included in StEStruct2ptCorrelations
 *
 * Revision 1.4  2005/09/07 20:26:18  prindle
 *
 *
 *     Support: Fixed some meory leaks.
 *
 * Revision 1.3  2005/03/08 21:56:42  porter
 * fixed bug in StEStructHAdd.cxx and added diagnostic option in ptcorrelations to
 * view individual terms separately
 *
 * Revision 1.2  2005/03/03 01:33:05  porter
 * Added pt-correlations method to support and included
 * these histograms to the HAdd routine
 *
 * Revision 1.1  2004/07/01 00:37:17  porter
 * new code previously my StEStructHelper. Takes hists from correltation
 * pass and builds final ressults.  Also the StEStructHAdd.h is a simple
 * replacemnt for my sumyt.C macro which could be expanded later as needed.
 *
 *
 *
 *********************************************************************/



