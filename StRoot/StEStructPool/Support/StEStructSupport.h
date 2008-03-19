/**********************************************************************
 *
 * $Id: StEStructSupport.h,v 1.12 2008/03/19 22:08:39 prindle Exp $
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

public:

  TFile* mtf;
  float* mnpairs; //! for normalization comparing different cuts 
  int   mbgMode;
  int   mNumZBins;
  char* mtmpString;
  bool  mapplyDEtaFix;
  bool  msilent;
  bool  mDoSymmetrize;
  bool  mPairNormalization;
  bool  mIdenticalPair;

  bool  goodName(const char* name); // test if name is one of ours
  bool  goodName_zBuf(const char* name, int zBin); // test if name is one of ours with zBuffer.
  char* getFrontName(int itype); 
  const char* getTypeName(int itype);
  const char* getChargeSignName(int ics);
  char* prepend(const char* name, const char* s1);
  char* swapIn(const char* name, const char* s1, const char* s2);
  void rescale(TH1** hists, int zBin);
  void rescalePt(TH1** hists, int zBin);
  void setSymmetrizeUS(bool symm);
  void symmetrizeUS(const char *name, TH1** histos);
  void symmetrizePtUS(const char *name, TH1** histos);

  StEStructSupport() {};   

  StEStructSupport(TFile* tf, int bgmode, float* npairs=0);
  virtual ~StEStructSupport();
  void setTFile(TFile* tf);
  void setBGMode( int mode);
  void setApplyDEtaFix();
  void unsetApplyDEtaFix(); // default
  bool applyDEtaFix();
  void setSilent();
  void unsetSilent();
  bool silent();

  int getNZBins();
  float *getCommonNumber(int zBin);
  float *getCommonPairs(int zBin);
  float *getChargeNumber(int zBin);
  float *getChargePairs(int zBin);

  double *getd2NdEtadPhi(int zBin);
  double *getptHat(int zBin);
  TH1** getHists(const char* name, int zBin);
  TH1** getLocalClones(const char* name, int zBin);
  TH1** getPtHists(const char* name, int zBin);
  TH1** getPtClones(const char* name, int zBin);
  float* getNorms(TH1** histArray);
  double getRatio(int iCombo, int zBin);

  // ++, +-, -+, --
  TH1** buildCommonRatios(const char* name);
  TH1** buildCommonRatios(const char* name, int zBin);
  TH1** buildCommonCFunctions(const char* name);
  TH1** buildCommonCFunctions(const char* name, int zBin);
  TH1** buildCommonRFunctions(const char* name);
  TH1** buildCommonRFunctions(const char* name, int zBin);
  TH1** buildCommon(const char* name, int opt=0);
  TH1** buildCommon(const char* name, int opt, int zBin);
  TH1** buildPtCommon(const char* name, int opt=0, int subtract=0);
  TH1** buildPtCommon(const char* name, int opt, int subtract, int zBin);

  // LS, US, CD, CI
  TH1** buildChargeTypeRatios(const char* name);
  TH1** buildChargeTypeRatios(const char* name, int zBin);
  TH1** buildChargeTypeCFunctions(const char* name);
  TH1** buildChargeTypeCFunctions(const char* name, int zBin);
  TH1** buildChargeTypeRFunctions(const char* name);
  TH1** buildChargeTypeRFunctions(const char* name, int zBin);
  TH1** buildChargeTypes(const char* name, int opt, float* sf=0);
  TH1** buildChargeTypes(const char* name, int opt, float* sf, int zBin);
  TH1** buildPtChargeTypes(const char* name, int opt=0, int subtract=0);
  TH1** buildPtChargeTypes(const char* name, int opt, int subtract, int zBin);

  TH1** buildChargeTypesSumOfRatios(const char* name, int opt, float* sf=0);
  TH1** buildChargeTypesSumOfRatios(const char* name, int opt, float* sf, int zBin);
  
  void scaleBackGround(TH1* sib, TH1* mix, float sf=0);
  TH1* getSqrt(TH1* h);
  void fixDEta(TH2** h, int numHists); // correct triangle in hists with DEta

  // helper for writing ascii dump of set a of histograms to file=fname
  void writeAscii(TH1** h, int numHists, const char* fname, int optErrors);

  ClassDef(StEStructSupport,1)
};

inline void StEStructSupport::setTFile(TFile* tf){
    mtf=tf;
    getNZBins();
};
inline void StEStructSupport::setBGMode(int mode){ mbgMode=mode; };
inline void StEStructSupport::setApplyDEtaFix()  { mapplyDEtaFix=true; };
inline void StEStructSupport::unsetApplyDEtaFix(){ mapplyDEtaFix=false; };
inline bool StEStructSupport::applyDEtaFix()     { return mapplyDEtaFix; };
inline void StEStructSupport::setSilent() { msilent=true; };
inline void StEStructSupport::unsetSilent(){ msilent=false; };
inline bool StEStructSupport::silent() { return msilent; };

#endif

/***********************************************************************
 *
 * $Log: StEStructSupport.h,v $
 * Revision 1.12  2008/03/19 22:08:39  prindle
 * Use GetObject instead of Get for type safety. Stop deleting objects we didn't create.
 * Treat \Delta\rho = d^2n/dEtadphi (rho - rho_ref)/rho_ref as basic unit when combining
 * centralities and z bins.
 *
 * This code should be check in more detail before being completely relied upon.
 *
 * Revision 1.11  2007/11/26 20:07:20  prindle
 * Modified to average \Delta\rho/sqrt(\rho) over z-bins (if more than one z-bin
 * present for given centrality. Note: I weight by number of tracks, not number of
 * pairs. This is important when we are also combining different centralities (which
 * I do by combining centrality tag with z-bin tag in macro/addCentralities.)
 *
 * Scale mixed histograms by number of events. Integral of \Delta\rho need not be 0.
 *
 * delete items that are created and valgrind complained were lost. (Not a big deal
 * since macro is run once)
 *
 * [Still need to commit StEStructHAdd.cxx which cvs complained that check-update failed.]
 *
 * Revision 1.10  2007/05/27 22:46:02  msd
 * Added buildChargeTypes mode 3 which takes rho_ref from track counts.
 * Added buildChargeTypeSumOfRatios.
 *
 * Revision 1.9  2007/01/26 17:20:59  msd
 * Updated HAdd for new binning scheme.
 * Improved Support::buildChargeTypes.
 *
 * Revision 1.8  2006/12/14 20:07:15  prindle
 *   I was calculating \Delta\rho/sqrt(rho) for ++, +-, -+ and --
 * and then combining those into LS, US, CD and CI. The was wrong
 * and now I am doing it correctly. For CI this makes only a slight
 * change, it seems the amplitude is decreased a little. For CD
 * this is a bigger change. I left the old versions (with _Old appended)
 * for now.
 *
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



