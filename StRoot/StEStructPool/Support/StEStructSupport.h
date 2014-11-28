/**********************************************************************
 *
 * $Id: StEStructSupport.h,v 1.19 2011/08/02 20:42:24 prindle Exp $
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
class TH2D;
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
  bool  mPairWeighting;
  bool  mIdenticalPair;
  bool  mYtYtNormalization;
  bool  mYtYtVolumeNormalization;
  
  bool  goodName(const char* name); // test if name is one of ours
  bool  goodName_zBuf(const char* name, int zBin); // test if name is one of ours with zBuffer.
  char* getFrontName(int itype); 
  const char* getTypeName(int itype);
  const char* getChargeSignName(int ics);
  char* prepend(const char* name, const char* s1);
  char* swapIn(const char* name, const char* s1, const char* s2);
  void rescale(TH2D** hists);
  void rescale(TH2D** hists, int zBin);
  void rescalePt(TH2D** hists, int zBin);
  void setSymmetrizeUS(bool symm);
  void symmetrizeUS(const char *name, TH2D** histos);
  void symmetrizePtUS(const char *name, TH2D** histos);

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

  double getCIdNdEtadPhi();
  double *getd2NdEtadPhi(int zBin, bool include2s=true);
  double *getScaleFactors();
  double *getScaleFactors(int zBin);
  double *getptHat();
  double *getptHat(int zBin);
  int    histogramExists(const char* name, int zBin);
  TH2D** getHists(const char* name, int zBin);
  TH2D** getLocalClones(const char* name, int zBin);
  TH2D** getPtHists(const char* name, int zBin);
  TH2D** getPtClones(const char* name, int zBin);
  float* getNorms(TH2D** histArray);
  double getRatio(int iCombo, int zBin);

  // ++, +-, -+, --
  TH2D** buildCommonRatios(const char* name, float* sf=0);
  TH2D** buildCommonRatios(const char* name, float* sf, int zBin);
  TH2D** buildCommonCFunctions(const char* name, float* sf=0);
  TH2D** buildCommonCFunctions(const char* name, float* sf, int zBin);
  TH2D** buildCommonRFunctions(const char* name, float* sf=0);
  TH2D** buildCommonRFunctions(const char* name, float* sf, int zBin);
  TH2D** buildCommon(const char* name, int opt=2, float* sf=0);
  TH2D** buildCommon(const char* name, int opt, float* sf, int zBin);
  TH2D** buildPtCommon(const char* name, int opt=2, int subtract=1);
  TH2D** buildPtCommon(const char* name, int opt, int subtract, int zBin);

  // LS, US, CD, CI
  TH2D** buildChargeTypeRatios(const char* name, float* sf=0);
  TH2D** buildChargeTypeRatios(const char* name, float* sf, int zBin);
  TH2D** buildChargeTypeCFunctions(const char* name, float* sf=0);
  TH2D** buildChargeTypeCFunctions(const char* name, float* sf, int zBin);
  TH2D** buildChargeTypeRFunctions(const char* name, float* sf=0);
  TH2D** buildChargeTypeRFunctions(const char* name, float* sf, int zBin);
  TH2D** buildChargeTypes(const char* name, int opt=2, float* sf=0);
  TH2D** buildChargeTypes(const char* name, int opt, float* sf, int zBin);
  TH2D** buildPtChargeTypes(const char* name, int opt=2, int subtract=1);
  TH2D** buildPtChargeTypes(const char* name, int opt, int subtract, int zBin);

  TH2D** buildChargeTypesSumOfRatios(const char* name, int opt, float* sf=0);
  TH2D** buildChargeTypesSumOfRatios(const char* name, int opt, float* sf, int zBin);
  
  void scaleBackGround(TH2D* sib, TH2D* mix, float sf=0);
  void fixDEtaGeometric(TH2** h, int numHists); // correct triangle in hists with DEta
  void fixDEta(TH2** h, int numHists); // tries to do an acceptance correction

  // helper for writing ascii dump of set a of histograms to file=fname
  void writeAscii(TH2D** h, int numHists, const char* fname, int optErrors);

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
 * Revision 1.19  2011/08/02 20:42:24  prindle
 *   Added YtYtVolumeNormalization.
 *   Fixed calculation of error for YtYt \Delta\rho/sqrt(\rho_{ref})
 *   Added error calculation for p_t histograms
 *   Added warning when either \rho_{sib} or \rho_{ref} has an empty bin. Set ratio
 *    bin to 0 instead of -1.
 *
 * Revision 1.18  2010/09/02 21:31:14  prindle
 *   Support: Can't find evidence that YtYt correlation depends on z vertex.
 *            Add numerators and denominators then take ratio. Need a new
 *            rescale method independent of z bin. Looks like we can normalize
 *            mixed so \int{sib/mix} = number of bins (what we have recently been
 *            doing) or \int{sib} = \int{mix} and the former is more snesitive
 *            to bins with very few counts. That isn't important for angular
 *            histograms but is for (yt,yt). I am calling the \int{sib} = \int(mix}
 *            Yt normalization (even though it is what we did long ago).
 *
 * Revision 1.17  2010/06/23 22:33:56  prindle
 *   In HAdd we distinguish between the parent distributions of the
 *    two particles.
 *   In Support I fixed a number of problems in the Pt correlation section.
 *
 * Revision 1.16  2010/03/02 21:48:30  prindle
 *   Fix addDensities (for checking pair cuts)
 *   Lots of small changes
 *
 * Revision 1.15  2009/07/29 21:47:47  dkettler
 * New weighting for z bins
 *
 * Revision 1.14  2009/05/08 00:21:42  prindle
 * In StEStructHadd remove support for old style of histogram names, do a better job calculating
 * errors (at least for number (\eta_\Delta,\phi_\Delta) histograms), double bins which
 * have an edge in the center (purely cosmetic when looking at intermediate histograms).
 * In StEStructSupport check for existance of histograms and return gracefully.
 * Code in buildChargeTypes and buildPtChargeTypes was essentially duplicate of code
 * in buildCommon and buildPtCommon so I refactored to reduce redundancy.
 *
 * Revision 1.13  2008/12/02 23:52:53  prindle
 * Get information about histogram XX being symmetrized from CutBin.
 * Changed TH1* to TH2D* in many places hoping to be able to plot DEtaDPhi
 * as colz (doesn't work yet).
 * Added direct calculation of \Delta\rho/\rho_{ref} (and  similar) which is
 * needed for YtYt correlations.
 *
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



