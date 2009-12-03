// \class  SmdGains
// \author Jan Balewski

#ifndef SmdGains_h
#define SmdGains_h
/*******************************************************
 * $Id: SmdGains.h,v 1.7 2009/12/03 22:35:03 ogrebeny Exp $
 *******************************************************
 This code should run only on histogram files,
 not on events, JB
 *
 *******************************************************/

#include "TObject.h"
#include "TFile.h"
class TObjArray  ;
class TH1F;
class TF1;
class TCanvas;
class TGraphErrors;

class StripG {
 public:
  int id;
  float sl,esl; // slopes from expo fit to raw spectra
  float mpv1,empv1; // Landau MPV from single strip energy deposit
  float gc,egc; // final gain corrections
  int sum1; // counts in raw spectrum for ADC range
  int flag;// type of result
  
  StripG();
  void clear();
  void print();
};

//==========================
//==========================

class SmdGains :public TObject{
  TCanvas *c1, *c2;
 private:
  enum {mxS=288,mxH=8};
  TFile *fdIn;
  TObjArray  *HList; // output histo
  TF1 *gnCorFn;

  int sectID;
  char planeUV;
  TString plCore;

  //........ cuts
  int adcMin,adcMax,minSum;
  float maxRelEr; // threshold to flag too large errors
  float minMipEne, maxMipEne; // (MeV) lower/upper thres for Landau Ene fit 
  float idealMipEne;

  //......histos
  TH1F *hA[mxH];
  TGraphErrors  * grA[mxH];

  //....  calib
  StripG str[mxS];

 public:  
  SmdGains();
  virtual ~SmdGains(){};
  void set( TObjArray * hL, int se, char uv){ HList=hL; sectID=se, planeUV=uv;}; 
  void plTGraph(const Char_t *shpFunc="pol1",int ig=1, int pl=0); // plot & fit tGraphs
  void plFGC();

  void doGainCorr(int str1, int str2, int ns=20, int pl=0);

   void avrMipNEne(int str1,int ns); // sum MIP ene (from 2 strips) of ns strips

  TFile* open(TString);
  void init();
  void doSlopesOnly(float fac=1.);
  void saveHisto(const Char_t *fname=0);
  void saveGains(FILE *fd=0);

  void fitSlopesSmd(int str1, int str2, int pl=0);
  void fitSlopesTile(int eta1, int nEta, char cT, int pl=0) ;

  ClassDef(SmdGains,1) 
};

#endif

/*****************************************************************
 * $Log: SmdGains.h,v $
 * Revision 1.7  2009/12/03 22:35:03  ogrebeny
 * Fixed compiler warnings, mostly char* -> const char*
 *
 * Revision 1.6  2007/07/12 19:27:20  fisyak
 * Add includes for TMath for ROOT 5.16
 *
 * Revision 1.5  2005/08/09 18:46:31  balewski
 * after smd calib in 2005
 *
 * Revision 1.4  2004/10/08 14:34:50  balewski
 * as used for PQRUV calib for pp200, 2004
 *
 * Revision 1.3  2004/09/22 00:45:52  balewski
 * ready for calib of smd
 *
 * Revision 1.2  2004/09/14 19:38:44  balewski
 * new version, SMD calib is now too complicated
 *
 * Revision 1.1  2004/09/11 04:57:34  balewski
 * cleanup
 *
 *
 *
 ********************************************************************/

