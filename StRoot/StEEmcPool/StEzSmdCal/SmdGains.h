// \class  SmdGains
// \author Jan Balewski

#ifndef SmdGains_h
#define SmdGains_h
/*******************************************************
 * $Id: SmdGains.h,v 1.3 2004/09/22 00:45:52 balewski Exp $
 *******************************************************
 This code should run only on histogram files, not on events, JB
 *
 *******************************************************/

#include "TObject.h"

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
  void plTGraph(char *shpFunc="pol1",int ig=1); // plot & fit tGraphs
  void plFGC();

  void doOneStripEne(int str1, int str2,char *shpFunc="pol0"); // average MIP peak in plane 
  void doGainCorr(int str1, int str2, int ns=20);

  float oneStripEne(int str1, int pl=0); //  MIP ene from one strip
  void avrRelNGain( int str1,int ns);
  void avrMipNEne(int str1,int ns, float &mpv, float &empv); // sum MIP ene (from 2 strips) of ns strips

  TFile* open(TString);
  void init();
  void finish(int k=0);
  void doSlopesOnly(float fac=1.);
  void saveHisto(char *fname=0);
  void saveGains(FILE *fd=0);

  void fitSlopesSmd(int str1, int str2, int pl=0);
  void fitSlopesTile(int eta1, int nEta, char cT, int pl=0) ;

  ClassDef(SmdGains,1) 
};

#endif

/*****************************************************************
 * $Log: SmdGains.h,v $
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

