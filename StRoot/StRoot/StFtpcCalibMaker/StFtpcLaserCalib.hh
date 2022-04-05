// $Id: StFtpcLaserCalib.hh,v 1.5 2009/10/14 15:59:55 jcs Exp $
//
// $Log: StFtpcLaserCalib.hh,v $
// Revision 1.5  2009/10/14 15:59:55  jcs
// changes to be able to vary the gas temperature in addition to varying t0 and
// gas composition
//
// Revision 1.4  2008/05/15 21:12:49  jcs
// replace StMagUtilities.h with StarMagField.h - necessary for HELIX_FIT
//
// Revision 1.3  2006/04/04 10:57:05  jcs
// Fix memory leak
//
// Revision 1.2  2006/03/15 15:13:57  jcs
// add lines for listing CVS update info
//

#ifndef STAR_StFtpcLaserCalib
#define STAR_StFtpcLaserCalib

#include "TMinuit.h"

#include "StFtpcLaser.hh"
#include "StFtpcLaserTrafo.hh"
#include "StarMagField/StarMagField.h"

const Float_t radcutmin[11]={0,8.5,10.5,12.5,14.5,16.5,18.5,21,22.5,25,26.5};
const Float_t radcutmax[11]={0,10,11.5,14,15.5,18,20,22,24,26,28};

const Float_t radcutminst[4]={0,9,18,27};
const Float_t radcutmaxst[4]={0,13.5,21,30};

const Float_t zfieldcage=263.0;
const Float_t Rad2Grad=180/TMath::Pi();

// define functions etc. used for fit
//static Double_t funcxz(float x,float z,Double_t *par);
//static Double_t funcyz(float y,float z,Double_t *par);
//static void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);

//TMinuit *mMinuit;

// im prinzip unschoen !!! Warning : defined but not used !!!
//static Double_t arglist[10];
//static Int_t ierflg, nhits;
//static Float_t z[11],x[11],y[11];
//static Float_t ex[11],ey[11];
//static Float_t x_s[11],y_s[11];


class StFtpcLaserCalib : public StFtpcLaser
{

 private:
   
  TMinuit *mMinuit;

  //Float_t radius[11],phi[11],z[11],x[11],y[11],ex[11],ey[11];
  Float_t phi[11];//,ex[11],ey[11];
  Float_t calcrad[11],calcphi[11],calcx[11],calcy[11];
  Float_t ppos[11],ppossigma[11],rpol[11],timepos[11];
  Float_t resx[11],resy[11],resrad[11],resphi[11];
  Float_t resx2[11],resy2[11],resrad2[11],resphi2[11];
  Float_t maxadc[11],padl[11],timel[11],charge[11];
  Float_t x_s[11],y_s[11];
  Float_t x_d[11],y_d[11];
  Int_t  hsec[11], softsec[11], softrow[11];

  Float_t MINZ,MAXZ,MINRAD,MAXRAD,GAUSFIT;
  //Int_t FTPC,LSEC,STRAIGHT,usedfit;
  float deltat0, deltagas,deltaTemp;

  TString filename;

  StFtpcLaserTrafo *mtrafo;
//  StMagUtilities *magf;
  StarMagField *magf;

  //TMinuit *gMinuit;
  Double_t arglist[10];
  Int_t ierflg;

  //void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);

  //TH1F *hresx,*hresy,*hresrad,*hresphi;
  //TH1F *hphi, *hrad,*hpad, *hpadsigma;
  //TH2F *hresrad2,*hresphi2,*hpadrad;

  //TH1F *hpadcut[11], *hradcut[11];

 protected:
   
 public: 

  TFile *outfile;
  //ofstream padfile,padfile2;
 
  Int_t FTPC,LSEC,STRAIGHT,usedfit;

  Int_t trcharge;
  Float_t p,pt,invp,invpt;

  TH1F *hresx,*hresy,*hresrad,*hresphi;
  TH1F *hhresx,*hhresy,*hhresrad,*hhresphi;
  TH1F *hphi, *hrad,*hpad, *hpadsigma, *htime;
  TH2F *hresrad2,*hresphi2,*hpadrad;
  TH2F *hhresrad2,*hhresphi2;
  TH2F *hhresx2,*hhresy2;
  TH2F *hresx2,*hresy2;
  TH1F *hradpol,*hcalcrad,*hcalcphi;
  TH2F *hradz, *hphiz; 
  TH1F *htimel,*hpadl,*hmaxadc,*hcharge;
  TH1F *hnhits;

  TH1F *hbdiffx,*hbdiffy;
  TH2F *hbdiffx2,*hbdiffy2;

  TH1F *htrcharge, *hp, *hpt, *hinvp, *hinvpt;

  TH1F *hpadcut[11], *hradcut[11], *hradpolcut[5];

  Float_t radius[11];

  StFtpcLaserCalib();
  StFtpcLaserCalib(int ftpc, int lsec, int straight, int gfit, int minz, int maxz, int minrad, int maxrad, float gt0, float ggas, float gTemp, StFtpcLaserTrafo *trafo,StarMagField *gmagf);
  virtual ~StFtpcLaserCalib();

  //Double_t funcxz(float x,float z,Double_t *par);
  //Double_t funcyz(float y,float z,Double_t *par);
  //void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag);

  void MakeOutput(TString eingabe,char* t0, char* gas, float gastemp);
  void MakePs();
  bool cut(int i);
  void Fill(int l);
  void minuit_init();
  void minuit_set_par();
  void minuit_print();
  void calc_res();
  int laser_fit(int getnhits);
  void defl_angle_transv();
  void defl_histograms_st();
  void defl_histograms();
  void extrapol_histograms();
  void fill_defl_histograms(float getrad, float getpadpos);
  void fill_defl_histograms_st(float getrad, float getpadpos);
  void fill_extrapol_histograms(float getradpol);
  void analyse_defl();
  void fillarray(float tx,float ty,float tz,float tex,float tey,int n,int nsec,float gppos,float gppossigma, int gsofstsec, int gsoftrow,float gtimepos, float getpadl, float gettimel, float getmaxadc, float getcharge);
  void PositionLog();

};

#endif
