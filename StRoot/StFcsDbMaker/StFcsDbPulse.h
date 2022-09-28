/*
David Kapukchyan
April 21, 2021
First instance of pulse db class to contain all the basic variables for the pulse shape from DEP boards as well as the functional forms
 */

#ifndef STROOT_STFCSDBPULSE_H_
#define STROOT_STFCSDBPULSE_H_

#include <iostream>

#include "TDataSet.h"
#include "TMath.h"
#include "TGraphAsymmErrors.h"
#include "TF1.h"

class StFcsDbPulse : public TDataSet {

 public:
  StFcsDbPulse(const char* name = "fcsPulse");
  virtual ~StFcsDbPulse();
  int Init();

  //Basic contants
  static double sqrtpi(){return sqrt(TMath::Pi());}
  static double sqrt2pi(){return sqrt(2.0*TMath::Pi());}
  void setTBPerRC(double v){mTBPerRC = v;}
  double TBPerRC(){return mTBPerRC;} //Timebins per RHIC crossing
  double nsecPerTB(){return 107.0/TBPerRC();}
  double BeamLengthSig(){return 10.0/nsecPerTB();}

  //Pulse shape constants
  void setTail(int tail);//This will define the below variables based on some known values values
  void setGSigma(double v){mGSigma = v;}
  void setA1(double v){mA1 = v;}
  void setA2(double v){mA2 = v;}
  void setXoff1(double v){mXoff1 = v;}
  void setXoff2(double v){mXoff2 = v;}
  void setTau1(double v){mTau1 = v;}
  void setTau2(double v){mTau2 = v;}
  void setP1(double v){mP1 = v;}
  void setP2(double v){mP2 = v;}

  double GSigma(){return mGSigma;}
  double A1(){return mA1;}
  double A2(){return mA2;}
  double Xoff1(){return mXoff1;}
  double Xoff2(){return mXoff2;}
  double Tau1(){return mTau1;}
  double Tau2(){return mTau2;}
  double P1(){return mP1;}
  double P2(){return mP2;}

  static void setTGraphAsymmErrors(TGraphAsymmErrors* gae, const int &i, const double &adc, double Yerr, double YerrSat);

  double pulseShape(double* x, double* p);
  double multiPulseShape(double* x, double* p);
  TF1* createPulse(double xlow=0, double xhigh=1,int npars=5 );//5 parameters is minimum. User is responsible for deleting this function.??

  static int GenericPadPos(int value, int Nvals, int PadNums );
  static int PadNum4x4(int det, int col, int row);

  static Int_t getYMinMax(TGraphAsymmErrors* gae, Double_t &Ymin, Double_t &Ymax, Double_t xmin=-5, Double_t xmax=2000);//Returns index for max y

  void Print(Option_t* opt = "");

 protected:
  double mTBPerRC;
  //Pulse shape
  double mGSigma;
  double mA1;
  double mA2;
  double mXoff1;
  double mXoff2;
  double mTau1;
  double mTau2;
  double mP1;
  double mP2;

 private:
  int mTail;
  static const int mAdcSaturation = 4000;

  ClassDef(StFcsDbPulse, 1);
};

#endif
