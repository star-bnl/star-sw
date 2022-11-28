/*
David Kapukchyan
April 21, 2021
First instance of pulse db class to contain all the basic variables for the pulse shape from DEP boards as well as the functional forms
 */

/*!
  The purpose of this class is to define the constants related to the pulse shape of the FCS data. This pulse shape will be used in both fitting the data and in simulating pulses.
  
  The pulse shape function is gaus+xexp+xexp, where gaus is a Gaussian function used to describe the main signal and the sum of xexp functions is used to describe the tail of the signal.\n 
  xexp=A/(tau^2) * (x-x0)^n * exp(-(x-x0)/tau)

  To use pulse shapes without a tail `setTail(0)`\n 
  To use pulse shapes for Run 22 data `setTail(2)`
*/

#ifndef STFCSDBPULSE_H
#define STFCSDBPULSE_H

#include <iostream>

#include "TDataSet.h"
#include "TMath.h"
#include "TGraphAsymmErrors.h"
#include "TF1.h"
#include "St_base/StMessMgr.h"

class StFcsDbPulse : public TDataSet {

 public:
  StFcsDbPulse(const char* name = "fcsPulse"); //!< Constructor
  virtual ~StFcsDbPulse();                     //!< Destructor
  int Init();                                  //!< Initialize object

  //Basic contants
  static double sqrtpi(){return sqrt(TMath::Pi());}        //!< sqrt(TMath::Pi)
  static double sqrt2pi(){return sqrt(2.0*TMath::Pi());}   //!< sqrt(2*TMath::Pi)
  
  //Constants for simulating pulses
  void setTBPerRC(double v){mTBPerRC = v;}         //!< @param v set #mTBPerRC
  double TBPerRC()const{return mTBPerRC;}               //!< @return #mTBPerRC
  double nsecPerTB()const{return 107.0/TBPerRC();}      //!< nanoseconds per timebin
  double BeamLengthSig()const{return 10.0/nsecPerTB();} //!< beam length sigma

  /**@brief Sets the variables needed by the sum of xexp functions that describe the tail of the pulse shape
     
     It will set the constants related to the tail of the pulse shape to known values based on previous data analysis.
     @param tail set values for pulse shape tail based on predefined values\n 
       0 = no tail\n 
       1 = tail shape based on LED data from Lab\n 
       2 = tail shape based on  Run 22 LED data
  */
  void setTail(int tail);

  void setGSigma(double v){mGSigma = v;} //!< @param v set #mGSigma
  void setA1(double v){mA1 = v;}         //!< @param v set #mA1
  void setA2(double v){mA2 = v;}         //!< @param v set #mA2
  void setXoff1(double v){mXoff1 = v;}   //!< @param v set #mXoff1
  void setXoff2(double v){mXoff2 = v;}   //!< @param v set #mXoff2
  void setTau1(double v){mTau1 = v;}     //!< @param v set #mTau1
  void setTau2(double v){mTau2 = v;}     //!< @param v set #mTau2
  void setP1(double v){mP1 = v;}         //!< @param v set #mP1
  void setP2(double v){mP2 = v;}         //!< @param v set #mP2

  //Variables related to the tail function
  double GSigma()const{return mGSigma;}  //!< @return #mGSigma
  double A1()const{return mA1;}          //!< @return #mA1
  double A2()const{return mA2;}          //!< @return #mA2
  double Xoff1()const{return mXoff1;}    //!< @return #mXoff1
  double Xoff2()const{return mXoff2;}    //!< @return #mXoff2
  double Tau1()const{return mTau1;}      //!< @return #mTau1
  double Tau2()const{return mTau2;}      //!< @return #mTau2
  double P1()const{return mP1;}          //!< @return #mP1
  double P2()const{return mP2;}          //!< @return #mP2

  /**@brief Figure out and set the errors on FCS pulse data stored in a TGraphAsymmErrors object
     
     Assign the error on each point of the TGraphAsymmErrors object that either holds the pulse data or is being filled with the pulse data. The error on X is assumed to be zero. ADC saturation is fixed at 4K because it is 12 bit.
     @param gae graph object that holds the pulse data
     @param i point on the graph object to modify
     @param adc ADC value to be checked for error assignment
     @param Yerr Y error on ADC for values below ADC saturation
     @param YerrSat Y error on ADC for values above ADC saturation
  */
  static void setTGraphAsymmErrors(TGraphAsymmErrors* gae, const int &i, const double &adc, double Yerr, double YerrSat);

  /**@brief Single pulse shape gaus+xexp+xexp
     
     1-dimensional function to describe the pulse shape which is a Gaussian for the main part plus a tail\n 
     gaus = Gaussian\n
     xexp = A/tau/tau*(x-x0)^n*exp(-(x-x0)/tau))\n 
     parameters of xexp are fixed constants in this class and describe the tail of the pulse shape
     @param x one dimension array as x-value input of function
     @param p array of parameters for Gaussian
  */
  double pulseShape(double* x, double* p);
  
  /**@brief Multi-pulse shape function constant+gaus+xexp+xexp for many pulses

     Extends #pulseShape() to be able to generate many pulses. The values associated with xexp are fixed for all pulses with the constants defined in this class
    @param x one dimension array as x-value input of function
    @param p parameters to describe number of pulses and the Gaussian for those pulses\n 
      p[0] = number of pulses\n 
      p[1] = constant offset\n 
      p[2] = height of Gaussian pulse 1\n 
      p[3] = mean of Gaussian pulse 1\n 
      p[4] = sigma of Gaussian pulse 1\n 
      p[5] = height of Gaussian pulse 2\n 
      ...\n 
  */
  double multiPulseShape(double* x, double* p);
  
  /**@brief Function to create pulse shape for FCS, 5 parameters is minimum

     Creates a ROOT TF1 for #multipulseShape()
     @param xlow lowest x-value for function
     @param xhigh highest x-value for function
     @param npars number of parameters needed by TF1 (2+3*number of pulses)
  */
  TF1* createPulse(double xlow=0, double xhigh=1,int npars=5 );

  /**@brief Function to tell you pad number when drawing multiple objects on the same pad

     This function is mostly used by #PadNum4x4() for drawing FCS Ecal and Hcal channels on the same canvas
     @param value is the row or column number of an Ecal or Hcal channel
     @param Nvals is the number of stuff per column or row to put on same pad
     @param PadNums is the total number of pads in the column or row
   */
  static int GenericPadPos(int value, int Nvals, int PadNums );
  
  /**@brief Function that gives pad number when drawing a specific detector id

     This function is used to know where to draw a specific Ecal or Hcal channels on utilizing a canvas with 4 rows and 4 columns
     @param det Ecal or Hcal detector ID
     @param col column number of Ecal or Hcal channel to draw
     @param row row number of Ecal or Hcal channel to draw
   */
  static int PadNum4x4(int det, int col, int row);

  /**@brief Finds minimum and maximum y-values in a TGraph and returns index for max y

     This function is used to scan a TGraph and find the global minimum and maximum y-values inside a specified x-range.
     @param gae TGraph to scan
     @param Ymin lowest possible y-value to check and then store result of found minimum y-value
     @param Ymax highest possible y-value to check and then store result of found maximum y-value
     @param xmin minimum x-value to start searching for y-min/max
     @param xmax minimum x-value to start seraching for y-min/max
  */
  static Int_t getYMinMax(TGraphAsymmErrors* gae, Double_t &Ymin, Double_t &Ymax, Double_t xmin=-5, Double_t xmax=2000);

  virtual void Print(Option_t* opt = "") const; //!< Print all the constants associated with this class

 protected:
  double mTBPerRC;  //!< number of timebins in one RHIC crossing
  double mGSigma;   //!< pulse shape nominal sigma of Gaussian part
  //Pulse shape constants
  double mA1;       //!< pulse shape tail: height of first xexp function
  double mA2;       //!< pulse shape tail: height of second xexp function
  double mXoff1;    //!< pulse shape tail: x offset of first xexp function
  double mXoff2;    //!< pulse shape tail: x offset of second xexp function
  double mTau1;     //!< pulse shape tail: scale of first xexp function
  double mTau2;     //!< pulse shape tail: scale of second xexp function
  double mP1;       //!< pulse shape tail: power of first xexp function
  double mP2;       //!< pulse shape tail: power of second xexp function

 private:
  int mTail;  //!< pulse shape tail, turn off or on to known values
  static const int mAdcSaturation = 4000; //!< ADC is 12 bit number so it saturates at 4K

  ClassDef(StFcsDbPulse, 1);
};

#endif
