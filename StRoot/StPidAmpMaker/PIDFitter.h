///////////////////////////////////////////////////////////////////////////////
//
// $Id: PIDFitter.h,v 1.2 2003/04/30 20:37:56 perev Exp $
//
// Authors: Aihong Tang
//
///////////////////////////////////////////////////////////////////////////////
//
// Description: fit histo. produced by StPidAmpMaker.
//
///////////////////////////////////////////////////////////////////////////////


#ifndef PIDFitter_h
#define PIDFitter_h

#include <TROOT.h>
#include "TH1.h"
#include "StPidProbabilityConst.hh"

class TF1;

class PIDFitter  {
 public:

  PIDFitter();
  virtual ~PIDFitter(){};

      void  FitMultiGaus(Char_t* fileNameOfInput, Char_t* fileNameOfOutput);
      void  DoPhaseSpaceCalibration(Char_t* fileName4Calibration,Char_t* phaseSpaceCalibFileName);
      void  GetSigmaOfSingleTrail(Char_t* fileName4Calibration,Char_t* sigmaFileName);
      void  ExtrapAmp(Char_t* fileNameOfInput, Char_t* fileNameOfOutput);

      void  Process( Char_t* sigmaOfSigmTrialInputName,
                     Char_t* sigmaOfSigmTrialOutputName, 
                     Char_t* phaseSpaceCalibInputName,
                     Char_t* phaseSpaceCalibOutputName, 
                     Char_t* gausFitInputName,
                     Char_t* gausFitOutputName,
                     Char_t* ampFitOutputName );

            virtual  void  Init();


 protected:

    Bool_t mWriteSigmaNSampleGraph;
    Bool_t mWriteGaus4SigmaNSampleHist;

    double * mSigmaOfSingleTrail; //!

    TF1* electronBandCenter;//!
    TF1* pionBandCenter;//!
    TF1* kaonBandCenter;//!
    TF1* antiprotonBandCenter; //!

    TF1* pionKaonBandCenter;//! //for drawing line between pion and kaon bands
    TF1* kaonAntiprotonBandCenter; //! //for drawing line between kaon and antiproton bands

    double    **BBOffSetPar; //!   
    double    **BBScalePar;  //!

 private:

    double mEGausHeight;    
    double mPiGausHeight;   
    double mKGausHeight;    
    double mPGausHeight;     

    double mEGausCenter;    
    double mPiGausCenter;   
    double mKGausCenter;    
    double mPGausCenter;     

    double mESigma;
    double mPiSigma;
    double mKSigma;
    double mPSigma;


    double delta(double calib, double pionPosition, double protonPosition);
    double look4MinDeltaDiff(double calibStart, double calibEnd, int calibSteps, double pionPosition, double protonPosition, double DeltaRef);
    double minimumIonizingdEdx(double calib, double pionPosition);

    float FitResoGaus(TH1F* resoHist,float fitRange,float& er,float theStart, float theEnd, int ParIndex, int j, int k, float thePPosition);
    void  PresetHeightAndSigma(double center, double& height, double& sigma,  TH1F* resoHist, double tempSigmaOfSingleTrial, int k);
    void  RefreshPresettings(TH1F* resoHist, double tempSigmaOfSingleTrial, int k, float thePPosition);

   ClassDef (PIDFitter,1)
     };


#endif

///////////////////////////////////////////////////////////////////////////////
//
// $Log: PIDFitter.h,v $
// Revision 1.2  2003/04/30 20:37:56  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.1  2002/02/14 21:25:55  aihong
// re-install the new version
//

//
/////////////////////////////////////////////////////////////////////////////////
