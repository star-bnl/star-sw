#ifndef __StarLight_h__
#define __StarLight_h__

#include "StarGenerator/BASE/StarGenerator.h"

#include "starlight.h"
#include "inputParameters.h"

class StarGenAAEvent;

//class inputParameters;

/**
   \class StarLight
   \brief Interface to the StarLight (c++ version) event generator
   
 */

class StarLight : public StarGenerator
{

 public:
  StarLight( const Char_t *name="STARlight" );
  ~StarLight(){ /* nada */ };

  Int_t Init();
  Int_t Generate();

  //void Set( const Char_t *s ){ mSTARlight -> readString(s); }

  //void const char *GetCVS() const
  //{static const char cvs[]="Tag $Name:  $ $Id: StarLight.h,v 1.4 2014/08/06 11:43:58 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

 private:
 protected:
  map<TString,Double_t> ParametersDouble;
  map<TString,Int_t>    ParametersInt;

  inputParameters *_parameters;

  //STARlight::STARlight *mSTARlight;
  starlight *mSTARlight;

  void FillAA( StarGenEvent *event );
  void FillPP( StarGenEvent *event );

public:
  void SetEtaCut( Double_t low, Double_t high );
  void SetPtCut( Double_t low, Double_t high );
  void SetRapidityValues( Double_t high, Int_t bins );
  void SetWValues( Double_t low, Double_t high, Int_t bin );
  void SetProductionMode( Int_t mode );
  void SetProductionPID( Int_t pid );
  void SetBreakupMode( Int_t mode );
  void SetInterference( Double_t percent );
  void SetIncoherence( Double_t percent );
  void SetBFORD( Double_t value );
  void SetInterferencePtValues( Double_t high, Int_t bins );
  void ProcessParameters();

  ClassDef(StarLight,1);

};

#endif
