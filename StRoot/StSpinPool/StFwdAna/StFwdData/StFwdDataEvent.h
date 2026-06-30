/*
  AUTHOR
  David Kapukchyan

  PURPOSE
  Classes related to holding the event data for the forward analysis framework (StFwdAna)

  DESCRIPTION
  Contains the class #FwdDataEvent that is used to store information related to the event and the trigger detectors. This includes, time, run number, fill number, spin information, and vertex information. Note that it does not use the prefix "St" since the idea is that these are decoupled from the STAR data scheme and can be loaded without any of the other STAR libraries

  LOG
  @[June 30, 2026] > Copied #FcsEventInfo from StMuFcsPi0Data.h into this new file and changed name from #FcsEventInfo to #StFwdEvent for new naming scheme that generalizes to any forward software analysis.
*/

#ifndef STFWDDATA_STFWDDATAEVENT_HH
#define STFWDDATA_STFWDDATAEVENT_HH

//C/C++ Headers
#include <iostream>

//ROOT Headers
#include "TObject.h"

class StFwdDataEvent : public TObject
{
public:
  StFwdDataEvent();
  ~StFwdDataEvent();

  Int_t mRunTime = -1;       ///< Time of the run (actually time of event)
  Int_t mRunNum = -1;        ///< Run number for event
  UInt_t mFill = 0;          ///< Fill number for event
  UInt_t mEvent = -1;        ///< STAR Event Id
  Int_t mBx48Id = -1;        ///< 48 bit bunch Id for event
  Int_t mBx7Id = -1;         ///< 7 bit bunch Id for event
  UShort_t mSpin = 0;        ///< [Spin bit, this is source polarization](https://drupal.star.bnl.gov/STAR/blog/oleg/spin-patterns-and-polarization-direction)
  //Short_t spinFrom4BitSpin(); ///< Correctly accounts for the spin flip when working with STAR data
  static Short_t BlueSpin(Int_t spinbit);        ///< Blue beam polarization at STAR +1 for B+ and -1 for B-
  static Short_t YellowSpin(Int_t spinbit);      ///< Yelllow beam polarization at STAR +1 for Y+ and -1 for Y-

  Int_t mTofMultiplicity = -1; ///< TOF Multiplicity

  Int_t mPrimVertRanking = -1;  ///< Ranking of primary vertex
  Double_t mPrimVx = -999;     ///< x position of found primary vertex
  Double_t mPrimVy = -999;     ///< y position of found primary vertex
  Double_t mPrimVz = -999;     ///< z position of found primary vertex
  Double_t mVpdVz = -999;      ///< VPD z Vertex
  Double_t mBbcVz = -999;      ///< BBC z Vertex
  Double_t mBbcTacDiff = 0; ///< BBC TAC difference
  Double_t mEpdTacEarlyW = 0;  ///< Earliest EPD TAC for West with cuts 1<adcnmip<15 && TAC>50
  Double_t mEpdTacEarlyE = 0;  ///< Earliest EPD TAC for East with cuts 1<adcnmip<15 && TAC>50
  Double_t mEpdAvgW = 0;    ///< Average EPD TAC for West with cuts 1<adcnmip<15 && TAC>50
  Double_t mEpdAvgE = 0;    ///< Average EPD TAC for East with cuts 1<adcnmip<15 && TAC>50
  //Double_t EpdTacDiffEarly();
  //Double_t EpdTacDiffAvg();
  Double_t mEpdVz = -999;      ///< EPD z Vertex
  Double_t mZdcVz = -999;      ///< ZDC z Vertex
  Short_t mFoundVertex = 0;    ///< Bit vector encoding for which vertex was best; 0 means no vertex, 1=Primary Vertex found,2=Vpd,3=Epd,4=Bbc

  //This will be used to indicate how many clusters are in the #TClonesArray of #FcsPhotonCandidate. Everything from this number to the size of the array will be points for a given detector Id. I did it this way so I don't have to create a separate branch holding these two numbers and there should only be one #StFwdDataEvent object. Also, didn't want a seperate class for clusters and points since they will store the same information. It is kind of a hack since I know that I am only looping up to detector id 2.
  UShort_t mClusterSize = 0;       ///< Size of clusters in #mPhArr in #StMuFcsPi0TreeMaker. This means 0 to <#mClusterSize is cluster photon candidates

  virtual void Clear(Option_t* opt="");          ///< Resets all variables to default
  virtual void Print(Option_t* opt="") const;    ///< Prints all values no options

  ClassDef( StFwdDataEvent, 1 )
};

