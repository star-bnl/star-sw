/*
 *
 * \class StFpostQaMaker
 *
 */

#ifndef STAR_StFpoatQaMaker_HH
#define STAR_StFpostQaMaker_HH

#include "StRoot/St_base/Stypes.h"
#include "StMaker.h"

class StFmsDbMaker;
class StFmsCollection;
class TH1F;
class TH2F;

class StFpostQaMaker : public StMaker {
 public: 
  StFpostQaMaker( const Char_t* name = "FpostQA");
  virtual ~StFpostQaMaker();
  virtual Int_t Init();   //Gets run first in the chain
  virtual Int_t Make();   //Gets run second in the chain
  virtual Int_t Finish(); //Gets run last in the chain
  void setRun(int v) {mRun = v;} //Assigns the run number to 'mRun' called in 'runqa.C'
  void setPed(int v) {mPed = v;} //Assigns the pedestal flag to 'mPed' called in 'runqa.C'
  
 private:
  StFmsDbMaker *mFmsDbMkr;             //Pointer to FMS database for retrieving values
  StFmsCollection *mFmsCollectionPtr;  //Pointer to FMS object that contains all the data from the file
  
  int mRun;             //The run number of interest
  int mPed;             //The pedestal or not flag (1 is pedestal, 0 is physics)
  TFile *mFile;         //ROOT file that will be created with the histogramss
  char mFilename[100];  //holds the name of the file that will be created
  
  static const int mNPREPOST=3; //Number of pre and post crossings
  static const int mNQ=2;       //Number of quadrants in FPOST
  static const int mNL=6;       //Number of layers in FPOST
  static const int mNS=43;      ///Number of slats in FPOST
  static const int mNID=241;    //Maximum Id for FPOST
  static const int mNTRG=64;    //Number of triggers in STAR

  bool ran_fpost = false;       //Variable to hold whether this run contains fpost or not
  TH1F* mDataSize[2];           //Histograms for datasize [0] is total [1] is 
  TH1F* mRccDiff[2];            //Histograms for the RCC-TCU difference
  TH1F* mXing[mNPREPOST*2+1];   //Histograms for crossings
  TH2F* mAdc2[2];               //2D histograms for ADCs where x-axis is slad id y-axis is adc value [0] is for upper adc range [1] is lower adc range 
  TH1F* mAdc[mNID][2];          //1D histograms for ADCs by ID where [0] is upper adc range and [1] is lower adc range
  TH1F* mNHit[mNQ][mNL];        //1D histogram for number of hits by quadrant and layer where x-axis is number of hits
  TH1F* mHit[mNQ][mNL];         //1D histogram for hits in each slat per quadrant per layer where x-axis is the slat number
  TH1F* mNHitTrg[mNTRG+1];      //1D histogram for number of hits for each trigger
  TH2F* mNHitTrg2;              //2D histogram for each trigger where x-axis is the number of hits and y-axis is the trigger
  
  ClassDef(StFpostQaMaker,1);// for Versioning?
};

#endif //STAR_StFpostQaMaker_HH


/*
 * $Id: StFpostQaMaker.h,v 1.1 2017/02/22 07:14:44 akio Exp $
 * $Log: StFpostQaMaker.h,v $
 * Revision 1.1  2017/02/22 07:14:44  akio
 * Initial version from David
 *
 * Revision 1.1  2017/02/01 23:10:12  dkap7827
 * new fpost qa maker
 *
 */


