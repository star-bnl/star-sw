/**********************************************************************
* StEmcCalibrationMaker
* Author: Alexandre A. P. Suaide 
*
* This maker does calibration on the EMC detector
***********************************************************************/

/*!\class StEmcCalibrationMaker
\author Alexandre A. P. Suaide

*/

#ifndef STAR_StEmcCalibrationMaker
#define STAR_StEmcCalibrationMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "tables/St_emcPed_Table.h"
#include "tables/St_smdPed_Table.h"

#include "StEmcMipSpectra.h"
//#include "StEmcElecSpectra.h"
#include "StEmcEqualSpectra.h"
#include "StEmcPedSpectra.h"

#include "TH1.h" 
#include "TH2.h"
#include "TString.h" 
#define maxdet 8

class StTrack;
class StEvent;
class StEmcCollection;
class StEmcGeom;
class StEmcFilter;
class StEmcPosition;
class StTrack;

class StEmcCalibrationMaker : public StMaker 
{
  private:
                      
           StEmcEqualSpectra*     mEqualSpec; ///< Equalization spectra
           StEmcEqualSpectra*     mGainSpec; ///< GainMonitor spectra
           StEmcPedSpectra*       mPedSpec; ///< Pedestal spectra
           StEmcMipSpectra*       mMipSpec; ///< Mip spectra
           //StEmcElecSpectra*      mElecSpec; ///< Electron spectra
                      
           StEmcGeom*           mCalibGeo;//!
           StEmcCollection*     mEmc;     //!
           StEvent*             mEvent;   //!
           StEmcFilter*         mFilter;  //!
           StEmcPosition*       mPosition;//!
					 
					 TH2F*                mCalib;   //!
					 TH2F*                mEqual;   //!
           TH2F*                mPed;     //!
					 TH1F*                mIsOnOff; //!
					 TH1F*                mHitsAdc; //!
					 TH1F*                mHitsE;   //!
					 
					 TString mDetName;
           
					 Bool_t  mSubPedestal;
					 Bool_t  mDoUseL3;
					 Bool_t  mDoEqual;
           Bool_t  mDoGain;
           Bool_t  mDoMip;
           Bool_t  mDoPed;
           Bool_t  mGain;
           Bool_t  mWaitForPed;
           Bool_t  mUseLocalPed;
           Bool_t  mSavePedToDB;
           Bool_t  mSaveCalibToDB;

           Int_t   mGainMode;
					 Int_t   mDetNum;
					 Int_t   mNBins;
					 Int_t   mNTracks;
           Int_t   mPedStatus;
           Int_t   mEqStatus;  
           Int_t   mGainStatus;    
           Int_t   mMipStatus;					 
                
           Int_t   mEvNumber;
           Int_t   mFirstEventTime;
           Int_t   mFirstEventDate;
           Int_t   mGainDate;
           Int_t   mGainTime;
           Int_t   mPedDate;
           Int_t   mPedTime;
           Float_t mPedInterval;
           Int_t   mFirstEventRun;
           Float_t mZVertex;
					 Float_t mZVertexMax;
           Float_t mBField;
           Float_t mEoverMIP;          

           Int_t   mNEtaBins;
           Int_t   mEtaBinSize;

					 Int_t   mElecStatus;
           Int_t   mCalibStatus;
           
           Bool_t  GetEvent();
					 Bool_t  ReadStEvent(); ///< Read Hits using StEvent format
           Bool_t  CalcZVertex(); ///< Gets zVertex. if fails try to calculate one
           Bool_t  CheckPedestal(); ///< Check electronics pedestal and subtract them 
           Bool_t  FillEmcVector(); ///< Fill EMC vector from StEvent
					 void    SetStatus();
      
   public:
   
                   StEmcCalibrationMaker(const char *name="EmcCalibration"); ///< Default constructor
   virtual        ~StEmcCalibrationMaker(); ///< Default destructor
   virtual Int_t   Init(); ///< Init method
   virtual Int_t   Make(); ///< Make mathod - process each event
   virtual Int_t   Finish(); ///< Finish method - save final numbers
           Bool_t  MakeCalibration(); ///< Get all calibration information and calculate final calibration constants
           Bool_t  SaveTables(); ///< Save all calibration tables
           Bool_t  SaveSpectra(char*); ///<Saves all spectra
           void    LoadSpectra(char*); ///< Load spectra from disk
           Float_t GetDeltaTime(Int_t,Int_t,Int_t,Int_t);
           void    SavePedestals(Int_t,Int_t);
           void    SaveCalibration(Int_t,Int_t);

   
           StEmcEqualSpectra* GetEqualSpec()      { return mEqualSpec;} ///< Equalization spectra
           StEmcEqualSpectra* GetGainSpec()       { return mGainSpec; }///< GainMonitor spectra
           StEmcPedSpectra*   GetPedSpec()        { return mPedSpec; }///< Pedestal spectra
           StEmcMipSpectra*   GetMipSpec()        { return mMipSpec; }///< Mip spectra

					 void    SetSubPedestal(Bool_t a)       {   mSubPedestal = a; }
					 void    SetDoUseL3(Bool_t a)           {   mDoUseL3 = a; }
					 void    SetDoEqual(Bool_t a)           {   mDoEqual = a; }
           void    SetDoGain(Bool_t a)            {   mDoGain = a; }
           void    SetDoMip(Bool_t a)             {   mDoMip = a; }
           void    SetDoPed(Bool_t a)             {   mDoPed = a; }
           void    SetWaitForPed(Bool_t a)        {   mWaitForPed = a; }
           void    SetUseLocalPed(Bool_t a)       {   mUseLocalPed = a; }
           void    SetSavePedToDB(Bool_t a)       {   mSavePedToDB = a; }
           void    SetSaveCalibToDB(Bool_t a)     {   mSaveCalibToDB = a; }

           void    SetGainMode(Int_t a)           {   mGainMode = a; }
					 void    SetDetNum(Int_t a)             {   mDetNum = a; }
           
					 void    SetZVertexMax(Float_t a)       {   mZVertexMax = a; }
           void    SetEoverMIP(Float_t a)         {   mEoverMIP = a; }           

           void    SetNEtaBins(Int_t a)           {   mNEtaBins = a; }
           void    SetEtaBinSize(Int_t a)         {   mEtaBinSize = a; }

   ClassDef(StEmcCalibrationMaker, 1)  
};

#endif
