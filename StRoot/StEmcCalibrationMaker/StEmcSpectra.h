/**********************************************************************
* StEmcSpectra
* Author: Alexandre A. P. Suaide 
*
* This is a general EMCSpectra class
***********************************************************************/

/*!\class StEmcSpectra
\author Alexandre A. P. Suaide

This class is the base class for EMC spectra. This class provides
standard functions to manipulate spectra, such as mean, rms, rebins, etc.
*/

#ifndef STAR_StEmcSpectra
#define STAR_StEmcSpectra
#include "St_DataSet.h"
#include "TH1.h"
#include "TH2.h"
#include "TH1.h"
#include "StEmcUtil/filters/StEmcFilter.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#define MAXHIST 2000

class StEmcGeom;
class StEvent;
class StTrack;
 
class StEmcSpectra : public St_DataSet 
{
  private:
           TH2F       		*mSpectra; //!
					 TH1F       		*mSum;     //!
           TH1F           *mEventCount; //!
					 TH1F       		*mIsOnOff; //!
					 TH2F       		*mEqual;   //!
					 TH2F       		*mCalib;   //!
					 TH1D           *mEtaBinTmp; //!
					 TH1D           *mRebinTmp;
           TH1D           *mHistTmp[MAXHIST];
           TH1D           *mHistTmpAsym[MAXHIST];
           
					 Float_t    		mSpecMax;
					 Float_t    		mSpecMin;
					 Int_t      		mSpecBins;
           
					 Int_t      		mNModules;
           Int_t      		mNEta;
           Int_t      		mNSub;
           Int_t      		mNBins;
					 
           Int_t      		mNEtaBins;
					 Int_t      		mEtaBinWidth;
           Int_t          mMaxMult;
           Int_t          mMinMult;
           Int_t          mMinHits;
					 Int_t      		mDetNum;
           Float_t        mMinOcc;
					 Float_t        mMinP;
           Float_t        mMaxP;
           
					 Bool_t         mDoUseL3;
					 
					 StEmcFilter*   mFilter; //!
					 StEmcPosition* mPosition; //!
					 StEmcGeom*     mGeo;                                     

  public: 

                    			StEmcSpectra(const char*, Int_t = 0, Float_t = 0, Float_t = 0);  ///< StEmcSpectra default constructor
  virtual           			~StEmcSpectra(); ///< StEmcSpectra default destructor
  virtual  void     			Init(); ///< Initialization method
           Bool_t   			ZeroAll(); ///< Zero all spectra
           Int_t    			GetID(Int_t,Int_t,Int_t); ///< Return bin id
           Bool_t   			FillSpectra(Int_t,Float_t); ///< fill bin spectrum
           Float_t  			GetAdcValue(Int_t,Float_t); ///< Return number of entries for a given ADC value
           Int_t    			GetStatus(Int_t); ///< Return the status of the bin
           Float_t  			GetSum(Int_t); ///< Return the sum (integral) of the bin spectrum
           Float_t        GetMaximum(Int_t); ///< Return the position with max statistics 
           Bool_t   			GetMeanAndRms(Int_t,Float_t*,Float_t*); ///< Get mean and RMS for a given bin
 					 Bool_t   			GetMeanAndRms(Int_t,Float_t,Float_t,Float_t*,Float_t*); ///< Get mean and RMS for a given bin and a given ADC range
 					 Bool_t   			GetLogMeanAndRms(Int_t,Float_t,Float_t,Float_t*,Float_t*);///< Get logaritmic mean and RMS for a given bin and a given ADC range 
           Bool_t   			GetOccupancy(Float_t,Float_t*,Float_t*,Float_t*); ///< Get detector occupancy
           Bool_t   			GetOccupancyEtaBin(Float_t,Float_t*,Float_t*,Float_t*); ///< Get eta bin occupancy
           void           Fit(Int_t,TF1*);
           TH1D*    			Draw(Int_t); ///< Draw bin spectrum
           void                         Draw(const Option_t* opt){TObject::Draw(opt);} //WarnOff
	   void     			DrawOccupancy(); ///< Draw occupancy
  virtual  TH1D*    			DrawEtaBin(Int_t); ///< Draw eta bin spectrum  
  virtual  void     			DrawAllEtaBin(Int_t,Float_t=10); ///< Draw all spectra in the eta bin 
	         TH1D*    			GetSpectra(Int_t); ///< Return spectrum for one bin
           TH1D*    			GetSpectra(Int_t,Float_t,Float_t); ///< Return spectrum for one bin after apply equalization
  virtual  void     			CalcEtaBin(Int_t,Int_t*,Int_t*,Int_t*,Int_t*); ///< Calculates eta bin
           Int_t    			GetEtaBinId(Int_t,Int_t); ///< Return eta bin Id
           Float_t  			GetSumEtaBin(Int_t);  ///< Return eta bin spectrum integral
					 Int_t    			GetNTracks(StEvent*);
					 StTrack* 			GetTrack(StEvent*,Int_t);
           void     			SaveAll(char*); ///< Save all spectra on file
           void     			LoadAll(char*); ///< Load all spectra from file
					 void           SetEqualConst(Int_t,Float_t,Float_t,Int_t);
					 void           GetEqualConst(Int_t,Float_t*,Float_t*,Int_t*);

           const char*    GetDetName(); ///< Return detector name
           StEmcGeom*     GetGeo()    									{ return mGeo; } ///< Return detector geometry
					 StEmcFilter*   GetFilter()   								{ return mFilter;}
					 StEmcPosition* GetPosition() 								{ return mPosition;}
					 Int_t          GetNBin()       							{ return mNBins; } ///< Return number of bins
           Int_t          GetNEtaBins()    							{ return mNEtaBins; }  ///< Return number of eta bins
           Int_t          GetEtaBinWidth()      				{ return mEtaBinWidth; }  ///< Return number of eta bins
           Int_t    			GetNModules()   							{ return mNModules; } ///< Return number of modules
           Int_t    			GetNEta()       							{ return mNEta; } ///< Return number of etas
           Int_t    			GetNSub()       							{ return mNSub; } ///< Return number of subs
           Int_t    			GetMinMultiplicity()       		{ return mMinMult; } ///< Return maximum multiplicity
           Int_t    			GetMaxMultiplicity()       		{ return mMaxMult; } ///< Return maximum multiplicity
           Int_t    			GetMinHits()       		        { return mMinHits; } ///< Return maximum multiplicity
					 Float_t        GetMinOcc()                   { return mMinOcc; }
           Float_t        GetSpecMin()                  { return mSpecMin; }
					 Float_t        GetSpecMax()                  { return mSpecMax; }
					 Float_t        GetSpecBins()                 { return mSpecBins; }
					 Float_t        GetMinMomentum()              { return mMinP; }
					 Float_t        GetMaxMomentum()              { return mMaxP; }
					 TH2F*          GetEqual()                    { return mEqual; }
					 TH2F*          GetCalib()                    { return mCalib; }
					 TH1F*          GetIsOnOff()                  { return mIsOnOff; }
					 
           void     			SetFilter(StEmcFilter* f) 		{ mFilter = f; }
					 void     			SetPosition(StEmcPosition* f) { mPosition = f; }
					 void           SetNEtaBins(Int_t a)          { mNEtaBins = a; }
					 void           SetEtaBinWidth(Int_t a)       { mEtaBinWidth = a; }
					 void           SetMinMultiplicity(Int_t a)   { mMinMult = a; }
					 void           SetMaxMultiplicity(Int_t a)   { mMaxMult = a; }
					 void           SetMinHits(Int_t a)           { mMinHits = a; }
					 void           SetMinOcc(Float_t a)          { mMinOcc = a; }
					 void           SetMinMomentum(Float_t a)     { mMinP = a; }
					 void           SetMaxMomentum(Float_t a)     { mMaxP = a; }
					 void           SetEqual(TH2F *a)             { mEqual = a; }
					 void           SetCalib(TH2F *a)             { mCalib = a; } 
					 void           SetIsOnOff(TH1F *a)           { mIsOnOff = a; } 
					 void     			SetDoUseL3(Bool_t a)  				{ mDoUseL3 = a; }
           void           Fill()                        { mEventCount->Fill(1); }
					 

           TH1D*          ReBin(Int_t,Float_t,Float_t); ///< Rebin spectrum
           TH1D*          GetEtaBinSpectra(Int_t,Int_t = 0); ///< Return eta bin spectrum
           

  ClassDef(StEmcSpectra,1)
};
#endif
