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
#include "TArrayI.h"
#include "TArrayF.h"
#include "TMatrix.h"

#include "tables/St_emcCalSettings_Table.h"
#include "tables/St_emcCalSummary_Table.h"
#include "tables/St_emcCalibration_Table.h"
#include "tables/St_emcEqualization_Table.h"
#include "tables/St_emcMipCalib_Table.h"
class StEmcGeom;
class StEmcSpectra : public St_DataSet 
{
  private:
           Int_t      detNum;
                                      
           TMatrix    Spectra;
           TArrayF    Sum;
           TArrayI    IsOnOff;
                                          
  protected:   
  public: 
           Int_t    nModules;
           Int_t    nEta;
           Int_t    nSub;
           Int_t    nBins;
           Int_t    nAdcMax;
           Int_t    nEtaBins;
           Float_t  etaBinWidth;

           St_emcCalSettings*     SettingsTable; //!< Calibration settings table (should be set from StEmcCalibrationMaker)
           St_emcEqualization*    EqualTable;  //!< Equalization table (should be set from StEmcCalibrationMaker)
           St_emcMipCalib*        MipTable; //!< MIP table (should be set from StEmcCalibrationMaker)
           St_emcCalibration*     CalibTable; //!< Calibration table (should be set from StEmcCalibrationMaker)

                   StEmcSpectra(const char*);  //!< StEmcSpectra default constructor
  virtual          ~StEmcSpectra(); //!< StEmcSpectra default destructor
  virtual  void    Init(); //!< Initialization method
           Bool_t  Zero(Int_t); //!< Zero the spectrum of one bin
           Bool_t  ZeroAll(); //!< Zero all spectra
           Int_t   GetID(Int_t,Int_t,Int_t); //!< Return bin id
           Bool_t  FillSpectra(Int_t,Int_t); //!< fill bin spectrum
           Float_t GetAdcValue(Int_t,Int_t); //!< Return number of entries for a given ADC value
           Int_t   GetStatus(Int_t); //!< Return the status of the bin
           Float_t GetSum(Int_t); //!< Return the sum (integral) of the bin spectrum
           Bool_t  GetMeanAndRms(Int_t,Float_t*,Float_t*); //!< Get mean and RMS for a given bin
           Bool_t  GetMeanAndRms(Int_t,Int_t,Int_t,Float_t*,Float_t*); //!< Get mean and RMS for a given bin and a given ADC range
           Bool_t  GetLogMeanAndRms(Int_t,Int_t,Int_t,Float_t*,Float_t*);//!< Get logaritmic mean and RMS for a given bin and a given ADC range
           Bool_t  GetOccupancy(Int_t,Float_t*,Float_t*,Float_t*); //!< Get detector occupancy
           Bool_t  GetOccupancyEtaBin(Int_t,Float_t*,Float_t*,Float_t*); //!< Get eta bin occupancy
 
           void    Draw(Int_t); //!< Draw bin spectrum
           void    DrawOccupancy(); //!< Draw occupancy
  virtual  void    DrawEtaBin(Int_t); //!< Draw eta bin spectrum        
           Int_t   GetNBin(); //!< Return number of bins
           Int_t   GetNEtaBin(); //!< Return number of eta bins
           Int_t   GetNAdcMax(); //!< Return max ADC possible
           Int_t   GetNModules(); //!< Return number of modules
           Int_t   GetNEta(); //!< Return number of etas
           Int_t   GetNSub(); //!< Return number of subs
     const char*   GetDetName(); //!< Return detector name
           TArrayF GetSpectra(Int_t); //!< Return spectrum for one bin
           TArrayF GetSpectra(Int_t,Float_t,Float_t); //!< Return spectrum for one bin after apply equalization
           TArrayF ReBin(Int_t,Float_t,Float_t); //!< Rebin spectrum
  virtual  void    CalcEtaBin(Int_t,Float_t,Int_t*,Int_t*,Int_t*,Int_t*); //!< Calculates eta bin
           TArrayF GetEtaBinSpectra(Int_t); //!< Return eta bin spectrum
           Float_t GetSumEtaBin(Int_t);  //!< Return eta bin spectrum integral
           Int_t   GetEtaBinId(Int_t,Int_t); //!< Return eta bin Id
           
           StEmcGeom* GetGeo(); //!< Return detector geometry
           
           void    SaveAll(char*); //!< Save all spectra on file
           void    LoadAll(char*); //!< Load all spectra from file

  ClassDef(StEmcSpectra,1)
};

inline Int_t   StEmcSpectra::GetNBin()              { return nBins; }
inline Int_t   StEmcSpectra::GetNEtaBin()           { return nEtaBins; }
inline Int_t   StEmcSpectra::GetNAdcMax()           { return nAdcMax; }
inline Int_t   StEmcSpectra::GetNModules()          { return nModules; }
inline Int_t   StEmcSpectra::GetNEta()              { return nEta; }
inline Int_t   StEmcSpectra::GetNSub()              { return nSub; }
#endif
