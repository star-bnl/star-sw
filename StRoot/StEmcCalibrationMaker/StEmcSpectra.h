/**********************************************************************
* StEmcSpectra
* Author: Alexandre A. P. Suaide 
*
* This is a general EMCSpectra class
***********************************************************************/

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

class StEmcSpectra : public St_DataSet 
{
  private:
           Int_t    detNum;
                                      
           TMatrix  Spectra;
           TArrayF  Sum;
           TArrayI  IsOnOff;
                                          
  protected:   
  public: 
           Int_t    nModules;
           Int_t    nEta;
           Int_t    nSub;
           Int_t    nBins;
           Int_t    nAdcMax;
           Int_t    nEtaBins;
           Float_t  etaBinWidth;

           St_emcCalSettings*     SettingsTable;
           St_emcEqualization*    EqualTable;  
           St_emcMipCalib*        MipTable; 
           St_emcCalibration*     CalibTable; 

                   StEmcSpectra(const char*);  
  virtual          ~StEmcSpectra();
  virtual  void    Init();
           Bool_t  Zero(Int_t);
           Bool_t  ZeroAll();
           Int_t   GetID(Int_t,Int_t,Int_t);
           Bool_t  FillSpectra(Int_t,Int_t);
           Float_t GetAdcValue(Int_t,Int_t);
           Int_t   GetStatus(Int_t);
           Float_t GetSum(Int_t);
           Bool_t  GetMeanAndRms(Int_t,Float_t*,Float_t*);
           Bool_t  GetMeanAndRms(Int_t,Int_t,Int_t,Float_t*,Float_t*);
           Bool_t  GetLogMeanAndRms(Int_t,Int_t,Int_t,Float_t*,Float_t*);
           Bool_t  GetOccupancy(Int_t,Float_t*,Float_t*,Float_t*);
           Bool_t  GetOccupancyEtaBin(Int_t,Float_t*,Float_t*,Float_t*);
 
           void    Draw(Int_t);
           void    DrawOccupancy();
  virtual  void    DrawEtaBin(Int_t);         
           Int_t   GetNBin();
           Int_t   GetNEtaBin();
           Int_t   GetNAdcMax();
           Int_t   GetNModules();
           Int_t   GetNEta();
           Int_t   GetNSub();
     const char*   GetDetName();
           TArrayF GetSpectra(Int_t);
           TArrayF GetSpectra(Int_t,Float_t,Float_t);
           TArrayF ReBin(Int_t,Float_t,Float_t);
  virtual  void    CalcEtaBin(Int_t,Float_t,Int_t*,Int_t*,Int_t*,Int_t*);
           TArrayF GetEtaBinSpectra(Int_t);
           Float_t GetSumEtaBin(Int_t); 
           Int_t   GetEtaBinId(Int_t,Int_t);
           
           void    SaveAll(char*);
           void    LoadAll(char*);

  ClassDef(StEmcSpectra,1)
};

inline Int_t   StEmcSpectra::GetNBin()              { return nBins; }
inline Int_t   StEmcSpectra::GetNEtaBin()           { return nEtaBins; }
inline Int_t   StEmcSpectra::GetNAdcMax()           { return nAdcMax; }
inline Int_t   StEmcSpectra::GetNModules()          { return nModules; }
inline Int_t   StEmcSpectra::GetNEta()              { return nEta; }
inline Int_t   StEmcSpectra::GetNSub()              { return nSub; }
#endif
