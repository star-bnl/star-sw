/**********************************************************************
* StEmcCalibrationMaker
* Author: Alexandre A. P. Suaide 
*
* This maker does calibration on the EMC detector
***********************************************************************/
#ifndef STAR_StEmcCalibrationMaker
#define STAR_StEmcCalibrationMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StEmcMipSpectra.h"
#include "StEmcEqualSpectra.h"

#include "tables/St_emcCalSettings_Table.h"
#include "tables/St_emcCalSummary_Table.h"
#include "tables/St_emcCalibration_Table.h"
#include "tables/St_emcEqualization_Table.h"
#include "tables/St_emcMipCalib_Table.h"
#include "TRandom.h"

#define StEmcCalibrationMaker_DEBUG

#define maxdet 8

class StTrack;

class StEmcCalibrationMaker : public StMaker 
{
  private:
           
           //QA histograms ******************
           TH1F* m_EqualOccupancy;        //!
           TH1F* m_MipOccupancy;          //!
           //********************************
           
           Int_t   detnum;
           
           Float_t zVertexMax;
           Float_t zVertex;
           Float_t ptMip;
           Float_t miptemp;
           Float_t equaltemp;
           Float_t evnumber;
           Float_t avg;
           Float_t sigma;
           
           Int_t   firstEventTime;
           Int_t   lastEventTime;
           Int_t   firstEventRun;
           Int_t   lastEventRun;
           Int_t   firstEventDate;
           Int_t   lastEventDate;

           Int_t   EqStatus;
           Int_t   MipStatus;
           Int_t   CalibStatus;
           Int_t   m_equalCounter;
           Int_t   m_mipCounter;
           Float_t m_equalStep;
           Float_t m_calibStep;
           Int_t   nTracks;
      
   public:
   
                   StEmcCalibrationMaker(const char *name="EmcCalibration");
   virtual        ~StEmcCalibrationMaker();
   virtual Int_t   Init();
   virtual Int_t   Make();
   virtual Int_t   Finish();
   virtual Int_t   Clear();
           Bool_t  ReadHitsOnline();
           Bool_t  ReadHitsFromDaqFile();
           Bool_t  ReadHitsOffline();
           Bool_t  CalcZVertex();
           Bool_t  FillEqual();
           Bool_t  FillMipCalib();
           Bool_t  Equalize();
           Bool_t  MipCalib();
           Bool_t  MakeCalibration();
           Bool_t  SaveTables();
           Bool_t  IsThisTrackGood(Int_t,Float_t*,Float_t*);
           Bool_t  ProjectTrack(StTrack*,double,Float_t*,Float_t*);
           Bool_t  GetPedestal();
           Bool_t  SubtractPedestal();
           void    ClearCalibTable();
           void    SetCalibStatus();
           void    ClearEqualTable();
           void    ClearMipTable();
           void    CalcEtaBin(Int_t,Float_t,Int_t*,Int_t*,Int_t*,Int_t*);

           St_emcCalSummary*      SummaryTable;
           St_emcCalSettings*     SettingsTable;
           St_emcCalibration*     CalibTable;
           St_emcEqualization*    EqualTable;
           St_emcMipCalib*        MipTable;
   
           StEmcEqualSpectra*     EqualSpec;
           StEmcMipSpectra*       MipSpec;



   ClassDef(StEmcCalibrationMaker, 1)  
};

#endif
