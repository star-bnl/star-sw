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
#include "StEmcEffPedSpectra.h"

#include "TArrayI.h"

#include "tables/St_emcCalSettings_Table.h"
#include "tables/St_emcCalSummary_Table.h"
#include "tables/St_emcCalibration_Table.h"
#include "tables/St_emcEqualization_Table.h"
#include "tables/St_emcMipCalib_Table.h"
#include "tables/St_emcPedestal_Table.h" 

//#define StEmcCalibrationMaker_DEBUG 1

#define maxdet 8

class StTrack;
class StEvent;
class StEmcCollection;
class StEmcGeom;

class StEmcCalibrationMaker : public StMaker 
{
  private:
           
           
           //QA histograms ******************
           TH1F* m_EqualOccupancy;        //!
           TH1F* m_MipOccupancy;          //!
           TH1F* m_EffPedOccupancy;       //!
           //********************************
           StEmcGeom*           calibGeo;//!
           StEmcCollection*     emc;   //!
           StEvent*             event; //!
           
           Int_t   detnum;
           Int_t   nbins;

           Float_t zVertexMax;
           Float_t zVertex;
           Float_t ptMip;
           Float_t miptemp;
           Float_t equaltemp;
           Float_t evnumber;
           Float_t avg;
           Float_t sigma;
           Float_t BField;
           
           TArrayI emcHits;
           TArrayI trackTower,tmp1,tmp2;
           
           Int_t   firstEventTime;
           Int_t   lastEventTime;
           Int_t   firstEventRun;
           Int_t   lastEventRun;
           Int_t   firstEventDate;
           Int_t   lastEventDate;

           Int_t   PedStatus;
           Int_t   EqStatus;
           Int_t   MipStatus;
           Int_t   CalibStatus;
           Int_t   m_pedCounter;
           Int_t   m_equalCounter;
           Int_t   m_mipCounter;
           Float_t m_equalStep;
           Float_t m_calibStep;
           Int_t   nTracks;
           
           Bool_t  ReadHitsOffline();
           Bool_t  CalcZVertex();
           Bool_t  FillEqual();
           Bool_t  FillMipCalib();
           Bool_t  FillEffPed();
           Bool_t  CheckTracks();
           Bool_t  ProjectTrack(StTrack*,double,Float_t*,Float_t*);
           Bool_t  SubtractPedestal();
           void    ClearCalibTable();
           void    SetCalibStatus();
           void    ClearEqualTable();
           void    ClearMipTable();
           Bool_t  FillEmcVector();
      
   public:
   
                   StEmcCalibrationMaker(const char *name="EmcCalibration");
   virtual        ~StEmcCalibrationMaker();
   virtual Int_t   Init();
   virtual Int_t   Make();
   virtual Int_t   Finish();
           void    Clear(const Option_t *option="");
           Bool_t  Equalize();
           Bool_t  MipCalib();
           Bool_t  MakeCalibration();
           Bool_t  MakeEffPed();
           Bool_t  SaveTables();
           void    LoadSpectra(char*);
           void    CalcEtaBin(Int_t,Float_t,Int_t*,Int_t*,Int_t*,Int_t*);

           St_emcCalSummary*      SummaryTable;
           St_emcCalSettings*     SettingsTable;
           St_emcCalibration*     CalibTable;
           St_emcEqualization*    EqualTable;
           St_emcMipCalib*        MipTable;
           St_emcPedestal*        ped;
   
           StEmcEqualSpectra*     EqualSpec;
           StEmcMipSpectra*       MipSpec;
           StEmcEffPedSpectra*    EffPedSpec;


   ClassDef(StEmcCalibrationMaker, 1)  
};

#endif
