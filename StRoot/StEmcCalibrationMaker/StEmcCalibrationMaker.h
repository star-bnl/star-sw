/**********************************************************************
* StEmcCalibrationMaker
* Author: Alexandre A. P. Suaide 
*
* This maker does calibration on the EMC detector
***********************************************************************/

/*!\class StEmcCalibrationMaker
\author Alexandre A. P. Suaide

This class is responsible for doing barrel EMC calibration. EMC calibration
will consist of a combination of three methods:

  - MIP calibration
  - Electron calibration
  - pi0 calibration
  
At the moment, only MIP calibration is working. <br><br>
There are two ways of doing EMC calibration:
  - Tower by tower calibration
    - In this case, each tower is calibrated independtly. This requires a very high number of events to get enough statistics on each tower
  - Eta bin calibration. 
    - In this case, all towers in the same eta bin are equalized and theier ststistics are added. In this case fewer events are necessary
    
To set the way calibration is going to be done, it is necessary to fill two calibration structures:
  - St_emcCalSummary
    - This structure contains the summary of calibration and the detector that is going to be calibrated.
    - Members are:
      -  DetNumber              - detector number  
      -  FirstRun               - first run used for calibration  
      -  LastRun                - last run used for calibration  
      -  NEvents                - number of events used for calibration  
      -  CalibMode              - calmode (1=Equalization, 10=MIP,100=Pi0,1000=Electron)  
      -  Operator[64]           - Operator Name  
      -  Comments[256]          - comments  
    - In this structure it is necessary to fill only DetNumber (1 for towers, 2 for pre-shower, 3 for SMDE and 4 for SMDP). Operator and Comments can also be filled. The other members are filled by the program.
  - St_emcCalSettings
    - This table defines the settings that will be used for calibration
    - Members are:
      - DataType                - data type  
      - UseL3Tracks             - type of track used (0 = reco tracks, 1 = L3 tracks)
      - ZVertexCut              - z vertex cut in cm
      - NEtaBins                - number of eta bins  for equalization
      - EtaBinWidth             - width of eta bin in detector units  
      - DoEqualization          - 1 - Equalize eta bins, 0 - dont equalize 
      - EqualizationMethod      - equalization method  (see StEmcEqualSpectra for detais)
      - EqEventsPerBin          - minimum number of events per bin for Equalization  
      - EqMinNumberOfTracks     - minimum number of tracks for a equalization event  
      - EqMinOccupancy          - minimum occupancy for hole detector for equalization 
      - UseMipCalib             - 1 - use MIP peak for calibration, 0 - Dont use MIP  
      - UseMipEtaBin            - 1 - use Etabin (requires equalization), 0 - Dont use Etabin  
      - EOverMipCte             - see V.Rykov for details  
      - MipPeakFitFuntion       - function used to fit the MIP peak, 0 = two gaussians  
      - MipEventsPerBin         - minimum number of events per bin for calibration  
      - MipMaxNumberOfTracks    - maximum number of tracks for a calibration event  
      - MipMinOccupancy         - minimum occupancy for hole detector for calibration  
      - MipMinimumMomentum      - minimum momentum for a MIP track  
      - UsePi0Calib             - 1 - use Pi0 for calibration, 0 - Dont use Pi0  
      - UseElectronCalib        - 1 - use Electron for calibration, 0 - Dont use Electron  
      - DoPedSubtraction        - subtract or not electronic pedestals  
      - DoEffPedCalculation     - calculate effective pedestals  
      - DoEffPedSubtraction     - do eff pedestal subtraction (requires effped calc)  
      - EffPedMethod            - method to calculate pedestal  
      - EffPedEventsPerBin      - number of events per bin  
      - EffPedMinOccupancy      - minimum occupancy to be ready for ped calculation  
      - EffPedMaxNumberOfTracks - max number of tracks for eff pedestal calulation  
    - All members have default values. Change only the ones that are really important
*/

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

#include "TH1.h" 
#include "TH2.h" 

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
           
           Bool_t  ReadHitsStEvent(); ///< Read Hits using StEvent format
           Bool_t  CalcZVertex(); ///< Gets zVertex. if fails try to calculate one
           Bool_t  FillEqual(); ///< Fill equalization spectra with EMC hits
           Bool_t  FillMipCalib();///< Fill MIP spectra with EMC hits
           Bool_t  FillEffPed();///< Fill effective pedestal spectra with EMC hits
           Bool_t  CheckTracks();///< Check tracks on EMC and select good candidates for each calibration mode
           Bool_t  ProjectTrack(StTrack*,double,Float_t*,Float_t*); ///< Project track on EMC surface
           Bool_t  CheckPedestal(); ///< Check electronics pedestal and subtract them 
           void    ClearCalibTable(); ///< Clear calibration table
           void    SetCalibStatus(); ///< Set calibration status with good towers/strips
           void    ClearEqualTable(); ///< Clear equalization table
           void    ClearMipTable(); ///< Clear MIP table
           Bool_t  FillEmcVector(); ///< Fill EMC vector from StEvent
      
   public:
   
                   StEmcCalibrationMaker(const char *name="EmcCalibration"); ///< Default constructor
   virtual        ~StEmcCalibrationMaker(); ///< Default destructor
   virtual Int_t   Init(); ///< Init method
   virtual Int_t   Make(); ///< Make mathod - process each event
   virtual Int_t   Finish(); ///< Finish method - save final numbers
           void    Clear(const Option_t *option=""); ///< Clear method. It is used to erase temp variables from event to event
           Bool_t  Equalize(); ///< Equalize the detector in eta bin
           Bool_t  MipCalib(); ///< Do MIP calibration
           Bool_t  MakeCalibration(); ///< Get all calibration information and calculate final calibration constants
           Bool_t  MakeEffPed(); ///< Calculate effective pedestals
           Bool_t  SaveTables(); ///< Save all calibration tables and spectra
           void    LoadSpectra(char*); ///< Load spectra from disk
           void    CalcEtaBin(Int_t,Float_t,Int_t*,Int_t*,Int_t*,Int_t*); ///< Calculates eta bin

           St_emcCalSummary*      SummaryTable; ///< Calibration summary table
           St_emcCalSettings*     SettingsTable;///< Calibration settings table
           St_emcCalibration*     CalibTable; ///< Calibration table
           St_emcEqualization*    EqualTable; ///< Equalization table
           St_emcMipCalib*        MipTable; ///< MIP table
   
           StEmcEqualSpectra*     EqualSpec; ///< Equalization spectra
           StEmcMipSpectra*       MipSpec; ///< Mip spectra
           StEmcEffPedSpectra*    EffPedSpec; ///< Effective pedestal spectra


   ClassDef(StEmcCalibrationMaker, 1)  
};

#endif
