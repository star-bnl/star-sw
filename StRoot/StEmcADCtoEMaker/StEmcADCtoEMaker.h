// $Id: StEmcADCtoEMaker.h,v 1.15 2001/12/28 15:03:09 suaide Exp $
// $Log: StEmcADCtoEMaker.h,v $
// Revision 1.15  2001/12/28 15:03:09  suaide
// fixed documentation
//
// Revision 1.14  2001/12/27 17:45:36  suaide
// removed obsolete files and updated documentation
//
// Revision 1.13  2001/12/26 19:25:34  suaide
// Added documentation and few modifications
//
// Revision 1.12  2001/12/06 17:50:08  suaide
// changes to save ADC without pedestal subtraction
//
// Revision 1.11  2001/12/05 22:31:12  suaide
// Modifications to include SMD
//
// Revision 1.10  2001/12/04 22:05:50  suaide
// new QA histogram for tower
//
// Revision 1.9  2001/10/31 15:01:07  suaide
// modified to discard bad EMC events
//
// Revision 1.8  2001/10/25 21:31:36  suaide
// modifications to get new database tables
//
// Revision 1.7  2001/10/24 14:47:16  suaide
// type correction
//
// Revision 1.6  2001/10/24 14:41:45  suaide
// huge change on StEmcADCtoEMaker to a different software.
// The other version is kept as *.old for future debug
//
//

/*!\class StEmcADCtoEMaker
\author Alexandre A. P. Suaide

This class gets EMC raw ADC's and convert them to calibrated energy.<br><br>

The main variables that should be set are:<br>
  - kCalib[det] - Set to kTRUE if you want to calibrate an EMC subdetector.
  - isDaqFile - Set to kTRUE if you are reading a DAQ dataset.
  - subtractPedestal - Set to kTRUE if pedestal should be subtracted.
  - saveOnlyCalibHits - Set to kTRUE if you want to save only calibrated hits. Otherwise, all hits with ADC>0 are saved.

The defaults values are:<br>
  - kCalib[det] = {kTRUE, kFALSE, kTRUE, kTRUE}
  - isDaqFile = kTRUE
  - subtractPedestal = kTRUE
  - saveOnlyCalibHits = kFALSE
*/

#ifndef STAR_StEmcADCtoEMaker
#define STAR_StEmcADCtoEMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include <TH1.h>
#include <TH2.h>

#include "tables/St_emcCalibration_Table.h"
#include "tables/St_emcPedestal_Table.h"
#include "tables/St_smdPedestal_Table.h"
#include "tables/St_emcRunning_Table.h"
#include "tables/St_smdRunning_Table.h"

#include "tables/St_controlADCtoE_Table.h"
#include "StEmcUtil/emcInternalDef.h"

class StEmcCollection;
class StEmcDecoder;
class StEmcGeom;

class StEmcADCtoEMaker : public StMaker 
{
  private: 
           TH2F              *m_nhit;           //! 
           TH2F              *m_etot;           //!
           TH2F              *m_hits[MAXDET];   //!
           TH2F              *m_adc[MAXDET];    //!
           TH2F              *m_energy[MAXDET]; //!
           TH1F              *m_adc1d[MAXDET];  //!
           
           TH2F              *m_tower;          //!
           TH1F              *m_towerMean;      //!
           TH1F              *m_towerRMS;       //!
           TH1F              *m_towerSum;       //!
           
           TH2F              *m_smdTimeBin;     //!
           
           TDataSet          *m_CalibDb;
           TDataSet          *m_StatusDb;
           StEmcCollection   *m_emc;            //!
           StEmcDecoder      *decoder;          //!
           
           Int_t             status[MAXDET][18000];
           Int_t             nChannels[MAXDET];
           Float_t           ADCTemp[18000];
           
           Int_t             smdTimeBin[2][8];
           Int_t             smdTimeBinTemp[18000];
           Bool_t            kCalibTemp[MAXDET];
           
           StEmcGeom         *geo[MAXDET];

           StEmcCollection   *GetEmcCollectionFromDaq(TDataSet* daq);//!< This method gets EMC collection from DAQ dataset.
           void              GetStatus(Int_t); //!< This method gets the status tables for a given detector
           
  protected:    
    
  public: 
           Bool_t            kCalib[MAXDET]; //!< Set to kTRUE if you want to calibrate some detector
           Bool_t            isDaqFile; //!< Set to kTRUE if you are reading a .daq file
           Bool_t            subtractPedestal; //!< Set to kTRUE to subtract pedestal
           Bool_t            saveOnlyCalibHits; //!< Set to kTRUE to save only calibrated hits
                 
                             StEmcADCtoEMaker(const char *name="Eread"); //!< StEmcADCtoEMaker constructor
   virtual                   ~StEmcADCtoEMaker(); //!< StEmcADCtoEMaker destructor
   virtual Int_t             Init(); //!< Init function. This method initializes the histograms
   virtual Int_t             Make(); //!< Process each event
   virtual Int_t             Finish(); //!< This method creates mean ADC and RMS histograms.
           Bool_t            GetEmcEvent(); //!< This method gets EMC hits from different sources.
           Bool_t            CreateVector(Int_t); //!< This method creates a temporary ADC vector for each detector.
           Bool_t            Calibrate(Int_t,Int_t*,Float_t*); //!< This method applies the calibration constants to get the hit energy.
           Bool_t            FillHistograms(Int_t,Int_t,Float_t); //!< This method fills QA histograms
           Bool_t            FillStEvent(); //!< This method makes a clean up of StEvent
   
   static  controlADCtoE_st  *getControlTable()  {return NULL;} //!< Return Control table (NULL)
           StEmcCollection   *getEmcCollection() {return m_emc;} //!< Return emcCollection
           void              clearStEventStaf()  { m_emc = 0;} //!< Clear emcCollection (does not delete from memory)

   ClassDef(StEmcADCtoEMaker, 1)  
};

#endif
