/***************************************************************************
 * Author:  Subhasis Chattopadhyay
 * Description: EMC StEvent Only Input Handling:
 ***************************************************************************/

#include <iostream.h>
#include <fstream.h>

#include "StEmcADCtoEMaker.h"
#include "StEmcApplyCalib.h"
//#include "St_DataSetIter.h"
#include "StEventTypes.h"
#include "StEmcUtil/emcDetectorName.h"
#include "StEmcHandleDB.h"
//
// Interfaces
//
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"

#include "tables/St_controlADCtoE_Table.h"

ClassImp(StEmcApplyCalib) // macro

Short_t calibBemc, calibSmd;
   
StEmcApplyCalib::StEmcApplyCalib(StEvent*event,TDataSet* calibdb)
: mevent(event), m_calibdb(calibdb)
{}

StEmcApplyCalib::~StEmcApplyCalib() {}

Int_t 
StEmcApplyCalib::Calibrate() 
{
  cout << "ApplyCalib::Calibrate()" << endl;
  StEmcHandleDB * db=0;
  calibBemc = StEmcADCtoEMaker::getControlTable()[0].bemcCalibration;
  calibSmd  = StEmcADCtoEMaker::getControlTable()[0].bsmdCalibration;
  if(calibBemc && calibSmd) {
    cout<<" StEmcApplyCalib::Calibrate() -> N O  C A L I B R A T I O N"
	<<" calibBemc "<<calibBemc<<" calibSmd "<< calibSmd<<endl;
    return kStWarn;
  }

// Get EmcCollection, apply separately for each subdetectors
  StEmcCollection * emccoll=mevent->emcCollection();  
  if(!emccoll){
    cout<<"EmcCollection does not exist **, quit" << endl;
    return kStWarn;
  }

  if(calibBemc || calibSmd) {
//Get DB
    cout<<"Getting CalibDB"<<endl;
    db = new StEmcHandleDB(m_calibdb);
    db->Process_TowerCalibDB(); 
    cout<<"DB handled"<<endl;
  }

  //First , Tower 
  if(calibBemc) {
    Int_t stat_tower = Calibrate_Tower(db,emccoll);
    if(stat_tower!=kStOK) {
      cout<<"Tower Calibration not OK**"<<endl;
    } else {
      cout<<"Tower Calibration OK**"<<endl;
    }
  }
  if(calibSmd) {
  //Second  , SMD 
    Int_t stat_smd = Calibrate_Smd(db,emccoll);
    if(stat_smd!=kStOK) {
      cout<<"Smd Calibration not OK**"<<endl;
    } else {
      cout<<"Smd Calibration OK**"<<endl;
    }
  }
  if(db) delete db; 
  db=0;
  return kStOK;
}

Int_t 
StEmcApplyCalib::Calibrate_Tower(StEmcHandleDB* db,StEmcCollection* emccoll)
{
    StDetectorId id = static_cast<StDetectorId>(1+kBarrelEmcTowerId);
    StEmcDetector* detector1=(StEmcDetector*)emccoll->detector(id);
    if(detector1){
     for(UInt_t j=1;j<121;j++)
      {
        StEmcModule* module1 = detector1->module(j);
        StSPtrVecEmcRawHit& rawHit1=module1->hits();
 
        for(UInt_t k1=0;k1<rawHit1.size();k1++)
        {
             Int_t m1, e1, s1;
              m1=(Int_t)rawHit1[k1]->module();
              e1=(Int_t)rawHit1[k1]->eta();
              s1=abs(rawHit1[k1]->sub());

          Float_t calib[5];
          for(Int_t i=0;i<5;i++){calib[i]=0.;}
          Float_t energy=0;
            int calstat=db->GetTowerCalibs(m1,e1,s1,calib);
             if(calstat==kStOK){
                Float_t ADC=rawHit1[k1]->adc();
//                energy=ADC*calib[0];
                Float_t adcpower=1;
                for(Int_t i=0;i<5;i++)
                {energy+=calib[i]*adcpower; adcpower*=ADC;} 
            }
            else{
             cout<<" error in calstat, what to do??"<<endl;
            }
        rawHit1[k1]->setEnergy(energy);
       }
      }
    }
       else{cout<<"detector not found**"<<endl;}
 return kStOK;
}

Int_t 
StEmcApplyCalib::Calibrate_Smd(StEmcHandleDB* db,StEmcCollection* emccoll)
{
  for(UInt_t idet=3;idet<=4;idet++){
    StDetectorId id = static_cast<StDetectorId>(idet+kBarrelEmcTowerId);
    if(StEmcDetector* detector1=(StEmcDetector*)emccoll->detector(id))
 
    for(UInt_t j=1;j<121;j++)
      {
        StEmcModule* module1 = detector1->module(j);
        StSPtrVecEmcRawHit& rawHit1=module1->hits();
 
        for(UInt_t k1=0;k1<rawHit1.size();k1++)
        {
             Int_t m1, e1, s1;
              m1=(Int_t)rawHit1[k1]->module();
              e1=(Int_t)rawHit1[k1]->eta();
              s1=abs(rawHit1[k1]->sub());
 
          Float_t calib=0;
          Float_t energy=0;
          int calstat=0;
            if(idet==3){calstat=db->GetSmdECalibs(m1,e1,calib);}
            if(idet==4){calstat=db->GetSmdPCalibs(m1,e1,s1,calib);}
             if(calstat){
                Float_t ADC=rawHit1[k1]->adc();
                energy=ADC*calib;
             }
            else{
             cout<<" error in calstat, what to do??"<<endl;
            }
        rawHit1[k1]->setEnergy(energy);
       }
      }    
   }

 return kStOK;
}
