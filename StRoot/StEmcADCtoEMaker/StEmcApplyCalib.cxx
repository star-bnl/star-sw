/***************************************************************************
 *
 * $Id: StEmcApplyCalib.cxx,v 1.1 2001/07/17 00:14:37 perev Exp $
 *
 * Author:  bl
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              StRchMaker.cxx - ROOT/STAR Maker for offline chain.
 *              Incorporation of cluster finder here
 ***************************************************************************
 *
 * See Log Comments at bottom
 ***************************************************************************/

#include <iostream.h>
#include <fstream.h>

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

ClassImp(StEmcApplyCalib) // macro

   
//--------------------------------------------------------------

    StEmcApplyCalib::StEmcApplyCalib(const StEvent*event,const TDataSet* calibdb)
      : mevent(event), m_calibdb(calibdb)
{
}

//-----------------------------------------------------------------

StEmcApplyCalib::~StEmcApplyCalib() {}

//-----------------------------------------------------------------

Int_t StEmcApplyCalib::Calibrate() {
    cout << "ApplyCalib::Calibrate()" << endl;

//Get DB
 cout<<"Getting DB"<<endl;
    StEmcHandleDB * db=new StEmcHandleDB(m_calibdb);
    cout<<"DB handled"<<endl;

// Get EmcCollection, apply separately for each subdetectors
      StEmcCollection * emccoll=mevent->emcCollection();  

   if(!emccoll){
    cout<<"EmcCollection does not exist **, quit"<<endl;return kStWarn;
   }

    //
    //First , Tower 
    Int_t stat_tower = Calibrate_Tower(db,emccoll);
    if(stat_tower!=kStOK){cout<<"Tower Calibration not OK**"<<endl;}
    else{cout<<"Tower Calibration OK**"<<endl;}
    Int_t stat_smd = Calibrate_Smd(db,emccoll);
    if(stat_smd!=kStOK){cout<<"Smd Calibration not OK**"<<endl;}
    else{cout<<"Smd Calibration OK**"<<endl;}
    delete db;db=0;
    return kStOK;
}

//////////////////////////////////////////////////
Int_t StEmcApplyCalib::Calibrate_Tower(StEmcHandleDB* db,StEmcCollection* emccoll)
{

/*
    StDetectorId id = static_cast<StDetectorId>(1+kBarrelEmcTowerId);
    if(StEmcDetector* detector=(StEmcDetector*)emctemp->detector(id))

  {
    for(UInt_t m=1;m<=120;m++){
      for(UInt_t e=1;e<=20;e++){
        for(UInt_t s=1;s<=2;s++)
        {
        StEmcRawHit* hit=;
          if(hit->ADC()>0)
          {
            UInt_t ADC=hit->ADC();
          Float_t calib=0;
//            int calstat=db->GetTowerCalibs(i,j,k,calib);
            int calstat=kStOK;
      ifcalstat==kStOK)ADC*=calib;   
       }                            
        }
      }
    }     

}
*/
 return kStOK;
}
/////////////////////////////////////////////////////
Int_t StEmcApplyCalib::Calibrate_Smd(StEmcHandleDB* db,StEmcCollection* emccoll)
{
 return kStOK;
}
