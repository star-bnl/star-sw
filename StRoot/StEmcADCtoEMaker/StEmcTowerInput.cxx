/***************************************************************************
 * Author:  Subhasis Chattopadhyay
 *   Corr:  Aleksei Pavlinov
 ***************************************************************************
 *
 * Description: EMC Tower Handling:
 ***************************************************************************/

#include <iostream.h>
#include <fstream.h>

#include "StEmcADCtoEMaker.h"
#include "StEmcTowerInput.h"
#include "StEventTypes.h"
#include "StEmcUtil/emcDetectorName.h"
#include "StEmcUtil/StEmcGeom.h"
#include "StEmcHandleDB.h"
//
// Interfaces
//
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"

#include "tables/St_controlADCtoE_Table.h"

ClassImp(StEmcTowerInput) // macro

St_emcCalibration*   cal;
//StEmcHandleDB* db;
   
StEmcTowerInput::StEmcTowerInput(StEvent*event, StEMCReader* emcreader,TDataSet* calibdb)
: mevent(event), mTheEmcReader(emcreader),mCalibDb(calibdb)
{ }

StEmcTowerInput::~StEmcTowerInput() {}

Int_t 
StEmcTowerInput::ProcessInput() {
  cout << "TowerInput::ProcessInput()" << endl;
  static unsigned short ADC=0;
// Initialize tower ADC array
  for(Int_t i=0;i<120;i++){
    for(Int_t j=0;j<20;j++){
      for(Int_t k=0;k<2;k++){
      //      mTowerADC[i][j][k]=0.; // 1-oct-2001 by pai 
        int stat = mTheEmcReader->getTowerADC(i,j,k,ADC);
        if(stat == 1) {
          mTowerADC[i][j][k]=(Float_t)(ADC);
        } else {
          mTowerADC[i][j][k] = 0.0;
          cout<<" problem in getting tower ADC**"<<endl;
        }
      }
    }
  }
  
  Int_t stat_fill=0, stat_ped=0;
  controlADCtoE_st *tmpTab = StEmcADCtoEMaker::getControlTable();
  Short_t deductPed = tmpTab->bemcDeductPedestal;

  StEmcHandleDB *db=0;;

  if(deductPed) {
    //Get DB
    cout<<"Getting DB"<<endl;
    db = new StEmcHandleDB(mCalibDb);
    db->ProcessDB();
    cout<<"DB handled"<<endl;
    //
    //First , Pedestals
    //Check if the data is pedestal subtracted, otherwisecheck if
    //pedestal tables exist in DB
    cout<<"subtract peds **"<<endl;
    stat_ped = subtractPedestals(db);
    cout<<"peds subtracted ** stat_ped"<<stat_ped<<endl;
  }
    //Apply Equalization consts
//    Int_t stat_eual = applyEqualization(db);
    //Write into StEvent
  stat_fill = fillEvent();
  if(stat_fill == kStOK) return kStOK;
  if(db) delete db;
  db=0;
  return kStOK;
}

Int_t 
StEmcTowerInput::subtractPedestals(StEmcHandleDB* db)
{
  // subtract pedestals
  //If peds table absent then return kStErr
  for(Int_t i=0;i<120;i++){
    for(Int_t j=0;j<20;j++){
      for(Int_t k=0;k<2;k++){
        Float_t ped=0;
        int pedstat=db->GetTowerPeds(i,j,k,ped);
//     if(pedstat==kStOK)cout<<"i "<<i<<" j "<<j<<"k "<<k<<"ADC "<<mTowerADC[i][j][k]<<"ped  "<<ped<<endl;
        if(pedstat==kStOK)mTowerADC[i][j][k]-=ped;
      }
    }
  }
  return kStOK;
}

Int_t 
StEmcTowerInput::applyEqualization(StEmcHandleDB* db)
{
  // Apply Equalization
  //If equalization table is absent then return kStErr
  for(Int_t i=1;i<120;i++){
    for(Int_t j=1;j<20;j++){
      for(Int_t k=1;k<2;k++){
        Float_t equal=1.;
        //      int pedstat=db->GetTowerEquals(i,j,k,equal);
        mTowerADC[i][j][k]*=equal;
      }
    }
  }
  return kStOK;
}

Int_t 
StEmcTowerInput::fillEvent()
{
  // Fill StEvent - correct 1-oct-2001 by PAI
  StEmcCollection *emctemp = mevent->emcCollection(); 
  if(!emctemp){
    cout<<" Emc Collection does not exist ";
    emctemp=new StEmcCollection();
    mevent->setEmcCollection(emctemp);
    cout<<" so => emcCollection created and set to StEvent emccollection SET "<< endl;
  }

  StDetectorId id = static_cast<StDetectorId>(kBarrelEmcTowerId); // 1-oct-2001
  if(StEmcDetector* detector=(StEmcDetector*)emctemp->detector(id)){
    delete detector; 
    detector=0;
  }
  if(mTheEmcReader->NTowerHits() == 0) return kStOK; // Create empty detector entity only

  StEmcGeom *geo = StEmcGeom::getEmcGeom(1);  // Bemc geometry - 1-oct-2001  
  StEmcDetector* detector = new StEmcDetector(id, geo->NModule());

  static UInt_t ADC=0;;
  static StEmcRawHit *hit=0;
  for(UInt_t m=1;m<=120;m++){
    for(UInt_t e=1;e<=20;e++){
      for(UInt_t s=1;s<=2;s++){

        if(mTowerADC[m-1][e-1][s-1]>0){
	  ADC=(UInt_t)(mTowerADC[m-1][e-1][s-1]);
          hit=new StEmcRawHit(id,m,e,s,(UInt_t)ADC);
          detector->addHit(hit);
        }

      }
    }
  }

  emctemp->setDetector(detector);
  printf("StEmcTowerInput::fillEvent() %s #hits %5i \n", 
  detname[0].Data(), detector->numberOfHits());

  return kStOK;
}
