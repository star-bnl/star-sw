/***************************************************************************
 * Author:  Subhasis Chattopadhyay
 ***************************************************************************
 *
 * Description: EMC Smd Handling:
 ***************************************************************************/

#include <iostream.h>
#include <fstream.h>

#include "StEmcADCtoEMaker.h"
#include "StEmcSmdInput.h"
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

ClassImp(StEmcSmdInput) // macro

Short_t deductPedBsmd;

StEmcSmdInput::StEmcSmdInput
(StEmcCollection *emccol, StEMCReader* emcreader,   TDataSet* calibdb)
:mEmcCollection(emccol),  mTheEmcReader(emcreader), mCalibDb(calibdb)
{}

StEmcSmdInput::~StEmcSmdInput() {}

Int_t 
StEmcSmdInput::processInput() {
  cout << "StEmcSmdInput::processInput()" << endl;
  static unsigned short ADC;
  static Int_t stat;

  //Initialize SMD arrays
  // SMDE
  for(Int_t i=0;i<120;i++){
    for(Int_t j=0;j<150;j++){
      stat = mTheEmcReader->getSMDE_ADC(i,j,ADC);
      if(stat) mSMDEADC[i][j] = (Float_t)(ADC);
      else     mSMDEADC[i][j] = 0.;
    }
  }

  //SMDP
  for(Int_t i=0;i<120;i++){
    for(Int_t j=0;j<10;j++){
      for(Int_t k=0;k<15;k++){
        stat = mTheEmcReader->getSMDP_ADC(i,j,k,ADC);
        if(stat) mSMDPADC[i][j][k] = (Float_t)(ADC);
        else     mSMDPADC[i][j][k] = 0.;
      }
    }
  }

  StEmcHandleDB *db = 0;

  controlADCtoE_st *tmpTab = StEmcADCtoEMaker::getControlTable();  
  deductPedBsmd = tmpTab->bsmdDeductPedestal;

  //
  //First , Pedestals
  //Check if the data is pedestal subtracted, otherwisecheck if
  //pedestal tables exist in DB
  static int stat_ped, stat_fill;
  if(deductPedBsmd) {
    //Get DB
    db = new StEmcHandleDB(mCalibDb);
    stat_ped = subtractPedestals(db);
  }
  //Apply Equalization consts
  //    int stat_eual = applyAmpEqualization(db);
  //Perform eta_correction
  //    int stat_eta = applyEtaCorrection(db);
  //Write into StEvent
  stat_fill = fillEmcHitsCollection();
  if(stat_fill==kStOK) return kStOK;
  return kStOK;
}

Int_t 
StEmcSmdInput::subtractPedestals(StEmcHandleDB *db)
{
  // subtract pedestals

  // Get pdestal tables from mCalibDb
  //If peds table absent then return kStErr
  //SMDE
  static Float_t ped=0;
  if(deductPedBsmd) {
    for(Int_t i=0;i<120;i++){
      for(Int_t j=0;j<150;j++){
        Int_t pedstat=db->getSmdEPeds(i,j,ped);
        if(pedstat==kStOK) mSMDEADC[i][j]-=ped;
      }
    }
  }

//SMDP
  if(deductPedBsmd) {
    for(Int_t i=0;i<120;i++){
      for(Int_t j=0;j<10;j++){
        for(Int_t k=0;k<15;k++){
          Int_t pedstat=db->getSmdPPeds(i,j,k,ped);
          if(pedstat==kStOK)mSMDPADC[i][j][k]-=ped;
        }
      }
    }
  }
 return kStOK;
}

Int_t 
StEmcSmdInput::applyAmpEqualization(StEmcHandleDB* db)
{
  // Apply Equalization
  //If equalization table is absent then return kStErr
  //SMDE
  static Float_t equal=1;
  for(Int_t i=0;i<120;i++){
    for(Int_t j=0;j<150;j++){
      Int_t pedstat=db->getSmdEPeds(i,j,equal);
      if(pedstat==kStOK)mSMDEADC[i][j]*=equal;
    }
  }

//SMDP
  for(Int_t i=0;i<120;i++){
    for(Int_t j=0;j<10;j++){
      for(Int_t k=0;k<15;k++){
        Int_t pedstat=db->getSmdPPeds(i,j,k,equal);
        if(pedstat==kStOK)mSMDPADC[i][j][k]*=equal;
      }
    }
  }
  return kStOK;
}

Int_t 
StEmcSmdInput::applyEtaCorrection(StEmcHandleDB* db)
{
  return kStOK;
}

Int_t 
StEmcSmdInput::fillEmcHitsCollection()
{
  static UInt_t ADC=0;
  static StEmcRawHit *hit=0;
  for(Int_t det=3;det<=4;det++){

    if(mTheEmcReader->NSmdHits() > 0) {
      StEmcGeom *geo = StEmcGeom::getEmcGeom(det);  // smd geometry
      StDetectorId id = static_cast<StDetectorId>((det-1)+kBarrelEmcTowerId);
      StEmcDetector* detector = new StEmcDetector(id, geo->NModule());

      for(Int_t m=1; m<=geo->NModule(); m++){
        for(Int_t e=1; e<=geo->NEta(); e++){
          for(Int_t s=1; s<=geo->NSub(); s++) {

            if(det==3) ADC=(UInt_t)(mSMDEADC[m-1][e-1]); 
            if(det==4) ADC=(UInt_t)(mSMDPADC[m-1][e-1][s-1]); 
            if(ADC>0){
              hit = new StEmcRawHit(id,m,e,s,(UInt_t)ADC);
              detector->addHit(hit);
            }
          }
        }
      }
      mEmcCollection->setDetector(detector);
      printf("StEmcSmdInput::fillEmcHitsCollection() %s #hits %5i \n", 
      detname[det-1].Data(), detector->numberOfHits());
    } else printf("StEmcSmdInput::fillEmcHitsCollection() %s => NO HITS \n",
      detname[det-1].Data());
  }
  return kStOK;
}





