/***************************************************************************
 * Author:  Subhasis Chattopadhyay
 ***************************************************************************
 *
 * Description: EMC Smd Handling:
 ***************************************************************************/

#include <iostream.h>
#include <fstream.h>

#include "StEmcSmdInput.h"
//#include "St_DataSetIter.h"
#include "StEventTypes.h"
#include "StEmcHandleDB.h"

//
// Interfaces
//
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"

ClassImp(StEmcSmdInput) // macro

//--------------------------------------------------------------

    StEmcSmdInput::StEmcSmdInput(StEvent*event, StEMCReader* emcreader,TDataSet* calibdb)
      : mevent(event), mTheEmcReader(emcreader),m_calibdb(calibdb)
{}

//-----------------------------------------------------------------

StEmcSmdInput::~StEmcSmdInput() {}

//-----------------------------------------------------------------

Int_t StEmcSmdInput::ProcessInput() {
    cout << "TowerInput::ProcessInput()" << endl;
    //Initialize SMD arrays

for(Int_t i=0;i<120;i++){
  for(Int_t j=0;j<150;j++){
    unsigned short ADC=0;
      m_SMDEADC[i][j]=0.;
      int stat=mTheEmcReader->getSMDE_ADC(i,j,ADC);
      m_SMDEADC[i][j]=(Float_t)(ADC);
      if(!stat)cout<<"problem in getting SMDE_ADC**"<<endl;
    }
  }

//SMDP
for(Int_t i=0;i<120;i++){
  for(Int_t j=0;j<10;j++){
    for(Int_t k=0;k<15;k++){
      m_SMDPADC[i][j][k]=0.;
      unsigned short ADC=0;
      int stat=mTheEmcReader->getSMDP_ADC(i,j,k,ADC);
      m_SMDPADC[i][j][k]=(Float_t)(ADC);
      if(!stat)cout<<"problem in getting SMDP_ADC**"<<endl;
    }
  }
}
//Get DB
 StEmcHandleDB * db=new StEmcHandleDB(m_calibdb);

    //
    //First , Pedestals
    //Check if the data is pedestal subtracted, otherwisecheck if
    //pedestal tables exist in DB
    int stat_ped = subtract_pedestals(db);
    //Apply Equalization consts
    int stat_eual = Apply_amp_equalization(db);
    //Perform eta_correction
    int stat_eta = Apply_etaCorrection(db);
    //Write into StEvent
    int stat_fill = fillevent();
    if(stat_fill==kStOK)return kStOK;
    return kStOK;
}
//////////////////////////////////////////////////
Int_t StEmcSmdInput::subtract_pedestals(StEmcHandleDB *db)
{
  // subtract pedestals

  // Get pdestal tables from m_calibdb
  //If peds table absent then return kStErr
  //SMDE
for(Int_t i=0;i<120;i++){
  for(Int_t j=0;j<150;j++){
      Float_t ped=0;
      Int_t pedstat=db->GetSmdEPeds(i,j,ped);
      if(pedstat==kStOK)m_SMDEADC[i][j]-=ped;
    }
  }

//SMDP
for(Int_t i=0;i<120;i++){
  for(Int_t j=0;j<10;j++){
    for(Int_t k=0;k<15;k++){
      Float_t ped=0.;
      Int_t pedstat=db->GetSmdPPeds(i,j,k,ped);
      if(pedstat==kStOK)m_SMDPADC[i][j][k]-=ped;
    }
  }
}
 return kStOK;
}
////////////////////////////////////////////////////
Int_t StEmcSmdInput::Apply_amp_equalization(StEmcHandleDB* db)
{
  // Apply Equalization
  //If equalization table is absent then return kStErr
  //SMDE
for(Int_t i=0;i<120;i++){
  for(Int_t j=0;j<150;j++){
      Float_t equal=1;
      Int_t pedstat=db->GetSmdEPeds(i,j,equal);
      if(pedstat==kStOK)m_SMDEADC[i][j]*=equal;
    }
  }

//SMDP
for(Int_t i=0;i<120;i++){
  for(Int_t j=0;j<10;j++){
    for(Int_t k=0;k<15;k++){
      Float_t equal=1.;
      Int_t pedstat=db->GetSmdPPeds(i,j,k,equal);
      if(pedstat==kStOK)m_SMDPADC[i][j][k]*=equal;
    }
  }
}
 return kStOK;
}
/////////////////////////////////////////
Int_t StEmcSmdInput::Apply_etaCorrection(StEmcHandleDB* db)
{return kStOK;
}

/////////////////////////////////////////////
Int_t StEmcSmdInput::fillevent()
{

       StEmcCollection * emctemp=0;
       emctemp=mevent->emcCollection(); 
 if(!emctemp){
   cout<<" Emc Collection does not exist so Create it "<<endl;
   emctemp=new StEmcCollection();
    mevent->setEmcCollection(emctemp);
 }
  UInt_t eta[]={20,20,150,10};
  UInt_t sub[]={2,2,1,15};
    for(Int_t det=3;det<=4;det++)
  {
    StDetectorId id = static_cast<StDetectorId>((det-1)+kBarrelEmcTowerId);
    StEmcDetector* detector = new StEmcDetector(id,120);

    for(UInt_t m=1;m<=120;m++)
      for(UInt_t e=1;e<=eta[det-1];e++)
        for(UInt_t s=1;s<=sub[det-1];s++)
        {
          UInt_t ADC=0;
          if(det==3) ADC=(UInt_t)(m_SMDEADC[m-1][e-1]); 
          if(det==4) ADC=(UInt_t)(m_SMDPADC[m-1][e-1][s-1]); 
          if(ADC>0)
          {
            StEmcRawHit* hit=new StEmcRawHit(id,m,e,s,(UInt_t)ADC);
            detector->addHit(hit);
          }
        }
 
    emctemp->setDetector(detector);
  }
return kStOK;
}





