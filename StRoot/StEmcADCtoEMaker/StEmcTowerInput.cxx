/***************************************************************************
 *
 * $Id: StEmcTowerInput.cxx,v 1.1 2001/07/17 00:14:37 perev Exp $
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

#include "StEmcTowerInput.h"
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

ClassImp(StEmcTowerInput) // macro

St_emcCalibration*   cal;
//StEmcHandleDB* db;
   
//--------------------------------------------------------------

    StEmcTowerInput::StEmcTowerInput(const StEvent*event, const StEMCReader* emcreader,const TDataSet* calibdb)
      : mevent(event), mTheEmcReader(emcreader),m_calibdb(calibdb)
{
}

//-----------------------------------------------------------------

StEmcTowerInput::~StEmcTowerInput() {}

//-----------------------------------------------------------------

Int_t StEmcTowerInput::ProcessInput() {
    cout << "TowerInput::ProcessInput()" << endl;

  // Initialize tower ADC array
for(Int_t i=0;i<120;i++){
  for(Int_t j=0;j<20;j++){
    for(Int_t k=0;k<2;k++){
      m_TowerADC[i][j][k]=0.;
      unsigned short ADC=0;
      int stat=mTheEmcReader->getTowerADC(i,j,k,ADC);
      m_TowerADC[i][j][k]=(Float_t)(ADC);
      if(!stat)cout<<" problem in getting tower ADC**"<<endl;
    }
  }
}
//Get DB
 cout<<"Getting DB"<<endl;
    StEmcHandleDB * db=new StEmcHandleDB(m_calibdb);
    db->ProcessDB();
    cout<<"DB handled"<<endl;
    //
    //First , Pedestals
    //Check if the data is pedestal subtracted, otherwisecheck if
    //pedestal tables exist in DB
    cout<<"subtract peds **"<<endl;
    Int_t stat_ped = subtract_pedestals(db);
    cout<<"peds subtracted **"<<endl;
    //Apply Equalization consts
//    Int_t stat_eual = Apply_equalization(db);
    //Write into StEvent
    Int_t stat_fill = fillevent();
    if(stat_fill==kStOK)return kStOK;
    delete db;db=0;
    return kStOK;
}

//////////////////////////////////////////////////
Int_t StEmcTowerInput::subtract_pedestals(StEmcHandleDB* db)
{
  // subtract pedestals
  //If peds table absent then return kStErr
for(Int_t i=0;i<120;i++){
  for(Int_t j=0;j<20;j++){
    for(Int_t k=0;k<2;k++){
      Float_t ped=0;
      cout<<"peds***get tower peds***"<<endl;
            int pedstat=db->GetTowerPeds(i,j,k,ped);
      cout<<"peds***obtained***"<<endl;
      if(pedstat==kStOK)m_TowerADC[i][j][k]-=ped;
    }
  }
}
 return kStOK;
}
////////////////////////////////////////////////////
Int_t StEmcTowerInput::Apply_equalization(StEmcHandleDB* db)
{
  // Apply Equalization
  //If equalization table is absent then return kStErr
for(Int_t i=0;i<120;i++){
  for(Int_t j=0;j<20;j++){
    for(Int_t k=0;k<2;k++){
      Float_t equal=1.;
      //      int pedstat=db->GetTowerEquals(i,j,k,equal);
      m_TowerADC[i][j][k]*=equal;
    }
  }
}
 return kStOK;
}

/////////////////////////////////////////////
Int_t StEmcTowerInput::fillevent()
{
  // Fill StEvent
       StEmcCollection * emctemp=0;
       emctemp=mevent->emcCollection(); 
 if(!emctemp){
   cout<<" Emc Collection does not exist so Create it "<<endl;
   emctemp=new StEmcCollection();
cout<<"emcCollection created**"<<endl; 
   mevent->setEmcCollection(emctemp);
cout<<"emccollection SET**"<<endl;
 }

    StDetectorId id = static_cast<StDetectorId>(1+kBarrelEmcTowerId);
    if(StEmcDetector* detector=(StEmcDetector*)emctemp->detector(id)){delete detector; detector=0;}

    StEmcDetector* detector = new StEmcDetector(id,120);

    for(UInt_t m=1;m<=120;m++){
      for(UInt_t e=1;e<=20;e++){
        for(UInt_t s=1;s<=2;s++)
        {
          if(m_TowerADC[m-1][e-1][s-1]>0)
          {
	    UInt_t ADC=(UInt_t)(m_TowerADC[m-1][e-1][s-1]);
            StEmcRawHit* hit=new StEmcRawHit(id,m,e,s,(UInt_t)ADC);
            detector->addHit(hit);
          }
        }
      }
    }
    emctemp->setDetector(detector);
return kStOK;
}
