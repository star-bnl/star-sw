/***************************************************************************
 * Author:  Subhasis Chattopadhyay
 ***************************************************************************
 *
 * Description: EMC INput Handling:
 ***************************************************************************/

#include <iostream.h>
#include <fstream.h>

#include "StEmcHandleInput.h"
#include "StEmcnoDaqInput.h"
#include "StEmcTowerInput.h"
#include "StEmcSmdInput.h"
#include "StEventTypes.h"

//
// Interfaces
//
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"

ClassImp(StEmcHandleInput) // macro
   
StEmcHandleInput::StEmcHandleInput(StEvent*event, StEMCReader* emcreader, TDataSet* calibdb)
: mevent(event), mTheEmcReader(emcreader),mCalibDb(calibdb)
{ }

StEmcHandleInput::~StEmcHandleInput() {}

Int_t 
StEmcHandleInput::ProcessInput() 
{
  cout << "HandleInput::ProcessInput()" << endl;

  if(!mTheEmcReader){
    // case 1 , StEvent exist but no daq
    cout<<"ProcessInput::EmcRead does not exist"<<endl;
    StEmcnoDaqInput *ndaq = new StEmcnoDaqInput(mevent,mTheEmcReader,mCalibDb);
    int stat=ndaq->ProcessInput();
    if(stat!=kStOK)return kStWarn;
  }

  if(mTheEmcReader){
    //case 2 StEvent, emcreader exist
    cout<<"ProcessInput::EmcRead "<<endl;

    StEmcTowerInput *tower = new StEmcTowerInput(mevent, mTheEmcReader,mCalibDb);
    int stat=tower->ProcessInput();
    if(stat==kStOK) cout<<"Towerinput OK"<<endl;

    StEmcSmdInput *smd = new StEmcSmdInput(mevent, mTheEmcReader,mCalibDb);
    int statsmd=smd->ProcessInput();
    if(statsmd==kStOK) cout<<"SmdInput OK"<<endl;
  }
//
//    if(ndaq){delete ndaq;ndaq=0;}
//    if(tower){delete tower;tower=0;}
//    if(smd){delete smd;smd=0;}
//
     return kStOK;
}

void  StEmcHandleInput::clear()
{
}
