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
//#include "St_DataSetIter.h"
#include "StEventTypes.h"

//
// Interfaces
//
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"

ClassImp(StEmcHandleInput) // macro
   
//-----------------------------------------------------------------

    StEmcHandleInput::StEmcHandleInput(StEvent*event, StEMCReader* emcreader, TDataSet* calibdb)
      : mevent(event), mTheEmcReader(emcreader),m_calibdb(calibdb)
{}

//-----------------------------------------------------------------

StEmcHandleInput::~StEmcHandleInput() {}

//-----------------------------------------------------------------

Int_t StEmcHandleInput::ProcessInput() {
    cout << "HandleInput::ProcessInput()" << endl;

    //
    if(!mTheEmcReader){
    // case 1 , StEvent exist but no daq
        StEmcnoDaqInput * ndaq = new StEmcnoDaqInput(mevent,mTheEmcReader,m_calibdb);
      int stat=ndaq->ProcessInput();
      if(stat!=kStOK)return kStWarn;
    }
    //case 2 StEvent, emcreader exist
    if(mTheEmcReader){
        if(mTheEmcReader->NTowerHits()>0){
	StEmcTowerInput *tower = new StEmcTowerInput(mevent, mTheEmcReader,m_calibdb);
        int stat=tower->ProcessInput();
        if(stat==kStOK){cout<<"Towerinput OK"<<endl;}
	      }
	      else{
		cout<<" No towers Hit **"<<endl;
	      }

	     if(mTheEmcReader->NSmdHits()>0){
	StEmcSmdInput *smd = new StEmcSmdInput(mevent, mTheEmcReader,m_calibdb);
        int statsmd=smd->ProcessInput();
        if(statsmd==kStOK){cout<<"SmdInput OK"<<endl;}
	      }
	      else{
		cout<<" No SMD strip Hit **"<<endl;
      }
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
