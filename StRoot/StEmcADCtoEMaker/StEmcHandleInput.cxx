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
#include <StMessMgr.h>

//
// Interfaces
//
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/EMC/EMC_Reader.hh"
#include "StDAQMaker/StDAQReader.h"

ClassImp(StEmcHandleInput) // macro
   
StEmcHandleInput::StEmcHandleInput
(StEmcCollection *emccol, StEMCReader* emcreader,   TDataSet* calibdb)
:mEmcCollection(emccol),  mTheEmcReader(emcreader), mCalibDb(calibdb)
{ }

StEmcHandleInput::~StEmcHandleInput() {}

Int_t 
StEmcHandleInput::processInput() 
{
  gMessMgr->Info()<<"HandleInput::processInput()" << endm;
  static Int_t statTower, statSmd;

  StEmcTowerInput *tower = new StEmcTowerInput(mEmcCollection, mTheEmcReader,mCalibDb);
  statTower = tower->processInput();
  if(statTower == kStOK) gMessMgr->Info()<<"Tower input OK" << endm;

  StEmcSmdInput *smd = new StEmcSmdInput(mEmcCollection, mTheEmcReader,mCalibDb);
  statSmd = smd->processInput();
  if(statSmd == kStOK) gMessMgr->Info()<<"SMD input OK" << endm;
  return (statTower || statSmd);
}

void  StEmcHandleInput::clear()
{
}
