/***************************************************************************
 *
 * $Id: StEmcnoDaqInput.cxx,v 1.1 2001/07/17 00:14:37 perev Exp $
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

#include "StEmcnoDaqInput.h"
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

ClassImp(StEmcnoDaqInput) // macro

//--------------------------------------------------------------

    StEmcnoDaqInput::StEmcnoDaqInput(const StEvent*event, const StEMCReader* emcreader,const TDataSet* calibdb)
      : mevent(event), mTheEmcReader(emcreader),m_calibdb(calibdb)
{
}

//-----------------------------------------------------------------

StEmcnoDaqInput::~StEmcnoDaqInput() {}

//-----------------------------------------------------------------

Int_t StEmcnoDaqInput::ProcessInput() {
    cout << "noDaqInput::ProcessInput()" << endl;
//Get EmcCollection
    StEmcCollection* emccoll = mevent->emcCollection();
    if(!emccoll){cout<<" NO EMC COLLECTION**, quit"<<endl;return kStWarn;}
    return kStOK;
}

