/***************************************************************************
 *
 * $Id: StRTSBaseMaker.cxx,v 1.2 2008/11/21 18:16:46 fine Exp $
 *
 * Author: Valeri Fine, BNL Feb 2008
 ***************************************************************************
 *
 * Description:  Create the DAQ table from the RTS_READER
 *               StRTSBaseMaker  is a base class for the concrete
 *               StMaker classes that access teh concrete detector
 *               subsystem to fee the suitable offline production
 *               datasets
 * Input:  RTS_Reader
 * Output: daq tables
 *
 ***************************************************************************
 *
 * $Log: StRTSBaseMaker.cxx,v $
 * Revision 1.2  2008/11/21 18:16:46  fine
 * Change the return type of the GetNextDaqElement method to be StRtsTable *
 *
 * Revision 1.1  2008/01/29 15:14:05  fine
 * Introduce the base class to access RTS raw data
 *
 * Revision 1.2  2008/01/29 01:42:48  fine
 * Add the static dara-member impl
 *
 * Revision 1.1  2008/01/25 22:30:23  fine
 * Add the base makr for all RTS-based makers and template for the TofEventMaker
 *
 * Revision 1.4  2008/01/12 00:22:01  fine
 * Update the test macro
  *
 **************************************************************************/
#include "StRTSBaseMaker.h"

#include "StRtsTable.h"

const char *StRTSBaseMaker::fRTSRootDataset="RTS/"; // The name of the Root dataset

ClassImp(StRTSBaseMaker);

//_____________________________________________________________
StRTSBaseMaker::StRTSBaseMaker(const char *name):StMaker(name)
      , fDaq_Dta(0)
{
  LOG_INFO << "StRTSBaseMaker::ctor"  << endm;
}

//_____________________________________________________________
StRTSBaseMaker::~StRTSBaseMaker() 
{ }

//_____________________________________________________________
StRtsTable *StRTSBaseMaker::GetNextDaqElement(const char *elementPath)
{
  // Query:  RTS/tpx/cld[%d] cluster data from DAQ system
  // Return: the pointer the StRtsTable object filled with the query data
  //         = 0; the no data for the "elementPath" was found
  fDaq_Dta = 0;
  if (elementPath && elementPath[0]) {
     TString path = fRTSRootDataset;
     path +=elementPath; 
     fDaq_Dta =  dynamic_cast<StRtsTable *>(GetDataSet((const char*)path));
  }
  return fDaq_Dta;
}

//_____________________________________________________________
Int_t StRTSBaseMaker::Sector () const
{
    return fDaq_Dta ? fDaq_Dta->Sector() : -1;
}
//_____________________________________________________________
Int_t StRTSBaseMaker::Pad () const
{
    return fDaq_Dta ? fDaq_Dta->Pad() : -1;
}

//_____________________________________________________________
Int_t StRTSBaseMaker::Rdo () const
{
    return fDaq_Dta ? fDaq_Dta->Rdo() : -1;
}

//_____________________________________________________________
Int_t StRTSBaseMaker::Row () const
{
    return fDaq_Dta ? fDaq_Dta->Row() : -1;
}

