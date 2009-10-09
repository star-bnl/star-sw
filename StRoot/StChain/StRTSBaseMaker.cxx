/***************************************************************************
 *
 * $Id: StRTSBaseMaker.cxx,v 1.8 2009/10/09 20:30:08 fine Exp $
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
 * Revision 1.8  2009/10/09 20:30:08  fine
 * make the global daqReader to be the static data-member to fix   issue #1657
 *
 * Revision 1.7  2009/07/22 21:42:52  fine
 * Add DAQ event header to pass
 *
 * Revision 1.6  2009/04/28 16:31:19  fine
 * downgrade the message level from INFO to DEBUG
 *
 * Revision 1.5  2008/12/03 20:41:00  fine
 * add the DetectorName method to make DAQ_READER happy
 *
 * Revision 1.4  2008/12/02 23:40:08  fine
 * GetNext should  not virtual
 *
 * Revision 1.3  2008/12/02 22:55:14  fine
 * Add a few access  methods
 *
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
 * Add the base maker for all RTS-based makers and template for the TofEventMaker
 *
 * Revision 1.4  2008/01/12 00:22:01  fine
 * Update the test macro
 *
 **************************************************************************/
#include "StRTSBaseMaker.h"

#include "StRtsTable.h"

const char *StRTSBaseMaker::fRTSRootDataset="RTS/"; // The name of the Root dataset

ClassImp(StRTSBaseMaker);
//
// The maker name must mutch the detectot DAQ name
//
//_____________________________________________________________
StRTSBaseMaker::StRTSBaseMaker(const char *detectorName,const char *makerName)
      :StMaker(detectorName), fDaq_Dta(0), fDetectorName(detectorName)
{
  if (makerName && makerName[0]) SetName(makerName);
  LOG_DEBUG << "StRTSBaseMaker::ctor"  << endm;
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
StRtsTable *StRTSBaseMaker::GetNext(const char* bank) 
{
   // Get the next "bank" element
   StRtsTable *daqData = 0;
   if (bank && bank[0]) {
     TString query = DetectorName();
     query += "/"; query += bank;
     daqData =  GetNextDaqElement(query);
   } else {
      LOG_ERROR << "No bank name was provided tp query DAQ data from "
           << DetectorName() << " detector" << endm;
   }
   return daqData;
}

//_____________________________________________________________
StRtsTable *StRTSBaseMaker::GetNextRaw()
{
   // Get "raw" DAQ data assuming the maker name 
   // matches the "detector name"
   return GetNext("raw");
}
//_____________________________________________________________
StRtsTable *StRTSBaseMaker::GetNextAdc()
{
   // Get "adc" DAQ data assuming the maker name 
   // matches the "detector name"
   return GetNext("adc");
}
//_____________________________________________________________
StRtsTable *StRTSBaseMaker::GetNextLegacy()
{
   // Get "legacy" DAQ data assuming the maker name 
   // matches the "detector name"
   return GetNext("legacy");
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
 
//_____________________________________________________________
UInt_t StRTSBaseMaker::Token()  { 
   // current token
   return StRtsTable::Token();
}
//_____________________________________________________________
UInt_t StRTSBaseMaker::Trgcmd()  {
   // current trigger command
   return StRtsTable::Trgcmd();
}

//_____________________________________________________________
UInt_t StRTSBaseMaker::Daqcmd()  {
   // current DAQ command
   return StRtsTable::Daqcmd(); 
}

//_____________________________________________________________
UInt_t StRTSBaseMaker::Trgword()  {
   // the Trigger Word
   return StRtsTable::Trgword(); 
}

//_____________________________________________________________
UInt_t StRTSBaseMaker::Phyword()  {
   // the Physics Word
   return StRtsTable::Phyword(); 
}

//_____________________________________________________________
UInt_t StRTSBaseMaker::Daqbits()  {
   // "offline" bits aka L3 summary...
   return StRtsTable::Daqbits(); 
}

//_____________________________________________________________
UInt_t StRTSBaseMaker::Daqbits_l1() {
     // triggers satisfying l1 
   return StRtsTable::Daqbits_l1();
}

//_____________________________________________________________
UInt_t StRTSBaseMaker::Daqbits_l2() {
    // triggers satisfying l2
   return StRtsTable::Daqbits_l2();
}

//_____________________________________________________________
UInt_t StRTSBaseMaker::Evpgroups()
{
   // evp groups aka L3 summary[2]     
   return StRtsTable::Evpgroups();
}

//_____________________________________________________________
UInt_t StRTSBaseMaker::Detectors()  const {
   // detectors present bit mask according to DAQ!
   return StRtsTable::Detectors();
}

