/***************************************************************************
 *
 * $Id: StRTSBaseMaker.cxx,v 1.15 2012/01/25 23:10:06 genevb Exp $
 *
 * Author: Valeri Fine, BNL Feb 2008
 ***************************************************************************
 *
 * Description:  Create the DAQ table from the RTS_READER
 *               StRTSBaseMaker  is a base class for the concrete
 *               StMaker classes that access the concrete detector
 *               subsystem to feed the suitable offline production
 *               datasets
 * Input:  RTS_Reader
 * Output: daq tables
 *
 ***************************************************************************
 *
 * $Log: StRTSBaseMaker.cxx,v $
 * Revision 1.15  2012/01/25 23:10:06  genevb
 * Move StMaker name logic before StMaker instantiation
 *
 * Revision 1.14  2011/06/20 15:13:51  fisyak
 * Force to call Finish with SIGTERM signal obtained from condor_vacate_job after time limit reached
 *
 * Revision 1.13  2010/03/17 16:01:08  fine
 * RT #1880. Fix the the bug of the assert condition
 *
 * Revision 1.12  2010/03/17 15:58:30  fine
 * RT #1880. Fix the the bug of the assert condition
 *
 * Revision 1.11  2010/02/01 01:46:59  fine
 * RT #1840 Add the method GetNextLegacy(int)
 *
 * Revision 1.10  2009/11/10 19:17:39  fine
 * Add doxygen docs
 *
 * Revision 1.9  2009/11/10 17:41:19  fine
 * remove the compilation warning on SL5
 *
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
//! StRTSBaseMaker( const char *detectorName,TVirtualPad *pad) ctor
/*!
 * \param detectorName - the  DAQ name of the detector as defined by the column \b detector 
 * FROM THE "Table 1" 
 * of the <a href="http://docs.google.com/Doc?docid=dgv8pf9t_60dwhg3zd4&hl=en">"A DAQ_READER Cookbook"</a>
 * \param makerName - The object name. It is adviced to provide each object instance with its own name
 */
//_____________________________________________________________
StRTSBaseMaker::StRTSBaseMaker(const char *detectorName,const char *makerName)
      :StMaker((makerName && makerName[0] ? makerName : detectorName))
      ,fDaq_Dta(0), fDetectorName(detectorName)
{
  LOG_DEBUG << "StRTSBaseMaker::ctor"  << endm;
}

//! StRTSBaseMaker class dtor
//_____________________________________________________________
StRTSBaseMaker::~StRTSBaseMaker() 
{ }

//_____________________________________________________________
//! Query the STAR production chain for the DAQ data
/*! Use the syntax defined by 
    \htmlonly 
      <a href="http://docs.google.com/Doc?docid=dgv8pf9t_60dwhg3zd4&hl=en">A DAQ_READER Cookbook"</a> 
    \endhtmlonly
 *  to query the DAQ data for the current event. 
 *  It generates the text query using the format  \c RTS/detector/bank[%d] \n
 *  where 
 *  \arg \c "RTS" is a text constant \n
 *  \arg \c detector is the name  of the detector defined by the class ctor \sa StRTSBaseMaker::StRTSBaseMaker \n
 *  \arg\c bank is the name of the bank 
 *    as defined by the "DAQ_READER bank name" column from the \b "A DAQ_READER Cookbook" \b "Table 1" 
 *    \sa   \htmlonly 
      <a href="http://docs.google.com/Doc?docid=dgv8pf9t_60dwhg3zd4&hl=en">A DAQ_READER Cookbook"</a> 
      \endhtmlonly
 *  \arg \c [%d] - is an optional component which is a comma separated list of the indexis surrounding by the square brackets.
 *
 *  For example the  text "RTS/tpx/cld[1]" is to query the DAQ cluster data for the first sector of TPX detector
 * \return  the pointer the StRtsTable object filled with the query data\n
         \b =0; no data for the \a "elementPath" was found
 *  \note the methgod is designed to be called from the StMaker::Make implementation indirectly.
 * Normally one does not need to invoke this method from the end-user code.\n
 */
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
//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! 
   \param bank - the name of the bank. \n
     The format is \c bank[%d], where 
     \arg\c bank is the name of the bank 
      as defined by the "DAQ_READER bank name" column from the \b "A DAQ_READER Cookbook" Table 1.
   \arg \c [%d] - is an optional component which is a comma separated list of the indexis surrounding by the square brackets.
   \return the DAQ data for the bank \a "bank" if exists
   \sa \htmlonly 
      <a href="http://docs.google.com/Doc?docid=dgv8pf9t_60dwhg3zd4&hl=en">"A DAQ_READER Cookbook"</a> 
    \endhtmlonly
 */
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
      LOG_ERROR << "No bank name was provided to query DAQ data from "
           << DetectorName() << " detector" << endm;
   }
   return daqData;
}

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! 
   \return the DAQ data for the bank \c "raw" if exists
   \sa \htmlonly 
      <a href="http://docs.google.com/Doc?docid=dgv8pf9t_60dwhg3zd4&hl=en">"A DAQ_READER Cookbook"</a> 
    \endhtmlonly
 */
//_____________________________________________________________
StRtsTable *StRTSBaseMaker::GetNextRaw()
{
   // Get "raw" DAQ data assuming the maker name 
   // matches the "detector name"
   return GetNext("raw");
}

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! 
   \return the DAQ data for the bank \c "raw" from sector \c sec if exists
   \sa \htmlonly 
      <a href="http://docs.google.com/Doc?docid=dgv8pf9t_60dwhg3zd4&hl=en">"A DAQ_READER Cookbook"</a> 
    \endhtmlonly
 */
//_____________________________________________________________
StRtsTable *StRTSBaseMaker::GetNextRaw(int sec)
{
   // Get "raw" DAQ data assuming the maker name 
   // matches the "detector name"
   assert(sec > 0 && "Only positive  value is allowed");
   return GetNext(Form("raw[%i]",sec));
}
//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! 
   \return the DAQ data for the bank \c "adc" if exists
   \sa \htmlonly 
      <a href="http://docs.google.com/Doc?docid=dgv8pf9t_60dwhg3zd4&hl=en">"A DAQ_READER Cookbook"</a> 
    \endhtmlonly
 */
//_____________________________________________________________
StRtsTable *StRTSBaseMaker::GetNextAdc()
{
   // Get "adc" DAQ data assuming the maker name 
   // matches the "detector name"
   return GetNext("adc");
}
//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! 
   \return the DAQ data for the bank \c "adc" from sector \c sec if exists 
   \sa \htmlonly 
      <a href="http://docs.google.com/Doc?docid=dgv8pf9t_60dwhg3zd4&hl=en">"A DAQ_READER Cookbook"</a> 
    \endhtmlonly
 */
//_____________________________________________________________
StRtsTable *StRTSBaseMaker::GetNextAdc(int sec)
{
   // Get "adc" DAQ data assuming the maker name 
   // matches the "detector name"
   assert(sec > 0 && "Only positive  value is allowed");
   return GetNext(Form("adc[%i]",sec));
}
//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! 
   \return the DAQ data for the bank \c "legacy" if exists
   \sa \htmlonly 
      <a href="http://docs.google.com/Doc?docid=dgv8pf9t_60dwhg3zd4&hl=en">"A DAQ_READER Cookbook"</a> 
    \endhtmlonly
 */
//_____________________________________________________________
StRtsTable *StRTSBaseMaker::GetNextLegacy()
{
   // Get "legacy" DAQ data assuming the maker name 
   // matches the "detector name"
   return GetNext("legacy");
}

//__________________________________________________________________________________________
//! This is an overloaded member function, provided for convenience.
/*! 
   \return the DAQ data for the bank \c "legacy" from sector \c sec if exists
   \sa \htmlonly 
      <a href="http://docs.google.com/Doc?docid=dgv8pf9t_60dwhg3zd4&hl=en">"A DAQ_READER Cookbook"</a> 
    \endhtmlonly
 */
//_____________________________________________________________
StRtsTable *StRTSBaseMaker::GetNextLegacy(int sec)
{
   // Get "legacy" DAQ data assuming the maker name 
   // matches the "detector name"
   assert(sec > 0 && "Only positive  value is allowed");
   return GetNext(Form("legacy[%i]",sec));
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

