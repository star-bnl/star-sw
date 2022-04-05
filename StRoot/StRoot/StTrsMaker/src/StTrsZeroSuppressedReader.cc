/***************************************************************************
 *
 * $Id: StTrsZeroSuppressedReader.cc,v 1.14 2008/06/20 15:01:21 fisyak Exp $
 *
 * Authors: bl, mcbs
 ***************************************************************************
 *
 * Description: Access to the digital information via the abstract
 *              interface...this is the OLD UNPACKER
 ***************************************************************************
 *
 * $Log: StTrsZeroSuppressedReader.cc,v $
 * Revision 1.14  2008/06/20 15:01:21  fisyak
 * move from StTrsData to StTpcRawData
 *
 * Revision 1.13  2005/09/09 22:12:49  perev
 * Bug fix + IdTruth added
 *
 * Revision 1.11  2005/07/19 22:23:05  perev
 * Bug fix
 *
 * Revision 1.10  2003/12/24 13:44:54  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.9  2003/09/19 22:17:50  jecc
 * Fix bug in getSequences intruced during gcc 3.2 updates
 *
 * Revision 1.8  2003/09/02 17:59:19  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.7  2000/06/07 02:03:12  lasiuk
 * exit/abort ultimatum
 *
 * Revision 1.6  2000/03/15 23:33:55  calderon
 * Remove extra messages
 *
 * Revision 1.5  2000/03/15 18:08:56  calderon
 * ZSR is no longer a singleton.  Two will be needed for mixer chain.
 *
 * Revision 1.4  2000/03/14 01:00:24  calderon
 * remove annoying beep
 *
 * Revision 1.3  2000/01/10 23:14:31  lasiuk
 * Include MACROS for compatiblity with SUN CC5
 *
 * Revision 1.2  1999/12/08 02:10:43  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.1  1999/11/05 22:18:17  calderon
 *
 * Made private copy constructor and operator= in StTrsDigitalSector.
 * Renamed DigitalSignalGenerators: Fast -> Old, Parameterized -> Fast
 * and use new "Fast" as default.
 * Added StTrsZeroSuppressedReader and StTrsZeroSuppressedReader for DAQ type
 * data access.
 * Removed vestigial for loop in sampleAnalogSignal() method.
 * Write version of data format in .trs data file.
 *
 ***************************************************************************/
#include "StTrsZeroSuppressedReader.hh"
#include <assert.h>
#include <algorithm>
#include <iterator>
#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::find;
using std::distance;
#endif
#ifndef ST_NO_EXCEPTIONS
#   include <stdexcept>
#   if !defined(ST_NO_NAMESPACES)
        using std::out_of_range;
#   endif
#endif

#include "StGlobals.hh"
#include "StTrsRawDataEvent.hh"
//________________________________________________________________________________
StTrsZeroSuppressedReader::StTrsZeroSuppressedReader(StTpcRawData* ev) :mSector(-1), mTheSector(0), mTrsEvent(0) {
  
  assert(sizeof(StSequence) == sizeof(Sequence)); 
  if (ev) {
    mTrsEvent = dynamic_cast<StTrsRawDataEvent*>(ev);
    if (!mTrsEvent) {
      cerr << "Error constructing StTrsZeroSuppressedReader" << endl;
      cerr << "Cast Failed! ev not of required type (StTrsRawDataEvent*)\n";
    }
  }
}
//________________________________________________________________________________
Int_t StTrsZeroSuppressedReader::setSector(Int_t sector)
{
  //    clear();
    // Check if the data is there --> getSector() in the Unpacker!
    // ...you may even want to check the mVersion to return the
    // proper mZSR...
    
    if(checkTheData(sector)) {
	    mSector = sector;
	    mTheSector = mTrsEvent->getSector(sector);
	    return 1;
    }
    return 0;
}
//________________________________________________________________________________
Int_t StTrsZeroSuppressedReader::checkTheData(UInt_t which)
{
    Int_t status(0);
    
    if(mTrsEvent->size() >= which)  // bounds check
      if ( (mTrsEvent->getSector(which)) ) status = 1;
    return status;   
}
