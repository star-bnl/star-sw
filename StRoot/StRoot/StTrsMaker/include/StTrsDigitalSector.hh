/***************************************************************************
 *
 * $Id: StTrsDigitalSector.hh,v 1.13 2008/06/20 15:01:03 fisyak Exp $
 *
 * Author: bl prelim
 ***************************************************************************
 *
 * Description: Store the digital information in a space
 *              efficient manner:
 * Data:
 * TimeBin   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15...
 * ADC Vaue  0  0  0  5  9  4  0  0  0  0  7  9  3  0  0  1...
 *
 * will be stored as an array of short in a compressed manner
 * where the ADC values remain and the number of zeros are compressed
 * and stored as a negative number.  The data above will be stored
 * as:
 *
 *           -3 5 9 4 -4 7 9 3 -2 1....
 *           
 *  This will be the format coming from the DigitalSignalGenerator
 *  NOTE: Encapsulation is broken in this class to allow easy
 *        access to the data via the TPC data reader interface!
 ***************************************************************************
 *
 * $Log: StTrsDigitalSector.hh,v $
 * Revision 1.13  2008/06/20 15:01:03  fisyak
 * move from StTrsData to StTpcRawData
 *
 * Revision 1.12  2007/07/12 20:25:05  fisyak
 * Use StarLogger, use time of flight, fix cluster shape
 *
 * Revision 1.11  2005/09/09 22:12:48  perev
 * Bug fix + IdTruth added
 *
 * Revision 1.9  2005/07/19 22:22:02  perev
 * Bug fix
 *
 * Revision 1.8  2003/12/24 13:44:51  fisyak
 * Add (GEANT) track Id information in Trs; propagate it via St_tpcdaq_Maker; account interface change in StTrsZeroSuppressedReaded in StMixerMaker
 *
 * Revision 1.7  2000/06/23 00:12:24  snelling
 * Removed dependence on local files now pointed to StDbUtilities
 *
 * Revision 1.6  2000/01/10 23:11:31  lasiuk
 * Include MACROS for compatibility with SUN CC5.0
 *
 * Revision 1.5  1999/11/05 22:17:04  calderon
 * Made private copy constructor and operator= in StTrsDigitalSector.
 * Renamed DigitalSignalGenerators: Fast -> Old, Parameterized -> Fast
 * and use new "Fast" as default.
 * Added StTrsZeroSuppressedReader and StTrsZeroSuppressedReader for DAQ type
 * data access.
 *
 * Revision 1.4  1999/10/19 21:17:57  calderon
 * New format of data for the Digital Sector without mZeros.
 * Member functions modified accordingly.  Not tested yet, but
 * committed because cons still can't handle changes in the
 * TRS header files.
 *
 * Revision 1.3  1999/10/11 23:55:10  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 * Revision 1.2  1999/01/22 08:06:03  lasiuk
 * use unsigned char for compatibilty with interface.
 * requires use of two arrays...ugly but fine for now.
 * use of pair<>; values returned by pointer
 *
 * Revision 1.1  1999/01/18 10:23:42  lasiuk
 * initial Revision
 *
 *
 **************************************************************************/
#ifndef ST_TRS_DIGITAL_SECTOR_HH
#define ST_TRS_DIGITAL_SECTOR_HH
#include "StTpcRawData.h"
typedef StTpcDigitalSector StTrsDigitalSector;
#endif
