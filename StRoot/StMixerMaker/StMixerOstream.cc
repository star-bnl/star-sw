/***************************************************************************
 *
 * $Id: StMixerOstream.cc,v 1.1 2000/02/16 21:02:10 pfachini Exp $
 *
 * Author: Patricia Fachini - a copy from StTrsOstream.cc
 *
 ***************************************************************************
 *
 * Description: Ostream class for
 *              writing Mixed data.
 *
 ***************************************************************************
 *
 **************************************************************************/
#include <utility>
#include <time.h>
#include <sys/utsname.h>
#include "StMixerOstream.hh"
#include "StMixerDataEvent.hh"
#include "StMixerDigitalSector.hh"
#include "StTrsMaker/include/StTpcGeometry.hh"

#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::transform;
#endif

#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<int> intVec;
typedef vector<unsigned char> digitalTimeBins;
#if !defined (__SUNPRO_CC)  //__SUNPRO_CC >= 0x500
typedef istream_iterator<unsigned char> istream_iter_uns_char;
typedef ostream_iterator<int> ostream_iter_int;
#endif

#else
typedef vector<int, allocator<int> > intVec;
typedef vector<unsigned char, allocator<unsigned char> > digitalTimeBins;
#if !defined __SUNPRO_CC >= 0x500
typedef istream_iterator<unsigned char,ptrdiff_t> istream_iter_uns_char;
typedef ostream_iterator<int> ostream_iter_int;
#endif
#endif

int getInt2(unsigned char a) { return static_cast<int>(a);}

StMixerOstream::StMixerOstream(string streamName, int numberOfEvents, StTpcGeometry* dB)
{
    mGeomDb = dB;
    mEvents = numberOfEvents;
    mSectors = mGeomDb->numberOfSectors();
    mRows    = mGeomDb->numberOfRows();
    padsAtRow.resize(mRows);
    for (unsigned int k=0; k<mRows; k++) padsAtRow[k]=mGeomDb->numberOfPadsAtRow(k+1);
    
    //
    // Open the specified file
    //
    ofs.open(streamName.c_str());
    assert(ofs);

    //
    // Write Header
    //
    time_t now = time(0);
    struct utsname sysinfo;
    uname(&sysinfo);
    ofs << "# Mixed (TRS+DAQ) Data File" << endl;
    ofs << "# Created: " << ctime(&now);
    ofs << "# version MixerDatav1.0" << endl;
    ofs << "# Operating System: " << sysinfo.sysname << " " << sysinfo.release << " " << sysinfo.version << endl;
    ofs << "# Number of events " << mEvents << endl;
    ofs << "# Geometry Information ----" << endl;
    ofs << "# sectors " << mSectors << endl;
    ofs << "# rows " << mRows << endl;
    ofs << "# pads ";
    for (unsigned int j=0; j<padsAtRow.size(); j++) ofs << padsAtRow[j] << " ";
    ofs << endl;
    ofs << "##" << endl;

}

StMixerOstream::~StMixerOstream(){ofs.close();}

void StMixerOstream::writeMixerEvent(StMixerDataEvent* EventData)
{
    cout << "Writing Output to file " << endl;
    // Write output
    unsigned int iSector;
    for (iSector = 0; iSector < mSectors; iSector++) { // sector loop
	if(EventData->mSectors[iSector]) { // Make sure the sector has data
	    StMixerDigitalSector* aDigitalSector = EventData->mSectors[iSector];
	    cout << "Cleanup of Sector " << iSector+1 << endl;	
	    if (aDigitalSector->cleanup())
		cout << "Sector " << iSector+1 << " has no data. Skip it." << endl;
	    else { // sector has data, write it out
		ofs << static_cast<unsigned short>(iSector) << " ";
		cout << "Writing Sector " << iSector+1 << endl;
		unsigned int iRow;
		for (iRow = 0; iRow < mRows; iRow++) { // row loop
		    if (aDigitalSector->mData[iRow].size()>0) { //Make sure the row has data
			ofs << static_cast<unsigned short>(iRow) << " ";
			//cout << endl;
			//PR(iRow);
			int iPad;
			for (iPad = 0; iPad < padsAtRow[iRow]; iPad++) { // pad loop
			    //PR(iPad);
			    int lengthData = aDigitalSector->mData[iRow][iPad].size();
			    //PR(lengthData);
			    if (lengthData){ // Make sure the pad has data
								
				ofs << static_cast<unsigned short>(iPad) << " ";
								
				ofs << static_cast<unsigned short>(lengthData);
				ofs.put(' ');
				ofs.write(static_cast<const unsigned char*>(&(aDigitalSector->mData[iRow][iPad][0])),lengthData);
				ofs << " ";

				if (false) { // to debug, change to true
				intVec DataOut(lengthData);
				transform (aDigitalSector->mData[iRow][iPad].begin(), aDigitalSector->mData[iRow][iPad].end(), DataOut.begin(), getInt2);
#if !defined (__SUNPRO_CC) //&& __SUNPRO_CC >= 0x500
				copy (DataOut.begin(), DataOut.end(), ostream_iter_int(cout, " "));
#endif
				cout << endl;
				cout << "Wrote..." << endl;
				}
				
			    } // if pad has data
			    else ofs << " ";
			} // Pads
			ofs << static_cast<unsigned short>(iPad) << " " ;
		    } // if row has data
		} // Rows
		ofs << static_cast<unsigned short>(iRow) << " " ;
	    } // else, write sector
	} //if sector has data
    } // Sectors
    ofs << static_cast<unsigned short>(iSector) << " " ;
}
