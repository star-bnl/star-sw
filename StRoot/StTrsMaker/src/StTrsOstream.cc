/***************************************************************************
 *
 * $Id: StTrsOstream.cc,v 1.1 1999/10/11 23:55:23 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez 
 ***************************************************************************
 *
 * Description: Ostream class for
 *              writing TRS data.
 ***************************************************************************
 *
 * $Log: StTrsOstream.cc,v $
 * Revision 1.1  1999/10/11 23:55:23  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 *
 *
 **************************************************************************/
#include <utility>
#include <time.h>
#include <sys/utsname.h>
#include "StTrsOstream.hh"
#include "StTrsRawDataEvent.hh"
#include "StTrsDigitalSector.hh"
#include "StTpcGeometry.hh"

#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<int> intVec;
typedef vector<unsigned char> digitalTimeBins;
typedef istream_iterator<unsigned char> istream_iter_uns_char;
typedef ostream_iterator<int> ostream_iter_int;


#else
typedef vector<int, allocator<int> > intVec;
typedef vector<unsigned char, allocator<unsigned char> > digitalTimeBins;
typedef istream_iterator<unsigned char,ptrdiff_t> istream_iter_uns_char;
typedef ostream_iterator<int> ostream_iter_int;

#endif

int getInt2(unsigned char a) { return static_cast<int>(a);}

StTrsOstream::StTrsOstream(string streamName, int numberOfEvents, StTpcGeometry* dB)
{
    mGeomDb = dB;
    mEvents = numberOfEvents;
    mSectors = mGeomDb->numberOfSectors();
    mRows    = mGeomDb->numberOfRows();
    padsAtRow.resize(mRows);
    for (int k=0; k<mRows; k++) padsAtRow[k]=mGeomDb->numberOfPadsAtRow(k+1);
    
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
    ofs << "# Tpc Response Simulator Data File" << endl;
    ofs << "# Created: " << ctime(&now);
    ofs << "# System Information ------" << endl;
    ofs << "# Operating System: " << sysinfo.sysname << " " << sysinfo.release << " " << sysinfo.version << endl;
    ofs << "# Number of events " << mEvents << endl;
    ofs << "# Geometry Information ----" << endl;
    ofs << "# sectors " << mSectors << endl;
    ofs << "# rows " << mRows << endl;
    ofs << "# pads ";
    for (int j=0; j<padsAtRow.size(); j++) ofs << padsAtRow[j] << " ";
    ofs << endl;
    ofs << "##" << endl;

}

StTrsOstream::~StTrsOstream(){ofs.close();}

void StTrsOstream::writeTrsEvent(StTrsRawDataEvent* EventData)
{
    cout << "Writing Output to file " << endl;
    // Write output
    for (int iSector = 0; iSector < mSectors; iSector++) {
	if(EventData->mSectors[iSector]) { // Make sure the sector has data
	    StTrsDigitalSector* aDigitalSector = EventData->mSectors[iSector];
	    if (aDigitalSector->cleanup()) 
		cout << "Sector " << iSector << " has no data. Skip it." << endl;
	    else {
		ofs << static_cast<unsigned short>(iSector) << " ";
		
		for (int iRow = 0; iRow < mRows; iRow++) {
		    if (aDigitalSector->mData[iRow].size()>0) { //Make sure the row has data
			ofs << static_cast<unsigned short>(iRow) << " ";
			//PR(iRow);
			for (int iPad = 0; iPad < padsAtRow[iRow]; iPad++) {
			    int lengthData = aDigitalSector->mData[iRow][iPad].size();
			    if (lengthData){ // Make sure the pad has data
				ofs << static_cast<unsigned short>(iPad) << " ";
				
// 				ofs << static_cast<unsigned short>(lengthData);
				
// 				ofs.write(static_cast<const unsigned char*>(aDigitalSector->mData[iRow][iPad].begin()),lengthData);
// 				ofs.write(static_cast<const unsigned char*>(aDigitalSector->mZeros[iRow][iPad].begin()),lengthData);
// 				ofs << " ";
				//
				// Transform this into output format
				//
				digitalTimeBins::iterator dataBegin  = aDigitalSector->mData[iRow][iPad].begin();
				digitalTimeBins::iterator dataEnd    = aDigitalSector->mData[iRow][iPad].end();
				digitalTimeBins::iterator rangeBegin = dataBegin;
				unsigned int nZeros = count(rangeBegin,dataEnd,static_cast<unsigned char>(0));
				digitalTimeBins outputData(lengthData+nZeros);
				digitalTimeBins::iterator currentPos = outputData.begin();

				while (rangeBegin!=dataEnd && currentPos!=outputData.end()) {
				    digitalTimeBins::iterator rangeEnd = find(rangeBegin,dataEnd,static_cast<unsigned char>(0));
				    copy(rangeBegin,rangeEnd,currentPos);
				    currentPos+=distance(rangeBegin,rangeEnd);
				    if (*rangeEnd == static_cast<unsigned char>(0)) {
					unsigned int index = distance(dataBegin, rangeEnd);
					*currentPos++ = static_cast<unsigned char>(0);
					*currentPos++ = aDigitalSector->mZeros[iRow][iPad][index];
					
				    }
				    rangeBegin = (rangeEnd==dataEnd) ?  dataEnd : rangeEnd+1;
				}
								
				//
				// Write the Output vector
				//
				
				ofs << static_cast<unsigned short>(outputData.size());
				
				ofs.write(static_cast<const unsigned char*>(outputData.begin()),outputData.size());
				ofs << " ";
				
// 			    PR(iPad);
// 			    PR(lengthData);
// 			    intVec DataOut(lengthData);
// 			    transform (aDigitalSector->mData[iRow][iPad].begin(), aDigitalSector->mData[iRow][iPad].end(), DataOut.begin(), getInt2);
// 			    cout << "Read Data" << endl;
// 			    copy (DataOut.begin(), DataOut.end(), ostream_iter_int(cout, " "));
// 			    cout << endl;

			    } // if pad has data
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
