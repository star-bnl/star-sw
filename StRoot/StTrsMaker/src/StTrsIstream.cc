/***************************************************************************
 *
 * $Id: StTrsIstream.cc,v 1.4 1999/10/22 00:00:14 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez 
 ***************************************************************************
 *
 * Description: Istream class for
 *              reading TRS data.
 ***************************************************************************
 *
 * $Log: StTrsIstream.cc,v $
 * Revision 1.4  1999/10/22 00:00:14  calderon
 * -added macro to use Erf instead of erf if we have HP and Root together.
 * -constructor with char* for StTrsDedx so solaris doesn't complain
 * -remove mZeros from StTrsDigitalSector.  This causes several files to
 *  be modified to conform to the new data format, so only mData remains,
 *  access functions change and digitization procedure is different.
 *
 * Revision 1.3  1999/10/12 01:39:55  calderon
 * The <algorithm> header wasn't missing, it
 * was the STL distance() function in ObjectSpace
 * that had a different call than in Linux and HP
 * (as usual).  It only has the version that
 * takes 3 arguments, not 2.
 *
 * Revision 1.2  1999/10/12 00:07:25  calderon
 * Add missing <algorithm> include.
 *
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
#include "StTrsIstream.hh"
#include "StTrsRawDataEvent.hh"
#include "StTrsDigitalSector.hh"
#include "StTpcDbGeometry.hh"

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

int getInt(unsigned char a) { return static_cast<int>(a);}

StTrsIstream::StTrsIstream(string streamName, StTpcGeometry* dB)
{
    //
    // Open the specified file
    //
    ifs.open(streamName.c_str());
    assert(ifs);

    //
    // Read Header
    //
    string oneWord;
    const string eohMarker = "##";
    while (oneWord != eohMarker) {
	ifs >> oneWord;
	if (oneWord == string("events")) {
	    ifs >> mEvents;
	}
	if (oneWord == string("sectors")) {
	    ifs >> mSectors;
	}
	if (oneWord == string("rows")) {
	    ifs >> mRows;
	    padsAtRow.resize(mRows);
	}
	if (oneWord == string("pads")) {
	    for(int m=0; m<mRows; m++) ifs >> padsAtRow[m];
	}
    }
    cout << "# of events  read from file = " << mEvents << endl;
    cout << "# of sectors read from file = " << mSectors << endl;
    cout << "# of rows    read from file = " << mRows << endl;
    // Finished Reading Header
    mGeomDb = dB;
}

StTrsIstream::~StTrsIstream() {ifs.close();}

void StTrsIstream::fillTrsEvent(StTrsRawDataEvent* EventData)
{
    if (ifs.eof()) {
	cerr << "End of file Reached" << endl;
	cerr << "This file has only " << mEvents << " events, make sure you don't request more!" << endl;
	exit(1);
    }
    cout << "Filling Tpc Info from File ... " << endl; 
    unsigned short lengthData;
    unsigned short currentSectorNum;
    unsigned short currentRowNum;
    unsigned short currentPadNum;
    ifs >> currentSectorNum;
    while (currentSectorNum < static_cast<unsigned short>(mSectors)) {
	//PR(currentSectorNum);
	StTrsDigitalSector* aDigitalSector = new StTrsDigitalSector(mGeomDb);

	digitalTimeBins digitalPadData;
	
	ifs >> currentRowNum;
	while (currentRowNum < static_cast<unsigned short>(mRows)) {
	    //PR(currentRowNum);
	    
	    aDigitalSector->mData[currentRowNum].resize(padsAtRow[currentRowNum]);
	    
	    ifs >> currentPadNum;
	    while (static_cast<unsigned int>(currentPadNum) < padsAtRow[currentRowNum]) {
		
		//PR(currentPadNum);
		ifs >> lengthData;
		//PR(lengthData);
		digitalPadData.clear();
		if (lengthData>0) { // We have data, read it in
		    digitalPadData.resize(lengthData);
		    ifs.read(static_cast<unsigned char*>(digitalPadData.begin()), lengthData);
		}
		aDigitalSector->assignTimeBins(currentRowNum+1,currentPadNum+1,&digitalPadData);
		ifs >> currentPadNum;
	    } // while currentPadNum is not = padsAtRow[currentRowNum] 
	    ifs >> currentRowNum;
	} // while currentRowNum is not = mRows
	EventData->mSectors[currentSectorNum] = aDigitalSector;
	ifs >> currentSectorNum;
    } // While currentSectorNum is not = mSectors

    // See what we read
    bool PrintToScreen = false;
    if (PrintToScreen) {
	intVec DataOut;
	intVec ZeroOut;
	cout << "Sector   Row   Pad    TimeBins" << endl;
	cout << "==========================================" << endl;
	for (unsigned int iSector = 0; iSector < mSectors; iSector++) { // sector loop
	    if (EventData->mSectors[iSector]) { // does sector exist?
		cout << "Data from Sector " << iSector+1 << endl;
		for (unsigned int iRow = 0; iRow < mRows; iRow++) { // row loop
		    for (unsigned int iPad = 0; iPad < padsAtRow[iRow]; iPad++) { // pad loop
			StTrsDigitalSector* currentSector = EventData->mSectors[iSector];
			digitalTimeBins padData = currentSector->mData[iRow][iPad];
			if (padData.size()>0){ // check if there is data
			    DataOut.clear();
			    
			    DataOut.resize(padData.size());
			    
			    transform (padData.begin(), padData.end(), DataOut.begin(), getInt);
			    // Output the vector to the screen.
			    cout << "     " << iSector+1;
			    cout << "     " << iRow+1;
			    cout << "     " << iPad+1;
			    cout << "     ";
			    copy (DataOut.begin(), DataOut.end(), ostream_iter_int(cout, " "));
			    cout << endl;
			} // if there's data to write...
		    } // pad
		} // row
	    } // check for existence of sector
	} // sector
    } // if print
    
}
