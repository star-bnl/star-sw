/***************************************************************************
 *
 * $Id: StTrsIstream.cc,v 1.2 1999/10/12 00:07:25 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez 
 ***************************************************************************
 *
 * Description: Istream class for
 *              reading TRS data.
 ***************************************************************************
 *
 * $Log: StTrsIstream.cc,v $
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
#include <algorithm>
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
    cout << "Filling Tpc Info from File " << endl; 
    unsigned short lengthData;
    unsigned short currentSectorNum;
    unsigned short currentRowNum;
    unsigned short currentPadNum;
    ifs >> currentSectorNum;
    while (currentSectorNum < static_cast<unsigned short>(mSectors)) {
	//PR(currentSectorNum);
	//EventData->mSectors[iSector] = new StTrsDigitalSector;
	StTrsDigitalSector* aDigitalSector = new StTrsDigitalSector(mGeomDb);

	digitalTimeBins digitalPadData;
	digitalTimeBins digitalPadZeros;
	ifs >> currentRowNum;
	while (currentRowNum < static_cast<unsigned short>(mRows)) {
	    //PR(currentRowNum);
	    
	    aDigitalSector->mData[currentRowNum].resize(padsAtRow[currentRowNum]);
	    aDigitalSector->mZeros[currentRowNum].resize(padsAtRow[currentRowNum]);
	    ifs >> currentPadNum;
	    while (static_cast<unsigned int>(currentPadNum) < padsAtRow[currentRowNum]) {
		
		//PR(currentPadNum);
		ifs >> lengthData;
		//PR(lengthData);
		digitalPadData.clear();
		digitalPadZeros.clear();
// 		if (lengthData>0) {
// 		    digitalPadData.resize(lengthData);
// 		    digitalPadZeros.resize(lengthData);

// 		    ifs.read(static_cast<unsigned char*>(digitalPadData.begin()), lengthData);
// 		    ifs.read(static_cast<unsigned char*>(digitalPadZeros.begin()), lengthData);
		    
// 		}
		if (lengthData>0) {
		    digitalTimeBins inputData(lengthData);
		    ifs.read(static_cast<unsigned char*>(inputData.begin()), lengthData);
		    unsigned int  nZeros = count(inputData.begin(),inputData.end(),static_cast<unsigned char>(0));
		    digitalPadData.resize(lengthData-nZeros);
		    digitalPadData.resize(lengthData-nZeros);
		    
		    digitalTimeBins::iterator dataBegin  = inputData.begin();
		    digitalTimeBins::iterator dataEnd    = inputData.end();
		    digitalTimeBins::iterator rangeBegin = dataBegin;
		    digitalTimeBins::iterator currentPos = digitalPadData.begin();
		    while (rangeBegin!=inputData.end() && currentPos != digitalPadData.end()){
			digitalTimeBins::iterator rangeEnd = find(rangeBegin,dataEnd,static_cast<unsigned char>(0));
			copy(rangeBegin,rangeEnd,currentPos);
			currentPos+=distance(rangeBegin,rangeEnd);
			if (*rangeEnd == static_cast<unsigned char>(0)) {
			    unsigned int index = distance(dataBegin, rangeEnd);
			    *currentPos++ = static_cast<unsigned char>(0);
			    digitalPadZeros[index] = *currentPos;
			    currentPos++;
			}
			rangeBegin = (rangeEnd==dataEnd) ?  dataEnd : rangeEnd+1;
		    }
		    
		}
		pair<digitalTimeBins*, digitalTimeBins*> tmp(&digitalPadData, &digitalPadZeros);
		aDigitalSector->assignTimeBins(currentRowNum+1,currentPadNum+1,tmp);
		ifs >> currentPadNum;
	    } // while currentPadNum is not = padsAtRow[currentRowNum] 
	    ifs >> currentRowNum;
	} // while currentRowNum is not = mRows
	EventData->mSectors[currentSectorNum] = aDigitalSector;
	ifs >> currentSectorNum;
    } // While currentSectorNum is not = mSectors

    // See what we read
    bool PrintToScreen = true;
    if (PrintToScreen) {
    intVec DataOut;
    intVec ZeroOut;
    cout << "Sector   Row   Pad    TimeBins" << endl;
    cout << "==========================================" << endl;
    for (int iSector = 0; iSector < mSectors; iSector++) {
	if (EventData->mSectors[iSector]) {
	    cout << "Data from Sector " << iSector+1 << endl;
	    for (int iRow = 0; iRow < mRows; iRow++) {
		for (int iPad = 0; iPad < padsAtRow[iRow]; iPad++) {
		    StTrsDigitalSector* currentSector = EventData->mSectors[iSector];
		    digitalTimeBins padData = currentSector->mData[iRow][iPad];
		    digitalTimeBins padZero = currentSector->mZeros[iRow][iPad]; 
		    if (padData.size() != padZero.size()) {
			cerr << "Error reading from file" << endl;
			cerr << "Data and Zero vector don't have the same lenght!!" << endl;
			exit(1);
		    }
		    if (padData.size()>0){
			DataOut.clear();
			ZeroOut.clear();
			
			DataOut.resize(padData.size());
			ZeroOut.resize(padZero.size());
			
			transform (padData.begin(), padData.end(), DataOut.begin(), getInt);
			transform (padZero.begin(), padZero.end(), ZeroOut.begin(), getInt);
			// Output the vector to the screen.
			cout << "     " << iSector+1;
			cout << "     " << iRow+1;
			cout << "     " << iPad+1;
			cout << "     ";
			copy (DataOut.begin(), DataOut.end(), ostream_iter_int(cout, " "));
			cout << endl;
			cout << "                         ";
			copy (ZeroOut.begin(), ZeroOut.end(), ostream_iter_int(cout, " "));
			cout << endl;
		    } // if there's data to write...
		} // pad
	    } // row
	} // check for existence of sector
    } // sector
    } // if print
    
}
