/***************************************************************************
 *
 * $Id: StTrsZeroSuppressedReader.cc,v 1.5 2000/03/15 18:08:56 calderon Exp $
 *
 * Authors: bl, mcbs
 ***************************************************************************
 *
 * Description: Access to the digital information via the abstract
 *              interface...this is the OLD UNPACKER
 ***************************************************************************
 *
 * $Log: StTrsZeroSuppressedReader.cc,v $
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
 * Added StTrsDetectorReader and StTrsZeroSuppressedReader for DAQ type
 * data access.
 * Removed vestigial for loop in sampleAnalogSignal() method.
 * Write version of data format in .trs data file.
 *
 ***************************************************************************/
#include "StTrsZeroSuppressedReader.hh"

#include <algorithm>
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
#include "StTrsDigitalSector.hh"


StTrsZeroSuppressedReader::StTrsZeroSuppressedReader()
    :mSector(1), mTheSector(0), mPadList(0), mSequence(0), mTrsEvent(0)
{ /*nopt */ }

// StTrsZeroSuppressedReader::StTrsZeroSuppressedReader(int sector, StTpcRawEvent& theEvent) : mSector(sector), mTheEvent(theEvent)
// {  }

StTrsZeroSuppressedReader::StTrsZeroSuppressedReader(StTpcRawDataEvent* ev)
    :mSector(1), mTheSector(0), mPadList(0), mSequence(0)
{
    mTrsEvent = dynamic_cast<StTrsRawDataEvent*>(ev);
    if (!mTrsEvent) {
	cerr << "Error constructing StTrsZeroSuppressedReader" << endl;
	cerr << "Cast Failed! ev not of required type (StTrsRawDataEvent*)\n";
    }
}

int StTrsZeroSuppressedReader::setSector(int sector)
{
    clear();
    // Check if the data is there --> getSector() in the Unpacker!
    // ...you may even want to check the mVersion to return the
    // proper mZSR...
    
    if(checkTheData(sector)) {
	    mSector = sector;
	    mTheSector = mTrsEvent->mSectors[sector-1];
	    return 1;
    }
    else {
	cerr << "**No Data in Sector " << sector << " **" << endl;
	return 0;
    }
}


StTrsZeroSuppressedReader::~StTrsZeroSuppressedReader()
{
    if (mSequence) delete [] mSequence;
    if (mPadList)  delete [] mPadList;
    mSequence = 0;
    mPadList  = 0;
}

int StTrsZeroSuppressedReader::checkTheData(unsigned int which)
{
    int status(0);
    
    if(mTrsEvent->mSectors.size() >= which)  // bounds check
	if ( (mTrsEvent->mSectors[(which-1)]) ) status = 1;
    return status;   
}

int StTrsZeroSuppressedReader::getPadList(int padRow, unsigned char **padList)
{
    if(mPadList) {
	delete [] mPadList;
	mPadList = 0;
    }
    //
    // Count the sequences on the pad and store the list
    int numberOfPadsWithSignals = 0;
    //
    // Should be data base derived quatities...
    if(padRow<1 || padRow>45) {
#ifndef ST_NO_EXCEPTIONS
	throw out_of_range("Pad Row out of range");
#else
	cerr << "Pad Row " << padRow << " out of range" << endl;
	cerr << "Normally one would throw an exception here" << endl;
	exit(1);
#endif
    }
    
//     PR(padRow);
//     PR(mTheSector->numberOfPadsInRow(padRow));

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<unsigned char> tmp;
#else
    vector<unsigned char, allocator<unsigned char> > tmp;
#endif
    tmp.clear();
    // Loop over all the pads:
    for(int ii=1; ii<=mTheSector->numberOfPadsInRow(padRow); ii++) {
	if (mTheSector->numberOfTimeBins(padRow,ii) > 0) {
  	    //cout << " pad " << ii << " " << (mTheSector->numberOfTimeBins(padRow,ii)) << endl;
	    numberOfPadsWithSignals++;
	    tmp.push_back(ii);
	}
    }

    if (tmp.size() == 0) {
	mPadList = 0;
    }
    else {
	// Otherwise fill the pad list
	mPadList = new unsigned char[(tmp.size())];

	for(unsigned int ii=0; ii< tmp.size(); ii++) {
	    mPadList[ii] = tmp[ii];
// 	    PR(static_cast<int>(mPadList[ii]));
	}

	*padList = mPadList;
    }

    return numberOfPadsWithSignals;
}

int getInt3(unsigned char a) { return static_cast<int>(a);}

//int StTrsZeroSuppressedReader::getSequences(int PadRow, int Pad, int *nSeq, StSequence** Seq)
int StTrsZeroSuppressedReader::getSequences(int PadRow, int Pad, int *nSeq, Sequence** Seq)
{
    if(mSequence) {
	delete [] mSequence;
	mSequence = 0;
    }
    
    digitalTimeBins* TrsPadData = mTheSector->timeBinsOfRowAndPad(PadRow,Pad);
//     typedef vector<int> intVec;
//     typedef vector<unsigned char> digitalTimeBins;
//     typedef istream_iterator<unsigned char> istream_iter_uns_char;
//     typedef ostream_iterator<int> ostream_iter_int;
//     intVec DataOut(TrsPadData->size());
//     transform (TrsPadData->begin(), TrsPadData->end(), DataOut.begin(), getInt3);
//     copy (DataOut.begin(), DataOut.end(), ostream_iter_int(cout, " "));
//     cout << endl;

    unsigned short currentTimeBin = 0;
    digitalTimeBinIterator rangeBegin = TrsPadData->begin();
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<Sequence> tmp;
#else
    vector<Sequence, allocator<Sequence> > tmp;
#endif
    tmp.clear();

    //
    // Construct the sequences:
    do {
	digitalTimeBinIterator rangeEnd = find(rangeBegin, TrsPadData->end(), static_cast<unsigned char>(0));
	int length=0;
	distance(rangeBegin,rangeEnd,length);
	if (length){
	    Sequence aSequence;
	    aSequence.startTimeBin = currentTimeBin;
	    aSequence.FirstAdc     = rangeBegin;
	    aSequence.Length       = length;
	    tmp.push_back(aSequence);
	    currentTimeBin += length;
	}
	else {
	    if (rangeEnd==TrsPadData->end()) continue;
	    rangeEnd++;
	    currentTimeBin += *rangeEnd;
	    rangeEnd++;
	}
	rangeBegin = rangeEnd;
    }while (rangeBegin!=TrsPadData->end());

    // Return as an array

    *nSeq = tmp.size();
    mSequence = new Sequence[*nSeq];

//     PR(tmp.size());
    
    for(unsigned int ii=0; ii< tmp.size(); ii++) mSequence[ii] = tmp[ii];
    *Seq = mSequence;

    return 0;
}

void StTrsZeroSuppressedReader::clear()
{
    //cout << "StTrsZeroSuppressedReader::clear()" << endl;
    if (mPadList) delete [] mPadList;
    mPadList = 0;
    if (mSequence) delete [] mSequence;
    mSequence = 0;
}

int StTrsZeroSuppressedReader::getSpacePts(int PadRow, int* nSpacePts, SpacePt** SpacePoints)
{
    cout << "StTrsZeroSuppressedReader::getSpacePts() NOT IMPLEMENTED!!" << endl;
    return 0;
}

int StTrsZeroSuppressedReader::MemUsed()
{
    return 0;
}
