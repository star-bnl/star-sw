/***************************************************************************
 *
 * $Id: StTrsUnpacker.cc,v 1.6 1999/02/16 18:15:41 fisyak Exp $
 *
 * Author: bl prelim
 ***************************************************************************
 *
 * Description: Access to the digital information via the abstract
 *              interface
 ***************************************************************************
 *
 * $Log: StTrsUnpacker.cc,v $
 * Revision 1.6  1999/02/16 18:15:41  fisyak
 * Check in the latest updates to fix them
 *
 * Revision 1.7  1999/02/23 14:05:16  lasiuk
 * exit if last time bin has non-zero count in ADCs
 *
 * Revision 1.6  1999/02/16 18:15:41  fisyak
 * Check in the latest updates to fix them
 *
 * Revision 1.5  1999/02/14 20:45:15  lasiuk
 * use assert and index (ii) was 'off by 1'
 *
 * Revision 1.4  1999/02/12 01:27:18  lasiuk
 * Limit Debug output
 *
 * Revision 1.3  1999/02/10 20:55:18  lasiuk
 * Feb 10,1999
 *
 * Revision 1.2  1999/02/10 04:26:21  lasiuk
 * TObject for passing
 *
 * Revision 1.1  1999/02/04 18:38:10  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include <assert.h> 

#include "StGlobals.hh"

#include "StTrsUnpacker.hh"

StTrsUnpacker::StTrsUnpacker() { /* nopt */}

StTrsUnpacker::~StTrsUnpacker() { /* nopt */}
    
int StTrsUnpacker::getSector(int which, StTpcRawDataEvent* eventData)
{
    int status;
    StTrsRawDataEvent *theData = (StTrsRawDataEvent*)eventData;
//     PR(theData->mSectors.size());

    // Diagnostic...make sure the pointers are zero!
//     for(int bbb=0; bbb<theData->mSectors.size(); bbb++)
// 	cout << bbb << '\t' << (theData->mSectors[bbb]) << endl;
    
    if(theData->mSectors.size() >= which) { // bounds check
	if ( (theData->mSectors[(which-1)]) ) {  // check the pointer...
	    cout << "Sector: " << which << " data exists." << endl;
	    mSector = theData->mSectors[(which-1)];
	    status = 0;
	}
	else { // If the pointer is NULL
	       // Go on to the next sector
	    cout << "Sector: " << which << " data exists NOT." << endl;
	    status = -1;
	}
    } // Bounds check
    return status;
} 

int StTrsUnpacker::getSequences(int padRow, int npad, int *nSeq, StSequence** Seq)
{
    
    //PR(TrsPadData.first->size());
	TrsPadData = mSector->timeBinsOfRowAndPad(padRow,npad);

//     PR(TrsPadData.first->size());

    short numberOfZeros = 0;
    short numberOfEntriesD = TrsPadData.first->size();
    short numberOfEntriesZ = TrsPadData.second->size();
    //PR(numberOfEntriesD);
    //PR(numberOfEntriesZ);
    assert(numberOfEntriesD == numberOfEntriesZ);
    // if not, you are in trouble anyway

    short startTimeBin = 0;
    unsigned short ii = 0;

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StSequence> tmp;
#else
//     for(int bbb=0; bbb<numberOfEntriesD; bbb++) {
// 	cout << bbb << '\t' << (int)(*TrsPadData.first)[bbb] << '\t' << (int)(*TrsPadData.second)[bbb] << endl;
//     }
    // Construct the sequences:
    for(; ii<numberOfEntriesD; ii++) {
	if ( (*TrsPadData.first)[ii] == static_cast<unsigned char>(0) ) {
	    numberOfZeros += (*TrsPadData.second)[ii];
	    continue;
	}
	if ( (*TrsPadData.second)[ii] == static_cast<unsigned char>(255) )
	    continue;

	StSequence aSequence;
// 	PR(aSequence.startTimeBin);
// 	PR(static_cast<int>(*aSequence.firstAdc));
//  	PR(aSequence.startTimeBin);

// 	PR(numberOfEntriesD);
	aSequence.length = static_cast<unsigned short>(0);
	
// 	    PR(aSequence.length);
		  (ii<numberOfEntriesD) );
// 	    PR(static_cast<int>((*TrsPadData.first)[ii]));
// 	PR(aSequence.length);
		   (ii<numberOfEntriesD) );
	ii--; // Adjust it, since you overstep the sequence...
//  	PR(aSequence.length);
	numberOfZeros += aSequence.length;
	tmp.push_back(aSequence);
    }

    // Return as an array!
    // CAREFUL::Must call clear() to deallocate this memory when you are done!
    //
    //PR(tmp.size());
    mSequence = new StSequence[*nSeq];

//     PR(tmp.size());
    
    for(ii=0; ii< tmp.size(); ii++) {
	//cout << " " << ii << endl;
	//PR(mSequence[ii].startTimeBin);
	//PR(mSequence[ii].length);
	//PR(static_cast<int>(*mSequence[ii].firstAdc));
    }
	    
// 	PR(mSequence[ii].length);
// 	PR(static_cast<int>(*mSequence[ii].firstAdc));
    }    
    *Seq = mSequence;

    return 0;
}

int  StTrsUnpacker::getPadList(int padRow, unsigned char **padList)
{
    //cout << "StTrsUnpacker::getPadList()" << endl;
    // Count the sequences on the pad and store the list
    int numberOfPadsWithSignals = 0;

    // Should be data base derived quatities...
    if(padRow<1 || padRow>45) {
	cerr << "Pad Row " << padRow << " out of range" << endl;
	cerr << "Normally one would throw an exception here" << endl;
    //PR(padRow);
    //PR(mSector->numberOfPadsInRow(padRow));
    
//     PR(padRow);
//     PR(mSector->numberOfPadsInRow(padRow));

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<unsigned char> tmp;
#else
    // For the SUN
    vector<unsigned char, allocator<unsigned char> > tmp;
#endif
    tmp.clear();
// 	    cout << " pad " << ii << " " << (mSector->numberOfTimeBins(padRow,ii)) << endl;
    for(int ii=1; ii<=mSector->numberOfPadsInRow(padRow); ii++) {
	if (mSector->numberOfTimeBins(padRow,ii) > 0) {
//  	    cout << " pad " << ii << " " << (mSector->numberOfTimeBins(padRow,ii)) << endl;
	    numberOfPadsWithSignals++;
	    tmp.push_back(ii);
    //PR(tmp.size());
    }

//     PR(tmp.size());

    if (tmp.size() == 0) {
	mPadList = 0;
    }
    else {  // Otherwise fill the pad list
	mPadList = new unsigned char[(tmp.size())];
	    //PR(static_cast<int>(mPadList[ii]));
	for(ii=0; ii< tmp.size(); ii++) {
	    mPadList[ii] = tmp[ii];
// 	    PR(static_cast<int>(mPadList[ii]));
	}
    
	*padList = mPadList;
    }

    return numberOfPadsWithSignals;
}

void StTrsUnpacker::clear()
{
    delete [] mSequence;
    delete [] mPadList;
}
