/***************************************************************************
 *
 * $Id: StTrsUnpacker.cc,v 1.1 1999/02/04 18:38:10 lasiuk Exp $
 *
 * Author: bl prelim
 ***************************************************************************
 *
 * Description: Access to the digital information via the abstract
 *              interface
 ***************************************************************************
 *
 * $Log: StTrsUnpacker.cc,v $
 * Revision 1.1  1999/02/04 18:38:10  lasiuk
 * Initial Revision
 *
 * Revision 1.3  1999/02/10 20:55:18  lasiuk
 * Feb 10,1999
 *
 * Revision 1.2  1999/02/10 04:26:21  lasiuk
 * Revision 1.1  1999/02/04 18:38:10  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include <assert.h> 

#include "StGlobals.hh"

#include "StTrsUnpacker.hh"
    PR(theData->mSectors.size());
    if(theData->mSectors.size() >= which) {
	cout << "Sector: " << which << " data exists." << endl;
	mSector = theData->mSectors[(which-1)];
	status = 0;
    }
    else {
	status = -1;
    }
	    mSector = theData->mSectors[(which-1)];
}
	}
	else { // If the pointer is NULL
	       // Go on to the next sector
	    cout << "Sector: " << which << " data exists NOT." << endl;
	    status = -1;
	}
    } // Bounds check
    PR(TrsPadData.first->size());
} 

int StTrsUnpacker::getSequences(int padRow, int npad, int *nSeq, StSequence** Seq)
{
    PR(numberOfEntriesD);
    PR(numberOfEntriesZ);
	TrsPadData = mSector->timeBinsOfRowAndPad(padRow,npad);

//     PR(TrsPadData.first->size());
    //assert(numberOfEntriesD == numberOfEntriesZ);
    short numberOfEntriesD = TrsPadData.first->size();
    //PR(numberOfEntriesD);

    short startTimeBin = 0;
    unsigned short ii = 0;

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StSequence> tmp;
	    cout << "ii(" << ii << ") is zero" << endl;
	    PR(numberOfZeros);
	//PR(static_cast<int>(TrsPadData.first->at(ii)));
	    PR(numberOfZeros);
#else
//     for(int bbb=0; bbb<numberOfEntriesD; bbb++) {
	    //PR(numberOfZeros);
// 	cout << bbb << '\t' << (int)(*TrsPadData.first)[bbb] << '\t' << (int)(*TrsPadData.second)[bbb] << endl;
//     }
    // Construct the sequences:
    for(; ii<numberOfEntriesD; ii++) {
	if ( (*TrsPadData.first)[ii] == static_cast<unsigned char>(0) ) {
	    numberOfZeros += (*TrsPadData.second)[ii];
	PR(*aSequence.firstAdc);
	unsigned short theLength = 0;
	PR(aSequence.startTimeBin);
	//PR(static_cast<int>(*aSequence.firstAdc));
	//unsigned short theLength = 0;
	StSequence aSequence;
// 	PR(static_cast<int>(*aSequence.firstAdc));
//  	PR(aSequence.startTimeBin);
	aSequence.length--;
// 	PR(numberOfEntriesD);
	aSequence.length = static_cast<unsigned short>(0);
	//aSequence.length--;
// 	    PR(static_cast<int>((*TrsPadData.first)[ii]));

	numberOfZeros = 0;
// 	PR(aSequence.length);
		   (ii<numberOfEntriesD) );
	ii--; // Adjust it, since you overstep the sequence...
//  	PR(aSequence.length);
	numberOfZeros += aSequence.length;
    PR(tmp.size());
    }

    // Return as an array!
    // CAREFUL::Must call clear() to deallocate this memory when you are done!
    //
	cout << " " << ii << endl;
	PR(mSequence[ii].startTimeBin);
	PR(mSequence[ii].length);
	PR(static_cast<int>(*mSequence[ii].firstAdc));
    
    for(ii=0; ii< tmp.size(); ii++) {
	//cout << " " << ii << endl;
	//PR(mSequence[ii].startTimeBin);
	//PR(mSequence[ii].length);
	//PR(static_cast<int>(*mSequence[ii].firstAdc));
    }
	    
// 	PR(mSequence[ii].length);
    cout << "StTrsUnpacker::getPadList() not yet implemented" << endl;
    return 0;
	for(ii=0; ii< tmp.size(); ii++) {
	    mPadList[ii] = tmp[ii];
// 	    PR(static_cast<int>(mPadList[ii]));
	}
    
    }

    return numberOfPadsWithSignals;
}

void StTrsUnpacker::clear()
{
    delete [] mSequence;
    delete [] mPadList;
}
