/***************************************************************************
 *
 * $Id: StMixerDigitalSector.cc,v 1.1 2000/02/16 21:02:08 pfachini Exp $
 *
 * Author: Patricia Fachini - a copy from StTrsDigitalSector.cc
 * 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 **************************************************************************/
#include "StMixerDigitalSector.hh"

StMixerDigitalSector::StMixerDigitalSector(StTpcGeometry* geoDb)
{
    digitalTimeBins  timeBins;
    digitalPadRow    padRow;
    int     irow;

    for(irow=0; irow< geoDb->numberOfRows(); irow++) {
	//cout << " NumberOfPadsAtRow(" << irow << "): " << geoDb->numberOfPadsAtRow(irow+1) << endl;
	//padRow.assign(geoDb->numberOfPadsAtRow(irow+1), timeBins);
	padRow.resize(geoDb->numberOfPadsAtRow(irow+1), timeBins);
	mData.push_back(padRow);
    }
    // tmp
    // check size at creation?
    //cout << "  NumberOfRows in Data Sector: " << mData.size() << endl;
//     for(int ii=0; ii<mSector.size(); ii++) {
//  	cout << "  Data  PadsInRow(" << ii << "): " << mData[ii].size()  << endl;
//     }
}

StMixerDigitalSector::~StMixerDigitalSector() {/* nopt */}

void StMixerDigitalSector::clear() // clears only the time bins
{
    cout << "StMixerDigitalSector::clear()" << endl;
    for(unsigned int irow=0; irow<mData.size(); irow++) {
	for(unsigned int ipad=0; ipad<mData[irow].size(); ipad++) {
	    mData[irow][ipad].clear();
	}
    }
}

// Caution: rowN specifies rowNumber 1..45
// Below, rowIndex specifies index 0..44
void StMixerDigitalSector::assignTimeBins(int rowN, int padN, digitalTimeBins* tbins)
{
#ifdef ST_SECTOR_BOUNDS_CHECK
    if( (rowIndex > 0 && rowIndex <= mData.size()) )
	if( (padIndex > 0 && padIndex <= mData[rowIndex].size()) )
#endif
	    {
	    mData[(rowN-1)][(padN-1)] = *tbins;
	    }
}

void StMixerDigitalSector::assignTimeBins(StTpcPadCoordinate& coord, digitalTimeBins* tbins)
{
    assignTimeBins(coord.row(), coord.pad(), tbins);
}
int StMixerDigitalSector::cleanup()
{
    unsigned int numberOfEmptyRows=0;
    for (unsigned int iRow=0; iRow<mData.size(); iRow++) {
	unsigned int numberOfEmptyPads=0;
	for (unsigned int iPad=0; iPad<mData[iRow].size(); iPad++) {
	    if (mData[iRow][iPad].size()<7) {
		mData[iRow][iPad].clear();
		numberOfEmptyPads++;
	    }
	} // Pads are now clean
	if (numberOfEmptyPads==mData[iRow].size()) {
	    mData[iRow].clear();
	    numberOfEmptyRows++;
	}
    } // Rows are now clean
    cout << "This sector had " << numberOfEmptyRows << " empty rows." << endl;
    if (numberOfEmptyRows==mData.size()) return 1;
    else return 0;
}    
