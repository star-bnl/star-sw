/***************************************************************************
 *
 * $Id: StTrsDigitalSector.cc,v 1.1 1999/01/18 10:23:51 lasiuk Exp $
 *
 * Author: bl 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTrsDigitalSector.cc,v $
 * Revision 1.1  1999/01/18 10:23:51  lasiuk
 * initial Revision
 *
 * Revision 1.2  1999/01/18 21:02:47  lasiuk
 * comment diagnostics
 *
 * Revision 1.1  1999/01/18 10:23:51  lasiuk
 * initial Revision
 *
 *
 **************************************************************************/
#include "StTrsDigitalSector.hh"

StTrsDigitalSector::StTrsDigitalSector(StTpcGeometry* geoDb)
{
    digitalTimeBins  timeBins;
    digitalPadRow    padRow;
	cout << " NumberOfPadsAtRow(" << irow << "): " << geoDb->numberOfPadsAtRow(irow+1) << endl;

    for(irow=0; irow< geoDb->numberOfRows(); irow++) {
	//cout << " NumberOfPadsAtRow(" << irow << "): " << geoDb->numberOfPadsAtRow(irow+1) << endl;
	//padRow.assign(geoDb->numberOfPadsAtRow(irow+1), timeBins);
	padRow.resize(geoDb->numberOfPadsAtRow(irow+1), timeBins);
	mSector.push_back(padRow);
    }

    cout << "  NumberOfRows in Sector: " << mSector.size() << endl;
    for(int ii=0; ii<mSector.size(); ii++) {
 	cout << "  PadsInRow(" << ii << "): " << mSector[ii].size() << endl;
    }
//     for(int ii=0; ii<mSector.size(); ii++) {
//  	cout << "  PadsInRow(" << ii << "): " << mSector[ii].size() << endl;
//     }
}

StTrsDigitalSector::~StTrsDigitalSector() {/* nopt */}

void StTrsDigitalSector::clear() // clears only the time bins
{
    cout << "in StTrsDigitalSector::clear()" << endl;
    for(int irow=0; irow<mSector.size(); irow++) {
	for(int ipad=0; ipad<mSector[irow].size(); ipad++) {
	    mSector[irow][ipad].clear();
	}
    }
}

// Caution: rowN specifies rowNumber 1..45
// Below, rowIndex specifies index 0..44
void StTrsDigitalSector::assignTimeBins(int rowN, int padN, digitalTimeBins& tbins)
{
#ifdef ST_SECTOR_BOUNDS_CHECK
    if( (rowIndex > 0 && rowIndex <= mSector.size()) )
	if( (padIndex > 0 && padIndex <= mSector[rowIndex].size()) )
#endif
	    mSector[(rowN-1)][(padN-1)] = tbins;
}

void StTrsDigitalSector::assignTimeBins(StTpcPadCoordinate& coord, digitalTimeBins& tbins)
{
    assignTimeBins(coord.row(), coord.pad(), tbins);
}
