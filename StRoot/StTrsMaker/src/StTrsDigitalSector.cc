/***************************************************************************
 *
 * $Id: StTrsDigitalSector.cc,v 1.4 1999/02/10 04:25:43 lasiuk Exp $
 *
 * Author: bl 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTrsDigitalSector.cc,v $
 * Revision 1.4  1999/02/10 04:25:43  lasiuk
 * remove debug
 *
 * Revision 1.3  1999/01/22 08:06:21  lasiuk
 * use unsigned char for compatibilty with interface.
 * requires use of two arrays...ugly but fine for now.
 * use of pair<>; values returned by pointer
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
    int              irow;

    for(irow=0; irow< geoDb->numberOfRows(); irow++) {
	//cout << " NumberOfPadsAtRow(" << irow << "): " << geoDb->numberOfPadsAtRow(irow+1) << endl;
	//padRow.assign(geoDb->numberOfPadsAtRow(irow+1), timeBins);
	padRow.resize(geoDb->numberOfPadsAtRow(irow+1), timeBins);
	mData.push_back(padRow);
	mZeros.push_back(padRow);
    }
    // tmp
    // check size at creation?
    //cout << "  NumberOfRows in Data Sector: " << mData.size() << endl;
    //cout << "  NumberOfRows in Zero Sector: " << mZeros.size() << endl;
//     for(int ii=0; ii<mSector.size(); ii++) {
//  	cout << "  Data  PadsInRow(" << ii << "): " << mData[ii].size()  << endl;
//  	cout << "  Zeros PadsInRow(" << ii << "): " << mZeros[ii].size() << endl;
//     }
}

StTrsDigitalSector::~StTrsDigitalSector() {/* nopt */}

void StTrsDigitalSector::clear() // clears only the time bins
{
    cout << "StTrsDigitalSector::clear()" << endl;
    if(mData.size() != mZeros.size()) {
	cerr << "Error:StTrsDigitalSector::~StTrsDigitalSector()" << endl;
	cerr << "Data and Zero array not same size!" << endl;
	exit(-1);
    }
    for(int irow=0; irow<mData.size(); irow++) {
	if(mData[irow].size() != mZeros[irow].size()) {
	    cerr << "Error:StTrsDigitalSector::~StTrsDigitalSector()" << endl;
	    cerr << "Data[irow] and Zeros[irow] array not same size!" << endl;
	    exit(-1);
	}
	for(int ipad=0; ipad<mData[irow].size(); ipad++) {
	    mData[irow][ipad].clear();
	    mZeros[irow][ipad].clear();
	}
    }
}

// Caution: rowN specifies rowNumber 1..45
// Below, rowIndex specifies index 0..44
void StTrsDigitalSector::assignTimeBins(int rowN, int padN, pair<digitalTimeBins*, digitalTimeBins*> tbins)
{
#ifdef ST_SECTOR_BOUNDS_CHECK
    if( (rowIndex > 0 && rowIndex <= mData.size()) &&
	(rowIndex > 0 && rowIndex <= mZeros.size()) )
	if( (padIndex > 0 && padIndex <= mData[rowIndex].size()) &&
	    (padIndex > 0 && padIndex <= mZeros[rowIndex].size()) )
#endif
	    {
	    mData[(rowN-1)][(padN-1)] = *(tbins.first);
	    mZeros[(rowN-1)][(padN-1)] = *(tbins.second);
	    }
}

void StTrsDigitalSector::assignTimeBins(StTpcPadCoordinate& coord, pair<digitalTimeBins*, digitalTimeBins*> tbins)
{
    assignTimeBins(coord.row(), coord.pad(), tbins);
}
