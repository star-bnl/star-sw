/***************************************************************************
 *
 * $Id: StTrsSector.cc,v 1.1 1998/11/10 17:12:26 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTrsSector.cc,v $
 * Revision 1.1  1998/11/10 17:12:26  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.3  1999/01/18 21:02:57  lasiuk
 * comment diagnostics
 *
 * Revision 1.2  1998/11/16 14:49:06  lasiuk
 * make sure of index when filling sector
 *
 * Revision 1.1  1998/11/10 17:12:26  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.3  1998/11/08 17:06:41  lasiuk
 * use resize() for LINUX compatibiltiy
 * allocator specification for vector<>
 *
 * Revision 1.2  1998/11/04 18:49:40  lasiuk
 * modify constructors
 * macro ST_SECTOR_BOUND_CHECK
 * addEntry()
 * addTimeBins()
 *
 * Revision 1.1  1998/06/30 22:46:50  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include "StTrsSector.hh"

StTrsSector::StTrsSector(StTpcGeometry* geoDb)
{
    tpcTimeBins  timeBins;
    tpcPadRow    padRow;
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

StTrsSector::~StTrsSector() {/* nopt */}

void StTrsSector::clear() // clears only the time bins
{
    cout << "in StTrsSector::clear()" << endl;
    for(int irow=0; irow<mSector.size(); irow++) {
	for(int ipad=0; ipad<mSector[irow].size(); ipad++) {
	    mSector[irow][ipad].clear();
void StTrsSector::addEntry(int row, int pad, StTrsAnalogSignal& sign)
    if( (row>0 && row<mSector.size()) )
	if( (pad>0 && pad<mSector[row].size()) )
void StTrsSector::addEntry(int rowN, int padN, StTrsAnalogSignal& signl)
	    mSector[row][pad].push_back(sign);
	if( (pad > 1 && pad <= mSector[row].size()) )
#ifdef ST_SECTOR_BOUNDS_CHECK
	    mSector[(row-1)][(pad-1)].push_back(sign);
	if( (padN > 0 && pad <= mSector[(rowN-1)].size()) )
#endif
	    mSector[(rowN-1)][(padN-1)].push_back(signl);
}

void StTrsSector::addEntry(StTpcPadCoordinate& coord, StTrsAnalogSignal& sig)
{
    if( (row>0 && row<mSector.size()) )
	if( (pad>0 && pad<mSector[row].size()) )
// Above: rowN specifies rowNumber 1..45
	    mSector[row][pad] = tbins;
	if( (pad > 1 && pad <= mSector[row].size()) )
#ifdef ST_SECTOR_BOUNDS_CHECK
	    mSector[(row-1)][(pad-1)] = tbins;
	if( (padIndex > 0 && padIndex <= mSector[rowIndex].size()) )
#endif
	    mSector[(rowN-1)][(padN-1)] = tbins;
}

void StTrsSector::assignTimeBins(StTpcPadCoordinate& coord, tpcTimeBins& tbins)
{
    assignTimeBins(coord.row(), coord.pad(), tbins);
}
