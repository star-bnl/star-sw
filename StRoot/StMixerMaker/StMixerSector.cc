/***************************************************************************
 *
 * $Id: StMixerSector.cc,v 1.1 2000/02/16 21:02:10 pfachini Exp $
 *
 * Author: Patricia Fachini - a copy from StTrsSector.cc
 *
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 **************************************************************************/
#include "StMixerSector.hh"

StMixerSector::StMixerSector(StTpcGeometry* geoDb)
{
    tpcTimeBins  timeBins;
    tpcPadRow    padRow;
    int          irow;

    for(irow=0; irow< geoDb->numberOfRows(); irow++) {
	padRow.resize(geoDb->numberOfPadsAtRow(irow+1), timeBins);
	mSector.push_back(padRow);
    }
}

StMixerSector::~StMixerSector() {/* nopt */}

void StMixerSector::clear() // clears only the time bins
{
    for(unsigned int irow=0; irow<mSector.size(); irow++) {
	for(unsigned int ipad=0; ipad<mSector[irow].size(); ipad++) {
	    mSector[irow][ipad].clear();
	}
    }
}

void StMixerSector::addEntry(StTpcPadCoordinate& coord, StMixerAnalogSignal& sig)
{
    addEntry(coord.row(), coord.pad(), sig);
}

void StMixerSector::assignTimeBins(StTpcPadCoordinate& coord, tpcTimeBins& tbins)
{
    assignTimeBins(coord.row(), coord.pad(), tbins);
}
