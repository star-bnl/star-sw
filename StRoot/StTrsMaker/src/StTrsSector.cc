/***************************************************************************
 *
 * $Id: StTrsSector.cc,v 1.10 2007/07/12 20:25:05 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTrsSector.cc,v $
 * Revision 1.10  2007/07/12 20:25:05  fisyak
 * Use StarLogger, use time of flight, fix cluster shape
 *
 * Revision 1.9  2005/10/06 19:59:10  fisyak
 * adjust for ICC
 *
 * Revision 1.8  2005/09/09 22:12:49  perev
 * Bug fix + IdTruth added
 *
 * Revision 1.7  1999/12/08 02:10:42  calderon
 * Modified to eliminate warnings on Linux.
 *
 * Revision 1.6  1999/11/11 19:45:12  calderon
 * Made variables-> data members in analog signal generator to avoid
 * initialization time when member functions are called.
 * Inlined:
 *  StTrsParameterizedAnalogSignalGenerator::signalSampler()
 *  StTrsSector::addEntry()
 *  StTrsSector::assignTimeBins()
 *
 * Revision 1.5  1999/02/16 23:36:42  lasiuk
 * carefull...row>0
 *
 * Revision 1.4  1999/02/12 01:26:37  lasiuk
 * Limit debug output
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
#include "StMCTruth.h"
//______________________________________________________________________________
StTrsSector::StTrsSector(StTpcGeometry* geoDb)
{
    tpcTimeBins  timeBins;
    tpcPadRow    padRow;
    int          irow;

    for(irow=0; irow< geoDb->numberOfRows(); irow++) {
	//cout << " NumberOfPadsAtRow(" << irow << "): " << geoDb->numberOfPadsAtRow(irow+1) << endl;
	//padRow.assign(geoDb->numberOfPadsAtRow(irow+1), timeBins);
	padRow.resize(geoDb->numberOfPadsAtRow(irow+1), timeBins);
	mSector.push_back(padRow);
    }

    // tmp
    // check size at creation?
//     cout << "  NumberOfRows in Sector: " << mSector.size() << endl;
//     for(int ii=0; ii<mSector.size(); ii++) {
//  	cout << "  PadsInRow(" << ii << "): " << mSector[ii].size() << endl;
//     }
}

//______________________________________________________________________________
StTrsSector::~StTrsSector() {/* nopt */}

//______________________________________________________________________________
void StTrsSector::clear() // clears only the time bins
{
    //cout << "in StTrsSector::clear()" << endl;
    for(unsigned int irow=0; irow<mSector.size(); irow++) {
	for(unsigned int ipad=0; ipad<mSector[irow].size(); ipad++) {
	    mSector[irow][ipad].clear();
	}
    }
}

// Caution: rowN specifies rowNumber 1..45
// Below, rowIndex specifies index 0..44
// void StTrsSector::addEntry(int rowN, int padN, StTrsAnalogSignal& signl)
// {
//     //cout << "rowN " << rowN << " padN " << padN << endl;
// #ifdef ST_SECTOR_BOUNDS_CHECK
//     if( (rowN > 0 && row <= mSector.size()) )
// 	if( (padN > 0 && pad <= mSector[(rowN-1)].size()) )
// #endif
// 	    mSector[(rowN-1)][(padN-1)].push_back(signl);
// }

//______________________________________________________________________________
void StTrsSector::addEntry(StTpcPadCoordinate& coord, StTrsAnalogSignal& sig)
{
    addEntry(coord.row(), coord.pad(), sig);
}

// Caution: rowIndex specifies index 0..44
// Above: rowN specifies rowNumber 1..45
// void StTrsSector::assignTimeBins(int rowN, int padN, tpcTimeBins& tbins)
// {
// #ifdef ST_SECTOR_BOUNDS_CHECK
//     if( (rowIndex > 0 && rowIndex <= mSector.size()) )
// 	if( (padIndex > 0 && padIndex <= mSector[rowIndex].size()) )
// #endif
// 	    mSector[(rowN-1)][(padN-1)] = tbins;
// }

//______________________________________________________________________________
void StTrsSector::assignTimeBins(StTpcPadCoordinate& coord, tpcTimeBins& tbins)
{
    assignTimeBins(coord.row(), coord.pad(), tbins);
}
//______________________________________________________________________________
int StTrsSector::sort()
{
  int nadd=0;
  StMCPivotTruth  pivo(1);
  int nrows = mSector.size();
  for (int irow=0;irow<nrows;irow++) {
    int npads = mSector[irow].size();
    for (int ipad=0; ipad<npads; ipad++) {
      tpcTimeBins &tb = mSector[irow][ipad];
      int ntb = tb.size();
      if (ntb<2) continue;
#if defined( __ICC ) &&  __ICC < 910
      sort(tb.begin(), tb.end(),StTrsAnalogSignalComparator());
#else
      std::sort(tb.begin(), tb.end(),StTrsAnalogSignalComparator());
#endif
      int jl=0,jr=1;
      for (;1;jr++) {
        assert(jr==ntb || tb[jl].time() <= tb[jr].time());
        if (jr==ntb || tb[jl].time() < tb[jr].time()) {
          if (pivo.Size()) {tb[jl].setId(pivo.Get()); pivo.Reset();}
          if (jr==ntb) break;
          jl++; if (jl != jr) tb[jl] = tb[jr];
        } else {
          nadd++;
          if (!pivo.Size()) pivo.Add(tb[jl].id(),tb[jl].amplitude());
          pivo.Add(tb[jr].id(),tb[jr].amplitude());
	  tb[jl]+=tb[jr];
	}
      }
      tb.resize(jl+1);
    }
  }
  return nadd;
}
