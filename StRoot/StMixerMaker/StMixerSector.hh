/***************************************************************************
 *
 * $Id: StMixerSector.hh,v 1.1 2000/02/16 21:02:10 pfachini Exp $
 *
 * Author: Patricia Fachini - a copy from StTrsSector.hh
 *
 ***************************************************************************
 *
 * Description: Keeps the analog (and later) digital information
 *              in the sector's time bins
 *
 ***************************************************************************
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_MIXER_SECTOR_HH
#define ST_MIXER_SECTOR_HH

#include <vector>
#include "StMixerAnalogSignal.hh"
#include "StTrsMaker/include/StTpcGeometry.hh"

#include "StTrsMaker/include/StTpcPadCoordinate.hh"

#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StMixerAnalogSignal>  tpcTimeBins;
typedef vector<tpcTimeBins>        tpcPadRow;
typedef vector<tpcPadRow>          tpcSector;

typedef vector<StMixerAnalogSignal>::iterator timeBinIterator;
#else
typedef vector<StMixerAnalogSignal, allocator<StMixerAnalogSignal> > tpcTimeBins;
typedef vector<tpcTimeBins, allocator<tpcTimeBins> >             tpcPadRow;
typedef vector<tpcPadRow, allocator<tpcPadRow> >                 tpcSector;

typedef vector<StMixerAnalogSignal, allocator<StMixerAnalogSignal> >::iterator timeBinIterator;
#endif

typedef tpcPadRow::iterator                 padRowIterator;
typedef tpcSector::iterator                 rowIterator;

class StMixerSector {
public:
    StMixerSector(StTpcGeometry*);
    ~StMixerSector();
    
    //StMixerSector(const StMixerSector&);
    //StMixerSector& operator=(const StMixerSector&);

    // access functions
    tpcTimeBins&   timeBinsOfRowAndPad(int, int);
    tpcPadRow&     padsOfRow(int);
    tpcSector&     rows();

    int  size()                 const;
    int  numberOfRows()         const;
    int  numberOfPadsInRow(int) const;
    
    // Adding
    void clear();
    void addEntry(StTpcPadCoordinate&, StMixerAnalogSignal&);
    void addEntry(int, int, StMixerAnalogSignal&);            // row,pad

    void assignTimeBins(int, int, tpcTimeBins&);
    void assignTimeBins(StTpcPadCoordinate&, tpcTimeBins&);

private:
    tpcSector mSector;
};
inline tpcTimeBins& StMixerSector::timeBinsOfRowAndPad(int rowN, int padN) { return (mSector[(rowN-1)][(padN-1)]); }
inline tpcPadRow&   StMixerSector::padsOfRow(int rowN) { return (mSector[(rowN-1)]); }
inline tpcSector&   StMixerSector::rows() { return (mSector); }
inline int          StMixerSector::size() const { return mSector.size();}
inline int          StMixerSector::numberOfRows() const { return mSector.size();}
inline int          StMixerSector::numberOfPadsInRow(int rowN) const { return mSector[(rowN-1)].size();}
inline void StMixerSector::addEntry(int rowN, int padN, StMixerAnalogSignal& signl)
{
    
#ifdef ST_SECTOR_BOUNDS_CHECK
    if( (rowN > 0 && row <= mSector.size()) )
	if( (padN > 0 && pad <= mSector[(rowN-1)].size()) )
#endif
	    mSector[(rowN-1)][(padN-1)].push_back(signl);
}
inline void StMixerSector::assignTimeBins(int rowN, int padN, tpcTimeBins& tbins)
{
#ifdef ST_SECTOR_BOUNDS_CHECK
    if( (rowIndex > 0 && rowIndex <= mSector.size()) )
	if( (padIndex > 0 && padIndex <= mSector[rowIndex].size()) )
#endif
	    mSector[(rowN-1)][(padN-1)] = tbins;
}

#endif
