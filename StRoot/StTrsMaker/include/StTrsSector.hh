/***************************************************************************
 *
 * $Id: StTrsSector.hh,v 1.1 1998/11/10 17:12:12 fisyak Exp $
 *
 * Author: bl prelim
 ***************************************************************************
 *
 * Description: Keeps the analog (and later) digital information
 *              in the sector's time bins
 *
 ***************************************************************************
 *
 * $Log: StTrsSector.hh,v $
 * Revision 1.1  1998/11/10 17:12:12  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/11/10 17:12:12  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.3  1998/11/08 17:06:33  lasiuk
 * use resize() for LINUX compatibiltiy
 * allocator specification for vector<>
 *
 * Revision 1.2  1998/11/04 18:49:26  lasiuk
 * modify constructors
 * macro ST_SECTOR_BOUND_CHECK
 * addEntry()
 * addTimeBins()
 *
 * Revision 1.1  1998/06/30 22:54:10  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TRS_SECTOR_HH
#define ST_TRS_SECTOR_HH

#include <vector>
#include "StTrsAnalogSignal.hh"
#include "StTpcGeometry.hh"

#include "StTpcPadCoordinate.hh"

#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StTrsAnalogSignal>  tpcTimeBins;
typedef vector<tpcTimeBins>        tpcPadRow;
typedef vector<tpcPadRow>          tpcSector;

typedef vector<StTrsAnalogSignal>::iterator timeBinIterator;
#else
typedef vector<StTrsAnalogSignal, allocator<StTrsAnalogSignal> > tpcTimeBins;
typedef vector<tpcTimeBins, allocator<tpcTimeBins> >             tpcPadRow;
typedef vector<tpcPadRow, allocator<tpcPadRow> >                 tpcSector;

typedef vector<StTrsAnalogSignal, allocator<StTrsAnalogSignal> >::iterator timeBinIterator;
#endif

typedef tpcPadRow::iterator                 padRowIterator;
typedef tpcSector::iterator                 rowIterator;

class StTrsSector {
public:
    StTrsSector(StTpcGeometry*);
    ~StTrsSector();
    
    //StTrsSector(const StTrsSector&);
    //StTrsSector& operator=(const StTrsSector&);

    // access functions
    tpcTimeBins&   timeBinsOfRowAndPad(int, int);
    tpcPadRow&     padsOfRow(int);
    tpcSector&     rows();

    int  size()                 const;
    int  numberOfRows()         const;
    int  numberOfPadsInRow(int) const;
    
    // Adding
    void clear();
    void addEntry(StTpcPadCoordinate&, StTrsAnalogSignal&);
    void addEntry(int, int, StTrsAnalogSignal&);            // row,pad

    void assignTimeBins(int, int, tpcTimeBins&);
    void assignTimeBins(StTpcPadCoordinate&, tpcTimeBins&);

private:
    tpcSector mSector;
};
inline tpcTimeBins& StTrsSector::timeBinsOfRowAndPad(int row, int pad) { return (mSector[row][pad]); }
inline tpcPadRow&   StTrsSector::padsOfRow(int row) { return (mSector[row]); }
inline tpcSector&   StTrsSector::rows() { return (mSector); }
inline int          StTrsSector::size() const { return mSector.size();}
inline int          StTrsSector::numberOfRows() const { return mSector.size();}
inline int          StTrsSector::numberOfPadsInRow(int r) const { return mSector[r].size();}
#endif
