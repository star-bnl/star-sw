/***************************************************************************
 *
 * $Id: StTrsDigitalSector.hh,v 1.1 1999/01/18 10:23:42 lasiuk Exp $
 *
 * Author: bl prelim
 ***************************************************************************
 *
 * Description: Store the digital information in a space
 *              efficient manner:
 * Data:
 * TimeBin   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15...
 * ADC Vaue  0  0  0  5  9  4  0  0  0  0  7  9  3  0  0  1...
 *
 * will be stored as an array of short in a compressed manner
 * where the ADC values remain and the number of zeros are compressed
 * and stored as a negative number.  The data above will be stored
 * as:
 *
 *           -3 5 9 4 -4 7 9 3 -2 1....
 *           
 *  This will be the format coming from the DigitalSignalGenerator
 *  NOTE: Encapsulation is broken in this class to allow easy
 *        access to the data via the TPC data reader interface!
 ***************************************************************************
 *
 * $Log: StTrsDigitalSector.hh,v $
 * Revision 1.1  1999/01/18 10:23:42  lasiuk
 * initial Revision
 *
 * Revision 1.1  1999/01/18 10:23:42  lasiuk
 * initial Revision
 *
 *
 **************************************************************************/
#ifndef ST_TRS_DIGITAL_SECTOR_HH
#define ST_TRS_DIGITAL_SECTOR_HH

#include <vector>
#include "StTrsAnalogSignal.hh"
#include "StTpcGeometry.hh"

#include "StTpcPadCoordinate.hh"

#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<short>              digitalTimeBins;
typedef vector<digitalTimeBins>    digitalPadRow;
typedef vector<digitalPadRow>      digitalSector;

typedef vector<short>::iterator digitialTimeBinIterator;
#else
typedef vector<short, allocator<short> >                       digitalTimeBins;
typedef vector<digitalTimeBins, allocator<digitalTimeBins> > digitalPadRow;
typedef vector<digitalPadRow, allocator<digitalPadRow> >     digitalSector;

typedef vector<short, allocator<short> >::iterator digitalTimeBinIterator;
#endif

typedef digitalPadRow::iterator                  digitalPadRowIterator;
typedef digitalSector::iterator                  digitalRowIterator;

class StTrsDigitalSector {
public:
    StTrsDigitalSector(StTpcGeometry*);
    ~StTrsDigitalSector();
    
    //StTrsDigitalSector(const StTrsDigitalSector&);
    //StTrsDigitalSector& operator=(const StTrsDigitalSector&);

    // access functions
    digitalTimeBins&   timeBinsOfRowAndPad(int, int);
    digitalPadRow&     padsOfRow(int);
    digitalSector&     rows();

    int  numberOfRows()             const;
    int  numberOfPadsInRow(int)     const;
    int  numberOfTimeBins(int, int) const;
    
    // Adding
    void clear();

    void assignTimeBins(int, int, digitalTimeBins&);
    void assignTimeBins(StTpcPadCoordinate&, digitalTimeBins&);

public:
    digitalSector mSector;
};
inline digitalTimeBins& StTrsDigitalSector::timeBinsOfRowAndPad(int rowN, int padN) { return (mSector[(rowN-1)][(padN-1)]); }
inline digitalPadRow&   StTrsDigitalSector::padsOfRow(int rowN) { return (mSector[(rowN-1)]); }
inline digitalSector&   StTrsDigitalSector::rows() { return (mSector); }
inline int StTrsDigitalSector::numberOfRows() const { return mSector.size();}
inline int StTrsDigitalSector::numberOfPadsInRow(int rowN) const { return mSector[(rowN-1)].size();}
inline int StTrsDigitalSector::numberOfTimeBins(int rowN, int padN) const { return mSector[(rowN-1)][(padN-1)].size();}
#endif
