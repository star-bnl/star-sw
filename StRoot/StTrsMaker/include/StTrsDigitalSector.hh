/***************************************************************************
 *
 * $Id: StTrsDigitalSector.hh,v 1.3 1999/10/11 23:55:10 calderon Exp $
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
 * Revision 1.3  1999/10/11 23:55:10  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 * Revision 1.2  1999/01/22 08:06:03  lasiuk
 * use unsigned char for compatibilty with interface.
 * requires use of two arrays...ugly but fine for now.
 * use of pair<>; values returned by pointer
 *
 * Revision 1.1  1999/01/18 10:23:42  lasiuk
 * initial Revision
 *
 *
 **************************************************************************/
#ifndef ST_TRS_DIGITAL_SECTOR_HH
#define ST_TRS_DIGITAL_SECTOR_HH

#include <vector>
#include <utility>

#include "StTrsAnalogSignal.hh"
#include "StTpcGeometry.hh"

#include "StTpcPadCoordinate.hh"

#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<unsigned char>      digitalTimeBins;
typedef vector<digitalTimeBins>    digitalPadRow;
typedef vector<digitalPadRow>      digitalSector;

typedef vector<unsigned char>::iterator digitialTimeBinIterator;
#else
typedef vector<unsigned char, allocator<unsigned char> >     digitalTimeBins;
typedef vector<digitalTimeBins, allocator<digitalTimeBins> > digitalPadRow;
typedef vector<digitalPadRow, allocator<digitalPadRow> >     digitalSector;

typedef vector<unsigned char, allocator<unsigned char> >::iterator digitalTimeBinIterator;
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
    pair<digitalTimeBins*, digitalTimeBins*>   timeBinsOfRowAndPad(int, int);
    pair<digitalPadRow*,   digitalPadRow*>     padsOfRow(int);
    pair<digitalSector*,   digitalSector*>     rows();

    int  numberOfRows()             const;
    int  numberOfPadsInRow(int)     const;
    int  numberOfTimeBins(int, int) const;
    
    // Adding
    void clear();

    void assignTimeBins(int, int, pair<digitalTimeBins*, digitalTimeBins*> );
    void assignTimeBins(StTpcPadCoordinate&, pair<digitalTimeBins*, digitalTimeBins*>);
    // When writing, make sure we don't carry unnecessary zeros:
    int cleanup();
public:
    digitalSector mData;
    digitalSector mZeros;
};
inline pair<digitalTimeBins*,digitalTimeBins*> StTrsDigitalSector::timeBinsOfRowAndPad(int rowN, int padN)
{
    pair<digitalTimeBins*,digitalTimeBins*> a(&mData[(rowN-1)][(padN-1)],&mZeros[(rowN-1)][(padN-1)]);
    return (a);
}
inline pair<digitalPadRow*,digitalPadRow*> StTrsDigitalSector::padsOfRow(int rowN)
{
    pair<digitalPadRow*,digitalPadRow*> a(&mData[(rowN-1)],&mZeros[(rowN-1)]);
    return(a);
}
inline pair<digitalSector*,digitalSector*> StTrsDigitalSector::rows()
{
    pair<digitalSector*,digitalSector*> a(&mData,&mZeros);
    return (a);
}
inline int StTrsDigitalSector::numberOfRows() const { return mData.size();}
inline int StTrsDigitalSector::numberOfPadsInRow(int rowN) const { return mData[(rowN-1)].size();}
inline int StTrsDigitalSector::numberOfTimeBins(int rowN, int padN) const { return mData[(rowN-1)][(padN-1)].size();}
#endif
