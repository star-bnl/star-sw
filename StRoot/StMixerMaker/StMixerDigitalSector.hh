/***************************************************************************
 *
 * $Id: StMixerDigitalSector.hh,v 1.1 2000/02/16 21:02:08 pfachini Exp $
 *
 * Author: Patricia Fachini - a copy from StTrsDigitalSector.hh
 *
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
 *           0 3 5 9 4 0 4 7 9 3 0 2 1....
 *           
 *  This will be the format coming from the DigitalSignalGenerator
 *  NOTE: Encapsulation is broken in this class to allow easy
 *        access to the data via the TPC data reader interface!
 ***************************************************************************
 *
 **************************************************************************/
#ifndef ST_TRS_DIGITAL_SECTOR_HH
#define ST_TRS_DIGITAL_SECTOR_HH

#include <vector>
#include <utility>

#include "StTrsMaker/include/StTrsAnalogSignal.hh"
#include "StTrsMaker/include/StTpcGeometry.hh"

#include "StTrsMaker/include/StTpcPadCoordinate.hh"

#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<unsigned char>      digitalTimeBins;
typedef vector<digitalTimeBins>    digitalPadRow;
typedef vector<digitalPadRow>      digitalSector;

typedef vector<unsigned char>::iterator digitalTimeBinIterator;
#else
typedef vector<unsigned char, allocator<unsigned char> >     digitalTimeBins;
typedef vector<digitalTimeBins, allocator<digitalTimeBins> > digitalPadRow;
typedef vector<digitalPadRow, allocator<digitalPadRow> >     digitalSector;

typedef vector<unsigned char, allocator<unsigned char> >::iterator digitalTimeBinIterator;
#endif

typedef digitalPadRow::iterator                  digitalPadRowIterator;
typedef digitalSector::iterator                  digitalRowIterator;

class StMixerDigitalSector {
public:
    StMixerDigitalSector(StTpcGeometry*);
    ~StMixerDigitalSector();
    

    // access functions
    digitalTimeBins*   timeBinsOfRowAndPad(int, int);
    digitalPadRow*     padsOfRow(int);
    digitalSector*     rows();

    int  numberOfRows()             const;
    int  numberOfPadsInRow(int)     const;
    int  numberOfTimeBins(int, int) const;
    
    // Adding
    void clear();

    void assignTimeBins(int, int, digitalTimeBins*);
    void assignTimeBins(StTpcPadCoordinate&, digitalTimeBins*);
    // When writing, make sure we don't carry unnecessary zeros:
    int cleanup();
public:
    digitalSector mData;
//     digitalSector mZeros;
private:
    StMixerDigitalSector(const StMixerDigitalSector&);
    StMixerDigitalSector& operator=(const StMixerDigitalSector&);

};
inline digitalTimeBins* StMixerDigitalSector::timeBinsOfRowAndPad(int rowN, int padN)
{
    return (&mData[(rowN-1)][(padN-1)]);
}
inline digitalPadRow* StMixerDigitalSector::padsOfRow(int rowN)
{
    return(&mData[(rowN-1)]);
}
inline digitalSector* StMixerDigitalSector::rows()
{
    return (&mData);
}
inline int StMixerDigitalSector::numberOfRows() const { return mData.size();}
inline int StMixerDigitalSector::numberOfPadsInRow(int rowN) const { return mData[(rowN-1)].size();}
inline int StMixerDigitalSector::numberOfTimeBins(int rowN, int padN) const { return mData[(rowN-1)][(padN-1)].size();}
#endif
