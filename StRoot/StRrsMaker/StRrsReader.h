/*************************************************************
 * $Id: StRrsReader.h,v 1.3 2000/02/08 16:36:52 lasiuk Exp $
 *
 * $Log: StRrsReader.h,v $
 * Revision 1.3  2000/02/08 16:36:52  lasiuk
 * Bring into line with HP
 *
 * Revision 1.2  2000/01/25 22:02:23  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/18 21:32:05  lasiuk
 * Initial Revision
 **************************************************************/
#ifndef ST_RRS_READER
#define ST_RRS_READER

#ifdef __ROOT__
#include "StDaqLib/RICH/RICH_Reader.hh"
#endif

#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
#include "StRichRrsMacros.h"
#include "StRichPadPlane.h"
    
class StRrsReader
#ifdef __ROOT__
    : public StRichReaderInterface
#endif
{
public:
    StRrsReader(); // shouldn't really call this one
    // (but ifso must use setEvent)
    StRrsReader(StRichPadPlane*, int);
    ~StRrsReader();
    
    unsigned short GetADCFromCoord(int x, int y);
    unsigned short GetADCFromCramChannel(int cramBlock, int channelNum);
    unsigned int   GetEventNumber();

    //
    // MC Event ONLY!
    //list<StRichID>
    anIDList GetMCDetectorInfo(int row, int col);
    
    void setEvent(StRichPadPlane*);
    void setVersion(int);
    
private:
    StRichPadPlane *mData;
    int             mVersion;
};

inline
void StRrsReader::setEvent(StRichPadPlane* data) { mData = data; }

inline
void StRrsReader::setVersion(int ver) { mVersion = ver; }

#ifndef ST_NO_NAMESPACES
//}
#endif

#endif
