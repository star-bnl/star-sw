//*************************************************************
// $Id: StRrsReader.h,v 1.1 2000/01/18 21:32:05 lasiuk Exp $
//
// $Log: StRrsReader.h,v $
// Revision 1.1  2000/01/18 21:32:05  lasiuk
// Initial Revision
//
//*************************************************************
 * Revision 1.1  2000/01/18 21:32:05  lasiuk
 * Initial Revision
 **************************************************************/
#ifndef ST_RRS_READER
#define ST_RRS_READER

#include "StDaqLib/RICH/RICH_Reader.hh"


//namespace StRichRawData {
#endif
    class StRrsReader : public StRichReaderInterface {
    public:
	StRrsReader(); // shouldn't really call this one
	               // (but ifso must use setEvent)
	StRrsReader(StRichPadPlane*, int);
	~StRrsReader();
    // MC Event ONLY!
	unsigned short GetADCFromCoord(int x, int y);
	unsigned short GetADCFromCramChannel(int cramBlock, int channelNum);
	unsigned int   GetEventNumber();

	void setEvent(StRichPadPlane*);
	void setVersion(int);
    
    private:
	StRichPadPlane *mData;
	int       mVersion;
    };
    StRichPadPlane *mData;
    inline
    void StRrsReader::setEvent(StRichPadPlane* data) { mData = data; }

    inline
    void StRrsReader::setVersion(int ver) { mVersion = ver; }

inline
void StRrsReader::setVersion(int ver) { mVersion = ver; }

#ifndef ST_NO_NAMESPACES
//}
#endif

#endif
