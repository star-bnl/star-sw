//*************************************************************
// $Id: StRrsReader.cxx,v 1.1 2000/01/18 21:32:05 lasiuk Exp $
//
// $Log: StRrsReader.cxx,v $
// Revision 1.1  2000/01/18 21:32:05  lasiuk
// Initial Revision
//
// Revision 1.1  2000/01/18 21:32:05  lasiuk
// Initial Revision
//
//*************************************************************
#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
    StRrsReader::StRrsReader()
	: mData(0) { /* nopt */ }

    StRrsReader::StRrsReader(StRichPadPlane* data, int version)
	: mData(data), mVersion(version) { /* nopt */ }
    // This is not implemented yet
    StRrsReader::~StRrsReader() { /* nopt */ }
    
    unsigned short StRrsReader::GetADCFromCoord(int row, int col)
    {
	// check Writer
	return static_cast<unsigned short>((*mData)[row][col].signal); 
    }

    unsigned short StRrsReader::GetADCFromCramChannel(int cramBlock, int channelNum)
    {
	// This is not implemented yet
	return (-999);
    }

    unsigned int StRrsReader::GetEventNumber()
    {
	return -9999;
    }
{
    return (9999);
}

#ifndef ST_NO_NAMESPACES
//}
#endif
