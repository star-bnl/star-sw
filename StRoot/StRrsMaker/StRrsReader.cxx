//*************************************************************
// $Id: StRrsReader.cxx,v 1.5 2000/04/05 16:08:27 lasiuk Exp $
//
// $Log: StRrsReader.cxx,v $
// Revision 1.5  2000/04/05 16:08:27  lasiuk
// row/col order
//
// Revision 1.4  2000/03/12 23:50:10  lasiuk
// order of arguments
//
// Revision 1.3  2000/02/14 01:06:49  lasiuk
// change structure StRichID
//
// Revision 1.2  2000/01/25 22:02:23  lasiuk
// Second Revision
//
// Revision 1.1  2000/01/18 21:32:05  lasiuk
// Initial Revision
//
//*************************************************************
#ifndef ST_NO_NAMESPACES
//namespace StRichRawData {
#endif
#include "StRrsReader.h"

StRrsReader::StRrsReader()
    : mData(0) { /* nopt */ }

StRrsReader::StRrsReader(StRichPadPlane* data, int version)
    : mData(data), mVersion(version) { /* nopt */ }

StRrsReader::~StRrsReader() { /* nopt */ }

unsigned short StRrsReader::GetADCFromCoord(int col, int row)
{
    // check Writer
    return static_cast<unsigned short>((*mData)[row][col].signal); 
}

//typedef list<StRichID> anIDList;
//int G_ID
//double amount
anIDList StRrsReader::GetMCDetectorInfo(int col, int row)
{
    return ( (*mData)[row][col].IDs );
}

unsigned short StRrsReader::GetADCFromCramChannel(int cramBlock, int channelNum)
{
    // This is not implemented yet
    return (999);
}

unsigned int StRrsReader::GetEventNumber()
{
    return (9999);
}

#ifndef ST_NO_NAMESPACES
//}
#endif
