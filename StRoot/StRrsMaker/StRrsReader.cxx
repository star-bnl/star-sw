//*************************************************************
// $Id: StRrsReader.cxx,v 1.3 2000/02/14 01:06:49 lasiuk Exp $
//
// $Log: StRrsReader.cxx,v $
// Revision 1.3  2000/02/14 01:06:49  lasiuk
// change structure StRichID
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
unsigned short StRrsReader::GetADCFromCoord(int row, int col)
StRrsReader::~StRrsReader() { /* nopt */ }

unsigned short StRrsReader::GetADCFromCoord(int col, int row)
{
    // check Writer
    return static_cast<unsigned short>((*mData)[row][col].signal); 
}

//typedef list<StRichID> anIDList;
//int G_ID
//double amount
anIDList StRrsReader::GetMCDetectorInfo(int row, int col)
{
    anIDList myList;
    anIDList::iterator iter;
    for (iter  = (*mData)[row][col].IDs.begin();
	 iter != (*mData)[row][col].IDs.end();
	 ++iter ) {
	//g_id.id  = (*k).G_ID;
// 	double charge;
// 	if ( (*mData)[row][col].signal )
// 	    charge = (*iter).mAmount / (*mData)[row][col].signal;
// 	else
// 	    charge = 1;

	  StRichID anID((*iter).mG_ID, (*iter).mTrackp, (*iter).mAmount);
	  myList.push_back(anID);
	}   

    return myList;
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
