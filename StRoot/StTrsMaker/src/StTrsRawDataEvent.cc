/***************************************************************************
 *
 * $Id: StTrsRawDataEvent.cc,v 1.3 1999/03/24 22:19:58 lasiuk Exp $
 *
 * Author: bl prelim
 ***************************************************************************
 *
 * Description: Contains a complete event of raw data
 *
 ***************************************************************************
 *
 * $Log: StTrsRawDataEvent.cc,v $
 * Revision 1.3  1999/03/24 22:19:58  lasiuk
 * actively clean up pointers in destructors
 *
 * Revision 1.2  1999/02/14 20:43:24  lasiuk
 * indexing (ii) and debug info
 *
 * Revision 1.1  1999/02/04 18:36:43  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include "StTrsRawDataEvent.hh"

StTrsRawDataEvent::StTrsRawDataEvent()
{
    // Please do not hard-code this
    // You should actually pass the geometry data
    // in the constructor and you could read
    // the total number of sectors from there
    // USE resize() for LINUX compatibility

    //cout << "StTrsRawDataEvent::StTrsRawDataEvent()" << endl;
    mSectors.resize(24);
    //PR(mSectors.size());

    for(int ii=0; ii<mSectors.size(); ii++) {
	mSectors[ii] = 0;
    }
    
}

StTrsRawDataEvent::~StTrsRawDataEvent()
{
    //cout << "StTrsRawDataEvent::~StTrsRawDataEvent()" << endl;
    for(int ii=0; ii<mSectors.size(); ii++)
	delete mSectors[ii];
    mSectors.clear();
    //PR(mSectors.size());
}

unsigned long StTrsRawDataEvent::size()
{
    // This should return the size of the event
    // in memory (MB)?
    return 1;
}
