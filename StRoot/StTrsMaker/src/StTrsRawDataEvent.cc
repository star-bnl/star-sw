/***************************************************************************
 *
 * $Id: StTrsRawDataEvent.cc,v 1.5 1999/10/11 23:55:23 calderon Exp $
 *
 * Author: bl prelim
 ***************************************************************************
 *
 * Description: Contains a complete event of raw data
 *
 ***************************************************************************
 *
 * $Log: StTrsRawDataEvent.cc,v $
 * Revision 1.5  1999/10/11 23:55:23  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 * Revision 1.4  1999/04/29 00:16:17  lasiuk
 * add the member function clear() to take care of
 * allocated memory within the event loop.
 *
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

StTrsRawDataEvent::StTrsRawDataEvent(int numSectors)
{
    // Please do not hard-code this
    // You should actually pass the geometry data
    // in the constructor and you could read
    // the total number of sectors from there
    // USE resize() for LINUX compatibility

    //cout << "StTrsRawDataEvent::StTrsRawDataEvent()" << endl;
    mSectors.resize(numSectors);
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

void StTrsRawDataEvent::clear()
{
    cout << "StTrsRawDataEvent::clear()" << endl;
    for(int ii=0; ii<mSectors.size(); ii++) {
	delete mSectors[ii];
	mSectors[ii] = 0;     // remember to make sure pointer in NULL
    }
}

unsigned long StTrsRawDataEvent::size()
{
    // This should return the size of the event
    // in memory (MB)?
    return 1;
}
