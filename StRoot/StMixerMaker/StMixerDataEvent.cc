/***************************************************************************
 *
 * $Id: StMixerDataEvent.cc,v 1.1 2000/02/16 21:02:07 pfachini Exp $
 *
 * Author: Patricia Fachini
 * 
 ***************************************************************************
 *
 * Description: Contains a complete event of mixed data
 *
 ***************************************************************************
 *
 **************************************************************************/
#include "StMixerDataEvent.hh"

StMixerDataEvent::StMixerDataEvent(int numSectors)
{
    // Please do not hard-code this
    // You should actually pass the geometry data
    // in the constructor and you could read
    // the total number of sectors from there
    // USE resize() for LINUX compatibility

    //cout << "StMixerDataEvent::StMixerDataEvent()" << endl;
    mSectors.resize(numSectors);
    //PR(mSectors.size());

    for(unsigned int ii=0; ii<mSectors.size(); ii++) {
	mSectors[ii] = 0;
    }
    
}

StMixerDataEvent::~StMixerDataEvent()
{
    //cout << "StMixerDataEvent::~StMixerDataEvent()" << endl;
    for(unsigned int ii=0; ii<mSectors.size(); ii++)
	delete mSectors[ii];
    mSectors.clear();
    //PR(mSectors.size());
}

void StMixerDataEvent::clear()
{
    cout << "StMixerDataEvent::clear()" << endl;
    for(unsigned int ii=0; ii<mSectors.size(); ii++) {
	delete mSectors[ii];
	mSectors[ii] = 0;     // remember to make sure pointer in NULL
    }
}

unsigned long StMixerDataEvent::size()
{
    // This should return the size of the event
    // in memory (MB)?
    return 1;
}
