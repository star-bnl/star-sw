/***************************************************************************
 *
 * $Id: StMixerIos.hh,v 1.1 2000/02/16 21:02:09 pfachini Exp $
 *
 * Author: 
 *
 ***************************************************************************
 *
 * Description: Base class for the Istream and Ostream classes for
 *              reading & writing TRS data.
 ***************************************************************************
 *
 **************************************************************************/
#ifndef ST_MIXER_IOS_HH
#define ST_MIXER_IOS_HH

#include <fstream.h>
#include <assert.h>
#include <string>
#include <vector>
#include <algorithm>

class StTpcGeometry;

class StMixerIos {

public:
    
    virtual ~StMixerIos() {};

protected:
    unsigned int mEvents;
    unsigned int mSectors;
    unsigned int mRows;
    StTpcGeometry* mGeomDb;
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<int> padsAtRow;
#else
    vector<int, allocator<int> > padsAtRow;
#endif
};
#endif
