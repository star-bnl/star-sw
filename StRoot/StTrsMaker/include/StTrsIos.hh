/***************************************************************************
 *
 * $Id: StTrsIos.hh,v 1.1 1999/10/11 23:55:11 calderon Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez 
 ***************************************************************************
 *
 * Description: Base class for the Istream and Ostream classes for
 *              reading & writing TRS data.
 ***************************************************************************
 *
 * $Log: StTrsIos.hh,v $
 * Revision 1.1  1999/10/11 23:55:11  calderon
 * Version with Database Access and persistent file.
 * Not fully tested due to problems with cons, it
 * doesn't find the local files at compile time.
 * Yuri suggests forcing commit to work directly with
 * files in repository.
 *
 *
 *
 **************************************************************************/
#ifndef ST_TRS_IOS_HH
#define ST_TRS_IOS_HH

#include <fstream.h>
#include <assert.h>
#include <string>
#include <vector>
#include <algorithm>

class StTpcGeometry;

class StTrsIos {

public:
    
    virtual ~StTrsIos() {};

protected:
    int mEvents;
    int mSectors;
    int mRows;
    StTpcGeometry* mGeomDb;
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<int> padsAtRow;
#else
    vector<int, allocator<int> > padsAtRow;
#endif
};
#endif
