/***************************************************************************
 *
 * $Id: StTrsIos.hh,v 1.4 2003/09/02 17:59:16 perev Exp $
 *
 * Author: Manuel Calderon de la Barca Sanchez 
 ***************************************************************************
 *
 * Description: Base class for the Istream and Ostream classes for
 *              reading & writing TRS data.
 ***************************************************************************
 *
 * $Log: StTrsIos.hh,v $
 * Revision 1.4  2003/09/02 17:59:16  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2000/01/10 23:11:32  lasiuk
 * Include MACROS for compatibility with SUN CC5.0
 *
 * Revision 1.2  1999/12/08 02:10:25  calderon
 * Modified to eliminate warnings on Linux.
 *
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

#include "Stiostream.h"
#include <assert.h>
#include <string>
#include <vector>
#include <algorithm>

#if defined (__SUNPRO_CC) && __SUNPRO_CC >= 0x500
using std::vector;
using std::string;
#endif

class StTpcGeometry;

class StTrsIos {

public:
    
    virtual ~StTrsIos() {};

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
