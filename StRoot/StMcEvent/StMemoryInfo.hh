/***************************************************************************
 *
 * $Id: StMemoryInfo.hh,v 1.1.1.1 1999/07/13 18:24:18 uid2620 Exp $
 *
 * Author: Thomas Ullrich, June 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMemoryInfo.hh,v $
 * Revision 1.1.1.1  1999/07/13 18:24:18  uid2620
 * Initial import
 *
 * Revision 1.1  1999/06/04 17:57:02  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StMemoryInfo_hh
#define StMemoryInfo_hh

#include <malloc.h>
#include <iostream.h>

#ifndef ST_NO_NAMESPACES
using namespace std;
#endif

class StMemoryInfo {
public:
    static StMemoryInfo* instance();
    void   snapshot();
    void   print(ostream& = cout);
    
private:
    StMemoryInfo();
    StMemoryInfo(const StMemoryInfo &);
    const StMemoryInfo & operator=(const StMemoryInfo &);

    void   printLine(ostream&, const char*, int, int, const char* = 0);
    static StMemoryInfo* mMemoryInfo;
    struct mallinfo      mInfo;
    struct mallinfo      mOldInfo;
    size_t               mCounter;
};

#endif
