/***************************************************************************
 *
 * $Id: StMemoryInfo.hh,v 1.2 1999/12/21 15:14:18 ullrich Exp $
 *
 * Author: Thomas Ullrich, June 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMemoryInfo.hh,v $
 * Revision 1.2  1999/12/21 15:14:18  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.2  1999/12/21 15:14:18  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.1  1999/06/04 17:57:02  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StMemoryInfo_hh
#define StMemoryInfo_hh

#include <malloc.h>
#include <iostream.h>

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
