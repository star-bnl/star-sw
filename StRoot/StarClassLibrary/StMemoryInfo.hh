/***************************************************************************
 *
 * $Id: StMemoryInfo.hh,v 1.3 2000/01/04 14:57:54 ullrich Exp $
 *
 * Author: Thomas Ullrich, June 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMemoryInfo.hh,v $
 * Revision 1.3  2000/01/04 14:57:54  ullrich
 * Added friend declaration to avoid warning messages
 * under Linux.
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
    
    friend class nobody;

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
