/***************************************************************************
 *
 * $Id: StMemoryInfo.hh,v 1.5 2013/01/17 14:40:04 fisyak Exp $
 *
 * Author: Thomas Ullrich, June 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMemoryInfo.hh,v $
 * Revision 1.5  2013/01/17 14:40:04  fisyak
 * Add APPLE
 *
 * Revision 1.4  2003/09/02 17:59:35  perev
 * gcc 3.2 updates + WarnOff
 *
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

#ifndef __APPLE__
#include <malloc.h>
#endif
#include <Stiostream.h>

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
#ifndef __APPLE__
    struct mallinfo      mInfo;
    struct mallinfo      mOldInfo;
#endif
    size_t               mCounter;
};

#endif
