/***************************************************************************
 *
 * $Id: TMemoryInfo.h,v 1.1 2009/12/01 01:33:33 fine Exp $
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: TMemoryInfo.h,v $
 * Revision 1.1  2009/12/01 01:33:33  fine
 * Move online display udner OnlTools
 *
 * Revision 1.1  2007/10/26 14:12:29  fine
 *  add the header files
 *
 *
 **************************************************************************/
#ifndef TMemoryInfo_h
#define TMemoryInfo_h
#include "Riostream.h"

#if 1
#ifndef __CINT__
# include <malloc.h>
// # include <iostream.h>
#else
//  class ostream;
//  ostream &cout;
#endif
#endif

class TMemoryInfo {
public:
    static TMemoryInfo* Instance();
    void   Snapshot();
    void   Print(ostream& = cout);
    
    friend class nobody;

private:
    TMemoryInfo();
    TMemoryInfo(const TMemoryInfo &);
    const TMemoryInfo & operator=(const TMemoryInfo &);

    void   PrintLine(ostream&, const char*, int, int, const char* = 0);
    static TMemoryInfo* fgMemoryInfo;
    struct mallinfo      fInfo;
    struct mallinfo      fOldInfo;
    size_t               fCounter;
};

#endif
