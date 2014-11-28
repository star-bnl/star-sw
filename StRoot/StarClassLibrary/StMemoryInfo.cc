/***************************************************************************
 *
 * $Id: StMemoryInfo.cc,v 1.5 2013/01/17 14:40:04 fisyak Exp $
 *
 * Author: Thomas Ullrich, June 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMemoryInfo.cc,v $
 * Revision 1.5  2013/01/17 14:40:04  fisyak
 * Add APPLE
 *
 * Revision 1.4  2012/06/11 15:29:26  fisyak
 * std namespace
 *
 * Revision 1.3  1999/12/21 15:14:16  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.2  1999/11/05 18:10:47  ullrich
 * Added blank line as last printed line.
 *
 * Revision 1.1  1999/06/04 17:57:01  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StMemoryInfo.hh"

StMemoryInfo* StMemoryInfo::mMemoryInfo = 0;

StMemoryInfo* StMemoryInfo::instance()
{
    if (!mMemoryInfo)
	mMemoryInfo = new StMemoryInfo();
    return mMemoryInfo;
}

StMemoryInfo::StMemoryInfo()
{
#if !defined(__SUNPRO_CC) && !defined(__APPLE__)
    mInfo = mOldInfo = mallinfo();
#endif
    mCounter = 0;
}

StMemoryInfo::StMemoryInfo(const StMemoryInfo &) {/* private */}

const StMemoryInfo&
StMemoryInfo::operator=(const StMemoryInfo &) {/* private */ return *this;}

void StMemoryInfo::snapshot()
{
#ifndef __APPLE__
    mOldInfo = mInfo;
#endif
#if !defined(__SUNPRO_CC) && !defined(__APPLE__)
    mInfo = mallinfo();
#endif
    mCounter++;
}

void StMemoryInfo::printLine(ostream& os, const char* str, int cur, int old, const char* unit)
{
    os.width(40);
    //    os.fill('.');
    os.setf(std::ios::left);
    os << str << ' ' << cur << " (";
    os.setf(std::ios::showpos);
    os << cur-old << ") ";
    if (unit) os << unit;
    os << endl;
    os.unsetf(std::ios::showpos);
}

void StMemoryInfo::print(ostream& os)
{   
    os << "---------- Memory Status (snapshot #" << mCounter << ") ----------" << endl;
#if defined(__SUNPRO_CC) || defined(__APPLE__)
    os << "Sorry, StMemoryInfo is not supported on SUN." << endl;    
#elif defined(__GNUC__)
    printLine(os, "total space allocated from system", mInfo.arena, mOldInfo.arena);
    printLine(os, "number of non-inuse chunks", mInfo.ordblks, mOldInfo.ordblks);
    printLine(os, "number of mmapped regions", mInfo.hblks, mOldInfo.hblks);
    printLine(os, "total space in mmapped regions", mInfo.hblkhd, mOldInfo.hblkhd);
    printLine(os, "total allocated space", mInfo.uordblks, mOldInfo.uordblks);
    printLine(os, "total non-inuse space", mInfo.fordblks, mOldInfo.fordblks);
    printLine(os, "top-most, releasable space", mInfo.keepcost, mOldInfo.keepcost);
#else
    printLine(os, "total space in arena", mInfo.arena, mOldInfo.arena);                    
    printLine(os, "number of ordinary blocks", mInfo.ordblks, mOldInfo.ordblks);               
    printLine(os, "number of small blocks", mInfo.smblks, mOldInfo.smblks);                  
    printLine(os, "space in holding block headers", mInfo.hblks, mOldInfo.hblks);          
    printLine(os, "number of holding blocks", mInfo.hblkhd, mOldInfo.hblkhd);                
    printLine(os, "space in small blocks in use", mInfo.usmblks, mOldInfo.usmblks);            
    printLine(os, "space in free small blocks", mInfo.fsmblks, mOldInfo.fsmblks);              
    printLine(os, "space in ordinary blocks in use", mInfo.uordblks, mOldInfo.uordblks);         
    printLine(os, "space in free ordinary blocks", mInfo.fordblks, mOldInfo.fordblks);          
    printLine(os, "space penalty if keep option is used", mInfo.keepcost, mOldInfo.keepcost);
#endif
    os << endl;
}
