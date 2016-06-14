//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMessageCounter                                                     //
//                                                                      //
// This class manages message limiting in STAR. It is a singleton.      //
// Limits can be placed on message types (i.e. "I" for info messages)   //
// or on strings in messages (i.e. "dst_track empty")                   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StMessageCounter.h"
#include "StMessageStream.h"
#include <cstring>
using namespace std;
StMessageCounter* StMessageCounter::mInstance = 0;

//_____________________________________________________________________________
StMessageCounter::StMessageCounter() : ostrstream(),
limitMessage(" - COUNT LIMIT REACHED!\n") {
  messTypeList = StMessTypeList::Instance();
  yesLimits = 0;
  noLimits = 0;
  // Initialize buffer with 256 bytes
  *this << ch64 << ch64 << ch64 << ch64;
}
//_____________________________________________________________________________
StMessageCounter::~StMessageCounter() {
}
//_____________________________________________________________________________
StMessageCounter* StMessageCounter::Instance() {
  if (!mInstance) {
    mInstance = new StMessageCounter;
  }
  return mInstance;
}
//_____________________________________________________________________________
void StMessageCounter::SetLimit(const char* str, int n) {
  if (noLimits) return;
  if (!yesLimits && (n >= 0)) yesLimits = 1;
  const size_t len = strlen(str);
  char* temp;
  if (len==1) {                         // Limits by type
    int typeN = messTypeList->FindTypeNum(str);
    if (typeN) {
      if (limitTList[typeN] != -5)      // -5 means fixed with no limit
        limitTList[typeN] = n;
    } else {                            // Waiting list for type limits
      if (limitWList.size()) {
        size_t index = 0;
        for (curString=limitWList.begin(); curString!=limitWList.end();
	                                   curString++) {
          if (*str == *(*curString)) {  // Already in the waiting list
            if (limitWNList[index] != -5)  // -5 means fixed with no limit
              limitWNList[index] = n;
            return;
          }
          index++;
        }
      }
      temp = new char[len+1];
      temp[len] = 0;
      limitWList.push_back(strncpy(temp,str,len));
      limitWNList.push_back(n);
    }
  } else {                              // Limits by string
    size_t index=0;
    for (curString=limitList.begin(); curString!=limitList.end();
                                      curString++) {
      if (!strcmp(str,(*curString))) {
        if (limitNList[index] == -5) return;  // -5 means fixed with no limit
        if ((n < 0) && (n != -5)) {
          limitList.erase(curString);
          limitNList.erase(limitNList.begin()+index);
          limitNCountList.erase(limitNCountList.begin()+index);
        } else {
          limitNList[index] = n;
        }
        return;
      }
      index++;
    }
    if ((n < 0) && (n != -5)) return;
    temp = new char[len+1];
    temp[len] = 0;
    limitList.push_back(strncpy(temp,str,len));
    limitNList.push_back(n);
    limitNCountList.push_back(0);
  }
  return;
}
//_____________________________________________________________________________
int StMessageCounter::GetLimit(const char* str) {
  const size_t len = strlen(str);
  if (len==1) {                         // Limits by type
    int typeN = messTypeList->FindTypeNum(str);
    if (typeN) {
      return limitTList[typeN];
    } else {                            // Waiting list for type limits
      if (limitWList.size()) {
        size_t index = 0;
        for (curString=limitWList.begin(); curString!=limitWList.end();
	                                   curString++) {
          if (*str == *(*curString)) return limitWNList[index];
          index++;
        }
      }
    }
  } else {                              // Limits by string
    size_t index=0;
    for (curString=limitList.begin(); curString!=limitList.end();
                                      curString++) {
      if (!strcmp(str,(*curString))) return limitNList[index];
      index++;
    }
  }
  return -1;                            // Not found, no limit
}
//_____________________________________________________________________________
void StMessageCounter::ListLimits() {
  if (yesLimits) {
    myout << "StMessage Limits: negative limit means no limit, ";
    myout << "-5 means fixed with no limit\n";
    myout << "  Limits :   counts : on message types" << endl;
    size_t index=0;
    for (index = 1; index < limitTList.size(); index++) {
      myout.width(8);
      myout << limitTList[index] << " : ";
      myout.width(8);
      myout << limitTCountList[index] << " : ";
      myout << messTypeList->FindNumType(index) << " - ";
      myout << messTypeList->FindNumText(index) << endl;
    }
    for (index = 0; index < limitWList.size(); index++) {
      myout.width(8);
      myout << limitWNList[index] << " : ";
      myout.width(8);
      myout << 0 << " : ";
      myout << limitWList[index] << " - ";
      myout << "???" << endl;
    }
    myout << "  Limits :   counts : on message strings" << endl;
    index=0;
    for (curString=limitList.begin(); curString!=limitList.end();
                                      curString++) {
      myout.width(8);
      myout << limitNList[index] << " : ";
      myout.width(8);
      myout << limitNCountList[index++] << " : ";
      myout << (*curString) << endl;
    }
  } else {
    myout << "No limits have been set on messages." << endl;
  }
  return;
}
//_____________________________________________________________________________
int StMessageCounter::CheckLimit(char* mess, const char* type) {
  static const char* leader="St";
  static const char* colon =": ";
  static const char* stmess="StMessage: ";
  int printIt = 1;
  int typeN = messTypeList->FindTypeNum(type);
  int typeNewSize = limitTCountList[typeN] + 1;
  limitTCountList[typeN] = typeNewSize;

  if (yesLimits && (! noLimits)) {
    seekp(0);
    int limit = limitTList[typeN];
    if (typeNewSize == limit) {
      *this << leader;
      *this << (messTypeList->FindType(type)->Text());
      *this << colon << limitMessage;
    } else if ((limit >= 0) && (typeNewSize > limit)) {
      printIt = 0;
    }

    size_t index=0;
    for (curString=limitList.begin(); curString!=limitList.end();
                                      curString++) {
      if (strstr(mess,(*curString))) {
        int counts = limitNCountList[index] + 1;
        limitNCountList[index] = counts;
        limit = limitNList[index];
        if (counts==limit) {
          *this << stmess << (*curString) << limitMessage;
        } else if ((limit >= 0) && (counts > limit)) {
          printIt = 0;
        }
      }
      index++;
    }
    *this << ends;
  }
  return printIt;
}
//_____________________________________________________________________________
void StMessageCounter::AddType(const char* type) {
  limitTList.push_back(-1);
  limitTCountList.push_back(0);
  if (limitWList.size()) {
    size_t index=0;                                // Now check the waiting list
    for (curString=limitWList.begin(); curString!=limitWList.end();
                                       curString++) {
      if (*type == *(*curString)) {
        SetLimit((*curString),limitWNList[index]);
        limitWList.erase(curString);
        limitWNList.erase(limitWNList.begin()+index);
        return;
      }
      index++;
    }
  }
}

//_____________________________________________________________________________
// $Id: StMessageCounter.cxx,v 1.22 2016/06/14 06:26:34 genevb Exp $
// $Log: StMessageCounter.cxx,v $
// Revision 1.22  2016/06/14 06:26:34  genevb
// better initializations (Coverity)
//
// Revision 1.21  2012/06/11 15:05:34  fisyak
// std namespace
//
// Revision 1.20  2009/08/26 19:39:04  fine
// fix the compilation issues under SL5_64_bits  gcc 4.3.2
//
// Revision 1.19  2003/10/01 20:06:50  genevb
// Initialize and test ostrstream buffer sizes (support for gcc before 3.2)
//
// Revision 1.18  2003/09/25 21:19:22  genevb
// Some new cout-like functions and friend functions, some doxygen-ization
//
// Revision 1.17  2003/09/02 17:59:20  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.16  2000/06/07 00:05:36  genevb
// Added FixOn(), enforcing no limits on a specific message type/string
//
// Revision 1.15  2000/03/30 16:12:55  genevb
// Add NoLimits() capability to turn off message limiting.
//
// Revision 1.14  2000/03/24 14:48:39  genevb
// Insurance on end-of-strings
//
// Revision 1.13  2000/01/05 19:53:46  genevb
// Fixed CC5 warnings, and several other small improvements under the hood
//
// Revision 1.12  1999/07/17 00:38:03  genevb
// Small typo
//
// Revision 1.11  1999/07/17 00:23:23  genevb
// Fixed bug when option fields are empty in FORTRAN, and let type limits be set before types are even added
//
// Revision 1.10  1999/07/15 05:15:06  genevb
// Fixed an odd bug with seekp(0) on an empty stream buffer
//
// Revision 1.9  1999/07/01 01:24:46  genevb
// Fixed FORTRAN character string bug on linux, removed a memory leak from Summary()
//
// Revision 1.7  1999/06/30 04:18:45  genevb
// Fixes: summary wrap-around, unsigned ints, last character of message, <> for time; no KNOWN remaining bugs
//
// Revision 1.6  1999/06/29 17:37:31  genevb
// Lots of fixes...
//
// Revision 1.5  1999/06/28 02:40:55  genevb
// Additional backward compatibilit with MSG (msg_enable, msg_enabled, msg_disable
//
// Revision 1.4  1999/06/26 00:24:52  genevb
// Fixed const type mismatches
//
// Revision 1.3  1999/06/25 22:57:56  genevb
// Fixed a small bug in MSG compatibiliti
//
// Revision 1.2  1999/06/24 16:30:42  genevb
// Fixed some memory leaks
//
// Revision 1.1  1999/06/23 15:17:49  genevb
// Introduction of StMessageManager
//
