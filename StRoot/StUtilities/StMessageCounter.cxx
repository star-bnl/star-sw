// $Id: StMessageCounter.cxx,v 1.13 2000/01/05 19:53:46 genevb Exp $
// $Log: StMessageCounter.cxx,v $
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
//
// Revision 1.1 1999/01/27 10:28:29 genevb
//
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

StMessageCounter* StMessageCounter::mInstance = 0;

//_____________________________________________________________________________
StMessageCounter::StMessageCounter() : ostrstream(new char[4096],4096,ios::out),
limitMessage(" - COUNT LIMIT REACHED!\n") {
  messTypeList = StMessTypeList::Instance();
  yesLimits = 0;
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
  if (!yesLimits && (n >= 0)) yesLimits = 1;
  const size_t len = strlen(str);
  char* temp;
  if (len==1) {                         // Limits by type
    int typeN = messTypeList->FindTypeNum(str);
    if (typeN) {
      limitTList[typeN] = n;
    } else {                            // Waiting list for type limits
      if (limitWList.size()) {
        index = 0;
        for (curString=limitWList.begin(); curString!=limitWList.end(); curString++) {
          if (*str == *(*curString)) {  // Already in the waiting list
            limitWCountList[index] = n;
            return;
          }
          index++;
        }
      }
      temp = new char[len];
      limitWList.push_back(strcpy(temp,str));
      limitWCountList.push_back(n);
    }
  } else {                              // Limits by string
    index=0;
    for (curString=limitList.begin(); curString!=limitList.end(); curString++) {
      if (!strcmp(str,(*curString))) {
        if (n < 0) {
          limitList.erase(curString);
          limitNList.erase(&(limitNList[index]));
          limitNCountList.erase(&(limitNCountList[index]));
        } else {
          limitNList[index] = n;
        }
        return;
      }
      index++;
    }
    if (n < 0) return;
    temp = new char[len];
    limitList.push_back(strcpy(temp,str));
    limitNList.push_back(n);
    limitNCountList.push_back(0);
  }
  return;
}
//_____________________________________________________________________________
int StMessageCounter::GetLimit(const char* str) {
  if (yesLimits) {
    index=0;
    for (curString=limitList.begin(); curString!=limitList.end(); curString++) {
      if (!strcmp(str,(*curString))) {
        return limitNList[index];
      }
      index++;
    }
  }
  return -1;
}
//_____________________________________________________________________________
void StMessageCounter::ListLimits() {
  if (yesLimits) {
    cout << "  Limits :   counts : on message types";
    cout << " (a negative limit means no limit)" << endl;
    for (index = 1; index < limitTList.size(); index++) {
      cout.width(8);
      cout << limitTList[index] << " : ";
      cout.width(8);
      cout << limitTCountList[index] << " : ";
      cout << messTypeList->FindNumType(index) << " - ";
      cout << messTypeList->FindNumText(index) << endl;
    }
    for (index = 0; index < limitWList.size(); index++) {
      cout.width(8);
      cout << limitWCountList[index] << " : ";
      cout.width(8);
      cout << 0 << " : ";
      cout << limitWList[index] << " - ";
      cout << "???" << endl;
    }
    cout << "  Limits :   counts : on message strings" << endl;
    index=0;
    for (curString=limitList.begin(); curString!=limitList.end(); curString++) {
      cout.width(8);
      cout << limitNList[index] << " : ";
      cout.width(8);
      cout << limitNCountList[index++] << " : ";
      cout << (*curString) << endl;
    }
  } else {
    cout << "No limits have been set on messages." << endl;
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

  if (yesLimits) {
    seekp(0);
    int limit = limitTList[typeN];
    if (typeNewSize == limit) {
      *this << leader;
      *this << (messTypeList->FindType(type)->Text());
      *this << colon << limitMessage;
    } else if ((limit >= 0) && (typeNewSize > limit)) {
      printIt = 0;
    }

    index=0;
    for (curString=limitList.begin(); curString!=limitList.end(); curString++) {
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
    index=0;                                // Now check the waiting list
    for (curString=limitWList.begin(); curString!=limitWList.end(); curString++) {
      if (*type == *(*curString)) {
        SetLimit((*curString),limitWCountList[index]);
        limitWList.erase(curString);
        limitWCountList.erase(&(limitWCountList[index]));
        return;
      }
      index++;
    }
  }
}
