// $Id: StMessageCounter.cxx,v 1.8 1999/06/30 17:24:50 genevb Exp $
// $Log: StMessageCounter.cxx,v $
// Revision 1.8  1999/06/30 17:24:50  genevb
// Better limit management, remove Bool_t
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
StMessageCounter::StMessageCounter() : ostrstream(new char[4096],4096),
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
void StMessageCounter::SetLimit(char* str, int n) {
  yesLimits = 1;
  const size_t len = strlen(str);
  if (len==1) {
    int typeN = messTypeList->FindTypeNum(str);
    if (typeN) {
      limitTList[typeN] = n;
    }
  } else {
    messCharVecIter curString;
    int index=0;
    for (curString=limitList.begin(); curString!=limitList.end(); curString++) {
      if (!strcmp(str,(*curString))) {
        if (n<0) {
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
    char* temp = new char[len];
    strcpy(temp,str);
    limitList.push_back(temp);
    limitNList.push_back(n);
    limitNCountList.push_back(0);
  }
  return;
}
//_____________________________________________________________________________
int StMessageCounter::GetLimit(char* str) {
  if (yesLimits) {
    messCharVecIter curString;
    int index=0;
    for (curString=limitList.begin(); curString!=limitList.end(); curString++) {
      if (!strcmp(str,(*curString))) {
        return limitNList[index];
      }
      index++;
    }
  }
  return 0;
}
//_____________________________________________________________________________
void StMessageCounter::ListLimits() {
  if (yesLimits) {
    cout << "  Limits :   counts : on message types";
    cout << " (a negative limit means no limit)" << endl;
    size_t index;
    for (index = 1; index < limitTList.size(); index++) {
      cout.width(8);
      cout << limitTList[index] << " : ";
      cout.width(8);
      cout << limitTCountList[index] << " : ";
      cout << messTypeList->FindNumType(index) << " - ";
      cout << messTypeList->FindNumText(index) << endl;
    }
    cout << "  Limits :   counts : on message strings" << endl;
    messCharVecIter curString;
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

    messCharVecIter curString;
    int index=0;
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
void StMessageCounter::AddType() {
    limitTList.push_back(-1);
    limitTCountList.push_back(0);
}
