// $Id: StMessageCounter.cxx,v 1.3 1999/06/25 22:57:56 genevb Exp $
// $Log: StMessageCounter.cxx,v $
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

#include "TROOT.h"
#include "StMessageCounter.h"

StMessageCounter* StMessageCounter::mInstance = 0;

ClassImp(StMessageCounter)

//_____________________________________________________________________________
StMessageCounter::StMessageCounter() : limitMessage(" - COUNT LIMIT REACHED!\n") {
  messTypeList = StMessTypeList::Instance();
  outMessage = new Char_t[5000];
  yesLimits = 0;
}
//_____________________________________________________________________________
StMessageCounter::~StMessageCounter() {
  int i;
  for (i=0; i<limitTList.size(); i++) {
    delete (limitTList[i]);
    delete (limitTCountList[i]);
  }
  for (i=0; i<limitNList.size(); i++) {
    delete (limitNList[i]);
    delete (limitNCountList[i]);
  }
}
//_____________________________________________________________________________
StMessageCounter* StMessageCounter::Instance() {
  if (!mInstance) {
    mInstance = new StMessageCounter;
  }
  return mInstance;
}
//_____________________________________________________________________________
void StMessageCounter::SetLimit(Char_t* str, Int_t n) {
  yesLimits = 1;
  const int len = strlen(str);
  if (len==1) {
    int typeN = messTypeList->FindTypeNum(str);
    if (typeN) {
      delete (limitTList[typeN]);
      limitTList[typeN] = new Int_t(n);
    }
  } else {
    messCharVecIter curString;
    int index=0;
    for (curString=limitList.begin(); curString!=limitList.end(); curString++) {
      if (!strcmp(str,(*curString))) {
        delete (limitNList[index]);
        limitNList[index] = new Int_t(n);
        return;
      }
      index++;
    }
    Char_t* temp = new Char_t[len];
    strcpy(temp,str);
    limitList.push_back(temp);
    limitNList.push_back(new Int_t(n));
    limitNCountList.push_back(new Int_t(0));
  }
  return;
}
//_____________________________________________________________________________
void StMessageCounter::ListLimits() {
  if (yesLimits) {
//  cout<<"Limits on message types: (a negative number means no limit)"<<endl;
    cout << "  Limits :   counts : on message strings";
    cout << " (a negative number means no limit)" << endl;
    messCharVecIter curString;
    int index=0;
    for (curString=limitList.begin(); curString!=limitList.end(); curString++) {
      cout.width(8);
      cout << *(limitNList[index]) << " : ";
      cout.width(8);
      cout << *(limitNCountList[index++]) << " : ";
      cout << (*curString) << endl;
    }
  } else {
    cout << "No limits have been set on messages." << endl;
  }
  return;
}
//_____________________________________________________________________________
int StMessageCounter::CheckLimit(Char_t* mess, Char_t* type) {
  int printIt = 1;
  int typeN = messTypeList->FindTypeNum(type);
  int typeNewSize = *(limitTCountList[typeN]) + 1;
  delete (limitTCountList[typeN]);
  limitTCountList[typeN] = new Int_t(typeNewSize);

  if (yesLimits) {
    strcpy(outMessage,"");
    int limit;
    limit = *(limitTList[typeN]);
    if (typeNewSize == limit) {
      strcat(outMessage,"St");
      strcat(outMessage,(messTypeList->FindType(type)->Text()));
      strcat(outMessage,": ");
      strcat(outMessage,limitMessage);
    } else if ((limit >= 0) && (typeNewSize > limit)) {
      printIt = 0;
    }

    messCharVecIter curString;
    int index=0;
    for (curString=limitList.begin(); curString!=limitList.end(); curString++) {
      if (strstr(mess,(*curString))) {
        Int_t counts = *(limitNCountList[index]) + 1;
        delete (limitNCountList[index]);
        limitNCountList[index] = new Int_t(counts);
        limit = *(limitNList[index]);
        if (counts==limit) {
          strcat(outMessage,"StMessage: ");
          strcat(outMessage,(*curString));
          strcat(outMessage,limitMessage);
        } else if ((limit >= 0) && (counts > limit)) {
          printIt = 0;
        }
      }
      index++;
    }
  }
  return printIt;
}
//_____________________________________________________________________________
void StMessageCounter::AddType() {
    limitTList.push_back(new Int_t(-1));
    limitTCountList.push_back(new Int_t(0));
}
