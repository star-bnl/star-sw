// $Id: StMessageCounter.cxx,v 1.1 1999/06/23 15:17:49 genevb Exp $
// $Log: StMessageCounter.cxx,v $
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
void StMessageCounter::SetLimit(Char_t* str, Int_t n) {
  const int len = strlen(str);
  if (len==1) {
    int typeN = messTypeList->FindTypeNum(str);
    delete limitTList[typeN];
    limitTList[typeN] = new Int_t(n);
  } else {
    Char_t* temp = new Char_t[len];
    strcpy(temp,str);
    limitList.push_back(temp);
    limitNList.push_back(new Int_t(n));
    limitNCountList.push_back(new Int_t(0));
  }
  return;
}
//_____________________________________________________________________________
Char_t* StMessageCounter::CheckLimit(Char_t* mess, Char_t* type) {
  Bool_t printIt=kTRUE;
  Char_t* temp=new Char_t[80];
  strcpy(temp,"");
  messCharVecIter curString;
  int index=0;
  for (curString=limitList.begin(); curString!=limitList.end(); curString++) {
//      cout << "HHHK: " << mess << ":::" << (*curString) << endl;
    if (strstr(mess,(*curString))) {
      Int_t counts = *(limitNCountList[index]);
//      cout << "HHHL: " << mess << " " << counts << endl;
      if (counts == *(limitNList[index])) {
//        cout << "HHHN: " << endl;
        printIt = kFALSE;
        continue;
      }
      delete limitNCountList[index];
      limitNCountList[index] = new Int_t(++counts);
      if (counts==*(limitNList[index])) {
//        cout << "HHHO: " << endl;
        strcat(temp,"StMessage: ");
        strcat(temp,(*curString));
        strcat(temp,limitMessage);
      }
    }
    index++;
  }
  int typeN = messTypeList->FindTypeNum(type);
  int typeNewSize = *(limitTCountList[typeN]) + 1;
  delete limitTCountList[typeN];
  limitTCountList[typeN] = new Int_t(typeNewSize);
  if (typeNewSize == *(limitTList[typeN])) {
    strcat(temp,"St");
    strcat(temp,(messTypeList->FindType(type)->Text()));
    strcat(temp,": ");
    strcat(temp,limitMessage);
  } else if ((*(limitTList[typeN])>=0) && (typeNewSize > *(limitTList[typeN]))) {
    printIt = kFALSE;
  }
  if (!printIt) temp=strcat("<||>",temp);
  return temp;
}
//_____________________________________________________________________________
void StMessageCounter::AddType() {
    limitTList.push_back(new Int_t(-1));
    limitTCountList.push_back(new Int_t(0));
}
