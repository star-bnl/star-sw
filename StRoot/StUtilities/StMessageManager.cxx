// $Id: StMessageManager.cxx,v 1.1 1999/06/23 15:17:52 genevb Exp $
// $Log: StMessageManager.cxx,v $
// Revision 1.1  1999/06/23 15:17:52  genevb
// Introduction of StMessageManager
//
//
// Revision 1.1 1999/01/27 10:28:29 genevb
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMessageManager                                                     //
//                                                                      //
// This class manages the messages in STAR software. It is a singleton. //
// Messages are stored in a vector, and come in several types           //
// (i.e. info, error). The types "I" (info), "W" (warning) and          //
// "E" (error) are predefined. Message finding and summary tools are    //
//  also available.                                                     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TROOT.h"
#include "StMessageManager.h"
#include <ctype.h>

StMessageManager* gMessMgr=0;

StMessageManager* StMessageManager::mInstance = 0;

// C and Fortran routines:
//________________________________________
void type_of_call StMessage_(Char_t* mess, Char_t* type, Char_t* opt) {
  StMessageManager::Instance()->Message(mess,type,opt);
}
//________________________________________
void type_of_call StInfo_(Char_t* mess, Char_t* opt) {
  StMessage_(mess,"I",opt);
}
//________________________________________
void type_of_call StWarning_(Char_t* mess, Char_t* opt) {
  StMessage_(mess,"W",opt);
}
//________________________________________
void type_of_call StError_(Char_t* mess, Char_t* opt) {
  StMessage_(mess,"E",opt);
}
//________________________________________
void type_of_call StMessAddType_(const Char_t* type, const Char_t* text) {
  StMessageManager::Instance()->AddType(type,text);
}

// C++ routines:
//_____________________________________________________________________________
ClassImp(StMessageManager)
//_____________________________________________________________________________
StMessageManager::StMessageManager() {
  gMessMgr = this;
  messTypeList = StMessTypeList::Instance();
  messCounter = StMessageCounter::Instance();
  messCounter->AddType();
  // First messVec on collection is list of all messages
  messCollection.push_back(&messList);
  AddType("I","Info");
  AddType("W","Warning");
  AddType("E","Error");
}
//_____________________________________________________________________________
StMessageManager::~StMessageManager() {
  messVecIter current;
  for (current=messList.begin(); current!=messList.end(); current++)
    delete *current;
}
//_____________________________________________________________________________
StMessageManager* StMessageManager::Instance() {
  if (!mInstance) {
    mInstance = new StMessageManager;
  }
  return mInstance;
}
//_____________________________________________________________________________
StMessage& StMessageManager::Message(Char_t* mess, Char_t* type, Char_t* opt) {
  messObj = new StMessage(mess, type, opt);
  if (!strchr(opt,'-')) {
    messList.push_back(messObj);
    Char_t ty = toupper(type[0]);
    int typeN = messTypeList->FindTypeNum(&ty);
    if (!typeN) typeN = 1;                          // default is Info
    messCollection[typeN]->push_back(messObj);
  }
  return (*messObj);
}
//_____________________________________________________________________________
int StMessageManager::PrintList(messVec* list) {
  messVecIter current;
  int i=0;
  for (current=list->begin(); current!=list->end(); current++)
    {(*current)->Print(-1); i++;}
  return i;
}
//_____________________________________________________________________________
messVecIter StMessageManager::FindMessageIter(const Char_t* s1, Char_t* s2,
               Char_t* s3, Char_t* s4, messVec* list) {
  if (!list) list = &messList;
  messVecIter current;
  const Char_t* curMess;
  for (current=list->begin(); current!=list->end(); current++) {
    curMess = (*current)->GetMessage();
    if ((strstr(curMess,s1)) && (strstr(curMess,s2)) &&
        (strstr(curMess,s3)) && (strstr(curMess,s4))) return current;
  }
  return 0;
}
//_____________________________________________________________________________
StMessage* StMessageManager::FindMessage(const Char_t* s1, Char_t* s2,
               Char_t* s3, Char_t* s4, messVec* list) {
  messVecIter current = FindMessageIter(s1,s2,s3,s4,list);
  return ( (current) ? (*current) : 0 );
}
//_____________________________________________________________________________
messVec& StMessageManager::FindMessageList(const Char_t* s1, Char_t* s2,
               Char_t* s3, Char_t* s4, messVec* list) {
  Char_t* s1a = new Char_t[strlen(s1)];
  strcpy(s1a,s1);
  if ((strlen(s1)==1) && (!list)) {
    int typeN = messTypeList->FindTypeNum(s1);
    if (typeN) {
      list = messCollection[typeN];
      strcpy(s1a,"");
    }
  }
  if (!list) list = &messList;
  if (!(strcmp(s1a,"") || strcmp(s2,"") || strcmp(s3,"") || strcmp(s4,"")))
    return (*list);
  messVec* newList = new messVec();
  messVecIter current;
  const Char_t* curMess;
  for (current=list->begin(); current!=list->end(); current++) {
    curMess = (*current)->GetMessage();
    if ((strstr(curMess,s1)) && (strstr(curMess,s2)) &&
        (strstr(curMess,s3)) && (strstr(curMess,s4)))
      newList->push_back(*current);
  }
  return (*newList);
}
//_____________________________________________________________________________
int StMessageManager::RemoveMessage(StMessage* mess) {
  if (!mess) return 3;
  const Char_t* curMess = mess->GetMessage();
  messVecIter current = FindMessageIter(curMess);
  if (!current) return 1;
  messList.erase(current);
  int typeN = messTypeList->FindTypeNum(mess->GetType());
  current = FindMessageIter(curMess,"","","",messCollection[typeN]);
  if (!current) return 2;
  messCollection[typeN]->erase(current);
  delete mess;
  if (mess==gMessage) gMessage = 0;
  return 0;
}
//_____________________________________________________________________________
void StMessageManager::Summary(Int_t nTerms) {
  int max = 67;
  int nMess = messList.size();
  StVector(Bool_t) done;
  typedef StVector(Char_t*) CharPtrVec;
  CharPtrVec mType;
  StVector(CharPtrVec) toks;
  int i;
  int j;
  int k;
  Bool_t agree;
  Char_t* temp;
  cout << "  ***** StMessageManager message summary *****" << endl;
  for (i=0; i<nMess; i++) {
    done.push_back(kFALSE);
    mType.push_back(temp = messList[i]->GetType());
    toks.push_back(*(new CharPtrVec));
    temp = new Char_t[81];
    temp = strncpy(temp,(messList[i]->GetMessage()),80);
    temp = strtok(temp, " ");
    toks[i].push_back(temp);
    while (temp != NULL) {
      temp = strtok(NULL, " ");
      toks[i].push_back(temp);
    }
    for (j=toks[i].size(); j<nTerms; j++) toks[i].push_back(temp);
  }
  for (i=0; i<nMess; i++) {
    int count = 1;
    if (!(done[i])) {
      for (j=(i+1); j<nMess; j++) {
        if ((*(mType[i]))==(*(mType[j]))) {
          agree = kTRUE;
          for (k=0; k<nTerms; k++) {
            if ((toks[i])[k] != NULL) {
              if (((toks[j])[k] == NULL) ||
                        strcmp((toks[i])[k],(toks[j])[k])) agree = kFALSE;
            }
            else if ((toks[j])[k] != NULL) agree = kFALSE;
          }
          if (agree) {
            done[j] = kTRUE;
            count++;
          }
        }
      }
      done[i] = kTRUE;
      for (j = messList[i]->Print(max); j<max; j++) cout << ".";
      cout << ".. " << count << endl;
    }
  }
  return;
}
//_____________________________________________________________________________
int StMessageManager::AddType(const Char_t* type, const Char_t* text) {
  int typeN = messTypeList->AddType(type,text);
  if (typeN) {
    messVec* temp = new messVec();   // Add a new messVec for each message type
    messCollection.push_back(temp);
    messCounter->AddType();
  }
  return typeN;
}
//_____________________________________________________________________________
void StMessageManager::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StMessageManager.cxx,v 1.1 1999/06/23 15:17:52 genevb Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
}


StMessageManager* temp=StMessageManager::Instance();
