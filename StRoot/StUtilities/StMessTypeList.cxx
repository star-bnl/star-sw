// $Id: StMessTypeList.cxx,v 1.4 1999/06/29 17:37:30 genevb Exp $
// $Log: StMessTypeList.cxx,v $
// Revision 1.4  1999/06/29 17:37:30  genevb
// Lots of fixes...
//
// Revision 1.3  1999/06/26 00:24:51  genevb
// Fixed const type mismatches
//
// Revision 1.2  1999/06/24 16:30:41  genevb
// Fixed some memory leaks
//
// Revision 1.1  1999/06/23 15:17:44  genevb
// Introduction of StMessageManager
//
//
// Revision 1.1 1999/01/27 10:28:29 genevb
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMessTypeList                                                       //
//                                                                      //
// This class manages the message types in STAR. It is a singleton.     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StMessTypeList.h"
#include <iostream.h>
#include <ctype.h>

StMessTypePair::StMessTypePair(const char* ty, const char* te) :
type(ty),
text(te) {}
StMessTypePair::~StMessTypePair() {}

StMessTypeList* StMessTypeList::mInstance = 0;

//________________________________________
StMessTypeList::StMessTypeList() {
}
//________________________________________
StMessTypeList::~StMessTypeList() {
  for (int i=0; i<messList.size(); i++)
    delete (messList[i]);
}
//_____________________________________________________________________________
StMessTypeList* StMessTypeList::Instance() {
  if (!mInstance) {
    mInstance = new StMessTypeList;
  }
  return mInstance;
}
//_____________________________________________________________________________
int StMessTypeList::AddType(const char* type, const char* text) {
  StMessTypePair* temp = FindType(type);
  if (temp) return 0;
  temp = new StMessTypePair(type,text);
  messList.push_back(temp);
  return messList.size();
}
//_____________________________________________________________________________
int StMessTypeList::FindTypeNum(const char* type) {
  StMessTypeVecIter iter;
  char ty=toupper(*type);
  int j=0;
  for (iter=messList.begin(); iter!=messList.end(); iter++) {
    j++;
    if (*((*iter)->Type())==ty) return j;
  }
  return 0;
}
//_____________________________________________________________________________
StMessTypePair* StMessTypeList::FindType(const char* type) {
  int j = FindTypeNum(type);
  return ( (j) ? messList[(j-1)] : 0 );
}
//_____________________________________________________________________________
const char* StMessTypeList::Text(const char* type) {
  StMessTypePair* temp = FindType(type);
  return ( (temp) ? temp->Text() : 0 );
}
//_____________________________________________________________________________
int StMessTypeList::ListTypes() {
  StMessTypeVecIter iter;
  cout << "List of StMessage types:" << endl;
  cout << "--------------------------------------------------------" << endl;
  char* temp = "    :                                               ";
  for (iter=messList.begin(); iter!=messList.end(); iter++) {
    StMessTypePair* current = (*iter);
    strncpy(&temp[2],current->Type(),1);
    strcpy(&temp[6],current->Text());
    cout << temp << endl;
  }
  return messList.size();
}

