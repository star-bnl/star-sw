// $Id: StMessTypeList.cxx,v 1.1 1999/06/23 15:17:44 genevb Exp $
// $Log: StMessTypeList.cxx,v $
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

#include "TROOT.h"
#include "StMessTypeList.h"
#include <iostream.h>

ClassImp(StMessTypePair)
StMessTypePair::StMessTypePair(const Char_t* ty, const Char_t* te) :
type(ty),
text(te) {}
StMessTypePair::~StMessTypePair() {}

StMessTypeList* StMessTypeList::mInstance = 0;
ClassImp(StMessTypeList)

//________________________________________
StMessTypeList::StMessTypeList() {
}
//________________________________________
StMessTypeList::~StMessTypeList() {
}
//_____________________________________________________________________________
StMessTypeList* StMessTypeList::Instance() {
  if (!mInstance) {
    mInstance = new StMessTypeList;
  }
  return mInstance;
}
//_____________________________________________________________________________
Int_t StMessTypeList::AddType(const Char_t* type, const Char_t* text) {
  StMessTypePair* temp = FindType(type);
  if (temp) return 0;
  temp = new StMessTypePair(type,text);
  messList.push_back(temp);
  return messList.size();
}
//_____________________________________________________________________________
Int_t StMessTypeList::FindTypeNum(Char_t* type) {
  StMessTypeVecIter iter;
  Int_t j=0;
  for (iter=messList.begin(); iter!=messList.end(); iter++) {
    j++;
    if (*((*iter)->Type())==(*type)) return j;
  }
  return 0;
}
//_____________________________________________________________________________
StMessTypePair* StMessTypeList::FindType(Char_t* type) {
  Int_t j = FindTypeNum(type);
  return ( (j) ? messList[(j-1)] : 0 );
}
//_____________________________________________________________________________
const Char_t* StMessTypeList::Text(Char_t* type) {
  StMessTypePair* temp = FindType(type);
  return ( (temp) ? temp->Text() : 0 );
}
//_____________________________________________________________________________
Int_t StMessTypeList::ListTypes() {
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

