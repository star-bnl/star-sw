//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMessTypeList                                                       //
//                                                                      //
// This class manages the message types in STAR. It is a singleton.     //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StMessTypeList.h"
#include "StMessageStream.h"
#include <ctype.h>
using namespace std;
StMessTypePair::StMessTypePair(const char* ty, const char* te) :
type(ty),
text(te) {}
StMessTypePair::~StMessTypePair() {}

StMessTypeList* StMessTypeList::mInstance = 0;

//________________________________________
StMessTypeList::StMessTypeList() : messList() {
}
//________________________________________
StMessTypeList::~StMessTypeList() {
  for (size_t i=0; i<messList.size(); i++)
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
  if (islower(*type)) {
    *(const_cast<char*> (type)) = toupper(*type);
  }
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
    if (*((*iter)->Type())==ty) {return j;}
  }
  return 0;
}
//_____________________________________________________________________________
StMessTypePair* StMessTypeList::FindType(const char* type) {
  StMessTypeVecIter iter;
  char ty=toupper(*type);
  for (iter=messList.begin(); iter!=messList.end(); iter++) {
    if (*((*iter)->Type())==ty) {return (*iter);}
  }
  return 0;
}
//_____________________________________________________________________________
const char* StMessTypeList::FindNumType(size_t typeNum) {
  if ((typeNum < 1) || (typeNum > messList.size())) return 0;
  return messList[(typeNum - 1)]->Type();
}
//_____________________________________________________________________________
const char* StMessTypeList::FindNumText(size_t typeNum) {
  if ((typeNum < 1) || (typeNum > messList.size())) return 0;
  return messList[(typeNum - 1)]->Text();
}
//_____________________________________________________________________________
int StMessTypeList::ListTypes() {
  StMessTypeVecIter iter;
  myout << "List of StMessage types:\n"
        << "--------------------------------------------------------"
        << endl;
  for (iter=messList.begin(); iter!=messList.end(); iter++) {
    StMessTypePair* current = (*iter);
    myout << "  " << current->Type() << " : " << current->Text() << endl;
  }
  return messList.size();
}

//_____________________________________________________________________________
// $Id: StMessTypeList.cxx,v 1.11 2012/06/11 15:05:34 fisyak Exp $
// $Log: StMessTypeList.cxx,v $
// Revision 1.11  2012/06/11 15:05:34  fisyak
// std namespace
//
// Revision 1.10  2003/09/25 21:19:22  genevb
// Some new cout-like functions and friend functions, some doxygen-ization
//
// Revision 1.9  2003/09/02 17:59:20  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.8  1999/09/14 15:42:02  genevb
// Some bug fixes, workaround for nulls in strings
//
// Revision 1.7  1999/09/10 21:05:55  genevb
// Some workarounds for RedHat6.0
//
// Revision 1.6  1999/07/01 01:24:45  genevb
// Fixed FORTRAN character string bug on linux, removed a memory leak from Summary()
//
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
// Revision 1.0  1999/01/27 10:28:29  genevb
//
