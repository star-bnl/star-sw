#include "StDbFieldIIterator.h"
#include "FieldList.h"

using namespace TxLogging;
// using namespace StDbField;

FieldList::FieldList():std::vector<StDbFieldI*>(){};
FieldList::FieldList(const FieldList &me):std::vector<StDbFieldI*>(me){};
FieldList &FieldList::operator=(const FieldList &me)
{ 
 //  this->std::vector<StDbFieldI*>operator=(me);  
   return *this;
}
#if 0
Iterator FieldList::iter()
{
   return Iterator(this);
}
#endif
