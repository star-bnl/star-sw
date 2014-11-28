#include "StRecordIterator.h"
#include "RecordList.h"
#include "StRecord.h"

#include <algorithm>

using namespace  TxLogging;
// using namespace  StDbField;
using namespace  std;

//________________________________________________________________________
RecordList::RecordList():vector<StRecord*>(){};
//________________________________________________________________________
RecordList::RecordList(const RecordList &me):vector<StRecord*>(me){};

//________________________________________________________________________
RecordList &RecordList::operator=(const RecordList &me)
{ 
 //  this->std::vector<StDbFieldI*>operator=(me);  
   return *this;
}
//________________________________________________________________________
RecordList::~RecordList()
{
   Clear();
}

//________________________________________________________________________
namespace {  
  void Delete(StRecord* r)      {   delete r;         }
  void PrintFields(StRecord* r) {   r->print();       } 
  void PrintHeader(StRecord* r) {   r->printHeader(); } 
}
//________________________________________________________________________
void RecordList::Clear()
{
    for_each(begin(),end(),Delete);
    clear();
}
//________________________________________________________________________
void RecordList::Print() 
{
   PrintHeader(*begin());
   for_each(begin(),end(),PrintFields);
}
//________________________________________________________________________
TxLogging::Iterator RecordList::iterator()
{
   return TxLogging::Iterator(this);
}

