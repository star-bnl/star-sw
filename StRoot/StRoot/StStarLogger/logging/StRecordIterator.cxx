#include "StRecordIterator.h"
#include "StRecord.h"
#include <cassert>

using namespace TxLogging;
using namespace std;

//_______________________________________________________________________
Iterator& Iterator::operator++() {++fField;return *this;}
//_______________________________________________________________________
Iterator& Iterator::operator++(int) {fField++;return *this;}
//_______________________________________________________________________
StRecord* Iterator::operator*() {return *fField;}
//_______________________________________________________________________
const StRecord* Iterator::operator*()  const {return *fField;} 
//_______________________________________________________________________
StRecord* Iterator::next()
{
   assert((fField!=fFieldEnd ) && "No item to iterate");
   StRecord* nxt = *fField;
   operator++();
   return nxt;
}

