#include "StDbFieldIIterator.h"
#include "StDbFieldI.h"
#include <cassert>

using namespace TxLogging;
using namespace StDbField;
using namespace std;

//_______________________________________________________________________
StDbField::Iterator& StDbField::Iterator::operator++() {++fField;return *this;}
//_______________________________________________________________________
StDbField::Iterator& StDbField::Iterator::operator++(int) {fField++;return *this;}
//_______________________________________________________________________
StDbFieldI* StDbField::Iterator::operator*() {return *fField;}
//_______________________________________________________________________
const StDbFieldI* StDbField::Iterator::operator*()  const {return *fField;} 
//_______________________________________________________________________
StDbFieldI* StDbField::Iterator::next()
{
   assert((fField!=fFieldEnd ) && "No item to iterate");
   StDbFieldI* nxt = *fField;
   operator++();
   return nxt;
}

