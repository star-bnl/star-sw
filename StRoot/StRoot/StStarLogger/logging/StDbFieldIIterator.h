#ifndef STDBFIELDIITERATOR_H
#define STDBFIELDIITERATOR_H

#include <iterator>
#include "FieldList.h"
namespace TxLogging {

namespace StDbField {

class Iterator : public std::iterator<std::input_iterator_tag, FieldList::iterator>
{
 FieldList::iterator fField;
 FieldList::iterator fFieldEnd;
public:
  Iterator(FieldList &fields) : fField(fields.begin()), fFieldEnd(fields.end()) {}
  Iterator(FieldList *fields) : fField(fields->begin()), fFieldEnd(fields->end()) {}
  Iterator(const Iterator& mit) : fField(mit.fField), fFieldEnd(mit.fFieldEnd) { }
  Iterator(const FieldList::iterator &first,const FieldList::iterator &last) :fField(first), fFieldEnd(last) {}
  Iterator& operator++();
  Iterator& operator++(int);
  bool operator==(const Iterator& rhs) {return fField==rhs.fField;}
  bool operator!=(const Iterator& rhs) {return fField!=rhs.fField;}
  StDbFieldI* operator*();
  const StDbFieldI* operator*()  const;
  bool hasNext() const { return fField != fFieldEnd; }
  StDbFieldI* next();
};
}
}

#endif
