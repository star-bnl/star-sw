#ifndef StRecordITERATOR_H
#define StRecordITERATOR_H

#include <iterator>
#include "RecordList.h"

namespace TxLogging {
   class Iterator : public std::iterator<std::input_iterator_tag, RecordIterator>
{
 RecordIterator fField;
 RecordIterator fFieldEnd;
public:
  Iterator(RecordList &fields) : fField(fields.begin()), fFieldEnd(fields.end()) {}
  Iterator(RecordList *fields) : fField(fields->begin()), fFieldEnd(fields->end()) {}
  Iterator(const Iterator& mit) : fField(mit.fField), fFieldEnd(mit.fFieldEnd) { }
  Iterator(const RecordIterator &first,const RecordIterator &last) :fField(first), fFieldEnd(last) {}
  Iterator& operator++();
  Iterator& operator++(int);
  bool operator==(const Iterator& rhs) {return fField==rhs.fField;}
  bool operator!=(const Iterator& rhs) {return fField!=rhs.fField;}
  StRecord* operator*();
  const StRecord* operator*()  const;
  bool hasNext() const { return fField != fFieldEnd; }
  StRecord* next();
};
}

#endif
