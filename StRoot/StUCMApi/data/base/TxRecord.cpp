/*
 * TxRecord.cpp
 *
 * Created on Aug 20, 2007
 *
 * Author: Zhengqiang Liang (Wayne State University)
 *            Valeri Fine (Brookhaven National Laboratory)
 *            Jerome Lauret (Brookhaven National Laboratory)
 *            Stephen Tramer (Tech-X Corp.)
 *            
 *
 * This file is part of the UCM project funded under an SBIR
 * Copyright (c) 2007-2008 STAR Collaboration - Brookhaven National Laboratory
 *
 * @(#)cpp/api:$Id: TxRecord.cpp,v 1.1 2008/12/08 21:10:30 fine Exp $
 *
 *
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * STAR Scheduler is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with STAR Scheduler; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
 
#include <string>

#include "TxField.h"
#include "TxRecord.h"
#include "TxDataException.h"

TxRecord::TxRecord(FieldList* fields) 
  : fields_(NULL) {
  // If the length is 0, the record is already in
  // an unusable state so throwing an exception is kosher.
  if ( fields->empty() ) {
    throw TxDataException("Attempted to create an empty record",
			  TxDataException::RECORD);
  }

  // TODO: Deep copy?
  fields_ = fields;
}


TxRecord::TxRecord(const TxRecord &r) 
  : fields_(NULL) {
  copy(r);
}


TxRecord::~TxRecord() {
  if (fields_) {
    clearList();
    delete fields_;
  }
}


const TxRecord& TxRecord::operator=(const TxRecord& r){
  if ( this != &r ) {
    copy(r);
  }
  return *this;
}


void TxRecord::copy(const TxRecord& r) {
  if (fields_) {
    for (std::vector<TxField*>::iterator fieldIter = fields_->begin();
	 fieldIter != fields_->end();
	 fieldIter++) {
      delete (*fieldIter);
    }
    delete fields_;
  }

  fields_ = new std::vector<TxField*>;
  
  // Deep copy field list
  for ( FieldList::iterator iter = r.getFields()->begin();
	iter != r.getFields()->end();
	iter++ ) {
    fields_->push_back(new TxField(**iter));
  }
}


void TxRecord::clearList() {
  for (FieldList::iterator iter = fields_->begin();
       iter != fields_->end();
       iter++) {
    delete *iter;
  }
}


std::string TxRecord::toString() const {
  std::string recordStr;

  for (FieldList::const_iterator iter = fields_->begin();
       iter != fields_->end();
       iter++) {  
    recordStr += "|" + (*iter)->toString();
  }
  recordStr += "|";
  return recordStr;
}


std::vector<TxField*>* TxRecord::getFields() const {
  return fields_;
}


TxField* TxRecord::getField(const std::string& name) const {
  for (FieldList::const_iterator iter = fields_->begin();
       iter != fields_->end();
       iter++) {  
    if ( (*iter)->getName() == name ) {
      return *iter;
    }
  }
  return NULL;
}	


TxField* TxRecord::getField(unsigned int col) const {
  if ( col < 1 ||
       col > fields_->size() )
  {
    return NULL;
  }

  return (*fields_)[col-1];
}


void TxRecord::removeField(const std::string& name) {
  for (FieldList::iterator iter = fields_->begin();
       iter != fields_->end();
       iter++) {
    if ((*iter)->getName() == name) {
      fields_->erase(iter);
      break;
    }
  }
}


void TxRecord::removeField(int col) {
  int i;
  FieldList::iterator iter;

  for (iter = fields_->begin(), i=0;
       iter != fields_->end();
       i++, iter++) {
    if (i = col) {
      fields_->erase(iter);
      break;
    }
  }
}
