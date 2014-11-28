/*
 * StRecord.cpp
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
 * @(#)cpp/api:$Id: StRecord.cxx,v 1.2 2010/03/30 20:05:36 fine Exp $
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
#include <iostream>

#include "StDbFieldI.h"
#include "StRecord.h"
#include "StDataException.h"

using namespace TxLogging;
using namespace std;
//________________________________________________________________________
StRecord::StRecord() : fFields() 
{
}

//________________________________________________________________________
StRecord::StRecord(const FieldList &fields) : fFields(fields) {
  // If the length is 0, the record is already in
  // an unusable state so throwing an exception is kosher.
  if ( fields.empty() ) {
    throw StDataException("Attempted to create an empty record",
			  StDataException::RECORD);
  }
  // TODO: Deep copy?
}


//________________________________________________________________________
StRecord::StRecord(const StRecord &r) : fFields() {
  copy(r);
}

//________________________________________________________________________
StRecord::~StRecord() {
  if (!fFields.empty()) { }
}


//________________________________________________________________________
const StRecord& StRecord::operator=(const StRecord& r){
  if ( this != &r ) {
    copy(r);
  }
  return *this;
}


//________________________________________________________________________
void StRecord::copy(const StRecord& r) {
  if (!fFields.empty())     clearList();

    // Deep copy field list
  for ( FieldList::const_iterator iter = r.getFields().begin();
	iter != r.getFields().end();
	iter++ ) {
    fFields.push_back(new StDbFieldI(**iter));
  }
}


//________________________________________________________________________
void StRecord::clearList() {
  for (FieldList::iterator iter = fFields.begin();
       iter != fFields.end();
       iter++) {
    delete *iter;
  }
  fFields.clear();
}


//________________________________________________________________________
const char*StRecord::toString() const {
  static std::string recordStr;
  recordStr="";
  
  for (FieldList::const_iterator iter = fFields.begin();
       iter != fFields.end();
       ++iter) {
         recordStr += string("|") + (*iter)->fieldAsString();
  }
  recordStr += "|";
  return recordStr.c_str() ;
}

//________________________________________________________________________
const FieldList& StRecord::getFields() const {
   return fFields;
}

//________________________________________________________________________
FieldList& StRecord::getFields() {
   return fFields;
}

//________________________________________________________________________
StDbFieldI* StRecord::getField(const char *name) const 
{
  StDbFieldI* field = 0;
  for (FieldList::const_iterator iter = fFields.begin();
       !field && (iter != fFields.end()) ;
       iter++) {
       if (string(name) == (*iter)->getName()) field = *iter;
  }
  return field;
}	


//________________________________________________________________________
StDbFieldI* StRecord::getField(unsigned int col) const 
{
   return
      ( col > 0 && col <= fFields.size() ) ? fFields[col-1] : 0;
}
//________________________________________________________________________
const RecordList &StRecord::getRecords() const{
   return fRecords; 
}

//________________________________________________________________________
RecordList &StRecord::getRecords() {
   return fRecords; 
}

//________________________________________________________________________
void StRecord::removeField(const char *name) {
  for (FieldList::iterator iter = fFields.begin();
       iter != fFields.end();
       iter++) {
    if (string(name) == (*iter)->getName()) {
      fFields.erase(iter);
      break;
    }
  }
}


//________________________________________________________________________
void StRecord::removeField(int col) 
{
  int i;
  FieldList::iterator iter;

  for (iter = fFields.begin(), i=0; iter != fFields.end(); i++, iter++) 
  {
    if ( i == col ) {
      fFields.erase(iter);
      break;
    }
  }
}

//________________________________________________________________________
void StRecord::printHeader() const
{
   FieldList::const_iterator it = fFields.begin();
   for (; it != fFields.end(); ++it) {
     cout << (*it)->getName() << " | " ;
   }
   cout << endl;
   for (; it != fFields.end(); ++it) {
     cout << (*it)->getTypeAsString() << " | " ;
   }
   cout << endl;
}

//________________________________________________________________________
void StRecord::print() const
{
   FieldList::const_iterator it = fFields.begin();
   for (; it != fFields.end(); ++it) {
     cout << (*it)->getValueAsString() << " | " ;
   }
   cout << endl;
}