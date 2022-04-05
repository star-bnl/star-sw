/*
 * StRecord.h
 *
 * Created on Aug 20, 2007
 *
 * Author: Zhengqiang Liang (Wayne State University)
 *            Valeri Fine (Brookhaven National Laboratory)
 *            Jerome Lauret (Brookhaven National Laboratory)
 *            Stephen Tramer (Tech-X Corp.)
 *
 * This file is part of the UCM project funded under an SBIR
 * Copyright (c) 2007-2008 STAR Collaboration - Brookhaven National Laboratory
 *
 * @(#)cpp/api:$Id: StRecord.h,v 1.2 2010/03/30 20:05:36 fine Exp $
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
 
#ifndef STRECORD_H
#define STRECORD_H

#include <string>
#include <vector>

#include "StDbFieldI.h"
#include "RecordList.h"
namespace TxLogging {
/**
 * Abstraction for records in memory.
 * TODO: Is the key information necessary for records as well as collections?
 */
class StRecord {
 public:
   StRecord();

  /**
   * Constructor: Creates a record from the provided vector of
   * fields.
   * @param fields The vector containing fields to set as the
   *               field list.
   * @throws StDataException If the length of the vector is zero.
   */
  StRecord(const TxLogging::FieldList& fields);

  /**
   * Copying mechanisms.  Copy constructor and operator= rely on the
   * copy() method.
   */
  StRecord(const StRecord&);//!Copy constructor
  const StRecord& operator=(const StRecord& );//!Assignment

  /**
   * Destructor: Deletes sublist.
   */
  virtual ~StRecord();//!Deconstructor

  /**
   * Returns a string representing the record.  Primarily useful for
   * debugging purposes.
   */
  const char *toString() const;

  /**
   * Returns the field from the sublist with the given name, or NULL if none is found.
   * @param name The name of the field to retrieve.
   */
  StDbFieldI* getField(const char *name) const;

  /**
   * Returns the field from the sublist which is in the given column.  Column
   * indexing begins at 1.  Returns NULL if the column number is out of bounds.
   * @param col The column number.
   */
  StDbFieldI* getField(unsigned int col) const;

  /**
   * Removes the specified field from the record.  This deletes
   * the pointer, so any lists retrieved from BEFORE this operation
   * should be considered UNSAFE.  This is provided as a convenient
   * way to remove unwanted field information when constructing patterns.
   * TODO: Maybe make a TxPattern class which represents patterns?
   * @param name The name of the field to delete.
   */
  void removeField(const char *name);

  /**
   * Removes the specified field from the record.  This deletes
   * the pointer, so any lists retrieved from BEFORE this operation
   * should be considered UNSAFE.
   * @param col The column to delete.
   */
  void removeField(int col);

  /**
   * Returns the complete list of stored fields.
   */
  TxLogging::FieldList &getFields();
  const TxLogging::FieldList &getFields() const;
  void printHeader() const;
  void print() const;

  /**
   * Returns the complete list of child records
   */
  TxLogging::RecordList &getRecords();
  const  TxLogging::RecordList &getRecords() const;
private:
  /**
   * @internal
   * Copy mechanism used by copy constructor and op=.
   */
  void copy(const StRecord& r);

  /**
   * @internal
   * Clears the list of fields.
   */
  void clearList();
 
 private:

  TxLogging::FieldList   fFields;
  TxLogging::RecordList  fRecords;
};
}
#endif


    
