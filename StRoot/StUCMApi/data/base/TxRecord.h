/*
 * TxRecord.h
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
 * @(#)cpp/api:$Id: TxRecord.h,v 1.1 2008/12/08 21:10:30 fine Exp $
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
 
#ifndef TXRECORD_H
#define TXRECORD_H

#include <string>
#include <vector>

#include "TxField.h"

/**
 * Abstraction for records in memory.
 * TODO: Is the key information necessary for records as well as collections?
 */
class TxRecord {
 public:
  /**
   * Constructor: Creates a record from the provided vector of
   * fields.
   * @param fields The vector containing fields to set as the
   *               field list.
   * @throws TxDataException If the length of the vector is zero.
   */
  TxRecord(std::vector<TxField*>* fields);

  /**
   * Copying mechanisms.  Copy constructor and operator= rely on the
   * copy() method.
   */
  TxRecord(const TxRecord&);//!Copy constructor
  const TxRecord& operator=(const TxRecord& );//!Assignment

  /**
   * Destructor: Deletes sublist.
   */
  ~TxRecord();//!Deconstructor

  /**
   * Returns a string representing the record.  Primarily useful for
   * debugging purposes.
   */
  std::string toString() const;

  /**
   * Returns the field from the sublist with the given name, or NULL if none is found.
   * @param name The name of the field to retrieve.
   */
  TxField* getField(const std::string& name) const;

  /**
   * Returns the field from the sublist which is in the given column.  Column
   * indexing begins at 1.  Returns NULL if the column number is out of bounds.
   * @param col The column number.
   */
  TxField* getField(unsigned int col) const;

  /**
   * Removes the specified field from the record.  This deletes
   * the pointer, so any lists retrieved from BEFORE this operation
   * should be considered UNSAFE.  This is provided as a convenient
   * way to remove unwanted field information when constructing patterns.
   * TODO: Maybe make a TxPattern class which represents patterns?
   * @param name The name of the field to delete.
   */
  void removeField(const std::string& name);

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
  std::vector<TxField*>* getFields() const;

 private:
  /**
   * @internal
   * Copy mechanism used by copy constructor and op=.
   */
  void copy(const TxRecord& r);

  /**
   * @internal
   * Clears the list of fields.
   */
  void clearList();

 private:
  typedef std::vector<TxField*> FieldList;

  FieldList* fields_;
};

#endif


    
