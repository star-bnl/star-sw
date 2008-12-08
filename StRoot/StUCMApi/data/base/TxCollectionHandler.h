/*
 * TxCollection.h
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
 * @(#)cpp/api:$Id: TxCollectionHandler.h,v 1.1 2008/12/08 21:10:29 fine Exp $
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
 
#ifndef TXCOLLECTION_H
#define TXCOLLECTION_H

#include <string>

#include "TxHandler.h"
#include "TxRecord.h"
#include "TxModule.h"


/**
 * Abstraction for the collection in memory, with facilities to manipulate 
 * collection data in the store.
 */
class TxCollectionHandler : public TxHandler {
 public:
  /**
   * Constructor: Creates a new collection with the provided name and pattern.
   * The first field of the pattern is assumed to be the key.
   * @param name The name of the collection.
   * @param pattern The pattern for the collection.  Note that if there are any
   *                values stored in the pattern, they are purged.
   * @throws TxDataException If pattern length is 0.
   */
  TxCollectionHandler(const std::string& name, TxRecord* pattern, TxModule* module);

  /**
   * Constructor: Creates a new collection with the provided name, key, and pattern.
   * The first field of the pattern is assumed to be the key.
   * @param name The name of the collection.
   * @param key The key of the collection.  If the key is not found in the pattern,
   *            the first field is used as the key name.
   * @param pattern The pattern for the collection.  Note that if there are any
   *                values stored in the pattern, they are purged.
   * @throws TxDataException If pattern length is 0.
   * @throws TxDataException Warning if key name is not found in pattern.
   */
  TxCollectionHandler(const std::string& name, const std::string& key, 
		      TxRecord* pattern, TxModule* module);

  /**
   * Destructor: Clears pattern information.
   */
  ~TxCollectionHandler();

  /**
   * Executes an operation related to the collection on the associated store.
   * The operations are defined as follows:
   *---------------------------------------------------------------------------
   *SELECT     || pattern*        || NULL    || 
   *   Returns a list of TxRecords which match all of the field values in
   *   the given pattern.  All fields in the pattern are initially marked
   *   IGNORE, so the user only need set those which are necessary for 
   *   matching/updates.
   *   * A NULL pattern retrieves all records.
   *---------------------------------------------------------------------------
   *INSERT     || record          || NULL    || 
   *   Inserts the given record into the collection.  Returns an empty list.
   *   * A field set to NULL that is written into a NON-NULL column is set
   *     to the default value for that column.
   *---------------------------------------------------------------------------
   *DELETE     || pattern         || NULL    || 
   *   Deletes all records matching the given pattern.  Returns an empty list.
   *---------------------------------------------------------------------------
   *UPDATE     || Old record pattern || New record pattern
   *    Replaces data in all rows matching the old pattern with the data in
   *    the new pattern.  Returns an empty list.
   *    * A field set NULL that is written into a NON-NULL column is set
   *      to the default value for that column.
   *---------------------------------------------------------------------------
   * @param op The operation (TxHandler::Operation) to perform.
   * @param pattern The matching pattern for records to operate on.  See operation
   *                list for details).
   * @param newPattern The updated pattern information for all matching patterns.
   *                   Used only by the UPDATE operation.
   * @throws TxDataException Will throw an exception if the operation does not
   *                         succeed.
   * @return Returns a list of <<TxRecord*>>.  See operation list for details.
   *         WARNING: Operations which select the same records will return DUPLCIATES,
   *         not a reference to the same object.  So execute(SELECT);execute(SELECT)
   *         will return two distinct list of records which contain the same content.
   *         The only way to get around this is performing a SELECT * and caching the
   *         result, which may be undesirable.
   */
  std::vector<TxRecord*> execute(TxHandler::Operation op, TxRecord* pattern, 
				 TxRecord* newPattern=NULL);
		
  /**
   * Returns the key for the collection.
   */
  std::string getKey() const;

  /**
   * Returns the name of the collection.
   */
  std::string getName() const;

  /**
   * Returns a COPY of the collection's pattern, so that users can safely modify
   * field values.  All fields are initially set IGNORE so that the user only needs
   * to set those fields which will be used in operations.
   */
  TxRecord* createPattern() const;

  /**
   * Returns the column number which has the given name, or -1 if no column is found.
   * @param name The name of the column.
   * @throws TxDataException If no column with the provided name is found.
   */
  int getColumnForFieldName(const std::string& name) const;

  /**
   * Prints a human-readable version of the collection.  Primarily useful in
   * debugging.
   */
  std::string toString() const;

  /**
   * Determine the state of the last read operation (execute(SELECT)).
   * @return Returns TRUE if the last read operation didn't fill the read buffer,
   *         false otherwise.
   */
  bool finishedReading() const;

  /**
   * Reset the state of read operations.  Only necessary if the read continues
   * on the previous pattern - if it doesn't, then the read state is automatically
   * cleared.
   */
  void restartReading();

 private:
  /**
   * @internal
   * Purges all values in the pattern, clearing it out.
   */
  void purgePattern();

 private:
  std::string key_;
  std::string name_;

  TxRecord* pattern_;
  TxRecord* previousSelectPattern_; // Necessary for proper read buffering

  TxModule::TxOpFlag readFlag_;
};
#endif


