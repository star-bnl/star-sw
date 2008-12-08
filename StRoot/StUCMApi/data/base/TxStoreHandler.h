/*
 * TxStoreHandler.h
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
 * @(#)cpp/api:$Id: TxStoreHandler.h,v 1.1 2008/12/08 21:10:30 fine Exp $
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

#ifndef TXSTOREHANDLER_H
#define TXSTOREHANDLER_H

#include <string>

#include "TxHandler.h"
#include "TxCollectionHandler.h"

/**
 * Abstraction for the data module in memory. In TxStoreHandler, we can control storage and collection.
 * This class also provide the entry point  for using the data tier. 
 */
class TxStoreHandler: public TxHandler {
 public:
  /**
   * Constructor: Creates/gets a module representing the store.
   * @param location The location of the store.
   */
  TxStoreHandler(const std::string& location);

  /**
   * Constructor: Wrapper for TxStoreHandler(string) to handle
   * NULL const char* properly.
   * @param location The location of the store.
   * @throws TxDataException if location == NULL.
   */
  TxStoreHandler(const char* location);

  /**
   * Constructor: Uses the provided module as the store.
   * @param module The module representing the store.
   */
  TxStoreHandler(TxModule* module);

  /**
   * Destructor: No-op.
   */
  ~TxStoreHandler();

  /**
   * Connects to the storage handler's module.  Note that this is SEPERATE from
   * the storage right now - the database represents a set of TxStorage objects.
   * Returns true if successful.
   */
  bool open();

  /**
   * Returns true if the module connection is currently open.
   */
  bool isOpen() const; 

  /**
   * Closes the database connection.  The user is responsible for explicitly
   * closing the store, as they are for explicitly opening it.
   */
  void close();

  /**
   * Performs an operation on the store represented by the handler.
   * The operations are defined as follows:
   *---------------------------------------------------------------------------
   *SELECT     || collection name || NULL    || 
   *  Returns the TxCollectionHandler associated with the collection name.
   *---------------------------------------------------------------------------
   *INSERT     || collection name || pattern || 
   *  Inserts an (empty) new table with the given name and pattern into 
   *  the store.  Returns a TxCollectionHandler associated with the new 
   *  collection.
   *  * The primary key is assumed to be the first field of the pattern.
   *---------------------------------------------------------------------------
   *DELETE     || collection name || NULL	   || 
   *  Deletes the collection with the given name.  Returns NULL.
   *---------------------------------------------------------------------------
   *UPDATE     || NULL            || NULL    || 
   *  Currently a no-op.  Returns NULL.
   *---------------------------------------------------------------------------
   *@param op The operation to perform.
   *@param name The name of the collection to operate on.
   *@param pattern The pattern of the collection to add.  Only used by the
   *               INSERT operation.  Note that if a field in the pattern is
   *               marked NULL, it is assumed that NULL is a valid value for
   *               this column in the table.  The first field is assumed to be
   *               the key, which cannot be NULL.
   *@throws TxDataException If missing a required argument, a no-op operation is called,
   *                        or an unknown operation is called.
   *@return Returns a TxCollectionHandler* object representing the collection operated on
   *        for SELECT and INSERT, otherwise NULL.  WARNING: Operations which select
   *        the same collection will return DUPLICATES, not a reference to the same object.
   */
  TxCollectionHandler* execute(TxHandler::Operation op, const std::string& name,
			       TxRecord* pattern = NULL) const;

  /**
   * Gets the read buffer size for the module.
   */
  int getBufferSize() const;

  /**
   * Sets the size of the module's read buffer.
   * @param size The new size of the buffer.
   */
  void setBufferSize(int size);
};

#endif
