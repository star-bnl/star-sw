/*
 * TxModule.h
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
 * @(#)cpp/api:$Id: TxModule.h,v 1.1 2008/12/08 21:10:32 fine Exp $
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
 
#ifndef TXMODULE_H
#define TXMODULE_H

#include <string>
#include <vector>

#include "TxRecord.h"
class TxCollectionHandler;

/**
 * Interface class for module implementations.
 */
class TxModule {

 public:
  /**
   * Flag marking whether to preform a new opteration or continue the
   * previous read, if the buffer was filled.
   */
  enum TxOpFlag {
    NEWOP, 
    CONTINUEOP
  };
    	
  /**
   * Default constructor; empty
   */
  TxModule();

  /**
   * Default destructor; empty
   */
  virtual ~TxModule();

  /**
   * Opens module connection to store.
   */
  virtual bool open()=0;

  /**
   * Closes module connection to store.
   */
  virtual void close()=0;

  /**
   * Returns true if connection to store is open.
   */
  virtual bool isOpen()=0;

  /**
   * Returns size of read buffer.
   */
  virtual int getBufferSize() = 0;
  
  /**
   * Set the read buffer size.
   * @param size The new size of the read buffer.
   */
  virtual void setBufferSize(int size ) = 0;

  /**
   * Retrieves all records from the named collection matching the provided
   * pattern.  Uses the read buffer to throttle the amount of information 
   * that can be processed at one time.
   * @param collectionName The name of the collection to load records from.
   * @param pattern The pattern indicating which records to select.
   * @param flag PRECONDITION: Whether or not to continue the previous read operation.
   *             POSTCONDITION: Whether or not the operation needs to continue.
   * @return A list of the selected records.
   * @throws TxModuleException If a collection with the provided name does
   *                           not exist.
   */
  virtual std::vector<TxRecord*> selectRecords(const std::string& collectionName, 
					       TxRecord* pattern,
					       TxOpFlag& flag )=0;
  /**
   * Add a record to the named collection with the specified pattern.
   * @param collectionName The name of the collection to add the record to.
   * @param content The content of the new record.
   * @throws TxModuleException If the record already exists in the table.  This
   *                           is a non-fatal exception; should only WARN.
   * @throws TxModuleException If a collection with the provided name does
   *                           not exist.
   */
  virtual void insertRecord(const std::string& collectionName,
			    TxRecord* content)=0;

  /**
   * Deletes all records from the named table matching the specified pattern.
   * @param collectionName The name of the collection to delete records from.
   * @param pattern The matching pattern to select records to delete.
   * @throws TxModuleException If a collection with the provided name does
   *                           not exist.
   */
  virtual void deleteRecords(const std::string& collectionName, 
			     TxRecord* pattern)=0;

  /**
   * Updates all records from the named collection matching the old
   * pattern to contain the new content.  Note that the fields in
   * the old pattern and new content can be distinct.
   * @param collectionName The name of the collection to operate on.
   * @param pattern The pattern to match existing records against.
   * @param newContent The content to set on all matching records.
   * @throws TxModuleException If a collection with the provided name does
   *                           not exist.
   */
  virtual void updateRecords(const std::string& collectionName, TxRecord* pattern,
			     TxRecord* newContent)=0;

  /**
   * Retrieves a TxCollectionHandler for the named collection in the store.
   * @param collectionName The name of the collection to retrieve.
   * @return Returns a TxCollectionHandler* representing the collection.
   * @throws TxModuleException If the named collection does not exist.
   */
  virtual TxCollectionHandler* describeCollection(const std::string& collectionName)=0;

  /**
   * Creates a new collection with the specified name in the store.
   * @param collectionName The name of the collection to create.
   * @param pattern The pattern to associate with the new collection.  If a
   *                pattern field is marked NULL, it is assumed NULL values are
   *                allowed for that column.  The first field is the primary key.
   * @return Returns a TxCollectionHandler* to the new collection.
   * @throws TxModuleException If a collection with the provided name already exists.
   */
  virtual TxCollectionHandler* createCollection(const std::string& collectionName,
						TxRecord* pattern)=0;

  /**
   * Deletes the named collection from the store.
   * @param collectionName The name of the collection to delete.
   * @throws TxModuleException If a collection with the provided name does
   *                           not exist.
   */
  virtual void deleteCollection(const std::string& collectionName)=0;
};

#endif
