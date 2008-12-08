/*
 * TxMySQLModule.h
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
 * @(#)cpp/api:$Id: TxMySQLModule.h,v 1.1 2008/12/08 21:10:32 fine Exp $
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
 
#ifndef TXMYSQLMODULEH
#define TXMYSQLMODULEH

#include <string>
#include "TxModule.h"
#include "TxField.h"
#include "TxMySQLConnection.h"

class TxField;
class TxRecord;
class TxMySQLResult;


/**
 * Class representing a MySQL interface.
 */
class TxMySQLModule: public TxModule
{
 public:
  /**
   * Constructor: Set the module's connection information.  The user must still
   *              call open() explicitly.
   * @param location The location to connect to, in the form:
   *                 user(password)@hostname:port/dbname
   */
  TxMySQLModule(const std::string& location);

  /**
   * Destructor: Closes connection to MySQL server.
   */
  ~TxMySQLModule();

  /**
   * See documentation for TxModule.
   */
  virtual bool open();
  virtual void close();
  virtual bool isOpen();
  virtual int getBufferSize();
  virtual void setBufferSize(int size );
  virtual std::vector<TxRecord*> selectRecords(const std::string& collectionName, 
					       TxRecord* pattern,
					       TxOpFlag& flag );
  virtual void insertRecord(const std::string& collectionName, TxRecord* content);
  virtual void deleteRecords(const std::string& collectionName, TxRecord* pattern);
  virtual void updateRecords(const std::string& collectionName, TxRecord* pattern,
			     TxRecord* newContent);
  virtual TxCollectionHandler* describeCollection(const std::string& collectionName);
  virtual TxCollectionHandler* createCollection(const std::string& collectionName,
						TxRecord* pattern);
  virtual void deleteCollection(const std::string& collectionName);

 private:
  /**
   * @internal
   * Convert from a string representing a MySQL type (retrieved via
   * DESC) to an internally usable DataType.
   * @param mysqlType The MySQL type to convert.  Note that not all SQL
   *                  types are supported at this time.
   */
  TxField::DataType mysqlTypeToDataType(const std::string& mysqlType) const;

  /**
   * @internal Extracts the length of data from a mysql type.
   * @param mysqlType The MySQL type to extract length data from.
   */
  int extractDataLength(const std::string& mysqlType) const;

  /**
   * @internal
   * Creates a WHERE clause from the provided pattern.
   * @param pattern The pattern to convert.
   * @return A WHERE clause in the form:
   *   (col1="val1" and col2="val2" and ...)
   */
  std::string createWhereClause(TxRecord* pattern) const;

  /**
   * @internal Translates a DataType into a string which can be used
   * as a type in a mysql CREATE command.
   * @param dataType The type to convert to a string.
   * @param dataTypeStr The data type as a string to use in any thrown
   *                    exceptions.
   */
  std::string dataTypeToSqlType(TxField::DataType dataType, 
				const std::string& dataTypeStr) const;

 private:
  std::string fDbhost; //!The machine where MySQL resides inside
  std::string fUsername; //!Username to connect MySQL
  std::string fPassword; //!Password to connect MySQL
  std::string fDbname; //!Name of DB
  int fDbport;  //!Port to connect MySQL

  int fBuffersize; //!Buffersize

  TxMySQLConnection fConnection;//!Connection to MySQL
};

#endif
