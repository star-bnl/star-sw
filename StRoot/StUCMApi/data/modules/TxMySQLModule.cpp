/*
 * TxMySQLModule.cpp
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
 * @(#)cpp/api:$Id: TxMySQLModule.cpp,v 1.2 2008/12/30 22:01:07 fine Exp $
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
#include <cstdlib>
#include <cstdio>
#include <sstream>
#include <sys/time.h>
#include <iostream>
#include <unistd.h>

#include "mysql.h"
#include "TxField.h"
#include "TxRecord.h"
#include "TxCollectionHandler.h"

#include "TxModule.h"
#include "TxMySQLRow.h"
#include "TxMySQLResult.h"
#include "TxMySQLModule.h"
#include "TxMySQLConnection.h"
#include "TxModuleException.h"

using namespace std;

TxMySQLModule::TxMySQLModule(const std::string& location) {
  // location format: user(password)@host:port/db

  size_t lpPos = location.find('(');
  size_t rpPos = location.find(')');
  size_t atPos = location.find('@');
  size_t colonPos = location.find(':');
  size_t slashPos = location.find('/');

  fUsername = location.substr(0,lpPos);
  fPassword = location.substr(lpPos+1, rpPos - (lpPos + 1));
  fDbhost = location.substr(atPos+1, colonPos - (atPos + 1));
  fDbport = atoi( location.substr(colonPos+1, slashPos - (colonPos+1)).c_str() );
  fDbname = location.substr(slashPos+1, location.length()-(slashPos+1));
       
  fBuffersize = 100;
}


TxMySQLModule::~TxMySQLModule() { 
  if (isOpen()) {
    fConnection.close();
  }
}


bool TxMySQLModule::isOpen() {
  return fConnection.isConnected();
}

bool TxMySQLModule::open() {
  fConnection.connect(fDbhost, fUsername, fPassword, fDbport, fDbname);

  return isOpen();
}


void TxMySQLModule::close() {
  fConnection.close();
}


void TxMySQLModule::setBufferSize(int newbuffersize) { 
  fBuffersize = newbuffersize; 
}


int TxMySQLModule::getBufferSize() { 
  return fBuffersize; 
}


std::vector<TxRecord*> TxMySQLModule::selectRecords(const std::string& collectionName,
						    TxRecord* pattern,
						    TxOpFlag& flag) {
  static int continueCount = 0;

  if (flag == CONTINUEOP) {
    continueCount++;
  }
  else {
    continueCount = 0;
  }

  std::string queryStr = "SELECT * FROM " + collectionName;
  TxMySQLResult* result = NULL;

  if (!isOpen()) {
    throw TxModuleException("SQL: Attempted operation on closed connection.",
			    TxModuleException::SELECT);
  }

  if (pattern != NULL) {
    queryStr += " WHERE " + createWhereClause(pattern);
  }

  // Add buffering limits to SQL
  // TODO: There should be a more efficient way that doesn't involve using two seperate streams.
  std::ostringstream converter1;
  converter1 << fBuffersize * continueCount;
  std::string start = converter1.str();

  std::ostringstream converter2;
  converter2 << fBuffersize;
  std::string limit = converter2.str();

  queryStr += " LIMIT " + start + "," + limit;

  int attempts = 0;

  while (attempts < 60) {
    try {
      result = fConnection.query(queryStr);
    }
    catch (...) {
      // Do nothing, result == NULL;
    }
    if (result) {
      break;
    }
    else {
      attempts++;
      sleep(1);
    }
  }

  std::vector<TxRecord*> records;
  if (result) {
    // TxMySQLResult* is guaranteed to be non-null on successful SELECT
    while (TxMySQLRow* mysqlRow = result->nextRow()) {
      std::vector<TxField*>* fields = new std::vector<TxField*>;
      for (int col= 1;
	   col <= mysqlRow->getFieldCount();
	   col++) {
	fields->push_back(mysqlRow->getField(col));
      }
      records.push_back(new TxRecord(fields));
      delete mysqlRow;
    }
    // Clean up memory
    delete result;
  }
  else {
    throw TxModuleException("SQL: Did not return an appropriate result "
			    "for query: " + queryStr,
			    TxModuleException::SELECT);
  }

  if (records.size() == fBuffersize) {
    flag = CONTINUEOP;
  }
  else {
    flag = NEWOP;
  }

  return records;
}


void TxMySQLModule::insertRecord(const std::string& collectionName,
				 TxRecord* content) {
  if (!isOpen()) {
    throw TxModuleException("SQL: Attempted operation on closed connection.",
			    TxModuleException::INSERT);
  }

  if (content->getFields()->empty()) {
    throw TxModuleException("SQL: Performed operation on record with empty data.",
			    TxModuleException::INSERT);
  }

  std::string columns = "(";
  std::string values = "(";
  std::vector<TxField*>* fields = content->getFields();

  // Set columns and values
  for (std::vector<TxField*>::iterator iter = fields->begin();
       iter != fields->end();
       iter++) {
    if (!(*iter)->isIgnore()) {
      columns += (*iter)->getName() + ",";
      // If the value should be NULL, insert it appropriately.
      if ((*iter)->isNull()) {
	values += "NULL,";
      }
      else {
	if ((*iter)->getType() == TxField::STRING &&
	    (*iter)->getValueAsString() != "CURRENT_TIMESTAMP") {
	  // If a string, quote value... unless it's CURRENT_TIMESTAMP.
	  // TODO: We need better handling for keywords (like NULL, CURRENT_TIMESTAMP, etc.)
	  // because maybe somebody WANTS to store a CURRENT_TIMESTAMP string.
	  values += "\"" + (*iter)->getValueAsString() + "\",";
	}
	else {
	  values += (*iter)->getValueAsString() + ","; 
	}
      }
    }
  }

  // String has no 'pop' - do something goofy to remove the last comma
  // and close parens
  columns = columns.substr(0, columns.length()-1) + ")";
  values = values.substr(0, values.length()-1) + ")";

  std::string queryStr = "INSERT INTO " + collectionName + " " + columns + " VALUES " 
                          + values;

  TxMySQLResult* res = (TxMySQLResult*)0x01;  // non-NULL is the bad value here
  int attempts = 0;

  while (attempts < 60) {
    try {
      res = fConnection.query(queryStr);
    }
    catch (...) {
      // Do nothing, res != NULL
    }
    if (!res) {
      break;
    }
    else {
      attempts++;
      sleep(1);
    }
  }

  if (res) {
    // Result was returned - something was weird with the query,
    // since no results are returned for INSERT operations.
    if (res != (TxMySQLResult*)0x01)
      delete res;
    throw TxModuleException("SQL: Retrieved invalid result information.",
			    TxModuleException::INSERT);
  }
}


void TxMySQLModule::deleteRecords(const std::string& collectionName, 
				  TxRecord* pattern) {
  if (!isOpen()) {
    throw TxModuleException("SQL: Attempted operation on closed connection.",
			    TxModuleException::DELETE);
  }

  std::string sql = "DELETE FROM " + collectionName + " WHERE " 
                     + createWhereClause(pattern);

  TxMySQLResult* res = (TxMySQLResult*)0x01;
  int attempts = 0;
  
  while (attempts < 60) {
    try {
      res = fConnection.query(sql);
    }
    catch (...) {
      // Do nothing, res != NULL
    }
    if (!res) {
      break;
    }
    else {
      attempts++;
      sleep(1);
    }
  }

  if (res) {
    // Result was returned - something was weird with the query,
    // since no results are returned for INSERT operations.
    if (res != (TxMySQLResult*)0x01)
      delete res;
    throw TxModuleException("SQL: Retrieved invalid result information.",
			    TxModuleException::DELETE);
  }
}


void TxMySQLModule::updateRecords(const std::string& collectionName,
				  TxRecord* pattern, TxRecord* newContent) {
  std::string updateValues;

  if (!isOpen()) {
    throw TxModuleException("SQL: Attempted operation on closed connection.",
			    TxModuleException::UPDATE);
  }

  for (std::vector<TxField*>::iterator iter = newContent->getFields()->begin();
       iter != newContent->getFields()->end();
       iter++) {
    if (!(*iter)->isIgnore()) {
      updateValues += (*iter)->getName() + "=";
      // If the value should be NULL, insert it appropriately.
      if ((*iter)->isNull()) {
	updateValues += "NULL,";
      }
      else {
	if ((*iter)->getType() == TxField::STRING &&
	    (*iter)->getValueAsString() != "CURRENT_TIMESTAMP") {
	  updateValues += "\"" + (*iter)->getValueAsString() + "\",";
	}
	else {
	  updateValues += (*iter)->getValueAsString() + ","; 
	}
      }
    }
  }

  // String has no pop, so do something goofy
  updateValues = updateValues.substr(0, updateValues.length()-1);

  std::string sql = "UPDATE " + collectionName + " SET " + updateValues
                     + " WHERE " + createWhereClause(pattern);
  
  TxMySQLResult* res = (TxMySQLResult*)0x01;
  int attempts = 0;

  while (attempts < 60) {
    try {
      res = fConnection.query(sql);
    }
    catch (...) {
      // Skip, res != NULL
    }
    if (!res) {
      break;
    }
    else {
      attempts++;
      sleep(1);
    }
  }

  if (res) {
    // Result was returned - something was weird with the query,
    // since no results are returned for INSERT operations.
    if (res != (TxMySQLResult*)0x01) {
      delete res;
    }
    throw TxModuleException("SQL: Retrieved invalid result information.",
			    TxModuleException::UPDATE);
  }
}


// TODO: Allow better 'where' clauses (ex: 'or' instead of 'and')
std::string TxMySQLModule::createWhereClause(TxRecord* pattern) const {
  std::string whereClause = "(";

  for (std::vector<TxField*>::const_iterator iter = pattern->getFields()->begin();
       iter != pattern->getFields()->end();
       iter++) {
    if (!(*iter)->isIgnore()) {
      whereClause += (*iter)->getName();
      if ((*iter)->isNull()) {
	whereClause += " IS NULL and ";
      }
      else {
	if ((*iter)->getType() == TxField::STRING) {
	  whereClause += "=\"" + (*iter)->getValueAsString() + "\" and ";
	}
	else {
	  whereClause += "=" + (*iter)->getValueAsString() + " and ";
	}
      }
    }
  }

  // "Pop" the last " and " (5 chars) and add the closing paren
  whereClause = whereClause.substr(0,whereClause.length()-5) + ")";

  return whereClause;
}


TxCollectionHandler* TxMySQLModule::describeCollection(const std::string& name) {
  if (!isOpen()) {
    throw TxModuleException("SQL: Attempted operation on closed connection.",
			    TxModuleException::DESCRIBE);
  }
  
  std::string sql = "DESC " + name;
  
  TxMySQLResult* res = NULL;

  for (int attempts = 0; attempts < 60; attempts++) {
    try {
      res = fConnection.query(sql);
    }
    catch (...) {
      // Do nothing, res == NULL
    }

    if (res) {
      break;
    }
    else {
      sleep(1);
    }
  }

  if (!res) {
    throw TxModuleException("SQL: Retrieved invalid result information.",
			    TxModuleException::DESCRIBE);
  }
  
  std::vector<TxField*>* pattern = new std::vector<TxField*>;
  std::string key;

  while (TxMySQLRow* colInfo = res->nextRow()) {
    TxField* nameField = colInfo->getField(1);
    TxField* typeField = colInfo->getField(2);
    TxField* nullableField = colInfo->getField(3);
    TxField* keyField = colInfo->getField(4);

    if (keyField->getValueAsString() == "PRI") {
      key = nameField->getValueAsString();
    }

    TxField* cell = 
      new TxField(nameField->getValueAsString(),
		  mysqlTypeToDataType(typeField->getValueAsString()),
		  extractDataLength(typeField->getValueAsString()));

    if (nullableField->getValueAsString() == "YES") {
      cell->setNull(true);
    }
    else {
      cell->setNull(false);
    }

    // Returned fields should always be marked IGNORE
    cell->setIgnore(true);

    pattern->push_back(cell);

    // Clean up
    delete nameField;
    delete typeField;
    delete nullableField;
    delete keyField;
  }

  delete res;
  return new TxCollectionHandler(name, key, new TxRecord(pattern), this);
}


TxCollectionHandler* TxMySQLModule::createCollection(const std::string& name,
						     TxRecord* pattern) {
  
  if (!isOpen()) {
    throw TxModuleException("SQL: Attempted operation on closed connection.",
			    TxModuleException::CREATE);
  }

  std::string sql = "CREATE TABLE " + name + " (";
  // Handle the first field outside the loop, since it gets treated as the key.
  std::vector<TxField*>::iterator iter = pattern->getFields()->begin();
  // Loop to the first non-ignore setting
  while ((*iter)->isIgnore() && iter != pattern->getFields()->end()) {
    iter++;
  }

  if (iter == pattern->getFields()->end()) {
    throw TxModuleException("SQL: Attempted operation on pattern with all "
			     "fields set IGNORE.",
			    TxModuleException::CREATE);
  }

  sql += (*iter)->getName() + " " + dataTypeToSqlType((*iter)->getType(), 
						      (*iter)->getTypeAsString())
          + " NOT NULL PRIMARY KEY,";
  iter++;

  for (; // Condition preset
       iter != pattern->getFields()->end();
       iter++) {
    if (!(*iter)->isIgnore()) {
      std::string nullable;
      if ((*iter)->isNull()) {
	nullable = "NULL";
      }
      else {
	nullable = "NOT NULL";
      }
      
      sql += (*iter)->getName() + " " + dataTypeToSqlType((*iter)->getType(),
							  (*iter)->getTypeAsString())
	+ " " + nullable + ",";
    }
  }
   
  // Pop the last comma and replace with a closing bracket
  sql = sql.substr(0,sql.length()-1) + ")";
    
  TxMySQLResult* res = fConnection.query(sql);

  if (res) {
    // Result was returned - something was weird with the query,
    // since no results are returned for INSERT operations.
    delete res;
    throw TxModuleException("SQL: Retrieved invalid result information.",
			    TxModuleException::CREATE);
  }

  return new TxCollectionHandler(name, pattern->getField(1)->getName(), 
				 new TxRecord(*pattern), this);
}


void TxMySQLModule::deleteCollection(const std::string& name) {
  if (!isOpen()) {
    throw TxModuleException("SQL: Attempted operation on closed connection.",
			    TxModuleException::DROP);
  }

  std::string sql = "DROP TABLE " + name;

  TxMySQLResult* res = fConnection.query(sql);

  if (res) {
    // Result was returned - something was weird with the query,
    // since no results are returned for INSERT operations.
    delete res;
    throw TxModuleException("SQL: Retrieved invalid result information.",
			    TxModuleException::DROP);
  }
}


std::string TxMySQLModule::dataTypeToSqlType(TxField::DataType dataType,
					     const std::string& dataTypeStr) const {
  switch (dataType) {
    case TxField::LONG:
      return "INT";
      break;
    case TxField::ULONG:
      return "INT UNSIGNED";
      break;
    case TxField::DOUBLE:
      return "DOUBLE";
      break;
    case TxField::STRING:
      return "VARCHAR(64)";
      break;
  default:
    throw TxModuleException("SQL: Unable to translate data type " + dataTypeStr 
			     + " to mySQL type",
			    TxModuleException::MISC);
  }
  return "";
}


TxField::DataType TxMySQLModule::mysqlTypeToDataType(const std::string& mysqlType) const {
  // TODO: Make sure that this information is accurate for stranger type definitions
  // such as TEXT CHARACTER SET ... COLLATE ...
  // Data is returned in the form:
  // Numeric types: [type]([precision]) [unsigned?] [zerofill?]
  // String/binary types: [type]([length])
  // Date types: [type]
  // Text/blob types: [type]

  // Splitting on '(' always retrieves pure type, and we take it from there.
  // TODO: For character information we may want to preserve this length information.
  std::string type = mysqlType.substr(0,mysqlType.find('('));

  // All integer types contain 'int', 'bit' should be considered integer for safety
  if (type.find("int") != std::string::npos ||
      type == "bit") {
    // Don't support bigint (out of range)
    if (type != "bigint") {
      if (mysqlType.find("unsigned") != std::string::npos) {
	return TxField::ULONG;
      }
      else {
	return TxField::LONG;
      }
    }
  }
  // All character types contain 'char', all text types contain 'text'
  else if (type.find("char") != std::string::npos ||
	   type.find("text") != std::string::npos) {
    return TxField::STRING;
  }
  // Handle float types
  // TODO: Support unsigned float types
  else if (type == "real" ||
	   type == "double" ||
	   type == "float" ||
	   type == "decimal" ||
	   type == "numeric") {
    return TxField::DOUBLE;
  }
  // Handle date/time types
  else if (type == "date" ||
	   type == "time" ||
	   type == "timestamp" ||
	   type == "datetime" ||
	   type == "year") {
    return TxField::STRING;
  }
  
  throw TxModuleException("SQL: Unsupported type: " + mysqlType,
			  TxModuleException::MISC);
}
  

int TxMySQLModule::extractDataLength(const std::string& mysqlType) const {
  // TODO: Make a special date type?  In any case, these are always exactly
  // 19 characters long.
  if ( mysqlType == "date" ||
       mysqlType == "time" ||
       mysqlType == "timestamp" ||
       mysqlType == "datetime" ||
       mysqlType == "year") {
    return 19;
  }

  size_t lp = mysqlType.find('(');
  size_t rp = mysqlType.find(')');

  if (lp == std::string::npos ||
      rp == std::string::npos) {
    return 0;
  }

  std::string lenStr = mysqlType.substr(lp+1, rp-(lp+1));
  std::istringstream converter(lenStr);
  int length = 0;

  converter >> length;
  return length;
}
