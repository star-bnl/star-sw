/*
 * TxMySQLRow.cpp
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
 * @(#)cpp/api:$Id: TxMySQLRow.cpp,v 1.1 2008/12/08 21:10:33 fine Exp $
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
 
#include "TxMySQLRow.h"
#include "TxModuleException.h"


TxMySQLRow::TxMySQLRow(MYSQL_RES* res, MYSQL_ROW row) {
  mFields = row;
  mResult = res;
  mPattern = mysql_fetch_fields(res);
}


TxMySQLRow::~TxMySQLRow() {
  if( !isClosed() ) { 
    close(); 
  }
}


void TxMySQLRow::close() {
  if( isClosed() ) { 
    return;
  }
  mFields = NULL;
  mResult = NULL;
}


int TxMySQLRow::getFieldCount() {
  if( isClosed() ) { 
    throw TxModuleException("SQL: Attempted to get data from closed row.",
			    TxModuleException::CONNECT);
  }
  return mysql_num_fields(mResult);
}


TxField* TxMySQLRow::getField(int field) {

  if( isClosed() ) { 
    throw TxModuleException("SQL: Attempted to get data from closed row.",
			    TxModuleException::CONNECT);
  }

  if( field < 1 || field > getFieldCount() ) { 
    throw TxModuleException("SQL: Field index out of bounds.",
			    TxModuleException::MISC); 
  }

  MYSQL_FIELD fieldInfo = mPattern[field-1];
  std::string value;
  int length;

  // TODO: We only care about the width when creating a pattern...
  // not when extracting an actual row.  Find some better, safer
  // way to take care of this.

  bool isNull = false;

  if (mFields[field-1] && mFields[field-1] != "") {
    value = mFields[field-1];
    length = fieldInfo.max_length;
  }
  else {
    value = "";
    isNull = true;
    length = 120; // Current maximum length of anything in the table, ever.
  }

  TxField::DataType type = mysqlTypeToDataType(fieldInfo);

  TxField* resultField = new TxField(fieldInfo.name, value, type, length);
  resultField->setNull(isNull);
  // Results obtained from DB should always initially be IGNORE
  resultField->setIgnore(true);

  return resultField;
}


bool TxMySQLRow::isClosed() { 
  return (mFields == NULL); 
}


TxField::DataType TxMySQLRow::mysqlTypeToDataType(MYSQL_FIELD field) const {
  switch (field.type) {
     case MYSQL_TYPE_TINY:
     case MYSQL_TYPE_SHORT:
     case MYSQL_TYPE_LONG:
     case MYSQL_TYPE_INT24:
       if (field.flags & UNSIGNED_FLAG) {
	 return TxField::ULONG;
       }
       else {
	 return TxField::LONG;
       }
       break;
    case MYSQL_TYPE_FLOAT:
    case MYSQL_TYPE_DOUBLE:
    case MYSQL_TYPE_DECIMAL:
      return TxField::DOUBLE;
      break;
    case MYSQL_TYPE_TIMESTAMP:
    case MYSQL_TYPE_DATE:
    case MYSQL_TYPE_TIME:
    case MYSQL_TYPE_DATETIME:
    case MYSQL_TYPE_YEAR:
      return TxField::STRING;
      break;
    // These cases need special handling to determine if they are binary or character data
    case MYSQL_TYPE_STRING:
    case MYSQL_TYPE_VAR_STRING:
    case MYSQL_TYPE_BLOB:
      if (field.charsetnr != 63) { // Indicates non-binary data
	return TxField::STRING;
      }
    default: {
      std::string binary;
      if (field.charsetnr == 63) { // Indicates binary data
	binary = "(BINARY)";
      }
      throw TxModuleException("SQL: No internal representation for type "
			       + mysqlTypeToString(field.type) + binary,
			      TxModuleException::MISC);
    }
  }
  return TxField::INVALID;
}


std::string TxMySQLRow::mysqlTypeToString(int mysqlType) const {
  switch (mysqlType) {
    case MYSQL_TYPE_TINY:
      return "TINYINT";
    case MYSQL_TYPE_SHORT:
      return "SMALLINT";
    case MYSQL_TYPE_LONG: 
      return "INTEGER";
    case MYSQL_TYPE_INT24:
      return "MEDIUMINT";
    case MYSQL_TYPE_LONGLONG:
      return "BIGINT";
    case MYSQL_TYPE_DECIMAL:
      return "DECIMAL";
    case MYSQL_TYPE_FLOAT:
      return "FLOAT";
    case MYSQL_TYPE_DOUBLE:
      return "DOUBLE";
    case MYSQL_TYPE_TIMESTAMP:
      return "TIMESTAMP";
    case MYSQL_TYPE_DATE:
      return "DATE";
    case MYSQL_TYPE_TIME:
      return "TIME";
    case MYSQL_TYPE_DATETIME:
      return "DATETIME";
    case MYSQL_TYPE_YEAR:
      return "YEAR";
    case MYSQL_TYPE_STRING:
      return "CHAR";
    case MYSQL_TYPE_VAR_STRING:
      return "VARCHAR";
    case MYSQL_TYPE_BLOB:
      return "BLOB";
    case MYSQL_TYPE_SET:
      return "SET";
    case MYSQL_TYPE_ENUM:
      return "ENUM";
    case MYSQL_TYPE_GEOMETRY:
      return "GEOMETRY";
    case MYSQL_TYPE_NULL:
      return NULL;
    default:
      return "UNKNOWN";
  }
}
