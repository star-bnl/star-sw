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
 * @(#)cpp/api:$Id: TxMySQLRow.cpp,v 1.2 2008/12/30 22:01:07 fine Exp $
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

#ifndef FIELD_TYPE_TINY
#define MYSQL_TYPE_TINY FIELD_TYPE_TINY
#endif
#ifndef FIELD_TYPE_SHORT
#define MYSQL_TYPE_SHORT FIELD_TYPE_SHORT
#endif
#ifndef FIELD_TYPE_LONG
#define MYSQL_TYPE_LONG FIELD_TYPE_LONG
#endif
#ifndef FIELD_TYPE_INT24
#define MYSQL_TYPE_INT24 FIELD_TYPE_INT24
#endif
#ifndef FIELD_TYPE_FLOAT
#define MYSQL_TYPE_FLOAT FIELD_TYPE_FLOAT
#endif
#ifndef FIELD_TYPE_DOUBLE
#define MYSQL_TYPE_DOUBLE FIELD_TYPE_DOUBLE
#endif
#ifndef FIELD_TYPE_DECIMAL
#define MYSQL_TYPE_DECIMAL FIELD_TYPE_DECIMAL
#endif
#ifndef FIELD_TYPE_TIMESTAMP
#define MYSQL_TYPE_TIMESTAMP FIELD_TYPE_TIMESTAMP
#endif
#ifndef FIELD_TYPE_DATE
#define MYSQL_TYPE_DATE FIELD_TYPE_DATE
#endif
#ifndef FIELD_TYPE_TIME
#define MYSQL_TYPE_TIME FIELD_TYPE_TIME
#endif
#ifndef FIELD_TYPE_DATETIME
#define MYSQL_TYPE_DATETIME FIELD_TYPE_DATETIME
#endif
#ifndef FIELD_TYPE_YEAR
#define MYSQL_TYPE_YEAR FIELD_TYPE_YEAR
#endif
#ifndef FIELD_TYPE_VAR_STRING
#define MYSQL_TYPE_VAR_STRING FIELD_TYPE_VAR_STRING
#endif
#ifndef FIELD_TYPE_STRING
#define MYSQL_TYPE_STRING FIELD_TYPE_STRING
#endif
#ifndef FIELD_TYPE_BLOB
#define MYSQL_TYPE_BLOB FIELD_TYPE_BLOB
#endif
#ifndef FIELD_TYPE_TINY
#define MYSQL_TYPE_TINY FIELD_TYPE_TINY
#endif
#ifndef FIELD_TYPE_SHORT
#define MYSQL_TYPE_SHORT FIELD_TYPE_SHORT
#endif
#ifndef FIELD_TYPE_LONG
#define MYSQL_TYPE_LONG FIELD_TYPE_LONG
#endif
#ifndef FIELD_TYPE_INT24
#define MYSQL_TYPE_INT24 FIELD_TYPE_INT24
#endif
#ifndef FIELD_TYPE_LONGLONG
#define MYSQL_TYPE_LONGLONG FIELD_TYPE_LONGLONG
#endif
#ifndef FIELD_TYPE_DECIMAL
#define MYSQL_TYPE_DECIMAL FIELD_TYPE_DECIMAL
#endif
#ifndef FIELD_TYPE_FLOAT
#define MYSQL_TYPE_FLOAT FIELD_TYPE_FLOAT
#endif
#ifndef FIELD_TYPE_DOUBLE
#define MYSQL_TYPE_DOUBLE FIELD_TYPE_DOUBLE
#endif
#ifndef FIELD_TYPE_TIMESTAMP
#define MYSQL_TYPE_TIMESTAMP FIELD_TYPE_TIMESTAMP
#endif
#ifndef FIELD_TYPE_DATE
#define MYSQL_TYPE_DATE FIELD_TYPE_DATE
#endif
#ifndef FIELD_TYPE_TIME
#define MYSQL_TYPE_TIME  FIELD_TYPE_TIME
#endif
#ifndef FIELD_TYPE_DATETIME
#define MYSQL_TYPE_DATETIME FIELD_TYPE_DATETIME
#endif
#ifndef FIELD_TYPE_YEAR
#define MYSQL_TYPE_YEAR FIELD_TYPE_YEAR
#endif
#ifndef FIELD_TYPE_STRING
#define MYSQL_TYPE_STRING FIELD_TYPE_STRING
#endif
#ifndef FIELD_TYPE_BLOB
#define MYSQL_TYPE_BLOB FIELD_TYPE_BLOB
#endif
#ifndef FIELD_TYPE_SET
#define MYSQL_TYPE_SET FIELD_TYPE_SET
#endif
#ifndef FIELD_TYPE_ENUM
#define MYSQL_TYPE_ENUM FIELD_TYPE_ENUM
#endif
#ifndef FIELD_TYPE_GEOMETRY
#define MYSQL_TYPE_GEOMETRY FIELD_TYPE_GEOMETRY
#endif
#ifndef FIELD_TYPE_NULL
#define MYSQL_TYPE_NULL FIELD_TYPE_NULL
#endif

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


TxField::DataType TxMySQLRow::mysqlTypeToDataType(MYSQL_FIELD field) const 
{
   // Convert the MySQL type to the UCM types
  TxField::DataType ucmType = TxField::INVALID;
  switch (field.type) {
     case MYSQL_TYPE_TINY:
     case MYSQL_TYPE_SHORT:
     case MYSQL_TYPE_LONG:
     case MYSQL_TYPE_INT24:
       ucmType = (field.flags & UNSIGNED_FLAG) ? TxField::ULONG 
                                               : TxField::LONG;
       break;
    case MYSQL_TYPE_FLOAT:
    case MYSQL_TYPE_DOUBLE:
    case MYSQL_TYPE_DECIMAL:
      ucmType = TxField::DOUBLE;
      break;
    case MYSQL_TYPE_TIMESTAMP:
    case MYSQL_TYPE_DATE:
    case MYSQL_TYPE_TIME:
    case MYSQL_TYPE_DATETIME:
    case MYSQL_TYPE_YEAR:
      ucmType = TxField::STRING;
      break;
    // These cases need special handling to determine if they are binary or character data
    case MYSQL_TYPE_STRING:
    case MYSQL_TYPE_VAR_STRING:
    case MYSQL_TYPE_BLOB: 
       ucmType = TxField::STRING;
#ifndef MYSQL_TYPE_SET
       if  (field.charsetnr != 63)
#endif       
       break;
    default: {
        ucmType = TxField::INVALID;
        std::string binary;
#ifndef MYSQL_TYPE_SET
        if (field.charsetnr == 63) { 
           // Indicates binary data
           binary = "(BINARY)";
        }
#endif        
        throw TxModuleException("SQL: No internal representation for type "
			       + mysqlTypeToString(field.type) + binary,
			      TxModuleException::MISC);
        break;
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
