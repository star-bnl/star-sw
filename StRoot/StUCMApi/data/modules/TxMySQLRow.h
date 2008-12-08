/*
 * TxMySQLRow.h
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
 * @(#)cpp/api:$Id: TxMySQLRow.h,v 1.1 2008/12/08 21:10:33 fine Exp $
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
 
#ifndef TXMYSQLROW_H
#define TXMYSQLROW_H

#include <mysql.h>

#include "TxField.h"

//!Abstract one row in the query result
class TxMySQLRow 
{
 public:
  /**
   * Constructor: Create an abstraction around a MySQL result representing
   * a row.
   */
  TxMySQLRow(MYSQL_RES* , MYSQL_ROW);

  /**
   * Destructor: Closes connection.
   */
  ~TxMySQLRow();//Deconstructor
	
  /**
   * Returns the field value for the specified column.
   * @param col Column to retrieve value from.
   * @return A TxField with a typed variant.
   */
  TxField* getField(int col);

  /**
   * Get the number of columns in the row.
   */
  int getFieldCount();

  /**
   * Test if the result set has been closed.
   */
  bool isClosed();

  /**
   * Closes out row and result by resetting the class
   * information.
   */
  void close();

 private:
  /**
   * @internal
   * Convert from an integer representing a MySQL type to an internally usable DataType.
   * @param mysqlType The MySQL type to convert.  Note that not all SQL
   *                  types are supported at this time.
   */
  TxField::DataType mysqlTypeToDataType(MYSQL_FIELD field) const;

  /**
   * @internal
   * Convert from an integer representing a MySQL type to a human-readable
   * string.
   * @param field The field which supplies the type information to convert.
   */
  std::string mysqlTypeToString(int mysqlType) const;


 private:
  MYSQL_RES* mResult;
  MYSQL_FIELD* mPattern;
  MYSQL_ROW mFields;
};
#endif 
