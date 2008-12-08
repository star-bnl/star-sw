/*
 * TxMySQLResult.cpp
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
 * @(#)cpp/api:$Id: TxMySQLResult.cpp,v 1.1 2008/12/08 21:10:32 fine Exp $
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

#include <vector>
 
#include "TxField.h"
#include "TxRecord.h"
#include "TxMySQLResult.h"
#include "TxMySQLModule.h"
#include "TxModuleException.h"


TxMySQLResult::TxMySQLResult(MYSQL_RES *r) 
  : mResult(r) 
{} // EMPTY


TxMySQLResult::~TxMySQLResult() {
  if(mResult != NULL) { 
    close(); 
  }
}


bool TxMySQLResult::isResultOpen() {
  return (mResult != NULL);
}


void TxMySQLResult::close() {
  if( mResult == NULL) { 
    return; 
  }

  mysql_free_result(mResult);
  mResult = NULL;
}


TxMySQLRow* TxMySQLResult::nextRow() {
  MYSQL_ROW row;
  if( mResult == NULL) { 
    throw TxModuleException("SQL: Attempted to get data from closed result.",
			    TxModuleException::CONNECT); 
  }

  row = mysql_fetch_row(mResult);
  if( !row ) {
    return NULL;
  }
  else {
    return new TxMySQLRow(mResult, row);
  }
}


int TxMySQLResult::getRowCount() {
  if( mResult == NULL) { 
    throw TxModuleException("SQL: Attempted to get data from closed result.",
			    TxModuleException::CONNECT); 
  }
  
  return mysql_num_rows(mResult);
}


int TxMySQLResult::getFieldNumInResult() { 
  return mysql_num_fields(mResult);	
}
