/*
 * TxMySQLConnection.cpp
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
 * @(#)cpp/api:$Id: TxMySQLConnection.cpp,v 1.2 2008/12/30 22:01:07 fine Exp $
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
 
#include "TxMySQLConnection.h"
#include "TxModuleException.h"
#include <unistd.h>


TxMySQLConnection::TxMySQLConnection() 
  :  mConnection(NULL)
{
  memset(&mMysql,0,sizeof(MYSQL));
}


void TxMySQLConnection::connect(const std::string& host,
				const std::string& uid,
				const std::string& pw,
				int port,
				const std::string& db) {
  if( !isConnected() ) { 
    mysql_init(&mMysql);

    for (int attempts=0; attempts < 300; attempts++) {
      mConnection =
	mysql_real_connect(&mMysql, host.c_str(), uid.c_str(), pw.c_str(), 0, 
			   port, 0, 0);
      if (isConnected()) {
	break;
      }
      else {
	sleep(1);
      }
    }

    if( !isConnected() ) { 
      throw TxModuleException("SQL error: " + getError(),
			      TxModuleException::CONNECT);
    }

    if (mysql_select_db(mConnection, db.c_str()) != 0)
    {
      throw TxModuleException("SQL error: " + getError(),
			      TxModuleException::CONNECT);
    }
  }
}


TxMySQLConnection::~TxMySQLConnection() {
  if( isConnected() ) {
    close(); 
  }
}


void TxMySQLConnection::close() {
  if( !isConnected() ) {
    return; 
  }
  mysql_close(mConnection);
  mConnection = NULL;
}


TxMySQLResult* TxMySQLConnection::query(const std::string& sql) {
  MYSQL_RES* res;
  int state;

  if( !isConnected() ) {
    throw TxModuleException("SQL: Attempted query on unconnected module.",
			    TxModuleException::CONNECT);
  }

  state = mysql_query(mConnection, sql.c_str());

  if( state < 0 ) { 
    throw TxModuleException("SQL query error: " + getError(),
			    TxModuleException::CONNECT);
  }

  res = mysql_store_result(mConnection);

  // if the result was null, it was an update or an error occurred
  if( res == NULL ) {
    if( mysql_errno(mConnection) != 0 ) {
      throw TxModuleException("SQL query error: " + getError(),
			      TxModuleException::CONNECT);
    }

    // Return NULL for updates
    return NULL;
  }

  // Return result wrapper for non-update operations
  return new TxMySQLResult(res);
}

std::string TxMySQLConnection::getError() {
  if( isConnected()) {
    return mysql_error(mConnection);
  } 
  else {
    return mysql_error(&mMysql); 
  }
}


bool TxMySQLConnection::isConnected() const {
  return (mConnection != NULL);
}
