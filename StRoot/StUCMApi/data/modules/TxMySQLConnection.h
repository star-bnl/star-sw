/*
 * TxMySQLConnection.h
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
 * @(#)cpp/api:$Id: TxMySQLConnection.h,v 1.1 2008/12/08 21:10:32 fine Exp $
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
 
#ifndef TXMYSQLCONNECTION_H
#define TXMYSQLCONNECTION_H

#include <mysql.h>
#include <string>

#include "TxMySQLResult.h"


//! Abstraction of MySQL connection
class TxMySQLConnection
{
 public:
  /**
   * Constructor: Initializes empty connection.
   */
  TxMySQLConnection();

  /**
   * Destructor: Closes connection.
   */
  ~TxMySQLConnection();
		
  /**
   * Closes connection to MySQL server.
   */
  void close();

  /**
   * Connects to the specified MySQL server.
   * @param host The URL where the MySQL host resides
   * @param uid The user ID to log in with
   * @param pw The password for the provided user
   * @param port The port to connect to on the host
   * @param db The DB to connect to.
   */
  void connect(const std::string& host, const std::string& uid, 
	       const std::string& pw, int port, const std::string& db);

  /**
   * Returns true if currently connected to a mySQL server.
   */
  bool isConnected() const;

  /**
   * Returns the number of rows affected by the last query.
   * @return Returns -1 if the last query was not UPDATE, DELETE,
   *         or INSERT.
   */
  int getNumOfAffectedRows() const;

  /**
   * Send a query to the MySQL server.
   * @param sql The SQL statement for the server to execute.
   * @throws TxModuleException If an error occurs while running a query.
   */
  TxMySQLResult* query(const std::string& sql);

 private:
  /**
   * Get MySQL error information.
   */
  std::string getError();

 private:
  MYSQL mMysql;
  MYSQL* mConnection;
};
#endif
