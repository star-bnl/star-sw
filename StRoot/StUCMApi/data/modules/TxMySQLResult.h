/*
 * TxMySQLResult.h
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
 * @(#)cpp/api:$Id: TxMySQLResult.h,v 1.1 2008/12/08 21:10:32 fine Exp $
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
 
#ifndef TXMYSQLRESULT_H
#define TXMYSQLRESULT_H
#include <string>
#include <mysql.h>
#include <sys/time.h>
#include "TxMySQLRow.h"

using namespace std;

class TxRecord;

/**
 * Abstraction of a MYSQL_RES structure.
 */
class TxMySQLResult
{
 public:
  /**
   * Constructor: Encapsulates MySQL result.
   */
  TxMySQLResult(MYSQL_RES*);

  /**
   * Destructor: Closes result.
   */
  ~TxMySQLResult();//!Deconstructor
	
  /**
   * Closes out and frees the encapsulated result.
   */
  void close();

  /**
   * Returns the next row from the result.
   */
  TxMySQLRow* nextRow();
	      
  /**
   * Returns the number of rows in the result.
   */
  int getRowCount();

  /**
   * Get the number of fields in the result.
   */
  int getFieldNumInResult();
		
  /**
   * Returns true if the result information is available.
   */
  bool isResultOpen();

 private:
  MYSQL_RES* mResult;
};

#endif
