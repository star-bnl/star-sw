/*
 * TxStoreHandler.cpp
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
 * @(#)cpp/api:$Id: TxStoreHandler.cpp,v 1.1 2008/12/08 21:10:30 fine Exp $
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

#include "TxStoreHandler.h"
#include "TxModuleFactory.h"
#include "TxDataException.h"

TxStoreHandler::TxStoreHandler(const std::string& location)
  : TxHandler(TxModuleFactory::createModule(location)) 
{} // EMPTY


TxStoreHandler::TxStoreHandler(const char* location) {
  if (location == NULL) {
    throw TxDataException("Provided with NULL connectionInfo",
			  TxDataException::STORE);
  }

  setModule(TxModuleFactory::createModule(location));
}


TxStoreHandler::TxStoreHandler(TxModule* module)
  : TxHandler(module)
{} // EMPTY


TxStoreHandler::~TxStoreHandler() 
{} // EMPTY

bool TxStoreHandler::open() {
  return getModule()->open();
}


void TxStoreHandler::close() {
  getModule()->close();
}


bool TxStoreHandler::isOpen() const {
  return getModule()->isOpen();
}


int TxStoreHandler::getBufferSize() const {
  return getModule()->getBufferSize();
}


void TxStoreHandler::setBufferSize(int size) {
  getModule()->setBufferSize(size);
}


TxCollectionHandler* TxStoreHandler::execute(TxHandler::Operation op,
					     const std::string& name,
					     TxRecord* pattern) const {
  switch (op) {
    case SELECT:
      return getModule()->describeCollection(name);
      break;
    case INSERT:
      if (!pattern) {
	throw TxDataException("Empty pattern provided in INSERT.",
			      TxDataException::STORE);
      }
      return getModule()->createCollection(name, pattern);
      break;
    case DELETE:
      getModule()->deleteCollection(name);
      return NULL;
      break;
    case UPDATE:
      // No-op
      return NULL;
      break;
    default:
      throw TxDataException("Attempted unrecognized operation.",
			    TxDataException::STORE);
  }
}


