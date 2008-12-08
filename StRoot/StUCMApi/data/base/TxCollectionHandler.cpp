/*
 * TxCollection.cpp
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
 * @(#)cpp/api:$Id: TxCollectionHandler.cpp,v 1.1 2008/12/08 21:10:29 fine Exp $
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

#include "TxField.h"
#include "TxRecord.h"
#include "TxCollectionHandler.h"
#include "TxDataException.h"


TxCollectionHandler::TxCollectionHandler(const std::string& name, 
					 TxRecord* pattern,
					 TxModule* module) 
  : TxHandler(module),
    key_(""),
    name_(""),
    pattern_(NULL),
    previousSelectPattern_(NULL),
    readFlag_(TxModule::NEWOP)
{
  // The collection is in an invalid state (empty name/key, null pattern)
  // so throwing this exception is kosher.
  if (pattern->getFields()->size() == 0) {
    throw TxDataException("Initialization with empty pattern.",
			  TxDataException::COLLECTION);
  }

  name_ = name;
  pattern_ = pattern;
  purgePattern();

  key_ = (*(pattern_->getFields()))[0]->getName();
}


TxCollectionHandler::TxCollectionHandler(const std::string& name, 
					 const std::string& key,
					 TxRecord* pattern,
					 TxModule* module) 
  : TxHandler(module),
    key_(""),
    name_(""),
    pattern_(NULL),
    previousSelectPattern_(NULL),
    readFlag_(TxModule::NEWOP)
{
  // The collection is in an invalid state (empty name/key, null pattern)
  // so throwing this exception is kosher.
  if (pattern->getFields()->size() == 0) {
    throw TxDataException("Initialization with empty pattern.",
			  TxDataException::COLLECTION);
  }

  name_ = name;
  pattern_ = pattern;
  purgePattern();

  for (std::vector<TxField*>::iterator iter = pattern_->getFields()->begin();
       iter != pattern_->getFields()->end();
       iter++) {
    if (key == (*iter)->getName()) {
      key_ = key;
      return;
    }
  }

  // TODO: We need a way to log when this happens.
  key_ = (*(pattern_->getFields()))[0]->getName();
}


TxCollectionHandler::~TxCollectionHandler() {
  delete pattern_;
  delete previousSelectPattern_;
}


std::vector<TxRecord*> 
TxCollectionHandler::execute(TxHandler::Operation op, 
			     TxRecord* pattern,
			     TxRecord* newPattern) {
  switch (op) {
    case SELECT: {
      // Determine if we have a continued read or not.
      if (previousSelectPattern_ && pattern) {
	std::vector<TxField*>* patternFields = pattern->getFields();
	std::vector<TxField*>* previousFields = previousSelectPattern_->getFields();
	
	if (patternFields->size() == previousFields->size()) {
	  for (unsigned int i=0; i < patternFields->size(); i++) {
	    if ((*patternFields)[i]->getValueAsString() 
		!= (*previousFields)[i]->getValueAsString()) {
	      readFlag_ = TxModule::NEWOP;
	      delete previousSelectPattern_;
	      previousSelectPattern_ = new TxRecord(*pattern);
	      break;
	    }
	  }
	}
	else {
	  readFlag_ = TxModule::NEWOP;
	  delete previousSelectPattern_;
	  previousSelectPattern_ = new TxRecord(*pattern);
	}
      }
      else if (previousSelectPattern_ && !pattern) {
	delete previousSelectPattern_;
	previousSelectPattern_ = NULL;
	readFlag_ = TxModule::NEWOP;
      }

      // Call modifies readFlag_ value.
      return getModule()->selectRecords(name_, pattern, readFlag_);
      break;
    }
    // All other operations must clear previous SELECT if they succeed, 
    // since they modify SELECTable data.
    case INSERT:
      getModule()->insertRecord(name_, pattern);

      delete previousSelectPattern_;
      readFlag_ = TxModule::NEWOP;
      previousSelectPattern_ = NULL;
      break;
    case DELETE:
      getModule()->deleteRecords(name_, pattern);

      delete previousSelectPattern_;
      readFlag_ = TxModule::NEWOP;
      previousSelectPattern_ = NULL;
      break;
    case UPDATE:
      if (!newPattern) {
	throw TxDataException("Attempted UPDATE with NULL new pattern",
			      TxDataException::COLLECTION);
      }
      getModule()->updateRecords(name_, pattern, newPattern);

      delete previousSelectPattern_;
      readFlag_ = TxModule::NEWOP;
      previousSelectPattern_ = NULL;
      break;
    default:
      throw TxDataException("Attempted unrecognized operation",
			    TxDataException::COLLECTION);
  }

  // Return empty list
  return std::vector<TxRecord*>();
}
  

TxRecord* TxCollectionHandler::createPattern() const {
  return new TxRecord(*pattern_);
}


int TxCollectionHandler::getColumnForFieldName(const std::string& name) const {
  std::vector<TxField*>* patternFields = pattern_->getFields();

  for (unsigned int i=0; i < patternFields->size(); i++) {
    if ((*patternFields)[i]->getName() == name) {
      return i;
    }
  }

  throw TxDataException(name_  + ": No column with name '" + name + "' found.", 
			TxDataException::COLLECTION);
}


std::string TxCollectionHandler::getName() const {
  return name_;
}


std::string TxCollectionHandler::getKey() const {
  return key_;
}


std::string TxCollectionHandler::toString() const {
  std::string collectionString = "NAME: ";
  collectionString += name_ + "\tKEY: " + key_;
  return collectionString;
}


bool TxCollectionHandler::finishedReading() const {
  return ( readFlag_ == TxModule::NEWOP );
}


void TxCollectionHandler::restartReading() {
  readFlag_ = TxModule::NEWOP;
}


void TxCollectionHandler::purgePattern() {
  if (pattern_) {
    for (std::vector<TxField*>::iterator iter = pattern_->getFields()->begin();
	 iter != pattern_->getFields()->end();
	 iter++) {
      (*iter)->setValueFromString("");
      (*iter)->setNull(false);
      (*iter)->setIgnore(true);
    }
  }
}
