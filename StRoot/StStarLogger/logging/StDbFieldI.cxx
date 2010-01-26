/*
 * StDbFieldI.cpp
 *
 * Created on Aug 20, 2007
 *
 * Author: Zhengqiang Liang (Wayne State University)
 *            Valeri Fine (Brookhaven National Laboratory)
 *            Jerome Lauret (Brookhaven National Laboratory)
 *            
 *
 * This file is part of the UCM project funded under an SBIR
 * Copyright (c) 2007-2008 STAR Collaboration - Brookhaven National Laboratory
 *
 * @(#)cpp/api:$Id: StDbFieldI.cxx,v 1.1 2010/01/26 23:43:45 fine Exp $
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

#include "StDbFieldI.h"
using namespace std;

//________________________________________________________________________________
StDbFieldI::StDbFieldI(const char* name, StDbFieldI::DataType type, int length)
  : fType(INVALID), 
    fEncodedValue(""),
    fNull(true),
    fIgnore(true) {

  if (string(name).empty()) {
    throw StDataException("Attempted to define field with empty name.",
			  StDataException::FIELD,
			  StUCMException::ERROR);
  }

  if (type == STRING)
  {
    if (length < 0)
    {
      throw StDataException("String type specified with length < 0 in field '" 
			     + string(name) + "'.", 
			    StDataException::FIELD,
			    StUCMException::ERROR);
    }  
    fMaxLength = length;
  }
  else
  {
    fMaxLength = 0;
  }

  fName = name;
  fType = type;
  fIgnore = false;
}


// template<class T> StDbFieldI(const char* name, T value, int length=0)
// in header


//________________________________________________________________________________
StDbFieldI::StDbFieldI(const char* name, const char*value,
		 StDbFieldI::DataType type, int length)
  : fType(INVALID),
    fIgnore(true) {
  if (string(name).empty()) {
    throw StDataException("Attempted to define field with empty name.",
			  StDataException::FIELD,
			  StUCMException::ERROR);
  }

  if (type == STRING)
  {
    if (length <= 0)
    {
      throw StDataException("String type specified with length <= 0 in field '" 
			     + string(name) + "'.", 
			    StDataException::FIELD,
			    StUCMException::ERROR);
    }
    fMaxLength = length;
  }
  else
  {
    fMaxLength = 0;
  }

  fName = name;
  fType = type;
  setValueFromString(value);
}


//________________________________________________________________________________
StDbFieldI::StDbFieldI(const StDbFieldI &f) {
  copy(f);
}


//!Deconstructor
//________________________________________________________________________________
StDbFieldI::~StDbFieldI() 
{} // EMPTY


//________________________________________________________________________________
const StDbFieldI& StDbFieldI::operator=(const StDbFieldI& f) {
  if ( this != &f ) {
    copy(f);
  }
  return *this;
}


//________________________________________________________________________________
void StDbFieldI::copy(const StDbFieldI &f) {
  fName = f.getName();
  fType = f.getType();
  fEncodedValue = f.getValueAsString();
  fNull = f.isNull();
  fMaxLength = f.getMaxLength();
  fIgnore = f.isIgnore();
}

//________________________________________________________________________________
const char *
StDbFieldI::getName() const {
  return fName.c_str();
}


//________________________________________________________________________________
const char *
StDbFieldI::toString() const {
  static  string tostring;
   tostring = string(getName()) + "::"
    + getValueAsString()
    + "::"
    + getTypeAsString();
    return tostring.c_str();
}


//________________________________________________________________________________
void
StDbFieldI::setValueFromString(const char* strValue)
{
  std::string value = strValue;
  if (fType == STRING) {
    if (value.length() > fMaxLength) {
      value = string(strValue).substr(0,fMaxLength);
    }
    fNull = false;
  }
  else  {
    fNull =string(strValue).empty();
  }

  fEncodedValue = value;
  fIgnore      = false;
}


//________________________________________________________________________________
StDbFieldI::DataType
StDbFieldI::getType() const
{
  return fType;
}


//________________________________________________________________________________
const char *
StDbFieldI::getValueAsString() const
{
  return fEncodedValue.c_str();
}


// template<class T> T StDbFieldI::getValue() in HEADER


//________________________________________________________________________________
bool
StDbFieldI::isNull() const 
{
  return fNull;
}


//________________________________________________________________________________
void
StDbFieldI::setNull(bool null) 
{
  if (null) {
    fEncodedValue = "";
  }

  fIgnore = false;
  fNull = null;
}


//________________________________________________________________________________
const char *
StDbFieldI::getTypeAsString() const
{
  static std::string typeStr;
  
  switch ( getType() )
  {
    case BOOL:
      typeStr = "BOOL";
      break;
    case INT:
      typeStr = "INT";
      break;
    case UINT:
      typeStr = "UINT";
      break;
    case LONG:
      typeStr = "LONG";
      break;
    case ULONG:
      typeStr = "ULONG";
      break;
    case DOUBLE:
      typeStr = "DOUBLE";
      break;
    case CHAR:
      typeStr = "CHAR";
      break;
    case STRING:
      typeStr = "STRING";
      break;
    default:
      typeStr = "INVALID";
  }
  
  return typeStr.c_str();
}


//________________________________________________________________________________
int
StDbFieldI::getMaxLength() const
{
  return fMaxLength;
}


//________________________________________________________________________________
bool StDbFieldI::isIgnore() const {
  return fIgnore;
}


//________________________________________________________________________________
void StDbFieldI::setIgnore(bool ignore) {
  fIgnore = ignore;
}
