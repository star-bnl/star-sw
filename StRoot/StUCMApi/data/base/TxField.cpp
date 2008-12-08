/*
 * TxField.cpp
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
 * @(#)cpp/api:$Id: TxField.cpp,v 1.1 2008/12/08 21:10:29 fine Exp $
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

TxField::TxField(const std::string& name, TxField::DataType type, int length)
  : type_(INVALID), 
    encodedValue_(""),
    null_(true),
    ignore_(true) {

  if (name.empty()) {
    throw TxDataException("Attempted to define field with empty name.",
			  TxDataException::FIELD,
			  TxUCMException::ERROR);
  }

  if (type == STRING)
  {
    if (length < 0)
    {
      throw TxDataException("String type specified with length < 0 in field '" 
			     + name + "'.", 
			    TxDataException::FIELD,
			    TxUCMException::ERROR);
    }  
    maxLength_ = length;
  }
  else
  {
    maxLength_ = 0;
  }

  name_ = name;
  type_ = type;
  ignore_ = false;
}


// template<class T> TxField(const std::string& name, T value, int length=0)
// in header


TxField::TxField(const std::string& name, const std::string&value,
		 TxField::DataType type, int length)
  : type_(INVALID),
    ignore_(true) {
  if (name.empty()) {
    throw TxDataException("Attempted to define field with empty name.",
			  TxDataException::FIELD,
			  TxUCMException::ERROR);
  }

  if (type == STRING)
  {
    if (length <= 0)
    {
      throw TxDataException("String type specified with length <= 0 in field '" 
			     + name + "'.", 
			    TxDataException::FIELD,
			    TxUCMException::ERROR);
    }
    maxLength_ = length;
  }
  else
  {
    maxLength_ = 0;
  }

  name_ = name;
  type_ = type;
  setValueFromString(value);
}


TxField::TxField(const TxField &f) {
  copy(f);
}


//!Deconstructor
TxField::~TxField() 
{} // EMPTY


const TxField& TxField::operator=(const TxField& f) {
  if ( this != &f ) {
    copy(f);
  }
  return *this;
}


void TxField::copy(const TxField &f) {
  name_ = f.getName();
  type_ = f.getType();
  encodedValue_ = f.getValueAsString();
  null_ = f.isNull();
  maxLength_ = f.getMaxLength();
  ignore_ = f.isIgnore();
}

std::string
TxField::getName() const {
  return name_;
}


std::string 
TxField::toString() const {
  return getName() + "::"
    + getValueAsString()
    + "::"
    + getTypeAsString();
}


void
TxField::setValueFromString(const std::string& strValue)
{
  std::string value = strValue;
  if (type_ == STRING) {
    if (value.length() > maxLength_) {
      value = strValue.substr(0,maxLength_);
    }
    null_ = false;
  }
  else if( strValue.empty())
  {
    null_ = true;
  }
  else {
    null_ = false;
  }

  encodedValue_ = value;
  ignore_ = false;
}


TxField::DataType
TxField::getType() const
{
  return type_;
}


std::string
TxField::getValueAsString() const
{
  return encodedValue_;
}


// template<class T> T TxField::getValue() in HEADER


bool
TxField::isNull() const 
{
  return null_;
}


void
TxField::setNull(bool null) 
{
  if (null) {
    encodedValue_ = "";
  }

  ignore_ = false;
  null_ = null;
}


// template<class T> void TxVarant::typeMatches(T value)


std::string
TxField::getTypeAsString() const
{
  std::string typeStr;
  
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
  
  return typeStr;
}


int
TxField::getMaxLength() const
{
  return maxLength_;
}


bool TxField::isIgnore() const {
  return ignore_;
}


void TxField::setIgnore(bool ignore) {
  ignore_ = ignore;
}
