/*
 * TxField.h
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
 * @(#)cpp/api:$Id: TxField.h,v 1.1 2008/12/08 21:10:29 fine Exp $
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
 
#ifndef TXFIELD_H
#define TXFIELD_H

#include <string>
#include <sstream>
#include <typeinfo>

#include "TxDataException.h"

/**
 * Abstraction for the field in memory.
 */
class TxField {
 public:
  /**
   * List of types supported by UCM.  The INVALID type is provided to set
   * an error state with the data, and is explicitly defined as 0 for use
   * in conditionals (if !INVALID).  Used to abstract typeid.
   * TODO: Since values are stored as a string right now, we may need
   * to add BLOB support in some other way if/when required.
   */
  enum DataType {
    INVALID=0,
    BOOL,
    INT,
    UINT,
    LONG,
    ULONG,
    DOUBLE,
    CHAR,
    STRING
  };

 public:

  /**
   * Constructor: Creates a field with the specified name and type,
   * but with an empty value.
   * @param name The name of the field.
   * @param type The field type, as a TxEType.
   * @param length The field length.  Not optional for STRING type.
   */
  TxField(const std::string& name, TxField::DataType type, int length=0);

    /**
   * Constructor: The constructor takes an argument of type T, and
   * converts it to a string.  Supported types are:
   * ----
   * bool
   * all signed integer types (stored as long)
   * all unsigned integer types (stored as ulong)
   * (signed) float types (stored as double)
   * char
   * std::string
   * ----
   * All other types will throw a TxDataException and set the variant type as
   * INVALID.  Note that once a TxField has its datatype set, it cannot be changed.
   * @param name The name of the field.
   * @param value The value to encode in the variant.
   * @param length Non-optional for strings; sets the max length of the string.
   */
  template<class T> TxField(const std::string& name, T value, int length=0);

  /**
   * Constructor: Creates a complete field.  Note that it may
   * not be typesafe.
   * @param name The name of the field.
   * @param type The type of the field.  Note that in this constructor the user
   *             is responsible for ensuring type safety.
   * @param value The value of the field.
   * @param length The length of the field.  Not optional for STRING type.
   */
  TxField(const std::string& name, const std::string& value,
	  TxField::DataType type, int length=0);

  /**
   * Copy operators: Copy constructor and operator=.
   */
  TxField(const TxField& f);
  const TxField& operator=(const TxField& f);

  /**
   * Destructor: No-op.
   */
  ~TxField();
	       
  /**
   * Returns the name of the field.
   */
  std::string getName() const;

  /**
   * Returns the field information in the form (name)::(type)::(value).
   * Included for debugging purposes.
   */
  std::string toString() const;

  /**
   * Sets the value (and type) of the data in the variant.  Setting 
   * a value implicitly unsets the IGNORE flag.
   * @param The value to encode in the variant.
   * @throws TxDataException If there is a type mismatch between the type of
   *                         T and the stored type information.
   */
  template<class T> void setValue(T value);

  /**
   * Sets the value from the provided string. Setting a value implicitly
   * unsets the IGNORE flag.
   * Note that there is NO type safety here, as the user may unwittingly produce
   * a type mismatch between the string value and the existing data type.
   * @param strValue The string to set as value.
   */
  void setValueFromString(const std::string& strValue);

  /**
   * Returns the type information of the field.  Useful for type
   * safety and returning a true value.
   */
  DataType getType() const;

  /**
   * Returns the max length of the field for those of type STRING.
   * @return For string types, returns the maximum length of the
   *         string.  For non-string types, returns 0.
   */
  int getMaxLength() const;

  /**
   * Returns the type information of the field as a human-readable
   * string.
   */
  std::string getTypeAsString() const;
  
  /**
   * Returns the value of the field in its encoded string form.
   * Note that this may not be safe in all situations: The store may
   * treat the string "123" differently from the integer 123, although
   * both have the same internal string representation.
   */
  std::string getValueAsString() const;

  /**
   * Returns the 'real' value of the data stored in the object.  If
   * the provided type does NOT match the data type stored in the
   * field, a TxDataException is thrown.
   * @throws TxDataException If provided type does not match actual type.
   */
  template<class T> T getValue() const;

  /**
   * Returns true if the field represents a NULL value.
   */
  bool isNull() const;

  /**
   * Sets whether or not the field is a NULL value.  Implicitly sets the 
   * IGNORE flag to 'false' (a value has been explicitly set)
   * @param null True if the field is to be set NULL.
   */
  void setNull(bool null);

  /**
   * Sets whether or not the field should be ignored when
   * performing execute operations.  Alternative to TxRecord's
   * 'removeField'.
   * TODO: Move to TxRecord?
   * @param ignore Whether or not to ignore the field in operations.
   */
  void setIgnore(bool ignore);

  /**
   * Returns true if the field will be ignored when performing
   * execute operations.
   */
  bool isIgnore() const;

 private:
  /**
   * @internal
   * Used by copy constructor and op= to perform copying.
   */
  void copy(const TxField &f);

  /**
   * @internal
   * Confirms that the type of the argument matches the type
   * of the variant.
   * @throws TxDataException Thrown if the provided type does not match
   *                         the type of the variant.
   */
  template<class T> void typeMatches() const;

 private:
  std::string name_;
  DataType type_;
  std::string encodedValue_;
  int maxLength_;
  bool null_;
  bool ignore_;
};



//---------------
// BEGIN TEMPLATE FUNCTION DEFINITIONS:
// Note that this is required by the gcc linker to properly link templated
// functions.
//---------------

template<class T> TxField::TxField(const std::string& name, T value, int length)
  : type_(INVALID),
    null_(true),
    encodedValue_(""),
    ignore_(true)
{
  if (name.empty()) {
    throw TxDataException("Attempted to define field with empty name.",
			  TxDataException::FIELD,
			  TxUCMException::ERROR);
  }

  // Massive if/else, to set the type from typeid.
  if ( typeid(value) == typeid(bool) ) 
  {
    type_ = BOOL;
  } 
  else if ( typeid(T) == typeid(short) ||
	    typeid(T) == typeid(int) ) 
  {
    type_ = INT;
  }
  else if ( typeid(T) == typeid(unsigned short) ||
	    typeid(T) == typeid(unsigned int) )
  {
    type_ = UINT;
  }
  else if ( typeid(T) == typeid(long) )
  {
    type_ = LONG;
  }
  else if ( typeid(T) == typeid(unsigned long) )
  {
    type_ = ULONG;
  }
  else if ( typeid(T) == typeid(float) ||
	    typeid(T) == typeid(double) )
  {
    type_ = DOUBLE;
  }
  else if ( typeid(T) == typeid(char) )
  {
    type_ = CHAR;
  }
  else if ( typeid(T) == typeid(std::string) )
  {
    // If we already have a string, don't bother doing
    // a conversion - but throw an exception if length=0.
    if (length <= 0)
    {
      throw TxDataException("String type specified with length <= 0 in field '"
 			     + name + "'.",
			    TxDataException::FIELD,
			    TxUCMException::ERROR);;
    }
    maxLength_ = length;
    type_ = STRING;
    encodedValue_ = value;
    return;
  }
  else
  {
    // Invalid type
    type_ = INVALID;
    throw TxDataException( "Invalid type provided for field '" + name + "'.",
			   TxDataException::FIELD,
			   TxUCMException::ERROR );
  }

  name_ = name;
  ignore_ = false;

  // Existing value is guaranteed to be flushed.
  std::ostringstream encodingStream;

  // Convert value to string
  encodingStream << value;
  encodedValue_ = encodingStream.str();
  null_ = false;
  maxLength_ = 0;
}


template <class T> void
TxField::setValue(T value)
{
  typeMatches<T>();

  // Convert value to string
  std::ostringstream encodingStream;
  encodingStream << value;

  // If we have a string type..
  if ( type_ == STRING ) {
    if ( encodingStream.str().length() > maxLength_ ) {
      std::ostringstream lenStr;
      lenStr << maxLength_;
      
      // Conversion to std::string is safe, but potentially redundant
      throw TxDataException("Invalid value for field '" + name_ 
			     + "': Length of string '" + encodingStream.str() 
			     + "' is longer than variant length '" 
			     + lenStr.str() + "'.",
			    TxDataException::FIELD,
			    TxUCMException::ERROR);
    }
  }

  encodedValue_ = encodingStream.str();
  null_ = false;
  ignore_ = false;
}


template<class T> T
TxField::getValue() const
{
  T dummy;
  typeMatches<T>();
  
  std::istringstream decodingStream(encodedValue_);
  T realValue;

  decodingStream >> realValue;
  return realValue;
}


template<class T> void
TxField::typeMatches() const 
{
  // Massive check: If the provided type T does not match the stored
  // data type, throw an exception.
  
  if ( getType() == INVALID ||
       ( typeid(T) == typeid(bool) &&
	 getType() != BOOL ) ||
       ( (typeid(T) == typeid(short) ||
	  typeid(T) == typeid(int)) &&
	 getType() != INT ) ||
       ( (typeid(T) == typeid(unsigned short) ||
	  typeid(T) == typeid(unsigned int)) &&
	 getType() != UINT ) ||
       ( typeid(T) == typeid(long) &&
	 getType() != LONG ) ||
       ( typeid(T) == typeid(unsigned long) &&
	 getType() != ULONG ) ||
       ( (typeid(T) == typeid(float) ||
	  typeid(T) == typeid(double)) &&
	 getType() != DOUBLE ) ||
       ( typeid(T) == typeid(char) &&
	 getType() != CHAR ) ||
       ( (typeid(T) == typeid(std::string) ||
	  typeid(T) == typeid(const char*) ||
	  typeid(T) == typeid(char*)) &&
	 getType() != STRING ) )
  {
    throw TxDataException( "Type mismatch in field '" + name_ + "': Actual type " 
			    + getTypeAsString() + std::string(", requested type ") 
			    + typeid(T).name(),
			   TxDataException::FIELD,
			   TxUCMException::ERROR);
  }
}


#endif


    
