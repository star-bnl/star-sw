/*
 * StDbFieldI.h
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
 * @(#)cpp/api:$Id: StDbFieldI.h,v 1.4 2010/03/30 20:05:36 fine Exp $
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
 
#ifndef STDBFIELD_H
#define STDBFIELD_H

#include <string>
#include <sstream>
#include <map>
#include <typeinfo>

#include "StDataException.h"
#include "FieldList.h"

/**
 * Abstraction for the field in memory.
 */
namespace TxLogging {
class Init_StDbFieldI;

class StDbFieldI {
 friend class TxLogging::Init_StDbFieldI;
 public:
  /**
   * List of types supported by UCM.  The INVALID type is provided to set
   * an error state with the data, and is explicitly defined as 0 for use
   * in conditionals (if !INVALID).  Used to abstract typeid.
   * TODO: Since values are stored as a string right now, we may need
   * to add BLOB support in some other way if/when required.
   */
  enum EDataType {
    kINVALID=0,
    kBOOL,
    kINT,
    kUINT,
    kLONG,
    kULONG,
    kDOUBLE,
    kCHAR,
    kUNIXTIME,
    kSTRING,
    kEND
  };

 public:

  /**
   * Constructor: Creates a field with the specified name and type,
   * but with an empty value.
   * @param name The name of the field.
   * @param type The field type, as a TxEType.
   * @param length The field length.  Not optional for STRING type.
   */
  StDbFieldI(const char* name, StDbFieldI::EDataType type, int length=0);

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
   * All other types will throw a StDataException and set the variant type as
   * INVALID.  Note that once a StDbFieldI has its datatype set, it cannot be changed.
   * @param name The name of the field.
   * @param value The value to encode in the variant.
   * @param length Non-optional for strings; sets the max length of the string.
   */
    // StDbFieldI(const char* name, void *value, int length=0);

  /**
   * Constructor: Creates a complete field.  Note that it may
   * not be typesafe.
   * @param name The name of the field.
   * @param type The type of the field.  Note that in this constructor the user
   *             is responsible for ensuring type safety.
   * @param value The value of the field.
   * @param length The length of the field.  Not optional for STRING type.
   */
  StDbFieldI(const char* name, const char* value,
	  StDbFieldI::EDataType type, int length=0);

  /**
   * Copy operators: Copy constructor and operator=.
   */
  StDbFieldI(const StDbFieldI& );
  const StDbFieldI& operator=(const StDbFieldI& );

  /**
   * Destructor: No-op.
   */
  virtual ~StDbFieldI();
	       
  /**
   * Returns the name of the field.
   */
  const char* getName() const;

  /**
   * Returns the field information in the form (name)::(type)::(value).
   * Included for debugging purposes.
   */
  const char* fieldAsString() const;

  /**
   * Sets the value (and type) of the data in the variant.  Setting 
   * a value implicitly unsets the IGNORE flag.
   * @param The value to encode in the variant.
   * @throws StDataException If there is a type mismatch between the type of
   *                         T and the stored type information.
   */

   void setValue(const int           &value);
   void setValue(const unsigned int  &value);
   void setValue(const long          &value);
   void setValue(const unsigned long &value);
   void setValue(const double        &value);
   void setValue(const char          &value);
  //  void setValue(const std::string   &value);
   
   int           toInt() const;
   unsigned      toUInt() const;
   long          toLong() const;
   unsigned long toULong() const;
   double        toDouble() const;
   char          toChar() const;
   // const std::string toString() const;
  /**
   * Sets the value from the provided string. Setting a value implicitly
   * unsets the IGNORE flag.
   * Note that there is NO type safety here, as the user may unwittingly produce
   * a type mismatch between the string value and the existing data type.
   * @param strValue The string to set as value.
   */
  void setValueFromString(const char* strValue);

  /**
   * Returns the type information of the field.  Useful for type
   * safety and returning a true value.
   */
  EDataType getType() const;

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
  const char*getTypeAsString() const;
  
  /**
   * Returns the value of the field in its encoded string form.
   * Note that this may not be safe in all situations: The store may
   * treat the string "123" differently from the integer 123, although
   * both have the same internal string representation.
   */
  const char*getValueAsString() const;

  /**
   * Returns true if the field represents a NULL value.
   */
  bool isNull() const;

  /**
   * Sets whether or not the field is a NULL value.  Implicitly sets the 
   * IGNORE flag to 'false' (a value has been explicitly set)
   * @param null True if the field is to be set NULL.
   */
  void setNull(bool Nil);

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
  
 protected:
   template<class T> 
   StDbFieldI(const char* name, const T &value, int length);


   template <class T>
   void setValue(const T &value);
 
   /**
   * Returns the 'real' value of the data stored in the object.  If
   * the provided type does NOT match the data type stored in the
   * field, a StDataException is thrown.
   * @throws StDataException If provided type does not match actual type.
   */
   template <class T>
   T toValue() const;

 private:
  /**
   * @internal
   * Used by copy constructor and op= to perform copying.
   */
  void copy(const StDbFieldI &f);
  /**
   * @internal
   * Confirms that the type of the argument matches the type
   * of the variant.
   * @throws StDataException Thrown if the provided type does not match
   *                         the type of the variant.
   */
  template<class T> void typeMatches() const;

 private:
  static void MakeTypeMap();
  std::string fName;
  EDataType fType;
  std::string fEncodedValue;
  size_t fMaxLength;
  bool fNull;
  bool fIgnore;
  static std::map<EDataType,std::string> fTypeMap;
  static std::map<EDataType,std::string> fTypeMapName;
  static std::map<std::string,EDataType> fTypeMapInv;
};


//---------------
// BEGIN TEMPLATE FUNCTION DEFINITIONS:
// Note that this is required by the gcc linker to properly link templated
// functions.
//---------------

//_______________________________________________________________________________
template<class T> StDbFieldI::StDbFieldI(const char* name, const T &value, int length)
  : fType(kINVALID),
    fNull(true),
    fEncodedValue(""),
    fMaxLength(length),
    fIgnore(true),
    fName(name)
{
  if (std::string(name).empty()) {
    throw StDataException("Attempted to define field with empty name.",
			  StDataException::FIELD,
			  StUCMException::ERROR);
  }
  setValue(value);
}


//_______________________________________________________________________________
template <class T> void
StDbFieldI::setValue(const T &value)
{
  typeMatches<T>();

  // Convert value to string
  std::ostringstream encodingStream;
  encodingStream << value;

  // If we have a string type..
  if ( fType == kSTRING ) {
    if ( encodingStream.str().length() > fMaxLength ) {
      std::ostringstream lenStr;
      lenStr << fMaxLength;
      
      // Conversion to std::string is safe, but potentially redundant
      throw StDataException("Invalid value for field '" + fName
			     + "': Length of string '" + encodingStream.str() 
			     + "' is longer than variant length '" 
			     + lenStr.str() + "'.",
			    StDataException::FIELD,
			    StUCMException::ERROR);
    }
  }

  fEncodedValue = encodingStream.str();
  fNull         = false;
  fIgnore       = false;
}


//_______________________________________________________________________________
template<class T> 
T StDbFieldI::toValue() const
{
  typeMatches<T>();

  std::istringstream decodingStream(fEncodedValue);
  T realValue;

  decodingStream >> realValue;
  return realValue;
}

//_______________________________________________________________________________
template<class T> void
StDbFieldI::typeMatches() const 
{
   // Massive check: If the provided type T does not match the stored
   // data type, throw an exception.
   std::map<EDataType,std::string>::const_iterator t  =  fTypeMap.find(getType());

   if (t == fTypeMap.end()) {
   throw StDataException( "Type mismatch in field '" + fName + "': Actual type " 
           + getTypeAsString() + std::string(", requested type ") 
           + typeid(T).name(),
            StDataException::FIELD,
            StUCMException::ERROR);
  }
}
}
#endif
