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
 * Copyright (c) 2007-2008 STAR Collaboration - Brookhaven National Laboratory
 *
 * @(#)cpp/api:$Id: StDbFieldI.cxx,v 1.5 2010/03/30 20:05:36 fine Exp $
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
#include <iostream>

#include "StDbFieldI.h"
using namespace std;
using namespace TxLogging;
// using namespace StDbField;

std::map<StDbFieldI::EDataType,std::string> StDbFieldI::fTypeMap;
std::map<StDbFieldI::EDataType,std::string> StDbFieldI::fTypeMapName;
std::map<std::string,StDbFieldI::EDataType> StDbFieldI::fTypeMapInv;
namespace TxLogging {
struct Init_StDbFieldI {
   Init_StDbFieldI() { StDbFieldI::MakeTypeMap();} 
};
}
namespace {
  Init_StDbFieldI a;
}
//________________________________________________________________________________
StDbFieldI::StDbFieldI(const char* name, StDbFieldI::EDataType type, int length)
  : fType(kINVALID), 
    fEncodedValue(""),
    fNull(true),
    fIgnore(true) {

  if (string(name).empty()) {
    throw StDataException("Attempted to define field with empty name.",
			  StDataException::FIELD,
			  StUCMException::ERROR);
  }

  if (type == kSTRING)
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
		 StDbFieldI::EDataType type, int length)
  : fType(kINVALID),
    fIgnore(true) 
{
  if (string(name).empty()) {
    throw StDataException("Attempted to define field with empty name.",
			  StDataException::FIELD,
			  StUCMException::ERROR);
  }

  if (type == kSTRING)
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
  if (!value) value = " ";
  setValueFromString(value);
#ifdef DEBUG  
  cout << __FUNCTION__ 
       << ":  name =" << name
	   << "; value =" << value
	   << " type =" << type
	   << "; lenght=" << length
	   << endl;
#endif	   
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
StDbFieldI::fieldAsString() const {
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
  if (fType == kSTRING) {
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
StDbFieldI::EDataType
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
  return fTypeMapName[getType()].c_str();
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
//________________________________________________________________________________
void StDbFieldI::MakeTypeMap() {
#ifdef TYPEtypeNAME
#error   TYPEtypeNAME  redefinitions
#else
#define TYPEtypeNAME(TYPENAME,datatype)                                                              \
      case k##TYPENAME: {                                                                \
      fTypeMap.insert(pair<EDataType,std::string>(k##TYPENAME,typeid(datatype).name())); \
      fTypeMapName.insert(pair<EDataType,std::string>(k##TYPENAME,#TYPENAME));        \
      fTypeMapInv.insert(pair<std::string,EDataType>(typeid(datatype).name(),k##TYPENAME));         \
      break;}
#endif
  for (int i=kBOOL;i<kEND;++i) {
    switch (i) {
       TYPEtypeNAME(BOOL,bool)
       TYPEtypeNAME(INT,int)
       TYPEtypeNAME(UINT,unsigned int)
       TYPEtypeNAME(LONG,long)
       TYPEtypeNAME(ULONG,unsigned long)
       TYPEtypeNAME(DOUBLE,double)
       TYPEtypeNAME(CHAR,char)
       TYPEtypeNAME(UNIXTIME,unsigned int)
       //TYPEtypeNAME(STRING,string)       
       default: {
         fTypeMap.insert(pair<EDataType,std::string>(kINVALID,"invalid"));
         fTypeMapName.insert(pair<EDataType,std::string>(kINVALID,"unkown"));
         fTypeMapInv.insert(pair<std::string,EDataType>("unkown",kINVALID));
         break;
      }
    }
  }
}
#ifdef DBVALUECONV_
#error DBVALUECONV_ redefinitions
#else
#define DBVALUECONV_(DATATYPE,dataname)                                         \
  void StDbFieldI::setValue(const DATATYPE &value) {setValue<DATATYPE>(value);} \
  DATATYPE  StDbFieldI::to##dataname() const { return toValue<DATATYPE>(); }
#endif
    DBVALUECONV_(char,Char)
    DBVALUECONV_(int,Int)
    DBVALUECONV_(unsigned int,UInt)
    DBVALUECONV_(long,Long)
    DBVALUECONV_(unsigned long,ULong)
    DBVALUECONV_(double,Double)
//    DBVALUECONV_(std::string,String)



