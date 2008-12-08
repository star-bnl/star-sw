/*
 * @file TxDataException.cpp
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: TxDataException.cpp,v 1.1 2008/12/08 21:10:29 fine Exp $
 *
 * See TxDataException.h for detals.
 */

#include <string>
 
#include "TxDataException.h"

TxDataException::TxDataException( const std::string& description,
				  ObjectType object,
				  TxUCMException::Severity severity)
  : TxUCMException(severity),
    object_(object) {
  setDescription("[DATA:" + typeToString() + "]" + description);
}


TxDataException::~TxDataException()
{} //EMPTY


TxDataException::ObjectType TxDataException::getObjectType() const {
  return object_;
}
								   

std::string TxDataException::typeToString() const {
  switch (object_) {
    case STORE:
      return "STORE";
      break;
    case COLLECTION:
      return "COLLECTION";
      break;
    case RECORD:
      return "RECORD";
      break;
    case FIELD:
      return "FIELD";
      break;
    default:
      return "UNKNOWN";
  }
}
