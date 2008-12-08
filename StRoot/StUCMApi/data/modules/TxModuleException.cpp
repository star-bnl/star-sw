/*
 * @file TxModuleException.cpp
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: TxModuleException.cpp,v 1.1 2008/12/08 21:10:32 fine Exp $
 *
 * See TxModuleException.h for detals.
 */

#include <string>
 
#include "TxModuleException.h"

TxModuleException::TxModuleException(const std::string& description,
				     TxModuleException::Operation op,
				     TxUCMException::Severity severity)
  : TxUCMException(severity),
    operation_(op)
{
  setDescription("[MODULE:" + operationToString() + "] " + description);
}


TxModuleException::~TxModuleException()
{} //EMPTY


TxModuleException::Operation TxModuleException::getOperation() const {
  return operation_;
}


std::string TxModuleException::operationToString() const {
  switch (operation_) {
    case CONNECT:
      return "CONNECT";
      break;
    case INSERT:
      return "INSERT";
      break;
    case DELETE:
      return "DELETE";
      break;
    case SELECT:
      return "SELECT";
      break;
    case DESCRIBE:
      return "DESCRIBE";
      break;
    case CREATE:
      return "CREATE";
      break;
    case DROP:
      return "DROP";
      break;
    default:
      return "UNKNOWN";
  };
}
