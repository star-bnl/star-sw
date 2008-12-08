/*
 * @file TxUCMException.cpp
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: TxUCMException.cpp,v 1.1 2008/12/08 21:10:28 fine Exp $
 *
 * See TxUCMException.h for detals.
 */

#include <string>
 
#include "TxUCMException.h"

TxUCMException::TxUCMException(const std::string& description,
			       Severity severity)
  : description_(description),
    severity_(severity)
{} // EMPTY


TxUCMException::TxUCMException(Severity severity)
  : severity_(severity)
{} // EMPTY


TxUCMException::~TxUCMException()
{} //EMPTY


std::string
TxUCMException::getDescription() const
{
  return description_;
}


TxUCMException::Severity
TxUCMException::getSeverity() const
{
  return severity_;
}


void
TxUCMException::setDescription(const std::string& desc)
{
  description_ = desc;
}
