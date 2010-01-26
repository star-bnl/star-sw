/*
 * @file StUCMException.cpp
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: StUCMException.cxx,v 1.1 2010/01/26 23:43:45 fine Exp $
 *
 * See StUCMException.h for detals.
 */

#include <string>
 
#include "StUCMException.h"

StUCMException::StUCMException(const std::string& description,
			       Severity severity)
  : description_(description),
    severity_(severity)
{} // EMPTY


StUCMException::StUCMException(Severity severity)
  : severity_(severity)
{} // EMPTY


StUCMException::~StUCMException()
{} //EMPTY


std::string
StUCMException::getDescription() const
{
  return description_;
}


StUCMException::Severity
StUCMException::getSeverity() const
{
  return severity_;
}


void
StUCMException::setDescription(const std::string& desc)
{
  description_ = desc;
}
