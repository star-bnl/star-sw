/*
 * @file StDataException.h
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: StDataException.h,v 1.2 2010/03/30 20:05:36 fine Exp $
 *
 * StDataException is the most generic exception class thrown by the base
 * part of the data tier (and is so named because the base part of the library
 * represents all data abstractions).  It modifies provided descriptions
 * to include additional information by prepending "[DATA:$type]", where
 * $type indicates the type of data for where the exception occured.
 */
 
#ifndef _TX_DATA_EXCEPTION_H_
#define _TX_DATA_EXCEPTION_H_

#include <string>
#include "StUCMException.h"

/**
 * The exception class for all exceptions thrown by the 'base' component
 * of the data tier.
 */
class StDataException : public StUCMException {
 public:
  /**
   * Represents the type of data object where the exception
   * occured.
   */
  enum ObjectType {
    STORE,
    COLLECTION,
    RECORD,
    FIELD
  };

 public:
  /**
   * Constructor: Provide description and exception severity.
   * @param descrption The error message for the exception.
   * @param object The type of object on which the exception occured.
   * @param severity The severity of the exception.  Optional, defaults to ERROR.
   */
  StDataException( const std::string& description,
		   ObjectType object,
		   StUCMException::Severity severity = StUCMException::ERROR);

  /**
   * Destructor: No-op.
   */
  virtual ~StDataException();


  /**
   * Retrieves the type of object where the exception occured.  Useful for
   * ignoring all DataExceptions which did not occur on a particular object
   * (i.e. catch StDataExceptions, but ignore any which did not occur on a STORE)
   */
  ObjectType getObjectType() const;

 private:
  /**
   * @internal
   * Returns the exception's object_ field in a string format.
   * Used to modify description.
   */
  std::string typeToString() const;

 private:
  ObjectType object_;
};

#endif
