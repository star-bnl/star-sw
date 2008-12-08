/*
 * @file TxDataException.h
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: TxDataException.h,v 1.1 2008/12/08 21:10:29 fine Exp $
 *
 * TxDataException is the most generic exception class thrown by the base
 * part of the data tier (and is so named because the base part of the library
 * represents all data abstractions).  It modifies provided descriptions
 * to include additional information by prepending "[DATA:$type]", where
 * $type indicates the type of data for where the exception occured.
 */
 
#ifndef _TX_DATA_EXCEPTION_H_
#define _TX_DATA_EXCEPTION_H_

#include <string>
#include "TxUCMException.h"

/**
 * The exception class for all exceptions thrown by the 'base' component
 * of the data tier.
 */
class TxDataException : public TxUCMException {
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
  TxDataException( const std::string& description,
		   ObjectType object,
		   TxUCMException::Severity severity = TxUCMException::ERROR);

  /**
   * Destructor: No-op.
   */
  virtual ~TxDataException();


  /**
   * Retrieves the type of object where the exception occured.  Useful for
   * ignoring all DataExceptions which did not occur on a particular object
   * (i.e. catch TxDataExceptions, but ignore any which did not occur on a STORE)
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
