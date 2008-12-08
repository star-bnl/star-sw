/*
 * @file TxModuleException.h
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: TxModuleException.h,v 1.1 2008/12/08 21:10:32 fine Exp $
 *
 * TxModuleException is the most generic exception class thrown by the module
 * part of the data tier.  It modifies provided descriptions to include
 * additional data in the form of '[MODULE:$op]' where $op represents the
 * operation that was being performed when the exception occured.
 */
 
#ifndef _TX_MODULE_EXCEPTION_H_
#define _TX_MODULE_EXCEPTION_H_

#include <string>

#include "TxUCMException.h"

/**
 * This class is the most generic exception class in the module library.
 */
class TxModuleException : public TxUCMException {
 public:
  enum Operation {
    CONNECT,
    INSERT,
    DELETE,
    SELECT,
    UPDATE,
    DESCRIBE,
    CREATE,
    DROP,
    MISC
  };

 public:
  /**
   * Constructor: Provide description, severity, and operation where
   * the exception occurred.
   * @param descrption The error message for the exception.
   * @param op The operation being performed when the error occured.
   * @param severity The severity of the exception.  Defaults to ERROR.
   */
  TxModuleException(const std::string& description,
		    Operation op,
		    TxUCMException::Severity severity = TxUCMException::ERROR);

  /**
   * Destructor: No-op
   */
  virtual ~TxModuleException();

  
  /**
   * Returns the type of operation that was being performed when the
   * exception occurred.  Useful for conditionally handling exceptions
   * from the modules (i.e. only care about CONNECT failures).
   */
  Operation getOperation() const;

 private:
  /**
   * @internal
   * Converts an operation into a usable string.
   */
  std::string operationToString() const;

 private:
  Operation operation_;
};

#endif
