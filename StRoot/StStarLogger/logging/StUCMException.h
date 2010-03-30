/*
 * @file StUCMException.h
 * @author Stephen Tramer
 *
 * @(#)cpp/api:$Id: StUCMException.h,v 1.2 2010/03/30 20:05:36 fine Exp $
 *
 * StUCMException is the generic exception thrown by UCM; it serves
 * as the base class for the more specific TxDataException and TxModuleException
 * classes.  
 */
 
#ifndef _ST_UCM_EXCEPTION_H_
#define _ST_UCM_EXCEPTION_H_

#include <string>

/**
 * The base class for all exceptions thrown by the UCM library.
 */
class StUCMException {
 public:
  /**
   * Represents the available levels of exception severity.
   */
  enum Severity
    {
      WARN,
      ERROR,
      FATAL
    };

 public:
  /**
   * Constructor: Provide description of the error, and optionally a severity.
   * @param descrption The error message for the exception.
   * @param severity The severity of the exception.  Optional, defaults to ERROR.
   */
  StUCMException(const std::string& description, Severity severity = ERROR);

  /**
   * Destructor: No-op
   */
  virtual ~StUCMException();

  /**
   * Retrieve the error message.
   */
  virtual std::string getDescription() const;

  /**
   * Retrieve the exception severity.
   */
  virtual Severity getSeverity() const;

 protected:
  /**
   * @internal
   * Constructor which does not set a description, and optionally sets
   * a severity level.  Used by subclasses which modify provided descriptions
   * with additional information.
   * @param severity The severity of the exception.  Optional, defaults to ERROR.
   */
  StUCMException(Severity severity = ERROR);

  /**
   * @internal
   * Sets the description of the exception.
   * @param description The description for the exception.
   */
  void setDescription(const std::string& description);

 private:
  std::string description_;
  Severity severity_;
};

#endif
