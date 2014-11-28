/***************************************************************************
 *
 * $Id: StDbMessService.hh,v 1.2 2003/12/16 01:30:32 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Simple interface to allow using StMessage without breaking
 *               StDbLib's stand-alone capability
 *
 ***************************************************************************
 *
 * $Log: StDbMessService.hh,v $
 * Revision 1.2  2003/12/16 01:30:32  porter
 * additional fixes for change from ostrstream to ostringstream that were not exposed until
 * running in online
 *
 * Revision 1.1  2001/01/22 18:37:56  porter
 * Update of code needed in next year running. This update has little
 * effect on the interface (only 1 method has been changed in the interface).
 * Code also preserves backwards compatibility so that old versions of
 * StDbLib can read new table structures.
 *  -Important features:
 *    a. more efficient low-level table structure (see StDbSql.cc)
 *    b. more flexible indexing for new systems (see StDbElememtIndex.cc)
 *    c. environment variable override KEYS for each database
 *    d. StMessage support & clock-time logging diagnostics
 *  -Cosmetic features
 *    e. hid stl behind interfaces (see new *Impl.* files) to again allow rootcint access
 *    f. removed codes that have been obsolete for awhile (e.g. db factories)
 *       & renamed some classes for clarity (e.g. tableQuery became StDataBaseI
 *       and mysqlAccessor became StDbSql)
 *
 *
 **************************************************************************/
#ifndef __STDBMESSSERVICE_HH
#define __STDBMESSSERVICE_HH


// simple debug, warn, connection, & error  
enum StDbMessLevel { dbMDebug=0, dbMWarn, dbMConnect, dbMErr, dbMFatal }; 

class StDbMessService {

protected:

  StDbMessLevel mdbLevel;

public:

  StDbMessService(): mdbLevel(dbMWarn) {};
  virtual ~StDbMessService(){};

  void          setMessLevel(StDbMessLevel dbLevel);
  StDbMessLevel getMessLevel();

  virtual void printMessage(const char* message,
                            StDbMessLevel dbLevel,
                            int lineNumber,
                            const char* className, 
                            const char* methodName) = 0;

  virtual void printMessage(const char* message,
                            const char* levelString,
                            int lineNumber,
                            const char* className, 
                            const char* methodName)  = 0;
};

inline void StDbMessService::setMessLevel(StDbMessLevel lev){ mdbLevel=lev;  }
inline StDbMessLevel StDbMessService::getMessLevel()        {return mdbLevel;}

#endif
