/***************************************************************************
 *
 * $Id: StDbMessenger.hh,v 1.2 2003/09/02 17:57:49 perev Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Implementation of StDbMessService used within StDbLib
 *               in stand-alone mode
 *
 ***************************************************************************
 *
 * $Log: StDbMessenger.hh,v $
 * Revision 1.2  2003/09/02 17:57:49  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  2001/01/22 18:37:57  porter
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
#ifndef __StDbMessenger_HH
#define __StDbMessenger_HH

#include "StDbMessService.hh"
#include <Stiostream.h>

class StDbMessenger : public StDbMessService {

public:

ostream& mos;

   StDbMessenger():mos(cout){};
   StDbMessenger(ostream& os):mos(os){};
   StDbMessenger(StDbMessenger& m): mos(m.mos) { mdbLevel=m.getMessLevel(); }

   virtual ~StDbMessenger(){};
 
   virtual void printMessage(const char* message,
                            StDbMessLevel dbLevel,
                            int lineNumber,
                            const char* className, 
                            const char* methodName);

   virtual void printMessage(const char* message,
                            const char* levelString,
                            int lineNumber,
                            const char* className, 
                            const char* methodName) ;

};

#endif
