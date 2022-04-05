/***************************************************************************
 *   
 * $Id: StDbLogger.hh,v 1.2 2016/05/24 20:26:48 dmitry Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Records elapsed times for DBI & query calls
 *
 ***************************************************************************
 *
 * $Log: StDbLogger.hh,v $
 * Revision 1.2  2016/05/24 20:26:48  dmitry
 * coverity - unreachable delete loop suppression
 *
 * Revision 1.1  2001/01/22 18:37:55  porter
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
 **************************************************************************/
#ifndef STDBLOGGER_HH
#define STDBLOGGER_HH

#include <sys/time.h>

class StDbLogger {

protected:

  double mt0;
  double mtotalTimes;
  int    mnTotal;    //! calls to clock

public:
  
  StDbLogger(): mt0(0), mtotalTimes(0), mnTotal(0) {};
  ~StDbLogger(){};

  void   start();
  double end();

  double getTotalTimes();
  int    getNumCalls();
  double wallTime();

};

inline void StDbLogger::start(){mt0=wallTime();}
inline double StDbLogger::end()  {
  double elapsedTime=wallTime()-mt0;
  mtotalTimes+=elapsedTime;
  mnTotal++;
  return elapsedTime;
}
inline double StDbLogger::getTotalTimes() { return mtotalTimes; };
inline int    StDbLogger::getNumCalls() { return mnTotal; } 
inline double StDbLogger::wallTime(){
    struct timeval Tp;
    gettimeofday( &Tp, (struct timezone *) 0);
// seconds + microseconds/1000000:  
    return Tp.tv_sec + Tp.tv_usec/1000000.0;
};

#endif
