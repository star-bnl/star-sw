
/***************************************************************************
 *
 * $Id: StDbBufferI.h,v 1.7 2001/10/24 04:05:20 porter Exp $
 *
 * Author: Laurent Conin & Jeff Porter
 ***************************************************************************
 *
 * Description: Buffer Interface to negotiate data I/O between database
 *
 ***************************************************************************
 *
 * $Log: StDbBufferI.h,v $
 * Revision 1.7  2001/10/24 04:05:20  porter
 * added long long type to I/O and got rid of obsolete dataIndex table
 *
 * Revision 1.6  2001/01/22 18:37:51  porter
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
 * Revision 1.5  2000/03/28 17:03:18  porter
 * Several upgrades:
 * 1. configuration by timestamp for Conditions
 * 2. query by whereClause made more systematic
 * 3. conflict between db-stored comments & number lists resolved
 * 4. ensure endtime is correct for certain query falures
 * 5. dbstl.h->handles ObjectSpace & RogueWave difference (Online vs Offline)
 *
 * Revision 1.4  1999/12/29 13:49:34  porter
 * fix for Solaris-CC4.2 within StRoot make (cons)...
 * replaced #include <config.h> with #include <ospace/config.h>
 *
 * Revision 1.3  1999/12/28 21:31:41  porter
 * added 'using std::vector' and 'using std::list' for Solaris CC5 compilation.
 * Also fixed some warnings arising from the CC5 compiles
 *
 * Revision 1.2  1999/09/30 02:06:01  porter
 * add StDbTime to better handle timestamps, modify SQL content (mysqlAccessor)
 * allow multiple rows (StDbTable), & Added the comment sections at top of
 * each header and src file
 *
 **************************************************************************/
#ifndef STDBBUFFERI_H
#define STDBBUFFERI_H

class StDbBufferI  { 

public: 
 
  virtual ~StDbBufferI(){};
  virtual void SetClientMode() = 0; 
  virtual void SetStorageMode() = 0;
  virtual bool IsClientMode() = 0;
  virtual bool IsStorageMode() = 0;
   
  virtual bool  ReadScalar(char   &c, const char *aName)  = 0;
  virtual bool  ReadScalar(unsigned char  &c, const char *)  = 0; 
  virtual bool  ReadScalar(short  &h, const char *)  = 0;
  virtual bool  ReadScalar(unsigned short &h, const char *)  = 0;
  virtual bool  ReadScalar(int    &i, const char *)  = 0;
  virtual bool  ReadScalar(unsigned int   &i, const char *)  = 0;
  virtual bool  ReadScalar(long   &l, const char *)  = 0;
  virtual bool  ReadScalar(unsigned long  &l, const char *)  = 0;
  virtual bool  ReadScalar(long long  &l, const char *)  = 0;
  virtual bool  ReadScalar(float  &f, const char *)  = 0;
  virtual bool  ReadScalar(double &d, const char *)  = 0;
  virtual bool  ReadScalar(char   *&c, const char *)  = 0; 
  
  virtual bool     WriteScalar(const char   c, const char *)  = 0;
  virtual bool     WriteScalar(const unsigned char  c, const char *)  = 0;
  virtual bool     WriteScalar(const short  h, const char *)  = 0;
  virtual bool     WriteScalar(const unsigned short h, const char *)  = 0;
  virtual bool     WriteScalar(const int    i, const char *)  = 0;
  virtual bool     WriteScalar(const unsigned int   i, const char *)  = 0;
  virtual bool     WriteScalar(const long   l, const char *)  = 0;
  virtual bool     WriteScalar(const unsigned long  l, const char *)  = 0;
  virtual bool     WriteScalar(const long long  l, const char *)  = 0;
  virtual bool     WriteScalar(const float  f, const char *)  = 0;
  virtual bool     WriteScalar(const double d, const char *)  = 0;
  virtual bool     WriteScalar(const char  *c, const char *) = 0;

  virtual bool     ReadArray(char    *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(unsigned char  *&c, int &len, const char *) = 0;
  virtual bool     ReadArray( short  *&c, int &len, const char *) = 0;
  virtual bool     ReadArray( unsigned short  *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(int  *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(unsigned int   *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(long  *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(unsigned long  *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(long long  *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(float   *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(double  *&c, int &len, const char *) = 0;
  virtual bool     ReadArray(char  **&c, int &len, const char *) = 0;

  virtual bool     WriteArray(char    *c, int len, const char *) = 0;
  virtual bool     WriteArray(unsigned char   *c, int len, const char *) = 0;
  virtual bool     WriteArray(short   *c, int len, const char *) = 0;
  virtual bool     WriteArray(unsigned short  *c, int len, const char *) = 0;
  virtual bool     WriteArray(int     *c, int len, const char *) = 0;
  virtual bool     WriteArray(unsigned int    *c, int len, const char *) = 0;
  virtual bool     WriteArray(long    *c, int len, const char *) = 0;
  virtual bool     WriteArray(unsigned long   *c, int len, const char *) = 0;
  virtual bool     WriteArray(long long   *c, int len, const char *) = 0;
  virtual bool     WriteArray(float   *c, int len, const char *) = 0;
  virtual bool     WriteArray(double  *c, int len, const char *) = 0;
  virtual bool     WriteArray(char **c, int len, const char *) = 0;
};
#endif
