
/***************************************************************************
 *
 * $Id: StDbBufferI.h,v 1.3 1999/12/28 21:31:41 porter Exp $
 *
 * Author: Laurent Conin & Jeff Porter
 ***************************************************************************
 *
 * Description: Buffer Interface to negotiate data I/O between database
 *
 ***************************************************************************
 *
 * $Log: StDbBufferI.h,v $
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


#ifdef ST_NO_TEMPLATE_DEF_ARGS
#include <config.h>
#endif

//#ifdef SOLARIS
//# ifndef false
//typedef int bool;
//#define false 0
//#define true 1
//#endif
//#endif

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
  virtual bool     WriteArray(float   *c, int len, const char *) = 0;
  virtual bool     WriteArray(double  *c, int len, const char *) = 0;
  virtual bool     WriteArray(char **c, int len, const char *) = 0;


};



#endif






