#ifndef Loch_h
#define Lock_h

#include <iostream>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h> 

using namespace std;

#include "TSystem.h"
#include "TMapFile.h"
#include "TRandom.h"



class Lock {
 private:
  static TRandom* mRandom;
 public:
  
  static int lock1();
  static int lock2();
  
  static void unlock1();
  static void unlock2();
  
  static void sleep();
  
  static int update(TMapFile* mFile);

  //  template<class T> static T* read(TMapFile* mFile, const char* name, T* t);

  static void unlock();
  
};
  
#endif





/***************************************************************************
 *
 * $Id: Lock.h,v 1.1 2009/01/23 16:10:56 jeromel Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: Lock.h,v $
 * Revision 1.1  2009/01/23 16:10:56  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.1  2007/02/27 15:23:38  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:16  laue
 * Initial Version
 *
 *
 ***************************************************************************/

