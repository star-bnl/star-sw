#include "Lock.h"
#include "EvpUtil.h"

TRandom* Lock::mRandom = new TRandom(123);


int Lock::lock1() { 
  int iret = 0<open("/locker1",O_EXCL|O_CREAT|O_RDWR,S_IRWXU);
  return iret;
}

int Lock::lock2() {
  int iret = 0<open("/locker2",O_EXCL|O_CREAT|O_RDWR,S_IRWXU);
  return iret;  
}

void Lock::unlock1() { 
  unlink("/locker1"); 
}

void Lock::unlock2() { 
  unlink("/locker2"); 
}

void Lock::sleep() {
  gSystem->Sleep( mRandom->Integer(100)+50 );
}

int Lock::update(TMapFile* mFile) {
  //printf("TMapFile 0x%x\n",mFile);
  //fflush(stdout);
 
  mFile->Update();

  //printf("nope....TMapFile 0x%x\n",mFile);
  //fflush(stdout);
 

  return 0;
}

/*
template<class T>
T* read(TMapFile* mFile, const char* name, T* t=0) {
  bool done = false;
  while ( !lock1() && !done) { 
    sleep();
  }
  if ( lock2() ) {
    t = (T*) mFile->Get("name", t);
    done = true;
    unlock2();
  }
  unlock1();
  return t;
}
*/

void Lock::unlock() {
  unlock1();
  unlock2();

}





/***************************************************************************
 *
 * $Id: Lock.cxx,v 1.1 2009/01/23 16:11:05 jeromel Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: Lock.cxx,v $
 * Revision 1.1  2009/01/23 16:11:05  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.4  2008/12/09 19:10:28  fine
 * replace the mmap API with the regular I/O API to simplify debugging
 *
 * Revision 1.3  2007/05/30 13:13:54  jml
 * blah
 *
 * Revision 1.2  2007/05/25 14:53:44  jml
 * blah
 *
 * Revision 1.1  2006/10/04 20:31:16  laue
 * Initial Version
 *
 *
 ***************************************************************************/

