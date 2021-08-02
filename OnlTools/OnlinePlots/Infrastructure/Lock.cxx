#include "Lock.h"
#include "EvpUtil.h"

TRandom* Lock::mRandom = new TRandom(123);



int Lock::update(TMapFile* mFile) {
  //printf("TMapFile 0x%x\n",mFile);
  //fflush(stdout);
 
  mFile->Update();

  //printf("nope....TMapFile 0x%x\n",mFile);
  //fflush(stdout);
 

  return 0;
}


/***************************************************************************
 *
 * $Id: Lock.cxx,v 1.2 2009/07/06 22:46:49 fine Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: Lock.cxx,v $
 * Revision 1.2  2009/07/06 22:46:49  fine
 * remove the redundant lock methods
 *
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

