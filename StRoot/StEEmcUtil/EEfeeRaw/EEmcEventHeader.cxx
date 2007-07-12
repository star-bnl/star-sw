// $Id: EEmcEventHeader.cxx,v 1.11 2007/07/12 19:30:13 fisyak Exp $ 

#include "EEmcEventHeader.h"
#include <string.h>

//#include <Stiostream.h>

ClassImp(EEmcEventHeader)

  


//--------------------------------------------------
//
//--------------------------------------------------
EEmcEventHeader ::  EEmcEventHeader() {
  mComment=NULL;
  
  clear();
}

//--------------------------------------------------
//
//--------------------------------------------------
EEmcEventHeader ::  ~EEmcEventHeader() 
{ 
  if(mComment) delete [] mComment;
}

//--------------------------------------------------
void EEmcEventHeader :: setComment(const char* s) {
  mCommentLen = (strlen(s)/8+1)*8; //make it 8 byte aligned
  //mCommentLen = (mCommentLen<MaxCommentLen) ? mCommentLen : MaxCommentLen;
  if(mComment) delete [] mComment;
  mComment = new char[mCommentLen];
  strncpy(mComment,s,mCommentLen);
}


//--------------------------------------------------
//
//--------------------------------------------------
void EEmcEventHeader :: clear() {
  mTimeStamp       = -1;
  mProcessingTime  = -1;
  mCommentLen = 0;
  mStatus     = 0;
  mRunNumber  = 0;
  if(mComment) delete [] mComment;
}



//--------------------------------------------------
//
//--------------------------------------------------
void EEmcEventHeader :: print(FILE *fd) const{
  fprintf(fd,"EEmcEventHeader:\n");
  fprintf(fd,"\trun number   : %-6d \n",mRunNumber);
  fprintf(fd,"\tevent number : %-6d (0x%06x)\n",mEventNumber,mEventNumber);
  fprintf(fd,"\ttoken        : %-6d (0x%03x)\n",mToken      ,mToken);
  fprintf(fd,"\ttime stamp   : %ld / %s",mTimeStamp,
		  	ctime((const time_t *)&mTimeStamp));
  fprintf(fd,"\tproc. time   : %ld / %s",mProcessingTime,
		  	ctime((const time_t *)&mProcessingTime));
  
  fprintf(fd,"\tcomment      : %s\n"    ,mComment);
  fprintf(fd,"\tstatus       : 0x%08x\n",mStatus);
}


// $Log: EEmcEventHeader.cxx,v $
// Revision 1.11  2007/07/12 19:30:13  fisyak
// Add includes for ROOT 5.16
//
// Revision 1.10  2004/07/09 02:38:05  balewski
// BTOW data are not masked out any more but headres are checked as for EEMC
//
// Revision 1.9  2003/09/17 22:05:25  zolnie
// delete mumbo-jumbo
//
// Revision 1.8  2003/09/05 15:04:20  zolnie
// remove Stiostream/iostream from the source code
//
// Revision 1.7  2003/09/02 17:57:55  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.6  2003/06/03 02:41:03  zolnie
// *** empty log message ***
//
// Revision 1.5  2003/06/03 02:40:02  zolnie
// added run number
//
// Revision 1.4  2003/05/27 20:25:21  zolnie
// print status
//
// Revision 1.3  2003/05/27 19:11:43  zolnie
// added dE/dx info
//
// Revision 1.2  2003/05/26 14:44:34  zolnie
// rewritten implementation of EEmcL3Tracks using TClonesArray
// introduced a common Makefile and mklinkdef.pl
//
// Revision 1.1  2003/05/20 19:22:58  zolnie
// new additions for ..... :)
//

