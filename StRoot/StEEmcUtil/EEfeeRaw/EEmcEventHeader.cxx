// $Id: EEmcEventHeader.cxx,v 1.1 2003/05/20 19:22:58 zolnie Exp $ 
// $Log: EEmcEventHeader.cxx,v $
// Revision 1.1  2003/05/20 19:22:58  zolnie
// new additions for ..... :)
//

#include "EEmcEventHeader.h"
 
#include <iostream.h>

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
EEmcEventHeader ::  ~EEmcEventHeader() { }

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
  mTimeStamp  = -1;
  mProcessingTime  = -1;
  mCommentLen = 0;
  if(mComment) delete [] mComment;
}



//--------------------------------------------------
//
//--------------------------------------------------
void EEmcEventHeader :: print(FILE *fd) const{
  fprintf(fd,"EEmcEventHeader:\n");
  fprintf(fd,"\tevent number : %d (0x%04x)\n",mEventNumber,mEventNumber);
  fprintf(fd,"\ttoken        : %d (0x%04x)\n",mToken      ,mToken);
  fprintf(fd,"\ttime stamp   : %ld / %s",mTimeStamp     ,
		  	ctime((const time_t *)&mTimeStamp));
  fprintf(fd,"\tproc. time   : %ld / %s",mProcessingTime,
		  	ctime((const time_t *)&mProcessingTime));
  
  fprintf(fd,"\tcomment      : %s\n"    ,mComment);
}



