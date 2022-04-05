// $Id: EztEventHeader.cxx,v 1.1 2004/10/28 00:10:19 mvl Exp $ 

#include "EztEventHeader.h"

ClassImp(EztEventHeader)

  
//--------------------------------------------------
//
//--------------------------------------------------
EztEventHeader ::  EztEventHeader() {
  mComment=NULL;  
  clear();
}

//--------------------------------------------------
//
//--------------------------------------------------
EztEventHeader ::  ~EztEventHeader() 
{ 
  if(mComment) delete [] mComment;
}

//--------------------------------------------------
//
//--------------------------------------------------
void EztEventHeader :: setComment(const char* s) {
  mCommentLen = (strlen(s)/8+1)*8; //make it 8 byte aligned
  if(mComment) delete [] mComment;
  mComment = new char[mCommentLen];
  strncpy(mComment,s,mCommentLen);
}


//--------------------------------------------------
//
//--------------------------------------------------
void EztEventHeader :: clear() {
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
void EztEventHeader :: print(int flag, FILE *fd) const{
  fprintf(fd,"EztEventHeader:\n");
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


// $Log: EztEventHeader.cxx,v $
// Revision 1.1  2004/10/28 00:10:19  mvl
// Initial revision of ezTree classes (for EEmc raw data)
//
//

