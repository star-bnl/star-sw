#include "EEfeeRunDescr.h"
 
//#include <Stiostream.h>

ClassImp(EEfeeRunDescr)

  


//--------------------------------------------------
//
//--------------------------------------------------
EEfeeRunDescr ::  EEfeeRunDescr() {
  mComment=NULL;
  
  clear();
}

//--------------------------------------------------
//
//--------------------------------------------------
EEfeeRunDescr ::  ~EEfeeRunDescr() 
{ 
  if(mComment) delete [] mComment;
}



//--------------------------------------------------
void EEfeeRunDescr :: setComment(const char* s) {
  mCommentLen = (strlen(s)/8+1)*8; //make it 8 byte aligned
  //mCommentLen = (mCommentLen<MaxCommentLen) ? mCommentLen : MaxCommentLen;
  if(mComment) delete [] mComment;
  mComment = new char[mCommentLen];
  strncpy(mComment,s,mCommentLen);
}




//--------------------------------------------------
//
//--------------------------------------------------
void EEfeeRunDescr :: clear() {
  mTimeStamp  = -1;
  mProcessingTime  = -1;
  mCommentLen = 0;
  if(mComment) delete [] mComment;
}



//--------------------------------------------------
//
//--------------------------------------------------
void EEfeeRunDescr :: print() const{
  printf("EEfeeRunDescr:: print()\nmTimeStamp=%ld --> %s",mTimeStamp,ctime((const time_t *)&mTimeStamp));

printf("mProcessingTime =%ld --> %s",mProcessingTime ,
       ctime((const time_t *)&mProcessingTime));
 
 printf("mComment=%s\n",mComment);
}



