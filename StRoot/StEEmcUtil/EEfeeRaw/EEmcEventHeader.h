#ifndef EEmcEventHeader_h
#define EEmcEventHeader_h
/*********************************************************************
 * $Id: EEmcEventHeader.h,v 1.1 2003/05/20 19:22:58 zolnie Exp $
 *********************************************************************
 * $Log: EEmcEventHeader.h,v $
 * Revision 1.1  2003/05/20 19:22:58  zolnie
 * new additions for ..... :)
 *
 *********************************************************************/
#include <stdio.h>
#include <time.h>
#include "TObject.h"


class EEmcEventHeader : public TObject {
 private:
  unsigned mEventNumber;       //
  unsigned mToken;             //
  time_t   mTimeStamp;         //(unix time, GMT) 
  time_t   mProcessingTime;    // auxiliary 
  int      mCommentLen;        //
  char    *mComment;           //[mCommentLen];
  
 public:
  EEmcEventHeader();
  virtual ~EEmcEventHeader();
  void         print(FILE *f = stdout) const;
  void         clear();

  void         setEventNumber   ( unsigned en) { mEventNumber    = en; };
  void         setToken         ( unsigned et) { mToken          = et; };
  void         setTimeStamp     ( time_t    t) { mTimeStamp      = t;  };
  void         setProcessingTime( time_t    t) { mProcessingTime = t;  };
  void         setComment       ( const char *str); 

  unsigned     getEventNumber()    const { return mEventNumber;    }
  unsigned     getToken()          const { return mToken;          }
  time_t       getTimeStamp()      const { return mTimeStamp;      }
  time_t       getProcessingTime() const { return mProcessingTime; }
  const char * getComment ()       const { return mComment;        }
  
  ClassDef(EEmcEventHeader,1) 

};
#endif


