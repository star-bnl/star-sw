#ifndef EztEventHeader_h
#define EztEventHeader_h
/*********************************************************************
 * $Id: EztEventHeader.h,v 1.1 2004/10/28 00:10:19 mvl Exp $
 *********************************************************************/

#include <cstdio>
#include <ctime>
#include "TObject.h"


class EztEventHeader : public TObject {
 private:
  unsigned mEventNumber;       //
  unsigned mToken;             //
  time_t   mTimeStamp;         //(unix time, GMT) 
  time_t   mProcessingTime;    // auxiliary 
  unsigned mStatus;            // event status 
  int      mCommentLen;        //
  char    *mComment;           //[mCommentLen];
  unsigned mRunNumber;         //
  
 public:
  EztEventHeader();
  ~EztEventHeader();
  void         print(int flag=0, FILE *f = stdout) const;
  void         clear();

  void         setEventNumber   ( unsigned en) { mEventNumber    = en; }
  void         setRunNumber     ( unsigned rn) { mRunNumber      = rn; }
  void         setToken         ( unsigned et) { mToken          = et; }
  void         setTimeStamp     ( time_t    t) { mTimeStamp      = t;  }
  void         setProcessingTime( time_t    t) { mProcessingTime = t;  }
  void         setStatus        ( unsigned st) { mStatus         = st; }
  void         setComment       ( const char *str); 


  unsigned     getEventNumber()    const { return mEventNumber;    }
  unsigned     getRunNumber  ()    const { return mRunNumber;      }
  unsigned     getToken()          const { return mToken;          }
  time_t       getTimeStamp()      const { return mTimeStamp;      }
  time_t       getProcessingTime() const { return mProcessingTime; }
  unsigned     getStatus        () const { return mStatus;         }
  const char * getComment ()       const { return mComment;        }
  
  ClassDef(EztEventHeader,1) 
 
};
#endif


/*
 * $Log: EztEventHeader.h,v $
 * Revision 1.1  2004/10/28 00:10:19  mvl
 * Initial revision of ezTree classes (for EEmc raw data)
 *
 *
 *********************************************************************/

