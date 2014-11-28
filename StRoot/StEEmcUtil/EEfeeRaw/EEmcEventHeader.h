#ifndef EEmcEventHeader_h
#define EEmcEventHeader_h
/*********************************************************************
 * $Id: EEmcEventHeader.h,v 1.9 2007/07/12 19:30:13 fisyak Exp $
 *********************************************************************/
#include <string.h>
#include <cstdio>
#include <ctime>
#include "TObject.h"


class EEmcEventHeader : public TObject {
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
  EEmcEventHeader();
  virtual ~EEmcEventHeader();
  void         print(FILE *f = stdout) const;
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
  
  ClassDef(EEmcEventHeader,4) 

};
#endif


/*
 * $Log: EEmcEventHeader.h,v $
 * Revision 1.9  2007/07/12 19:30:13  fisyak
 * Add includes for ROOT 5.16
 *
 * Revision 1.8  2003/12/02 17:22:08  balewski
 * fix after version mixup
 *
 * Revision 1.6  2003/11/24 05:40:55  balewski
 * new stuff for miniDaq
 *
 * Revision 1.5  2003/09/11 19:41:01  zolnie
 * updates for gcc3.2
 *
 * Revision 1.4  2003/06/16 16:03:54  zolnie
 * updated root version number
 *
 * Revision 1.3  2003/06/02 18:55:00  zolnie
 * added run number to the header
 *
 * Revision 1.2  2003/05/27 19:11:44  zolnie
 * added dE/dx info
 *
 * Revision 1.1  2003/05/20 19:22:58  zolnie
 * new additions for ..... :)
 *
 *********************************************************************/

