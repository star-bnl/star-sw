/***************************************************************************
 *
 * $Id: StRichMCInfo.h,v 2.1 2000/05/22 21:44:38 ullrich Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description:
 *   Contains the simulator info that is NOT stored in the
 *   g2t tables --> most importantly a tag of the process type
 ***************************************************************************
 *
 * $Log: StRichMCInfo.h,v $
 * Revision 2.1  2000/05/22 21:44:38  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StRichMCInfo_hh
#define StRichMCInfo_hh

#include "StObject.h"
#include "StEnumerations.h"

class StRichMCInfo : public StObject {
public:
    StRichMCInfo();
    StRichMCInfo(Long_t id, Long_t gid,  Long_t trk,
		 Float_t q, Long_t process);
    // StRichMCInfo(const StRichMCInfo&);            use default
    // StRichMCInfo& operator=(const StRichMCInfo&); use default
    ~StRichMCInfo();
    
    Int_t operator==(const StRichMCInfo&) const;
    Int_t operator!=(const StRichMCInfo&) const;

    Long_t  id()      const;
    Long_t  gid()     const;
    Long_t  trackp()  const;
    Float_t charge()  const;
    Long_t  process() const;
    
protected:
    Long_t  mId;
    Long_t  mGid;
    Long_t  mTrackp;
    Float_t   mCharge;
    Long_t  mProcess;
    
    ClassDef(StRichMCInfo,1)
};

inline Long_t StRichMCInfo::id()  const { return ( mId ); }
inline Long_t StRichMCInfo::gid()  const { return ( mGid );}
inline Long_t StRichMCInfo::trackp()  const {return ( mTrackp );}
inline Float_t StRichMCInfo::charge()  const {return ( mCharge );}
inline Long_t StRichMCInfo::process()  const { return ( mProcess );}

#endif
