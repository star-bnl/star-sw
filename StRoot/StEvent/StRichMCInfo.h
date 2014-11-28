/*!
 * \class StRichMCInfo 
 * \author Brian Lasiuk, May 2000
 *
 *    Contains the simulator info that is NOT stored in the
 *    g2t tables --> most importantly a tag of the process type
 *
 */
/***************************************************************************
 *
 * $Id: StRichMCInfo.h,v 2.3 2002/02/22 22:56:49 jeromel Exp $
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
 * Revision 2.3  2002/02/22 22:56:49  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:40  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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
    StRichMCInfo(int id, int gid,  int trk,
                 float q, int process);
    // StRichMCInfo(const StRichMCInfo&);            use default
    // StRichMCInfo& operator=(const StRichMCInfo&); use default
    ~StRichMCInfo();
    
    int operator==(const StRichMCInfo&) const;
    int operator!=(const StRichMCInfo&) const;

    int   id()      const;
    int   gid()     const;
    int   trackp()  const;
    float charge()  const;
    int   process() const;
    
protected:
    Int_t     mId;
    Int_t     mGid;
    Int_t     mTrackp;
    Float_t   mCharge;
    Int_t     mProcess;
    
    ClassDef(StRichMCInfo,1)
};

inline int StRichMCInfo::id()  const { return ( mId ); }
inline int StRichMCInfo::gid()  const { return ( mGid );}
inline int StRichMCInfo::trackp()  const {return ( mTrackp );}
inline float StRichMCInfo::charge()  const {return ( mCharge );}
inline int StRichMCInfo::process()  const { return ( mProcess );}

#endif
