/***************************************************************************
 *
 * $Id: StRichMCPixel.h,v 2.1 2000/05/22 21:44:44 ullrich Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description:
 *   MC pixel contains  the raw pixel info but also 
 *
 ***************************************************************************
 *
 * $Log: StRichMCPixel.h,v $
 * Revision 2.1  2000/05/22 21:44:44  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StRichMCPixel_hh
#define StRichMCPixel_hh

#include "StRichPixel.h"
#include "StRichMCInfo.h"
#include "StContainers.h"

class StRichMCPixel : public StRichPixel {
public:
    StRichMCPixel();
    StRichMCPixel(ULong_t packedData);
    StRichMCPixel(ULong_t packedData, const StSPtrVecRichMCInfo&);
    // StRichMCPixel(const StRichMCPixel&);            use default
    // StRichMCPixel& operator=(const StRichMCPixel&); use default
    ~StRichMCPixel();
    
    Int_t operator==(const StRichMCPixel&) const;
    Int_t operator!=(const StRichMCPixel&) const;

    UShort_t contributions() const;
    
    void addInfo(const StRichMCInfo*);
    void setInfo(const StSPtrVecRichMCInfo&);
    
    const StSPtrVecRichMCInfo& getMCInfo() const;
    StSPtrVecRichMCInfo&       getMCInfo();
    
protected:
    StSPtrVecRichMCInfo  mInfo;
    
    ClassDef(StRichMCPixel,1)
};

inline UShort_t StRichMCPixel::contributions() const { return mInfo.size(); }
inline void StRichMCPixel::addInfo(const StRichMCInfo* p) { mInfo.push_back(p);}
inline void StRichMCPixel::setInfo(const StSPtrVecRichMCInfo& p) { mInfo = p;}
inline const StSPtrVecRichMCInfo& StRichMCPixel::getMCInfo() const {return mInfo;}
inline StSPtrVecRichMCInfo& StRichMCPixel::getMCInfo() { return mInfo;}

#endif
