/*!
 * \class StRichMCPixel 
 * \author Brian Lasiuk, May 2000
 *
 *    MC pixel contains  the raw pixel info but also
 *
 */
/***************************************************************************
 *
 * $Id: StRichMCPixel.h,v 2.3 2002/02/22 22:56:49 jeromel Exp $
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
 * Revision 2.3  2002/02/22 22:56:49  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:40  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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
    StRichMCPixel(unsigned int packedData);
    StRichMCPixel(unsigned int packedData, const StSPtrVecRichMCInfo&);
    // StRichMCPixel(const StRichMCPixel&);            use default
    // StRichMCPixel& operator=(const StRichMCPixel&); use default
    ~StRichMCPixel();
    
    int operator==(const StRichMCPixel&) const;
    int operator!=(const StRichMCPixel&) const;

    unsigned short contributions() const;
    
    void addInfo(const StRichMCInfo*);
    void setInfo(const StSPtrVecRichMCInfo&);
    
    const StSPtrVecRichMCInfo& getMCInfo() const;
    StSPtrVecRichMCInfo&       getMCInfo();
    
protected:
    StSPtrVecRichMCInfo  mInfo;
    
    ClassDef(StRichMCPixel,1)
};

inline unsigned short StRichMCPixel::contributions() const { return mInfo.size(); }
inline void StRichMCPixel::addInfo(const StRichMCInfo* p) { mInfo.push_back(p);}
inline void StRichMCPixel::setInfo(const StSPtrVecRichMCInfo& p) { mInfo = p;}
inline const StSPtrVecRichMCInfo& StRichMCPixel::getMCInfo() const {return mInfo;}
inline StSPtrVecRichMCInfo& StRichMCPixel::getMCInfo() { return mInfo;}

#endif
