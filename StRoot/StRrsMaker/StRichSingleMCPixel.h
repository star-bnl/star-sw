/****************************************************************
 * $Id: StRichSingleMCPixel.h,v 1.3 2000/05/19 15:44:38 lasiuk Exp $
 *
 * Description:
 *  Definition of a single MC pixel object
 *
 *
 ****************************************************************
 *
 * $Log: StRichSingleMCPixel.h,v $
 * Revision 1.3  2000/05/19 15:44:38  lasiuk
 * clone members added
 *
 * Revision 1.2  2000/05/17 22:29:14  lasiuk
 * keep charge info as a float only.  Access with charge() uniformly
 *
 * Revision 1.1  2000/04/05 15:55:20  lasiuk
 * Initial Revision
 *
 ****************************************************************/
#ifndef ST_RICH_SINGLE_MC_PIXEL_H
#define ST_RICH_SINGLE_MC_PIXEL_H

#include "StRichSinglePixel.h"
#include "StRichPadPlane.h" // for typedefs

class StRichSingleMCPixel : public StRichSinglePixel {
public:
    StRichSingleMCPixel();
    StRichSingleMCPixel(int p, int r, float q);
    StRichSingleMCPixel(int p, int r, float q, anIDList info);

    ~StRichSingleMCPixel();
    StRichSingleMCPixel* clone();
    
    //StRichSingleMCPixel(const StRichSingleMCPixel&) {/*use default*/}
    //StRichSingleMCPixel& operator=(const StRichSingleMCPixel&) {/*use default*/|

    const anIDList& MCInfo() const;
    void  setMCInfo(const anIDList&);
    
protected:
    anIDList mMCInfo;//!
};

inline const anIDList& StRichSingleMCPixel::MCInfo() const { return mMCInfo;}
inline void StRichSingleMCPixel::setMCInfo(const anIDList& id) { mMCInfo = id;}
inline StRichSingleMCPixel* StRichSingleMCPixel::clone() {return new StRichSingleMCPixel(*this);}

// Non-member
ostream& operator<<(ostream& os, const StRichSingleMCPixel& pix);
#endif
