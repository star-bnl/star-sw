/***************************************************************************
 *
 *   StLocalHit.hh
 *  base class for localHits on rows of subdetector (TPC, SVT, FTPC)
 *             devices.
 * MALisa 9jun99
 * $Log: StLocalHit.hh,v $
 * Revision 1.3  1999/09/23 21:25:21  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 ******************************************************************/
#ifndef StLocalHit_HH
#define StLocalHit_HH

#include <iostream.h>

class StLocalHit{
public:
    StLocalHit();
    StLocalHit(const float&, const float&);
    ~StLocalHit();
    
    float localX() const {return mLocalX;};  // returns local X coordinate
    float globalZ() const {return mGlobalZ;};  // returns local Z coordinate
    
    void setLocalX(float x) { mLocalX = x;}
    void setGlobalZ(float z){ mGlobalZ = z;}


    int operator==(const StLocalHit&) const;
    int operator!=(const StLocalHit&) const;
    
private:
    float mLocalX;
    float mGlobalZ;
};
ostream& operator<<(ostream &, const StLocalHit&);


#endif
