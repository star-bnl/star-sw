/*
 * StLocalHit: base class for localHits on rows of subdetector (TPC, SVT, FTPC)
 *             devices.
 * MALisa 9jun99
 */
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
    
    bool operator==(const StLocalHit&) const;
    bool operator!=(const StLocalHit&) const;
    
    
private:
    float mLocalX;
    float mGlobalZ;
};
ostream& operator<<(ostream &, const StLocalHit&);


#endif
