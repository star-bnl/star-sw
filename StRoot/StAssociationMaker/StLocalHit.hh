/***************************************************************************
 *
 *   StLocalHit.hh
 *  base class for localHits on rows of subdetector (TPC, SVT, FTPC)
 *             devices.
 * MALisa 9jun99
 * $Log: StLocalHit.hh,v $
 * Revision 1.4  1999/10/01 14:08:58  calderon
 * Added Local Hit resolution Histogram. It is made by default
 * without any requirement of association, to serve
 * as a diagnostic.
 * Before building track multimap, check the size of the
 * tpc hit map.  If it is too small, print out a warning
 * and exit.
 *
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
    
    float localX() const {return mLocalX;}  // returns local X coordinate
    float globalZ() const {return mGlobalZ;}  // returns local Z coordinate
    
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
