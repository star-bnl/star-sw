/***************************************************************************
 * $Id: StTofMC.h,v 1.1 2001/04/24 20:27:37 wzhang Exp $
 *
 * Author: Wei-Ming Zhang, April 2001
 *
 *****************************************************************
 * Description:
 * Generate StTofMCSlat from g2t tof-hit.
 *
 *****************************************************************
 * 
 * $Log: StTofMC.h,v $
 * Revision 1.1  2001/04/24 20:27:37  wzhang
 * First release
 *
 *
 **************************************************************************/
#ifndef StTofMC_hh
#define StTofMC_hh

#include <iostream.h>
#include "StTofDatabase.h"
#include "StTofMCSlat.h"
#include "StTofMCHit.h"

#ifndef ST_NO_NAMESPACES
using std::vector;
using std::copy;
#endif

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    typedef vector<StTofMCSlat> tofMCSlatVector;
    typedef vector<StTofMCSlat>::iterator tofMCSlatVecIter;
#else
    typedef vector<StTofMCSlat, allocator<StTofMCSlat> > tofMCSlatVector;
    typedef vector<StTofMCSlat, allocator<StTofMCSlat> >::iterator tofMCSlatVecIter;

#endif

class StTofMC {
public:
    StTofMC();
    //StTofMC(const StTofMC&);
    //StTofMC& operator=(const StTofMC&);
    virtual  ~StTofMC();

// generate a vector of StTofMCSlats from g2t hits
//    void gHit2MCSlat(int nTofHits, StTofG2THit* g2tTofHit, 
//                             StTofG2TTrack* g2tTrack, StTofDatabase* tofDb);

// simluate slat response from a g2t-hit 
    StTofMCSlat slatResponse(Int_t slatId, StTofG2THit* g2tTofHit, 
                             StTofG2TTrack* g2tTrack, StTofDatabase* tofDb);

// two options for responding function
    Float_t slatResponseExp(Float_t length, StTofSimParam* simParam);

    Float_t slatResponseTable(Float_t length, Float_t delEdge,
                                    Float_t tof, StTofSimParam* simParam);

//  random Gaussian (mean = 0 and variance = 1) function 
    Float_t gaussian();

    Bool_t       push_back(StTofMCSlat MCSlat);
    size_t       size()  const;
    StTofMCSlat  front() const;
    StTofMCSlat  back()  const;
    StTofMCSlat  getSlat(size_t index) const;

    void clear();

private:

    tofMCSlatVector     mMCSlatVec;

    ClassDef(StTofMC,1)
};

#endif
