/*****************************************************************
 * $Id: StTofCross.h,v 1.1 2001/04/24 20:27:37 wzhang Exp $
 *
 * Author: Wei-Ming Zhang, April 2001
 *
 *****************************************************************
 * Description:
 * Public functions to check if a track crosses TOF 
 *
 *****************************************************************
 *
 * $Log: StTofCross.h,v $
 * Revision 1.1  2001/04/24 20:27:37  wzhang
 * First release
 *
 *                              
 *****************************************************************/

#ifndef ST_TOF_CROSS_H
#define ST_TOF_CROSS_H

#include <vector>

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#include "StPhysicalHelixD.hh"
#include "StTofDatabase.h"
#include "TH1.h"

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    typedef vector<Int_t>  idVector;  
    typedef vector<Int_t>::iterator idVectorIter;
#else
    typedef vector<Int_t, allocator<Int_t> >  idVector; 
    typedef vector<Int_t, allocator<Int_t> >::iterator idVectorIter;
#endif   

    struct StructSlatHit {
          StructSlatHit() { }
          Int_t             slatIndex;   
          StThreeVectorD    hitPosition;
          idVector          trackIdVec;
    };

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    typedef vector<StructSlatHit>  tofSlatHitVector; 
    typedef vector<StructSlatHit>::iterator tofSlatHitVectorIter; 
#else
    typedef vector<SructSlatHit, allocator<StructSlatHit> >  tofSlatHitVector;
    typedef vector<SructSlatHit, allocator<StructSlatHit> >::iterator tofSlatHitVectorIter;
#endif   

class StTofCross {
public:

    StTofCross();
    virtual ~StTofCross();

    Bool_t               tofSlatCross(StThreeVectorD& point,
                                           StructTofSlat* tofSlat)       const;

    Int_t                tofSlatCrossId(StThreeVectorD& point,
                                             StTofDatabase* tofDb)       const;

    Int_t                tofSlatCrossId(Int_t volumeId,
                                             StTofDatabase* tofDb)       const;

    idVector             tofClusterFinder(StThreeVectorD& pointAtCyl,
                                             StTofDatabase* tofDb)       const;
/*
 one dim histogram hist is used to study hit pattern of TOF hits. 
 it could be taken out later.
*/ 
    tofSlatHitVector     tofHelixToSlat(TH1F* hist, StPhysicalHelixD& helix,
                          Double_t pathLength, StTofDatabase* tofDb);
/*
    tofSlatHitVector     tofHelixToSlat(StPhysicalHelixD helix,
                          Double_t pathLength, StTofDatabase* tofDb);
*/

};

#endif
