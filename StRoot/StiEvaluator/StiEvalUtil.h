//
// RunClassUtil.h
// mcbs
//
#ifndef StiEvalUtil_hh
#define StiEvalUtil_hh

#include <cmath>
#include "StMiniMcEvent/StTinyMcTrack.h"
#include "StMiniMcEvent/StTinyRcTrack.h"


float pirapidity(float pt,float pz) {
    float E = ::sqrt(.13957018*.13957018 + pt*pt + pz*pz);
    float y = .5 * ::log((E+pz)/(E-pz));    
    return y;
}

float tinymctrackrapidity(StTinyMcTrack* tinymctrack) {
    float pz = tinymctrack->pzMc();
    float pt = tinymctrack->ptMc();
    return pirapidity(pt,pz);
}

float tinyrectrackrapidity(StTinyRcTrack* tinyrectrack) {
    float pz = tinyrectrack->pzPr();
    float pt = tinyrectrack->ptPr();
    return pirapidity(pt,pz);
}
#endif
