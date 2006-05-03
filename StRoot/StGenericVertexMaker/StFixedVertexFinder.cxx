/*
 *  StFixedVertexFinder.cxx
 *  
 *
 *  Created by Lee Barnby on 02/05/2006.
 *  Copyright 2006 Univerity of Birmingham. All rights reserved.
 *
 */

#include <StEventTypes.h>
#include <StEnumerations.h>
#include <StMessMgr.h>


#include "StFixedVertexFinder.h"

StFixedVertexFinder::StFixedVertexFinder(){
    mFixedX=mFixedY=mFixedZ=0.0;
}

int StFixedVertexFinder::fit(StEvent* event){
    LOG_DEBUG << "StFixedVertexFinder::fit() fixing a vertex" << endm;
    StPrimaryVertex prim;
    StThreeVectorD pos(mFixedX,mFixedY,mFixedZ);
    prim.setPosition(pos);
    Float_t cov[6] = {0.0,0.0,0.0,0.0,0.0,0.0}; // All errors are zero
    prim.setCovariantMatrix(cov);
    prim.setFlag(1); // So that we know its the primary vertex
    prim.setRanking(-5); // Have to have something
    prim.setVertexFinderId(undefinedVertexFinder); // Unless/until this 'finder' gets its own id
    addVertex(&prim);
    
    return size();
}

void StFixedVertexFinder::printInfo(ostream& os)const{
    os << "StFixedVertexFinder - fixed vertex" << endl;
    os << "Fixed position: x=" << mFixedX << " y=" << mFixedY << " z=" << mFixedZ << endl;
}

void StFixedVertexFinder::UseVertexConstraint(double x0, double y0, double dxdz, double dydz, double weight){
    LOG_WARN << "StFixedVertexFinder::UseVertexConstraint() - vertex beam constraint NOT implemented in context of fixed vertex finder" << endm;
}
