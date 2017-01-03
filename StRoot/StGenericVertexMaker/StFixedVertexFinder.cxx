/*
 *  StFixedVertexFinder.cxx
 *  $Id: StFixedVertexFinder.cxx,v 1.6 2017/01/03 22:17:36 smirnovd Exp $
 *
 *  Author Lee Barnby (University of Birmingham) May 2006.
 *
 */

#include <StEventTypes.h>
#include <StEnumerations.h>
#include <StMessMgr.h>


#include "StFixedVertexFinder.h"
#include "StMcEvent.hh"
#include "StMcVertex.hh"
#include "StMaker.h"


StFixedVertexFinder::StFixedVertexFinder(){
    mFixedX=mFixedY=mFixedZ=0.0;
    mMode=0;
}

int StFixedVertexFinder::fit(StEvent* event)
{
  StPrimaryVertex  prim;
  StVertexFinderId VFId;
  Float_t cov[6] = {0.0,0.0,0.0,0.0,0.0,0.0}; // All errors are zero

  if (mMode == 0){
    // Really the default which takes the SetPos() TBI
    LOG_DEBUG << "StFixedVertexFinder::fit() fixing a vertex" << endm;
    StThreeVectorD pos(mFixedX,mFixedY,mFixedZ);
    prim.setPosition(pos);
    VFId = undefinedVertexFinder;

  } else {
    
    LOG_DEBUG << "StFixedVertexFinder::fit() reading a vertex from StMcEvent" << endm;
    StMcEvent* mcEvent= (StMcEvent*) StMaker::GetChain()->GetDataSet("StMcEvent");
    
    if ( ! mcEvent){
      // Not sure what to do here, will leave and do nothing (so no vertex)
      // and it will be super obvious that something went terribly wrong
      LOG_ERROR << "StFixedVertexFinder::fit() no StMcEvent" << endm;
      return 0;

    } else {
      StMcVertex *pv = mcEvent->primaryVertex();
      StThreeVectorD pos(pv->position().x(),
    			 pv->position().y(),
    			 pv->position().z());
      //cout << "DEBUG :: saving McVertex " << pos << endl;
      prim.setPosition(pos);
    }
    VFId = mcEventVertexFFinder;
  }

  prim.setCovariantMatrix(cov);
  prim.setFlag(1);                    // So that we know its the primary vertex
  prim.setRanking(-5);                // Have to have something
  prim.setVertexFinderId(VFId);       // Id depends on MC or fixed position used
  addVertex(prim);

  return size();
}

void StFixedVertexFinder::printInfo(ostream& os)const{
    os << "StFixedVertexFinder - fixed vertex" << endl;
    os << "Fixed position: x=" << mFixedX << " y=" << mFixedY << " z=" << mFixedZ << endl;
}

void StFixedVertexFinder::UseVertexConstraint(){
    LOG_WARN << "StFixedVertexFinder::UseVertexConstraint() - vertex beam constraint NOT implemented in context of fixed vertex finder" << endm;

}

void StFixedVertexFinder::SetVertexPosition(double x, double y, double z){
    mFixedX=x;
    mFixedY=y;
    mFixedZ=z;
}

/*
 * $Log: StFixedVertexFinder.cxx,v $
 * Revision 1.6  2017/01/03 22:17:36  smirnovd
 * [Stylistic] Changed public addVertex() to accept references
 *
 * Avoid unnecessary gymnastics with pointers
 *
 * Revision 1.5  2016/08/18 17:46:14  smirnovd
 * Squashed commit of the following refactoring changes:
 *
 * Date:   Wed Jul 27 18:31:18 2016 -0400
 *
 *     Removed unused arguments in UseVertexConstraint()
 *
 *     In StiPPVertexFinder and StvPPVertexFinder this method does nothing
 *
 * Date:   Wed Jul 27 16:47:58 2016 -0400
 *
 *     Make old UseVertexConstraint private virtual and call it from its public replacement in the base class
 *
 *     also mark methods as private explicitly
 *
 * Date:   Wed Jul 27 16:52:02 2016 -0400
 *
 *     Removed unused private data member mWeight
 *
 * Date:   Wed Jul 27 16:50:42 2016 -0400
 *
 *     Prefer base class static beamline parameters rather than this class private members
 *
 * Date:   Wed Jul 27 16:21:49 2016 -0400
 *
 *     StPPVertexFinder: Got rid of unused private beamline parameters
 *
 *     The equivalent measurements are available from the base class
 *     StGenericVertexFinder
 *
 * Date:   Wed Jul 27 16:19:19 2016 -0400
 *
 *     StPPVertexFinder: For beamline position use equivalent static methods from parent class
 *
 * Date:   Wed Jul 27 16:05:50 2016 -0400
 *
 *     StGenericVertexMaker: Assigning once is enough
 *
 * Date:   Mon Aug 15 10:43:49 2016 -0400
 *
 *     StGenericVertexFinder: Print out beamline parameters
 *
 *     Print beamline values as extracted from the database before any modification.
 *
 * Date:   Wed Jul 6 15:33:02 2016 -0400
 *
 *     Stylistic changes and minor refactoring
 *
 *     Whitespace and comments for improved readability
 *     s/track/stiKalmanTrack/
 *
 * Date:   Wed Jul 6 15:28:16 2016 -0400
 *
 *     StPPVertexFinder: Switched to cleaner c++11 range loop syntax
 *
 * Date:   Wed Jul 6 15:22:14 2016 -0400
 *
 *     StPPVertexFinder: Minor c++ refactoring
 *
 *     - Removed unused counter
 *     - c-style array to std::array
 *
 * Date:   Wed Jul 6 15:20:11 2016 -0400
 *
 *     Deleted commented out code
 *
 *     Removed unused #include's StMinuitVertexFinder
 *
 * Revision 1.4  2006/05/18 19:14:24  lbarnby
 * Added SetVertexPosition function. Tidied up comments/docs
 *
 */
