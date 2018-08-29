/*
 *  StFixedVertexFinder.cxx
 *  $Id: StFixedVertexFinder.cxx,v 1.5.4.2 2018/04/30 16:36:43 didenko Exp $
 *
 *  Author Lee Barnby (University of Birmingham) May 2006.
 *
 */

#include <StEvent/StEventTypes.h>
#include <StEvent/StEnumerations.h>
#include <St_base/StMessMgr.h>


#include "StGenericVertexMaker/StFixedVertexFinder.h"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcVertex.hh"
#include "StChain/StMaker.h"


StFixedVertexFinder::StFixedVertexFinder() : 
  StGenericVertexFinder(),
  mFixedX(0),
  mFixedY(0),
  mFixedZ(0),
  mFixedEx(0),
  mFixedEy(0),
  mFixedEz(0)
{
    mMode=0;
}

int StFixedVertexFinder::fit(StEvent* event)
{
  StPrimaryVertex  prim;
  StVertexFinderId VFId;

  // All errors are zero... by default
  // if user has set a vertex error, we use it, regardless of mode

  float s2x = mFixedEx * mFixedEx;
  float s2y = mFixedEy * mFixedEy;
  float s2z = mFixedEz * mFixedEz;

  float   cov[6] = {s2x,
		    0.0,s2y,
		    0.0,0.0,s2z}; 


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
  addVertex(&prim);

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
void StFixedVertexFinder::SetVertexError(double x, double y, double z){
    mFixedEx=x;
    mFixedEy=y;
    mFixedEz=z;
}

/*
 * $Log: StFixedVertexFinder.cxx,v $
 * Revision 1.5.4.2  2018/04/30 16:36:43  didenko
 * revision for SL16j_embed library
 *
 * Revision 1.8.2.1  2018/04/13 23:59:13  didenko
 * updated for SL16d_embed
 *
 * Revision 1.8  2018/03/24 20:10:41  jwebb
 *
 * Added option for user to specify the uncertainties on the vertex.  Useful
 * in embedding jobs in order to get the track association with primary
 * vertex correct (especially when tracks are from precision tracking, eg
 * HFT).
 *
 * Revision 1.7  2017/05/12 18:37:23  smirnovd
 * Cosmetic changes
 *
 * Removed log messages from source files
 * Prefixed included headers with paths to respective modules
 *
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
