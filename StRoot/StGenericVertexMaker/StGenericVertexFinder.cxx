/***************************************************************************
 * $Id: StGenericVertexFinder.cxx,v 1.21 2016/04/12 19:48:47 smirnovd Exp $
 *
 * Author: Lee Barnby, April 2003
 *
 ***************************************************************************
 * Description: Base class for vertex finders
 *
 ***************************************************************************/
#include "StarRoot/TRMatrix.h"
#include "StarRoot/TRSymMatrix.h"
#include "StGenericVertexFinder.h"
#include "StMessMgr.h"
#include "StMaker.h"
#include "StEventTypes.h"

// Initialize static variable with default values
vertexSeed_st StGenericVertexFinder::sBeamline;


//______________________________________________________________________________
StGenericVertexFinder::StGenericVertexFinder() : 
  mVertexConstrain(false), mMode(0), mDebugLevel(0)
{
  
  mIsMC	  =0;            	// flag minor differences between Data & M-C
  mUseBtof=0;           	// default use btof = false
  mUseCtb =0;            	// default use ctb = false
  mVertexOrderMethod = orderByNumberOfDaughters;
}
//______________________________________________________________________________
StGenericVertexFinder::~StGenericVertexFinder()
{
}

/*!
  Adds the vertex to StEvent (currently as a primary)
  Here we invent our own flag and other data to put in
  In real life we have to get it from somewhere (as done for position)
*/
//______________________________________________________________________________
void 
StGenericVertexFinder::FillStEvent(StEvent* event){

  for(UInt_t i=0;i<mVertexList.size(); i++) {
    //allocates new memory for each vertex
    StPrimaryVertex* primV = new StPrimaryVertex(mVertexList[i]); 
    event->addPrimaryVertex(primV,mVertexOrderMethod);
    LOG_INFO << "StGenericVertexFinder::FillStEvent: Added " <<i+1 
		     <<" primary vertex (" << mVertexOrderMethod << ")" << endm;
  }

  // Use StEvent's ordering
  // (might be undesirable for some debugging)
  // Also could be wrong if StEvent already has vertices for some reason
  mVertexList.clear();
  for(UInt_t i=0;i<event->numberOfPrimaryVertices(); i++)
    mVertexList.push_back(*(event->primaryVertex(i)));

}
//______________________________________________________________________________
void StGenericVertexFinder::addVertex(StPrimaryVertex* vtx)
{
  mVertexList.push_back(*vtx);
}
//______________________________________________________________________________
void StGenericVertexFinder::UsePCT(bool usePCT)
{
  LOG_WARN << "StGenericVertexFinder::UsePCT() not implemented for this vertex finder." << endm;
  LOG_WARN << "StGenericVertexFinder::Expect Post-crossing tracks to be used by default in old finders." << endm;
}
//_____________________________________________________________________________
int StGenericVertexFinder::size() const
{
  return mVertexList.size();
}
//______________________________________________________________________________
StPrimaryVertex* StGenericVertexFinder::getVertex(int idx) const
{
   return (idx<(int)mVertexList.size())? (StPrimaryVertex*)(&(mVertexList[idx])) : 0;
}
//______________________________________________________________________________
void
StGenericVertexFinder::Clear()
{
  mVertexList.clear();
}



/**
 * Calculates chi^2 for the beamline and a point (xv, yv, zv) passed as input
 * argument.
 *
 * The beamline parameters are taken from this class static object sBeamline of
 * vertexSeed_st type which is initialized in UseVertexConstraint().
 *
 * The distance between the beamline and the point is experssed in terms of the
 * measured beamline parameters and the point's coordinates. We then calculate
 * the Jacobian for thus parameterized distance w.r.t. measured parameters, and
 * calculate the error on the distance itself by calculating a new covariance
 * matrix S' = JSJ^T. Note that in the representation along the line connecting
 * the poin with the beamline the covariance matrix has a dimension of 1.
 *
 * \author Dmitri Smirnov
 * \date April, 2016
 */
double StGenericVertexFinder::CalcBeamlineChi2(const StThreeVectorD& point)
{
   // Just for shorthand
   const vertexSeed_st& bl = sBeamline;

   double dx  = point.x() - bl.x0;
   double dy  = point.y() - bl.y0;
   double zv  = point.z();

   double kx  = bl.dxdz;
   double kx2 = kx*kx;
   double ky  = bl.dydz;
   double ky2 = ky*ky;

   double kx2_ky2_1  = kx2 + ky2 + 1;
   double ky_dy_zv   = ky*dy + zv;
   double kx_dx_zv   = kx*dx + zv;
   double ky_dy_zv_2 = ky_dy_zv*ky_dy_zv;
   double kx_dx_zv_2 = kx_dx_zv*kx_dx_zv;

   // The distance between the line and the point
   StThreeVectorD dist_vec (
      (  (ky2 + 1)*dx -     kx*ky*dy -          kx*zv)/kx2_ky2_1,
      (    - kx*ky*dx + (kx2 + 1)*dy -          ky*zv)/kx2_ky2_1,
      (       - kx*dx -        ky*dy + (kx2 + ky2)*zv)/kx2_ky2_1
   );

   double denom_sqrt = kx2_ky2_1 * sqrt( ( (kx*dy - ky*dx)*(kx*dy - ky*dx) + (ky*zv - dy)*(ky*zv - dy) + (kx*zv - dx)*(kx*zv - dx) ) /kx2_ky2_1);

   double denom = kx2_ky2_1 * denom_sqrt;

   // The Jacobian for the distance w.r.t. measured beamline parameters, i.e. x0, y0, kx, and ky
   double jacobian[4] = {
      ( -(ky2 + 1)*dx +     kx*ky*dy + kx*zv ) / denom_sqrt,
      (      kx*ky*dx - (kx2 + 1)*dy + ky*zv ) / denom_sqrt,

      ( kx*ky_dy_zv_2 - kx*(ky2 + 1)*dx*dx + (kx2 - ky2 - 1)*dx*ky_dy_zv ) / denom,
      ( ky*kx_dx_zv_2 - ky*(kx2 + 1)*dy*dy - (kx2 - ky2 + 1)*dy*kx_dx_zv ) / denom
   };

   // ... and the covariance matrix for the beamline parameters
   double variance_x0   = bl.err_x0*bl.err_x0;
   double variance_y0   = bl.err_y0*bl.err_y0;
   double variance_dxdz = bl.err_dxdz*bl.err_dxdz;
   double variance_dydz = bl.err_dydz*bl.err_dydz;

   double covBeamline[10] = {
     variance_x0,
               0, variance_y0,
               0,           0, variance_dxdz,
               0,           0,             0, variance_dydz
   };

   // Finaly, calculate the covariance matrix along the vector connecting the beamline and the point
   // The result is a 1x1 matrix
   TRSymMatrix covarianceMprime(TRMatrix(1, 4, jacobian), TRArray::kAxSxAT, TRSymMatrix(4, covBeamline) );

   double dist_mag = dist_vec.mag();
   double chi2 = dist_mag*dist_mag/covarianceMprime[0];

   return chi2;
}


//______________________________________________________________________________
void StGenericVertexFinder::NoVertexConstraint() 
{
  mVertexConstrain = false; 
  LOG_INFO << "StGenericVertexFinder::No Vertex Constraint" << endm;
}


/**
 * Stores beamline parameters (aka vertexSeed) from DB record in this class
 * static member sBeamline.
 */
void StGenericVertexFinder::UseVertexConstraint(const vertexSeed_st& beamline)
{
   sBeamline = beamline;

   sBeamline.err_x0 = std::max(0.01f, sBeamline.err_x0);
   sBeamline.err_y0 = std::max(0.01f, sBeamline.err_y0);

   // 0.0001f radians corresponds to a 0.1mm arc at 1m = 1000mm length
   sBeamline.err_dxdz = std::max(0.0001f, sBeamline.err_dxdz);
   sBeamline.err_dydz = std::max(0.0001f, sBeamline.err_dydz);

   LOG_INFO << "BeamLine constraint: weight =  " << sBeamline.weight << endm;
   LOG_INFO << "x(z) = (" << sBeamline.x0 << " +/- " << sBeamline.err_x0 << ") + (" << sBeamline.dxdz << " +/- " << sBeamline.err_dxdz << ") * z" << endm;
   LOG_INFO << "y(z) = (" << sBeamline.y0 << " +/- " << sBeamline.err_y0 << ") + (" << sBeamline.dydz << " +/- " << sBeamline.err_dydz << ") * z" << endm;
}


// $Log: StGenericVertexFinder.cxx,v $
// Revision 1.21  2016/04/12 19:48:47  smirnovd
// [Cosmetic] Prefixed included headers with path to corresponding module
//
// Revision 1.20  2016/04/11 20:53:20  smirnovd
// StGenericVertexFinder: Added static method to calculate chi2 for beamline and a point
//
// Revision 1.19  2016/04/11 20:53:13  smirnovd
// Use all available beamline (aka vertex seed) parameters from DB
//
// We overload StGenericVertexFinder::UseVertexConstraint for this puspose. The
// parameters are cached in static StGenericVertexFinder::sBeamline. Note that if
// there is a need to do so, UseVertexConstraint can do some preprocessing of the
// raw DB values before caching them.
//
// Revision 1.18  2016/04/11 20:44:26  smirnovd
// StGenericVertexFinder: Added static member to keep beamline parameters
//
// Revision 1.17  2016/02/29 22:58:22  jwebb
// Moved include of StEventTypes from header of generic class to implementation files of generic and concrete classes.
//
// Revision 1.16  2013/08/16 20:49:38  perev
// PPV with only StEvent dependency
//
// Revision 1.15  2010/09/10 21:06:45  rjreed
// Added function UseBOTF and bool mUseBtof to switch the use of the TOF on and off in vertex finding.  Default value is off (false).
//
// Revision 1.14  2009/11/11 03:52:14  genevb
// Re-order the vertices upon filling StEvent
//
// Revision 1.13  2008/10/23 20:37:31  genevb
// Add switches for turning on/off use of Post-Crossing Tracks [default:off]
//
// Revision 1.12  2006/05/04 20:01:30  jeromel
// Switched to logger
//
// Revision 1.11  2006/04/26 15:37:03  jeromel
// mVertexOrderMethod (To be tested)
//
// Revision 1.10  2006/04/08 00:18:09  mvl
// Added member for debuglevel
//
// Revision 1.9  2005/07/19 21:45:07  perev
// MultiVertex
//
// Revision 1.8  2005/07/14 15:39:22  balewski
// nothing, to force recompilation of this code by Autobuild
//
// Revision 1.7  2005/06/21 02:16:36  balewski
// multiple prim vertices are stored in StEvent
//
// Revision 1.6  2004/12/13 20:39:58  fisyak
// Add initaition of StGenericVertexFinder variables, replace mDumMaker by StMaker::GetChain() method
//
// Revision 1.5  2004/07/30 22:59:00  calderon
// Setting the primary vertex flag to 1 for the moment, as per
// dst_vertex.idl.  This was causing the FTPC code to reject the
// primary vertex used as their seed.
//
// Revision 1.4  2004/07/24 02:57:40  balewski
// clean up of ppLMV, CTB-util separated
//
// Revision 1.3  2004/07/23 00:57:43  jeromel
// Base class method implementation
//
// Revision 1.2  2004/04/06 02:43:43  lbarnby
// Fixed identification of bad seeds (no z~0 problem now). Better flagging. Message manager used.
//
// Revision 1.1  2003/05/09 22:22:46  lbarnby
// Initial revision: a base class for STAR (StEvent-based) vertex finders
//
