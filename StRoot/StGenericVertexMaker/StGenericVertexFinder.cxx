/***************************************************************************
 * $Id: StGenericVertexFinder.cxx,v 1.23 2016/04/20 22:03:45 smirnovd Exp $
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
#include "StEventTypes.h"
#include "StPrimaryVertex.h"
#include "StMessMgr.h"
#include "StMaker.h"
#include "StEventTypes.h"

// Initialize static variable with default values
vertexSeed_st StGenericVertexFinder::sBeamline;


//______________________________________________________________________________
StGenericVertexFinder::StGenericVertexFinder(VertexFit_t fitMode) :
  mVertexConstrain(false), mMode(0), mVertexFitMode(fitMode), mDebugLevel(0)
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
  mVertexFitMode = VertexFit_t::NoBeamline;
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
