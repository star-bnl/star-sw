/***************************************************************************
 * $Id: StGenericVertexFinder.cxx,v 1.42 2017/01/20 17:48:49 smirnovd Exp $
 *
 * Author: Lee Barnby, April 2003
 *
 ***************************************************************************
 * Description: Base class for vertex finders
 *
 ***************************************************************************/
#include <algorithm>
#include <cmath>

#include "TH1F.h"
#include "TSpectrum.h"

#include "StarRoot/TRMatrix.h"
#include "StarRoot/TRSymMatrix.h"
#include "StGenericVertexFinder.h"
#include "StMessMgr.h"
#include "StEvent/StDcaGeometry.h"
#include "StEventTypes.h"



// Initialize static variable with default values

/// Pointers to DCA states to be used in a vertex fit
StGenericVertexFinder::StDcaList&  StGenericVertexFinder::sDCAs()
{
   static StDcaList* sDCAs = new StDcaList();
   return *sDCAs;
}


/// All measured parameters of the beamline
vertexSeed_st StGenericVertexFinder::sBeamline;



StGenericVertexFinder::StGenericVertexFinder() :
  StGenericVertexFinder(SeedFinder_t::Unspecified, VertexFit_t::Unspecified)
{
}


//______________________________________________________________________________
StGenericVertexFinder::StGenericVertexFinder(SeedFinder_t seedFinder, VertexFit_t fitMode) :
  mVertexOrderMethod(orderByNumberOfDaughters),
  mVertexConstrain(false),
  mMode(0),
  mVertexFitMode(fitMode),
  mSeedFinderType(seedFinder),
  mDebugLevel(0),
  mUseBtof(false),
  mUseCtb(false)
{
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


/**
 * Searches for vertex seeds using the ROOT's TSpectrum peak finder applied to
 * track's DCA z distribution. This method can be transfered to a separate class
 * complying with a VertexSeedFinder interface (not available as of now).
 * Returns a vector of peak positions along the `z`. We copy the result to
 * a vector because TSpectrum::GetPositionX() may return either float* or
 * double* depending on ROOT version.
 */
std::vector<double> StGenericVertexFinder::FindSeeds_TSpectrum()
{
   TSpectrum tSpectrum(200);

   TH1F fVtx("fVtx", "z-dca distribution", 2500, -250, 250);

   // The size of window in cm where the probability is averaged
   static double zWindow = 2;

   for (const StDcaGeometry* trackDca : sDCAs())
   {
      double xyzp[6], covXyzp[21];

      trackDca->GetXYZ(xyzp, covXyzp);

      double offset = 0.5 * xyzp[5]/trackDca->pt();
      double sigmaZ = std::sqrt(covXyzp[5] + offset*offset);
      sigmaZ += fVtx.GetBinWidth(1);

      // Fill TSpectrum histogram with the average probability of having vertex
      // at z (bin center) given the tracks DCA Z coordinate (xyzp[2])
      int bin_first = fVtx.FindBin(xyzp[2] - 5*sigmaZ);
      int bin_last  = fVtx.FindBin(xyzp[2] + 5*sigmaZ);

      for (int iBin = bin_first; iBin <= bin_last; ++iBin)
      {
         double z = fVtx.GetBinCenter(iBin);
         fVtx.AddBinContent(iBin, (TMath::Erfc((z - xyzp[2] - zWindow)/sigmaZ) - TMath::Erfc((z - xyzp[2] + zWindow)/sigmaZ))/2.);
      }
   }

   int npeaks = tSpectrum.Search(&fVtx, 3, "nodraw", std::min(0.1, 5./sDCAs().size()) );

   auto* peaks = tSpectrum.GetPositionX();

   return std::vector<double>(peaks, peaks + npeaks);
}


//______________________________________________________________________________
void StGenericVertexFinder::addVertex(const StPrimaryVertex& vtx)
{
  mVertexList.push_back(vtx);
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


double StGenericVertexFinder::CalcChi2DCAs(const StThreeVectorD &point)
{
   static double scale = 100;

   // Initialize f with value for beamline
   double f = 0;

   for (const StDcaGeometry* dca : sDCAs())
   {
      double err2;
      double dist = dca->thelix().Dca( &point.x(), &err2);
      double chi2 = dist*dist/err2;

      f += scale*(1. - TMath::Exp(-chi2/scale)); // robust potential
   }

   return f;
}


double StGenericVertexFinder::CalcChi2DCAsBeamline(const StThreeVectorD &point)
{
   static double scale = 100;

   return CalcChi2DCAs(point) + scale*(1. - TMath::Exp(-CalcChi2Beamline(point)/scale));
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
double StGenericVertexFinder::CalcChi2Beamline(const StThreeVectorD& point)
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

   double denom_sqrt = kx2_ky2_1 * sqrt( ( (kx*dy - ky*dx)*(kx*dy - ky*dx) + (ky*zv - dy)*(ky*zv - dy) + (kx*zv - dx)*(kx*zv - dx) ) /kx2_ky2_1);

   // The denominator is zero when the point is exactly on the beamline
   // We just return a zero for the chi2 in this case. This makes sense for all
   // non-zero errors and if they are zero they are unphysical anyway.
   if (denom_sqrt == 0) return 0;

   // The distance between the line and the point
   StThreeVectorD dist_vec (
      (  (ky2 + 1)*dx -     kx*ky*dy -          kx*zv)/kx2_ky2_1,
      (    - kx*ky*dx + (kx2 + 1)*dy -          ky*zv)/kx2_ky2_1,
      (       - kx*dx -        ky*dy + (kx2 + ky2)*zv)/kx2_ky2_1
   );


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


/**
 * Estimates vertex position from track DCA states in sDCAs.
 * The beam position is not taken into account.
 *
 * \author Dmitri Smirnov
 * \date April 2016
 */
StThreeVectorD StGenericVertexFinder::CalcVertexSeed(const StDcaList &trackDcas)
{
   // Estimate new seed position using provided tracks
   StThreeVectorD vertexSeed(0, 0, 0);
   StThreeVectorD totalWeigth(0, 0, 0);

   if (trackDcas.size() == 0) {
      LOG_WARN << "StGenericVertexFinder::CalcVertexSeed: Empty container with track DCAs. "
                  "Returning default seed: StThreeVectorD(0, 0, 0)" << endm;
      return vertexSeed;
   }


   double xyzp[6], covXyzp[21];

   for (const StDcaGeometry* trackDca : trackDcas)
   {
      trackDca->GetXYZ(xyzp, covXyzp);

      double x_weight = 1./covXyzp[0];
      double y_weight = 1./covXyzp[2];
      double z_weight = 1./covXyzp[5];

      vertexSeed  += StThreeVectorD(xyzp[0]*x_weight, xyzp[1]*y_weight, xyzp[2]*z_weight);
      totalWeigth += StThreeVectorD(x_weight, y_weight, z_weight);
   }

   vertexSeed.set(vertexSeed.x()/totalWeigth.x(), vertexSeed.y()/totalWeigth.y(), vertexSeed.z()/totalWeigth.z());

   return vertexSeed;
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

   LOG_INFO << "BeamLine constraint: weight =  " << sBeamline.weight << "\n"
            << "x(z) = (" << sBeamline.x0   << " +/- max(0.01, "   << sBeamline.err_x0 << ") ) + "
            <<        "(" << sBeamline.dxdz << " +/- max(0.0001, " << sBeamline.err_dxdz << ") ) * z\n"
            << "y(z) = (" << sBeamline.y0   << " +/- max(0.01, "   << sBeamline.err_y0 << ") ) + "
            <<        "(" << sBeamline.dydz << " +/- max(0.0001, " << sBeamline.err_dydz << ") ) * z"
            << endm;

   sBeamline.err_x0 = std::max(0.01f, sBeamline.err_x0);
   sBeamline.err_y0 = std::max(0.01f, sBeamline.err_y0);

   // 0.0001f radians corresponds to a 0.1mm arc at 1m = 1000mm length
   sBeamline.err_dxdz = std::max(0.0001f, sBeamline.err_dxdz);
   sBeamline.err_dydz = std::max(0.0001f, sBeamline.err_dydz);

   UseVertexConstraint();
}


/**
 * Returns x coordinate on the beamline (given by sBeamline) corresponding to
 * the passed value of z.
 */
double StGenericVertexFinder::beamX(double z)
{
  return sBeamline.x0 + sBeamline.dxdz*z;
}


/**
 * Returns y coordinate on the beamline (given by sBeamline) corresponding to
 * the passed value of z.
 */
double StGenericVertexFinder::beamY(double z)
{
  return sBeamline.y0 + sBeamline.dydz*z;
}
