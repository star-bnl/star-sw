// $Id: StFtpcSlowSimCluster.cc,v 1.8 2003/07/03 13:28:59 fsimon Exp $
// $Log: StFtpcSlowSimCluster.cc,v $
// Revision 1.8  2003/07/03 13:28:59  fsimon
// Functionality for cathode offset simulation: Additional parameters for
// StFtpcSlowSimField::GetVelocityZ
//
// Revision 1.7  2003/02/14 16:56:56  fsimon
// Add functionality that allows for different temperature corrections
// in west and east, important for embedding. StFtpcSlowSimField now
// has to be called with the full padrow (0 to 19), to be able to
// select east/west.
//
// Revision 1.6  2002/06/07 09:55:39  fsimon
// Additional debug info to trace electron drift
//
// Revision 1.5  2002/04/19 22:24:12  perev
// fixes for ROOT/3.02.07
//
// Revision 1.4  2001/04/02 12:04:31  jcs
// get FTPC calibrations,geometry from MySQL database and code parameters from StarDb/ftpc
//
// Revision 1.3  2001/03/19 15:53:10  jcs
// use ftpcDimensions from database
//
// Revision 1.2  2001/03/06 23:35:55  jcs
// use database instead of params
//
// Revision 1.1  2000/11/23 10:16:43  hummler
// New FTPC slow simulator in pure maker form
//
//
///////////////////////////////////////////////////////////////////////////
//  Author: W.G.Gong
//  Email: gong@mppmu.mpg.de
//  Date:  Oct 25, 1996
///////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include "StFtpcSlowSimCluster.hh"
#include "StFtpcSlowSimField.hh"
#include "StFtpcClusterMaker/StFtpcParamReader.hh"
#include "StFtpcClusterMaker/StFtpcDbReader.hh"

StFtpcSlowSimCluster::StFtpcSlowSimCluster(StFtpcParamReader *paramReader,
                                           StFtpcDbReader *dbReader,
					   StFtpcSlowSimField *field,
					   const float el, const float rad_offset,
					   const float pad_offset, const float r,
					   const float ph, const float time,
					   const int call_padrow)
{
  outerRadius=dbReader->sensitiveVolumeOuterRadius();
  mIntDiffCoarseness=paramReader->diffusionCoarseness();
  mFlDiffCoarseness=(float) mIntDiffCoarseness;
  electron     = el;
  radialDipWidth      = rad_offset;
  azimuthalCrossWidth      = pad_offset;
  sigma_rad_squared    = 0;
  sigma_phi_squared    = 0;
  currentRadius          = r;
  currentPhi          = ph;
  drift_time   = time;

  original_padrow = call_padrow;  //needed for debug info

  padrow=call_padrow;
  /*  // Now, the full padrow is needed to correct for different temperatures! Done in StFtpcSlowSimField
  if(padrow>=10)
    padrow -= 10;
  // only absolute padrow in one chamber is needed for the field
  */
  deltaRadius = field->GetDeltaRadius();
  twoDeltaRadius = field->GetTwoDeltaRadius();
  electronLoss = -deltaRadius * dbReader->gasAttenuation();

}

StFtpcSlowSimCluster::~StFtpcSlowSimCluster() {}

void StFtpcSlowSimCluster::DriftDiffuse(StFtpcSlowSimField *field)
{


  //cout << "DriftDiffuse: Padrow: "<<original_padrow<<" Drift r = "<<currentRadius<<" ; phi = "<<currentPhi;
  int i=0;
  while(currentRadius < outerRadius )
    {
      float inverseRadius = 1/currentRadius;
      float deltaRadiusRelative = deltaRadius * inverseRadius;

      int     index = field->GetGridIndex(currentRadius);

      // deflection angle
      float lorentz, inverseVelocity;
      field->GetVelocityZ(inverseRadius, padrow, currentPhi, &inverseVelocity, &lorentz);
      currentPhi += deltaRadiusRelative * lorentz;

      // accumulative drift time
      drift_time   += deltaRadius * inverseVelocity;

      // calculate diffusion in coarser steps than the cluster center
      if(i==mIntDiffCoarseness)
	{
	  // longitudinal diffusion + velocity gradience
	  sigma_rad_squared = field->GetDiffusionZSqr(index)
	    *deltaRadius*mFlDiffCoarseness+
	    sigma_rad_squared*(1.+field->GetDlnvDr(index)
			       *twoDeltaRadius*mFlDiffCoarseness);

	  // transverse  diffusion + radius divergence
	  float divergenceFactor= 1.+deltaRadiusRelative*mFlDiffCoarseness;
	  sigma_phi_squared =field->GetDiffusionXSqr(index)
	    *mFlDiffCoarseness*deltaRadius +
	    sigma_phi_squared*divergenceFactor*divergenceFactor;

	  // attenuation
	  electron += mFlDiffCoarseness*electronLoss;
	  i=1;
	}
      else
	i++;
      // next step
      currentRadius += deltaRadius;
    }
  //cout <<" ====> r = "<<currentRadius<<" ; phi = "<<currentPhi<<endl;

}

void StFtpcSlowSimCluster::Print() const
{
    cout << " Cluster parameter : " << endl;
    cout << "number of electrons = " << electron << endl;
    cout << "sigma_rad [um]      = " << sqrt(sigma_rad_squared) << endl;
    cout << "sigma_phi [um]      = " << sqrt(sigma_phi_squared) << endl;
    cout << "currentPhi [deg]   = " << currentPhi << endl;
    cout << "currentRadius [cm]    = " << currentRadius << endl;
    cout << "Td [usec]   = " << drift_time << endl;
}
