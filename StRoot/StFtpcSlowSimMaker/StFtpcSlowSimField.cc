// $Id: StFtpcSlowSimField.cc,v 1.16 2007/05/15 14:35:18 jcs Exp $
// $Log: StFtpcSlowSimField.cc,v $
// Revision 1.16  2007/05/15 14:35:18  jcs
// update to be compatible with changes made to StFtpcTrackParams.cc
// use default microsecondsPerTimebin value from database if no RHIC clock info available
//
// Revision 1.15  2003/10/07 14:01:48  jcs
// remove double Stiostream.h include
//
// Revision 1.14  2003/09/02 17:58:16  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.13  2003/07/03 13:30:54  fsimon
// Implementation of cathode offset simulation:
// 	The inner radius (and thus the E-field) is changed according to
// 	phi of the cluster and the size of the offset.
// 	GetVelocityZ not inline anymore, since it got quite big
//
// Revision 1.12  2003/02/14 16:58:03  fsimon
// Add functionality that allows for different temperature corrections
// in west and east, important for embedding. Drift tables are created
// for east and west seperately.
//
// Revision 1.11  2002/09/13 13:44:55  fsimon
// Commented out anglefactor
//
// Revision 1.10  2002/04/19 22:24:12  perev
// fixes for ROOT/3.02.07
//
// Revision 1.9  2001/04/27 13:19:00  jcs
// cleanup comments
//
// Revision 1.8  2001/04/25 17:52:04  perev
// HPcorrs
//
// Revision 1.7  2001/04/24 12:54:59  oldi
// mParam->slowSimPressure() used instead of mParam->normalizedNowPressure().
//
// Revision 1.6  2001/04/02 12:04:33  jcs
// get FTPC calibrations,geometry from MySQL database and code parameters from StarDb/ftpc
//
// Revision 1.5  2001/03/19 15:53:10  jcs
// use ftpcDimensions from database
//
// Revision 1.4  2001/03/06 23:36:01  jcs
// use database instead of params
//
// Revision 1.3  2001/01/11 18:28:44  jcs
// use PhysicalConstants.h instead of math.h, remove print statement
//
// Revision 1.2  2000/11/27 14:08:00  hummler
// inplement tzero and lorentz angle correction factor
//
// Revision 1.1  2000/11/23 10:16:43  hummler
// New FTPC slow simulator in pure maker form
//
//
///////////////////////////////////////////////////////////////////////////
//  Author: W.G.Gong
//  Email: gong@mppmu.mpg.de
//  Date:  Oct 25, 1996
//
//  Modifications:
//          02/27/98    Janet Seyboth   correct definition of new arrays
///////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include "PhysicalConstants.h"

#include "StFtpcSlowSimField.hh"
#include "StFtpcClusterMaker/StFtpcParamReader.hh"
#include "StFtpcClusterMaker/StFtpcDbReader.hh"
#include "TMath.h"

StFtpcSlowSimField::StFtpcSlowSimField(StFtpcParamReader *paramReader,
                                       StFtpcDbReader *dbReader)
{
  mParam=paramReader;
  mDb = dbReader;
  // inplementation of drift information from fcl_padtrans, which is
  // more precise than that in ftpcSlowSimGas (includes calculated magfield)
  // there's a job to do: fuse both tables into one!
  innerRadius = mDb->sensitiveVolumeInnerRadius();
  outerRadius = mDb->sensitiveVolumeOuterRadius();
  angleFactor = mParam->lorentzAngleFactor();
  
  grid_point = new grid_data[mParam->numSlowSimGridPoints()];

  int i;
  float cathodeVoltage = mParam->chamberCathodeVoltage();

  // grid size
  del_r = (outerRadius - innerRadius)
    / (float) (mParam->numSlowSimGridPoints()-1);
  inverseDeltaRadius = 1/del_r; // '*' faster than '/' in later calculations
  twoDeltaRadius = 2*del_r;

  int num_data = mParam->numberOfFssGasValues();    // number of data point
  float * in_efield  = new float[num_data];
  float * in_velocity_z = new float[num_data];
  float * in_diffusion_z = new float[num_data];
  float * in_diffusion_x = new float[num_data];
  float * in_diffusion_y = new float[num_data];
  float * in_angle_lorentz = new float[num_data];
  for ( i=0; i<num_data; i++) {
    in_efield[i]        = mParam->fssGasEField(i);
    in_velocity_z[i]    = mParam->fssGasVDrift(i);
    in_diffusion_z[i]   = mParam->fssGasDiffusionZ(i);
    in_diffusion_x[i]   = mParam->fssGasDiffusionX(i);
    in_diffusion_y[i]   = mParam->fssGasDiffusionY(i);
    in_angle_lorentz[i] = mParam->fssGasLorentzAngle(i);
  }

  float eff;
  // big loop to define the field
  for ( i=0; i<mParam->numSlowSimGridPoints(); i++) {

    grid_point[i].rhit = innerRadius + i * del_r;

    eff = fabs(cathodeVoltage) / ::log(outerRadius / innerRadius)
      / grid_point[i].rhit;
    grid_point[i].ef   = eff;

    int index = Locate(num_data, in_efield, eff);

    grid_point[i].vel_z
      = Interpolate(num_data,in_efield,in_velocity_z,index,eff);
    float diffusionX=Interpolate(num_data,in_efield,in_diffusion_x,
				 index,eff);
    float diffusionY=Interpolate(num_data,in_efield,in_diffusion_y,
				 index,eff);
    float diffusionZ=Interpolate(num_data,in_efield,in_diffusion_z,
				 index,eff);
    grid_point[i].diff_z
      = diffusionZ*diffusionZ;
    grid_point[i].diff_x
      = diffusionX*diffusionY;
  }

  // calculate drift velocity gradients

  for (i=mParam->numSlowSimGridPoints()-1; i>0; i--) {
    grid_point[i].dlnv_dr
      = ( 1. - grid_point[i-1].vel_z/grid_point[i].vel_z)
      *inverseDeltaRadius;
  }
  grid_point[0].dlnv_dr = grid_point[1].dlnv_dr;
  delete [] in_efield;
  delete [] in_velocity_z;
  delete [] in_diffusion_z;
  delete [] in_diffusion_x;
  delete [] in_diffusion_y;
  delete [] in_angle_lorentz;

  radTimesField = mDb->radiusTimesField();
  nPadrowPositions = mDb->numberOfPadrowsPerSide();
  nMagboltzBins = mDb->numberOfMagboltzBins();
  preciseEField = new float[nMagboltzBins];
  inverseDriftVelocityWest = new float[nMagboltzBins*nPadrowPositions];
  preciseLorentzAngleWest = new float[nMagboltzBins*nPadrowPositions];
  inverseDriftVelocityEast = new float[nMagboltzBins*nPadrowPositions];
  preciseLorentzAngleEast = new float[nMagboltzBins*nPadrowPositions];

  // use pressure and temperature corrections for each FTPC seperately

  // calculate drift velocity and lorentz angle for west

  // use temperature corrected pressure (set in StFtpcSlowSimMaker
  //float deltaP = mParam->slowSimPressure()-mParam->standardPressure();
  float deltaP = mParam->adjustedAirPressureWest() - mParam->standardPressure();

  {for(int i=0; i<nMagboltzBins; i++)
    {
      preciseEField[i] = mDb->magboltzEField(i);
      for(int j=0; j<nPadrowPositions; j++)
	{
	  inverseDriftVelocityWest[i+nMagboltzBins*j] =
	    1/ (mDb->magboltzVDrift(i,j)
		+ deltaP * mDb->magboltzdVDriftdP(i,j));
	  preciseLorentzAngleWest[i+nMagboltzBins*j] =
	    mDb->magboltzDeflection(i,j)
	    + deltaP * mDb->magboltzdDeflectiondP(i,j);
	}
    }}

   // calculate drift velocity and lorentz angle for east
  deltaP = mParam->adjustedAirPressureEast() - mParam->standardPressure();

  {for(int i=0; i<nMagboltzBins; i++)
    {
      for(int j=0; j<nPadrowPositions; j++)
	{
	  inverseDriftVelocityEast[i+nMagboltzBins*j] =
	    1/ (mDb->magboltzVDrift(i,j)
		+ deltaP * mDb->magboltzdVDriftdP(i,j));
	  preciseLorentzAngleEast[i+nMagboltzBins*j] =
	    mDb->magboltzDeflection(i,j)
	    + deltaP * mDb->magboltzdDeflectiondP(i,j);
	}
    }}
  EFieldMin=preciseEField[0];
  EFieldStep=preciseEField[1]-EFieldMin;
  EFieldStepInverted= 1/EFieldStep;
  EFieldStepInvConverted= EFieldStepInverted * degree;
  finalVelocity = grid_point[mParam->numSlowSimGridPoints()-1].vel_z*10.;

  // fill variables for cathode offset
  mOffsetCathodeWest = mDb->offsetCathodeWest();
  mOffsetCathodeEast = mDb->offsetCathodeEast();
  mAngleOffsetWest = mDb->angleOffsetWest();
  mAngleOffsetEast = mDb->angleOffsetEast();


}

StFtpcSlowSimField::~StFtpcSlowSimField() {
  delete[] preciseEField;
  delete[] inverseDriftVelocityWest;
  delete[] preciseLorentzAngleWest;
  delete[] inverseDriftVelocityEast;
  delete[] preciseLorentzAngleEast;
  delete[] grid_point;
}

float StFtpcSlowSimField::Interpolate(const int npt, const float* x,
				      const float* y,const int ich,
				      const float xx)
{
//
// this subroutine will Interpolate the value (xx,yy) from
// the arrays {x(1:npt), y(1:npt)} at a given channel ich.
//
    float x1[5];
    float y1[5];
    register int i;

    if (ich < 2) {
        for ( i=0; i<2; i++) {
            x1[i] = *(x+i) ;
            y1[i] = *(y+i) ;
        }
        return  InterpValue(2, x1, y1, xx);
    }
    else if (ich > npt-3) {
        for ( i=0; i<2; i++) {
            x1[i] = *(x+npt-1-i) ;
            y1[i] = *(y+npt-1-i) ;
        }
        return  InterpValue(2, x1, y1, xx);
    }
    else {
        for ( i=0; i<5; i++) {
            x1[i] = *(x+ich-2+i) ;
            y1[i] = *(y+ich-2+i) ;
        }
        return  InterpValue(5, x1, y1, xx);
    }
}

float StFtpcSlowSimField::InterpValue(const int npt, const float* x, 
				      const float* y, const float xx)
{
//
// use Newton's method to Interpolate value at x
//
    float term;
    float sum = 0;

    for(register int i=0; i < npt; i++) {
        term = y[i];
        for(register int j=0; j < npt; j++) {
            if (j != i) term *= (xx-x[j])/(x[i]-x[j]);
        }
        sum += term;
    }
    return sum;
}



void StFtpcSlowSimField::GetVelocityZ(const float inverseRadius, const int padrow, const float phi, float *inverseVelocity, float *angle)
{
     
  // move cathode (standard: along x in pos direction
  float xOff = 0;
  float angleOff;
  float newR;
  float phis = phi - TMath::Pi()/2; // to get phi to run from 0 to 2 Pi

  if (padrow < 10) { 
    xOff = mOffsetCathodeWest;
    angleOff = -mAngleOffsetWest;  // this sign is not tested! Up to now, the Offset is 0!
    phis -= TMath::Pi()/2;
  }
  else {
    xOff = mOffsetCathodeEast;
    angleOff = -mAngleOffsetEast;
  }
      
  
  if (xOff != 0) {
    // rotate offset
    phis -= angleOff;
    if (phis < 0) phis += 2*TMath::Pi();
    if (phis > 2*TMath::Pi()) phis -= 2*TMath::Pi();
    
    if (phis > TMath::Pi()) phis = 2 * TMath::Pi() - phis;
    
    if (phis == 0) 
      newR = innerRadius + xOff;
    else if (phis == TMath::Pi())
      newR = innerRadius - xOff;
    else {
      float asinSum = TMath::ASin(xOff/innerRadius * TMath::Sin(phis));
      if (asinSum < 0 ) asinSum = asinSum* (-1);
      newR = innerRadius/TMath::Sin(phis) * TMath::Sin(TMath::Pi() - phis - asinSum);
    }
  }
  else
    newR = innerRadius;
  
  int fieldPadrow = padrow;
  if (padrow >= 10) fieldPadrow = padrow - 10; // bField symmetric, no diff east/west !
  //float e_now=radTimesField * inverseRadius;
  // e - Field corrected for changed cathode:
  float e_now=radTimesField * inverseRadius * (::log(outerRadius / (innerRadius))/::log(outerRadius / newR));
  int iLower= (int)((e_now-EFieldMin)*EFieldStepInverted);
  int iUpper= iLower + 1;
  int padrowIndex= nMagboltzBins*fieldPadrow;
  float diffUp=preciseEField[iUpper]-e_now;
  float diffDown=e_now-preciseEField[iLower];
  iLower+=padrowIndex;
  iUpper+=padrowIndex;
  if (padrow < 10) {//west
    *inverseVelocity = EFieldStepInverted*((inverseDriftVelocityWest[iUpper])*diffDown + (inverseDriftVelocityWest[iLower])*diffUp);
    *angle = EFieldStepInvConverted*((preciseLorentzAngleWest[iUpper])*diffDown + (preciseLorentzAngleWest[iLower])*diffUp);//*angleFactor;
  }
  else {
    *inverseVelocity = EFieldStepInverted*((inverseDriftVelocityEast[iUpper])*diffDown + (inverseDriftVelocityEast[iLower])*diffUp);
    *angle = EFieldStepInvConverted*((preciseLorentzAngleEast[iUpper])*diffDown + (preciseLorentzAngleEast[iLower])*diffUp);//*angleFactor;
  }
}



void StFtpcSlowSimField::Output() const
{
    ofstream fout("field.dat");
    char space = ' ';
    // print out the field value to check
    for (int  i=0; i<mParam->numSlowSimGridPoints(); i++) {
       fout << grid_point[i].rhit << space 
            << grid_point[i].ef << space 
            << grid_point[i].vel_z << space
            << grid_point[i].diff_z<< space 
            << grid_point[i].diff_x<< space 
            << grid_point[i].lorentz/degree << space 
            << grid_point[i].dlnv_dr 
		 *mParam->lorentzAngleFactor()
            << endl;
    }
}

