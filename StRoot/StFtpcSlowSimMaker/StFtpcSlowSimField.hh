// $Id: StFtpcSlowSimField.hh,v 1.7 2003/09/02 17:58:16 perev Exp $
// $Log: StFtpcSlowSimField.hh,v $
// Revision 1.7  2003/09/02 17:58:16  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.6  2003/07/03 13:30:56  fsimon
// Implementation of cathode offset simulation:
// 	The inner radius (and thus the E-field) is changed according to
// 	phi of the cluster and the size of the offset.
// 	GetVelocityZ not inline anymore, since it got quite big
//
// Revision 1.5  2003/02/14 16:58:03  fsimon
// Add functionality that allows for different temperature corrections
// in west and east, important for embedding. Drift tables are created
// for east and west seperately.
//
// Revision 1.4  2002/09/13 13:45:23  fsimon
// Commented out anglefactor
//
// Revision 1.3  2001/03/06 23:36:05  jcs
// use database instead of params
//
// Revision 1.2  2000/11/27 14:08:04  hummler
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
///////////////////////////////////////////////////////////////////////////
#ifndef STAR_StFtpcSlowSimField
#define STAR_StFtpcSlowSimField

class Inparam;
class StFtpcParamReader;
class StFtpcDbReader;
extern  int Locate(const int npt, const float* x, const float xx);

//
//  class field  will divide radius into grid points and
//  define efield and gas parameters at grid points.
//
class StFtpcSlowSimField
{
public:
  StFtpcSlowSimField(StFtpcParamReader *paramReader,
                     StFtpcDbReader    *dbReader);
  ~StFtpcSlowSimField();
  float Interpolate(const int npt, const float* x,
		    const float* y,const int ich,
		    const float xx);
  float InterpValue(const int npt, const float* x,
		    const float* y, const float xx);
  void Output() const;     // write out the field value for checking

  int GetGridIndex(const float r) const
    { return ((int) ((r - innerRadius)*inverseDeltaRadius) ) ; }

  // GetVelocityZ is inline although too long because it is called in the
  // main drift loop
  void GetVelocityZ(const float inverseRadius, const int padrow, const float phi, float *inverseVelocity, float *angle);

  float GetVelAtReadout() const { return  finalVelocity; }

  float GetDeltaRadius() const { return del_r; }

  float GetInverseDeltaRadius() const { return inverseDeltaRadius; }

  float GetTwoDeltaRadius() const { return twoDeltaRadius; }

  float GetDiffusionXSqr(const int i) const {return grid_point[i].diff_x;}

  float GetDiffusionZSqr(const int i) const {return grid_point[i].diff_z;}

  float GetDlnvDr(const int i) const {return grid_point[i].dlnv_dr;}

private:
  StFtpcParamReader *mParam;
  StFtpcDbReader *mDb;
  // variable declarations
  float del_r;             // radial grid size
  float inverseDeltaRadius; // 1/del_r
  float twoDeltaRadius; // 2*del_r
  float finalVelocity;
  // variables to handle fcl_padtrans information:
  int nMagboltzBins;
  int nPadrowPositions;
  float EFieldMin;
  float EFieldStep;
  float EFieldStepInverted;
  float EFieldStepInvConverted; // include pi/180
  float radTimesField;
  float innerRadius;
  float outerRadius;
  float *preciseEField;
  // DV and LA needed for east & west seperately to permit different t corrections
  float *inverseDriftVelocityWest;
  float *preciseLorentzAngleWest;
  float *inverseDriftVelocityEast;
  float *preciseLorentzAngleEast;
  float angleFactor;

  // variables for cathode offset simulation
  float mOffsetCathodeWest;
  float mOffsetCathodeEast;
  float mAngleOffsetWest;
  float mAngleOffsetEast;

  struct grid_data {
    float rhit;    // hit radius in cm
    float ef;    // Efield in V/cm
    float vel_z;    // long.  drift speed in cm/usec
    float diff_z;    // long. diffusion coeff. in um/::sqrt(cm)
    float diff_x;    // trans. diffusion coef. in um/::sqrt(cm)
    float diff_y;    // trans. diffusion coef. in um/::sqrt(cm)
    float lorentz;    // Lorentz angle in rad
    float dlnv_dr;    // drift velocity gradient
  };
  grid_data *grid_point;
};

#endif

