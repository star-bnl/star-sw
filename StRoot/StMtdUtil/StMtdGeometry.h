#ifndef StMtdGeometry_HH
#define StMtdGeometry_HH

#include "TMath.h"
#include "TNamed.h"

const int kMaxMem = 10000;
const double muonMass= 0.105658389;
const double mEMCenergyloss=0.007511;

//------------------------//
// Geometry constant
//------------------------//
const int nTrays = 5;
const int nChannels = 24;
const int nStrips = 12;
const double mtdRadius=403.60; 
const double rModuleDiff=4.9911; 
const double innerSteelRadius = 303.29; //inner radius of magnetic steel
const double outerSteelRadius = 364.25;
const double innerEMCRadius = 225.505;
const double innerBSMDRadius=230.705;
const double outerBSMDRadius=232.742;
const double stripLength = 87.0; //cm
const double stripWidth  = 3.8 ; //cm
const double stripGap    = 0.4 ; //cm
const double bFieldInSteelFF = -1.26;
const double bFieldFF = 0.5;
const double eLossInSteel = 0.074;
const int nStep = 10;
const double backLegPhiWidth = 8.*(TMath::Pi())/180.; //rad,  8 degree per backLeg
const double backLegPhiGap   = 4.*(TMath::Pi())/180.; //rad,  4 degree 
const double mFirstBackLegPhi = 90.*(TMath::Pi())/180.; // rad, No.1 backLeg located at phi=90 degree, id from 1-30.
const double vDrift = 56.; // ps/cm drifting velocity of electronic signal

//TOF
const double tofRadius=210.968+3.6; //cm
const double mTofPadWidth = 3.45; //cm
const double mTofTrayZ0[120]={
	0.456712,0.666261, 0.71768, 1.00654, 1.21264,0.504901,0.062785,0.350721,0.375286,0.313752,
	0.562889,-0.180609,0.370083, -0.0504299,0.301356,0.150828,0.150644,-0.285295,0.278636,0.314912,
		0.251707,0.262374,0.271368,0.695487,0.927029,0.899819,0.492237,0.720113, 1.06218, 1.03113,
		0.868579, 1.62435,0.104284, 1.93179,-0.139964,0.647967, 1.57783, 1.58271, 1.94232, 1.46022,
		0.659128, 1.76036, 1.73066,0.949713,0.905353,0.756498,0.210295,0.470087,0.810709, 1.08613,
		0.940598, 1.10454, 1.15685, 1.07045,0.708036,0.421542,0.601668,0.579751, 1.03112,0.573439,
		0.983376,0.960392,0.763747, 1.07456,0.892364,0.431232,0.693468, 1.34398,0.994126,0.365718,
		0.489378, 0.26762,-0.0343417,-0.220991,-0.594403,0.117464,-0.0186761,-0.49274,0.143013, 1.28602,
		2.15682, 1.71213, 1.71793, 1.14863, 1.28236,0.867462, 1.45821, 1.03668,  1.3141,0.824919,
		0.675452,0.734607, 1.18578, 1.65205, 1.49599, 1.96678, 1.06956, 2.00802, 1.59186, 1.25303,
		1.39352, 0, 1.63993, 1.60719, 1.12031, 1.49205, 1.33758, 1.47987, 1.65667, 1.15155,
		0.794811, 1.08626,0.734363, 1.16125, 1.07977, 1.10196, 1.45764, 1.68846, 1.08673,0.920625
};

const double mTofModuleLocalZ[32]={
5.43, 11.42, 17.71, 23.56, 29.96, 35.66, 42.71, 49.04,
55.35, 61.65, 67.99, 74.42, 80.91, 87.44, 93.93, 100.53,
107.30,114.17,121.22,128.29,135.52,142.80,150.19,157.72,
165.34,173.07,180.92,188.93,197.02,205.24,213.61,222.08 };

class StMtdGeometry : public TNamed{
 public:
   StMtdGeometry(const char* name="mtdGeo",
                  const char* title="Simplified Mtd Geometry");
   ~StMtdGeometry();
	const char *GetCVS() const
		{static const char cvs[]="Tag $Name:  $ $Id: StMtdGeometry.h,v 1.1 2013/03/21 11:23:17 jeromel Exp $ built "__DATE__" "__TIME__ ; return cvs;}
};

R__EXTERN  StMtdGeometry* gMtdGeometry;

#endif
