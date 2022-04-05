// $Id: StFmsClusterFitter.cxx,v 1.10 2019/06/26 16:49:53 akio Exp $
//
// $Log: StFmsClusterFitter.cxx,v $
// Revision 1.10  2019/06/26 16:49:53  akio
// shower shape scaling for all shapes
//
// Revision 1.9  2018/03/26 15:55:37  akio
// removing some print outs
//
// Revision 1.8  2018/03/02 20:27:29  akio
// Big update from	Zhanwen Zhu with new shower shape and six z slices
//
// Revision 1.7  2018/01/04 17:35:44  smirnovd
// [Cosmetic] Remove StRoot/ from include path
//
// $STAR/StRoot is already in the default path search
//
// Revision 1.6  2015/11/05 17:54:57  akio
// Adding option to scale up shower shape function for large cells
//
// Revision 1.5  2015/10/30 21:33:56  akio
// fix parameter initialization
// adding new cluster categorization method
//
// Revision 1.4  2015/10/29 21:14:55  akio
// increase max number of clusters
// a bug fixes in valley tower association
// removing some debug comments
//
// Revision 1.3  2015/10/21 15:58:04  akio
// Code speed up (~x2) by optimizing minimization fuctions and showershape function
// Add option to merge small cells to large, so that it finds cluster at border
// Add option to perform 1photon fit when 2photon fit faield
// Add option to turn on/off global refit
// Moment analysis done without ECUTOFF when no tower in cluster exceed ECUTOFF=0.5GeV
//
// Revision 1.2  2015/09/02 15:01:32  akio
// Removing StFmsGeometry class, and now it uses StFmsDbMaker to get appropriate parameters.
//
// Revision 1.1  2015/03/10 14:38:54  jeromel
// First version of FmsUtil from Yuxi Pan - reviewd 2015/02
//
/**
 \file      StFmsClusterFitter.cxx
 \brief     Implementation of StFmsClusterFitter, shower-shape fitting routine
 \author    Steven Heppelmann <steveheppelmann@gmail.com>
 \author    Yuxi Pan <yuxipan@physics.ucla.edu>
 \author    Thomas Burton <tpb@bnl.gov>
 \date      2014
 \copyright Brookhaven National Lab
 */
#include "StFmsClusterFitter.h"
#include "StFmsDbMaker/StFmsDbMaker.h"

#include <algorithm>  // For std::max()
#include <array>
#include <cmath>
#include <numeric>
#include <vector>

#include "TF2.h"
#include "TMath.h"
#include "TString.h"

#include "St_base/StMessMgr.h"
#include "StEvent/StFmsHit.h"

//#include "StFmsGeometry.h"
#include "StFmsTower.h"
#include "StFmsConstant.h"

namespace {
const Int_t kMaxNPhotons = 7;  // Maximum number of photons that can be fitted


int mshowershapewithangle=1;// for static func
int mmerge=1;// for static func
int vertexz=0;
void setOption(int v1, int v2 , double v3){
     mshowershapewithangle=v1;
     mmerge=v2;	
     vertexz=v3;
}

//Yuxi's 
double unused=0.0;
double a11=0.998438; double a12=0.222782;  double a13=-0.22122;double b11=0.177028;double b12=0.000473222;double b13=0.178897;double w1 = 0.0372556;
double a21=1.07711 ; double a22=-0.0281385;double a23=-0.0489747;double b21=0.199964;double b22=3.5021;double b23=2.35246;double w2 = 0.202699;
double a31=1.07901 ; double a32=0.0650143; double a33=-0.144025;double b31=0.446845;double b32=0.00544512;double b33=1.64565;double w3 = 0.293878;
double a41=0.922174; double a42=0.0778254; double a43=1.07474e-07;double b41=0.593804;double b42=0.6199;double b43=3.49798;double w4 = 0.236854;
double a51=0.999849; double a52=0.000151185;double a53=2.20244e-07;double b51=0.949953;double b52=1.84451;double b53=3.40799;double w5 = 0.146041;
double a61=0.997454; double a62=0.00254497;double a63=1.02127e-06;double b61=1.43387;double b62=2.91155;double b63=3.4484;double w6 = 0.0832717;


//Zhanwen's small 45GeV new
double a1S[6]={0.0303644,0.212191,0.277429,0.0370035,0.0524404,0.00844062};
double a2S[6]={0.00122867,0.105355,0.10538,0.152656,0.00664331,0.0108688};
double b1S[6]={0.403493,0.514546,0.672826,1.82344,0.727991,1.48785};
double b2S[6]={0.270492,0.514593,0.672655,0.644871,4.32003,0.25};


//large 45GeV new
double a1L[6]={0.0275364,0.200363,0.277157,0.0391611,0.0590757,0.0101089};
double a2L[6]={0.000429808,0.0991777,0.104781,0.161916,0.00764026,0.012653};
double b1L[6]={0.515974,0.661722,0.865167,2.35237,0.932038,1.87933};
double b2L[6]={0.53531,0.661519,0.865226,0.828017,5.49041,0.321139};

double a1[6]={0. , 0  ,  0. , 0. , 0. , 0. };
double a2[6]={0. , 0  ,  0. , 0. , 0. , 0. };
double b1[6]={0. , 0. ,  0. , 0. , 0. , 0. };
double b2[6]={0. , 0. ,  0. , 0. , 0. , 0. };
double a3[6]={0. , 0  ,  0. , 0. , 0. , 0. };
double b3[6]={0. , 0. ,  0. , 0. , 0. , 0. };
double w[6]={1. , 1. ,  1. , 1. , 1. , 1. };

std::array<double,60 > fitParameters;
void clear_fitParameters(){
  for (int  i=0 ; i<60 ; i++) {  fitParameters[i]=0 ;  }
}

void choose_fitParameters(int detID, float scl=1.0){
    if (detID<=9) {unused=5;} //large cell
    else          {unused=3;} //small cell
    if ( mshowershapewithangle==2 ) {
	fitParameters= {unused, a11, a12, a13, b11*scl, b12*scl, b13*scl, w1 ,unused, unused,
			unused, a21, a22, a23, b21*scl, b22*scl, b23*scl, w2 ,unused, unused,
			unused, a31, a32, a33, b31*scl, b32*scl, b33*scl, w3 ,unused, unused,
			unused, a41, a42, a43, b41*scl, b42*scl, b43*scl, w4 ,unused, unused,
			unused, a51, a52, a53, b51*scl, b52*scl, b53*scl, w5 ,unused, unused,
			unused, a61, a62, a63, b61*scl, b62*scl, b63*scl, w6 ,unused, unused          };
    } else if ( mshowershapewithangle==1 ) {
        if (detID>9) {//small cell
	    for (int i=0 ; i<6 ; i++) {
                a1[i]=a1S[i];
                a2[i]=a2S[i];
                b1[i]=b1S[i]*scl;
                b2[i]=b2S[i]*scl;
	    }	    
        }else{//large cell
	    for (int i=0 ; i<6 ; i++) {
                a1[i]=a1L[i];
                a2[i]=a2L[i];
                b1[i]=b1L[i]*scl;
                b2[i]=b2L[i]*scl;
	    }
	}	
	fitParameters= {  unused, a1[0], a2[0], a3[0], b1[0], b2[0], b3[0], w[0] ,unused, unused,
			  unused, a1[1], a2[1], a3[1], b1[1], b2[1], b3[1], w[1] ,unused, unused,
			  unused, a1[2], a2[2], a3[2], b1[2], b2[2], b3[2], w[2] ,unused, unused,
			  unused, a1[3], a2[3], a3[3], b1[3], b2[3], b3[3], w[3] ,unused, unused,
			  unused, a1[4], a2[4], a3[4], b1[4], b2[4], b3[4], w[4] ,unused, unused,
			  unused, a1[5], a2[5], a3[5], b1[5], b2[5], b3[5], w[5] ,unused, unused};	
    }else if( mshowershapewithangle==0 ) {
	fitParameters[1] =  SS_A1;
	fitParameters[2] =  SS_A2;
	fitParameters[3] =  SS_A3;
	fitParameters[4] =  SS_B1 * scl;
	fitParameters[5] =  SS_B2 * scl;
	fitParameters[6] =  SS_B3 * scl;	
    }
}

TF2 showerShapeFitFunction("showerShapeFitFunction",
                       &FMSCluster::StFmsClusterFitter::energyDepositionInTower,
                      -25.0, 25.0, -25.0, 25.0, fitParameters.size());
/*
 Compose Minuit step size in each fit variable.
 The first value is for the number of photons.
 Each subsequent triplet is for the (x, y, E) of a photon, up to kMaxNPhotons.
 */
std::vector<double> defaultMinuitStepSizes() {
  std::vector<double> steps(1, 0.);  // Initialise with nPhoton step
  // Append default (x, y, E) steps for each photon
  for (int i(0); i < kMaxNPhotons; ++i) {
    steps.insert(steps.end(), {0.1, 0.1, 0.2});
  }  // for
  return steps;
}

std::vector<double> towerWidths;  // Tower (x, y) width in cm

// Helper function for accumulating tower energies
double addTowerEnergy(double energy, const FMSCluster::StFmsTower* tower) {
  return energy + tower->hit()->energy();
}

}  // unnamed namespace

namespace FMSCluster {
// Instantiate static members
StFmsTowerCluster::Towers* StFmsClusterFitter::mTowers(nullptr);
Double_t StFmsClusterFitter::mEnergySum(0.0);

StFmsClusterFitter::StFmsClusterFitter( //const StFmsGeometry* geometry,
         Int_t detectorId, Float_t xw, Float_t yw, 
	 Float_t scaleShowerShapeLarge , Float_t scaleShowerShapeSmall,
	 Int_t ShowerShapeWithAngle, Int_t MergeSmallToLarge, double vertexZ)
    : mMinuit(3 * kMaxNPhotons + 1), 
      mScaleShowerShapeLarge(scaleShowerShapeLarge), mScaleShowerShapeSmall(scaleShowerShapeSmall),
      mShowerShapeWithAngle(ShowerShapeWithAngle) , mMergeSmallToLarge(MergeSmallToLarge){
  // Set tower (x, y) widths for this detector
  towerWidths.clear();
  towerWidths.push_back(xw);
  towerWidths.push_back(yw);

  setOption(mShowerShapeWithAngle,mMergeSmallToLarge ,vertexZ);
  clear_fitParameters();
  choose_fitParameters(detectorId);
  showerShapeFitFunction.SetParameters(fitParameters.data());
  mMinuit.SetPrintLevel(-1);  // Quiet, including suppression of warnings (change to 0 for more info)
}

StFmsClusterFitter::~StFmsClusterFitter() { }

TF2* StFmsClusterFitter::showerShapeFunction() {
  return &showerShapeFitFunction;
}

Double_t StFmsClusterFitter::fitNPhoton(const std::vector<double>& parameters,
                                        const std::vector<double>& steps,
                                        const std::vector<double>& lower,
                                        const std::vector<double>& upper,
                                        PhotonList* photons) {
  Double_t chiSquare(-1.);  // Return value
  // Check that there is a pointer to TObjArray of towers
  if (!StFmsClusterFitter::mTowers) {
    LOG_ERROR << "no tower data available! return -1!" << endm;
    return chiSquare;
  }  // if
  mMinuit.SetFCN(minimizationFunctionNPhoton);
  int nPhotons = parameters.size() / 3;
  if (nPhotons < 1 || nPhotons > kMaxNPhotons) {
    LOG_ERROR << "Number of photons must be between 1 and " << kMaxNPhotons <<
      "not " << nPhotons << " for fit. Setting it to be 1..." << endm;
    nPhotons = 1;
  }  // if
  mMinuit.mncler();  // Clear old parameters so we can define the new parameters
  // The first parameter tells Minuit how many photons to fit!
  // It should be a fixed parameter, and between 1 and the max number of photons
  setMinuitParameter(0, "nph", parameters, steps, lower, upper);
  // Set the rest of parameters: 3 parameters per photon
  for (Int_t i = 0; i < nPhotons; i++) {
    Int_t j = 3 * i + 1;  // Need to set 3 parameters per photon
    setMinuitParameter(j++, Form("x%d", i), parameters, steps, lower, upper);
    setMinuitParameter(j++, Form("y%d", i), parameters, steps, lower, upper);
    setMinuitParameter(j++, Form("E%d", i), parameters, steps, lower, upper);
  }  // if
  
  //ZZ never change energy!!!!!
  if (nPhotons==1) {mMinuit.FixParameter(3) ; } // fix Energy during 1P fit
  if (nPhotons==2) {mMinuit.FixParameter(3) ; mMinuit.FixParameter(6);  } // fix Energy during 2P fit

   runMinuitMinimization();
  // Populate the list of photons from the fit results
  if (mMinuit.GetStatus() == 0) {
    // Get the fit results and errors
    std::vector<double> params(parameters.size(), 0.);
    std::vector<double> errors(parameters.size(), 0.);
    readMinuitParameters(params, errors);
    for (unsigned i(1); i < parameters.size(); i += 3) {
      photons->emplace_back(params.at(i), params.at(i + 1), params.at(i + 2));
    }  // for
    // Evaluate chi-square (*not* chi-square per degree of freedom)
    mMinuit.Eval(photons->size(), nullptr, chiSquare, params.data(), 1);
  }  // for
  return chiSquare;
}

Double_t StFmsClusterFitter::fitNPhoton(const std::vector<double>& parameters,
                                        const std::vector<double>& lower,
                                        const std::vector<double>& upper,
                                        PhotonList* photons) {
  return fitNPhoton(parameters, defaultMinuitStepSizes(),
                    lower, upper, photons);
}

/*
 A different set of parameters for 2-photon clusters only:
  0: still a constant parameter, should be set to 2 for 2-photon fitting
  1: xPi, x-position of pi^0
  2: yPi, y-position of pi^0
  3: d_gg, distance between 2 photons
  4: theta, angle of displacement vector from photon 2 to photon 1
  5: z_gg, can go from -1 to +1, so we do not set E1 > E2
  6: E_gg, total energy of two photons
 Thus, in the more conventional fitNPhoton() parameterization:
  E1 = E_gg * (1 + z_gg) / 2
  E2 = E_gg * (1 - z_gg) / 2
  x1 = xPi + cos(theta) * d_gg * (1 - z_gg) / 2
  y1 = yPi + sin(theta) * d_gg * (1 - z_gg) / 2
  x2 = xPi - cos(theta) * d_gg * (1 + z_gg) / 2
  y2 = yPi - sin(theta) * d_gg * (1 + z_gg) / 2

 The advantage of this parameterization is that for 2-photon cluster fitting
 we can ensure that the two photons never get to close. The old parameterization
 suffers from this shortcoming if we let the parameters vary freely.

 What we already know about the limits of the new parameters:
  xPi and yPi: rarely do they go beyond 0.3 unit of lgd
  theta:       have a narrow theta range (for r = sigmaMax / sigmaMax,
               |theta| < 0.5 * r / 0.65 when r < 0.65, and linear increase
               from 0.5 to pi/2 for 0.65 < r < 1)
  E_gg:        given by Ec (+/- 20% or less)
  z_gg:        should just let it vary from -1 to 1
  d_gg:        a lower bound is given by r = sqrt(sigmaX^2 + sigmaY^2). 
               d_gg > Max(2.5 * (r - 0.6), 0.5)
 */
Int_t StFmsClusterFitter::fit2Photon(const std::array<double, 7>& parameters,
                                     const std::array<double, 7>& steps,
                                     const std::array<double, 7>& lower,
                                     const std::array<double, 7>& upper,
                                     PhotonList* photons) {
  Double_t chiSquare(-1.);  // Return value
  if (!StFmsClusterFitter::mTowers) {
    LOG_ERROR << "no tower data available! return -1!" << endm;
    return chiSquare;
  }  // if
  mMinuit.SetFCN(minimizationFunction2Photon);
  mMinuit.mncler();  // Clear old parameters so we can define the new parameters
  const std::vector<TString> names = {
    "nph", "xPi", "yPi", "d_gg", "theta", "z_gg", "E_gg"
  };
  for (unsigned i = 0; i < names.size(); ++i) {
    setMinuitParameter(i, names.at(i), parameters, steps, lower, upper);
  }  // for
  // Fix E_total and theta, we don't want these to be free parameters
  mMinuit.FixParameter(4);
  mMinuit.FixParameter(6);
  runMinuitMinimization();
  if (mMinuit.GetStatus() == 0) {
    // Get the fit results for starting positions and errors
    // 3 * nPhotons + 1 parameters = 7 for 2 photons
    std::vector<double> param(parameters.size(), 0.);
    std::vector<double> error(parameters.size(), 0.);
    readMinuitParameters(param, error);
    // Put the fit result back in "clust". Need to translate the special
    // parameters for 2-photon fit into x, y, E, which looks a bit complicated!
    // First photon
    double x = param[1] + cos(param[4]) * param[3] * (1 - param[5]) / 2.0;
    double y = param[2] + sin(param[4]) * param[3] * (1 - param[5]) / 2.0;
    double E = param[6] * (1 + param[5]) / 2.0;
    photons->emplace_back(x, y, E);
    // Second photon
    x = param[1] - cos(param[4]) * param[3] * (1 + param[5]) / 2.0;
    y = param[2] - sin(param[4]) * param[3] * (1 + param[5]) / 2.0;
    E = param[6] * (1 - param[5]) / 2.0;
    photons->emplace_back(x, y, E);
    // Evaluate the chi-square function
    mMinuit.Eval(7, nullptr, chiSquare, param.data(), 1);
  }  // if
  return chiSquare;
}

Double_t StFmsClusterFitter::energyDepositionInTower(Double_t* xy,                                                     
						     Double_t* parameters) {
    // Calculate the energy deposited in a tower by evaluating
    // energyDepositionDistribution() at x+/-d/2 and y+/-d/2, for tower
    // width d. The double-loop below is equivalent to
    // F(x+d/2, y+d/2) + F(x-d/2, y-d/2) - F(x-d/2, y+d/2) - F(x+d/2, y-d/2)
    Double_t energy=0.0;

    if(mshowershapewithangle==0){
	double w = parameters[0];
	for (int ix = 0; ix < 2; ++ix) {
	    for (int iy = 0; iy < 2; ++iy) {
		double signX = std::pow(-1., ix);  // 1 or -1
		double signY = std::pow(-1., iy);  // 1 or -1
		std::array<double, 2> s{ {xy[0] + signX * w / 2.,    // x +/- d/2
			                  xy[1] + signY * w / 2.} }; // y +/- d/2
		energy += signX * signY * energyDepositionDistribution(s.data(),
								       parameters);
	    }  // for
	}  // for
        //cout<<"we are calling the OLD energyDepositionInTower(Double_t* xy,Double_t* parameters"<<endl;
    }else{
	Double_t *Zc;
        Double_t ZcS[6] = {720.45,727.95,735.45,742.95,750.45,757.95};
        Double_t ZcL[6] = {722.98,733.01,743.04,753.07,763.10,773.13};	
	Double_t Zmax,xoff,yoff;
        if (parameters[0]>4) { 
	    yoff=98.8; Zmax=735.45; xoff=0.3; Zc=ZcL;
        }else{
	    yoff=46.5; Zmax=735.45; xoff=0.93; Zc=ZcS;
        }
        if (mmerge >0) yoff=98.8;  // large cells always 98.8

        Double_t tany = ( yoff - xy[3]) / (Zmax - vertexz); 
        Double_t tanx = ( xy[1]- xoff ) / (Zmax - vertexz); //ZZ just in case     

        for(Int_t i = 0; i < 6; i++){
	    Double_t xc = xy[1] + tanx*(Zc[i] - Zmax);
	    Double_t yc = xy[3] - tany*(Zc[i] - Zmax); //large (positive) tany corresponds to smaller row # (yc)
	    Int_t istart = i*10;
	    energy += energyDepositionInTowerSingleLayer(xy[0]-xc,xy[2]-yc,&parameters[istart])  *  parameters[istart+7]    ;
        }
    }    
    return energy;
}

int StFmsClusterFitter::maxNFittedPhotons() {
  return kMaxNPhotons;
}

int StFmsClusterFitter::runMinuitMinimization() {
  std::vector<double> arguments = {1000., 1.};  // Max calls and tolerance
  int errorFlag = -1;
  mMinuit.mnexcm("MIGRAD", arguments.data(), arguments.size(), errorFlag);
  // Free fixed parameters before next use of mMinuit. Wrap in if() to avoid
  // noisy warning messages in case of no fixed parameters.
  if (mMinuit.GetNumFixedPars() > 0) {
    mMinuit.mnfree(0);
  }  // if
  return errorFlag;
}

// Calculate fractional photon energy deposition in a tower based on its (x, y)
// position relative to the tower center
Double_t StFmsClusterFitter::energyDepositionDistribution(
    Double_t* xy,
    Double_t* parameters) {
    double f = 0;
    // The parameter array has 10 elements, but we only use 6 here
    // 1 to 6 are a1, a2, a3, b1, b2, b3 as defined in
    // https://drupal.star.bnl.gov/STAR/blog/leun/2010/aug/02/fms-meeting-20100802
    for (int i = 1; i < 4; i++) {  // 1, 2, 3
	f += showerShapeComponent(xy[0], xy[1], parameters[i], parameters[i + 3]);
    }  // for
    return f / TMath::TwoPi();;
}

// Uses the signature needed for TMinuit interface:
// http://root.cern.ch/root/htmldoc/TMinuit.html#TMinuit:SetFCN
void StFmsClusterFitter::minimizationFunctionNPhoton(Int_t& npara,
                                                     Double_t* grad,
                                                     Double_t& fval,
                                                     Double_t* para,
                                                     Int_t /* not used */) {
  //mEnergySum was already calcurated in setTowers()
  //const double energySum = std::accumulate(mTowers->begin(), mTowers->end(),0., addTowerEnergy);
  fval = 0;  // Stores sum of chi2 over each tower
  const int nPhotons = static_cast<int>(para[0]);
  //  for (auto i = mTowers->begin(); i != mTowers->end(); ++i) {
  for (auto i = mTowers->begin(), e = mTowers->end(); i!=e; ++i){
    const StFmsTower* tower = *i;
    // The shower shape function expects the centers of towers in units of cm
    // Tower centers are stored in row/column i.e. local coordinates
    // Therefore convert to cm, remembering to subtract 0.5 from row/column to
    // get centres not edges
    /* Stop getting xy here. Already in tower StFmsTower class
    Double_t x, y;
    if(tower->hit()->detectorId()<10){
        x = tower->column() - 0.5;
        y = tower->row() - 0.5;
    }else{
        x = (tower->column()-0.5)*2.0/3.0;
        y = (tower->row() - 0.5)*2.0/3.0 + 9.0;
    }
    //const double x = towerWidths.at(0) * (tower->column() - 0.5);
    //const double y = towerWidths.at(1) * (tower->row() - 0.5);
    x *= towerWidths.at(0);
    y *= towerWidths.at(1);
    */
    Double_t x=tower->x();
    Double_t y=tower->y();
    fitParameters.front() = tower->w(); 
    // Add expected energy in tower from each photon, according to shower-shape
    double expected = 0;
    for (int j = 0; j < nPhotons; ++j) {  // Recall there are 3 paras per photon
	int k = 3 * j;

        if(mshowershapewithangle>0 ){   expected += para[k + 3] *
            energyDepositionInTower(x , y ,para[k+1] , para[k+2], fitParameters.data(),mmerge,vertexz); 
        }
        if(mshowershapewithangle==0 ){  expected += para[k + 3] *
            energyDepositionInTowerOLD(x - para[k + 1], y - para[k + 2], fitParameters.data());
        }

    }
    //const double measured = tower->hit()->energy();
    const double measured = tower->e();
    const double deviation = measured - expected;
    // Larisa's chi2 function definition
    /*
    const Double_t err = 0.03 *
                         pow(measured / energySum, 1. - 0.001 * energySum) *
                         pow(1 - measured / energySum, 1. - 0.007 * energySum) *
                         energySum + 0.01;
    */
    const Double_t ratio = measured / mEnergySum;
    // const double err = 0.03 * measured * (1 - ratio) + 0.01;
    const Double_t err = 0.03 *
                         pow(ratio, 1. - 0.001 * mEnergySum) *
                         pow(1. - ratio, 1. - 0.007 * mEnergySum) *
                         mEnergySum + 0.01;
    //fval += pow(deviation, 2.) / err;
    fval += deviation * deviation / err;
    //cout << Form("n=%1d x=%6.2f y=%6.2f e=%6.2f exp=%6.2f diff=%6.2f sum=%6.2f err=%6.2f fval=%6.2f",
    //             nPhotons,x,y,measured,expected,deviation,mEnergySum,err,deviation*deviation/err)<<endl;
  }  // for
  fval = std::max(fval, 0.);  // require that the fraction be positive
  //cout << Form("nPhotons=%d fval=%6.2f",nPhotons,fval)<<endl;
}

// Uses the signature needed for TMinuit interface:
// http://root.cern.ch/root/htmldoc/TMinuit.html#TMinuit:SetFCN
void StFmsClusterFitter::minimizationFunction2Photon(Int_t& nparam,
                                                     Double_t* grad,
                                                     Double_t& fval,
                                                     Double_t* param,
                                                     Int_t /* not used */) {
  // Only need to translate into the old parameterization
  const double dgg   = param[3];
  const double zgg   = param[5];
  const double angle = param[4];
  std::array<double, 7> oldParam{ {
    param[0],  // Number of photons, unchanged
    param[1] + cos(angle) * dgg * (1 - zgg) / 2.0,  // x 1
    param[2] + sin(angle) * dgg * (1 - zgg) / 2.0,  // y 1
    param[6] * (1 + zgg) / 2.0,                     // Energy 1
    param[1] - cos(angle) * dgg * (1 + zgg) / 2.0,  // x 2
    param[2] - sin(angle) * dgg * (1 + zgg) / 2.0,  // y 2
    param[6] * (1 - zgg) / 2.0                      // Energy 2
  } };
  // Now call the regular minimization function with the translated parameters
  minimizationFunctionNPhoton(nparam, grad, fval, oldParam.data(), 0);
}

template<class Container>
int StFmsClusterFitter::setMinuitParameter(int index, const TString& name,
                                           const Container& params,
                                           const Container& steps,
                                           const Container& lower,
                                           const Container& upper) {
  int error = 0;
  mMinuit.mnparm(index, name, params.at(index), steps.at(index),
                 lower.at(index), upper.at(index), error);
  return error;
}

int StFmsClusterFitter::readMinuitParameters(std::vector<double>& parameters,
                                             std::vector<double>& errors) {
  errors.resize(parameters.size(), 0.);
  for (int i = 0, size = parameters.size(); i != size; ++i) {
    mMinuit.GetParameter(i, parameters.at(i), errors.at(i));
  }  // for
  return parameters.size();
}

void StFmsClusterFitter::setTowers(StFmsTowerCluster::Towers* towers) { 
    mTowers = towers; 
    mEnergySum = std::accumulate(mTowers->begin(), mTowers->end(), 0., addTowerEnergy);
    //if mScaleShowerShape is on, and if top cell is in large cell, scale shower shape up    
    //if(mScaleShowerShape ==1 && mShowerShapeWithAngle==0){
    int d=mTowers->front()->hit()->detectorId();
    if(d <= 9){ 
	choose_fitParameters(d,mScaleShowerShapeLarge);
    }else{
	choose_fitParameters(d,mScaleShowerShapeSmall);
    }
    showerShapeFitFunction.SetParameters(fitParameters.data());
}
    
}  // namespace FMSCluster
