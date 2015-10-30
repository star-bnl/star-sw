// $Id: StFmsClusterFitter.h,v 1.5 2015/10/29 21:14:55 akio Exp $
//
// $Log: StFmsClusterFitter.h,v $
// Revision 1.5  2015/10/29 21:14:55  akio
// increase max number of clusters
// a bug fixes in valley tower association
// removing some debug comments
//
// Revision 1.4  2015/10/23 15:05:07  akio
// fixed constexpr
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
// Revision 1.1  2015/03/10 14:38:53  jeromel
// First version of FmsUtil from Yuxi Pan - reviewd 2015/02
//
/**
 \file      StFmsClusterFitter.h
 \brief     Declaration of StFmsClusterFitter, shower-shape fitting routine
 \author    Steven Heppelmann <steveheppelmann@gmail.com>
 \author    Yuxi Pan <yuxipan@physics.ucla.edu>
 \author    Thomas Burton <tpb@bnl.gov>
 \date      2014
 \copyright Brookhaven National Lab
 */
#ifndef STROOT_STFMSPOINTMAKER_STFMSCLUSTERFITTER_H_
#define STROOT_STFMSPOINTMAKER_STFMSCLUSTERFITTER_H_

#ifndef __CINT__  // Hide std::array from CINT, as CINT cannot parse C++11
#include <array>
#endif  // __CINT__
#include <list>
#include <vector>

#include "TMinuit.h"
#include "TObject.h"

#include "StFmsFittedPhoton.h"
#include "StFmsTowerCluster.h"

class TF2;
class TString;

namespace FMSCluster {  // $NMSPC
typedef std::list<StFmsFittedPhoton> PhotonList;
  //class StFmsGeometry;
class StFmsTower;
/**
 Photon shower-shape fitting routine for FMS clusters.

 Fits tower clusters with the photon energy-deposition shower-shape
 distribution function i.e. how a photon hitting the FMS distributes its energy
 over towers.
 This serves to:
  - find the best-fit photon properties for a cluster.
  - distinguish clusters made by a single photon from those made by two
    nearby photons merging into a single cluster, which can frequently happen
    at the far-forward rapidity of the FMS.

 \todo
 It may be safer to make StFmsClusterFitter a singleton class, or something like that.
 The shower shape fit function is shared across all objects (by necessity, in
 order to interface with TMinuit), but each object updates the function itself
 with different parameters. Therefore bad things would happen if there were
 more than one object in existence at any time. There isn't ever more than one
 instance created in this code, but I think it would be good to enforce that.
 */
class StFmsClusterFitter : public TObject {
 public:
  /** Constructor using detector geometry for a single sub-detector */
  StFmsClusterFitter(/*const StFmsGeometry* geometry,*/ Int_t detectorId, Float_t xw, Float_t yw);
  /**
   Default constructor.

   \todo Actually initialize things!
   */
  StFmsClusterFitter() { }
  /** Destructor */
  virtual ~StFmsClusterFitter();
  /**
   Return the shower shape function.
   
   The shower shape gives the fractional energy deposition by a photon in a
   tower as a function of the distance of the photon from the tower center.
   */
  TF2* showerShapeFunction();
  /** Set the tower list to fit when calling fitNPhoton() or fit2Photon() */
  void setTowers(StFmsTowerCluster::Towers* towers);
  /**
   Fit photons to the list of towers.

   All array arguments are of size 3N+1 for an N-photon fit:
    - par: initial guess values for each fit variable.
    - step: step size when fitting (if empty use default values).
    - low: lower bound on each fit variable.
    - up: upper bound on each fit variable.

   In each 3N+1D array, the first element is the number of photons to fit.
   Each subsequent triplet is the x position, y position and energy of a photon.
   e.g.
    - for a 1-photon fit: [1, x0, y0, E0]
    - for a 2-photon fit: [2, x0, y0, E0, x1, y1, E1]

   Returns the &chi;<sup>2</sup> of the fit.
   */
  Double_t fitNPhoton(const std::vector<double>& parameteres,
                      const std::vector<double>& steps,
                      const std::vector<double>& lower,
                      const std::vector<double>& up,
                      PhotonList* photons);
  /** Calls fitNPhoton() with default step sizes */
  Double_t fitNPhoton(const std::vector<double>& parameters,
                      const std::vector<double>& lower,
                      const std::vector<double>& up,
                      PhotonList* photons);
  /**
   Specialized fit function for exactly 2-photon fit.

   Argument meanings are as for fitNPhoton().
   However as this is for 2-photon fits only, the input arrays should always
   have 7 (3 * 2 photons + 1) elements. Additionally each element has a
   different meaning here:
    - 0: Still a constant parameter, should be set to 2 for 2-photon fitting.
    - 1: x-position of pi<sup>0</sup>.
    - 2: y-position of pi<sup>0</sup>.
    - 3: Distance between 2 photons.
    - 4: Theta angle of displacement vector from photon 2 to photon 1.
    - 5: z_gg, from -1 to +1, so we do not set E1 > E2.
    - 6: Total energy of two photons.

   Returns the &chi;<sup>2</sup> of the fit.
   */
#ifndef __CINT__  // Hide std::array from CINT, as CINT cannot parse C++11
  Int_t fit2Photon(const std::array<double, 7>& parameters,
                   const std::array<double, 7>& steps,
                   const std::array<double, 7>& lower,
                   const std::array<double, 7>& upper,
                   PhotonList* photons);
#endif  // __CINT__
  /**
   Energy-deposition shower-shape function, for use with a TF2.

   Yields the fraction of energy deposited in a tower by a photon, as a function
   of the distance from the photon to the tower center.
   Arguments:
    - x: array with x[0] = x distance from tower center, x[1] = y distance.
    - par: array of fit parameters (fixed, derived from FMS studies).

   Integrates F(x, y) over a tower, with F(x, y) defined as here:
   https://drupal.star.bnl.gov/STAR/blog/leun/2010/aug/02/fms-meeting-20100802

   \todo Provide LaTeX math function in documentation
   */
  static inline Double_t energyDepositionInTower(Double_t x, Double_t y, Double_t* par);
  static Double_t energyDepositionInTower(Double_t* x, Double_t* par);
  /** Maximum number of photons that can be fit at once. */
  static int maxNFittedPhotons();

 private:
  /** Disallow copy construction. */
  StFmsClusterFitter(const StFmsClusterFitter&);
  /** Disallow assignment. */
  StFmsClusterFitter& operator=(const StFmsClusterFitter&);
  /**
   Run Minuit MIGRAD function minimization.

   Frees any previously fixed parameters after minimization finishes.
   Returns the Minuit mnexcm() error flag.
   0 indicates success; for other values see
   http://root.cern.ch/root/htmldoc/TMinuit.html#TMinuit:mnexcm
   */
  int runMinuitMinimization();
  /**
   Shower shape helper function.

   Evaluates F(x,y) as defined here:
   https://drupal.star.bnl.gov/STAR/blog/leun/2010/aug/02/fms-meeting-20100802

   Used by energyDepositionInTower() to integrate F(x,y) over a tower.

   \todo Provide LaTeX math function in documentation
   */
  static inline Double_t energyDepositionDistribution(Double_t x, Double_t y, Double_t* par);
  static Double_t energyDepositionDistribution(Double_t* x, Double_t* par);
  /**
   Minuit minimization function for fitNPhoton() routine.
   
   For the purpose of this function and a description of its arguments see
   https://wwwasdoc.web.cern.ch/wwwasdoc/minuit/node14.html

   For its use in ROOT via TMinuit see
   http://root.cern.ch/root/htmldoc/TMinuit.html#TMinuit:SetFCN
   */
  static void minimizationFunctionNPhoton(Int_t& npar, Double_t* grad,
                                          Double_t& fval, Double_t* par,
                                          Int_t iflag);
  /**
   Minuit minimization function for fit2Photon() routine
   
   Also see comments for minimizationFunction().
   */
  static void minimizationFunction2Photon(Int_t& nparam, Double_t* grad,
                                          Double_t& fval, Double_t* param,
                                          Int_t iflag);
  /**
   Sets the nth Minuit fit parameter.

   See fitNPhoton() for the meaning of the vector arguments.
   The Container type must support an at() method to access elements (e.g. STL
   vector, deque, array).
   Returns the Minuit error flag.
   */
  template<class Container>
  int setMinuitParameter(int index, const TString& name,
                         const Container& parameters,
                         const Container& steps,
                         const Container& lower,
                         const Container& upper);
  /**
   Reads Minuit parameters and errors.

   The number of parameters read is determined by the size() of the parameters
   array.

   Returns the number of parameters read.
   */
  int readMinuitParameters(std::vector<double>& parameters,
                           std::vector<double>& errors);
  TMinuit mMinuit;  ///< Minuit fitting interface
  static StFmsTowerCluster::Towers* mTowers;  ///< List of towers to fit
  static Double_t mEnergySum;

  ClassDef(StFmsClusterFitter, 0)
};  // class StFmsClusterFitter

inline double asmsqrt(double x) { //slower than std::sqrt()                                                                                                                       
    __asm__ ("fsqrt" : "+t" (x));
    return x;
}

// Returns a * f(x,y,b) as defined here:
// https://drupal.star.bnl.gov/STAR/blog/leun/2010/aug/02/fms-meeting-20100802
inline double showerShapeComponent(double x, double y, double a, double b) {
    return a * atan(x * y / (b * sqrt(b * b + x * x + y * y)));
}

inline Double_t StFmsClusterFitter::energyDepositionDistribution(double x, double y, double* parameters){

    constexpr double ootwopi = 1.0/2.0/3.14159265358979323846;

    return (     showerShapeComponent(x, y, parameters[1], parameters[4])
	       + showerShapeComponent(x, y, parameters[2], parameters[5])
	       + showerShapeComponent(x, y, parameters[3], parameters[6]) ) * ootwopi;
}

inline Double_t StFmsClusterFitter::energyDepositionInTower(Double_t x, Double_t y, Double_t* parameters){
    const double w = parameters[0]/2.0;
    return  energyDepositionDistribution(x-w,y-w,parameters)
          - energyDepositionDistribution(x-w,y+w,parameters)
          - energyDepositionDistribution(x+w,y-w,parameters)
          + energyDepositionDistribution(x+w,y+w,parameters);
}


}  // namespace FMSCluster
#endif  // STROOT_STFMSPOINTMAKER_STFMSCLUSTERFITTER_H_
