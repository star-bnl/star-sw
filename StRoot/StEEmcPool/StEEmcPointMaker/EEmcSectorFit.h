#ifndef __EEmcSectorFit_h__
#define __EEmcSectorFit_h__

#include "TMinuit.h"
#include <vector>
class TH1F;
class EEmcSectorFit : public TMinuit
{
public:
  /// Constructor
  /// \param maxGammas: Maximum number of gammas which we will attempt to fit.
  EEmcSectorFit(Int_t maxGammas=10);

  /// Destructor
  virtual ~EEmcSectorFit();

  /// print summary
  void print() const; 

  /// Set pointers to histograms which will be fit
  void SetHistograms(TH1F *u, TH1F *v){ mSMD[0]=u; mSMD[1]=v; }  

  /// Evaluate the N gamma fit for the specified plane
  Double_t FitFunc( Double_t x, Int_t plane ) const;

  /// Overrides TMinuit's chi^2 function
  /// \param np = number of parameters
  /// \param gr = array of derivitavies of chi^2 function
  /// \param x2 = the chi^2 value computed
  /// \param p  = array of parameters
  /// \param flg = flag (not used) 
  virtual Int_t Eval(Int_t np,Double_t* gr,Double_t& x2,Double_t* p,Int_t flg); 

  /// Returns the residual (data - fit) for the specified strip index.
  /// \param x: strip index, [0,288)
  /// \param plane: smd plane, 0=u, 1=v
  Double_t Residual( Int_t x, Int_t plane) const;
  /// Returns the residual (data - fit) for the specified strip index,
  /// summed over +/- dx strips.
  /// \param x: strip index, [0,288)
  /// \param plane: smd plane, 0=u, 1=v
  /// \param dx: number of strips on either side to sum over
  /// \param side: 0=both, 1=left, 2=right
  Double_t Residual( Int_t x, Int_t plane, Int_t dx, Int_t side=0) const; 

  /// Find maximum residual strip in specified plane.  Returns strip index.
  Int_t MaxStrip(Int_t plane) const;

  /// Add a candidate to the list of candidate gammas
  void AddCandidate( Double_t yield, Double_t sigma, Double_t u, Double_t v );

  /// Draws the current fit
  void Draw(Option_t *opts);

  /// Initialize parameters
  void InitParameters();

  /// Once the driving routine believes that all gammas have been found,
  /// we try all permutations of u,v pairs to find the true best fit.
  void TryPermutations();
  
  /// Clear the array of photon candidates
  void Clear(Option_t *opts="");

  /// Returns the number of gamma candidates
  Int_t numberOfCandidates(){ return (Int_t)yield.size(); }

  /// Returns the parameters of the fit to the ith gamma candidate
  void  GetCandidate( Int_t i, Double_t &nmips, Double_t &width, Double_t &u, Double_t &v ){ nmips=yield[i]; width=sigma[i]; u=umean[i]; v=vmean[i]; }
  /// Returns the parameters of the last candidate found
  void  GetLastCandidate( Double_t &nmips, Double_t &width, Double_t &u, Double_t &v ){ nmips=yield.back(); width=sigma.back(); u=umean.back(); v=vmean.back(); }
 
  /// Return the histogram for the specified plane
  TH1F *histo(Int_t plane) { return mSMD[plane]; }
  /// Return the chisquared of the fit
  Double_t chi2() const { return mChi2; }
  /// Return the number of degrees of freedom
  Int_t ndf() const { return mNDF; }  

  /// Adds TF1 to histogram
  void AddFits(TH1F *u, TH1F *v);

  /// Flag to determine if we test all permutations or not
  Bool_t doPermutations;

protected:

  /// The histograms we fit
  TH1F *mSMD[2];

  /// Yield of N gammas
  std::vector<Double_t> yield;
  /// Width of N gammas
  std::vector<Double_t> sigma;
  /// Mean U position of N gammas
  std::vector<Double_t> umean;
  /// Mean V position of N gammas
  std::vector<Double_t> vmean;

  /// Chi^2 of the fit
  Double_t mChi2;
  /// Degrees o' freedom
  Int_t mNDF; 

  /// MAkes class available to root
  ClassDef(EEmcSectorFit,1);

};

#endif
