/// -*- mode: C++ -*-

///
/// \author Pibero Djawotho <pibero@indiana.edu>
/// \author Indiana University
/// \date 7 July 2007
///
/// \class StGammaFitter
/// \brief Computes maximal sided residual of SMD response in u- and v-plane
/// for gamma candidate.
///
/// This class is based on C++ code developed by Jason Webb from the original
/// FORTRAN code by Les Bland. The algorithm follows the steps below:
///
/// 1. The SMD response, which is SMD strips with hits in MeV, in each plane (U and V)
/// is stored in histogram hU and hV.
/// 2. Fit functions fU and fV are created. The functional form of the SMD peak is
/// a double-Gaussian with common mean and fixed widths. The widths were obtained by
/// fitting the SMD response of single photons from the EEMC slow simulator. As such,
/// the only free parameters are the common mean and the total yield.
/// The actual formula used is:
///
/// [0]*(0.69*exp(-0.5*((x-[1])/0.87)**2)/(sqrt(2*pi)*0.87)+0.31*exp(-0.5*((x-[1])/3.3)**2)/(sqrt(2*pi)*3.3))
///
/// 3. The centroid of the gamma candidate is used to determine the tower at that
/// position. The fitting range will be restricted to those SMD strips under the
/// tower.
/// 4. Guesses used in fit for the mean and yield are the strip with the highest signal
/// and the integral within +/- 2 strips of the mean, respectively.
/// 5. After obtaining the fit, the residual for each side of the peak is calculated
/// by subtracting the fit from the data (residual = data - fit) from 2 strips beyond
/// the mean out to 40 strips.
/// 6. The maximal sided residual is simply the greater residual of each side.
///

#ifndef ST_GAMMA_FITTER_H
#define ST_GAMMA_FITTER_H

class TH1;
class TF1;

class StGammaCandidate;
class StGammaFitterResult;

#include "TObject.h"

class StGammaFitter : public TObject {
public:
  /// \brief Destructor.
  ~StGammaFitter();

  /// \brief Access to single instance of this singleton class.
  /// \return Pointer to single instance of this singleton class.
  static StGammaFitter* instance();

  /// \brief Fit transverse SMD profile to predetermined peak in u- and v-plane.
  /// \param candidate cluster to be fitted in the SMD plane.
  /// \param u holds the result of the fit in the SMD u-plane.
  /// \param v holds the result of the fit in the SMD v-plane.
  /// \return True if fit was successful, false otherwise.
  bool fitSector(StGammaCandidate* candidate, StGammaFitterResult* u, StGammaFitterResult* v);

protected:
  /// \brief Constructor in protected section to prevent user from creating
  /// instances of this singleton class. Use instance() instead.
  StGammaFitter() {}

private:
  /// \brief Single instance of this singleton class.
  static StGammaFitter* mInstance;

  /// \brief Fit transverse SMD profile stored in h1 to peak stored in f1.
  /// \param h1 is the 1D histogram containing the response of the SMD plane
  /// \param f1 is the 1D function used to fit the response of the SMD plane
  /// \return True if the fit was successful, false otherwise.
  bool fitSector(TH1& h1, TF1& f1);

  /// \brief Computes maximal sided fit residual.
  /// \param h1 is the 1D histogram containing the response of the SMD plane
  /// \param f1 is the 1D function used to fit the response of the SMD plane
  /// \return Maximaml sided fit residual from h1 fitted with f1
  float residual(TH1& h1, TF1& f1);

  /// \brief Convert tower id to sector, subsector, etabin, and phibin.
  /// \param id of tower
  /// \param sector [0-12[
  /// \param subsector [0-5[
  /// \param etabin [0-12[
  /// \param phibin [0-60[
  void getSectorSubEtaPhiFromTowerId(int id, int& sector, int& subsector, int& etabin, int& phibin);

  ClassDef(StGammaFitter, 1);
};

#endif
