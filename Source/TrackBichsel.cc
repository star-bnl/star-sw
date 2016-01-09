#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <algorithm>

#include "Sensor.hh"
#include "TrackBichsel.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"

namespace Garfield {

TrackBichsel::TrackBichsel()
    : m_bg(3.16228),
      m_speed(SpeedOfLight * m_bg / sqrt(1. + m_bg * m_bg)),
      m_x(0.),
      m_y(0.),
      m_z(0.),
      m_t(0.),
      m_dx(0.),
      m_dy(0.),
      m_dz(1.),
      m_imfp(4.05090e4),
      m_datafile("SiM0invw.inv"),
      m_iCdf(2),
      m_nCdfEntries(-1),
      m_isInitialised(false),
      m_isInMedium(false) {

  m_className = "TrackBichsel";
}

bool TrackBichsel::NewTrack(const double x0, const double y0, const double z0,
                            const double t0, const double dx0, const double dy0,
                            const double dz0) {

  // Make sure a sensor has been defined.
  if (m_sensor) {
    std::cerr << m_className << "::NewTrack:\n"
              << "    Sensor is not defined.\n";
    m_isInMedium = false;
    return false;
  }

  // If not yet done, load the cross-section table from file.
  if (!m_isInitialised) {
    if (!LoadCrossSectionTable(m_datafile)) {
      std::cerr << m_className << "::NewTrack:\n"
                << "    Cross-section table could not be loaded.\n";
      return false;
    }
    m_isInitialised = true;
  }

  // Make sure we are inside a medium.
  Medium* medium;
  if (!m_sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << m_className << "::NewTrack:\n"
              << "    No medium at initial position.\n";
    m_isInMedium = false;
    return false;
  }

  // Check if the medium is silicon.
  if (medium->GetName() != "Si") {
    std::cerr << m_className << "::NewTrack:\n"
              << "    Medium at initial position is not silicon.\n";
    m_isInMedium = false;
    return false;
  }

  // Check if primary ionisation has been enabled.
  if (!medium->IsIonisable()) {
    std::cerr << m_className << "::NewTrack:\n"
              << "    Medium at initial position is not ionisable.\n";
    m_isInMedium = false;
    return false;
  }

  m_isInMedium = true;
  m_x = x0;
  m_y = y0;
  m_z = z0;
  m_t = t0;

  // Normalise the direction vector.
  const double d = sqrt(dx0 * dx0 + dy0 * dy0 + dz0 * dz0);
  if (d < Small) {
    // In case of a null vector, choose a random direction.
    const double phi = TwoPi * RndmUniform();
    const double ctheta = 1. - 2. * RndmUniform();
    const double stheta = sqrt(1. - ctheta * ctheta);
    m_dx = cos(phi) * stheta;
    m_dy = sin(phi) * stheta;
    m_dz = ctheta;
  } else {
    m_dx = dx0 / d;
    m_dy = dy0 / d;
    m_dz = dz0 / d;
  }

  // If the particle properties have changed, update the cross-section table.
  if (m_isChanged) {
    m_bg = GetBetaGamma();
    m_imfp = GetClusterDensity();
    m_speed = SpeedOfLight * GetBeta();
    SelectCrossSectionTable();
    m_isChanged = false;
  }

  return true;
}

bool TrackBichsel::GetCluster(double& xcls, double& ycls, double& zcls,
                              double& tcls, int& n, double& e, double& extra) {

  if (!m_isInitialised || !m_isInMedium) return false;

  const double d = -log(RndmUniformPos()) / m_imfp;
  m_x += m_dx * d;
  m_y += m_dy * d;
  m_z += m_dz * d;
  m_t += d / m_speed;

  xcls = m_x;
  ycls = m_y;
  zcls = m_z;
  tcls = m_t;
  n = 0;
  e = 0.;
  extra = 0.;

  Medium* medium;
  if (!m_sensor->GetMedium(m_x, m_y, m_z, medium)) {
    m_isInMedium = false;
    if (m_debug) {
      std::cout << m_className << "::GetCluster: Particle left the medium.\n";
    }
    return false;
  }

  if (medium->GetName() != "Si" || !medium->IsIonisable()) {
    m_isInMedium = false;
    if (m_debug) {
      std::cout << m_className << "::GetCluster: Particle left the medium.\n";
    }
    return false;
  }

  const double u = m_nCdfEntries * RndmUniform();
  const int j = int(u);
  if (j == 0) {
    e = 0. + u * m_cdf[0][m_iCdf];
  } else if (j >= m_nCdfEntries) {
    e = m_cdf[m_nCdfEntries - 1][m_iCdf];
  } else {
    e = m_cdf[j - 1][m_iCdf] + 
        (u - j) * (m_cdf[j][m_iCdf] - m_cdf[j - 1][m_iCdf]);
  }

  return true;
}

double TrackBichsel::GetClusterDensity() {

  const int nEntries = 38;

  const double tabBg[nEntries] = {
      0.316,   0.398,   0.501,   0.631,    0.794,    1.000,   1.259,   1.585,
      1.995,   2.512,   3.162,   3.981,    5.012,    6.310,   7.943,   10.000,
      12.589,  15.849,  19.953,  25.119,   31.623,   39.811,  50.119,  63.096,
      79.433,  100.000, 125.893, 158.489,  199.526,  251.189, 316.228, 398.107,
      501.187, 630.958, 794.329, 1000.000, 1258.926, 1584.894};

  const double tabImfp[nEntries] = {
      30.32496, 21.14965, 15.06555, 11.05635, 8.43259, 6.72876, 5.63184,
      4.93252,  4.49174,  4.21786,  4.05090,  3.95186, 3.89531, 3.86471,
      3.84930,  3.84226,  3.83952,  3.83887,  3.83912, 3.83970, 3.84035,
      3.84095,  3.84147,  3.84189,  3.84223,  3.84249, 3.84269, 3.84283,
      3.84293,  3.84300,  3.84304,  3.84308,  3.84310, 3.84311, 3.84312,
      3.84313,  3.84313,  3.84314};

  if (m_isChanged) m_bg = GetBetaGamma();

  if (m_bg < tabBg[0]) {
    if (m_debug) {
      std::cerr << m_className << "::GetClusterDensity:\n"
                << "    Bg is below the tabulated range.\n";
    }
    return tabImfp[0] * 1.e4;
  } else if (m_bg > tabBg[nEntries - 1]) {
    return tabImfp[nEntries - 1] * 1.e4;
  }

  // Locate the requested energy in the table
  int iLow = 0;
  int iUp = nEntries - 1;
  int iM;
  while (iUp - iLow > 1) {
    iM = (iUp + iLow) >> 1;
    if (m_bg >= tabBg[iM]) {
      iLow = iM;
    } else {
      iUp = iM;
    }
  }

  if (fabs(m_bg - tabBg[iLow]) < 1.e-6 * (tabBg[iUp] - tabBg[iLow])) {
    return tabImfp[iLow] * 1.e4;
  }
  if (fabs(m_bg - tabBg[iUp]) < 1.e-6 * (tabBg[iUp] - tabBg[iLow])) {
    return tabImfp[iUp] * 1.e4;
  }

  // Log-log interpolation
  const double logX0 = log(tabBg[iLow]);
  const double logX1 = log(tabBg[iUp]);
  const double logY0 = log(tabImfp[iLow]);
  const double logY1 = log(tabImfp[iUp]);
  double d = logY0 + (log(m_bg) - logX0) * (logY1 - logY0) / (logX1 - logX0);
  return 1.e4 * exp(d);
}

double TrackBichsel::GetStoppingPower() {

  const int nEntries = 51;

  const double tabBg[nEntries] = {
      0.316,     0.398,    0.501,    0.631,     0.794,     1.000,     1.259,
      1.585,     1.995,    2.512,    3.162,     3.981,     5.012,     6.310,
      7.943,     10.000,   12.589,   15.849,    19.953,    25.119,    31.623,
      39.811,    50.119,   63.096,   79.433,    100.000,   125.893,   158.489,
      199.526,   251.189,  316.228,  398.107,   501.187,   630.958,   794.329,
      1000.000,  1258.926, 1584.894, 1995.263,  2511.888,  3162.280,  3981.074,
      5011.875,  6309.578, 7943.287, 10000.010, 12589.260, 15848.940, 19952.640,
      25118.880, 31622.800};

  const double tabdEdx[nEntries] = {
      2443.71800, 1731.65600, 1250.93400, 928.69920, 716.37140, 578.28850,
      490.83670,  437.33820,  406.58490,  390.95170, 385.29000, 386.12000,
      391.07730,  398.53930,  407.39420,  416.90860, 426.63010, 436.30240,
      445.78980,  455.02530,  463.97370,  472.61410, 480.92980, 488.90240,
      496.51900,  503.77130,  510.65970,  517.19570, 523.39830, 529.29120,
      534.90670,  540.27590,  545.42880,  550.39890, 555.20800, 559.88820,
      564.45780,  568.93850,  573.34700,  577.69140, 581.99010, 586.25090,
      590.47720,  594.68660,  598.86880,  603.03510, 607.18890, 611.33250,
      615.46810,  619.59740,  623.72150};

  if (m_isChanged) m_bg = GetBetaGamma();

  if (m_bg < tabBg[0]) {
    if (m_debug) {
      std::cerr << m_className << "::GetStoppingPower:\n";
      std::cerr << "    Bg is below the tabulated range.\n";
    }
    return tabdEdx[0] * 1.e4;
  } else if (m_bg > tabBg[nEntries - 1]) {
    return tabdEdx[nEntries - 1] * 1.e4;
  }

  // Locate the requested energy in the table
  int iLow = 0;
  int iUp = nEntries - 1;
  int iM;
  while (iUp - iLow > 1) {
    iM = (iUp + iLow) >> 1;
    if (m_bg >= tabBg[iM]) {
      iLow = iM;
    } else {
      iUp = iM;
    }
  }

  if (m_debug) {
    std::cout << m_className << "::GetStoppingPower:\n";
    std::cout << "    Bg = " << m_bg << "\n";
    std::cout << "    Interpolating between " << tabBg[iLow] << " and "
              << tabBg[iUp] << "\n";
  }

  if (fabs(m_bg - tabBg[iLow]) < 1.e-6 * (tabBg[iUp] - tabBg[iLow])) {
    return tabdEdx[iLow] * 1.e4;
  }
  if (fabs(m_bg - tabBg[iUp]) < 1.e-6 * (tabBg[iUp] - tabBg[iLow])) {
    return tabdEdx[iUp] * 1.e4;
  }

  // Log-log interpolation
  const double logX0 = log(tabBg[iLow]);
  const double logX1 = log(tabBg[iUp]);
  const double logY0 = log(tabdEdx[iLow]);
  const double logY1 = log(tabdEdx[iUp]);
  const double dedx =
      logY0 + (log(m_bg) - logX0) * (logY1 - logY0) / (logX1 - logX0);
  return 1.e4 * exp(dedx);
}

bool TrackBichsel::LoadCrossSectionTable(const std::string& filename) {

  const int nRows = 10000;
  const int nBlocks = 2;
  const int nColumns = 5;

  const int iSwitch = 99999;

  // Get the path to the data directory.
  char* pPath = getenv("GARFIELD_HOME");
  if (pPath == 0) {
    std::cerr << m_className << "::LoadCrossSectionTable:\n";
    std::cerr << "    Environment variable GARFIELD_HOME is not set.\n";
    return false;
  }
  std::string filepath = pPath;
  filepath = filepath + "/Data/" + filename;

  // Open the file.
  std::ifstream infile;
  infile.open(filepath.c_str(), std::ios::in);
  // Check if the file could be opened.
  if (!infile) {
    std::cerr << m_className << "::LoadCrossSectionTable:\n";
    std::cerr << "    Error opening file " << filename << ".\n";
    return false;
  }

  // Initialise the cumulative distribution table.
  m_cdf.assign(nRows, std::vector<double>(nBlocks * nColumns, 0.));

  std::string line;
  std::istringstream data;
  int dummy1 = 0;
  double dummy2 = 0.;

  double val[nColumns];
  int iBlock = 0;
  int iRow = 0;

  while (!infile.eof() && !infile.fail()) {
    // Read the line.
    std::getline(infile, line);
    // Strip white space from the beginning of the line.
    line.erase(line.begin(),
               std::find_if(line.begin(), line.end(),
                            not1(std::ptr_fun<int, int>(isspace))));
    // Skip comments.
    if (line[0] == '#' || line[0] == '*' || (line[0] == '/' && line[1] == '/'))
      continue;
    // Extract the values.
    data.str(line);
    data >> dummy1 >> dummy2;
    for (int j = 0; j < nColumns; ++j) data >> val[j];
    // 99999 indicates the end of a data block.
    if (dummy1 == iSwitch) {
      ++iBlock;
      if (iBlock >= nBlocks) break;
      // Reset the row counter.
      iRow = 0;
      continue;
    } else if (dummy1 != iRow + 1) {
      std::cerr << m_className << "::LoadCrossSectionTable:\n";
      std::cerr << "    Error reading file " << filename << ".\n";
      std::cerr << "    Expected entry " << iRow + 1 << ", got entry " << dummy1
                << ".\n";
      infile.close();
      m_cdf.clear();
      return false;
    }
    if (iRow >= nRows) {
      std::cerr << m_className << "::LoadCrossSectionTable:\n";
      std::cerr << "    Table in file is longer than expected.\n";
      infile.close();
      m_cdf.clear();
      return false;
    }
    for (int j = nColumns; j--;) m_cdf[iRow][nColumns * iBlock + j] = val[j];
    ++iRow;
  }

  if (infile.fail()) {
    std::cerr << m_className << "::LoadCrossSectionTable:\n";
    std::cerr << "    Error reading file " << filename << ".\n";
    infile.close();
    m_cdf.clear();
    return false;
  }
  infile.close();

  if (m_debug) {
    std::cout << m_className << "::LoadCrossSectionTable:\n";
    std::cout << "    Input file: " << filename << std::endl;
    std::cout << "    Successfully loaded cross-section table from file.\n";
  }
  m_nCdfEntries = nRows;
  return true;
}

void TrackBichsel::SelectCrossSectionTable() {

  const int nTables = 10;
  const double tabBg[nTables] = {0.31623,    1.00000,    3.16228,   10.00000,
                                 31.62278,   100.00000,  316.22780, 1000.00000,
                                 3162.27800, 10000.00000};

  bool gotValue = false;
  // Chose the table which is closest to the actual value of bg.
  for (int i = 0; i < nTables - 1; ++i) {
    double split = exp(0.5 * (log(tabBg[i]) + log(tabBg[i + 1])));
    if (m_bg < split) {
      m_iCdf = i;
      gotValue = true;
      break;
    }
  }
  if (!gotValue) m_iCdf = nTables - 1;

  if (m_debug) {
    std::cout << m_className << "::SelectCrossSectionTable:\n";
    std::cout << "    Requested value: bg = " << m_bg << "\n";
    std::cout << "    Used table:      bg = " << tabBg[m_iCdf] << "\n";
  }
}
}
