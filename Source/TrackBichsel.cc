#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>

#include "TrackBichsel.hh"
#include "FundamentalConstants.hh"
#include "Random.hh"

namespace Garfield {

TrackBichsel::TrackBichsel() :
  bg(3.16228), speed(SpeedOfLight * bg / sqrt(1. + bg * bg)),
  x(0.), y(0.), z(0.), t(0.), dx(0.), dy(0.), dz(1.),
  imfp(4.05090e4), datafile("SiM0invw.inv"), iCdf(2), nCdfEntries(-1),
  isInitialised(false), isInMedium(false) {
  
}

void
TrackBichsel::NewTrack(
            const double x0, const double y0, const double z0, const double t0,
            const double dx0, const double dy0, const double dz0) {

  // Make sure a sensor has been defined.
  if (sensor == 0) {
    std::cerr << "TrackBichsel::NewTrack:\n";
    std::cerr << "    Sensor is not defined.\n";
    isInMedium = false;
    return;
  }

  // If not yet done, load the cross-section table from file. 
  if (!isInitialised) {
    if (!LoadCrossSectionTable(datafile)) {
      std::cerr << "TrackBichsel::NewTrack:\n";
      std::cerr << "    Cross-section table could not be loaded.\n";
      return;
    }
    isInitialised = true;
  }

  // Make sure we are inside a medium.
  Medium* medium;
  if (!sensor->GetMedium(x0, y0, z0, medium)) {
    std::cerr << "TrackBichsel::NewTrack:\n";
    std::cerr << "    No medium at initial position.\n";
    isInMedium = false;
    return;
  }

  // Check if the medium is silicon.
  if (medium->GetName() != "Si") {
    std::cerr << "TrackBichsel::NewTrack:" << std::endl;
    std::cerr << "    Medium at initial position is not silicon.\n";
    isInMedium = false;
    return;
  }

  // Check if primary ionisation has been enabled.
  if (!medium->IsIonisable()) {
    std::cerr << "TrackBichsel::NewTrack:\n";
    std::cerr << "    Medium at initial position is not ionisable.\n"; 
    isInMedium = false;
    return;
  }

  isInMedium = true;
  x = x0; y = y0; z = z0; t = t0;

  // Normalise the direction vector.
  const double d = sqrt(dx0 * dx0 + dy0 * dy0 + dz0 * dz0);
  if (d < Small) {
    // In case of a null vector, choose a random direction.
    const double phi = TwoPi * RndmUniform();
    const double ctheta = 1. - 2. * RndmUniform();
    const double stheta = sqrt(1. - ctheta * ctheta);
    dx = cos(phi) * stheta;
    dy = sin(phi) * stheta;
    dz = ctheta;
  } else {
    dx = dx0 / d; dy = dy0 / d; dz = dz0 / d;
  }
 
  // If the particle properties have changed, update the cross-section table.
  if (isChanged) {
    bg = GetBetaGamma();
    imfp = GetInverseMeanFreePath(bg);
    speed = SpeedOfLight * GetBeta();
    SelectCrossSectionTable();
  }

}

bool
TrackBichsel::GetCluster(
          double& xcls, double& ycls, double& zcls, double& tcls,
          int& n, double& e, double& extra) {


  if (!isInitialised || !isInMedium) return false;

  double d = -log(RndmUniformPos()) / imfp;
  x += dx * d;
  y += dy * d;
  z += dz * d;
  t += d / speed;

  xcls = x; ycls = y; zcls = z; tcls = t;
  n = 0; e = 0.; extra = 0.;

  Medium* medium;
  if (!sensor->GetMedium(x, y, z, medium)) {
    isInMedium = false;
    if (debug) {
      std::cout << "TrackBichsel::GetCluster:\n";
      std::cout << "    Particle left the medium.\n";
    }
    return false;
  }

  if (medium->GetName() != "Si" || !medium->IsIonisable()) {
    isInMedium = false;
    if (debug) {
      std::cout << "TrackBichsel::GetCluster:\n";
      std::cout << "    Particle left the medium.\n";
    }
    return false;
  }

  int j = int(RndmUniform() * nCdfEntries);
  if (j >= nCdfEntries) j = nCdfEntries - 1;
  if (j <= 0) j = 0;

  e = cdf[j][iCdf];
  return true;

}

double
TrackBichsel::GetInverseMeanFreePath(const double bg) {

  const int nEntries = 38;

  const double tabBg[nEntries] = {
       0.316,    0.398,    0.501,    0.631,    0.794,  
       1.000,    1.259,    1.585,    1.995,    2.512, 
       3.162,    3.981,    5.012,    6.310,    7.943,  
      10.000,   12.589,   15.849,   19.953,   25.119,
      31.623,   39.811,   50.119,   63.096,   79.433,  
     100.000,  125.893,  158.489,  199.526,  251.189,
     316.228,  398.107,  501.187,  630.958,  794.329, 
    1000.000, 1258.926, 1584.894};

  const double tabImfp[nEntries] = {
    30.32496, 21.14965, 15.06555, 11.05635,  8.43259,
     6.72876,  5.63184,  4.93252,  4.49174,  4.21786,
     4.05090,  3.95186,  3.89531,  3.86471,  3.84930,
     3.84226,  3.83952,  3.83887,  3.83912,  3.83970,
     3.84035,  3.84095,  3.84147,  3.84189,  3.84223,
     3.84249,  3.84269,  3.84283,  3.84293,  3.84300,
     3.84304,  3.84308,  3.84310,  3.84311,  3.84312,
     3.84313,  3.84313,  3.84314};

  if (bg < tabBg[0]) {
    if (debug) {
      std::cerr << "TrackBichsel::GetInverseMeanFreePath:\n";
      std::cerr << "    Bg is below the tabulated range.\n";
    }
    return tabImfp[0] * 1.e4;
  } else if (bg > tabBg[nEntries - 1]) {
    return tabImfp[nEntries - 1] * 1.e4;
  }

  // Locate the requested energy in the table
  int iLow = 0;
  int iUp = nEntries - 1;
  int iM;
  while (iUp - iLow > 1) {
    iM = (iUp + iLow) >> 1;
    if (bg >= tabBg[iM]) {
      iLow = iM;
    } else {
      iUp = iM;
    }
  }

  // Log-log interpolation
  return (tabImfp[iLow] + 
          exp((log(bg) - log(tabBg[iLow])) * 
              (log(tabImfp[iUp]) - log(tabImfp[iLow])) / 
              (log(tabBg[iUp]) - log(tabBg[iLow])))) * 1.e4;
  
}

bool
TrackBichsel::LoadCrossSectionTable(const std::string filename) {

  const int nRows = 10000;
  const int nBlocks = 2;
  const int nColumns = 5;

  const int iSwitch = 99999;

  // Get the path to the data directory.
  char* pPath = getenv("GARFIELD_HOME");
  if (pPath == 0) {
    std::cerr << "TrackBichsel:\n";
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
    std::cerr << "TrackBichsel::LoadCrossSectionTable:\n";
    std::cerr << "    Error opening file " << filename << ".\n";
    return false;
  }

  // Initialise the cumulative distribution table.
  cdf.clear();
  cdf.resize(nRows);
  for (int i = nRows; i--;) cdf[i].resize(nBlocks * nColumns);
  
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
    line.erase(line.begin(), std::find_if(line.begin(), line.end(),
               not1(std::ptr_fun<int, int>(isspace))));
    // Skip comments.
    if (line[0] == '#' || 
        (line[0] == '/' && line[1] == '/')) continue;
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
      std::cerr << "TrackBichsel::LoadCrossSectionTable:\n";
      std::cerr << "    Error reading file " << filename << ".\n";
      std::cerr << "    Expected entry " << iRow + 1 
                << ", got entry " << dummy1 << ".\n";
      infile.close();
      cdf.clear();
      return false;
    }
    if (iRow >= nRows) {
      std::cerr << "TrackBichsel::LoadCrossSectionTable:\n";
      std::cerr << "    Table in file is longer than expected.\n";
      infile.close();
      cdf.clear();
      return false;
    }
    for (int j = nColumns; j--;) cdf[iRow][nColumns * iBlock + j] = val[j];
    ++iRow;
  }

  if (infile.fail()) {
    std::cerr << "TrackBichsel::LoadCrossSectionTable:\n";
    std::cerr << "    Error reading file " << filename << ".\n";
    infile.close(),
    cdf.clear();
    return false;
  }
  infile.close();
  
  if (debug) {
    std::cout << "TrackBichsel::LoadCrossSectionTable:\n";
    std::cout << "    Input file: " << filename << std::endl;
    std::cout << "    Successfully loaded cross-section table from file.\n";
  }
  nCdfEntries = nRows;
  return true;

}

void
TrackBichsel::SelectCrossSectionTable() {

  const int nTables = 10;
  const double tabBg[nTables] = {
      0.31623,   1.00000,    3.16228,   10.00000,    31.62278,
    100.00000, 316.22780, 1000.00000, 3162.27800, 10000.00000};

  bool gotValue = false;
  // Chose the table which is closest to the actual value of bg.
  for (int i = 0; i < nTables - 1; ++i) {
    double split = exp(0.5 * (log(tabBg[i]) + log(tabBg[i + 1])));
    if (bg < split) {
      iCdf = i;
      gotValue = true;
      break;
    }
  }
  if (!gotValue) iCdf = nTables - 1;

  if (debug) {
    std::cout << "TrackBichsel::SelectCrossSectionTable:\n";
    std::cout << "    Requested value: bg = " << bg << "\n";
    std::cout << "    Select table for bg = " << tabBg[iCdf] << "\n";
  }

}

}
