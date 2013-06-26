#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <algorithm>
#include <cmath>

#include "ComponentVoxel.hh"

namespace Garfield {

ComponentVoxel::ComponentVoxel() : 
  ComponentBase(), 
  nX(0), nY(0), nZ(0),
  xMin(0.), yMin(0.), zMin(0.),
  xMax(0.), yMax(0.), zMax(0.),
  hasMesh(false), hasPotential(false), hasField(false),
  pMin(0.), pMax(0.) {

  className = "ComponentVoxel";
    
}

void 
ComponentVoxel::ElectricField(
                      const double xin, const double yin, const double zin,
                      double& ex, double& ey, double& ez, double& p,
                      Medium*& m, int& status) {
 
  m = 0; 
  // Make sure the field map has been loaded.
  if (!ready) {
    std::cerr << className << "::ElectricField:\n";
    std::cerr << "    Field map is not available for interpolation.\n";
    status = -10;
    return;
  }  

  unsigned int i, j, k;
  bool xMirrored, yMirrored, zMirrored;
  if (!GetElement(xin, yin, zin, i, j, k, 
                  xMirrored, yMirrored, zMirrored)) {
    status = -11;
    return;
  }
  status = 0;
  // Get the electric field and potential.
  ex = mesh[i][j][k].ex;
  ey = mesh[i][j][k].ey;
  ez = mesh[i][j][k].ez;
  p = mesh[i][j][k].v;
  if (xMirrored) ex = -ex;
  if (yMirrored) ey = -ey;
  if (zMirrored) ez = -ez;
  // Get the medium.
  int region = mesh[i][j][k].region;
  if (media.count(region) < 1) {
    m = 0;
    status = -5;
    return;
  }
  m = media[region];
  if (m == 0) status = -5;

}

void 
ComponentVoxel::ElectricField(const double x, const double y, const double z,
                               double& ex, double& ey, double& ez,
                               Medium*& m, int& status) {

  double v = 0.;
  ElectricField(x, y, z, ex, ey, ez, v, m, status);
  
}

bool 
ComponentVoxel::GetMedium(
                     const double xin, const double yin, const double zin,
                     Medium*& m) {

  m = 0;
  // Make sure the field map has been loaded.
  if (!ready) {
    std::cerr << className << "::GetMedium:\n";
    std::cerr << "    Field map not available for interpolation.\n";
    return false;
  }  
  
  unsigned int i, j, k;
  bool xMirrored, yMirrored, zMirrored;
  if (!GetElement(xin, yin, zin, i, j, k, 
                  xMirrored, yMirrored, zMirrored)) {
    return false;
  }
  if (media.count(mesh[i][j][k].region) < 1) {
    m = 0;
  } else {
    m = media[mesh[i][j][k].region];
  }
  if (m == 0) return false;
  return true;

}

void
ComponentVoxel::SetMesh(const unsigned int nx,
                        const unsigned int ny,
                        const unsigned int nz,
                        const double xmin, const double xmax,
                        const double ymin, const double ymax,
                        const double zmin, const double zmax) {

  ready = false;
  if (nx == 0 || ny == 0 || nz == 0) {
    std::cerr << className << "::SetMesh:\n";
    std::cerr << "    Number of mesh elements must be positive.\n";
    return;
  }
  if (xmin >= xmax) {
    std::cerr << className << "::SetMesh:\n";
    std::cerr << "    Invalid x range.\n";
    return;
  } else if (ymin >= ymax) {
    std::cerr << className << "::SetMesh:\n";
    std::cerr << "    Invalid y range.\n"; 
    return;
  } else if (zmin >= zmax) {
    std::cerr << className << "::SetMesh:\n";
    std::cerr << "    Invalid z range.\n";
    return;
  }
  nX = nx; nY = ny; nZ = nz;
  xMin = xmin; yMin = ymin; zMin = zmin;
  xMax = xmax; yMax = ymax; zMax = zmax;
  // Resize the mesh.
  mesh.clear();
  mesh.resize(nX);
  for (unsigned int i = 0; i < nX; ++i) {
    mesh[i].resize(nY);
    for (unsigned int j = 0; j < nY; ++j) {
      mesh[i][j].resize(nZ);
      for (unsigned int k = 0; k < nZ; ++k) {
        mesh[i][j][k].ex = 0.;
        mesh[i][j][k].ey = 0.;
        mesh[i][j][k].ez = 0.;
        mesh[i][j][k].v = 0.;
        mesh[i][j][k].region = -1;
      }
    }
  }
  hasMesh = true;

}

bool 
ComponentVoxel::LoadData(const std::string filename, std::string format,
                         const bool withPotential, const bool withRegion,
                         const double scaleX, const double scaleE, 
                         const double scaleP) {
 
  if (!hasMesh) {
    std::cerr << className << "::LoadData:\n";
    std::cerr << "    Mesh is not set. Call SetMesh first.\n";
    return false;
  }
  ready = false;
  hasPotential = hasField = false;
  pMin = pMax = 0.;
  if (withPotential) {
    pMin =  1.;
    pMax = -1.;
  }

  unsigned int nValues = 0;
  std::vector<std::vector<std::vector<bool> > > isSet;
  isSet.resize(nX);
  for (unsigned int i = 0; i < nX; ++i) {
    isSet[i].resize(nY);
    for (unsigned int j = 0; j < nY; ++j) {
      isSet[i][j].resize(nZ, false);
    }
  }
  std::ifstream infile;
  infile.open(filename.c_str(), std::ios::in);
  if (!infile) {
    std::cerr << className << "::LoadData:\n";
    std::cerr << "    Could not open file " << filename << ".\n";
    return false;
  }

  std::transform(format.begin(), format.end(), format.begin(), toupper);
  if (format != "XYZ" && format != "IJK") {
    std::cerr << className << "::LoadData:\n";
    std::cerr << "    Unkown format (" << format << ").\n";
    return false;
  }
 
  std::string line;
  unsigned int nLines = 0;
  bool bad = false;
  while (!infile.fail()) {
    // Read one line.
    std::getline(infile, line);
    ++nLines;
    // Strip white space from beginning of line.
    line.erase(line.begin(), std::find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Skip empty lines.
    if (line.size() < 2) continue;
    // Skip comments.
    if (line[0] == '#') continue;
    if (line[0] == '/' && line[1] == '/') continue;
    unsigned int i = 0;
    unsigned int j = 0;
    unsigned int k = 0;
    double ex = 0.;
    double ey = 0.;
    double ez = 0.;
    double v = 0.;
    int region = 0;
    std::istringstream data;
    data.str(line);
    if (format == "XYZ") {
      double x, y, z;
      data >> x >> y >> z;
      if (data.fail()) {
        std::cerr << className << "::LoadData:\n";
        std::cerr << "    Error reading line " << nLines << ".\n";
        std::cerr << "    Cannot retrieve element coordinates.\n";
        bad = true;
        break;
      }
      x *= scaleX;
      y *= scaleX;
      z *= scaleX;
      bool xMirrored, yMirrored, zMirrored;
      if (!GetElement(x, y, z, i, j, k, xMirrored, yMirrored, zMirrored)) {
        std::cerr << className << "::LoadData:\n";
        std::cerr << "    Error reading line " << nLines << ".\n";
        std::cerr << "    Point is outside mesh.\n";
        bad = true;
        break;
      }
    } else {
      data >> i >> j >> k;
      if (data.fail()) {
        std::cerr << className << "::LoadData:\n";
        std::cerr << "    Error reading line " << nLines << ".\n";
        std::cerr << "    Cannot retrieve element index.\n";
        bad = true;
        break;
      }
    }
    // Check the indices.
    if (i >= nX || j >= nY || k >= nZ) {
      std::cerr << className << "::LoadData:\n";
      std::cerr << "    Error reading line " << nLines << ".\n";
      std::cerr << "    Index (" << i << ", " << j << ", " << k 
                << ") out of range.\n";
      continue;
    }
    if (isSet[i][j][k]) {
      std::cerr << className << "::LoadData:\n";
      std::cerr << "    Error reading line " << nLines << ".\n";
      std::cerr << "    Mesh element (" << i << ", " << j << ", " << k
                << ") has already been set.\n";
      continue;
    }
    // Get the electric field values;
    data >> ex >> ey >> ez;
    if (data.fail()) {
      std::cerr << className << "::LoadData:\n";
      std::cerr << "    Error reading line " << nLines << ".\n";
      std::cerr << "    Cannot read electric field values.\n";
      bad = true;
      break;
    }
    ex *= scaleE;
    ey *= scaleE;
    ez *= scaleE;
    if (withPotential) {
      data >> v;
      if (data.fail()) {
        std::cerr << className << "::LoadData:\n";
        std::cerr << "    Error reading line " << nLines << ".\n";
        std::cerr << "    Cannot read potential.\n";
        bad = true;
        break;
      }
      v *= scaleP;
      if (pMin > pMax) {
        // First value.
        pMin = v;
        pMax = v;
      } else {
        if (v < pMin) pMin = v;
        if (v > pMax) pMax = v;
      }
    }
    if (withRegion) {
      data >> region;
      if (data.fail()) {
        std::cerr << className << "::LoadData:\n";
        std::cerr << "    Error reading line " << nLines << ".\n";
        std::cerr << "    Cannot read region.\n";
        bad = true;
        break;
      }
    }
    mesh[i][j][k].ex = ex;
    mesh[i][j][k].ey = ey;
    mesh[i][j][k].ez = ez;
    mesh[i][j][k].v = v;
    mesh[i][j][k].region = region;
    isSet[i][j][k] = true;
    ++nValues;
  }
  if (bad) return false;
  std::cout << className << "::LoadData:\n";
  std::cout << "    Read " << nValues << " values from file " 
            << filename << ".\n";
  const unsigned int nExpected = nX * nY * nZ;
  if (nExpected != nValues) {
    std::cerr << className << "::LoadData:\n";
    std::cerr << "   Expected " << nExpected << " values.\n"; 
  }
  ready = true;
  return true;

}

bool 
ComponentVoxel::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                               double& xmax, double& ymax, double& zmax) {

  if (!ready) return false;
  if (xPeriodic || xMirrorPeriodic) {
    xmin = -INFINITY; 
    xmax = +INFINITY;
  } else {
    xmin = xMin; 
    xmax = xMax;
  }

  if (yPeriodic || yMirrorPeriodic) {
    ymin = -INFINITY; 
    ymax = +INFINITY;
  } else {
    ymin = yMin; 
    ymax = yMax;
  }

  if (zPeriodic || zMirrorPeriodic) {
    zmin = -INFINITY; 
    zmax = +INFINITY;
  } else {
    zmin = zMin; 
    zmax = zMax;
  }
  return true;

}

bool 
ComponentVoxel::GetVoltageRange(double& vmin, double& vmax) {

  if (!ready) return false;
  vmin = pMin; vmax = pMax;
  return true;
  
}

bool
ComponentVoxel::GetElectricFieldRange(double& exmin, double& exmax,
                                      double& eymin, double& eymax,
                                      double& ezmin, double& ezmax) {

  if (!ready) {
    std::cerr << className << "::GetElectricFieldRange:\n";
    std::cerr << "    Field map not available.\n";
    return false;
  }
  bool gotValue = false;
  for (unsigned int i = 0; i < nX; ++i) {
    for (unsigned int j = 0; j < nY; ++j) {
      for (unsigned int k = 0; k < nZ; ++k) {
        if (!gotValue) {
          exmin = mesh[i][j][k].ex;
          exmax = mesh[i][j][k].ex;
          eymin = mesh[i][j][k].ey;
          eymax = mesh[i][j][k].ey;
          ezmin = mesh[i][j][k].ez;
          ezmax = mesh[i][j][k].ez;
	  gotValue = true;
	  continue;
        } 
        if (mesh[i][j][k].ex < exmin) exmin = mesh[i][j][k].ex;
        if (mesh[i][j][k].ex > exmax) exmax = mesh[i][j][k].ex;
        if (mesh[i][j][k].ey < eymin) eymin = mesh[i][j][k].ey;
        if (mesh[i][j][k].ey > eymax) eymax = mesh[i][j][k].ey;
        if (mesh[i][j][k].ez < ezmin) ezmin = mesh[i][j][k].ez;
        if (mesh[i][j][k].ez > ezmax) ezmax = mesh[i][j][k].ez;
      }
    }
  }
  return true;

}

void
ComponentVoxel::PrintRegions() {

  // Do not proceed if not properly initialised.
  if (!ready) {
    std::cerr << className << "::PrintRegions:\n";
    std::cerr << "    Field map not yet initialised.\n";
    return;
  }

  if (media.size() < 1) {
    std::cerr << className << "::PrintRegions:\n";
    std::cerr << "    No regions are currently defined.\n";
    return;
  }

  std::cout << className << "::PrintRegions:\n";
  if (media.size() == 1) {
    std::cout << "    1 region is defined.\n";
  } else {
    std::cout << "    " << media.size() << " regions are defined.\n";
  }
  std::cout << "      Index     Medium\n";
  std::map<int, Medium*>::iterator it;
  for (it = media.begin(); it != media.end(); ++it) {
    const int i = (*it).first;
    Medium* m = (*it).second;
    std::cout << "      " << i << "      ";
    if (m == 0) {
      std::cout << "      none\n";
    } else {
      std::cout << "      " << m->GetName() << "\n";
    }
  }

}
 
void 
ComponentVoxel::SetMedium(const int i, Medium* m) {

  if (m == 0) {
    std::cerr << className << "::SetMedium:\n";
    std::cerr << "    Warning: medium pointer is null.\n";
  }
  media[i] = m;
  
}

bool
ComponentVoxel::GetMedium(const int i, Medium*& m) {

  if (media.count(i) < 1) {
    std::cerr << className << "::GetMedium:\n";
    std::cerr << "    Medium " << i << " does not exist.\n";
    return false;
  }
  
  m = media[i];
  if (m == 0) return false;
  return true;

}

bool
ComponentVoxel::GetElement(const double xi, const double yi, const double zi,
                           unsigned int& i, unsigned int& j, unsigned int& k,
                           bool& xMirrored, bool& yMirrored, bool& zMirrored) {

  if (!hasMesh) {
    std::cerr << className << "::GetElement:\n";
    std::cerr << "    Mesh is not set.\n";
    return false;
  }

  double x = xi, y = yi, z = zi;
  xMirrored = yMirrored = zMirrored = false;
  // In case of periodicity, reduce to the basic cell.
  const double cellsx = xMax - xMin;
  if (xPeriodic) {
    x = xMin + fmod(x - xMin, cellsx);
    if (x < xMin) x += cellsx;
  } else if (xMirrorPeriodic) {
    double xNew = xMin + fmod(x - xMin, cellsx);
    if (xNew < xMin) xNew += cellsx;
    int nx = int(floor(0.5 + (xNew - x) / cellsx));
    if (nx != 2 * (nx / 2)) {
      xNew = xMin + xMax - xNew;
      xMirrored = true;
    }
    x = xNew;
  } 
  // Check if the point is outside the mesh.
  if (x < xMin || x > xMax) return false;

  const double cellsy = yMax - yMin;
  if (yPeriodic) {
    y = yMin + fmod(y - yMin, cellsy);
    if (y < yMin) y += cellsy;
  } else if (yMirrorPeriodic) {
    double yNew = yMin + fmod(y - yMin, cellsy);
    if (yNew < yMin) yNew += cellsy;
    int ny = int(floor(0.5 + (yNew - y) / cellsy));
    if (ny != 2 * (ny / 2)) {
      yNew = yMin + yMax - yNew;
      yMirrored = true;
    }
    y = yNew;
  } 
  // Check if the point is outside the mesh.
  if (y < yMin || y > yMax) return false;

  const double cellsz = zMax - xMin;
  if (zPeriodic) {
    z = zMin + fmod(z - zMin, cellsz);
    if (z < zMin) z += cellsz;
  } else if (zMirrorPeriodic) {
    double zNew = zMin + fmod(z - zMin, cellsz);
    if (zNew < zMin) zNew += cellsz;
    int nz = int(floor(0.5 + (zNew - z) / cellsz));
    if (nz != 2 * (nz / 2)) {
      zNew = zMin + zMax - zNew;
      zMirrored = true;
    }
    z = zNew;
  }
  // Check if the point is outside the mesh.
  if (z < zMin || z > zMax) return false;

  // Get the indices.
  const double dx = (xMax - xMin) / nX;
  const double dy = (yMax - yMin) / nY;
  const double dz = (zMax - zMin) / nZ;
  i = (unsigned int)((x - xMin) / dx); 
  j = (unsigned int)((y - yMin) / dy); 
  k = (unsigned int)((z - zMin) / dz);
  if (i >= nX) i = nX - 1;
  if (j >= nY) j = nY - 1;
  if (k >= nZ) k = nZ - 1;
  return true;

}

bool
ComponentVoxel::GetElement(const unsigned int i, 
                           const unsigned int j, 
                           const unsigned int k,
                           double& v, double& ex, double& ey, double& ez) {

  v = ex = ey = ez = 0.;
  if (!ready) {
    if (!hasMesh) {
      std::cerr << className << "::GetElement:\n";
      std::cerr << "    Mesh not set.\n";
      return false;
    }
    std::cerr << className << "::GetElement:\n";
    std::cerr << "    Fiel map not set.\n";
    return false;
  }
  if (i >= nX || j >= nY || k >= nZ) {
    std::cerr << className << "::GetElement:\n";
    std::cerr << "    Element index out of range.\n";
    return false;
  }
  v = mesh[i][j][k].v;
  ex = mesh[i][j][k].ex;
  ey = mesh[i][j][k].ey;
  ez = mesh[i][j][k].ez;
  return true;

}

void
ComponentVoxel::Reset() {

  mesh.clear();
  nX = nY = nZ = 0;
  xMin = yMin = zMin = 0.;
  xMax = yMax = zMax = 0.;
  pMin = pMax = 0.;
  media.clear();

  hasMesh = false;
  hasPotential = false;
  hasField = false;
  
  ready = false;

}

void 
ComponentVoxel::UpdatePeriodicity() {

  if (!ready) {
    std::cerr << className << "::UpdatePeriodicity:\n";
    std::cerr << "    Field map not available.\n";
    return;
  }

  // Check for conflicts.
  if (xPeriodic && xMirrorPeriodic) {
    std::cerr << className << "::UpdatePeriodicity:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along x requested; reset.\n";
    xPeriodic = xMirrorPeriodic = false;
  }

  if (yPeriodic && yMirrorPeriodic) {
    std::cerr << className << "::UpdatePeriodicity:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along y requested; reset.\n";
    yPeriodic = yMirrorPeriodic = false;
  }

  if (zPeriodic && zMirrorPeriodic) {
    std::cerr << className << "::UpdatePeriodicity:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along z requested; reset.\n";
    zPeriodic = zMirrorPeriodic = false;
  }

  if (xAxiallyPeriodic || yAxiallyPeriodic || zAxiallyPeriodic) {
    std::cerr << className << "::UpdatePeriodicity:\n";
    std::cerr << "    Axial symmetry is not supported; reset.\n";
    xAxiallyPeriodic = yAxiallyPeriodic = zAxiallyPeriodic = false;
  }

  if (xRotationSymmetry || yRotationSymmetry || zRotationSymmetry) {
    std::cerr << className << "::UpdatePeriodicity:\n";
    std::cerr << "    Rotation symmetry is not supported; reset.\n";
    xRotationSymmetry = yRotationSymmetry = zRotationSymmetry = false;
  }
 
}

}
