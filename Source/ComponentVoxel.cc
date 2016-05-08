#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <algorithm>
#include <cmath>

#include "ComponentVoxel.hh"

namespace Garfield {

ComponentVoxel::ComponentVoxel()
    : ComponentBase(),
      m_nX(0),
      m_nY(0),
      m_nZ(0),
      m_xMin(0.),
      m_yMin(0.),
      m_zMin(0.),
      m_xMax(0.),
      m_yMax(0.),
      m_zMax(0.),
      m_hasMesh(false),
      m_hasPotential(false),
      m_hasField(false),
      m_pMin(0.),
      m_pMax(0.) {

  m_className = "ComponentVoxel";
}

void ComponentVoxel::ElectricField(const double xin, const double yin,
                                   const double zin, double& ex, double& ey,
                                   double& ez, double& p, Medium*& m,
                                   int& status) {

  m = NULL;
  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::ElectricField:\n";
    std::cerr << "    Field map is not available for interpolation.\n";
    status = -10;
    return;
  }

  unsigned int i, j, k;
  bool xMirrored, yMirrored, zMirrored;
  if (!GetElement(xin, yin, zin, i, j, k, xMirrored, yMirrored, zMirrored)) {
    status = -11;
    return;
  }
  status = 0;
  // Get the electric field and potential.
  ex = m_mesh[i][j][k].ex;
  ey = m_mesh[i][j][k].ey;
  ez = m_mesh[i][j][k].ez;
  p = m_mesh[i][j][k].v;
  if (xMirrored) ex = -ex;
  if (yMirrored) ey = -ey;
  if (zMirrored) ez = -ez;
  // Get the medium.
  int region = m_mesh[i][j][k].region;
  if (m_media.count(region) < 1) {
    m = 0;
    status = -5;
    return;
  }
  m = m_media[region];
  if (m == NULL) status = -5;
}

void ComponentVoxel::ElectricField(const double x, const double y,
                                   const double z, double& ex, double& ey,
                                   double& ez, Medium*& m, int& status) {

  double v = 0.;
  ElectricField(x, y, z, ex, ey, ez, v, m, status);
}

void ComponentVoxel::WeightingField(const double x, const double y, const double z,
                                   double& wx, double& wy, double& wz, 
				   const std::string& label) {
  int status = 0;
  Medium* med = NULL;
  double v = 0.;
  ElectricField(x, y, z, wx, wy, wz, v, med, status);
}


Medium* ComponentVoxel::GetMedium(const double xin, const double yin,
                                  const double zin) {

  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::GetMedium:\n";
    std::cerr << "    Field map not available for interpolation.\n";
    return NULL;
  }

  unsigned int i, j, k;
  bool xMirrored, yMirrored, zMirrored;
  if (!GetElement(xin, yin, zin, i, j, k, xMirrored, yMirrored, zMirrored)) {
    return NULL;
  }
  if (m_media.count(m_mesh[i][j][k].region) < 1) {
    return NULL;
  } 
  return m_media[m_mesh[i][j][k].region];
}

void ComponentVoxel::SetMesh(const unsigned int nx, const unsigned int ny,
                             const unsigned int nz, const double xmin,
                             const double xmax, const double ymin,
                             const double ymax, const double zmin,
                             const double zmax) {

  m_ready = false;
  if (nx == 0 || ny == 0 || nz == 0) {
    std::cerr << m_className << "::SetMesh:\n";
    std::cerr << "    Number of mesh elements must be positive.\n";
    return;
  }
  if (xmin >= xmax) {
    std::cerr << m_className << "::SetMesh:\n";
    std::cerr << "    Invalid x range.\n";
    return;
  } else if (ymin >= ymax) {
    std::cerr << m_className << "::SetMesh:\n";
    std::cerr << "    Invalid y range.\n";
    return;
  } else if (zmin >= zmax) {
    std::cerr << m_className << "::SetMesh:\n";
    std::cerr << "    Invalid z range.\n";
    return;
  }
  m_nX = nx;
  m_nY = ny;
  m_nZ = nz;
  m_xMin = xmin;
  m_yMin = ymin;
  m_zMin = zmin;
  m_xMax = xmax;
  m_yMax = ymax;
  m_zMax = zmax;
  // Resize the mesh.
  m_mesh.resize(m_nX);
  for (unsigned int i = 0; i < m_nX; ++i) {
    m_mesh[i].resize(m_nY);
    for (unsigned int j = 0; j < m_nY; ++j) {
      m_mesh[i][j].resize(m_nZ);
      for (unsigned int k = 0; k < m_nZ; ++k) {
        m_mesh[i][j][k].ex = 0.;
        m_mesh[i][j][k].ey = 0.;
        m_mesh[i][j][k].ez = 0.;
        m_mesh[i][j][k].v = 0.;
        m_mesh[i][j][k].region = -1;
      }
    }
  }
  m_hasMesh = true;
}

bool ComponentVoxel::LoadData(const std::string filename, std::string format,
                              const bool withPotential, const bool withRegion,
                              const double scaleX, const double scaleE,
                              const double scaleP) {

  if (!m_hasMesh) {
    std::cerr << m_className << "::LoadData:\n";
    std::cerr << "    Mesh is not set. Call SetMesh first.\n";
    return false;
  }
  m_ready = false;
  m_hasPotential = m_hasField = false;
  m_pMin = m_pMax = 0.;
  if (withPotential) {
    m_pMin = 1.;
    m_pMax = -1.;
  }

  unsigned int nValues = 0;
  std::vector<std::vector<std::vector<bool> > > isSet;
  isSet.resize(m_nX);
  for (unsigned int i = 0; i < m_nX; ++i) {
    isSet[i].resize(m_nY);
    for (unsigned int j = 0; j < m_nY; ++j) {
      isSet[i][j].resize(m_nZ, false);
    }
  }
  std::ifstream infile;
  infile.open(filename.c_str(), std::ios::in);
  if (!infile) {
    std::cerr << m_className << "::LoadData:\n";
    std::cerr << "    Could not open file " << filename << ".\n";
    return false;
  }

  std::transform(format.begin(), format.end(), format.begin(), toupper);
  unsigned int fmt = 0;
  if (format == "XY") {
    fmt = 1;
  } else if (format == "XYZ") {
    fmt = 2;
  } else if (format == "IJ") {
    fmt = 3;
  } else if (format == "IJK") {
    fmt = 4;
  } else if (format == "YXZ") {
    fmt = 5;
  } else {
    std::cerr << m_className << "::LoadData:\n";
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
    line.erase(line.begin(),
               std::find_if(line.begin(), line.end(),
                            not1(std::ptr_fun<int, int>(isspace))));
    // Skip empty lines.
    if (line.empty()) continue;
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
    if (fmt == 1) {
      // "XY"
      double x, y;
      data >> x >> y;
      if (data.fail()) {
        std::cerr << m_className << "::LoadData:\n";
        std::cerr << "    Error reading line " << nLines << ".\n";
        std::cerr << "    Cannot retrieve element coordinates.\n";
        bad = true;
        break;
      }
      x *= scaleX;
      y *= scaleX;
      const double z = 0.5 * (m_zMin + m_zMax); 
      bool xMirrored, yMirrored, zMirrored;
      if (!GetElement(x, y, z, i, j, k, xMirrored, yMirrored, zMirrored)) {
        std::cerr << m_className << "::LoadData:\n";
        std::cerr << "    Error reading line " << nLines << ".\n";
        std::cerr << "    Point is outside mesh.\n";
        bad = true;
        break;
      }
    } else if (fmt == 2) {
      // "XYZ"
      double x, y, z;
      data >> x >> y >> z;
      if (data.fail()) {
        std::cerr << m_className << "::LoadData:\n";
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
        std::cerr << m_className << "::LoadData:\n";
        std::cerr << "    Error reading line " << nLines << ".\n";
        std::cerr << "    Point is outside mesh.\n";
        bad = true;
        break;
      }
    } else if (fmt == 3) {
      // "IJ"
      k = 0;
      data >> i >> j;
      if (data.fail()) {
        std::cerr << m_className << "::LoadData:\n";
        std::cerr << "    Error reading line " << nLines << ".\n";
        std::cerr << "    Cannot retrieve element index.\n";
        bad = true;
        break;
      }
    } else if (fmt == 4) {
      // "IJK"
      data >> i >> j >> k;
      if (data.fail()) {
        std::cerr << m_className << "::LoadData:\n";
        std::cerr << "    Error reading line " << nLines << ".\n";
        std::cerr << "    Cannot retrieve element index.\n";
        bad = true;
        break;
      }
    } else if (fmt == 5) {
      // "YXZ"
      // To obtain a right handed coordinate system we need z->-z with switching of Y and X. 
      double x, y, z, temp;
      data >> y >> x >> temp;
      z = temp;
      if (data.fail()) {
        std::cerr << m_className << "::LoadData:\n";
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
        std::cerr << m_className << "::LoadData:\n";
        std::cerr << "    Error reading line " << nLines << ".\n";
        std::cerr << "    Point is outside mesh.\n";
        bad = true;
        break;
      }
    }  
    // Check the indices.
    if (i >= m_nX || j >= m_nY || k >= m_nZ) {
      std::cerr << m_className << "::LoadData:\n";
      std::cerr << "    Error reading line " << nLines << ".\n";
      std::cerr << "    Index (" << i << ", " << j << ", " << k
                << ") out of range.\n";
      continue;
    }
    if (isSet[i][j][k]) {
      std::cerr << m_className << "::LoadData:\n";
      std::cerr << "    Error reading line " << nLines << ".\n";
      std::cerr << "    Mesh element (" << i << ", " << j << ", " << k
                << ") has already been set.\n";
      continue;
    }
    // Get the electric field values.
    if (fmt == 1 || fmt == 3) {
      // Two-dimensional field-map
      ez = 0.;
      data >> ex >> ey;
    } else if (fmt == 5){
      double temp;
      data >> ey >> ex >> temp;
      ez = temp;
    } else {
      data >> ex >> ey >> ez;
    }
    if (data.fail()) {
      std::cerr << m_className << "::LoadData:\n";
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
        std::cerr << m_className << "::LoadData:\n";
        std::cerr << "    Error reading line " << nLines << ".\n";
        std::cerr << "    Cannot read potential.\n";
        bad = true;
        break;
      }
      v *= scaleP;
      if (m_pMin > m_pMax) {
        // First value.
        m_pMin = v;
        m_pMax = v;
      } else {
        if (v < m_pMin) m_pMin = v;
        if (v > m_pMax) m_pMax = v;
      }
    }
    if (withRegion) {
      data >> region;
      if (data.fail()) {
        std::cerr << m_className << "::LoadData:\n";
        std::cerr << "    Error reading line " << nLines << ".\n";
        std::cerr << "    Cannot read region.\n";
        bad = true;
        break;
      }
    }
    if (fmt == 1 || fmt == 3) {
      // Two-dimensional field-map
      for (unsigned int kk = 0; kk < m_nZ; ++kk) {
        m_mesh[i][j][kk].ex = ex;
        m_mesh[i][j][kk].ey = ey;
        m_mesh[i][j][kk].ez = ez;
        m_mesh[i][j][kk].v = v;
        m_mesh[i][j][kk].region = region;
        isSet[i][j][kk] = true;
      }
    } else {
      m_mesh[i][j][k].ex = ex;
      m_mesh[i][j][k].ey = ey;
      m_mesh[i][j][k].ez = ez;
      m_mesh[i][j][k].v = v;
      m_mesh[i][j][k].region = region;
      isSet[i][j][k] = true;
    }
    ++nValues;
  }
  if (bad) return false;
  std::cout << m_className << "::LoadData:\n";
  std::cout << "    Read " << nValues << " values from file " << filename
            << ".\n";
  unsigned int nExpected = m_nX * m_nY;
  if (fmt == 2 || fmt == 4 || fmt == 5) nExpected *= m_nZ;
  if (nExpected != nValues) {
    std::cerr << m_className << "::LoadData:\n";
    std::cerr << "   Expected " << nExpected << " values.\n";
  }
  m_ready = true;
  return true;
}

bool ComponentVoxel::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                                    double& xmax, double& ymax, double& zmax) {

  if (!m_ready) return false;
  if (m_xPeriodic || m_xMirrorPeriodic) {
    xmin = -INFINITY;
    xmax = +INFINITY;
  } else {
    xmin = m_xMin;
    xmax = m_xMax;
  }

  if (m_yPeriodic || m_yMirrorPeriodic) {
    ymin = -INFINITY;
    ymax = +INFINITY;
  } else {
    ymin = m_yMin;
    ymax = m_yMax;
  }

  if (m_zPeriodic || m_zMirrorPeriodic) {
    zmin = -INFINITY;
    zmax = +INFINITY;
  } else {
    zmin = m_zMin;
    zmax = m_zMax;
  }
  return true;
}

bool ComponentVoxel::GetVoltageRange(double& vmin, double& vmax) {

  if (!m_ready) return false;
  vmin = m_pMin;
  vmax = m_pMax;
  return true;
}

bool ComponentVoxel::GetElectricFieldRange(double& exmin, double& exmax,
                                           double& eymin, double& eymax,
                                           double& ezmin, double& ezmax) {

  if (!m_ready) {
    std::cerr << m_className << "::GetElectricFieldRange:\n";
    std::cerr << "    Field map not available.\n";
    return false;
  }
  bool gotValue = false;
  for (unsigned int i = 0; i < m_nX; ++i) {
    for (unsigned int j = 0; j < m_nY; ++j) {
      for (unsigned int k = 0; k < m_nZ; ++k) {
        if (!gotValue) {
          exmin = m_mesh[i][j][k].ex;
          exmax = m_mesh[i][j][k].ex;
          eymin = m_mesh[i][j][k].ey;
          eymax = m_mesh[i][j][k].ey;
          ezmin = m_mesh[i][j][k].ez;
          ezmax = m_mesh[i][j][k].ez;
          gotValue = true;
          continue;
        }
        if (m_mesh[i][j][k].ex < exmin) exmin = m_mesh[i][j][k].ex;
        if (m_mesh[i][j][k].ex > exmax) exmax = m_mesh[i][j][k].ex;
        if (m_mesh[i][j][k].ey < eymin) eymin = m_mesh[i][j][k].ey;
        if (m_mesh[i][j][k].ey > eymax) eymax = m_mesh[i][j][k].ey;
        if (m_mesh[i][j][k].ez < ezmin) ezmin = m_mesh[i][j][k].ez;
        if (m_mesh[i][j][k].ez > ezmax) ezmax = m_mesh[i][j][k].ez;
      }
    }
  }
  return true;
}

void ComponentVoxel::PrintRegions() {

  // Do not proceed if not properly initialised.
  if (!m_ready) {
    std::cerr << m_className << "::PrintRegions:\n";
    std::cerr << "    Field map not yet initialised.\n";
    return;
  }

  if (m_media.size() < 1) {
    std::cerr << m_className << "::PrintRegions:\n";
    std::cerr << "    No regions are currently defined.\n";
    return;
  }

  std::cout << m_className << "::PrintRegions:\n";
  if (m_media.size() == 1) {
    std::cout << "    1 region is defined.\n";
  } else {
    std::cout << "    " << m_media.size() << " regions are defined.\n";
  }
  std::cout << "      Index     Medium\n";
  std::map<int, Medium*>::iterator it;
  for (it = m_media.begin(); it != m_media.end(); ++it) {
    const int i = (*it).first;
    Medium* m = (*it).second;
    std::cout << "      " << i << "      ";
    if (m == NULL) {
      std::cout << "      none\n";
    } else {
      std::cout << "      " << m->GetName() << "\n";
    }
  }
}

void ComponentVoxel::SetMedium(const int i, Medium* m) {

  if (m == NULL) {
    std::cerr << m_className << "::SetMedium:\n";
    std::cerr << "    Warning: medium pointer is null.\n";
  }
  m_media[i] = m;
}

Medium* ComponentVoxel::GetMedium(const unsigned int& i) {

  if (m_media.count(i) < 1) {
    std::cerr << m_className << "::GetMedium:\n";
    std::cerr << "    Medium " << i << " does not exist.\n";
    return NULL;
  }

  return m_media[i];
}

bool ComponentVoxel::GetElement(const double xi, const double yi,
                                const double zi, unsigned int& i,
                                unsigned int& j, unsigned int& k,
                                bool& xMirrored, bool& yMirrored,
                                bool& zMirrored) {

  if (!m_hasMesh) {
    std::cerr << m_className << "::GetElement:\n";
    std::cerr << "    Mesh is not set.\n";
    return false;
  }

  double x = xi, y = yi, z = zi;
  xMirrored = yMirrored = zMirrored = false;
  // In case of periodicity, reduce to the basic cell.
  const double cellsx = m_xMax - m_xMin;
  if (m_xPeriodic) {
    x = m_xMin + fmod(x - m_xMin, cellsx);
    if (x < m_xMin) x += cellsx;
  } else if (m_xMirrorPeriodic) {
    double xNew = m_xMin + fmod(x - m_xMin, cellsx);
    if (xNew < m_xMin) xNew += cellsx;
    int nx = int(floor(0.5 + (xNew - x) / cellsx));
    if (nx != 2 * (nx / 2)) {
      xNew = m_xMin + m_xMax - xNew;
      xMirrored = true;
    }
    x = xNew;
  }
  // Check if the point is outside the mesh.
  if (x < m_xMin || x > m_xMax) return false;

  const double cellsy = m_yMax - m_yMin;
  if (m_yPeriodic) {
    y = m_yMin + fmod(y - m_yMin, cellsy);
    if (y < m_yMin) y += cellsy;
  } else if (m_yMirrorPeriodic) {
    double yNew = m_yMin + fmod(y - m_yMin, cellsy);
    if (yNew < m_yMin) yNew += cellsy;
    int ny = int(floor(0.5 + (yNew - y) / cellsy));
    if (ny != 2 * (ny / 2)) {
      yNew = m_yMin + m_yMax - yNew;
      yMirrored = true;
    }
    y = yNew;
  }
  // Check if the point is outside the mesh.
  if (y < m_yMin || y > m_yMax) return false;

  const double cellsz = m_zMax - m_zMin;
  if (m_zPeriodic) {
    z = m_zMin + fmod(z - m_zMin, cellsz);
    if (z < m_zMin) z += cellsz;
  } else if (m_zMirrorPeriodic) {
    double zNew = m_zMin + fmod(z - m_zMin, cellsz);
    if (zNew < m_zMin) zNew += cellsz;
    int nz = int(floor(0.5 + (zNew - z) / cellsz));
    if (nz != 2 * (nz / 2)) {
      zNew = m_zMin + m_zMax - zNew;
      zMirrored = true;
    }
    z = zNew;
  }
  // Check if the point is outside the mesh.
  if (z < m_zMin || z > m_zMax) return false;

  // Get the indices.
  const double dx = (m_xMax - m_xMin) / m_nX;
  const double dy = (m_yMax - m_yMin) / m_nY;
  const double dz = (m_zMax - m_zMin) / m_nZ;
  i = (unsigned int)((x - m_xMin) / dx);
  j = (unsigned int)((y - m_yMin) / dy);
  k = (unsigned int)((z - m_zMin) / dz);
  if (i >= m_nX) i = m_nX - 1;
  if (j >= m_nY) j = m_nY - 1;
  if (k >= m_nZ) k = m_nZ - 1;
  return true;
}

bool ComponentVoxel::GetElement(const unsigned int i, const unsigned int j,
                                const unsigned int k, double& v, double& ex,
                                double& ey, double& ez) {

  v = ex = ey = ez = 0.;
  if (!m_ready) {
    if (!m_hasMesh) {
      std::cerr << m_className << "::GetElement:\n";
      std::cerr << "    Mesh not set.\n";
      return false;
    }
    std::cerr << m_className << "::GetElement:\n";
    std::cerr << "    Fiel map not set.\n";
    return false;
  }
  if (i >= m_nX || j >= m_nY || k >= m_nZ) {
    std::cerr << m_className << "::GetElement:\n";
    std::cerr << "    Element index out of range.\n";
    return false;
  }
  v = m_mesh[i][j][k].v;
  ex = m_mesh[i][j][k].ex;
  ey = m_mesh[i][j][k].ey;
  ez = m_mesh[i][j][k].ez;
  return true;
}

void ComponentVoxel::Reset() {

  m_mesh.clear();
  m_nX = m_nY = m_nZ = 0;
  m_xMin = m_yMin = m_zMin = 0.;
  m_xMax = m_yMax = m_zMax = 0.;
  m_pMin = m_pMax = 0.;
  m_media.clear();

  m_hasMesh = false;
  m_hasPotential = false;
  m_hasField = false;

  m_ready = false;
}

void ComponentVoxel::UpdatePeriodicity() {

  if (!m_ready) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Field map not available.\n";
    return;
  }

  // Check for conflicts.
  if (m_xPeriodic && m_xMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along x requested; reset.\n";
    m_xPeriodic = m_xMirrorPeriodic = false;
  }

  if (m_yPeriodic && m_yMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along y requested; reset.\n";
    m_yPeriodic = m_yMirrorPeriodic = false;
  }

  if (m_zPeriodic && m_zMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along z requested; reset.\n";
    m_zPeriodic = m_zMirrorPeriodic = false;
  }

  if (m_xAxiallyPeriodic || m_yAxiallyPeriodic || m_zAxiallyPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Axial symmetry is not supported; reset.\n";
    m_xAxiallyPeriodic = m_yAxiallyPeriodic = m_zAxiallyPeriodic = false;
  }

  if (m_xRotationSymmetry || m_yRotationSymmetry || m_zRotationSymmetry) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Rotation symmetry is not supported; reset.\n";
    m_xRotationSymmetry = m_yRotationSymmetry = m_zRotationSymmetry = false;
  }
}
}
