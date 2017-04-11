#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <algorithm>
#include <cmath>

#include "ComponentTcad2d.hh"

namespace {

void ltrim(std::string& line) {
  line.erase(line.begin(), find_if(line.begin(), line.end(),
                                   not1(std::ptr_fun<int, int>(isspace))));
}

}
namespace Garfield {

ComponentTcad2d::ComponentTcad2d()
    : ComponentBase(),
      m_hasPotential(false),
      m_hasField(false),
      m_hasElectronMobility(false),
      m_hasHoleMobility(false),
      m_hasElectronVelocity(false),
      m_hasHoleVelocity(false),
      m_hasElectronLifetime(false),
      m_hasHoleLifetime(false),
      m_validTraps(false),
      m_pMin(0.),
      m_pMax(0.),
      m_hasRangeZ(false),
      m_lastElement(0) {

  m_className = "ComponentTcad2d";

  m_regions.reserve(10);
  m_vertices.reserve(10000);
  m_elements.reserve(10000);
}


bool ComponentTcad2d::SetDonor(const unsigned int donorNumber, 
                               const double eXsec, const double hXsec,
                               const double conc) {


  if (donorNumber >= m_donors.size()) {
    std::cerr << m_className << "::SetDonor:\n"
              << "    Index (" << donorNumber << ") out of range.\n";
    return false;
  }
  m_donors[donorNumber].xsece = eXsec;
  m_donors[donorNumber].xsech = hXsec;
  m_donors[donorNumber].conc = conc;

  m_validTraps = CheckTraps();
  return true;
}

bool ComponentTcad2d::SetAcceptor(const unsigned int acceptorNumber, 
                                  const double eXsec, const double hXsec,
                                  const double conc) {

  if (acceptorNumber >= m_acceptors.size()) {
    std::cerr << m_className << "::SetAcceptor:\n"
              << "    Index (" << acceptorNumber << ") out of range.\n";
    return false;
  }
  m_acceptors[acceptorNumber].xsece = eXsec;
  m_acceptors[acceptorNumber].xsech = hXsec;
  m_acceptors[acceptorNumber].conc = conc;

  m_validTraps = CheckTraps();
  return true;
}

bool ComponentTcad2d::ElectronAttachment(const double x, const double y, 
                                         const double z, 
                                         double& eta) {
  eta = 0.;
  if (!m_validTraps) {
    std::cerr << m_className << "::ElectronAttachment:\n"
              << "    Trap cross-sections or concentrations are invalid.\n";
    return false;
  }

  if (m_donors.empty() && m_acceptors.empty()) {
    std::cerr << m_className << "::ElectronAttachment:\n"
              << "    There are no traps defined.\n";
    return false;
  }

  const unsigned int nAcceptors = m_acceptors.size(); 
  for (unsigned int i = 0; i < nAcceptors; ++i) {
    const Defect& acceptor = m_acceptors[i];
    double f = 0.;
    GetAcceptorOccupation(x, y, z, i, f);
	  eta += acceptor.conc * acceptor.xsece * (1. - f);
  }
  const unsigned int nDonors = m_donors.size();
  for (unsigned int i = 0; i < nDonors; ++i) {
    const Defect& donor = m_donors[i];
    double f = 0.;
    GetDonorOccupation(x, y, z, i, f);
	  eta += donor.conc * donor.xsece * f;
  }
  return true;
}

bool ComponentTcad2d::HoleAttachment(const double x, const double y, 
                                     const double z,
                                     double& eta) {
  eta = 0.;
  if (!m_validTraps) {
    std::cerr << m_className << "::HoleAttachment:\n"
              << "    Trap cross-sections or concentrations are invalid.\n";
    return false;
  }

  if (m_donors.empty() && m_acceptors.empty()) {
    std::cerr << m_className << "::HoleAttachment:\n"
              << "    There are no traps defined.\n";
    return false;
  }

  const unsigned int nAcceptors = m_acceptors.size(); 
  for (unsigned int i = 0; i < nAcceptors; ++i) {
    const Defect& acceptor = m_acceptors[i];
    double f = 0.;
    GetAcceptorOccupation(x, y, z, i, f);
	  eta += acceptor.conc * acceptor.xsech * f;
  }
  const unsigned int nDonors = m_donors.size();
  for (unsigned int i = 0; i < nDonors; ++i) {
    const Defect& donor = m_donors[i];
    double f = 0.;
    GetDonorOccupation(x, y, z, i, f);
	  eta += donor.conc * donor.xsech * (1. - f);
  }
  return true;
}

void ComponentTcad2d::WeightingField(const double x, const double y, const double z,
                                     double& wx, double& wy, double& wz, 
				                             const std::string& /*label*/) {
  int status = 0;
  Medium* med = NULL;
  double v = 0.;
  ElectricField(x, y, z, wx, wy, wz, v, med, status);
}

void ComponentTcad2d::ElectricField(const double xin, const double yin,
                                    const double zin, double& ex, double& ey,
                                    double& ez, double& p, Medium*& m,
                                    int& status) {

  // Initialise.
  ex = ey = ez = p = 0.;
  m = NULL;

  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::ElectricField:\n"
              << "    Field map is not available for interpolation.\n";
    status = -10;
    return;
  }

  // In case of periodicity, reduce to the cell volume.
  double x = xin, y = yin, z = zin;
  bool xmirr = false, ymirr = false;
  MapCoordinates(x, y, xmirr, ymirr);
  // Check if the point is inside the bounding box.
  if (x < m_xMinBB || x > m_xMaxBB || y < m_yMinBB || y > m_yMaxBB) {
    status = -11;
    return;
  }
  if (m_hasRangeZ && (z < m_zMinBB || z > m_zMaxBB)) {
    status = -11;
    return;
  }

  // Assume this will work.
  status = 0;
  double w[nMaxVertices] = {0};
  if (m_lastElement >= 0) {
    // Check if the point is still located in the previously found element.
    const Element& last = m_elements[m_lastElement];
    if (x >= last.xmin && x <= last.xmax && y >= last.ymin && y <= last.ymax) {
      if (CheckElement(x, y, last, w)) {
        const unsigned int nVertices = last.type + 1;
        for (unsigned int j = 0; j < nVertices; ++j) {
          const Vertex& vj = m_vertices[last.vertex[j]];
          ex += w[j] * vj.ex;
          ey += w[j] * vj.ey;
          p += w[j] * vj.p;
        }
        if (xmirr) ex = -ex;
        if (ymirr) ey = -ey;
        m = m_regions[last.region].medium;
        if (!m_regions[last.region].drift || !m) status = -5;
        return;
      }
    }
    // The point is not in the previous element.
    // Check the adjacent elements.
    const unsigned int nNeighbours = last.neighbours.size();
    for (unsigned int i = 0; i < nNeighbours; ++i) {
      const Element& element = m_elements[last.neighbours[i]];
      if (x < element.xmin || x > element.xmax || 
          y < element.ymin || y > element.ymax) continue; 
      if (!CheckElement(x, y, element, w)) continue;
      const unsigned int nVertices = element.type + 1;
      for (unsigned int j = 0; j < nVertices; ++j) {
        const Vertex& vj = m_vertices[element.vertex[j]];
        ex += w[j] * vj.ex;
        ey += w[j] * vj.ey;
        p += w[j] * vj.p;
      }
      if (xmirr) ex = -ex;
      if (ymirr) ey = -ey;
      m = m_regions[element.region].medium;
      if (!m_regions[element.region].drift || !m) status = -5;
      m_lastElement = last.neighbours[i];
      return;
    }
  }

  // The point is not in the previous element nor in the adjacent ones.
  // We have to loop over all elements.
  const unsigned int nElements = m_elements.size();
  for (unsigned int i = 0; i < nElements; ++i) {
    const Element& element = m_elements[i];
    if (x < element.xmin || x > element.xmax ||
        y < element.ymin || y > element.ymax) continue;
    if (!CheckElement(x, y, element, w)) continue;
    const unsigned int nVertices = element.type + 1;
    for (unsigned int j = 0; j < nVertices; ++j) {
      const Vertex& vj = m_vertices[element.vertex[j]];
      ex += w[j] * vj.ex;
      ey += w[j] * vj.ey;
      p += w[j] * vj.p;
    }
    if (xmirr) ex = -ex;
    if (ymirr) ey = -ey;
    m = m_regions[element.region].medium;
    if (!m_regions[element.region].drift || !m) status = -5;
    m_lastElement = i;
    return;
  }
  // Point is outside the mesh.
  if (m_debug) {
    std::cerr << m_className << "::ElectricField:\n"
              << "    Point (" << x << ", " << y << ") is outside the mesh.\n";
  }
  status = -6;
}

void ComponentTcad2d::ElectronVelocity(const double xin, const double yin,
                                       const double zin, 
                                       double& vx, double& vy, double& vz, 
                                       Medium*& m, int& status) {

  // Initialise.
  vx = vy = vz = 0.;
  m = NULL;
  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::ElectronVelocity:\n";
    std::cerr << "    Field map is not available for interpolation.\n";
    status = -10;
    return;
  }

  double x = xin, y = yin, z = zin;
  // In case of periodicity, reduce to the cell volume.
  bool xmirr = false, ymirr = false;
  MapCoordinates(x, y, xmirr, ymirr);
  if (x < m_xMinBB || x > m_xMaxBB || y < m_yMinBB || y > m_yMaxBB) {
    status = -11;
    return;
  }
  if (m_hasRangeZ && (z < m_zMinBB || z > m_zMaxBB)) {
    status = -11;
    return;
  }

  // Assume this will work.
  status = 0;
  double w[nMaxVertices] = {0};
  if (m_lastElement >= 0) {
    // Check if the point is still located in the previously found element.
    const Element& last = m_elements[m_lastElement];
    if (x >= last.xmin && x <= last.xmax && y >= last.ymin && y <= last.ymax) {
      if (CheckElement(x, y, last, w)) {
        const unsigned int nVertices = last.type + 1;
        for (unsigned int j = 0; j < nVertices; ++j) {
          const Vertex& vj = m_vertices[last.vertex[j]];
          vx += w[j] * vj.eVx;
          vy += w[j] * vj.eVy;
        }
        if (xmirr) vx = -vx;
        if (ymirr) vy = -vy;
        m = m_regions[last.region].medium;
        if (!m_regions[last.region].drift || !m) status = -5;
        return;
      }
    }
    // The point is not in the previous element.
    // Check the adjacent elements.
    const unsigned int nNeighbours = last.neighbours.size();
    for (unsigned int i = 0; i < nNeighbours; ++i) {
      const Element& element = m_elements[last.neighbours[i]];
      if (x < element.xmin || x > element.xmax || 
          y < element.ymin || y > element.ymax) continue; 
      if (!CheckElement(x, y, element, w)) continue;
      const unsigned int nVertices = element.type + 1;
      for (unsigned int j = 0; j < nVertices; ++j) {
        const Vertex& vj = m_vertices[element.vertex[j]];
        vx += w[j] * vj.eVx;
        vy += w[j] * vj.eVy;
      }
      if (xmirr) vx = -vx;
      if (ymirr) vy = -vy;
      m = m_regions[element.region].medium;
      if (!m_regions[element.region].drift || !m) status = -5;
      m_lastElement = last.neighbours[i];
      return;
    }
  }

  // The point is not in the previous element nor in the adjacent ones.
  // We have to loop over all elements.
  const unsigned int nElements = m_elements.size();
  for (unsigned int i = 0; i < nElements; ++i) {
    const Element& element = m_elements[i];
    if (x < element.xmin || x > element.xmax ||
        y < element.ymin || y > element.ymax) continue;
    if (!CheckElement(x, y, element, w)) continue;
    const unsigned int nVertices = element.type + 1;
    for (unsigned int j = 0; j < nVertices; ++j) {
      const Vertex& vj = m_vertices[element.vertex[j]];
      vx += w[j] * vj.eVx;
      vy += w[j] * vj.eVy;
    }
    if (xmirr) vx = -vx;
    if (ymirr) vy = -vy;
    m = m_regions[element.region].medium;
    if (!m_regions[element.region].drift || !m) status = -5;
    m_lastElement = i;
    return;
  }
  // Point is outside the mesh.
  if (m_debug) {
    std::cerr << m_className << "::ElectronVelocity:\n"
              << "    Point (" << x << ", " << y << ") is outside the mesh.\n";
  }
  status = -6;
}

void ComponentTcad2d::HoleVelocity(const double xin, const double yin,
                                   const double zin, 
                                   double& vx, double& vy, double& vz,
                                   Medium*& m, int& status) {

  // Initialise.
  vx = vy = vz = 0.;
  m = NULL;

  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::HoleVelocity:\n"
              << "    Field map is not available for interpolation.\n";
    status = -10;
    return;
  }

  double x = xin, y = yin, z = zin;
  // In case of periodicity, reduce to the cell volume.
  bool xmirr = false, ymirr = false;
  MapCoordinates(x, y, xmirr, ymirr);
  if (x < m_xMinBB || x > m_xMaxBB || y < m_yMinBB || y > m_yMaxBB) {
    status = -11;
    return;
  }
  if (m_hasRangeZ && (z < m_zMinBB || z > m_zMaxBB)) {
    status = -11;
    return;
  }

  // Assume this will work.
  status = 0;

  double w[nMaxVertices] = {0};
  if (m_lastElement >= 0) {
    // Check if the point is still located in the previously found element.
    const Element& last = m_elements[m_lastElement];
    if (x >= last.xmin && x <= last.xmax && y >= last.ymin && y <= last.ymax) {
      if (CheckElement(x, y, last, w)) {
        const unsigned int nVertices = last.type + 1;
        for (unsigned int j = 0; j < nVertices; ++j) {
          const Vertex& vj = m_vertices[last.vertex[j]];
          vx += w[j] * vj.hVx;
          vy += w[j] * vj.hVy;
        }
        if (xmirr) vx = -vx;
        if (ymirr) vy = -vy;
        m = m_regions[last.region].medium;
        if (!m_regions[last.region].drift || !m) status = -5;
        return;
      }
    }
    // The point is not in the previous element.
    // Check the adjacent elements.
    const unsigned int nNeighbours = last.neighbours.size();
    for (unsigned int i = 0; i < nNeighbours; ++i) {
      const Element& element = m_elements[last.neighbours[i]];
      if (x < element.xmin || x > element.xmax || 
          y < element.ymin || y > element.ymax) continue; 
      if (!CheckElement(x, y, element, w)) continue;
      const unsigned int nVertices = element.type + 1;
      for (unsigned int j = 0; j < nVertices; ++j) {
        const Vertex& vj = m_vertices[element.vertex[j]];
        vx += w[j] * vj.hVx;
        vy += w[j] * vj.hVy;
      }
      if (xmirr) vx = -vx;
      if (ymirr) vy = -vy;
      m = m_regions[element.region].medium;
      if (!m_regions[element.region].drift || !m) status = -5;
      m_lastElement = last.neighbours[i];
      return;
    }
  }

  // The point is not in the previous element nor in the adjacent ones.
  // We have to loop over all elements.
  const unsigned int nElements = m_elements.size();
  for (unsigned int i = 0; i < nElements; ++i) {
    const Element& element = m_elements[i];
    if (x < element.xmin || x > element.xmax ||
        y < element.ymin || y > element.ymax) continue;
    if (!CheckElement(x, y, element, w)) continue;
    const unsigned int nVertices = element.type + 1;
    for (unsigned int j = 0; j < nVertices; ++j) {
      const Vertex& vj = m_vertices[element.vertex[j]];
      vx += w[j] * vj.hVx;
      vy += w[j] * vj.hVy;
    }
    if (xmirr) vx = -vx;
    if (ymirr) vy = -vy;
    m = m_regions[element.region].medium;
    if (!m_regions[element.region].drift || !m) status = -5;
    m_lastElement = i;
    return;
  }

  // Point is outside the mesh.
  if (m_debug) {
    std::cerr << m_className << "::HoleVelocity:\n"
              << "    Point (" << x << ", " << y << ") is outside the mesh.\n";
  }
  status = -6;
}

Medium* ComponentTcad2d::GetMedium(const double xin, const double yin,
                                   const double zin) {

  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::GetMedium:\n"
              << "    Field map not available for interpolation.\n";
    return NULL;
  }

  double x = xin, y = yin, z = zin;
  // In case of periodicity, reduce to the cell volume.
  bool xmirr = false, ymirr = false;
  MapCoordinates(x, y, xmirr, ymirr);
  // Check if the point is inside the bounding box.
  if (x < m_xMinBB || x > m_xMaxBB || y < m_yMinBB || y > m_yMaxBB) {
    return NULL;
  }
  if (m_hasRangeZ && (z < m_zMinBB || z > m_zMaxBB)) return NULL;

  // Shape functions
  double w[nMaxVertices] = {0};
  if (m_lastElement >= 0) {
    // Check if the point is still located in the previously found element.
    const Element& last = m_elements[m_lastElement];
    if (x >= last.xmin && x <= last.xmax && y >= last.ymin && y <= last.ymax &&
        CheckElement(x, y, last, w)) {
      return m_regions[last.region].medium;
    }

    // The point is not in the previous element.
    // Check the adjacent elements.
    const unsigned int nNeighbours = last.neighbours.size();
    for (unsigned int i = 0; i < nNeighbours; ++i) {
      const Element& element = m_elements[last.neighbours[i]];
      if (x < element.xmin || x > element.xmax ||
          y < element.ymin || y > element.ymax) continue; 
      if (!CheckElement(x, y, element, w)) continue;
      m_lastElement = last.neighbours[i];
      return m_regions[element.region].medium;
    }
  }

  // The point is not in the previous element nor in the adjacent ones.
  // We have to loop over all elements.
  const unsigned int nElements = m_elements.size();
  for (unsigned int i = 0; i < nElements; ++i) {
    const Element& element = m_elements[i];
    if (x < element.xmin || x > element.xmax ||
        y < element.ymin || y > element.ymax) continue; 
    if (!CheckElement(x, y, element, w)) continue;
    m_lastElement = i;
    return m_regions[element.region].medium;
  }

  // Point is outside the mesh.
  return NULL;
}

bool ComponentTcad2d::GetElectronLifetime(const double xin, const double yin, 
                                          const double zin, double& tau) {

  tau  = 0.;
  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::GetElectronLifetime:\n"
              << "    Field map is not available for interpolation.\n";
    return false;
  }

  double x = xin, y = yin, z = zin;
  // In case of periodicity, reduce to the cell volume.
  bool xmirr = false, ymirr = false;
  MapCoordinates(x, y, xmirr, ymirr);
  // Check if the point is inside the bounding box.
  if (x < m_xMinBB || x > m_xMaxBB || y < m_yMinBB || y > m_yMaxBB) {
    return false;
  }
  if (m_hasRangeZ && (z < m_zMinBB || z > m_zMaxBB)) return false;

  double w[nMaxVertices] = {0};
  if (m_lastElement >= 0) {
    // Check if the point is still located in the previously found element.
    const Element& last = m_elements[m_lastElement];
    if (x >= last.xmin && x <= last.xmax && y >= last.ymin && y <= last.ymax) {
      if (CheckElement(x, y, last, w)) {
        const unsigned int nVertices = last.type + 1;
        for (unsigned int j = 0; j < nVertices; ++j) {
          const Vertex& vj = m_vertices[last.vertex[j]];
          tau += w[j] * vj.eTau;
        }
        return true;
      }
    }
    // The point is not in the previous element.
    // Check the adjacent elements.
    const unsigned int nNeighbours = last.neighbours.size();
    for (unsigned int i = 0; i < nNeighbours; ++i) {
      const Element& element = m_elements[last.neighbours[i]];
      if (x < element.xmin || x > element.xmax || 
          y < element.ymin || y > element.ymax) continue; 
      if (!CheckElement(x, y, element, w)) continue;
      const unsigned int nVertices = element.type + 1;
      for (unsigned int j = 0; j < nVertices; ++j) {
        const Vertex& vj = m_vertices[element.vertex[j]];
        tau += w[j] * vj.eTau;
      }
      m_lastElement = last.neighbours[i];
      return true;
    }
  }

  // The point is not in the previous element nor in the adjacent ones.
  // We have to loop over all elements.
  const unsigned int nElements = m_elements.size();
  for (unsigned int i = 0; i < nElements; ++i) {
    const Element& element = m_elements[i];
    if (x < element.xmin || x > element.xmax ||
        y < element.ymin || y > element.ymax) continue;
    if (!CheckElement(x, y, element, w)) continue;
    const unsigned int nVertices = element.type + 1;
    for (unsigned int j = 0; j < nVertices; ++j) {
      const Vertex& vj = m_vertices[element.vertex[j]];
      tau += w[j] * vj.eTau;
    }
    m_lastElement = i;
    return true;
  }

  // Point is outside the mesh.
  if (m_debug) {
    std::cerr << m_className << "::GetElectronLifetime:\n"
              << "    Point (" << x << ", " << y << ") is outside the mesh.\n";
  }
  return false;

}
bool ComponentTcad2d::GetHoleLifetime(const double xin, const double yin, 
                                      const double zin, double& tau) {

  tau  = 0.;
  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::GetHoleLifetime:\n"
              << "    Field map is not available for interpolation.\n";
    return false;
  }

  double x = xin, y = yin, z = zin;
  // In case of periodicity, reduce to the cell volume.
  bool xmirr = false, ymirr = false;
  MapCoordinates(x, y, xmirr, ymirr);
  // Check if the point is inside the bounding box.
  if (x < m_xMinBB || x > m_xMaxBB || y < m_yMinBB || y > m_yMaxBB) {
    return false;
  }
  if (m_hasRangeZ && (z < m_zMinBB || z > m_zMaxBB)) return false;

  double w[nMaxVertices] = {0};
  if (m_lastElement >= 0) {
    // Check if the point is still located in the previously found element.
    const Element& last = m_elements[m_lastElement];
    if (x >= last.xmin && x <= last.xmax && y >= last.ymin && y <= last.ymax) {
      if (CheckElement(x, y, last, w)) {
        const unsigned int nVertices = last.type + 1;
        for (unsigned int j = 0; j < nVertices; ++j) {
          const Vertex& vj = m_vertices[last.vertex[j]];
          tau += w[j] * vj.hTau;
        }
        return true;
      }
    }
    // The point is not in the previous element.
    // Check the adjacent elements.
    const unsigned int nNeighbours = last.neighbours.size();
    for (unsigned int i = 0; i < nNeighbours; ++i) {
      const Element& element = m_elements[last.neighbours[i]];
      if (x < element.xmin || x > element.xmax || 
          y < element.ymin || y > element.ymax) continue; 
      if (!CheckElement(x, y, element, w)) continue;
      const unsigned int nVertices = element.type + 1;
      for (unsigned int j = 0; j < nVertices; ++j) {
        const Vertex& vj = m_vertices[element.vertex[j]];
        tau += w[j] * vj.hTau;
      }
      m_lastElement = last.neighbours[i];
      return true;
    }
  }

  // The point is not in the previous element nor in the adjacent ones.
  // We have to loop over all elements.
  const unsigned int nElements = m_elements.size();
  for (unsigned int i = 0; i < nElements; ++i) {
    const Element& element = m_elements[i];
    if (x < element.xmin || x > element.xmax ||
        y < element.ymin || y > element.ymax) continue;
    if (!CheckElement(x, y, element, w)) continue;
    const unsigned int nVertices = element.type + 1;
    for (unsigned int j = 0; j < nVertices; ++j) {
      const Vertex& vj = m_vertices[element.vertex[j]];
      tau += w[j] * vj.hTau;
    }
    m_lastElement = i;
    return true;
  }

  // Point is outside the mesh.
  if (m_debug) {
    std::cerr << m_className << "::GetHoleLifetime:\n"
              << "    Point (" << x << ", " << y << ") is outside the mesh.\n";
  }
  return false;

}

bool ComponentTcad2d::GetMobility(const double xin, const double yin,
                                  const double zin, 
                                  double& emob, double& hmob) {

  emob = hmob = 0.;

  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::GetMobility:\n"
              << "    Field map is not available for interpolation.\n";
    return false;
  }

  double x = xin, y = yin, z = zin;
  // In case of periodicity, reduce to the cell volume.
  bool xmirr = false, ymirr = false;
  MapCoordinates(x, y, xmirr, ymirr);
  // Check if the point is inside the bounding box.
  if (x < m_xMinBB || x > m_xMaxBB || y < m_yMinBB || y > m_yMaxBB) {
    return false;
  }
  if (m_hasRangeZ && (z < m_zMinBB || z > m_zMaxBB)) {
    return false;
  }

  double w[nMaxVertices] = {0};
  if (m_lastElement >= 0) {
    // Check if the point is still located in the previously found element.
    const Element& last = m_elements[m_lastElement];
    if (x >= last.xmin && x <= last.xmax && y >= last.ymin && y <= last.ymax) {
      if (CheckElement(x, y, last, w)) {
        const unsigned int nVertices = last.type + 1;
        for (unsigned int j = 0; j < nVertices; ++j) {
          const Vertex& vj = m_vertices[last.vertex[j]];
          emob += w[j] * vj.emob;
          hmob += w[j] * vj.hmob;
        }
        return true;
      }
    }
    // The point is not in the previous element.
    // Check the adjacent elements.
    const unsigned int nNeighbours = last.neighbours.size();
    for (unsigned int i = 0; i < nNeighbours; ++i) {
      const Element& element = m_elements[last.neighbours[i]];
      if (x < element.xmin || x > element.xmax || 
          y < element.ymin || y > element.ymax) continue; 
      if (!CheckElement(x, y, element, w)) continue;
      const unsigned int nVertices = element.type + 1;
      for (unsigned int j = 0; j < nVertices; ++j) {
        const Vertex& vj = m_vertices[element.vertex[j]];
        emob += w[j] * vj.emob;
        hmob += w[j] * vj.hmob;
      }
      m_lastElement = last.neighbours[i];
      return true;
    }
  }

  // The point is not in the previous element nor in the adjacent ones.
  // We have to loop over all elements.
  const unsigned int nElements = m_elements.size();
  for (unsigned int i = 0; i < nElements; ++i) {
    const Element& element = m_elements[i];
    if (x < element.xmin || x > element.xmax ||
        y < element.ymin || y > element.ymax) continue;
    if (!CheckElement(x, y, element, w)) continue;
    const unsigned int nVertices = element.type + 1;
    for (unsigned int j = 0; j < nVertices; ++j) {
      const Vertex& vj = m_vertices[element.vertex[j]];
      emob += w[j] * vj.emob;
      hmob += w[j] * vj.hmob;
    }
    m_lastElement = i;
    return true;
  }

  // Point is outside the mesh.
  if (m_debug) {
    std::cerr << m_className << "::GetMobility:\n"
              << "    Point (" << x << ", " << y << ") is outside the mesh.\n";
  }
  return false;
}

bool ComponentTcad2d::GetDonorOccupation(const double xin, const double yin,
                                         const double zin, 
                                         const unsigned int donorNumber,
                                         double& f) {
  f = 0.;
  if (donorNumber >= m_donors.size()) {
    std::cerr << m_className << "::GetDonorOccupation:\n"
              << "    Donor " << donorNumber << " does not exist.\n";
    return false;
  }

  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::GetDonorOccupation:\n"
              << "    Field map is not available for interpolation.\n";
    return false;
  }

  double x = xin, y = yin, z = zin;
  // In case of periodicity, reduce to the cell volume.
  bool xmirr = false, ymirr = false;
  MapCoordinates(x, y, xmirr, ymirr);
  // Check if the point is inside the bounding box.
  if (x < m_xMinBB || x > m_xMaxBB || y < m_yMinBB || y > m_yMaxBB) {
    return false;
  }
  if (m_hasRangeZ && (z < m_zMinBB || z > m_zMaxBB)) {
    return false;
  }

  double w[nMaxVertices] = {0};
  if (m_lastElement >= 0) {
    // Check if the point is still located in the previously found element.
    const Element& last = m_elements[m_lastElement];
    if (x >= last.xmin && x <= last.xmax && y >= last.ymin && y <= last.ymax) {
      if (CheckElement(x, y, last, w)) {
        const unsigned int nVertices = last.type + 1;
        for (unsigned int j = 0; j < nVertices; ++j) {
          const Vertex& vj = m_vertices[last.vertex[j]];
          f += w[j] * vj.donorOcc[donorNumber];
        }
        return true;
      }
    }
    // The point is not in the previous element.
    // Check the adjacent elements.
    const unsigned int nNeighbours = last.neighbours.size();
    for (unsigned int i = 0; i < nNeighbours; ++i) {
      const Element& element = m_elements[last.neighbours[i]];
      if (x < element.xmin || x > element.xmax || 
          y < element.ymin || y > element.ymax) continue; 
      if (!CheckElement(x, y, element, w)) continue;
      const unsigned int nVertices = element.type + 1;
      for (unsigned int j = 0; j < nVertices; ++j) {
        const Vertex& vj = m_vertices[element.vertex[j]];
        f += w[j] * vj.donorOcc[donorNumber];
      }
      m_lastElement = last.neighbours[i];
      return true;
    }
  }

  // The point is not in the previous element nor in the adjacent ones.
  // We have to loop over all elements.
  const unsigned int nElements = m_elements.size();
  for (unsigned int i = 0; i < nElements; ++i) {
    const Element& element = m_elements[i];
    if (x < element.xmin || x > element.xmax ||
        y < element.ymin || y > element.ymax) continue;
    if (!CheckElement(x, y, element, w)) continue;
    const unsigned int nVertices = element.type + 1;
    for (unsigned int j = 0; j < nVertices; ++j) {
      const Vertex& vj = m_vertices[element.vertex[j]];
      f += w[j] * vj.donorOcc[donorNumber];
    }
    m_lastElement = i;
    return true;
  }

  // Point is outside the mesh.
  if (m_debug) {
    std::cerr << m_className << "::GetDonorOccupation:\n"
              << "    Point (" << x << ", " << y << ") is outside the mesh.\n";
  }
  return false;
}

bool ComponentTcad2d::GetAcceptorOccupation(const double xin, const double yin,
                                            const double zin, 
                                            const unsigned int acceptorNumber,
                                            double& f) {
  f = 0.;
  if (acceptorNumber >= m_acceptors.size()) { 
    std::cerr << m_className << "::GetAcceptorOccupation:\n"
              << "    Acceptor " << acceptorNumber << " does not exist.\n";
    return false;
  }

  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::GetAcceptorOccupation:\n"
              << "    Field map is not available for interpolation.\n";
    return false;
  }

  double x = xin, y = yin, z = zin;
  // In case of periodicity, reduce to the cell volume.
  bool xmirr = false, ymirr = false;
  MapCoordinates(x, y, xmirr, ymirr);
  // Check if the point is inside the bounding box.
  if (x < m_xMinBB || x > m_xMaxBB || y < m_yMinBB || y > m_yMaxBB) {
    return false;
  }
  if (m_hasRangeZ && (z < m_zMinBB || z > m_zMaxBB)) {
    return false;
  }


  double w[nMaxVertices] = {0};
  if (m_lastElement >= 0) {
    // Check if the point is still located in the previously found element.
    const Element& last = m_elements[m_lastElement];
    if (x >= last.xmin && x <= last.xmax && y >= last.ymin && y <= last.ymax) {
      if (CheckElement(x, y, last, w)) {
        const unsigned int nVertices = last.type + 1;
        for (unsigned int j = 0; j < nVertices; ++j) {
          const Vertex& vj = m_vertices[last.vertex[j]];
          f += w[j] * vj.acceptorOcc[acceptorNumber];
        }
        return true;
      }
    }
    // The point is not in the previous element.
    // Check the adjacent elements.
    const unsigned int nNeighbours = last.neighbours.size();
    for (unsigned int i = 0; i < nNeighbours; ++i) {
      const Element& element = m_elements[last.neighbours[i]];
      if (x < element.xmin || x > element.xmax || 
          y < element.ymin || y > element.ymax) continue; 
      if (!CheckElement(x, y, element, w)) continue;
      const unsigned int nVertices = element.type + 1;
      for (unsigned int j = 0; j < nVertices; ++j) {
        const Vertex& vj = m_vertices[element.vertex[j]];
        f += w[j] * vj.acceptorOcc[acceptorNumber];
      }
      m_lastElement = last.neighbours[i];
      return true;
    }
  }

  // The point is not in the previous element nor in the adjacent ones.
  // We have to loop over all elements.
  const unsigned int nElements = m_elements.size();
  for (unsigned int i = 0; i < nElements; ++i) {
    const Element& element = m_elements[i];
    if (x < element.xmin || x > element.xmax ||
        y < element.ymin || y > element.ymax) continue;
    if (!CheckElement(x, y, element, w)) continue;
    const unsigned int nVertices = element.type + 1;
    for (unsigned int j = 0; j < nVertices; ++j) {
      const Vertex& vj = m_vertices[element.vertex[j]];
      f += w[j] * vj.acceptorOcc[acceptorNumber];
    }
    m_lastElement = i;
    return true;
  }

  // Point is outside the mesh.
  if (m_debug) {
    std::cerr << m_className << "::GetAcceptorOccupation:\n"
              << "    Point (" << x << ", " << y << ") is outside the mesh.\n";
  }
  return false;
}

bool ComponentTcad2d::Initialise(const std::string& gridfilename,
                                 const std::string& datafilename) {

  m_ready = false;

  m_hasPotential = m_hasField = false;
  m_hasElectronMobility = m_hasHoleMobility = false;
  m_validTraps = false;
  m_donors.clear();
  m_acceptors.clear();

  // Import mesh data from .grd file.
  if (!LoadGrid(gridfilename)) {
    std::cerr << m_className << "::Initialise:\n"
              << "    Importing mesh data failed.\n";
    return false;
  }

  // Import electric field, potential, mobilities and 
  // trap occupation values from .dat file.
  if (!LoadData(datafilename)) {
    std::cerr << m_className << "::Initialise:\n"
              << "    Importing electric field and potential failed.\n";
    return false;
  }

  // Find min./max. coordinates and potentials.
  m_xMaxBB = m_xMinBB = m_vertices[m_elements[0].vertex[0]].x;
  m_yMaxBB = m_yMinBB = m_vertices[m_elements[0].vertex[0]].y;
  m_pMax = m_pMin = m_vertices[m_elements[0].vertex[0]].p;
  const unsigned int nElements = m_elements.size();
  for (unsigned int i = 0; i < nElements; ++i) {
    const Vertex& v0 = m_vertices[m_elements[i].vertex[0]];
    const Vertex& v1 = m_vertices[m_elements[i].vertex[1]];
    double xmin = std::min(v0.x, v1.x);
    double xmax = std::max(v0.x, v1.x);
    double ymin = std::min(v0.y, v1.y);
    double ymax = std::max(v0.y, v1.y);
    m_pMin = std::min(m_pMin, std::min(v0.p, v1.p));
    m_pMax = std::max(m_pMax, std::max(v0.p, v1.p));
    if (m_elements[i].type > 1) {
      const Vertex& v2 = m_vertices[m_elements[i].vertex[2]];
      xmin = std::min(xmin, v2.x); 
      xmax = std::max(xmax, v2.x); 
      ymin = std::min(ymin, v2.y); 
      ymax = std::max(ymax, v2.y);
      m_pMin = std::min(m_pMin, v2.p);
      m_pMax = std::max(m_pMax, v2.p);
    }
    if (m_elements[i].type > 2) {
      const Vertex& v3 = m_vertices[m_elements[i].vertex[3]];
      xmin = std::min(xmin, v3.x); 
      xmax = std::max(xmax, v3.x); 
      ymin = std::min(ymin, v3.y); 
      ymax = std::max(ymax, v3.y);
      m_pMin = std::min(m_pMin, v3.p);
      m_pMax = std::max(m_pMax, v3.p);
    } 
    const double tol = 1.e-6;
    m_elements[i].xmin = xmin - tol;
    m_elements[i].xmax = xmax + tol;
    m_elements[i].ymin = ymin - tol;
    m_elements[i].ymax = ymax + tol;
    m_xMinBB = std::min(m_xMinBB, xmin);
    m_xMaxBB = std::max(m_xMaxBB, xmax);
    m_yMinBB = std::min(m_yMinBB, ymin);
    m_yMaxBB = std::max(m_yMaxBB, ymax);
  }

  std::cout << m_className << "::Initialise:\n"
            << "    Available data:\n";
  if (m_hasPotential) {
    std::cout << "      Electrostatic potential\n";
  }
  if (m_hasField) {
    std::cout << "      Electric field\n";
  }
  if (m_hasElectronMobility) {
    std::cout << "      Electron mobility\n";
  }
  if (m_hasHoleMobility) {
    std::cout << "      Hole mobility\n";
  }
  if (m_hasElectronVelocity) {
    std::cout << "      Electron velocity\n";
  }
  if (m_hasHoleVelocity) {
    std::cout << "      Hole velocity\n";
  }
  if (m_hasElectronLifetime) {
    std::cout << "      Electron lifetimes\n";
  }
  if (m_hasHoleLifetime) {
    std::cout << "      Hole lifetimes\n";
  }
  if (!m_donors.empty()) {
    std::cout << "      " << m_donors.size() << " donor-type traps\n";
  }
  if (!m_acceptors.empty()) {
    std::cout << "      " << m_acceptors.size() << " acceptor-type traps\n";
  }
  std::cout << "    Bounding box:\n"
            << "      " << m_xMinBB << " < x [cm] < " << m_xMaxBB << "\n"
            << "      " << m_yMinBB << " < y [cm] < " << m_yMaxBB << "\n"
            << "    Voltage range:\n"
            << "      " << m_pMin << " < V < " << m_pMax << "\n";

  bool ok = true;

  // Count the number of elements belonging to a region.
  const int nRegions = m_regions.size();
  std::vector<int> nElementsRegion(nRegions, 0);

  // Count the different element shapes.
  unsigned int nLines = 0;
  unsigned int nTriangles = 0;
  unsigned int nRectangles = 0;
  unsigned int nOtherShapes = 0;

  // Check if there are elements which are not part of any region.
  unsigned int nLoose = 0;
  std::vector<int> looseElements;

  // Check if there are degenerate elements.
  unsigned int nDegenerate = 0;
  std::vector<int> degenerateElements;

  for (unsigned int i = 0; i < nElements; ++i) {
    const Element& element = m_elements[i];
    if (element.type == 1) {
      ++nLines;
      if (element.vertex[0] == element.vertex[1]) {
        degenerateElements.push_back(i);
        ++nDegenerate;
      }
    } else if (element.type == 2) {
      ++nTriangles;
      if (element.vertex[0] == element.vertex[1] ||
          element.vertex[1] == element.vertex[2] ||
          element.vertex[2] == element.vertex[0]) {
        degenerateElements.push_back(i);
        ++nDegenerate;
      }
    } else if (element.type == 3) {
      ++nRectangles;
      if (element.vertex[0] == element.vertex[1] ||
          element.vertex[0] == element.vertex[2] ||
          element.vertex[0] == element.vertex[3] ||
          element.vertex[1] == element.vertex[2] ||
          element.vertex[1] == element.vertex[3] ||
          element.vertex[2] == element.vertex[3]) {
        degenerateElements.push_back(i);
        ++nDegenerate;
      }
    } else {
      // Other shapes should not occur, since they were excluded in LoadGrid.
      ++nOtherShapes;
    }
    if (element.region >= 0 && element.region < nRegions) {
      ++nElementsRegion[element.region];
    } else {
      looseElements.push_back(i);
      ++nLoose;
    }
  }

  if (nDegenerate > 0) {
    std::cerr << m_className << "::Initialise:\n"
              << "    The following elements are degenerate:\n";
    for (unsigned int i = 0; i < nDegenerate; ++i) {
      std::cerr << "      " << degenerateElements[i] << "\n";
    }
    ok = false;
  }

  if (nLoose > 0) {
    std::cerr << m_className << "::Initialise:\n"
              << "    The following elements are not part of any region:\n";
    for (unsigned int i = 0; i < nLoose; ++i) {
      std::cerr << "      " << looseElements[i] << "\n";
    }
    ok = false;
  }

  std::cout << m_className << "::Initialise:\n"
            << "    Number of regions: " << nRegions << "\n";
  for (int i = 0; i < nRegions; ++i) {
    std::cout << "      " << i << ": " << m_regions[i].name << ", "
              << nElementsRegion[i] << " elements\n";
  }

  std::cout << "    Number of elements: " << nElements << "\n";
  if (nLines > 0) {
    std::cout << "      " << nLines << " lines\n";
  }
  if (nTriangles > 0) {
    std::cout << "      " << nTriangles << " triangles\n";
  }
  if (nRectangles > 0) {
    std::cout << "      " << nRectangles << " rectangles\n";
  }
  if (nOtherShapes > 0) {
    std::cerr << "      " << nOtherShapes << " elements of unknown type.\n"
              << "      Program bug!\n";
    m_ready = false;
    Cleanup();
    return false;
  }

  std::cout << "    Number of vertices: " << m_vertices.size() << "\n";

  // Find adjacent elements.
  std::cout << m_className << "::Initialise:\n"
            << "    Looking for neighbouring elements. Be patient...\n";
  FindNeighbours();

  if (!ok) {
    m_ready = false;
    Cleanup();
    return false;
  }

  m_ready = true;
  UpdatePeriodicity();
  std::cout << m_className << "::Initialise:\n"
            << "    Initialisation finished.\n"; 
  return true;
}

bool ComponentTcad2d::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                                     double& xmax, double& ymax, double& zmax) {

  if (!m_ready) return false;
  if (m_xPeriodic || m_xMirrorPeriodic) {
    xmin = -INFINITY;
    xmax = +INFINITY;
  } else {
    xmin = m_xMinBB;
    xmax = m_xMaxBB;
  }

  if (m_yPeriodic || m_yMirrorPeriodic) {
    ymin = -INFINITY;
    ymax = +INFINITY;
  } else {
    ymin = m_yMinBB;
    ymax = m_yMaxBB;
  }

  if (m_hasRangeZ) {
    zmin = m_zMinBB;
    zmax = m_zMaxBB;
  }
  return true;
}

void ComponentTcad2d::SetRangeZ(const double zmin, const double zmax) {

  if (fabs(zmax - zmin) <= 0.) {
    std::cerr << m_className << "::SetRangeZ:\n"
              << "    Zero range is not permitted.\n";
    return;
  }
  m_zMinBB = std::min(zmin, zmax);
  m_zMaxBB = std::max(zmin, zmax);
  m_hasRangeZ = true;
}

bool ComponentTcad2d::GetVoltageRange(double& vmin, double& vmax) {

  if (!m_ready) return false;
  vmin = m_pMin;
  vmax = m_pMax;
  return true;
}

void ComponentTcad2d::PrintRegions() const {

  // Do not proceed if not properly initialised.
  if (!m_ready) {
    std::cerr << m_className << "::PrintRegions:\n"
              << "    Field map not yet initialised.\n";
    return;
  }

  if (m_regions.empty()) {
    std::cerr << m_className << "::PrintRegions:\n"
              << "    No regions are currently defined.\n";
    return;
  }

  const unsigned int nRegions = m_regions.size();
  std::cout << m_className << "::PrintRegions:\n"
            << "    Currently " << nRegions << " regions are defined.\n"
            << "      Index  Name       Medium\n";
  for (unsigned int i = 0; i < nRegions; ++i) {
    std::cout << "      " << i << "  " << m_regions[i].name;
    if (!m_regions[i].medium) {
      std::cout << "      none  ";
    } else {
      std::cout << "      " << m_regions[i].medium->GetName();
    }
    if (m_regions[i].drift) {
      std::cout << " (active region)\n";
    } else {
      std::cout << "\n";
    }
  }
}

void ComponentTcad2d::GetRegion(const unsigned int i, 
                                std::string& name, bool& active) const {

  if (i >= m_regions.size()) {
    std::cerr << m_className << "::GetRegion:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }
  name = m_regions[i].name;
  active = m_regions[i].drift;
}

void ComponentTcad2d::SetDriftRegion(const unsigned int i) {

  if (i >= m_regions.size()) {
    std::cerr << m_className << "::SetDriftRegion:\n"
              << "    Region " << i << " does not exist.\n";
    return;
  }
  m_regions[i].drift = true;
}

void ComponentTcad2d::UnsetDriftRegion(const unsigned int i) {

  if (i >= m_regions.size()) {
    std::cerr << m_className << "::UnsetDriftRegion:\n"
              << "    Region " << i << " does not exist.\n";
    return;
  }
  m_regions[i].drift = false;
}

void ComponentTcad2d::SetMedium(const unsigned int i, Medium* medium) {

  if (i >= m_regions.size()) {
    std::cerr << m_className << "::SetMedium:\n"
              << "    Region " << i << " does not exist.\n";
    return;
  }

  if (!medium) {
    std::cerr << m_className << "::SetMedium:\n    Null pointer.\n";
    return;
  }

  m_regions[i].medium = medium;
}

Medium* ComponentTcad2d::GetMedium(const unsigned int i) const {

  if (i >= m_regions.size()) {
    std::cerr << m_className << "::GetMedium:\n"
              << "    Region " << i << " does not exist.\n";
    return NULL;
  }

  return m_regions[i].medium;
}

bool ComponentTcad2d::GetElement(const unsigned int i, double& vol, double& dmin,
                                 double& dmax, int& type) const {

  if (i >= m_elements.size()) {
    std::cerr << m_className << "::GetElement:\n"
              << "    Element index (" << i << ") out of range.\n";
    return false;
  }

  const Element& element = m_elements[i];
  if (element.type == 1) {
    const Vertex& v0 = m_vertices[element.vertex[0]];
    const Vertex& v1 = m_vertices[element.vertex[1]];
    const double dx = v1.x - v0.x;
    const double dy = v1.y - v0.y;
    const double d = sqrt(dx * dx + dy * dy);
    dmin = dmax = vol = d;
  } else if (m_elements[i].type == 2) {
    const Vertex& v0 = m_vertices[element.vertex[0]];
    const Vertex& v1 = m_vertices[element.vertex[1]];
    const Vertex& v2 = m_vertices[element.vertex[2]];
    vol = 0.5 * fabs((v2.x - v0.x) * (v1.y - v0.y) - 
                     (v2.y - v0.y) * (v1.x - v0.x));
    const double a = sqrt(pow(v1.x - v0.x, 2) + pow(v1.y - v0.y, 2));
    const double b = sqrt(pow(v2.x - v0.x, 2) + pow(v2.y - v0.y, 2));
    const double c = sqrt(pow(v1.x - v2.x, 2) + pow(v1.y - v2.y, 2));
    dmin = std::min(std::min(a, b), c);
    dmax = std::max(std::max(a, b), c);
  } else if (m_elements[i].type == 3) {
    const Vertex& v0 = m_vertices[element.vertex[0]];
    const Vertex& v1 = m_vertices[element.vertex[1]];
    const Vertex& v3 = m_vertices[element.vertex[3]];
    const double a = sqrt(pow(v1.x - v0.x, 2) + pow(v1.y - v0.y, 2));
    const double b = sqrt(pow(v3.x - v0.x, 2) + pow(v3.y - v0.y, 2));
    vol = a * b;
    dmin = std::min(a, b);
    dmax = sqrt(a * a + b * b);
  } else {
    std::cerr << m_className << "::GetElement:\n"
              << "    Unexpected element type (" << type << ")\n";
    return false;
  }
  return true;
}

bool ComponentTcad2d::GetElement(const unsigned int i, double& vol, 
                                 double& dmin, double& dmax, int& type,
                                 int& node1, int& node2,
                                 int& node3, int& node4, int& reg) const {

  if (!GetElement(i, vol, dmin, dmax, type)) return false;
  const Element& element = m_elements[i];
  node1 = element.vertex[0];
  node2 = element.vertex[1];
  node3 = element.vertex[2];
  node4 = element.vertex[3];
  reg = element.region;
  return true;
}

bool ComponentTcad2d::GetNode(const unsigned int i, 
                              double& x, double& y, double& v,
                              double& ex, double& ey) const {

  if (i >= m_vertices.size()) {
    std::cerr << m_className << "::GetNode:\n"
              << "    Node index (" << i << ") out of range.\n";
    return false;
  }

  x = m_vertices[i].x;
  y = m_vertices[i].y;
  v = m_vertices[i].p;
  ex = m_vertices[i].ex;
  ey = m_vertices[i].ey;
  return true;
}

bool ComponentTcad2d::LoadData(const std::string& datafilename) {

  std::ifstream datafile;
  datafile.open(datafilename.c_str(), std::ios::in);
  if (!datafile) {
    std::cerr << m_className << "::LoadData:\n"
              << "    Could not open file " << datafilename << ".\n";
    return false;
  }

  const unsigned int nVertices = m_vertices.size();
  std::vector<unsigned int> fillCount(nVertices, 0);
  for (unsigned int i = 0; i < nVertices; ++i) {
    m_vertices[i].p = 0.;
    m_vertices[i].ex = 0.;
    m_vertices[i].ey = 0.;
    m_vertices[i].emob = 0.;
    m_vertices[i].hmob = 0.;
    m_vertices[i].donorOcc.clear();
    m_vertices[i].acceptorOcc.clear();
  }
  m_donors.clear();
  m_acceptors.clear();

  while (!datafile.fail()) {
    // Read one line and strip white space from the beginning of the line.
    std::string line;
    std::getline(datafile, line);
    ltrim(line);
    // Find data section.
    if (line.substr(0, 8) != "function") continue;
    // Read type of data set.
    const std::string::size_type pEq = line.find('=');
    if (pEq == std::string::npos) {
      // No "=" found.
      std::cerr << m_className << "::LoadData:\n"
                << "    Error reading file " << datafilename << ".\n"
                << "    Line:\n    " << line << "\n";
      datafile.close();
      Cleanup();
      return false;
    }
    line = line.substr(pEq + 1);
    std::string dataset;
    std::istringstream data;
    data.str(line);
    data >> dataset;
    if (dataset == "ElectrostaticPotential") {
      if (!ReadDataset(datafile, dataset)) return false; 
      m_hasPotential = true;
    } else if (dataset == "ElectricField") {
      if (!ReadDataset(datafile, dataset)) return false;
      m_hasField = true;
    } else if (dataset == "eDriftVelocity") {
      if (!ReadDataset(datafile, dataset)) return false;
      m_hasElectronVelocity= true;
    } else if (dataset == "hDriftVelocity") {
      if (!ReadDataset(datafile, dataset)) return false;
      m_hasHoleVelocity= true;
    } else if (dataset == "eMobility") {
      if (!ReadDataset(datafile, dataset)) return false;
      m_hasElectronMobility = true;
    } else if (dataset == "hMobility") {
      if (!ReadDataset(datafile, dataset)) return false;
      m_hasHoleMobility = true;
    } else if (dataset == "eLifetime") {
      if (!ReadDataset(datafile, dataset)) return false;
      m_hasElectronLifetime = true;
    } else if (dataset == "hLifetime") {
      if (!ReadDataset(datafile, dataset)) return false;
      m_hasHoleLifetime = true;
    } else if (dataset.substr(0,14) == "TrapOccupation" && 
               dataset.substr(17,2) == "Do") {
      if (!ReadDataset(datafile, dataset)) return false;
      Defect donor;
      donor.xsece = -1.;
      donor.xsech = -1.;
      donor.conc = -1.;
      m_donors.push_back(donor);
    } else if (dataset.substr(0,14) == "TrapOccupation" && 
               dataset.substr(17, 2) == "Ac") {
      if (!ReadDataset(datafile, dataset)) return false;
      Defect acceptor;
      acceptor.xsece = -1.;
      acceptor.xsech = -1.;
      acceptor.conc = -1.;
      m_acceptors.push_back(acceptor);
    }
  }
  if (datafile.fail() && !datafile.eof()) {
    std::cerr << m_className << "::LoadData\n"
              << "    Error reading file " << datafilename << "\n";
    datafile.close();
    Cleanup();
    return false;
  }

  datafile.close();

  return true;
}

bool ComponentTcad2d::ReadDataset(std::ifstream& datafile, 
                                  const std::string& dataset) {

  enum DataSet {
    ElectrostaticPotential,
    ElectricField,
    eDriftVelocity,
    hDriftVelocity,
    eMobility,
    hMobility,
    eLifetime,
    hLifetime,
    DonorTrapOccupation,
    AcceptorTrapOccupation,
    Unknown
  };
  DataSet ds = Unknown;
  if (dataset == "ElectrostaticPotential") {
    ds = ElectrostaticPotential;
  } else if (dataset == "ElectricField") {
    ds = ElectricField;
  } else if (dataset == "eDriftVelocity") {
    ds = eDriftVelocity;
  } else if (dataset == "hDriftVelocity") {
    ds = hDriftVelocity;
  } else if (dataset == "eMobility") {
    ds = eMobility;
  } else if (dataset == "hMobility") {
    ds = hMobility;
  } else if (dataset == "eLifetime") {
    ds = eLifetime;
  } else if (dataset == "hLifetime") {
    ds = hLifetime;
  } else if (dataset.substr(0,14) == "TrapOccupation") { 
    if (dataset.substr(17,2) == "Do") {
      ds = DonorTrapOccupation;
    } else if (dataset.substr(17,2) == "Ac") {
      ds = AcceptorTrapOccupation;
    }
  } else {
    std::cerr << m_className << "::ReadDataset:\n"
              << "    Unexpected dataset " << dataset << ".\n";
    return false;
  } 

  bool isVector = false;
  if (ds == ElectricField || 
      ds == eDriftVelocity || ds == hDriftVelocity) {
    isVector = true;
  }

  if (!datafile.is_open()) return false;
  std::string line;
  std::getline(datafile, line);
  std::getline(datafile, line);
  std::getline(datafile, line);
  std::getline(datafile, line);
  // Get the region name (given in brackets).
  std::string::size_type bra = line.find('[');
  std::string::size_type ket = line.find(']');
  if (ket < bra || bra == std::string::npos || ket == std::string::npos) {
    std::cerr << m_className << "::ReadDataset:\n"
              << "    Cannot extract region name.\n"
              << "    Line:\n    " << line << "\n";
    datafile.close();
    Cleanup();
    return false;
  }
  line = line.substr(bra + 1, ket - bra - 1);
  std::string name;
  std::istringstream data;
  data.str(line);
  data >> name;
  data.clear();
  // Check if the region name matches one from the mesh file.
  const int index = FindRegion(name);
  if (index == -1) {
    std::cerr << m_className << "::ReadDataset:\n"
              << "    Unknown region " << name << ".\n";
    datafile.close();
    Cleanup();
    return false;
  }
  // Get the number of values.
  std::getline(datafile, line);
  bra = line.find('(');
  ket = line.find(')');
  if (ket < bra || bra == std::string::npos || ket == std::string::npos) {
    std::cerr << m_className << "::LoadData:\n"
              << "    Cannot extract number of values to be read.\n"
              << "    Line:\n    " << line << "\n";
    datafile.close();
    Cleanup();
    return false;
  }
  line = line.substr(bra + 1, ket - bra - 1);
  int nValues;
  data.str(line);
  data >> nValues;
  data.clear();
  if (isVector) nValues = nValues / 2;
  // Mark the vertices belonging to this region.
  const unsigned int nVertices = m_vertices.size();
  std::vector<bool> isInRegion(nVertices, false);
  const unsigned int nElements = m_elements.size();
  for (unsigned int j = 0; j < nElements; ++j) {
    if (m_elements[j].region != index) continue;
    for (int k = 0; k <= m_elements[j].type; ++k) {
      isInRegion[m_elements[j].vertex[k]] = true;
    }
  }

  unsigned int ivertex = 0;
  for (int j = 0; j < nValues; ++j) {
    // Read the next value.
    double val1, val2;
    if (isVector) {
      datafile >> val1 >> val2;
    } else {
      datafile >> val1;
    }
    // Find the next vertex belonging to the region.
    while (ivertex < nVertices) {
      if (isInRegion[ivertex]) break;
      ++ivertex;
    }
    // Check if there is a mismatch between the number of vertices
    // and the number of values.
    if (ivertex >= nVertices) {
      std::cerr << m_className << "::ReadDataset:\n"
                << "    Dataset " << dataset 
                << " has more values than vertices in region " << name << "\n";
      datafile.close();
      Cleanup();
      return false;
    }
    switch (ds) {
      case ElectrostaticPotential:
        m_vertices[ivertex].p = val1;
        break;
      case ElectricField:
        m_vertices[ivertex].ex = val1;
        m_vertices[ivertex].ey = val2;
        break;
      case eDriftVelocity:
        // Scale from cm/s to cm/ns.
        m_vertices[ivertex].eVx = val1 * 1.e-9; 
        m_vertices[ivertex].eVy = val2 * 1.e-9;
        break;
      case hDriftVelocity:
        // Scale from cm/s to cm/ns.
        m_vertices[ivertex].hVx = val1 * 1.e-9;
        m_vertices[ivertex].hVy = val2 * 1.e-9;
        break;
      case eMobility:
        // Convert from cm2 / (V s) to cm2 / (V ns).
        m_vertices[ivertex].emob = val1 * 1.e-9;
        break;
      case hMobility:
        // Convert from cm2 / (V s) to cm2 / (V ns).
        m_vertices[ivertex].hmob = val1 * 1.e-9;
        break;
      case eLifetime:
        // Convert from s to ns.
        m_vertices[ivertex].eTau = val1 * 1.e9;
        break;
      case hLifetime:
        // Convert from s to ns.
        m_vertices[ivertex].hTau = val1 * 1.e9;
        break;
      case DonorTrapOccupation:
        m_vertices[ivertex].donorOcc.push_back(val1);
        break;
      case AcceptorTrapOccupation:
        m_vertices[ivertex].acceptorOcc.push_back(val1);
        break;
      default:
        std::cerr << m_className << "::ReadDataset:\n"
                  << "    Unexpected dataset (" << ds << "). Program bug!\n";
        datafile.close();
        Cleanup();
        return false;
    }
    ++ivertex;
  }
  return true;

}

bool ComponentTcad2d::LoadGrid(const std::string& gridfilename) {

  // Open the file containing the mesh description.
  std::ifstream gridfile;
  gridfile.open(gridfilename.c_str(), std::ios::in);
  if (!gridfile) {
    std::cerr << m_className << "::LoadGrid:\n"
              << "    Could not open file " << gridfilename << ".\n";
    return false;
  }

  // Delete existing mesh information.
  Cleanup();
  // Count line numbers.
  unsigned int iLine = 0;
  // Get the number of regions.
  unsigned int nRegions = 0;
  while (!gridfile.fail()) {
    // Read one line and strip white space from the beginning of the line.
    std::string line;
    std::getline(gridfile, line);
    ++iLine;
    ltrim(line);
    // Find entry 'nb_regions'.
    if (line.substr(0, 10) != "nb_regions") continue;
    const std::string::size_type pEq = line.find('=');
    if (pEq == std::string::npos) {
      // No "=" sign found.
      std::cerr << m_className << "::LoadGrid:\n"
                << "    Could not read number of regions.\n";
      Cleanup();
      gridfile.close();
      return false;
    }
    line = line.substr(pEq + 1);
    std::istringstream data;
    data.str(line);
    data >> nRegions;
    break;
  }
  if (gridfile.eof()) {
    // Reached end of file.
    std::cerr << m_className << "::LoadGrid:\n"
              << "    Could not find entry 'nb_regions' in file\n"
              << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    // Error reading from the file.
    std::cerr << m_className << "::LoadGrid:\n"
              << "    Error reading file " << gridfilename << " (line " 
              << iLine << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }
  m_regions.resize(nRegions);
  for (unsigned int j = 0; j < nRegions; ++j) {
    m_regions[j].name = "";
    m_regions[j].drift = false;
    m_regions[j].medium = NULL;
  }

  if (m_debug) {
    std::cout << m_className << "::LoadGrid:\n"
              << "    Found " << nRegions << " regions.\n";
  }

  // Get the region names.
  while (!gridfile.fail()) {
    std::string line;
    std::getline(gridfile, line);
    ++iLine;
    ltrim(line);
    // Find entry 'regions'.
    if (line.substr(0, 7) != "regions") continue;
    // Get region names (given in brackets).
    const std::string::size_type bra = line.find('[');
    const std::string::size_type ket = line.find(']');
    if (ket < bra || bra == std::string::npos || ket == std::string::npos) {
      // No closed brackets [].
      std::cerr << m_className << "::LoadGrid:\n"
                << "    Could not read region names.\n";
      Cleanup();
      gridfile.close();
      return false;
    }
    line = line.substr(bra + 1, ket - bra - 1);
    std::istringstream data;
    data.str(line);
    for (unsigned int j = 0; j < nRegions; ++j) {
      data >> m_regions[j].name;
      data.clear();
      // Assume by default that all regions are active.
      m_regions[j].drift = true;
      m_regions[j].medium = NULL;
    }
    break;
  }
  if (gridfile.eof()) {
    // Reached end of file.
    std::cerr << m_className << "::LoadGrid:\n"
              << "    Could not find entry 'regions' in file\n"
              << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    // Error reading from the file.
    std::cerr << m_className << "::LoadGrid:\n"
              << "    Error reading file " << gridfilename << " (line " 
              << iLine << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }

  // Get the vertices.
  unsigned int nVertices = 0;
  while (!gridfile.fail()) {
    std::string line;
    std::getline(gridfile, line);
    ++iLine;
    ltrim(line);
    // Find section 'Vertices'.
    if (line.substr(0, 8) != "Vertices") continue; 
    // Get number of vertices (given in brackets).
    const std::string::size_type bra = line.find('(');
    const std::string::size_type ket = line.find(')');
    if (ket < bra || bra == std::string::npos || ket == std::string::npos) {
      // No closed brackets [].
      std::cerr << m_className << "::LoadGrid:\n"
                << "    Could not read number of vertices.\n";
      Cleanup();
      gridfile.close();
      return false;
    }
    line = line.substr(bra + 1, ket - bra - 1);
    std::istringstream data;
    data.str(line);
    data >> nVertices;
    m_vertices.resize(nVertices);
    // Get the coordinates of this vertex.
    for (unsigned int j = 0; j < nVertices; ++j) {
      gridfile >> m_vertices[j].x >> m_vertices[j].y;
      // Change units from micron to cm.
      m_vertices[j].x *= 1.e-4;
      m_vertices[j].y *= 1.e-4;
      ++iLine;
    }
    break;
  }
  if (gridfile.eof()) {
    std::cerr << m_className << "::LoadGrid:\n"
              << "    Could not find section 'Vertices' in file\n"
              << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    std::cerr << m_className << "::LoadGrid:\n"
              << "    Error reading file " << gridfilename << " (line " << iLine
              << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }

  // Get the "edges" (lines connecting two vertices).
  int nEdges = 0;
  // Temporary arrays for storing edge points.
  std::vector<int> edgeP1;
  std::vector<int> edgeP2;
  while (!gridfile.fail()) {
    std::string line;
    std::getline(gridfile, line);
    ++iLine;
    ltrim(line);
    // Find section 'Edges'.
    if (line.substr(0, 5) != "Edges") continue;
    // Get the number of edges (given in brackets).
    const std::string::size_type bra = line.find('(');
    const std::string::size_type ket = line.find(')');
    if (ket < bra || bra == std::string::npos || ket == std::string::npos) {
      // No closed brackets ()
      std::cerr << m_className << "::LoadGrid:\n"
                << "    Could not read number of edges.\n";
      Cleanup();
      gridfile.close();
      return false;
    }
    line = line.substr(bra + 1, ket - bra - 1);
    std::istringstream data;
    data.str(line);
    data >> nEdges;
    edgeP1.resize(nEdges);
    edgeP2.resize(nEdges);
    // Get the indices of the two endpoints.
    for (int j = 0; j < nEdges; ++j) {
      gridfile >> edgeP1[j] >> edgeP2[j];
      ++iLine;
    }
    break;
  }
  if (gridfile.eof()) {
    std::cerr << m_className << "::LoadGrid:\n"
              << "    Could not find section 'Edges' in file\n"
              << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    std::cerr << m_className << "::LoadGrid:\n"
              << "    Error reading file " << gridfilename << " (line " 
              << iLine << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }

  for (int i = 0; i < nEdges; ++i) {
    // Make sure the indices of the edge endpoints are not out of range.
    if (edgeP1[i] < 0 || edgeP1[i] >= (int)nVertices || 
        edgeP2[i] < 0 || edgeP2[i] >= (int)nVertices) {
      std::cerr << m_className << "::LoadGrid:\n"
                << "    Vertex index of edge " << i << " out of range.\n";
      Cleanup();
      gridfile.close();
      return false;
    }
    // Make sure the edge is non-degenerate.
    if (edgeP1[i] == edgeP2[i]) {
      std::cerr << m_className << "::LoadGrid:\n"
                << "    Edge " << i << " is degenerate.\n";
      Cleanup();
      gridfile.close();
      return false;
    }
  }

  // Get the elements.
  int edge0, edge1, edge2, edge3;
  int type;
  unsigned int nElements = 0;
  while (!gridfile.fail()) {
    std::string line;
    std::getline(gridfile, line);
    ++iLine;
    ltrim(line);
    // Find section 'Elements'.
    if (line.substr(0, 8) != "Elements") continue;
    // Get number of elements (given in brackets).
    const std::string::size_type bra = line.find('(');
    const std::string::size_type ket = line.find(')');
    if (ket < bra || bra == std::string::npos || ket == std::string::npos) {
      // No closed brackets ().
      std::cerr << m_className << "::LoadGrid:\n"
                << "    Could not read number of elements.\n";
      Cleanup();
      gridfile.close();
      return false;
    }
    line = line.substr(bra + 1, ket - bra - 1);
    std::istringstream data;
    data.str(line);
    data >> nElements;
    // Resize array of elements.
    m_elements.resize(nElements);
    // Get type and constituting edges of each element.
    for (unsigned int j = 0; j < nElements; ++j) {
      for (int k = nMaxVertices; k--;) m_elements[j].vertex[k] = -1;
      m_elements[j].neighbours.clear();
      ++iLine;
      gridfile >> type;
      switch (type) {
        case 1:
          // Line
          gridfile >> edge0 >> edge1;
          if (edge0 < 0) edge0 = -edge0 - 1;
          if (edge1 < 0) edge1 = -edge1 - 1;
          // Make sure the indices are not out of range.
          if (edge0 >= nEdges || edge1 >= nEdges) {
            std::cerr << m_className << "::LoadGrid:\n"
                      << "    Error reading file " << gridfilename
                      << " (line " << iLine << ").\n"
                      << "    Edge index out of range.\n";
            Cleanup();
            gridfile.close();
            return false;
          }
          // Get the vertices of this element.
          // Negative edge index means that the sequence of the two points
          // is supposed to be inverted.
          // The actual index is then given by "-index - 1".
          // Orientt the line such that the first point is on the left.
          if (m_vertices[edgeP1[edge0]].x > m_vertices[edgeP2[edge0]].x) {
            m_elements[j].vertex[0] = edgeP2[edge0];
            m_elements[j].vertex[1] = edgeP1[edge0];
          } else {
            m_elements[j].vertex[0] = edgeP1[edge0];
            m_elements[j].vertex[1] = edgeP2[edge0];
          }
          break;
        case 2:
          // Triangle
          gridfile >> edge0 >> edge1 >> edge2;
          // Make sure the indices are not out of range.
          if (edge0 < 0) edge0 = -edge0 - 1;
          if (edge1 < 0) edge1 = -edge1 - 1;
          if (edge2 < 0) edge2 = -edge2 - 1;
          if (edge0 >= nEdges || edge1 >= nEdges || edge2 >= nEdges) {
            std::cerr << m_className << "::LoadGrid:\n"
                      << "    Error reading file " << gridfilename
                      << " (line " << iLine << ").\n"
                      << "    Edge index out of range.\n";
            Cleanup();
            gridfile.close();
            return false;
          }
          m_elements[j].vertex[0] = edgeP1[edge0];
          m_elements[j].vertex[1] = edgeP2[edge0];
          if (edgeP1[edge1] != m_elements[j].vertex[0] &&
              edgeP1[edge1] != m_elements[j].vertex[1]) {
            m_elements[j].vertex[2] = edgeP1[edge1];
          } else {
            m_elements[j].vertex[2] = edgeP2[edge1];
          }
          // Rearrange vertices such that point 0 is on the left.
          while (m_vertices[m_elements[j].vertex[0]].x >
                 m_vertices[m_elements[j].vertex[1]].x ||
                 m_vertices[m_elements[j].vertex[0]].x >
                 m_vertices[m_elements[j].vertex[2]].x) {
            const int tmp = m_elements[j].vertex[0];
            m_elements[j].vertex[0] = m_elements[j].vertex[1];
            m_elements[j].vertex[1] = m_elements[j].vertex[2];
            m_elements[j].vertex[2] = tmp;
          }
          break;
        case 3:
          // Rectangle
          gridfile >> edge0 >> edge1 >> edge2 >> edge3;
          // Make sure the indices are not out of range.
          if (edge0 >= nEdges || -edge0 - 1 >= nEdges || 
              edge1 >= nEdges || -edge1 - 1 >= nEdges || 
              edge2 >= nEdges || -edge2 - 1 >= nEdges || 
              edge3 >= nEdges || -edge3 - 1 >= nEdges) {
            std::cerr << m_className << "::LoadGrid:\n"
                      << "    Error reading file " << gridfilename
                      << " (line " << iLine << ").\n"
                      << "    Edge index out of range.\n";
            Cleanup();
            gridfile.close();
            return false;
          }
          if (edge0 >= 0)
            m_elements[j].vertex[0] = edgeP1[edge0];
          else
            m_elements[j].vertex[0] = edgeP2[-edge0 - 1];
          if (edge1 >= 0)
            m_elements[j].vertex[1] = edgeP1[edge1];
          else
            m_elements[j].vertex[1] = edgeP2[-edge1 - 1];
          if (edge2 >= 0)
            m_elements[j].vertex[2] = edgeP1[edge2];
          else
            m_elements[j].vertex[2] = edgeP2[-edge2 - 1];
          if (edge3 >= 0)
            m_elements[j].vertex[3] = edgeP1[edge3];
          else
            m_elements[j].vertex[3] = edgeP2[-edge3 - 1];

          // Rearrange vertices such that point 0 is on the left.
          while (m_vertices[m_elements[j].vertex[0]].x >
                 m_vertices[m_elements[j].vertex[1]].x ||
                 m_vertices[m_elements[j].vertex[0]].x >
                 m_vertices[m_elements[j].vertex[2]].x ||
                 m_vertices[m_elements[j].vertex[0]].x >
                 m_vertices[m_elements[j].vertex[3]].x) {
            const int tmp = m_elements[j].vertex[0];
            m_elements[j].vertex[0] = m_elements[j].vertex[1];
            m_elements[j].vertex[1] = m_elements[j].vertex[2];
            m_elements[j].vertex[2] = m_elements[j].vertex[3];
            m_elements[j].vertex[3] = tmp;
          }
          break;
        default:
          // Other element types are not permitted for 2d grids.
          std::cerr << m_className << "::LoadGrid:\n"
                    << "    Error reading file " << gridfilename << " (line "
                    << iLine << ").\n";
          std::cerr << "    Invalid element type (" << type
                    << ") for 2d mesh.\n";
          Cleanup();
          gridfile.close();
          return false;
          break;
      }
      m_elements[j].type = type;
      m_elements[j].region = -1;
    }
    break;
  }
  if (gridfile.eof()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Could not find section 'Elements' in file\n";
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << " (line " << iLine
              << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }

  // Assign regions to elements.
  std::string name;
  while (!gridfile.fail()) {
    std::string line;
    std::getline(gridfile, line);
    ltrim(line);
    // Find section 'Region'.
    if (line.substr(0, 6) != "Region") continue;
    // Get region name (given in brackets).
    std::string::size_type bra = line.find('(');
    std::string::size_type ket = line.find(')');
    if (ket < bra || bra == std::string::npos || ket == std::string::npos) {
      std::cerr << m_className << "::LoadGrid:\n"
                << "    Could not read region name.\n";
      Cleanup();
      gridfile.close();
      return false;
    }
    line = line.substr(bra + 1, ket - bra - 1);
    std::istringstream data;
    data.str(line);
    data >> name;
    data.clear();
    const int index = FindRegion(name);
    if (index == -1) {
      // Specified region name is not in the list.
      std::cerr << m_className << "::LoadGrid:\n";
      std::cerr << "    Error reading file " << gridfilename << ".\n";
      std::cerr << "    Unknown region " << name << ".\n";
      continue;
    }
    std::getline(gridfile, line);
    std::getline(gridfile, line);
    bra = line.find('(');
    ket = line.find(')');
    if (ket < bra || bra == std::string::npos || ket == std::string::npos) {
      // No closed brackets ().
      std::cerr << m_className << "::LoadGrid:\n";
      std::cerr << "    Error reading file " << gridfilename << ".\n";
      std::cerr << "    Could not read number of elements in region " << name
                << ".\n";
      Cleanup();
      gridfile.close();
      return false;
    }
    line = line.substr(bra + 1, ket - bra - 1);
    int nElementsRegion;
    int iElement;
    data.str(line);
    data >> nElementsRegion;
    data.clear();
    for (int j = 0; j < nElementsRegion; ++j) {
      gridfile >> iElement;
      m_elements[iElement].region = index;
    }
  }

  gridfile.close();
  if (gridfile.fail() && !gridfile.eof()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << ".\n";
    Cleanup();
    return false;
  }

  return true;
}

void ComponentTcad2d::FindNeighbours() {

  const unsigned int nElements = m_elements.size();
  std::vector<std::vector<bool> > adjacent(nElements, std::vector<bool>(nElements, false));

  const double tol = 5.e-4; 
  for (unsigned int i = 0; i < nElements; ++i) {
    const Element& ei = m_elements[i];
    for (unsigned int j = 0; j < nElements; ++j) {
      if (i == j || adjacent[i][j]) continue;
      const Element& ej = m_elements[j];
      if (ei.xmin > ej.xmax + tol || ei.xmax < ej.xmin - tol) continue;
      if (ei.ymin > ej.ymax + tol || ei.ymax < ej.ymin - tol) continue;
      for (unsigned int m = 0; m < nMaxVertices; ++m) {
        if (ei.vertex[m] < 0) break;
        for (unsigned int n = 0; n < nMaxVertices; ++n) {
          if (ei.vertex[n] < 0) break;
          if (ei.vertex[m] == ej.vertex[n]) {
            adjacent[i][j] = adjacent[j][i] = true;
            break;
          }
        }
        if (adjacent[i][j]) break;
      }
    }
  }

  for (unsigned int i = 0; i < nElements; ++i) {
    m_elements[i].neighbours.clear();
    for (unsigned int j = 0; j < nElements; ++j) {
      if (adjacent[i][j]) {
        m_elements[i].neighbours.push_back(j);
      }
    }
  }
}

void ComponentTcad2d::Cleanup() {

  // Vertices
  m_vertices.clear();

  // Elements
  m_elements.clear();

  // Regions
  m_regions.clear();
}

bool ComponentTcad2d::CheckElement(const double x, const double y,
                                   const Element& element, 
                                   double w[nMaxVertices]) const {
  
  switch (element.type) {
    case 1:
      return CheckLine(x, y, element, w);
      break;
    case 2:
      return CheckTriangle(x, y, element, w);
      break;
    case 3:
      return CheckRectangle(x, y, element, w);
      break;
    default:
      std::cerr << m_className << "::CheckElement:\n"
                << "    Unknown element type. Program bug!\n";
      break;
  }
  return false;
}
  
bool ComponentTcad2d::CheckRectangle(const double x, const double y,
                                     const Element& element,
                                     double w[nMaxVertices]) const {

  const Vertex& v0 = m_vertices[element.vertex[0]];
  const Vertex& v1 = m_vertices[element.vertex[1]];
  const Vertex& v3 = m_vertices[element.vertex[3]];
  if (y < v0.y || x > v3.x || y > v1.y) return false;

  // Map (x, y) to local variables (u, v) in [-1, 1].
  const double u = (x - 0.5 * (v0.x + v3.x)) / (v3.x - v0.x);
  const double v = (y - 0.5 * (v0.y + v1.y)) / (v1.y - v0.y);
  // Compute weighting factors for each corner.
  w[0] = (0.5 - u) * (0.5 - v);
  w[1] = (0.5 - u) * (0.5 + v);
  w[2] = (0.5 + u) * (0.5 + v);
  w[3] = (0.5 + u) * (0.5 - v);
  return true;
}

bool ComponentTcad2d::CheckTriangle(const double x, const double y,
                                    const Element& element,
                                    double w[nMaxVertices]) const {

  const Vertex& v0 = m_vertices[element.vertex[0]];
  const Vertex& v1 = m_vertices[element.vertex[1]];
  const Vertex& v2 = m_vertices[element.vertex[2]];
  if (x > v1.x && x > v2.x) return false;
  if (y < v0.y && y < v1.y && y < v2.y) return false;
  if (y > v0.y && y > v1.y && y > v2.y) return false;

  // Map (x, y) onto local variables (b, c) such that
  // P = A + b * (B - A) + c * (C - A)
  // A point P is inside the triangle ABC if b, c > 0 and b + c < 1;
  // b, c are also weighting factors for points B, C
  const double v1x = v1.x - v0.x;
  const double v1y = v1.y - v0.y;
  const double v2x = v2.x - v0.x;
  const double v2y = v2.y - v0.y;

  w[1] = ((x - v0.x) * v2y - (y - v0.y) * v2x) / (v1x * v2y - v1y * v2x);
  if (w[1] < 0. || w[1] > 1.) return false;
  w[2] = ((v0.x - x) * v1y - (v0.y - y) * v1x) / (v1x * v2y - v1y * v2x);
  if (w[2] < 0. || w[1] + w[2] > 1.) return false;

  // Weighting factor for point A
  w[0] = 1. - w[1] - w[2];

  return true;
}

bool ComponentTcad2d::CheckLine(const double x, const double y, 
                                const Element& element,
                                double w[nMaxVertices]) const {

  const Vertex& v0 = m_vertices[element.vertex[0]];
  const Vertex& v1 = m_vertices[element.vertex[1]];
  if (x > v1.x) return false;
  if (y < v0.y && y < v1.y) return false;
  if (y > v0.y && y > v1.y) return false;
  const double tx = (x - v0.x) / (v1.x - v0.x);
  if (tx < 0. || tx > 1.) return false;
  const double ty = (y - v0.y) / (v1.y - v0.y);
  if (ty < 0. || ty > 1.) return false;
  if (tx == ty) {
    // Compute weighting factors for endpoints A, B
    w[0] = tx;
    w[1] = 1. - w[0];
    return true;
  }
  return false;
}

void ComponentTcad2d::Reset() {

  Cleanup();
  m_hasRangeZ = false;
  m_ready = false;
}

void ComponentTcad2d::UpdatePeriodicity() {

  if (!m_ready) {
    std::cerr << m_className << "::UpdatePeriodicity:\n"
              << "    Field map not available.\n";
    return;
  }

  // Check for conflicts.
  if (m_xPeriodic && m_xMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n"
              << "    Both simple and mirror periodicity\n"
              << "    along x requested; reset.\n";
    m_xPeriodic = m_xMirrorPeriodic = false;
  }

  if (m_yPeriodic && m_yMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n"
              << "    Both simple and mirror periodicity\n"
              << "    along y requested; reset.\n";
    m_yPeriodic = m_yMirrorPeriodic = false;
  }

  if (m_zPeriodic || m_zMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n"
              << "    Periodicity along z requested; reset.\n";
    m_zPeriodic = m_zMirrorPeriodic = false;
  }

  if (m_xAxiallyPeriodic || m_yAxiallyPeriodic || m_zAxiallyPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n"
              << "    Axial symmetry is not supported; reset.\n";
    m_xAxiallyPeriodic = m_yAxiallyPeriodic = m_zAxiallyPeriodic = false;
  }

  if (m_xRotationSymmetry || m_yRotationSymmetry || m_zRotationSymmetry) {
    std::cerr << m_className << "::UpdatePeriodicity:\n"
              << "    Rotation symmetry is not supported; reset.\n";
    m_xRotationSymmetry = m_yRotationSymmetry = m_zRotationSymmetry = false;
  }
}

int ComponentTcad2d::FindRegion(const std::string& name) const {

  const unsigned int nRegions = m_regions.size();
  for (unsigned int j = 0; j < nRegions; ++j) {
    if (name == m_regions[j].name) return j;
  }
  return -1;
}

void ComponentTcad2d::MapCoordinates(double& x, double& y, 
                                     bool& xmirr, bool& ymirr) const {

  // In case of periodicity, reduce to the cell volume.
  xmirr = false;
  const double cellsx = m_xMaxBB - m_xMinBB;
  if (m_xPeriodic) {
    x = m_xMinBB + fmod(x - m_xMinBB, cellsx);
    if (x < m_xMinBB) x += cellsx;
  } else if (m_xMirrorPeriodic) {
    double xNew = m_xMinBB + fmod(x - m_xMinBB, cellsx);
    if (xNew < m_xMinBB) xNew += cellsx;
    const int nx = int(floor(0.5 + (xNew - x) / cellsx));
    if (nx != 2 * (nx / 2)) {
      xNew = m_xMinBB + m_xMaxBB - xNew;
      xmirr = true;
    }
    x = xNew;
  }
  ymirr = false;
  const double cellsy = m_yMaxBB - m_yMinBB;
  if (m_yPeriodic) {
    y = m_yMinBB + fmod(y - m_yMinBB, cellsy);
    if (y < m_yMinBB) y += cellsy;
  } else if (m_yMirrorPeriodic) {
    double yNew = m_yMinBB + fmod(y - m_yMinBB, cellsy);
    if (yNew < m_yMinBB) yNew += cellsy;
    const int ny = int(floor(0.5 + (yNew - y) / cellsy));
    if (ny != 2 * (ny / 2)) {
      yNew = m_yMinBB + m_yMaxBB - yNew;
      ymirr = true;
    }
    y = yNew;
  }
}

bool ComponentTcad2d::CheckTraps() const {

  const unsigned int nDonors = m_donors.size();
  for (unsigned int i = 0; i < nDonors; ++i) {
    if (m_donors[i].xsece < 0. || m_donors[i].xsech < 0.) return false;
    if (m_donors[i].conc < 0.) return false;
  }

  const unsigned int nAcceptors = m_acceptors.size();
  for (unsigned int i = 0; i < nAcceptors; ++i) {
    if (m_acceptors[i].xsece < 0. || m_acceptors[i].xsech < 0.) return false;
    if (m_acceptors[i].conc < 0.) return false;
  }
  return true;
}

}
