#include <iostream>
#include "GeometrySimple.hh"

namespace Garfield {

GeometrySimple::GeometrySimple()
    : m_nMedia(0), m_nSolids(0), 
      m_hasBoundingBox(false), m_debug(false) {

  m_className = "GeometrySimple";

  m_media.clear();
  m_solids.clear();
}

void GeometrySimple::AddSolid(Solid* s, Medium* m) {

  // Make sure the solid and the medium are defined.
  if (!s) {
    std::cerr << m_className << "::AddSolid:\n";
    std::cerr << "    Solid pointer is null.\n";
    return;
  }

  if (!m) {
    std::cerr << m_className << "::AddSolid:\n";
    std::cerr << "    Medium pointer is null.\n";
    return;
  }

  int n = -1;
  int id = m->GetId();
  // Check if this medium is already in the list
  for (unsigned int i = 0; i < m_nMedia; ++i) {
    if (id == m_media[i].medium->GetId()) {
      n = i;
      break;
    }
  }
  // If the medium does not exist yet, add it to the list
  if (n < 0) {
    medium newMedium;
    newMedium.medium = m;
    m_media.push_back(newMedium);
    n = m_nMedia;
    ++m_nMedia;
  }

  // Update the bounding box ranges
  double xmin, ymin, zmin;
  double xmax, ymax, zmax;
  if (!s->GetBoundingBox(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << m_className << "::AddSolid:\n";
    std::cerr << "    Solid has no bounding box.\n";
    return;
  }

  if (m_hasBoundingBox) {
    if (xmin < m_xMinBoundingBox) m_xMinBoundingBox = xmin;
    if (ymin < m_yMinBoundingBox) m_yMinBoundingBox = ymin;
    if (zmin < m_zMinBoundingBox) m_zMinBoundingBox = zmin;
    if (xmax > m_xMaxBoundingBox) m_xMaxBoundingBox = xmax;
    if (ymax > m_yMaxBoundingBox) m_yMaxBoundingBox = ymax;
    if (zmax > m_zMaxBoundingBox) m_zMaxBoundingBox = zmax;
  } else {
    m_xMinBoundingBox = xmin;
    m_yMinBoundingBox = ymin;
    m_zMinBoundingBox = zmin;
    m_xMaxBoundingBox = xmax;
    m_yMaxBoundingBox = ymax;
    m_zMaxBoundingBox = zmax;
    m_hasBoundingBox = true;
  }

  // Add the new solid to the list
  solid newSolid;
  newSolid.solid = s;
  newSolid.medium = n;
  m_solids.push_back(newSolid);
  ++m_nSolids;
}

Solid* GeometrySimple::GetSolid(const double x, const double y, 
                                const double z) const {

  for (unsigned int i = 0; i < m_nSolids; ++i) {
    if (m_solids[i].solid->IsInside(x, y, z)) {
      return m_solids[i].solid;
    }
  }
  return NULL;
}

Medium* GeometrySimple::GetMedium(const double x, const double y, const double z) const {

  for (unsigned int i = 0; i < m_nSolids; ++i) {
    if (m_solids[i].solid->IsInside(x, y, z)) {
      if (m_solids[i].medium < 0) return NULL;
      return m_media[m_solids[i].medium].medium;
    }
  }
  return NULL;
}

Solid* GeometrySimple::GetSolid(const unsigned int i) const {

  if (i >= m_nSolids) {
    std::cerr << m_className << "::GetSolid:\n";
    std::cerr << "    Requested solid " << i << " does not exist.\n";
    return NULL;
  }

  return m_solids[i].solid;
}

Medium* GeometrySimple::GetMedium(const unsigned int i) const {

  if (i >= m_nMedia) {
    std::cerr << m_className << "::GetMedium:\n";
    std::cerr << "    Requested medium " << i << " does not exist.\n";
    return NULL;
  }

  return m_media[i].medium;
}

void GeometrySimple::Clear() {

  m_media.clear();
  m_solids.clear();
  m_nMedia = 0;
  m_nSolids = 0;
}

void GeometrySimple::PrintSolids() {

  std::cout << m_className << "::PrintSolids:\n";
  if (m_nSolids == 1) {
    std::cout << "    1 solid\n";
  } else {
    std::cout << "    " << m_nSolids << " solids\n";
  }
  if (m_nSolids == 0) return;
  std::cout << "      Index      Type    Medium\n";
  for (unsigned int i = 0; i < m_nSolids; ++i) {
    std::cout << "        " << i << "         ";
    if (m_solids[i].solid->IsBox()) {
      std::cout << "box      ";
    } else if (m_solids[i].solid->IsTube()) {
      std::cout << "tube     ";
    } else {
      std::cout << "unknown  ";
    }
    std::cout << m_media[m_solids[i].medium].medium->GetName() << "\n";
  }
}

bool GeometrySimple::IsInside(const double x, const double y, 
                              const double z) const {

  if (!IsInBoundingBox(x, y, z)) return false;

  for (unsigned int i = 0; i < m_nSolids; ++i) {
    if (m_solids[i].solid->IsInside(x, y, z)) return true;
  }
  return false;
}

bool GeometrySimple::IsInBoundingBox(const double x, const double y,
                                     const double z) const {

  if (!m_hasBoundingBox) {
    if (m_debug) {
      std::cerr << m_className << "::IsInBoundingBox:\n";
      std::cerr << "    Bounding box is not defined.\n";
    }
    return true;
  }

  if (x >= m_xMinBoundingBox && x <= m_xMaxBoundingBox && 
      y >= m_yMinBoundingBox && y <= m_yMaxBoundingBox && 
      z >= m_zMinBoundingBox && z <= m_zMaxBoundingBox)
    return true;
  return false;
}
}
