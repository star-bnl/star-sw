#include <iostream>
#include <algorithm>
#include "GeometrySimple.hh"

namespace Garfield {

GeometrySimple::GeometrySimple() : GeometryBase() {

  m_className = "GeometrySimple";
}

void GeometrySimple::AddSolid(Solid* s, Medium* m) {

  // Make sure the solid and the medium are defined.
  if (!s || !m) {
    std::cerr << m_className << "::AddSolid: Null pointer.\n";
    return;
  }

  // Check if this medium is already in the list
  const unsigned int nMedia = m_media.size();
  unsigned int n = nMedia;
  const int id = m->GetId();
  for (unsigned int i = 0; i < nMedia; ++i) {
    if (id == m_media[i]->GetId()) {
      n = i;
      break;
    }
  }
  // If the medium does not exist yet, add it to the list
  if (n == nMedia) m_media.push_back(m);

  // Update the bounding box ranges
  double xmin, ymin, zmin;
  double xmax, ymax, zmax;
  if (!s->GetBoundingBox(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << m_className << "::AddSolid: Solid has no bounding box.\n";
    return;
  }

  if (m_hasBoundingBox) {
    m_xMinBoundingBox = std::min(m_xMinBoundingBox, xmin);
    m_yMinBoundingBox = std::min(m_yMinBoundingBox, ymin);
    m_zMinBoundingBox = std::min(m_zMinBoundingBox, zmin);
    m_xMaxBoundingBox = std::max(m_xMaxBoundingBox, xmax);
    m_yMaxBoundingBox = std::max(m_yMaxBoundingBox, ymax);
    m_zMaxBoundingBox = std::max(m_zMaxBoundingBox, zmax);
  } else {
    m_xMinBoundingBox = xmin;
    m_yMinBoundingBox = ymin;
    m_zMinBoundingBox = zmin;
    m_xMaxBoundingBox = xmax;
    m_yMaxBoundingBox = ymax;
    m_zMaxBoundingBox = zmax;
    m_hasBoundingBox = true;
  }

  // Add the new solid to the list.
  m_solids.emplace_back(std::make_pair(s, n));
}

Solid* GeometrySimple::GetSolid(const double x, const double y, 
                                const double z) const {

  for (const auto& solid : m_solids) {
    if (solid.first->IsInside(x, y, z)) return solid.first;
  }
  return nullptr;
}

Medium* GeometrySimple::GetMedium(const double x, const double y, 
                                  const double z) const {

  for (const auto& solid : m_solids) {
    if (solid.first->IsInside(x, y, z)) {
      if (solid.second < 0) return nullptr;
      return m_media[solid.second];
    }
  }
  return nullptr;
}

Solid* GeometrySimple::GetSolid(const unsigned int i) const {

  if (i >= m_solids.size()) {
    std::cerr << m_className << "::GetSolid:\n"
              << "    Requested solid " << i << " does not exist.\n";
    return nullptr;
  }

  return m_solids[i].first;
}

Medium* GeometrySimple::GetMedium(const unsigned int i) const {

  if (i >= m_media.size()) {
    std::cerr << m_className << "::GetMedium:\n"
              << "    Requested medium " << i << " does not exist.\n";
    return nullptr;
  }
  return m_media[i];
}

void GeometrySimple::Clear() {

  m_media.clear();
  m_solids.clear();
}

void GeometrySimple::PrintSolids() {

  std::cout << m_className << "::PrintSolids:\n";
  const unsigned int nSolids = m_solids.size();
  if (nSolids == 1) {
    std::cout << "    1 solid\n";
  } else {
    std::cout << "    " << nSolids << " solids\n";
  }
  if (m_solids.empty()) return;
  std::cout << "      Index      Type    Medium\n";
  for (unsigned int i = 0; i < nSolids; ++i) {
    std::cout << "        " << i << "         ";
    if (m_solids[i].first->IsBox()) {
      std::cout << "box      ";
    } else if (m_solids[i].first->IsTube()) {
      std::cout << "tube     ";
    } else if (m_solids[i].first->IsSphere()) {
      std::cout << "sphere   ";
    } else {
      std::cout << "unknown  ";
    }
    std::cout << m_media[m_solids[i].second]->GetName() << "\n";
  }
}

bool GeometrySimple::IsInside(const double x, const double y, 
                              const double z) const {

  if (!IsInBoundingBox(x, y, z)) return false;

  for (const auto& solid : m_solids) {
    if (solid.first->IsInside(x, y, z)) return true;
  }
  return false;
}

bool GeometrySimple::IsInBoundingBox(const double x, const double y,
                                     const double z) const {

  if (!m_hasBoundingBox) {
    if (m_debug) {
      std::cerr << m_className << "::IsInBoundingBox:\n"
                << "    Bounding box is not defined.\n";
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
