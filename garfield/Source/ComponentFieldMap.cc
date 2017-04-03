#include <stdio.h>
#include <iostream>
#include <fstream>

#include <math.h>
#include <string>

#include "ComponentFieldMap.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

ComponentFieldMap::ComponentFieldMap() : ComponentBase(),
      m_is3d(true),
      nElements(-1),
      nNodes(-1),
      m_nMaterials(0),
      nWeightingFields(0),
      hasBoundingBox(false),
      m_deleteBackground(true),
      m_warning(false), m_nWarnings(0),
      m_checkMultipleElement(false),
      m_useTetrahedralTree(false),
      m_isTreeInitialized(false),
      m_tetTree(NULL),
      m_cacheElemBoundingBoxes(false),
      m_lastElement(-1) {

  m_className = "ComponentFieldMap";
}

ComponentFieldMap::~ComponentFieldMap() {
  if (m_tetTree) delete m_tetTree;
}

void ComponentFieldMap::PrintMaterials() {

  // Do not proceed if not properly initialised.
  if (!m_ready) PrintNotReady("PrintMaterials");

  if (materials.empty()) {
    std::cerr << m_className << "::PrintMaterials:\n"
              << "    No materials are currently defined.\n";
    return;
  }

  std::cout << m_className << "::PrintMaterials:\n"
            << "    Currently " << m_nMaterials << " materials are defined.\n"
            << "      Index Permittivity  Resistivity Notes\n";
  for (unsigned int i = 0; i < m_nMaterials; ++i) {
    printf("      %5d %12g %12g", i, materials[i].eps, materials[i].ohm);
    if (materials[i].medium) {
      std::string name = materials[i].medium->GetName();
      std::cout << " " << name;
      if (materials[i].medium->IsDriftable()) std::cout << ", drift medium";
      if (materials[i].medium->IsIonisable()) std::cout << ", ionisable";
    }
    if (materials[i].driftmedium) {
      std::cout << " (drift medium)\n";
    } else {
      std::cout << "\n";
    }
  }
}

void ComponentFieldMap::DriftMedium(const unsigned int imat) {

  // Do not proceed if not properly initialised.
  if (!m_ready) PrintNotReady("DriftMedium");

  // Check value
  if (imat >= m_nMaterials) {
    std::cerr << m_className << "::DriftMedium:\n";
    std::cerr << "    Material index " << imat << " is out of range.\n";
    return;
  }

  // Make drift medium
  materials[imat].driftmedium = true;
}

void ComponentFieldMap::NotDriftMedium(const unsigned int imat) {

  // Do not proceed if not properly initialised.
  if (!m_ready) PrintNotReady("NotDriftMedium");

  // Check value
  if (imat >= m_nMaterials) {
    std::cerr << m_className << "::NotDriftMedium:\n";
    std::cerr << "    Material index " << imat << " is out of range.\n";
    return;
  }

  // Make drift medium
  materials[imat].driftmedium = false;
}

double ComponentFieldMap::GetPermittivity(const unsigned int imat) {

  if (imat >= m_nMaterials) {
    std::cerr << m_className << "::GetPermittivity:\n";
    std::cerr << "    Material index " << imat << " is out of range.\n";
    return -1.;
  }

  return materials[imat].eps;
}

double ComponentFieldMap::GetConductivity(const unsigned int imat) {

  if (imat >= m_nMaterials) {
    std::cerr << m_className << "::GetConductivity:\n";
    std::cerr << "    Material index " << imat << " is out of range.\n";
    return -1.;
  }

  return materials[imat].ohm;
}

void ComponentFieldMap::SetMedium(const unsigned int imat, Medium* m) {

  if (imat >= m_nMaterials) {
    std::cerr << m_className << "::SetMedium:\n";
    std::cerr << "    Material index " << imat << " is out of range.\n";
    return;
  }

  if (!m) {
    std::cerr << m_className << "::SetMedium:\n    Null pointer.\n";
    return;
  }

  if (m_debug) {
    std::cout << m_className << "::SetMedium:\n    Associated material "
              << imat << " with medium " << m->GetName() << ".\n";
  }

  materials[imat].medium = m;
}

Medium* ComponentFieldMap::GetMedium(const unsigned int imat) const {

  if (imat >= m_nMaterials) {
    std::cerr << m_className << "::GetMedium:\n";
    std::cerr << "    Material index " << imat << " is out of range.\n";
    return NULL;
  }

  return materials[imat].medium;
}

bool ComponentFieldMap::GetElement(const unsigned int i, double& vol,
                                   double& dmin, double& dmax) {

  if ((int)i >= nElements) {
    std::cerr << m_className << "::GetElement:\n";
    std::cerr << "    Element index (" << i << ") out of range.\n";
    return false;
  }

  vol = GetElementVolume(i);
  GetAspectRatio(i, dmin, dmax);
  return true;
}

int ComponentFieldMap::FindElement5(const double x, const double y,
                                    double const z, double& t1, double& t2,
                                    double& t3, double& t4, double jac[4][4],
                                    double& det) {

  // Check if bounding boxes of elements have been computed
  if (!m_cacheElemBoundingBoxes) {
    std::cout << m_className << "::FindElement5:\n"
              << "    Caching the bounding boxes of all elements.\n";

    CalculateElementBoundingBoxes();
    m_cacheElemBoundingBoxes = true;
  }

  // Tetra list in the block that contains the input 3D point.
  std::vector<int> tetList;
  if (m_useTetrahedralTree) {
    if (!m_isTreeInitialized) {
      if (!InitializeTetrahedralTree()) {
        std::cerr << m_className << "::FindElement5:\n";
        std::cerr << "    Tetrahedral tree initialization failed.\n";
        return -1;
      }
    }
    tetList = m_tetTree->GetTetListInBlock(Vec3(x, y, z));
  }
  // Backup
  double jacbak[4][4], detbak = 1.;
  double t1bak = 0., t2bak = 0., t3bak = 0., t4bak = 0.;
  int imapbak = -1;

  // Initial values.
  t1 = t2 = t3 = t4 = 0;

  // Check previously used element
  if (m_lastElement > -1 && !m_checkMultipleElement) {
    if (elements[m_lastElement].degenerate) {
      if (Coordinates3(x, y, z, t1, t2, t3, t4, jac, det, m_lastElement) == 0) {
        if (t1 >= 0 && t1 <= +1 && t2 >= 0 && t2 <= +1 && t3 >= 0 && t3 <= +1) {
          return m_lastElement;
        }
      }
    } else {
      if (Coordinates5(x, y, z, t1, t2, t3, t4, jac, det, m_lastElement) == 0) {
        if (t1 >= -1 && t1 <= +1 && t2 >= -1 && t2 <= +1) return m_lastElement;
      }
    }
  }

  // Verify the count of volumes that contain the point.
  int nfound = 0;
  int imap = -1;

  // Number of elements to scan.
  // With tetra tree disabled, all elements are scanned.
  const int numElemToSearch =
      m_useTetrahedralTree ? tetList.size() : nElements;
  for (int i = 0; i < numElemToSearch; ++i) {
    const int idxToElemList = m_useTetrahedralTree ? tetList[i] : i;
    const Element& element = elements[idxToElemList];
    // Tolerance
    const double f = 0.2;
    const double tolx = f * (element.xmax - element.xmin);
    if (x < element.xmin - tolx || x > element.xmax + tolx) continue;
    const double toly = f * (element.ymax - element.ymin);
    if (y < element.ymin - toly || y > element.ymax + toly) continue;
    const double tolz = f * (element.zmax - element.zmin);
    if (z < element.zmin - tolz || z > element.zmax + tolz) continue;

    if (element.degenerate) {
      // Degenerate element
      if (Coordinates3(x, y, z, t1, t2, t3, t4, jac, det, idxToElemList) != 0) {
        continue;
      }
      if (t1 < 0 || t1 > 1 || t2 < 0 || t2 > 1 || t3 < 0 || t3 > 1) continue;
      ++nfound;
      imap = idxToElemList;
      m_lastElement = idxToElemList;
      if (m_debug) {
        std::cout << m_className << "::FindElement5:\n";
        std::cout << "    Found matching degenerate element " << idxToElemList
                  << ".\n";
      }
      if (!m_checkMultipleElement) return idxToElemList;
      for (int j = 0; j < 4; ++j) {
        for (int k = 0; k < 4; ++k) jacbak[j][k] = jac[j][k];
      }
      detbak = det;
      t1bak = t1;
      t2bak = t2;
      t3bak = t3;
      t4bak = t4;
      imapbak = imap;
      if (m_debug) {
        PrintElement("FindElement5", x, y, z, t1, t2, t3, t4, imap, 6);
      }
    } else {
      // Non-degenerate element
      if (Coordinates5(x, y, z, t1, t2, t3, t4, jac, det, idxToElemList) != 0) {
        continue;
      }
      if (t1 < -1 || t1 > 1 || t2 < -1 || t2 > 1) continue;
      ++nfound;
      imap = idxToElemList;
      m_lastElement = idxToElemList;
      if (m_debug) {
        std::cout << m_className << "::FindElement5:\n";
        std::cout << "    Found matching non-degenerate element "
                  << idxToElemList << ".\n";
      }
      if (!m_checkMultipleElement) return idxToElemList;
      for (int j = 0; j < 4; ++j) {
        for (int k = 0; k < 4; ++k) jacbak[j][k] = jac[j][k];
      }
      detbak = det;
      t1bak = t1;
      t2bak = t2;
      t3bak = t3;
      t4bak = t4;
      imapbak = imap;
      if (m_debug) {
        PrintElement("FindElement5", x, y, z, t1, t2, t3, t4, imap, 8);
      }
    }
  }

  // In checking mode, verify the tetrahedron/triangle count.
  if (m_checkMultipleElement) {
    if (nfound < 1) {
      if (m_debug) {
        std::cout << m_className << "::FindElement5:\n";
        std::cout << "    No element matching point (" << x << ", " << y
                  << ") found.\n";
      }
      m_lastElement = -1;
      return -1;
    }
    if (nfound > 1) {
      std::cout << m_className << "::FindElement5:\n";
      std::cout << "    Found " << nfound << " elements matching point (" << x
                << ", " << y << ").\n";
    }
    if (nfound > 0) {
      for (int j = 0; j < 4; ++j) {
        for (int k = 0; k < 4; ++k) jac[j][k] = jacbak[j][k];
      }
      det = detbak;
      t1 = t1bak;
      t2 = t2bak;
      t3 = t3bak;
      t4 = t4bak;
      imap = imapbak;
      m_lastElement = imap;
      return imap;
    }
  }

  if (m_debug) {
    std::cout << m_className << "::FindElement5:\n";
    std::cout << "    No element matching point (" << x << ", " << y
              << ") found.\n";
  }
  return -1;
}

int ComponentFieldMap::FindElement13(const double x, const double y,
                                     const double z, double& t1, double& t2,
                                     double& t3, double& t4, double jac[4][4],
                                     double& det) {
  // Check if bounding boxes of elements have been computed
  if (!m_cacheElemBoundingBoxes) {
    std::cout << m_className << "::FindElement13:\n"
              << "    Caching the bounding boxes of all elements.\n";

    CalculateElementBoundingBoxes();
    m_cacheElemBoundingBoxes = true;
  }

  // Backup
  double jacbak[4][4];
  double detbak = 1.;
  double t1bak = 0., t2bak = 0., t3bak = 0., t4bak = 0.;
  int imapbak = -1;

  // Initial values.
  t1 = t2 = t3 = t4 = 0.;

  // Check previously used element
  if (m_lastElement > -1 && !m_checkMultipleElement) {
    if (Coordinates13(x, y, z, t1, t2, t3, t4, jac, det, m_lastElement) == 0) {
      if (t1 >= 0 && t1 <= +1 && t2 >= 0 && t2 <= +1 && t3 >= 0 && t3 <= +1 &&
          t4 >= 0 && t4 <= +1) {
        return m_lastElement;
      }
    }
  }

  // Tetra list in the block that contains the input 3D point.
  std::vector<int> tetList;
  if (m_useTetrahedralTree) {
    if (!m_isTreeInitialized) {
      if (!InitializeTetrahedralTree()) {
        std::cerr << m_className << "::FindElement13:\n";
        std::cerr << "    Tetrahedral tree initialization failed.\n";
        return -1;
      }
    }
    tetList = m_tetTree->GetTetListInBlock(Vec3(x, y, z));
  }
  // Number of elements to scan.
  // With tetra tree disabled, all elements are scanned.
  const int numElemToSearch =
      m_useTetrahedralTree ? tetList.size() : nElements;
  // Verify the count of volumes that contain the point.
  int nfound = 0;
  int imap = -1;

  // Scan all elements
  for (int i = 0; i < numElemToSearch; i++) {
    const int idxToElemList = m_useTetrahedralTree ? tetList[i] : i;
    const Element& element = elements[idxToElemList];
    // Tolerance
    const double f = 0.2;
    const double tolx = f * (element.xmax - element.xmin);
    if (x < element.xmin - tolx || x > element.xmax + tolx) continue;
    const double toly = f * (element.ymax - element.ymin);
    if (y < element.ymin - toly || y > element.ymax + toly) continue;
    const double tolz = f * (element.zmax - element.zmin);
    if (z < element.zmin - tolz || z > element.zmax + tolz) continue;
    if (Coordinates13(x, y, z, t1, t2, t3, t4, jac, det, idxToElemList) != 0) {
      continue;
    }
    if (t1 < 0 || t1 > 1 || t2 < 0 || t2 > 1 || t3 < 0 || t3 > 1 || t4 < 0 ||
        t4 > 1) {
      continue;
    }
    ++nfound;
    imap = idxToElemList;
    m_lastElement = idxToElemList;
    if (m_debug) {
      std::cout << m_className << "::FindElement13:\n";
      std::cout << "    Found matching element " << i << ".\n";
    }
    if (!m_checkMultipleElement) return idxToElemList;
    for (int j = 0; j < 4; ++j) {
      for (int k = 0; k < 4; ++k) jacbak[j][k] = jac[j][k];
    }
    detbak = det;
    t1bak = t1;
    t2bak = t2;
    t3bak = t3;
    t4bak = t4;
    imapbak = imap;
    if (m_debug) {
      PrintElement("FindElement13", x, y, z, t1, t2, t3, t4, imap, 10);
    }
  }

  // In checking mode, verify the tetrahedron/triangle count.
  if (m_checkMultipleElement) {
    if (nfound < 1) {
      if (m_debug) {
        std::cout << m_className << "::FindElement13:\n";
        std::cout << "    No element matching point (" << x << ", " << y << ", "
                  << z << ") found.\n";
      }
      m_lastElement = -1;
      return -1;
    }
    if (nfound > 1) {
      std::cerr << m_className << "::FindElement13:\n";
      std::cerr << "    Found << " << nfound << " elements matching point ("
                << x << ", " << y << ", " << z << ").\n";
    }
    if (nfound > 0) {
      for (int j = 0; j < 4; ++j) {
        for (int k = 0; k < 4; ++k) jac[j][k] = jacbak[j][k];
      }
      det = detbak;
      t1 = t1bak;
      t2 = t2bak;
      t3 = t3bak;
      t4 = t4bak;
      imap = imapbak;
      m_lastElement = imap;
      return imap;
    }
  }

  if (m_debug) {
    std::cout << m_className << "::FindElement13:\n";
    std::cout << "    No element matching point (" << x << ", " << y << ", "
              << z << ") found.\n";
  }
  return -1;
}

int ComponentFieldMap::FindElementCube(const double x, const double y,
                                       const double z, double& t1, double& t2,
                                       double& t3, TMatrixD*& jac,
                                       std::vector<TMatrixD*>& dN) {

  int imap = -1;
  if (m_lastElement >= 0) {
    const Element& element = elements[m_lastElement];
    const Node& n3 = nodes[element.emap[3]];
    if (x >= n3.x && y >= n3.y && z >= n3.z) {
      const Node& n0 = nodes[element.emap[0]];
      const Node& n2 = nodes[element.emap[2]];
      const Node& n7 = nodes[element.emap[7]];
      if (x < n0.x && y < n2.y && z < n7.z) {
        imap = m_lastElement;
      }
    }
  }

  // Default element loop
  if (imap == -1) {
    for (int i = 0; i < nElements; ++i) {
      const Element& element = elements[i];
      const Node& n3 = nodes[element.emap[3]];
      if (x < n3.x || y < n3.y || z < n3.z) continue;
      const Node& n0 = nodes[element.emap[0]];
      const Node& n2 = nodes[element.emap[2]];
      const Node& n7 = nodes[element.emap[7]];
      if (x < n0.x && y < n2.y && z < n7.z) {
        imap = i;
        break;
      }
    }
  }

  if (imap < 0) {
    if (m_debug) {
      std::cout << m_className << "::FindElementCube:\n";
      std::cout << "    Point (" << x << "," << y << "," << z
                << ") not in the mesh, it is background or PEC.\n";
      const Node& first0 = nodes[elements.front().emap[0]]; 
      const Node& first2 = nodes[elements.front().emap[2]]; 
      const Node& first3 = nodes[elements.front().emap[3]]; 
      const Node& first7 = nodes[elements.front().emap[7]]; 
      std::cout << "    First node (" << first3.x << ","
                << first3.y << "," << first3.z << ") in the mesh.\n";
      std::cout << "  dx= " << (first0.x - first3.x)
                << ", dy= " << (first2.y - first3.y)
                << ", dz= " << (first7.z - first3.z) << "\n";
      const Node& last0 = nodes[elements.back().emap[0]];
      const Node& last2 = nodes[elements.back().emap[2]];
      const Node& last3 = nodes[elements.back().emap[3]];
      const Node& last5 = nodes[elements.back().emap[5]];
      const Node& last7 = nodes[elements.back().emap[7]];
      std::cout << "    Last node (" << last5.x << ","
                << last5.y << "," << last5.z << ") in the mesh.\n";
      std::cout << "  dx= " << (last0.x - last3.x)
                << ", dy= " << (last2.y - last3.y)
                << ", dz= " << (last7.z - last3.z) << "\n";
    }
    return -1;
  }
  CoordinatesCube(x, y, z, t1, t2, t3, jac, dN, imap);
  if (m_debug) {
    PrintElement("FindElementCube", x, y, z, t1, t2, t3, 0., imap, 8);
  }
  return imap;
}

void ComponentFieldMap::Jacobian3(const unsigned int i, const double u,
                                  const double v, const double w, double& det,
                                  double jac[4][4]) const {

  // Initial values
  det = 0;
  jac[0][0] = 0;
  jac[0][1] = 0;
  jac[1][0] = 0;
  jac[1][1] = 0;

  const Element& element = elements[i];
  const Node& n0 = nodes[element.emap[0]];
  const Node& n1 = nodes[element.emap[1]];
  const Node& n2 = nodes[element.emap[2]];
  const Node& n3 = nodes[element.emap[3]];
  const Node& n4 = nodes[element.emap[4]];
  const Node& n5 = nodes[element.emap[5]];

  // Determinant of the quadratic triangular Jacobian
  det = -(((-1 + 4 * v) * n1.x + n2.x - 4 * w * n2.x + 4 * u * n3.x -
           4 * u * n4.x - 4 * v * n5.x + 4 * w * n5.x) *
          (-n0.y + 4 * u * n0.y + 4 * v * n3.y + 4 * w * n4.y)) -
        ((-1 + 4 * u) * n0.x + n1.x - 4 * v * n1.x - 4 * u * n3.x +
         4 * v * n3.x + 4 * w * n4.x - 4 * w * n5.x) *
            (-n2.y + 4 * w * n2.y + 4 * u * n4.y + 4 * v * n5.y) +
        ((-1 + 4 * u) * n0.x + n2.x - 4 * w * n2.x + 4 * v * n3.x -
         4 * u * n4.x + 4 * w * n4.x - 4 * v * n5.x) *
            (-n1.y + 4 * v * n1.y + 4 * u * n3.y + 4 * w * n5.y);

  // Terms of the quadratic triangular Jacobian
  jac[0][0] = (-n1.x + 4 * v * n1.x + 4 * u * n3.x + 4 * w * n5.x) *
                  (-n2.y + 4 * w * n2.y + 4 * u * n4.y + 4 * v * n5.y) -
              (-n2.x + 4 * w * n2.x + 4 * u * n4.x + 4 * v * n5.x) *
                  (-n1.y + 4 * v * n1.y + 4 * u * n3.y + 4 * w * n5.y);
  jac[0][1] = (-1 + 4 * v) * n1.y + n2.y - 4 * w * n2.y + 4 * u * n3.y -
              4 * u * n4.y - 4 * v * n5.y + 4 * w * n5.y;
  jac[0][2] = n1.x - 4 * v * n1.x + (-1 + 4 * w) * n2.x - 4 * u * n3.x +
              4 * u * n4.x + 4 * v * n5.x - 4 * w * n5.x;
  jac[1][0] = (-n2.x + 4 * w * n2.x + 4 * u * n4.x + 4 * v * n5.x) *
                  (-n0.y + 4 * u * n0.y + 4 * v * n3.y + 4 * w * n4.y) -
              (-n0.x + 4 * u * n0.x + 4 * v * n3.x + 4 * w * n4.x) *
                  (-n2.y + 4 * w * n2.y + 4 * u * n4.y + 4 * v * n5.y);
  jac[1][1] = n0.y - 4 * u * n0.y - n2.y + 4 * w * n2.y - 4 * v * n3.y +
              4 * u * n4.y - 4 * w * n4.y + 4 * v * n5.y;
  jac[1][2] = (-1 + 4 * u) * n0.x + n2.x - 4 * w * n2.x + 4 * v * n3.x -
              4 * u * n4.x + 4 * w * n4.x - 4 * v * n5.x;
  jac[2][0] = -((-n1.x + 4 * v * n1.x + 4 * u * n3.x + 4 * w * n5.x) *
                (-n0.y + 4 * u * n0.y + 4 * v * n3.y + 4 * w * n4.y)) +
              (-n0.x + 4 * u * n0.x + 4 * v * n3.x + 4 * w * n4.x) *
                  (-n1.y + 4 * v * n1.y + 4 * u * n3.y + 4 * w * n5.y);
  jac[2][1] = (-1 + 4 * u) * n0.y + n1.y - 4 * v * n1.y - 4 * u * n3.y +
              4 * v * n3.y + 4 * w * n4.y - 4 * w * n5.y;
  jac[2][2] = n0.x - 4 * u * n0.x - n1.x + 4 * v * n1.x + 4 * u * n3.x -
              4 * v * n3.x - 4 * w * n4.x + 4 * w * n5.x;
}

void ComponentFieldMap::Jacobian5(const unsigned int i, const double u,
                                  const double v, double& det,
                                  double jac[4][4]) const {

  // Initial values
  det = 0;
  jac[0][0] = 0;
  jac[0][1] = 0;
  jac[1][0] = 0;
  jac[1][1] = 0;

  const Element& element = elements[i];
  const Node& n0 = nodes[element.emap[0]];
  const Node& n1 = nodes[element.emap[1]];
  const Node& n2 = nodes[element.emap[2]];
  const Node& n3 = nodes[element.emap[3]];
  const Node& n4 = nodes[element.emap[4]];
  const Node& n5 = nodes[element.emap[5]];
  const Node& n6 = nodes[element.emap[6]];
  const Node& n7 = nodes[element.emap[7]];
  // Determinant of the quadrilateral serendipity Jacobian
  det =
      (-2 * u * u * u * ((n2.x + n3.x - 2 * n6.x) * (n0.y + n1.y - 2 * n4.y) -
                         (n0.x + n1.x - 2 * n4.x) * (n2.y + n3.y - 2 * n6.y)) +
       2 * v * v * v * (-((n0.x + n3.x - 2 * n7.x) * (n1.y + n2.y - 2 * n5.y)) +
                        (n1.x + n2.x - 2 * n5.x) * (n0.y + n3.y - 2 * n7.y)) +
       2 * (-((n5.x - n7.x) * (n4.y - n6.y)) + (n4.x - n6.x) * (n5.y - n7.y)) +
       v * (-(n6.x * n0.y) - 2 * n7.x * n0.y + n6.x * n1.y - 2 * n7.x * n1.y -
            n6.x * n2.y - 2 * n7.x * n2.y + n4.x * (n0.y - n1.y + n2.y - n3.y) +
            n6.x * n3.y - 2 * n7.x * n3.y - n0.x * n4.y + n1.x * n4.y -
            n2.x * n4.y + n3.x * n4.y - 2 * n0.x * n5.y - 2 * n1.x * n5.y -
            2 * n2.x * n5.y - 2 * n3.x * n5.y + 8 * n7.x * n5.y + n0.x * n6.y -
            n1.x * n6.y + n2.x * n6.y - n3.x * n6.y +
            2 * n5.x * (n0.y + n1.y + n2.y + n3.y - 4 * n7.y) +
            2 * (n0.x + n1.x + n2.x + n3.x) * n7.y) +
       v * v *
           (-(n4.x * n0.y) + 2 * n5.x * n0.y + n6.x * n0.y + 2 * n7.x * n0.y +
            n4.x * n1.y - 2 * n5.x * n1.y - n6.x * n1.y - 2 * n7.x * n1.y +
            n4.x * n2.y + 2 * n5.x * n2.y - n6.x * n2.y + 2 * n7.x * n2.y -
            n4.x * n3.y - 2 * n5.x * n3.y + n6.x * n3.y - 2 * n7.x * n3.y +
            2 * n2.x * (n1.y + n3.y) - n2.x * n4.y + 2 * n5.x * n4.y -
            2 * n7.x * n4.y - 2 * n2.x * n5.y - 2 * n4.x * n5.y +
            2 * n6.x * n5.y + n2.x * n6.y - 2 * n5.x * n6.y + 2 * n7.x * n6.y +
            n0.x * (2 * n1.y + 2 * n3.y + n4.y - 2 * n5.y - n6.y - 2 * n7.y) -
            2 * (n2.x - n4.x + n6.x) * n7.y +
            n3.x * (-2 * n0.y - 2 * n2.y + n4.y + 2 * n5.y - n6.y + 2 * n7.y) +
            n1.x * (-2 * n0.y - 2 * n2.y - n4.y + 2 * n5.y + n6.y + 2 * n7.y)) +
       u * (n5.x * n0.y - 2 * n6.x * n0.y - n7.x * n0.y - n5.x * n1.y -
            2 * n6.x * n1.y + n7.x * n1.y + n5.x * n2.y - 2 * n6.x * n2.y -
            n7.x * n2.y - n5.x * n3.y - 2 * n6.x * n3.y + n7.x * n3.y -
            2 * n1.x * n4.y - 2 * n2.x * n4.y - 2 * n3.x * n4.y +
            8 * n6.x * n4.y + n1.x * n5.y - n2.x * n5.y + n3.x * n5.y +
            2 * n4.x * (n0.y + n1.y + n2.y + n3.y - 4 * n6.y) +
            2 * n1.x * n6.y + 2 * n2.x * n6.y + 2 * n3.x * n6.y -
            (n1.x - n2.x + n3.x) * n7.y +
            n0.x * (-2 * n4.y - n5.y + 2 * n6.y + n7.y) +
            v * v * (4 * n4.x * n0.y - 3 * n5.x * n0.y - 4 * n6.x * n0.y -
                     5 * n7.x * n0.y + 4 * n4.x * n1.y - 5 * n5.x * n1.y -
                     4 * n6.x * n1.y - 3 * n7.x * n1.y + 4 * n4.x * n2.y +
                     5 * n5.x * n2.y - 4 * n6.x * n2.y + 3 * n7.x * n2.y +
                     4 * n4.x * n3.y + 3 * n5.x * n3.y - 4 * n6.x * n3.y +
                     5 * n7.x * n3.y + 8 * n5.x * n4.y + 8 * n7.x * n4.y -
                     8 * n4.x * n5.y + 8 * n6.x * n5.y - 8 * n5.x * n6.y -
                     8 * n7.x * n6.y + n3.x * (5 * n0.y + 3 * n1.y - 4 * n4.y -
                                               3 * n5.y + 4 * n6.y - 5 * n7.y) +
                     n2.x * (3 * n0.y + 5 * n1.y - 4 * n4.y - 5 * n5.y +
                             4 * n6.y - 3 * n7.y) -
                     8 * n4.x * n7.y + 8 * n6.x * n7.y +
                     n1.x * (-5 * n2.y - 3 * n3.y - 4 * n4.y + 5 * n5.y +
                             4 * n6.y + 3 * n7.y) +
                     n0.x * (-3 * n2.y - 5 * n3.y - 4 * n4.y + 3 * n5.y +
                             4 * n6.y + 5 * n7.y)) -
            2 * v * (n6.x * n0.y - 3 * n7.x * n0.y + n6.x * n1.y - n7.x * n1.y +
                     3 * n6.x * n2.y - n7.x * n2.y + 3 * n6.x * n3.y -
                     3 * n7.x * n3.y - 3 * n0.x * n4.y - 3 * n1.x * n4.y -
                     n2.x * n4.y - n3.x * n4.y + 4 * n7.x * n4.y + n0.x * n5.y +
                     3 * n1.x * n5.y + 3 * n2.x * n5.y + n3.x * n5.y -
                     4 * n6.x * n5.y - n0.x * n6.y - n1.x * n6.y -
                     3 * n2.x * n6.y - 3 * n3.x * n6.y + 4 * n7.x * n6.y -
                     n5.x * (n0.y + 3 * n1.y + 3 * n2.y + n3.y -
                             4 * (n4.y + n6.y)) +
                     (3 * n0.x + n1.x + n2.x + 3 * n3.x - 4 * n6.x) * n7.y +
                     n4.x * (3 * n0.y + 3 * n1.y + n2.y + n3.y -
                             4 * (n5.y + n7.y)))) +
       u * u *
           (2 * n3.x * n0.y - 2 * n4.x * n0.y - n5.x * n0.y - 2 * n6.x * n0.y +
            n7.x * n0.y - 2 * n0.x * n1.y + 2 * n4.x * n1.y - n5.x * n1.y +
            2 * n6.x * n1.y + n7.x * n1.y + 2 * n3.x * n2.y - 2 * n4.x * n2.y +
            n5.x * n2.y - 2 * n6.x * n2.y - n7.x * n2.y + 2 * n4.x * n3.y +
            n5.x * n3.y + 2 * n6.x * n3.y - n7.x * n3.y - 2 * n3.x * n4.y +
            2 * n5.x * n4.y - 2 * n7.x * n4.y - n3.x * n5.y - 2 * n4.x * n5.y +
            2 * n6.x * n5.y - 2 * n3.x * n6.y - 2 * n5.x * n6.y +
            2 * n7.x * n6.y +
            n0.x * (-2 * n3.y + 2 * n4.y + n5.y + 2 * n6.y - n7.y) +
            (n3.x + 2 * n4.x - 2 * n6.x) * n7.y +
            n2.x * (-2 * n1.y - 2 * n3.y + 2 * n4.y - n5.y + 2 * n6.y + n7.y) -
            3 * v * v *
                (n5.x * n0.y - n6.x * n0.y - n7.x * n0.y + n5.x * n1.y +
                 n6.x * n1.y - n7.x * n1.y - n5.x * n2.y + n6.x * n2.y +
                 n7.x * n2.y - n5.x * n3.y - n6.x * n3.y + n7.x * n3.y -
                 2 * n5.x * n4.y + 2 * n7.x * n4.y - 2 * n6.x * n5.y +
                 2 * n5.x * n6.y - 2 * n7.x * n6.y +
                 n4.x * (n0.y - n1.y - n2.y + n3.y + 2 * n5.y - 2 * n7.y) +
                 n3.x * (n0.y - n2.y - n4.y + n5.y + n6.y - n7.y) +
                 2 * n6.x * n7.y +
                 (n0.x - n2.x) * (n1.y - n3.y - n4.y - n5.y + n6.y + n7.y)) +
            v * (4 * n5.x * n0.y + 3 * n6.x * n0.y - 4 * n7.x * n0.y +
                 4 * n5.x * n1.y - 3 * n6.x * n1.y - 4 * n7.x * n1.y +
                 4 * n5.x * n2.y - 5 * n6.x * n2.y - 4 * n7.x * n2.y +
                 4 * n5.x * n3.y + 5 * n6.x * n3.y - 4 * n7.x * n3.y -
                 8 * n5.x * n4.y + 8 * n7.x * n4.y + 8 * n6.x * n5.y -
                 8 * n5.x * n6.y + 8 * n7.x * n6.y +
                 n4.x * (5 * n0.y - 5 * n1.y - 3 * n2.y + 3 * n3.y + 8 * n5.y -
                         8 * n7.y) -
                 8 * n6.x * n7.y + n3.x * (3 * n1.y + 5 * n2.y - 3 * n4.y -
                                           4 * n5.y - 5 * n6.y + 4 * n7.y) +
                 n0.x * (5 * n1.y + 3 * n2.y - 5 * n4.y - 4 * n5.y - 3 * n6.y +
                         4 * n7.y) +
                 n2.x * (-3 * n0.y - 5 * n3.y + 3 * n4.y - 4 * n5.y + 5 * n6.y +
                         4 * n7.y)) +
            n1.x * ((-1 + v) * (-2 + 3 * v) * n0.y + 2 * n2.y - 2 * n4.y +
                    n5.y - 2 * n6.y - n7.y +
                    v * (-3 * n3.y + 5 * n4.y - 4 * n5.y + 3 * n6.y + 4 * n7.y -
                         3 * v * (n2.y + n4.y - n5.y - n6.y + n7.y))))) /
      8;
  // Jacobian terms
  jac[0][0] =
      (u * u * (-n0.y - n1.y + n2.y + n3.y + 2 * n4.y - 2 * n6.y) +
       2 * (-n4.y + n6.y +
            v * (n0.y + n1.y + n2.y + n3.y - 2 * n5.y - 2 * n7.y)) +
       u * (n0.y - 2 * v * n0.y - n1.y + 2 * v * n1.y + n2.y + 2 * v * n2.y -
            n3.y - 2 * v * n3.y - 4 * v * n5.y + 4 * v * n7.y)) /
      4;
  jac[0][1] =
      (u * u * (n0.x + n1.x - n2.x - n3.x - 2 * n4.x + 2 * n6.x) -
       2 * (-n4.x + n6.x +
            v * (n0.x + n1.x + n2.x + n3.x - 2 * n5.x - 2 * n7.x)) +
       u * ((-1 + 2 * v) * n0.x + n1.x - 2 * v * n1.x - n2.x - 2 * v * n2.x +
            n3.x + 2 * v * n3.x + 4 * v * n5.x - 4 * v * n7.x)) /
      4;
  jac[1][0] =
      (v * (-n0.y + n1.y - n2.y + n3.y) - 2 * n5.y +
       2 * u * ((-1 + v) * n0.y + (-1 + v) * n1.y - n2.y - v * n2.y - n3.y -
                v * n3.y + 2 * n4.y - 2 * v * n4.y + 2 * n6.y + 2 * v * n6.y) +
       v * v * (n0.y - n1.y - n2.y + n3.y + 2 * n5.y - 2 * n7.y) + 2 * n7.y) /
      4;
  jac[1][1] =
      (v * (n0.x - n1.x + n2.x - n3.x) +
       2 * u * (n0.x - v * n0.x + n1.x - v * n1.x + n2.x + v * n2.x + n3.x +
                v * n3.x - 2 * n4.x + 2 * v * n4.x - 2 * n6.x - 2 * v * n6.x) +
       2 * (n5.x - n7.x) +
       v * v * (-n0.x + n1.x + n2.x - n3.x - 2 * n5.x + 2 * n7.x)) /
      4;
}

void ComponentFieldMap::Jacobian13(const unsigned int i, const double t,
                                   const double u, const double v,
                                   const double w, double& det,
                                   double jac[4][4]) const {

  // Initial values
  det = 0;
  for (int j = 0; j < 4; ++j) {
    for (int k = 0; k < 4; ++k) jac[j][k] = 0;
  }

  const Element& element = elements[i];
  const Node& n0 = nodes[element.emap[0]];
  const Node& n1 = nodes[element.emap[1]];
  const Node& n2 = nodes[element.emap[2]];
  const Node& n3 = nodes[element.emap[3]];
  const Node& n4 = nodes[element.emap[4]];
  const Node& n5 = nodes[element.emap[5]];
  const Node& n6 = nodes[element.emap[6]];
  const Node& n7 = nodes[element.emap[7]];
  const Node& n8 = nodes[element.emap[8]];
  const Node& n9 = nodes[element.emap[9]];
  // Determinant of the quadrilateral serendipity Jacobian
  det =
      -(((-4 * v * n9.x - n1.x + 4 * u * n1.x + n3.x - 4 * w * n3.x +
          4 * t * n4.x - 4 * t * n6.x + 4 * v * n7.x - 4 * u * n8.x +
          4 * w * n8.x) *
             (4 * w * n9.y - n2.y + 4 * v * n2.y + 4 * t * n5.y +
              4 * u * n7.y) +
         (n1.x - 4 * u * n1.x - n2.x + 4 * v * n2.x - 4 * t * n4.x +
          4 * t * n5.x + 4 * u * n7.x - 4 * v * n7.x + 4 * w * (n9.x - n8.x)) *
             (4 * v * n9.y - n3.y + 4 * w * n3.y + 4 * t * n6.y +
              4 * u * n8.y) +
         (-4 * w * n9.x + 4 * v * (n9.x - n2.x) + n2.x - n3.x + 4 * w * n3.x -
          4 * t * n5.x + 4 * t * n6.x - 4 * u * n7.x + 4 * u * n8.x) *
             ((-1 + 4 * u) * n1.y + 4 * (t * n4.y + v * n7.y + w * n8.y))) *
        ((-1 + 4 * t) * n0.z + 4 * (u * n4.z + v * n5.z + w * n6.z))) -
      ((n1.x - 4 * u * n1.x - n3.x + 4 * w * n3.x - 4 * t * n4.x +
        4 * t * n6.x + 4 * v * (n9.x - n7.x) + 4 * u * n8.x - 4 * w * n8.x) *
           ((-1 + 4 * t) * n0.y + 4 * (u * n4.y + v * n5.y + w * n6.y)) -
       ((-1 + 4 * t) * n0.x + n1.x - 4 * u * n1.x +
        4 * (-(t * n4.x) + u * n4.x + v * n5.x + w * n6.x - v * n7.x -
             w * n8.x)) *
           (4 * v * n9.y - n3.y + 4 * w * n3.y + 4 * t * n6.y + 4 * u * n8.y) +
       ((-1 + 4 * t) * n0.x - 4 * v * n9.x + n3.x - 4 * w * n3.x +
        4 * u * n4.x + 4 * v * n5.x - 4 * t * n6.x + 4 * w * n6.x -
        4 * u * n8.x) *
           ((-1 + 4 * u) * n1.y + 4 * (t * n4.y + v * n7.y + w * n8.y))) *
          (4 * w * n9.z - n2.z + 4 * v * n2.z + 4 * t * n5.z + 4 * u * n7.z) +
      ((n1.x - 4 * u * n1.x - n2.x + 4 * v * n2.x - 4 * t * n4.x +
        4 * t * n5.x + 4 * u * n7.x - 4 * v * n7.x + 4 * w * (n9.x - n8.x)) *
           ((-1 + 4 * t) * n0.y + 4 * (u * n4.y + v * n5.y + w * n6.y)) -
       ((-1 + 4 * t) * n0.x + n1.x - 4 * u * n1.x +
        4 * (-(t * n4.x) + u * n4.x + v * n5.x + w * n6.x - v * n7.x -
             w * n8.x)) *
           (4 * w * n9.y - n2.y + 4 * v * n2.y + 4 * t * n5.y + 4 * u * n7.y) +
       ((-1 + 4 * t) * n0.x - 4 * w * n9.x + n2.x - 4 * v * n2.x +
        4 * u * n4.x - 4 * t * n5.x + 4 * v * n5.x + 4 * w * n6.x -
        4 * u * n7.x) *
           ((-1 + 4 * u) * n1.y + 4 * (t * n4.y + v * n7.y + w * n8.y))) *
          (4 * v * n9.z - n3.z + 4 * w * n3.z + 4 * t * n6.z + 4 * u * n8.z) +
      ((-4 * w * n9.x + 4 * v * (n9.x - n2.x) + n2.x - n3.x + 4 * w * n3.x -
        4 * t * n5.x + 4 * t * n6.x - 4 * u * n7.x + 4 * u * n8.x) *
           ((-1 + 4 * t) * n0.y + 4 * (u * n4.y + v * n5.y + w * n6.y)) +
       ((-1 + 4 * t) * n0.x - 4 * v * n9.x + n3.x - 4 * w * n3.x +
        4 * u * n4.x + 4 * v * n5.x - 4 * t * n6.x + 4 * w * n6.x -
        4 * u * n8.x) *
           (4 * w * n9.y - n2.y + 4 * v * n2.y + 4 * t * n5.y + 4 * u * n7.y) -
       ((-1 + 4 * t) * n0.x - 4 * w * n9.x + n2.x - 4 * v * n2.x +
        4 * u * n4.x - 4 * t * n5.x + 4 * v * n5.x + 4 * w * n6.x -
        4 * u * n7.x) *
           (4 * v * n9.y - n3.y + 4 * w * n3.y + 4 * t * n6.y + 4 * u * n8.y)) *
          ((-1 + 4 * u) * n1.z + 4 * (t * n4.z + v * n7.z + w * n8.z));

  jac[0][0] =
      -((((-1 + 4 * u) * n1.x + 4 * (t * n4.x + v * n7.x + w * n8.x)) *
             (4 * v * n9.y - n3.y + 4 * w * n3.y + 4 * t * n6.y +
              4 * u * n8.y) -
         (4 * v * n9.x - n3.x + 4 * w * n3.x + 4 * t * n6.x + 4 * u * n8.x) *
             ((-1 + 4 * u) * n1.y + 4 * (t * n4.y + v * n7.y + w * n8.y))) *
        (4 * w * n9.z - n2.z + 4 * v * n2.z + 4 * t * n5.z + 4 * u * n7.z)) +
      (((-1 + 4 * u) * n1.x + 4 * (t * n4.x + v * n7.x + w * n8.x)) *
           (4 * w * n9.y - n2.y + 4 * v * n2.y + 4 * t * n5.y + 4 * u * n7.y) -
       (4 * w * n9.x - n2.x + 4 * v * n2.x + 4 * t * n5.x + 4 * u * n7.x) *
           ((-1 + 4 * u) * n1.y + 4 * (t * n4.y + v * n7.y + w * n8.y))) *
          (4 * v * n9.z - n3.z + 4 * w * n3.z + 4 * t * n6.z + 4 * u * n8.z) +
      (-((4 * v * n9.x - n3.x + 4 * w * n3.x + 4 * t * n6.x + 4 * u * n8.x) *
         (4 * w * n9.y - n2.y + 4 * v * n2.y + 4 * t * n5.y + 4 * u * n7.y)) +
       (4 * w * n9.x - n2.x + 4 * v * n2.x + 4 * t * n5.x + 4 * u * n7.x) *
           (4 * v * n9.y - n3.y + 4 * w * n3.y + 4 * t * n6.y + 4 * u * n8.y)) *
          ((-1 + 4 * u) * n1.z + 4 * (t * n4.z + v * n7.z + w * n8.z));

  jac[0][1] =
      (n1.y - 4 * u * n1.y - n3.y + 4 * w * n3.y - 4 * t * n4.y + 4 * t * n6.y +
       4 * v * (n9.y - n7.y) + 4 * u * n8.y - 4 * w * n8.y) *
          (4 * w * n9.z - n2.z + 4 * v * n2.z + 4 * t * n5.z + 4 * u * n7.z) +
      (-4 * w * n9.y - n1.y + 4 * u * n1.y + n2.y - 4 * v * n2.y +
       4 * t * n4.y - 4 * t * n5.y - 4 * u * n7.y + 4 * v * n7.y +
       4 * w * n8.y) *
          (4 * v * n9.z - n3.z + 4 * w * n3.z + 4 * t * n6.z + 4 * u * n8.z) +
      (-4 * v * n9.y + 4 * w * n9.y - n2.y + 4 * v * n2.y + n3.y -
       4 * w * n3.y + 4 * t * n5.y - 4 * t * n6.y + 4 * u * n7.y -
       4 * u * n8.y) *
          ((-1 + 4 * u) * n1.z + 4 * (t * n4.z + v * n7.z + w * n8.z));

  jac[0][2] =
      (-4 * v * n9.x - n1.x + 4 * u * n1.x + n3.x - 4 * w * n3.x +
       4 * t * n4.x - 4 * t * n6.x + 4 * v * n7.x - 4 * u * n8.x +
       4 * w * n8.x) *
          (4 * w * n9.z - n2.z + 4 * v * n2.z + 4 * t * n5.z + 4 * u * n7.z) +
      (n1.x - 4 * u * n1.x - n2.x + 4 * v * n2.x - 4 * t * n4.x + 4 * t * n5.x +
       4 * u * n7.x - 4 * v * n7.x + 4 * w * (n9.x - n8.x)) *
          (4 * v * n9.z - n3.z + 4 * w * n3.z + 4 * t * n6.z + 4 * u * n8.z) +
      (-4 * w * n9.x + 4 * v * (n9.x - n2.x) + n2.x - n3.x + 4 * w * n3.x -
       4 * t * n5.x + 4 * t * n6.x - 4 * u * n7.x + 4 * u * n8.x) *
          ((-1 + 4 * u) * n1.z + 4 * (t * n4.z + v * n7.z + w * n8.z));

  jac[0][3] =
      (n1.x - 4 * u * n1.x - n3.x + 4 * w * n3.x - 4 * t * n4.x + 4 * t * n6.x +
       4 * v * (n9.x - n7.x) + 4 * u * n8.x - 4 * w * n8.x) *
          (4 * w * n9.y - n2.y + 4 * v * n2.y + 4 * t * n5.y + 4 * u * n7.y) +
      (-4 * w * n9.x - n1.x + 4 * u * n1.x + n2.x - 4 * v * n2.x +
       4 * t * n4.x - 4 * t * n5.x - 4 * u * n7.x + 4 * v * n7.x +
       4 * w * n8.x) *
          (4 * v * n9.y - n3.y + 4 * w * n3.y + 4 * t * n6.y + 4 * u * n8.y) +
      (-4 * v * n9.x + 4 * w * n9.x - n2.x + 4 * v * n2.x + n3.x -
       4 * w * n3.x + 4 * t * n5.x - 4 * t * n6.x + 4 * u * n7.x -
       4 * u * n8.x) *
          ((-1 + 4 * u) * n1.y + 4 * (t * n4.y + v * n7.y + w * n8.y));

  jac[1][0] =
      -((-((4 * v * n9.x - n3.x + 4 * w * n3.x + 4 * t * n6.x + 4 * u * n8.x) *
           (4 * w * n9.y - n2.y + 4 * v * n2.y + 4 * t * n5.y + 4 * u * n7.y)) +
         (4 * w * n9.x - n2.x + 4 * v * n2.x + 4 * t * n5.x + 4 * u * n7.x) *
             (4 * v * n9.y - n3.y + 4 * w * n3.y + 4 * t * n6.y +
              4 * u * n8.y)) *
        ((-1 + 4 * t) * n0.z + 4 * (u * n4.z + v * n5.z + w * n6.z))) +
      (-((4 * v * n9.x - n3.x + 4 * w * n3.x + 4 * t * n6.x + 4 * u * n8.x) *
         ((-1 + 4 * t) * n0.y + 4 * (u * n4.y + v * n5.y + w * n6.y))) +
       ((-1 + 4 * t) * n0.x + 4 * (u * n4.x + v * n5.x + w * n6.x)) *
           (4 * v * n9.y - n3.y + 4 * w * n3.y + 4 * t * n6.y + 4 * u * n8.y)) *
          (4 * w * n9.z - n2.z + 4 * v * n2.z + 4 * t * n5.z + 4 * u * n7.z) -
      (-((4 * w * n9.x - n2.x + 4 * v * n2.x + 4 * t * n5.x + 4 * u * n7.x) *
         ((-1 + 4 * t) * n0.y + 4 * (u * n4.y + v * n5.y + w * n6.y))) +
       ((-1 + 4 * t) * n0.x + 4 * (u * n4.x + v * n5.x + w * n6.x)) *
           (4 * w * n9.y - n2.y + 4 * v * n2.y + 4 * t * n5.y + 4 * u * n7.y)) *
          (4 * v * n9.z - n3.z + 4 * w * n3.z + 4 * t * n6.z + 4 * u * n8.z);

  jac[1][1] =
      (-4 * w * n9.y + 4 * v * (n9.y - n2.y) + n2.y - n3.y + 4 * w * n3.y -
       4 * t * n5.y + 4 * t * n6.y - 4 * u * n7.y + 4 * u * n8.y) *
          ((-1 + 4 * t) * n0.z + 4 * (u * n4.z + v * n5.z + w * n6.z)) +
      ((-1 + 4 * t) * n0.y - 4 * v * n9.y + n3.y - 4 * w * n3.y + 4 * u * n4.y +
       4 * v * n5.y - 4 * t * n6.y + 4 * w * n6.y - 4 * u * n8.y) *
          (4 * w * n9.z - n2.z + 4 * v * n2.z + 4 * t * n5.z + 4 * u * n7.z) -
      ((-1 + 4 * t) * n0.y - 4 * w * n9.y + n2.y - 4 * v * n2.y + 4 * u * n4.y -
       4 * t * n5.y + 4 * v * n5.y + 4 * w * n6.y - 4 * u * n7.y) *
          (4 * v * n9.z - n3.z + 4 * w * n3.z + 4 * t * n6.z + 4 * u * n8.z);

  jac[1][2] =
      (-4 * v * n9.x + 4 * w * n9.x - n2.x + 4 * v * n2.x + n3.x -
       4 * w * n3.x + 4 * t * n5.x - 4 * t * n6.x + 4 * u * n7.x -
       4 * u * n8.x) *
          ((-1 + 4 * t) * n0.z + 4 * (u * n4.z + v * n5.z + w * n6.z)) -
      ((-1 + 4 * t) * n0.x - 4 * v * n9.x + n3.x - 4 * w * n3.x + 4 * u * n4.x +
       4 * v * n5.x - 4 * t * n6.x + 4 * w * n6.x - 4 * u * n8.x) *
          (4 * w * n9.z - n2.z + 4 * v * n2.z + 4 * t * n5.z + 4 * u * n7.z) +
      ((-1 + 4 * t) * n0.x - 4 * w * n9.x + n2.x - 4 * v * n2.x + 4 * u * n4.x -
       4 * t * n5.x + 4 * v * n5.x + 4 * w * n6.x - 4 * u * n7.x) *
          (4 * v * n9.z - n3.z + 4 * w * n3.z + 4 * t * n6.z + 4 * u * n8.z);

  jac[1][3] =
      (-4 * w * n9.x + 4 * v * (n9.x - n2.x) + n2.x - n3.x + 4 * w * n3.x -
       4 * t * n5.x + 4 * t * n6.x - 4 * u * n7.x + 4 * u * n8.x) *
          ((-1 + 4 * t) * n0.y + 4 * (u * n4.y + v * n5.y + w * n6.y)) +
      ((-1 + 4 * t) * n0.x - 4 * v * n9.x + n3.x - 4 * w * n3.x + 4 * u * n4.x +
       4 * v * n5.x - 4 * t * n6.x + 4 * w * n6.x - 4 * u * n8.x) *
          (4 * w * n9.y - n2.y + 4 * v * n2.y + 4 * t * n5.y + 4 * u * n7.y) -
      ((-1 + 4 * t) * n0.x - 4 * w * n9.x + n2.x - 4 * v * n2.x + 4 * u * n4.x -
       4 * t * n5.x + 4 * v * n5.x + 4 * w * n6.x - 4 * u * n7.x) *
          (4 * v * n9.y - n3.y + 4 * w * n3.y + 4 * t * n6.y + 4 * u * n8.y);

  jac[2][0] =
      (((-1 + 4 * u) * n1.x + 4 * (t * n4.x + v * n7.x + w * n8.x)) *
           (4 * v * n9.y - n3.y + 4 * w * n3.y + 4 * t * n6.y + 4 * u * n8.y) -
       (4 * v * n9.x - n3.x + 4 * w * n3.x + 4 * t * n6.x + 4 * u * n8.x) *
           ((-1 + 4 * u) * n1.y + 4 * (t * n4.y + v * n7.y + w * n8.y))) *
          ((-1 + 4 * t) * n0.z + 4 * (u * n4.z + v * n5.z + w * n6.z)) +
      (-(((-1 + 4 * u) * n1.x + 4 * (t * n4.x + v * n7.x + w * n8.x)) *
         ((-1 + 4 * t) * n0.y + 4 * (u * n4.y + v * n5.y + w * n6.y))) +
       ((-1 + 4 * t) * n0.x + 4 * (u * n4.x + v * n5.x + w * n6.x)) *
           ((-1 + 4 * u) * n1.y + 4 * (t * n4.y + v * n7.y + w * n8.y))) *
          (4 * v * n9.z - n3.z + 4 * w * n3.z + 4 * t * n6.z + 4 * u * n8.z) -
      (-((4 * v * n9.x - n3.x + 4 * w * n3.x + 4 * t * n6.x + 4 * u * n8.x) *
         ((-1 + 4 * t) * n0.y + 4 * (u * n4.y + v * n5.y + w * n6.y))) +
       ((-1 + 4 * t) * n0.x + 4 * (u * n4.x + v * n5.x + w * n6.x)) *
           (4 * v * n9.y - n3.y + 4 * w * n3.y + 4 * t * n6.y + 4 * u * n8.y)) *
          ((-1 + 4 * u) * n1.z + 4 * (t * n4.z + v * n7.z + w * n8.z));

  jac[2][1] =
      (-4 * v * n9.y - n1.y + 4 * u * n1.y + n3.y - 4 * w * n3.y +
       4 * t * n4.y - 4 * t * n6.y + 4 * v * n7.y - 4 * u * n8.y +
       4 * w * n8.y) *
          ((-1 + 4 * t) * n0.z + 4 * (u * n4.z + v * n5.z + w * n6.z)) +
      ((-1 + 4 * t) * n0.y + n1.y - 4 * u * n1.y +
       4 * (-(t * n4.y) + u * n4.y + v * n5.y + w * n6.y - v * n7.y -
            w * n8.y)) *
          (4 * v * n9.z - n3.z + 4 * w * n3.z + 4 * t * n6.z + 4 * u * n8.z) -
      ((-1 + 4 * t) * n0.y - 4 * v * n9.y + n3.y - 4 * w * n3.y + 4 * u * n4.y +
       4 * v * n5.y - 4 * t * n6.y + 4 * w * n6.y - 4 * u * n8.y) *
          ((-1 + 4 * u) * n1.z + 4 * (t * n4.z + v * n7.z + w * n8.z));

  jac[2][2] =
      (n1.x - 4 * u * n1.x - n3.x + 4 * w * n3.x - 4 * t * n4.x + 4 * t * n6.x +
       4 * v * (n9.x - n7.x) + 4 * u * n8.x - 4 * w * n8.x) *
          ((-1 + 4 * t) * n0.z + 4 * (u * n4.z + v * n5.z + w * n6.z)) -
      ((-1 + 4 * t) * n0.x + n1.x - 4 * u * n1.x +
       4 * (-(t * n4.x) + u * n4.x + v * n5.x + w * n6.x - v * n7.x -
            w * n8.x)) *
          (4 * v * n9.z - n3.z + 4 * w * n3.z + 4 * t * n6.z + 4 * u * n8.z) +
      ((-1 + 4 * t) * n0.x - 4 * v * n9.x + n3.x - 4 * w * n3.x + 4 * u * n4.x +
       4 * v * n5.x - 4 * t * n6.x + 4 * w * n6.x - 4 * u * n8.x) *
          ((-1 + 4 * u) * n1.z + 4 * (t * n4.z + v * n7.z + w * n8.z));

  jac[2][3] =
      (-4 * v * n9.x - n1.x + 4 * u * n1.x + n3.x - 4 * w * n3.x +
       4 * t * n4.x - 4 * t * n6.x + 4 * v * n7.x - 4 * u * n8.x +
       4 * w * n8.x) *
          ((-1 + 4 * t) * n0.y + 4 * (u * n4.y + v * n5.y + w * n6.y)) +
      ((-1 + 4 * t) * n0.x + n1.x - 4 * u * n1.x +
       4 * (-(t * n4.x) + u * n4.x + v * n5.x + w * n6.x - v * n7.x -
            w * n8.x)) *
          (4 * v * n9.y - n3.y + 4 * w * n3.y + 4 * t * n6.y + 4 * u * n8.y) -
      ((-1 + 4 * t) * n0.x - 4 * v * n9.x + n3.x - 4 * w * n3.x + 4 * u * n4.x +
       4 * v * n5.x - 4 * t * n6.x + 4 * w * n6.x - 4 * u * n8.x) *
          ((-1 + 4 * u) * n1.y + 4 * (t * n4.y + v * n7.y + w * n8.y));

  jac[3][0] =
      -((((-1 + 4 * u) * n1.x + 4 * (t * n4.x + v * n7.x + w * n8.x)) *
             (4 * w * n9.y - n2.y + 4 * v * n2.y + 4 * t * n5.y +
              4 * u * n7.y) -
         (4 * w * n9.x - n2.x + 4 * v * n2.x + 4 * t * n5.x + 4 * u * n7.x) *
             ((-1 + 4 * u) * n1.y + 4 * (t * n4.y + v * n7.y + w * n8.y))) *
        ((-1 + 4 * t) * n0.z + 4 * (u * n4.z + v * n5.z + w * n6.z))) -
      (-(((-1 + 4 * u) * n1.x + 4 * (t * n4.x + v * n7.x + w * n8.x)) *
         ((-1 + 4 * t) * n0.y + 4 * (u * n4.y + v * n5.y + w * n6.y))) +
       ((-1 + 4 * t) * n0.x + 4 * (u * n4.x + v * n5.x + w * n6.x)) *
           ((-1 + 4 * u) * n1.y + 4 * (t * n4.y + v * n7.y + w * n8.y))) *
          (4 * w * n9.z - n2.z + 4 * v * n2.z + 4 * t * n5.z + 4 * u * n7.z) +
      (-((4 * w * n9.x - n2.x + 4 * v * n2.x + 4 * t * n5.x + 4 * u * n7.x) *
         ((-1 + 4 * t) * n0.y + 4 * (u * n4.y + v * n5.y + w * n6.y))) +
       ((-1 + 4 * t) * n0.x + 4 * (u * n4.x + v * n5.x + w * n6.x)) *
           (4 * w * n9.y - n2.y + 4 * v * n2.y + 4 * t * n5.y + 4 * u * n7.y)) *
          ((-1 + 4 * u) * n1.z + 4 * (t * n4.z + v * n7.z + w * n8.z));

  jac[3][1] =
      (n1.y - 4 * u * n1.y - n2.y + 4 * v * n2.y - 4 * t * n4.y + 4 * t * n5.y +
       4 * u * n7.y - 4 * v * n7.y + 4 * w * (n9.y - n8.y)) *
          ((-1 + 4 * t) * n0.z + 4 * (u * n4.z + v * n5.z + w * n6.z)) -
      ((-1 + 4 * t) * n0.y + n1.y - 4 * u * n1.y +
       4 * (-(t * n4.y) + u * n4.y + v * n5.y + w * n6.y - v * n7.y -
            w * n8.y)) *
          (4 * w * n9.z - n2.z + 4 * v * n2.z + 4 * t * n5.z + 4 * u * n7.z) +
      ((-1 + 4 * t) * n0.y - 4 * w * n9.y + n2.y - 4 * v * n2.y + 4 * u * n4.y -
       4 * t * n5.y + 4 * v * n5.y + 4 * w * n6.y - 4 * u * n7.y) *
          ((-1 + 4 * u) * n1.z + 4 * (t * n4.z + v * n7.z + w * n8.z));

  jac[3][2] =
      (-4 * w * n9.x - n1.x + 4 * u * n1.x + n2.x - 4 * v * n2.x +
       4 * t * n4.x - 4 * t * n5.x - 4 * u * n7.x + 4 * v * n7.x +
       4 * w * n8.x) *
          ((-1 + 4 * t) * n0.z + 4 * (u * n4.z + v * n5.z + w * n6.z)) +
      ((-1 + 4 * t) * n0.x + n1.x - 4 * u * n1.x +
       4 * (-(t * n4.x) + u * n4.x + v * n5.x + w * n6.x - v * n7.x -
            w * n8.x)) *
          (4 * w * n9.z - n2.z + 4 * v * n2.z + 4 * t * n5.z + 4 * u * n7.z) -
      ((-1 + 4 * t) * n0.x - 4 * w * n9.x + n2.x - 4 * v * n2.x + 4 * u * n4.x -
       4 * t * n5.x + 4 * v * n5.x + 4 * w * n6.x - 4 * u * n7.x) *
          ((-1 + 4 * u) * n1.z + 4 * (t * n4.z + v * n7.z + w * n8.z));

  jac[3][3] =
      (n1.x - 4 * u * n1.x - n2.x + 4 * v * n2.x - 4 * t * n4.x + 4 * t * n5.x +
       4 * u * n7.x - 4 * v * n7.x + 4 * w * (n9.x - n8.x)) *
          ((-1 + 4 * t) * n0.y + 4 * (u * n4.y + v * n5.y + w * n6.y)) -
      ((-1 + 4 * t) * n0.x + n1.x - 4 * u * n1.x +
       4 * (-(t * n4.x) + u * n4.x + v * n5.x + w * n6.x - v * n7.x -
            w * n8.x)) *
          (4 * w * n9.y - n2.y + 4 * v * n2.y + 4 * t * n5.y + 4 * u * n7.y) +
      ((-1 + 4 * t) * n0.x - 4 * w * n9.x + n2.x - 4 * v * n2.x + 4 * u * n4.x -
       4 * t * n5.x + 4 * v * n5.x + 4 * w * n6.x - 4 * u * n7.x) *
          ((-1 + 4 * u) * n1.y + 4 * (t * n4.y + v * n7.y + w * n8.y));
}

void ComponentFieldMap::JacobianCube(const unsigned int i, const double t1,
                                     const double t2, const double t3,
                                     TMatrixD*& jac,
                                     std::vector<TMatrixD*>& dN) const {
  if (!jac) {
    std::cerr << m_className << "::JacobianCube:\n";
    std::cerr << "    Pointer to Jacobian matrix is empty!\n";
    return;
  }
  dN.clear();

  // Here the partial derivatives of the 8 shaping functions are calculated
  double N1[3] = {-1 * (1 - t2) * (1 - t3), (1 - t1) * -1 * (1 - t3),
                  (1 - t1) * (1 - t2) * -1};
  double N2[3] = {+1 * (1 - t2) * (1 - t3), (1 + t1) * -1 * (1 - t3),
                  (1 + t1) * (1 - t2) * -1};
  double N3[3] = {+1 * (1 + t2) * (1 - t3), (1 + t1) * +1 * (1 - t3),
                  (1 + t1) * (1 + t2) * -1};
  double N4[3] = {-1 * (1 + t2) * (1 - t3), (1 - t1) * +1 * (1 - t3),
                  (1 - t1) * (1 + t2) * -1};
  double N5[3] = {-1 * (1 - t2) * (1 + t3), (1 - t1) * -1 * (1 + t3),
                  (1 - t1) * (1 - t2) * +1};
  double N6[3] = {+1 * (1 - t2) * (1 + t3), (1 + t1) * -1 * (1 + t3),
                  (1 + t1) * (1 - t2) * +1};
  double N7[3] = {+1 * (1 + t2) * (1 + t3), (1 + t1) * +1 * (1 + t3),
                  (1 + t1) * (1 + t2) * +1};
  double N8[3] = {-1 * (1 + t2) * (1 + t3), (1 - t1) * +1 * (1 + t3),
                  (1 - t1) * (1 + t2) * +1};
  // Partial derivatives are stored in dN
  TMatrixD* m_N1 = new TMatrixD(3, 1, N1);
  *m_N1 = (1. / 8. * (*m_N1));
  dN.push_back(m_N1);
  TMatrixD* m_N2 = new TMatrixD(3, 1, N2);
  *m_N2 = (1. / 8. * (*m_N2));
  dN.push_back(m_N2);
  TMatrixD* m_N3 = new TMatrixD(3, 1, N3);
  *m_N3 = (1. / 8. * (*m_N3));
  dN.push_back(m_N3);
  TMatrixD* m_N4 = new TMatrixD(3, 1, N4);
  *m_N4 = (1. / 8. * (*m_N4));
  dN.push_back(m_N4);
  TMatrixD* m_N5 = new TMatrixD(3, 1, N5);
  *m_N5 = (1. / 8. * (*m_N5));
  dN.push_back(m_N5);
  TMatrixD* m_N6 = new TMatrixD(3, 1, N6);
  *m_N6 = (1. / 8. * (*m_N6));
  dN.push_back(m_N6);
  TMatrixD* m_N7 = new TMatrixD(3, 1, N7);
  *m_N7 = (1. / 8. * (*m_N7));
  dN.push_back(m_N7);
  TMatrixD* m_N8 = new TMatrixD(3, 1, N8);
  *m_N8 = (1. / 8. * (*m_N8));
  dN.push_back(m_N8);
  // Calculation of the jacobian using dN
  const Element& element = elements[i];
  for (int j = 0; j < 8; ++j) {
    const Node& node = nodes[element.emap[j]];
    (*jac)(0, 0) += node.x * ((*dN.at(j))(0, 0));
    (*jac)(0, 1) += node.y * ((*dN.at(j))(0, 0));
    (*jac)(0, 2) += node.z * ((*dN.at(j))(0, 0));
    (*jac)(1, 0) += node.x * ((*dN.at(j))(1, 0));
    (*jac)(1, 1) += node.y * ((*dN.at(j))(1, 0));
    (*jac)(1, 2) += node.z * ((*dN.at(j))(1, 0));
    (*jac)(2, 0) += node.x * ((*dN.at(j))(2, 0));
    (*jac)(2, 1) += node.y * ((*dN.at(j))(2, 0));
    (*jac)(2, 2) += node.z * ((*dN.at(j))(2, 0));
  }

  // compute determinant
  if (m_debug) {
    std::cout << m_className << "::JacobianCube:" << std::endl;
    std::cout << "   Det.: " << jac->Determinant() << std::endl;
    std::cout << "   Jacobian matrix.: " << std::endl;
    jac->Print("%11.10g");
    std::cout << "   Hexahedral coordinates (t, u, v) = (" << t1 << "," << t2
              << "," << t3 << ")" << std::endl;
    std::cout << "   Node xyzV" << std::endl;
    for (int j = 0; j < 8; ++j) {
      const Node& node = nodes[element.emap[j]];
      std::cout << "         " << element.emap[j] << "          " << node.x
                << "         " << node.y << "         " << node.z << "         "
                << node.v << std::endl;
    }
  }
}

int ComponentFieldMap::Coordinates3(const double x, const double y,
                                    const double z, double& t1, double& t2,
                                    double& t3, double& t4, double jac[4][4],
                                    double& det,
                                    const unsigned int imap) const {

  if (m_debug) {
    std::cout << m_className << "::Coordinates3:\n";
    std::cout << "   Point (" << x << ", " << y << ", " << z << ")\n";
  }

  // Failure flag
  int ifail = 1;

  // Provisional values
  t1 = t2 = t3 = t4 = 0;

  const Element& element = elements[imap];
  // Make a first order approximation, using the linear triangle.
  const Node& n0 = nodes[element.emap[0]];
  const Node& n1 = nodes[element.emap[1]];
  const Node& n2 = nodes[element.emap[2]];
  const double tt1 = (x - n1.x) * (n2.y - n1.y) - (y - n1.y) * (n2.x - n1.x);
  const double tt2 = (x - n2.x) * (n0.y - n2.y) - (y - n2.y) * (n0.x - n2.x);
  const double tt3 = (x - n0.x) * (n1.y - n0.y) - (y - n0.y) * (n1.x - n0.x);
  const double f1 = (n0.x - n1.x) * (n2.y - n1.y) - (n2.x - n1.x) * (n0.y - n1.y);
  const double f2 = (n1.x - n2.x) * (n0.y - n2.y) - (n0.x - n2.x) * (n1.y - n2.y); 
  const double f3 = (n2.x - n0.x) * (n1.y - n0.y) - (n1.x - n0.x) * (n2.y - n0.y);
  if (f1 == 0 || f2 == 0 || f3 == 0) {
    std::cerr << m_className << "::Coordinates3:\n";
    std::cerr << "    Calculation of linear coordinates failed; abandoned.\n";
    return ifail;
  } else {
    t1 = tt1 / f1;
    t2 = tt2 / f2;
    t3 = tt3 / f3;
  }
  const Node& n3 = nodes[element.emap[3]];
  const Node& n4 = nodes[element.emap[4]];
  const Node& n5 = nodes[element.emap[5]];

  // Start iterative refinement.
  double td1 = t1, td2 = t2, td3 = t3;
  bool converged = false;
  for (int iter = 0; iter < 10; iter++) {
    if (m_debug) {
      std::cout << m_className << "::Coordinates3:\n";
      std::cout << "    Iteration " << iter << ":     (u, v, w) = (" << td1
                << ", " << td2 << ", " << td3 << "), sum = " << td1 + td2 + td3
                << "\n";
    }
    // Re-compute the (x,y,z) position for this coordinate.
    const double xr = n0.x * td1 * (2 * td1 - 1) + n1.x * td2 * (2 * td2 - 1) +
                      n2.x * td3 * (2 * td3 - 1) + n3.x * 4 * td1 * td2 +
                      n4.x * 4 * td1 * td3 + n5.x * 4 * td2 * td3;
    const double yr = n0.y * td1 * (2 * td1 - 1) + n1.y * td2 * (2 * td2 - 1) +
                      n2.y * td3 * (2 * td3 - 1) + n3.y * 4 * td1 * td2 +
                      n4.y * 4 * td1 * td3 + n5.y * 4 * td2 * td3;
    const double sr = td1 + td2 + td3;
    // Compute the Jacobian.
    Jacobian3(imap, td1, td2, td3, det, jac);
    // Compute the difference vector.
    const double diff[3] = {1 - sr, x - xr, y - yr};
    // Update the estimate.
    const double invdet = 1. / det;
    double corr[3] = {0., 0., 0.};
    for (int l = 0; l < 3; l++) {
      for (int k = 0; k < 3; k++) {
        corr[l] += jac[l][k] * diff[k] * invdet;
      }
    }
    // Debugging
    if (m_debug) {
      std::cout << m_className << "::Coordinates3:\n";
      std::cout << "    Difference vector:  (1, x, y)  = (" 
                << diff[0] << ", " << diff[1] << ", " << diff[2] << ").\n";
      std::cout << "    Correction vector:  (u, v, w) = (" 
                << corr[0] << ", " << corr[1] << ", " << corr[2] << ").\n";
    }
    // Update the vector.
    td1 += corr[0];
    td2 += corr[1];
    td3 += corr[2];
    // Check for convergence.
    if (fabs(corr[0]) < 1.0e-5 && fabs(corr[1]) < 1.0e-5 &&
        fabs(corr[2]) < 1.0e-5) {
      if (m_debug) {
        std::cout << m_className << "::Coordinates3:\n";
        std::cout << "    Convergence reached.\n";
      }
      converged = true;
      break;
    }
  }
  // No convergence reached
  if (!converged) {
    double xmin = n0.x;
    double xmax = n0.x;
    if (n1.x < xmin) xmin = n1.x;
    if (n1.x > xmax) xmax = n1.x;
    if (n2.x < xmin) xmin = n2.x;
    if (n2.x > xmax) xmax = n2.x;
    double ymin = n0.y;
    double ymax = n0.y;
    if (n1.y < ymin) ymin = n1.y;
    if (n1.y > ymax) ymax = n1.y;
    if (n2.y < ymin) ymin = n2.y;
    if (n2.y > ymax) ymax = n2.y;

    if (x >= xmin && x <= xmax && y >= ymin && y <= ymax) {
      std::cout << m_className << "::Coordinates3:\n";
      std::cout << "    No convergence achieved "
                << "when refining internal isoparametric coordinates\n";
      std::cout << "    in element " << imap << " at position (" << x << ", "
                << y << ").\n";
      t1 = t2 = t3 = t4 = 0;
      return ifail;
    }
  }

  // Convergence reached.
  t1 = td1;
  t2 = td2;
  t3 = td3;
  t4 = 0;
  if (m_debug) {
    std::cout << m_className << "::Coordinates3:\n";
    std::cout << "    Convergence reached at (t1, t2, t3) = (" << t1 << ", "
              << t2 << ", " << t3 << ").\n";
  }

  // For debugging purposes, show position
  if (m_debug) {
    double xr = n0.x * td1 * (2 * td1 - 1) + n1.x * td2 * (2 * td2 - 1) +
                n2.x * td3 * (2 * td3 - 1) + n3.x * 4 * td1 * td2 +
                n4.x * 4 * td1 * td3 + n5.x * 4 * td2 * td3;
    double yr = n0.y * td1 * (2 * td1 - 1) + n1.y * td2 * (2 * td2 - 1) +
                n2.y * td3 * (2 * td3 - 1) + n3.y * 4 * td1 * td2 +
                n4.y * 4 * td1 * td3 + n5.y * 4 * td2 * td3;
    double sr = td1 + td2 + td3;
    std::cout << m_className << "::Coordinates3:\n";
    std::cout << "    Position requested:     (" << x << ", " << y << ")\n";
    std::cout << "    Reconstructed:          (" << xr << ", " << yr << ")\n";
    std::cout << "    Difference:             (" << x - xr << ", " << y - yr
              << ")\n";
    std::cout << "    Checksum - 1:           " << sr - 1 << "\n";
  }

  // Success
  ifail = 0;
  return ifail;
}

int ComponentFieldMap::Coordinates4(const double x, const double y,
                                    const double z, double& t1, double& t2,
                                    double& t3, double& t4, double jac[4][4],
                                    double& det,
                                    const unsigned int imap) const {

  // Debugging
  if (m_debug) {
    std::cout << m_className << "::Coordinates4:\n";
    std::cout << "   Point (" << x << ", " << y << ", " << z << ")\n";
  }

  // Failure flag
  int ifail = 1;

  // Provisional values
  t1 = t2 = t3 = t4 = 0.;

  const Element& element = elements[imap];
  const Node& n0 = nodes[element.emap[0]];
  const Node& n1 = nodes[element.emap[1]];
  const Node& n2 = nodes[element.emap[2]];
  const Node& n3 = nodes[element.emap[3]];
  // Compute determinant.
  det = -(-((n0.x - n3.x) * (n1.y - n2.y)) + (n1.x - n2.x) * (n0.y - n3.y)) *
            (2 * x * (-n0.y + n1.y + n2.y - n3.y) -
             (n0.x + n3.x) * (n1.y + n2.y - 2 * y) +
             n1.x * (n0.y + n3.y - 2 * y) + n2.x * (n0.y + n3.y - 2 * y)) +
        pow(-(n0.x * n1.y) + n3.x * n2.y - n2.x * n3.y +
                x * (-n0.y + n1.y - n2.y + n3.y) + n1.x * (n0.y - y) +
                (n0.x + n2.x - n3.x) * y,
            2);

  // Check that the determinant is non-negative
  // (this can happen if the point is out of range).
  if (det < 0) {
    if (m_debug) {
      std::cerr << m_className << "::Coordinates4:\n";
      std::cerr << "    No solution found for isoparametric coordinates\n";
      std::cerr << "    in element " << imap << " because the determinant "
                << det << " is < 0.\n";
    }
    return ifail;
  }

  // Vector products for evaluation of T1.
  double prod = ((n2.x - n3.x) * (n0.y - n1.y) - (n0.x - n1.x) * (n2.y - n3.y));
  if (prod * prod >
      1.0e-12 *
          ((n0.x - n1.x) * (n0.x - n1.x) + (n0.y - n1.y) * (n0.y - n1.y)) *
          ((n2.x - n3.x) * (n2.x - n3.x) + (n2.y - n3.y) * (n2.y - n3.y))) {
    t1 = (-(n3.x * n0.y) + x * n0.y + n2.x * n1.y - x * n1.y - n1.x * n2.y +
          x * n2.y + n0.x * n3.y - x * n3.y - n0.x * y + n1.x * y - n2.x * y +
          n3.x * y + sqrt(det)) /
         prod;
  } else {
    double xp = n0.y - n1.y;
    double yp = n1.x - n0.x;
    double dn = sqrt(xp * xp + yp * yp);
    if (dn <= 0) {
      std::cerr << m_className << "::Coordinates4:\n";
      std::cerr << "    Element " << imap
                << " appears to be degenerate in the 1 - 2 axis.\n";
      return ifail;
    }
    xp = xp / dn;
    yp = yp / dn;
    double dpoint = xp * (x - n0.x) + yp * (y - n0.y);
    double dbox = xp * (n3.x - n0.x) + yp * (n3.y - n0.y);
    if (dbox == 0) {
      std::cerr << m_className << "::Coordinates4:\n";
      std::cerr << "    Element " << imap
                << " appears to be degenerate in the 1 - 3 axis.\n";
      return ifail;
    }
    double t = -1 + 2 * dpoint / dbox;
    double xt1 = n0.x + 0.5 * (t + 1) * (n3.x - n0.x);
    double yt1 = n0.y + 0.5 * (t + 1) * (n3.y - n0.y);
    double xt2 = n1.x + 0.5 * (t + 1) * (n2.x - n1.x);
    double yt2 = n1.y + 0.5 * (t + 1) * (n2.y - n1.y);
    dn = (xt1 - xt2) * (xt1 - xt2) + (yt1 - yt2) * (yt1 - yt2);
    if (dn <= 0) {
      std::cout << m_className << "::Coordinates4:\n";
      std::cout << "    Coordinate requested at convergence point of element "
                << imap << ".\n";
      return ifail;
    }
    t1 = -1 + 2 * ((x - xt1) * (xt2 - xt1) + (y - yt1) * (yt2 - yt1)) / dn;
  }

  // Vector products for evaluation of T2.
  prod = ((n0.x - n3.x) * (n1.y - n2.y) - (n1.x - n2.x) * (n0.y - n3.y));
  if (prod * prod >
      1.0e-12 *
          ((n0.x - n3.x) * (n0.x - n3.x) + (n0.y - n3.y) * (n0.y - n3.y)) *
          ((n1.x - n2.x) * (n1.x - n2.x) + (n1.y - n2.y) * (n1.y - n2.y))) {
    t2 = (-(n1.x * n0.y) + x * n0.y + n0.x * n1.y - x * n1.y - n3.x * n2.y +
          x * n2.y + n2.x * n3.y - x * n3.y - n0.x * y + n1.x * y - n2.x * y +
          n3.x * y - sqrt(det)) /
         prod;
  } else {
    double xp = n0.y - n3.y;
    double yp = n3.x - n0.x;
    double dn = sqrt(xp * xp + yp * yp);
    if (dn <= 0) {
      std::cerr << m_className << "Coordinates4:\n";
      std::cerr << "    Element " << imap
                << " appears to be degenerate in the 1 - 4 axis.\n";
      return ifail;
    }
    xp = xp / dn;
    yp = yp / dn;
    double dpoint = xp * (x - n0.x) + yp * (y - n0.y);
    double dbox = xp * (n1.x - n0.x) + yp * (n1.y - n0.y);
    if (dbox == 0) {
      std::cerr << m_className << "::Coordinates4:\n";
      std::cerr << "    Element " << imap
                << " appears to be degenerate in the 1 - 2 axis.\n";
      return ifail;
    }
    double t = -1 + 2 * dpoint / dbox;
    double xt1 = n0.x + 0.5 * (t + 1) * (n1.x - n0.x);
    double yt1 = n0.y + 0.5 * (t + 1) * (n1.y - n0.y);
    double xt2 = n3.x + 0.5 * (t + 1) * (n2.x - n3.x);
    double yt2 = n3.y + 0.5 * (t + 1) * (n2.y - n3.y);
    dn = (xt1 - xt2) * (xt1 - xt2) + (yt1 - yt2) * (yt1 - yt2);
    if (dn <= 0) {
      std::cout << m_className << "::Coordinates4:\n";
      std::cout << "    Coordinate requested at convergence point of element "
                << imap << ".\n";
      return ifail;
    }
    t2 = -1 + 2 * ((x - xt1) * (xt2 - xt1) + (y - yt1) * (yt2 - yt1)) / dn;
  }
  if (m_debug) {
    std::cout << m_className << "::Coordinates4:\n";
    std::cout << "    Isoparametric (u, v):   (" << t1 << ", " << t2 << ").\n";
  }

  // Re-compute the (x,y,z) position for this coordinate.
  if (m_debug) {
    double xr = n0.x * (1 - t1) * (1 - t2) * 0.25 +
                n1.x * (1 + t1) * (1 - t2) * 0.25 +
                n2.x * (1 + t1) * (1 + t2) * 0.25 + 
                n3.x * (1 - t1) * (1 + t2) * 0.25;
    double yr = n0.y * (1 - t1) * (1 - t2) * 0.25 +
                n1.y * (1 + t1) * (1 - t2) * 0.25 +
                n2.y * (1 + t1) * (1 + t2) * 0.25 + 
                n3.y * (1 - t1) * (1 + t2) * 0.25;
    std::cout << m_className << "::Coordinates4: \n";
    std::cout << "    Position requested:     (" << x << ", " << y << ")\n";
    std::cout << "    Reconstructed:          (" << xr << ", " << yr << ")\n";
    std::cout << "    Difference:             (" << x - xr << ", " << y - yr
              << ")\n";
  }

  // This should have worked if we get this far.
  ifail = 0;
  return ifail;
  // Variable jac is not used.
  // The following lines are just for quieting the compiler.
  jac[0][0] = jac[0][1] = jac[0][2] = jac[0][3] = 0.;
  jac[1][0] = jac[1][1] = jac[1][2] = jac[1][3] = 0.;
  jac[2][0] = jac[2][1] = jac[2][2] = jac[2][3] = 0.;
  jac[3][0] = jac[3][1] = jac[3][2] = jac[3][3] = 0.;
}

int ComponentFieldMap::Coordinates5(const double x, const double y,
                                    const double z, double& t1, double& t2,
                                    double& t3, double& t4, double jac[4][4],
                                    double& det,
                                    const unsigned int imap) const {

  // Debugging
  if (m_debug) {
    std::cout << m_className << "::Coordinates5:\n";
    std::cout << "   Point (" << x << ", " << y << ", " << z << ")\n";
  }

  // Failure flag
  int ifail = 1;

  // Provisional values
  t1 = t2 = t3 = t4 = 0;

  const Element& element = elements[imap];
  // Degenerate elements should have been treated as triangles.
  if (element.degenerate) {
    std::cerr << m_className << "::Coordinates5:\n";
    std::cerr << "    Received degenerate element " << imap << ".\n";
    return ifail;
  }

  // Set tolerance parameter.
  double f = 0.5;

  // Make a first order approximation.
  if (Coordinates4(x, y, z, t1, t2, t3, t4, jac, det, imap) > 0) {
    if (m_debug) {
      std::cout << m_className << "::Coordinates5:\n";
      std::cout << "    Failure to obtain linear estimate of isoparametric "
                   "coordinates\n";
      std::cout << "    in element " << imap << ".\n";
    }
    return ifail;
  }

  // Check whether the point is far outside.
  if (t1 < -(1 + f) || t1 > (1 + f) || t2 < -(1 + f) || t2 > (1 + f)) {
    if (m_debug) {
      std::cout << m_className << "::Coordinates5:\n";
      std::cout << "    Point far outside, (t1,t2) = (" << t1 << ", " << t2
                << ").\n";
    }
    return ifail;
  }

  // Start iteration
  double td1 = t1, td2 = t2;
  const Node& n0 = nodes[element.emap[0]];
  const Node& n1 = nodes[element.emap[1]];
  const Node& n2 = nodes[element.emap[2]];
  const Node& n3 = nodes[element.emap[3]];
  const Node& n4 = nodes[element.emap[4]];
  const Node& n5 = nodes[element.emap[5]];
  const Node& n6 = nodes[element.emap[6]];
  const Node& n7 = nodes[element.emap[7]];
  bool converged = false;
  for (int iter = 0; iter < 10; iter++) {
    if (m_debug) {
      std::cout << m_className << "::Coordinates5:\n";
      std::cout << "    Iteration " << iter << ":     (t1, t2) = (" << td1
                << ", " << td2 << ").\n";
    }
    // Re-compute the (x,y,z) position for this coordinate.
    double xr = n0.x * (-(1 - td1) * (1 - td2) * (1 + td1 + td2)) * 0.25 +
                n1.x * (-(1 + td1) * (1 - td2) * (1 - td1 + td2)) * 0.25 +
                n2.x * (-(1 + td1) * (1 + td2) * (1 - td1 - td2)) * 0.25 +
                n3.x * (-(1 - td1) * (1 + td2) * (1 + td1 - td2)) * 0.25 +
                n4.x * (1 - td1) * (1 + td1) * (1 - td2) * 0.5 +
                n5.x * (1 + td1) * (1 + td2) * (1 - td2) * 0.5 +
                n6.x * (1 - td1) * (1 + td1) * (1 + td2) * 0.5 +
                n7.x * (1 - td1) * (1 + td2) * (1 - td2) * 0.5;
    double yr = n0.y * (-(1 - td1) * (1 - td2) * (1 + td1 + td2)) * 0.25 +
                n1.y * (-(1 + td1) * (1 - td2) * (1 - td1 + td2)) * 0.25 +
                n2.y * (-(1 + td1) * (1 + td2) * (1 - td1 - td2)) * 0.25 +
                n3.y * (-(1 - td1) * (1 + td2) * (1 + td1 - td2)) * 0.25 +
                n4.y * (1 - td1) * (1 + td1) * (1 - td2) * 0.5 +
                n5.y * (1 + td1) * (1 + td2) * (1 - td2) * 0.5 +
                n6.y * (1 - td1) * (1 + td1) * (1 + td2) * 0.5 +
                n7.y * (1 - td1) * (1 + td2) * (1 - td2) * 0.5;
    // Compute the Jacobian.
    Jacobian5(imap, td1, td2, det, jac);
    // Compute the difference vector.
    double diff[2] = {x - xr, y - yr};
    // Update the estimate.
    double corr[2] = {0., 0.};
    for (int l = 0; l < 2; ++l) {
      for (int k = 0; k < 2; ++k) {
        corr[l] += jac[l][k] * diff[k] / det;
      }
    }
    // Debugging
    if (m_debug) {
      std::cout << m_className << "::Coordinates5:\n";
      std::cout << "    Difference vector: (x, y)   = (" << diff[0] << ", "
                << diff[1] << ").\n";
      std::cout << "    Correction vector: (t1, t2) = (" << corr[0] << ", "
                << corr[1] << ").\n";
    }
    // Update the vector.
    td1 += corr[0];
    td2 += corr[1];
    // Check for convergence.
    if (fabs(corr[0]) < 1.0e-5 && fabs(corr[1]) < 1.0e-5) {
      if (m_debug) {
        std::cout << m_className << "::Coordinates5:\n";
        std::cout << "    Convergence reached.\n";
      }
      converged = true;
      break;
    }
  }
  // No convergence reached.
  if (!converged) {
    double xmin = n0.x;
    double xmax = n0.x;
    if (n1.x < xmin) xmin = n1.x;
    if (n1.x > xmax) xmax = n1.x;
    if (n2.x < xmin) xmin = n2.x;
    if (n2.x > xmax) xmax = n2.x;
    if (n3.x < xmin) xmin = n3.x;
    if (n3.x > xmax) xmax = n3.x;
    if (n4.x < xmin) xmin = n4.x;
    if (n4.x > xmax) xmax = n4.x;
    if (n5.x < xmin) xmin = n5.x;
    if (n5.x > xmax) xmax = n5.x;
    if (n6.x < xmin) xmin = n6.x;
    if (n6.x > xmax) xmax = n6.x;
    if (n7.x < xmin) xmin = n7.x;
    if (n7.x > xmax) xmax = n7.x;
    double ymin = n0.y;
    double ymax = n0.y;
    if (n1.y < ymin) ymin = n1.y;
    if (n1.y > ymax) ymax = n1.y;
    if (n2.y < ymin) ymin = n2.y;
    if (n2.y > ymax) ymax = n2.y;
    if (n3.y < ymin) ymin = n3.y;
    if (n3.y > ymax) ymax = n3.y;
    if (n4.y < ymin) ymin = n4.y;
    if (n4.y > ymax) ymax = n4.y;
    if (n5.y < ymin) ymin = n5.y;
    if (n5.y > ymax) ymax = n5.y;
    if (n6.y < ymin) ymin = n6.y;
    if (n6.y > ymax) ymax = n6.y;
    if (n7.y < ymin) ymin = n7.y;
    if (n7.y > ymax) ymax = n7.y;

    if (x >= xmin && x <= xmax && y >= ymin && y <= ymax) {
      std::cout << m_className << "::Coordinates5:\n";
      std::cout << "    No convergence achieved "
                << "when refining internal isoparametric coordinates\n";
      std::cout << "    in element " << imap << " at position (" << x << ", "
                << y << ").\n";
      t1 = t2 = 0;
      return ifail;
    }
  }

  // Convergence reached.
  t1 = td1;
  t2 = td2;
  t3 = 0;
  t4 = 0;
  if (m_debug) {
    std::cout << m_className << "::Coordinates5:\n";
    std::cout << "    Convergence reached at (t1, t2) = (" << t1 << ", " << t2
              << ").\n";
  }

  // For debugging purposes, show position.
  if (m_debug) {
    double xr = n0.x * (-(1 - t1) * (1 - t2) * (1 + t1 + t2)) * 0.25 +
                n1.x * (-(1 + t1) * (1 - t2) * (1 - t1 + t2)) * 0.25 +
                n2.x * (-(1 + t1) * (1 + t2) * (1 - t1 - t2)) * 0.25 +
                n3.x * (-(1 - t1) * (1 + t2) * (1 + t1 - t2)) * 0.25 +
                n4.x * (1 - t1) * (1 + t1) * (1 - t2) * 0.5 +
                n5.x * (1 + t1) * (1 + t2) * (1 - t2) * 0.5 +
                n6.x * (1 - t1) * (1 + t1) * (1 + t2) * 0.5 +
                n7.x * (1 - t1) * (1 + t2) * (1 - t2) * 0.5;
    double yr = n0.y * (-(1 - t1) * (1 - t2) * (1 + t1 + t2)) * 0.25 +
                n1.y * (-(1 + t1) * (1 - t2) * (1 - t1 + t2)) * 0.25 +
                n2.y * (-(1 + t1) * (1 + t2) * (1 - t1 - t2)) * 0.25 +
                n3.y * (-(1 - t1) * (1 + t2) * (1 + t1 - t2)) * 0.25 +
                n4.y * (1 - t1) * (1 + t1) * (1 - t2) * 0.5 +
                n5.y * (1 + t1) * (1 + t2) * (1 - t2) * 0.5 +
                n6.y * (1 - t1) * (1 + t1) * (1 + t2) * 0.5 +
                n7.y * (1 - t1) * (1 + t2) * (1 - t2) * 0.5;
    std::cout << m_className << "::Coordinates5:\n";
    std::cout << "    Position requested:     (" << x << ", " << y << ")\n";
    std::cout << "    Reconstructed:          (" << xr << ", " << yr << ")\n";
    std::cout << "    Difference:             (" << x - xr << ", " << y - yr
              << ")\n";
  }

  // Success
  ifail = 0;
  return ifail;
}

int ComponentFieldMap::Coordinates12(const double x, const double y,
                                     const double z, double& t1, double& t2,
                                     double& t3, double& t4,
                                     const unsigned int imap) const {

  if (m_debug) {
    std::cout << m_className << "::Coordinates12:\n";
    std::cout << "   Point (" << x << ", " << y << ", " << z << ") for element "
              << imap << "\n";
  }

  // Failure flag
  int ifail = 1;
  const Element& element = elements[imap];
  const Node& n0 = nodes[element.emap[0]];
  const Node& n1 = nodes[element.emap[1]];
  const Node& n2 = nodes[element.emap[2]];
  const Node& n3 = nodes[element.emap[3]];
  // Compute tetrahedral coordinates.
  const double f1x = (n2.y - n1.y) * (n3.z - n1.z) - (n3.y - n1.y) * (n2.z - n1.z);
  const double f1y = (n2.z - n1.z) * (n3.x - n1.x) - (n3.z - n1.z) * (n2.x - n1.x);
  const double f1z = (n2.x - n1.x) * (n3.y - n1.y) - (n3.x - n1.x) * (n2.y - n1.y);
  t1 = (x - n1.x) * f1x + (y - n1.y) * f1y + (z - n1.z) * f1z;
  t1 = t1 / ((n0.x - n1.x) * f1x + (n0.y - n1.y) * f1y + (n0.z - n1.z) * f1z);
  const double f2x = (n0.y - n2.y) * (n3.z - n2.z) - (n3.y - n2.y) * (n0.z - n2.z);
  const double f2y = (n0.z - n2.z) * (n3.x - n2.x) - (n3.z - n2.z) * (n0.x - n2.x);
  const double f2z = (n0.x - n2.x) * (n3.y - n2.y) - (n3.x - n2.x) * (n0.y - n2.y); 
  t2 = (x - n2.x) * f2x + (y - n2.y) * f2y + (z - n2.z) * f2z;
  t2 = t2 / ((n1.x - n2.x) * f2x + (n1.y - n2.y) * f2y + (n1.z - n2.z) * f2z);
  const double f3x = (n0.y - n3.y) * (n1.z - n3.z) - (n1.y - n3.y) * (n0.z - n3.z);
  const double f3y = (n0.z - n3.z) * (n1.x - n3.x) - (n1.z - n3.z) * (n0.x - n3.x); 
  const double f3z = (n0.x - n3.x) * (n1.y - n3.y) - (n1.x - n3.x) * (n0.y - n3.y); 
  t3 = (x - n3.x) * f3x + (y - n3.y) * f3y + (z - n3.z) * f3z;
  t3 = t3 / ((n2.x - n3.x) * f3x + (n2.y - n3.y) * f3y + (n2.z - n3.z) * f3z);
  const double f4x = (n2.y - n0.y) * (n1.z - n0.z) - (n1.y - n0.y) * (n2.z - n0.z);
  const double f4y = (n2.z - n0.z) * (n1.x - n0.x) - (n1.z - n0.z) * (n2.x - n0.x);
  const double f4z = (n2.x - n0.x) * (n1.y - n0.y) - (n1.x - n0.x) * (n2.y - n0.y); 
  t4 = (x - n0.x) * f4x + (y - n0.y) * f4y + (z - n0.z) * f4z;
  t4 = t4 / ((n3.x - n0.x) * f4x + (n3.y - n0.y) * f4y + (n3.z - n0.z) * f4z);

  // Result
  if (m_debug) {
    std::cout << m_className << "::Coordinates12:\n";
    std::cout << "    Tetrahedral coordinates (t, u, v, w) = (" << t1 << ", "
              << t2 << ", " << t3 << ", " << t4
              << ") sum = " << t1 + t2 + t3 + t4 << ".\n";
  }
  // Re-compute the (x,y,z) position for this coordinate.
  if (m_debug) {
    const double xr = n0.x * t1 + n1.x * t2 + n2.x * t3 + n3.x * t4;
    const double yr = n0.y * t1 + n1.y * t2 + n2.y * t3 + n3.y * t4;
    const double zr = n0.z * t1 + n1.z * t2 + n2.z * t3 + n3.z * t4;
    const double sr = t1 + t2 + t3 + t4;
    std::cout << m_className << "::Coordinates12:\n";
    std::cout << "    Position requested:     (" << x << ", " << y << ", " << z
              << ")\n";
    std::cout << "    Reconstructed:          (" << xr << ", " << yr << ", "
              << zr << ")\n";
    std::cout << "    Difference:             (" << x - xr << ", " << y - yr
              << ", " << z - zr << ")\n";
    std::cout << "    Checksum - 1:           " << sr - 1 << "\n";
  }

  // This should always work.
  ifail = 0;
  return ifail;
}

int ComponentFieldMap::Coordinates13(const double x, const double y,
                                     const double z, double& t1, double& t2,
                                     double& t3, double& t4, double jac[4][4],
                                     double& det,
                                     const unsigned int imap) const {

  if (m_debug) {
    std::cout << m_className << "::Coordinates13:\n";
    std::cout << "   Point (" << x << ", " << y << ", " << z << ")\n";
  }

  // Failure flag
  int ifail = 1;

  // Provisional values
  t1 = t2 = t3 = t4 = 0.;

  // Make a first order approximation.
  if (Coordinates12(x, y, z, t1, t2, t3, t4, imap) > 0) {
    if (m_debug) {
      std::cout << m_className << "::Coordinates13:\n";
      std::cout << "    Failure to obtain linear estimate of isoparametric "
                   "coordinates\n";
      std::cout << "    in element " << imap << ".\n";
    }
    return ifail;
  }

  // Set tolerance parameter.
  const double f = 0.5;
  if (t1 < -f || t2 < -f || t3 < -f || t4 < -f || t1 > 1 + f || t2 > 1 + f ||
      t3 > 1 + f || t4 > 1 + f) {
    if (m_debug) {
      std::cout << m_className << "::Coordinates13:\n";
      std::cout << "    Linear isoparametric coordinates more than\n";
      std::cout << "    f (" << f << ") out of range in element " << imap
                << ".\n";
    }
    ifail = 0;
    return ifail;
  }

  // Start iteration.
  double td1 = t1, td2 = t2, td3 = t3, td4 = t4;
  if (m_debug) {
    std::cout << m_className << "::Coordinates13:\n";
    std::cout << "    Iteration starts at (t1,t2,t3,t4) = (" << td1 << ", "
              << td2 << ", " << td3 << ", " << td4 << ").\n";
  }
  const Element& element = elements[imap];
  const Node& n0 = nodes[element.emap[0]];
  const Node& n1 = nodes[element.emap[1]];
  const Node& n2 = nodes[element.emap[2]];
  const Node& n3 = nodes[element.emap[3]];
  const Node& n4 = nodes[element.emap[4]];
  const Node& n5 = nodes[element.emap[5]];
  const Node& n6 = nodes[element.emap[6]];
  const Node& n7 = nodes[element.emap[7]];
  const Node& n8 = nodes[element.emap[8]];
  const Node& n9 = nodes[element.emap[9]];

  // Loop
  bool converged = false;
  double diff[4], corr[4];
  for (int iter = 0; iter < 10; iter++) {
    if (m_debug) {
      std::cout << m_className << "::Coordinates13:\n";
      std::cout << "    Iteration " << iter << ":      (t1,t2,t3,t4) = (" << td1
                << ", " << td2 << ", " << td3 << ", " << td4 << ").\n";
    }
    // Re-compute the (x,y,z) position for this coordinate.
    const double xr = n0.x * td1 * (2 * td1 - 1) + n1.x * td2 * (2 * td2 - 1) +
                      n2.x * td3 * (2 * td3 - 1) + n3.x * td4 * (2 * td4 - 1) +
                      n4.x * 4 * td1 * td2 + n5.x * 4 * td1 * td3 +
                      n6.x * 4 * td1 * td4 + n7.x * 4 * td2 * td3 +
                      n8.x * 4 * td2 * td4 + n9.x * 4 * td3 * td4;
    const double yr = n0.y * td1 * (2 * td1 - 1) + n1.y * td2 * (2 * td2 - 1) +
                      n2.y * td3 * (2 * td3 - 1) + n3.y * td4 * (2 * td4 - 1) +
                      n4.y * 4 * td1 * td2 + n5.y * 4 * td1 * td3 +
                      n6.y * 4 * td1 * td4 + n7.y * 4 * td2 * td3 +
                      n8.y * 4 * td2 * td4 + n9.y * 4 * td3 * td4;
    const double zr = n0.z * td1 * (2 * td1 - 1) + n1.z * td2 * (2 * td2 - 1) +
                      n2.z * td3 * (2 * td3 - 1) + n3.z * td4 * (2 * td4 - 1) +
                      n4.z * 4 * td1 * td2 + n5.z * 4 * td1 * td3 +
                      n6.z * 4 * td1 * td4 + n7.z * 4 * td2 * td3 +
                      n8.z * 4 * td2 * td4 + n9.z * 4 * td3 * td4;
    const double sr = td1 + td2 + td3 + td4;

    // Compute the Jacobian.
    Jacobian13(imap, td1, td2, td3, td4, det, jac);
    // Compute the difference vector.
    diff[0] = 1 - sr;
    diff[1] = x - xr;
    diff[2] = y - yr;
    diff[3] = z - zr;

    // Update the estimate.
    const double invdet = 1. / det;
    for (int l = 0; l < 4; ++l) {
      corr[l] = 0;
      for (int k = 0; k < 4; ++k) {
        corr[l] += jac[l][k] * diff[k] * invdet;
      }
    }

    // Debugging
    if (m_debug) {
      std::cout << m_className << "::Coordinates13:\n";
      std::cout << "    Difference vector:  (1, x, y, z)  = (" << diff[0]
                << ", " << diff[1] << ", " << diff[2] << ", " << diff[3]
                << ").\n";
      std::cout << "    Correction vector:  (t1,t2,t3,t4) = (" << corr[0]
                << ", " << corr[1] << ", " << corr[2] << ", " << corr[3]
                << ").\n";
    }

    // Update the vector.
    td1 += corr[0];
    td2 += corr[1];
    td3 += corr[2];
    td4 += corr[3];

    // Check for convergence.
    if (fabs(corr[0]) < 1.0e-5 && fabs(corr[1]) < 1.0e-5 &&
        fabs(corr[2]) < 1.0e-5 && fabs(corr[3]) < 1.0e-5) {
      if (m_debug) {
        std::cout << m_className << "::Coordinates13:\n";
        std::cout << "    Convergence reached.\n";
      }
      converged = true;
      break;
    }
  }

  // No convergence reached.
  if (!converged) {
    double xmin = n0.x;
    double xmax = n0.x;
    if (n1.x < xmin) xmin = n1.x;
    if (n1.x > xmax) xmax = n1.x;
    if (n2.x < xmin) xmin = n2.x;
    if (n2.x > xmax) xmax = n2.x;
    if (n3.x < xmin) xmin = n3.x;
    if (n3.x > xmax) xmax = n3.x;
    double ymin = n0.y;
    double ymax = n0.y;
    if (n1.y < ymin) ymin = n1.y;
    if (n1.y > ymax) ymax = n1.y;
    if (n2.y < ymin) ymin = n2.y;
    if (n2.y > ymax) ymax = n2.y;
    if (n3.y < ymin) ymin = n3.y;
    if (n3.y > ymax) ymax = n3.y;
    double zmin = n0.z;
    double zmax = n0.z;
    if (n1.z < zmin) zmin = n1.z;
    if (n1.z > zmax) zmax = n1.z;
    if (n2.z < zmin) zmin = n2.z;
    if (n2.z > zmax) zmax = n2.z;
    if (n3.z < zmin) zmin = n3.z;
    if (n3.z > zmax) zmax = n3.z;

    if (x >= xmin && x <= xmax && y >= ymin && y <= ymax && z >= zmin &&
        z <= zmax) {
      std::cout << m_className << "::Coordinates13:\n";
      std::cout << "    No convergence achieved "
                << "when refining internal isoparametric coordinates\n";
      std::cout << "    in element " << imap << " at position (" << x << ", "
                << y << ", " << z << ").\n";
      t1 = t2 = t3 = t4 = -1;
      return ifail;
    }
  }

  // Convergence reached.
  t1 = td1;
  t2 = td2;
  t3 = td3;
  t4 = td4;
  if (m_debug) {
    std::cout << m_className << "::Coordinates13:\n";
    std::cout << "    Convergence reached at (t1, t2, t3, t4) = (" << t1 << ", "
              << t2 << ", " << t3 << ", " << t4 << ").\n";
  }

  // For debugging purposes, show position.
  if (m_debug) {
    // Re-compute the (x,y,z) position for this coordinate.
    double xr = n0.x * td1 * (2 * td1 - 1) + n1.x * td2 * (2 * td2 - 1) +
                n2.x * td3 * (2 * td3 - 1) + n3.x * td4 * (2 * td4 - 1) +
                n4.x * 4 * td1 * td2 + n5.x * 4 * td1 * td3 +
                n6.x * 4 * td1 * td4 + n7.x * 4 * td2 * td3 +
                n8.x * 4 * td2 * td4 + n9.x * 4 * td3 * td4;
    double yr = n0.y * td1 * (2 * td1 - 1) + n1.y * td2 * (2 * td2 - 1) +
                n2.y * td3 * (2 * td3 - 1) + n3.y * td4 * (2 * td4 - 1) +
                n4.y * 4 * td1 * td2 + n5.y * 4 * td1 * td3 +
                n6.y * 4 * td1 * td4 + n7.y * 4 * td2 * td3 +
                n8.y * 4 * td2 * td4 + n9.y * 4 * td3 * td4;
    double zr = n0.z * td1 * (2 * td1 - 1) + n1.z * td2 * (2 * td2 - 1) +
                n2.z * td3 * (2 * td3 - 1) + n3.z * td4 * (2 * td4 - 1) +
                n4.z * 4 * td1 * td2 + n5.z * 4 * td1 * td3 +
                n6.z * 4 * td1 * td4 + n7.z * 4 * td2 * td3 +
                n8.z * 4 * td2 * td4 + n9.z * 4 * td3 * td4;
    double sr = td1 + td2 + td3 + td4;
    std::cout << m_className << "::Coordinates13:\n";
    std::cout << "    Position requested:     (" << x << ", " << y << ", " << z
              << ")\n";
    std::cout << "    Reconstructed:          (" << xr << ", " << yr << ", "
              << zr << ")\n";
    std::cout << "    Difference:             (" << x - xr << ", " << y - yr
              << ", " << z - zr << ")\n";
    std::cout << "    Checksum - 1:           " << sr - 1 << "\n";
  }

  // Success
  ifail = 0;
  return ifail;
}

int ComponentFieldMap::CoordinatesCube(const double x, const double y,
                                       const double z, double& t1, double& t2,
                                       double& t3, TMatrixD*& jac,
                                       std::vector<TMatrixD*>& dN,
                                       const unsigned int imap) const {

  /*
  global coordinates   7__ _ _ 6     t3    t2
                      /       /|     ^   /|
    ^ z              /       / |     |   /
    |               4_______5  |     |  /
    |              |        |  |     | /
    |              |  3     |  2     |/     t1
     ------->      |        | /       ------->
    /      y       |        |/       local coordinates
   /               0--------1
  /
 v x
 */

  // Failure flag
  int ifail = 1;

  const Element& element = elements[imap];
  const Node& n0 = nodes[element.emap[0]];
  const Node& n2 = nodes[element.emap[2]];
  const Node& n3 = nodes[element.emap[3]];
  const Node& n7 = nodes[element.emap[7]];

  // Compute hexahedral coordinates (t1->[-1,1],t2->[-1,1],t3->[-1,1]) and
  // t1 (zeta) is in y-direction
  // t2 (eta)  is in opposite x-direction
  // t3 (mu)   is in z-direction
  // Nodes are set in that way, that node [0] has always lowest x,y,z!
  t2 = (2. * (x - n3.x) / (n0.x - n3.x) - 1) * -1.;
  t1 = 2. * (y - n3.y) / (n2.y - n3.y) - 1;
  t3 = 2. * (z - n3.z) / (n7.z - n3.z) - 1;
  // Re-compute the (x,y,z) position for this coordinate.
  if (m_debug) {
    double n[8];
    n[0] = 1. / 8 * (1 - t1) * (1 - t2) * (1 - t3);
    n[1] = 1. / 8 * (1 + t1) * (1 - t2) * (1 - t3);
    n[2] = 1. / 8 * (1 + t1) * (1 + t2) * (1 - t3);
    n[3] = 1. / 8 * (1 - t1) * (1 + t2) * (1 - t3);
    n[4] = 1. / 8 * (1 - t1) * (1 - t2) * (1 + t3);
    n[5] = 1. / 8 * (1 + t1) * (1 - t2) * (1 + t3);
    n[6] = 1. / 8 * (1 + t1) * (1 + t2) * (1 + t3);
    n[7] = 1. / 8 * (1 - t1) * (1 + t2) * (1 + t3);

    double xr = 0;
    double yr = 0;
    double zr = 0;

    for (int i = 0; i < 8; i++) {
      const Node& node = nodes[element.emap[i]];
      xr += node.x * n[i];
      yr += node.y * n[i];
      zr += node.z * n[i];
    }
    double sr = n[0] + n[1] + n[2] + n[3] + n[4] + n[5] + n[6] + n[7];
    std::cout << m_className << "::CoordinatesCube:\n";
    std::cout << "    Position requested:     (" << x << "," << y << "," << z
              << ")\n";
    std::cout << "    Position reconstructed: (" << xr << "," << yr << "," << zr
              << ")\n";
    std::cout << "    Difference:             (" << (x - xr) << "," << (y - yr)
              << "," << (z - zr) << ")\n";
    std::cout << "    Hexahedral coordinates (t, u, v) = (" << t1 << "," << t2
              << "," << t3 << ")\n";
    std::cout << "    Checksum - 1:           " << (sr - 1) << "\n";
  }
  if (jac != 0) JacobianCube(imap, t1, t2, t3, jac, dN);
  // This should always work.
  ifail = 0;
  return ifail;
}

void ComponentFieldMap::UpdatePeriodicityCommon() {

  // Check the required data is available.
  if (!m_ready) {
    std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
    std::cerr << "    No valid field map available.\n";
    return;
  }

  // No regular and mirror periodicity at the same time.
  if (m_xPeriodic && m_xMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along x requested; reset.\n";
    m_xPeriodic = false;
    m_xMirrorPeriodic = false;
    m_warning = true;
  }
  if (m_yPeriodic && m_yMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along y requested; reset.\n";
    m_yPeriodic = false;
    m_yMirrorPeriodic = false;
    m_warning = true;
  }
  if (m_zPeriodic && m_zMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along z requested; reset.\n";
    m_zPeriodic = false;
    m_zMirrorPeriodic = false;
    m_warning = true;
  }

  // In case of axial periodicity,
  // the range must be an integral part of two pi.
  if (m_xAxiallyPeriodic) {
    if (mapxamin >= mapxamax) {
      mapnxa = 0;
    } else {
      mapnxa = TwoPi / (mapxamax - mapxamin);
    }
    if (fabs(mapnxa - int(0.5 + mapnxa)) > 0.001 || mapnxa < 1.5) {
      std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
      std::cerr << "    X-axial symmetry has been requested but the map\n";
      std::cerr << "    does not cover an integral fraction of 2 pi; reset.\n";
      m_xAxiallyPeriodic = false;
      m_warning = true;
    }
  }

  if (m_yAxiallyPeriodic) {
    if (mapyamin >= mapyamax) {
      mapnya = 0;
    } else {
      mapnya = TwoPi / (mapyamax - mapyamin);
    }
    if (fabs(mapnya - int(0.5 + mapnya)) > 0.001 || mapnya < 1.5) {
      std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
      std::cerr << "    Y-axial symmetry has been requested but the map\n";
      std::cerr << "    does not cover an integral fraction of 2 pi; reset.\n";
      m_yAxiallyPeriodic = false;
      m_warning = true;
    }
  }

  if (m_zAxiallyPeriodic) {
    if (mapzamin >= mapzamax) {
      mapnza = 0;
    } else {
      mapnza = TwoPi / (mapzamax - mapzamin);
    }
    if (fabs(mapnza - int(0.5 + mapnza)) > 0.001 || mapnza < 1.5) {
      std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
      std::cerr << "    Z-axial symmetry has been requested but the map\n";
      std::cerr << "    does not cover an integral fraction of 2 pi; reset.\n";
      m_zAxiallyPeriodic = false;
      m_warning = true;
    }
  }

  // Not more than 1 rotational symmetry
  if ((m_xRotationSymmetry && m_yRotationSymmetry) ||
      (m_xRotationSymmetry && m_zRotationSymmetry) ||
      (m_yRotationSymmetry && m_zRotationSymmetry)) {
    std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
    std::cerr << "    Only 1 rotational symmetry allowed; reset.\n";
    m_xRotationSymmetry = false;
    m_yRotationSymmetry = false;
    m_zRotationSymmetry = false;
    m_warning = true;
  }

  // No rotational symmetry as well as axial periodicity
  if ((m_xRotationSymmetry || m_yRotationSymmetry || m_zRotationSymmetry) &&
      (m_xAxiallyPeriodic || m_yAxiallyPeriodic || m_zAxiallyPeriodic)) {
    std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
    std::cerr << "    Not allowed to combine rotational symmetry\n";
    std::cerr << "    and axial periodicity; reset.\n";
    m_xAxiallyPeriodic = false;
    m_yAxiallyPeriodic = false;
    m_zAxiallyPeriodic = false;
    m_xRotationSymmetry = false;
    m_yRotationSymmetry = false;
    m_zRotationSymmetry = false;
    m_warning = true;
  }

  // In case of rotational symmetry, the x-range should not straddle 0.
  if (m_xRotationSymmetry || m_yRotationSymmetry || m_zRotationSymmetry) {
    if (mapxmin * mapxmax < 0) {
      std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
      std::cerr << "    Rotational symmetry requested, \n";
      std::cerr << "    but x-range straddles 0; reset.\n";
      m_xRotationSymmetry = false;
      m_yRotationSymmetry = false;
      m_zRotationSymmetry = false;
      m_warning = true;
    }
  }

  // Recompute the cell ranges.
  xMinBoundingBox = mapxmin;
  xMaxBoundingBox = mapxmax;
  yMinBoundingBox = mapymin;
  yMaxBoundingBox = mapymax;
  zMinBoundingBox = mapzmin;
  zMaxBoundingBox = mapzmax;
  cellsx = fabs(mapxmax - mapxmin);
  cellsy = fabs(mapymax - mapymin);
  cellsz = fabs(mapzmax - mapzmin);
  if (m_xRotationSymmetry) {
    xMinBoundingBox = mapymin;
    xMaxBoundingBox = mapymax;
    yMinBoundingBox = -std::max(fabs(mapxmin), fabs(mapxmax));
    yMaxBoundingBox = +std::max(fabs(mapxmin), fabs(mapxmax));
    zMinBoundingBox = -std::max(fabs(mapxmin), fabs(mapxmax));
    zMaxBoundingBox = +std::max(fabs(mapxmin), fabs(mapxmax));
  } else if (m_yRotationSymmetry) {
    xMinBoundingBox = -std::max(fabs(mapxmin), fabs(mapxmax));
    xMaxBoundingBox = +std::max(fabs(mapxmin), fabs(mapxmax));
    yMinBoundingBox = mapymin;
    yMaxBoundingBox = mapymax;
    zMinBoundingBox = -std::max(fabs(mapxmin), fabs(mapxmax));
    zMaxBoundingBox = +std::max(fabs(mapxmin), fabs(mapxmax));
  } else if (m_zRotationSymmetry) {
    xMinBoundingBox = -std::max(fabs(mapxmin), fabs(mapxmax));
    xMaxBoundingBox = +std::max(fabs(mapxmin), fabs(mapxmax));
    yMinBoundingBox = -std::max(fabs(mapxmin), fabs(mapxmax));
    yMaxBoundingBox = +std::max(fabs(mapxmin), fabs(mapxmax));
    zMinBoundingBox = mapymin;
    zMaxBoundingBox = mapymax;
  }

  if (m_xAxiallyPeriodic) {
    yMinBoundingBox = -std::max(std::max(fabs(mapymin), fabs(mapymax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
    yMaxBoundingBox = +std::max(std::max(fabs(mapymin), fabs(mapymax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
    zMinBoundingBox = -std::max(std::max(fabs(mapymin), fabs(mapymax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
    zMaxBoundingBox = +std::max(std::max(fabs(mapymin), fabs(mapymax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
  } else if (m_yAxiallyPeriodic) {
    xMinBoundingBox = -std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
    xMaxBoundingBox = +std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
    zMinBoundingBox = -std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
    zMaxBoundingBox = +std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
  } else if (m_zAxiallyPeriodic) {
    xMinBoundingBox = -std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapymin), fabs(mapymax)));
    xMaxBoundingBox = +std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapymin), fabs(mapymax)));
    yMinBoundingBox = -std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapymin), fabs(mapymax)));
    yMaxBoundingBox = +std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapymin), fabs(mapymax)));
  }

  if (m_xPeriodic || m_xMirrorPeriodic) {
    xMinBoundingBox = -INFINITY;
    xMaxBoundingBox = +INFINITY;
  }
  if (m_yPeriodic || m_yMirrorPeriodic) {
    yMinBoundingBox = -INFINITY;
    yMaxBoundingBox = +INFINITY;
  }
  if (m_zPeriodic || m_zMirrorPeriodic) {
    zMinBoundingBox = -INFINITY;
    zMaxBoundingBox = +INFINITY;
  }

  // Display the range if requested.
  if (m_debug) PrintRange();
}

void ComponentFieldMap::UpdatePeriodicity2d() {

  // Check the required data is available.
  if (!m_ready) {
    std::cerr << m_className << "::UpdatePeriodicity2d:\n";
    std::cerr << "    No valid field map available.\n";
    return;
  }

  // No z-periodicity in 2d
  if (m_zPeriodic || m_zMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity2d:\n";
    std::cerr << "    Simple or mirror periodicity along z\n";
    std::cerr << "    requested for a 2d map; reset.\n";
    m_zPeriodic = false;
    m_zMirrorPeriodic = false;
    m_warning = true;
  }

  // Only z-axial periodicity in 2d maps
  if (m_xAxiallyPeriodic || m_yAxiallyPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity2d:\n";
    std::cerr << "    Axial symmetry has been requested \n";
    std::cerr << "    around x or y for a 2D map; reset.\n";
    m_xAxiallyPeriodic = false;
    m_yAxiallyPeriodic = false;
    m_warning = true;
  }
}

void ComponentFieldMap::SetRange() {

  // Initial values
  mapxmin = mapymin = mapzmin = 0.;
  mapxmax = mapymax = mapzmax = 0.;
  mapxamin = mapyamin = mapzamin = 0.;
  mapxamax = mapyamax = mapzamax = 0.;
  mapvmin = mapvmax = 0.;
  setangx = setangy = setangz = false;

  // Make sure the required data is available.
  if (!m_ready || nNodes < 1) {
    std::cerr << m_className << "::SetRange:\n";
    std::cerr << "    Field map not yet set.\n";
    return;
  }
  if (nNodes < 1) {
    std::cerr << m_className << "::SetRange:\n";
    std::cerr << "    Number of nodes < 1.\n";
    return;
  }

  // Loop over the nodes.
  mapxmin = mapxmax = nodes[0].x;
  mapymin = mapymax = nodes[0].y;
  mapzmin = mapzmax = nodes[0].z;
  mapvmin = mapvmax = nodes[0].v;

  double ang;
  for (int i = 1; i < nNodes; i++) {
    if (mapxmin > nodes[i].x) mapxmin = nodes[i].x;
    if (mapxmax < nodes[i].x) mapxmax = nodes[i].x;
    if (mapymin > nodes[i].y) mapymin = nodes[i].y;
    if (mapymax < nodes[i].y) mapymax = nodes[i].y;
    if (mapzmin > nodes[i].z) mapzmin = nodes[i].z;
    if (mapzmax < nodes[i].z) mapzmax = nodes[i].z;
    if (mapvmin > nodes[i].v) mapvmin = nodes[i].v;
    if (mapvmax < nodes[i].v) mapvmax = nodes[i].v;

    if (nodes[i].y != 0 || nodes[i].z != 0) {
      ang = atan2(nodes[i].z, nodes[i].y);
      if (setangx) {
        if (ang < mapxamin) mapxamin = ang;
        if (ang > mapxamax) mapxamax = ang;
      } else {
        mapxamin = mapxamax = ang;
        setangx = true;
      }
    }

    if (nodes[i].z != 0 || nodes[i].x != 0) {
      ang = atan2(nodes[i].x, nodes[i].z);
      if (setangy) {
        if (ang < mapyamin) mapyamin = ang;
        if (ang > mapyamax) mapyamax = ang;
      } else {
        mapyamin = mapyamax = ang;
        setangy = true;
      }
    }

    if (nodes[i].x != 0 || nodes[i].y != 0) {
      ang = atan2(nodes[i].y, nodes[i].x);
      if (setangz) {
        if (ang < mapzamin) mapzamin = ang;
        if (ang > mapzamax) mapzamax = ang;
      } else {
        mapzamin = mapzamax = ang;
        setangz = true;
      }
    }
  }

  // Fix the angular ranges.
  if (mapxamax - mapxamin > Pi) {
    double aux = mapxamin;
    mapxamin = mapxamax;
    mapxamax = aux + TwoPi;
  }

  if (mapyamax - mapyamin > Pi) {
    double aux = mapyamin;
    mapyamin = mapyamax;
    mapyamax = aux + TwoPi;
  }

  if (mapzamax - mapzamin > Pi) {
    double aux = mapzamin;
    mapzamin = mapzamax;
    mapzamax = aux + TwoPi;
  }

  // Set the periodicity length (maybe not needed).
  mapsx = fabs(mapxmax - mapxmin);
  mapsy = fabs(mapymax - mapymin);
  mapsz = fabs(mapzmax - mapzmin);

  // Set provisional cell dimensions.
  xMinBoundingBox = mapxmin;
  xMaxBoundingBox = mapxmax;
  yMinBoundingBox = mapymin;
  yMaxBoundingBox = mapymax;
  if (m_is3d) {
    zMinBoundingBox = mapzmin;
    zMaxBoundingBox = mapzmax;
  } else {
    mapzmin = zMinBoundingBox;
    mapzmax = zMaxBoundingBox;
  }
  hasBoundingBox = true;

  // Display the range if requested.
  if (m_debug) PrintRange();
}

void ComponentFieldMap::PrintRange() {

  std::cout << m_className << "::PrintRange:\n";
  std::cout << "        Dimensions of the elementary block\n";
  printf("            %15g < x < %-15g cm,\n", mapxmin, mapxmax);
  printf("            %15g < y < %-15g cm,\n", mapymin, mapymax);
  printf("            %15g < z < %-15g cm,\n", mapzmin, mapzmax);
  printf("            %15g < V < %-15g V.\n", mapvmin, mapvmax);

  std::cout << "        Periodicities\n";

  std::cout << "            x:";
  if (m_xPeriodic) {
    std::cout << " simple with length " << cellsx << " cm";
  }
  if (m_xMirrorPeriodic) {
    std::cout << " mirror with length " << cellsx << " cm";
  }
  if (m_xAxiallyPeriodic) {
    std::cout << " axial " << int(0.5 + mapnxa) << "-fold repetition";
  }
  if (m_xRotationSymmetry) std::cout << " rotational symmetry";
  if (!(m_xPeriodic || m_xMirrorPeriodic || m_xAxiallyPeriodic ||
        m_xRotationSymmetry))
    std::cout << " none";
  std::cout << "\n";

  std::cout << "            y:";
  if (m_yPeriodic) {
    std::cout << " simple with length " << cellsy << " cm";
  }
  if (m_yMirrorPeriodic) {
    std::cout << " mirror with length " << cellsy << " cm";
  }
  if (m_yAxiallyPeriodic) {
    std::cout << " axial " << int(0.5 + mapnya) << "-fold repetition";
  }
  if (m_yRotationSymmetry) {
    std::cout << " rotational symmetry";
  }
  if (!(m_yPeriodic || m_yMirrorPeriodic || m_yAxiallyPeriodic ||
        m_yRotationSymmetry))
    std::cout << " none";
  std::cout << "\n";

  std::cout << "            z:";
  if (m_zPeriodic) {
    std::cout << " simple with length " << cellsz << " cm";
  }
  if (m_zMirrorPeriodic) {
    std::cout << " mirror with length " << cellsz << " cm";
  }
  if (m_zAxiallyPeriodic) {
    std::cout << " axial " << int(0.5 + mapnza) << "-fold repetition";
  }
  if (m_zRotationSymmetry) {
    std::cout << " rotational symmetry";
  }
  if (!(m_zPeriodic || m_zMirrorPeriodic || m_zAxiallyPeriodic ||
        m_zRotationSymmetry))
    std::cout << " none";
  std::cout << "\n";
}

bool ComponentFieldMap::IsInBoundingBox(const double x, const double y,
                                        const double z) {

  if (x >= xMinBoundingBox && x <= xMaxBoundingBox && y >= yMinBoundingBox &&
      y <= yMaxBoundingBox && z >= zMinBoundingBox && z <= zMaxBoundingBox) {
    return true;
  }
  return false;
}

bool ComponentFieldMap::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                                       double& xmax, double& ymax,
                                       double& zmax) {

  if (!m_ready) return false;

  xmin = xMinBoundingBox;
  xmax = xMaxBoundingBox;
  ymin = yMinBoundingBox;
  ymax = yMaxBoundingBox;
  zmin = zMinBoundingBox;
  zmax = zMaxBoundingBox;
  return true;
}

void ComponentFieldMap::MapCoordinates(double& xpos, double& ypos, double& zpos,
                                       bool& xmirrored, bool& ymirrored,
                                       bool& zmirrored, double& rcoordinate,
                                       double& rotation) const {

  // Initial values
  rotation = 0;

  // If chamber is periodic, reduce to the cell volume.
  xmirrored = false;
  double auxr, auxphi;
  if (m_xPeriodic) {
    xpos = mapxmin + fmod(xpos - mapxmin, mapxmax - mapxmin);
    if (xpos < mapxmin) xpos += mapxmax - mapxmin;
  } else if (m_xMirrorPeriodic) {
    double xnew = mapxmin + fmod(xpos - mapxmin, mapxmax - mapxmin);
    if (xnew < mapxmin) xnew += mapxmax - mapxmin;
    int nx = int(floor(0.5 + (xnew - xpos) / (mapxmax - mapxmin)));
    if (nx != 2 * (nx / 2)) {
      xnew = mapxmin + mapxmax - xnew;
      xmirrored = true;
    }
    xpos = xnew;
  }
  if (m_xAxiallyPeriodic && (zpos != 0 || ypos != 0)) {
    auxr = sqrt(zpos * zpos + ypos * ypos);
    auxphi = atan2(zpos, ypos);
    rotation = (mapxamax - mapxamin) *
               floor(0.5 + (auxphi - 0.5 * (mapxamin + mapxamax)) /
                               (mapxamax - mapxamin));
    if (auxphi - rotation < mapxamin)
      rotation = rotation - (mapxamax - mapxamin);
    if (auxphi - rotation > mapxamax)
      rotation = rotation + (mapxamax - mapxamin);
    auxphi = auxphi - rotation;
    ypos = auxr * cos(auxphi);
    zpos = auxr * sin(auxphi);
  }

  ymirrored = false;
  if (m_yPeriodic) {
    ypos = mapymin + fmod(ypos - mapymin, mapymax - mapymin);
    if (ypos < mapymin) ypos += mapymax - mapymin;
  } else if (m_yMirrorPeriodic) {
    double ynew = mapymin + fmod(ypos - mapymin, mapymax - mapymin);
    if (ynew < mapymin) ynew += mapymax - mapymin;
    int ny = int(floor(0.5 + (ynew - ypos) / (mapymax - mapymin)));
    if (ny != 2 * (ny / 2)) {
      ynew = mapymin + mapymax - ynew;
      ymirrored = true;
    }
    ypos = ynew;
  }
  if (m_yAxiallyPeriodic && (xpos != 0 || zpos != 0)) {
    auxr = sqrt(xpos * xpos + zpos * zpos);
    auxphi = atan2(xpos, zpos);
    rotation = (mapyamax - mapyamin) *
               floor(0.5 + (auxphi - 0.5 * (mapyamin + mapyamax)) /
                               (mapyamax - mapyamin));
    if (auxphi - rotation < mapyamin)
      rotation = rotation - (mapyamax - mapyamin);
    if (auxphi - rotation > mapyamax)
      rotation = rotation + (mapyamax - mapyamin);
    auxphi = auxphi - rotation;
    zpos = auxr * cos(auxphi);
    xpos = auxr * sin(auxphi);
  }

  zmirrored = false;
  if (m_zPeriodic) {
    zpos = mapzmin + fmod(zpos - mapzmin, mapzmax - mapzmin);
    if (zpos < mapzmin) zpos += mapzmax - mapzmin;
  } else if (m_zMirrorPeriodic) {
    double znew = mapzmin + fmod(zpos - mapzmin, mapzmax - mapzmin);
    if (znew < mapzmin) znew += mapzmax - mapzmin;
    int nz = int(floor(0.5 + (znew - zpos) / (mapzmax - mapzmin)));
    if (nz != 2 * (nz / 2)) {
      znew = mapzmin + mapzmax - znew;
      zmirrored = true;
    }
    zpos = znew;
  }
  if (m_zAxiallyPeriodic && (ypos != 0 || xpos != 0)) {
    auxr = sqrt(ypos * ypos + xpos * xpos);
    auxphi = atan2(ypos, xpos);
    rotation = (mapzamax - mapzamin) *
               floor(0.5 + (auxphi - 0.5 * (mapzamin + mapzamax)) /
                               (mapzamax - mapzamin));
    if (auxphi - rotation < mapzamin)
      rotation = rotation - (mapzamax - mapzamin);
    if (auxphi - rotation > mapzamax)
      rotation = rotation + (mapzamax - mapzamin);
    auxphi = auxphi - rotation;
    xpos = auxr * cos(auxphi);
    ypos = auxr * sin(auxphi);
  }

  // If we have a rotationally symmetric field map, store coordinates.
  rcoordinate = 0;
  double zcoordinate = 0;
  if (m_xRotationSymmetry) {
    rcoordinate = sqrt(ypos * ypos + zpos * zpos);
    zcoordinate = xpos;
  } else if (m_yRotationSymmetry) {
    rcoordinate = sqrt(xpos * xpos + zpos * zpos);
    zcoordinate = ypos;
  } else if (m_zRotationSymmetry) {
    rcoordinate = sqrt(xpos * xpos + ypos * ypos);
    zcoordinate = zpos;
  }

  if (m_xRotationSymmetry || m_yRotationSymmetry || m_zRotationSymmetry) {
    xpos = rcoordinate;
    ypos = zcoordinate;
    zpos = 0;
  }
}

void ComponentFieldMap::UnmapFields(double& ex, double& ey, double& ez,
                                    double& xpos, double& ypos, double& zpos,
                                    bool& xmirrored, bool& ymirrored,
                                    bool& zmirrored, double& rcoordinate,
                                    double& rotation) const {

  // Apply mirror imaging.
  if (xmirrored) ex = -ex;
  if (ymirrored) ey = -ey;
  if (zmirrored) ez = -ez;

  // Rotate the field.
  double er, theta;
  if (m_xAxiallyPeriodic) {
    er = sqrt(ey * ey + ez * ez);
    theta = atan2(ez, ey);
    theta += rotation;
    ey = er * cos(theta);
    ez = er * sin(theta);
  }
  if (m_yAxiallyPeriodic) {
    er = sqrt(ez * ez + ex * ex);
    theta = atan2(ex, ez);
    theta += rotation;
    ez = er * cos(theta);
    ex = er * sin(theta);
  }
  if (m_zAxiallyPeriodic) {
    er = sqrt(ex * ex + ey * ey);
    theta = atan2(ey, ex);
    theta += rotation;
    ex = er * cos(theta);
    ey = er * sin(theta);
  }

  // Take care of symmetry.
  double eaxis;
  er = ex;
  eaxis = ey;

  // Rotational symmetry
  if (m_xRotationSymmetry) {
    if (rcoordinate <= 0) {
      ex = eaxis;
      ey = 0;
      ez = 0;
    } else {
      ex = eaxis;
      ey = er * ypos / rcoordinate;
      ez = er * zpos / rcoordinate;
    }
  }
  if (m_yRotationSymmetry) {
    if (rcoordinate <= 0) {
      ex = 0;
      ey = eaxis;
      ez = 0;
    } else {
      ex = er * xpos / rcoordinate;
      ey = eaxis;
      ez = er * zpos / rcoordinate;
    }
  }
  if (m_zRotationSymmetry) {
    if (rcoordinate <= 0) {
      ex = 0;
      ey = 0;
      ez = eaxis;
    } else {
      ex = er * xpos / rcoordinate;
      ey = er * ypos / rcoordinate;
      ez = eaxis;
    }
  }
}

int ComponentFieldMap::ReadInteger(char* token, int def, bool& error) {

  if (!token) {
    error = true;
    return def;
  }

  return atoi(token);
}

double ComponentFieldMap::ReadDouble(char* token, double def, bool& error) {

  if (!token) {
    error = true;
    return def;
  }
  return atof(token);
}

void ComponentFieldMap::CalculateElementBoundingBoxes(void) {

  // Do not proceed if not properly initialised.
  if (!m_ready) {
    PrintNotReady("CalculateElementBoundingBoxes");
    return;
  }

  // Calculate the bounding boxes of all elements
  for (int i = 0; i < nElements; ++i) {
    Element& elem = elements[i];
    const Node& n0 = nodes[elem.emap[0]];
    const Node& n1 = nodes[elem.emap[1]];
    const Node& n2 = nodes[elem.emap[2]];
    const Node& n3 = nodes[elem.emap[3]];
    elem.xmin = std::min(std::min(n0.x, n1.x), std::min(n2.x, n3.x));
    elem.xmax = std::max(std::max(n0.x, n1.x), std::max(n2.x, n3.x));
    elem.ymin = std::min(std::min(n0.y, n1.y), std::min(n2.y, n3.y));
    elem.ymax = std::max(std::max(n0.y, n1.y), std::max(n2.y, n3.y));
    elem.zmin = std::min(std::min(n0.z, n1.z), std::min(n2.z, n3.z));
    elem.zmax = std::max(std::max(n0.z, n1.z), std::max(n2.z, n3.z));
  }
}

bool ComponentFieldMap::InitializeTetrahedralTree() {

  // Do not proceed if not properly initialised.
  if (!m_ready) {
    PrintNotReady("InitializeTetrahedralTree");
    return false;
  }

  std::cerr << m_className << "::InitializeTetrahedralTree:\n";
  std::cerr << "    About to initialize the tetrahedral tree.\n";

  // check if the caching has not been done before
  if (!m_cacheElemBoundingBoxes) CalculateElementBoundingBoxes();

  // Determine the bounding box
  double xmin = 0., ymin = 0., zmin = 0., xmax = 0., ymax = 0., zmax = 0.;
  for (unsigned int i = 0; i < nodes.size(); i++) {
    const Node& n = nodes[i];
    if (n.x <= xmin) xmin = n.x;
    if (n.x > xmax) xmax = n.x;
    if (n.y <= ymin) ymin = n.y;
    if (n.y > ymax) ymax = n.y;
    if (n.z <= zmin) zmin = n.z;
    if (n.z > zmax) zmax = n.z;
  }

  std::cout << "    Bounding box:\n"
            << std::scientific << "\tx: " << xmin << " -> " << xmax << "\n"
            << std::scientific << "\ty: " << ymin << " -> " << ymax << "\n"
            << std::scientific << "\tz: " << zmin << " -> " << zmax << "\n";

  const double hx = 0.5 * (xmax - xmin);
  const double hy = 0.5 * (ymax - ymin);
  const double hz = 0.5 * (zmax - zmin);
  m_tetTree = new TetrahedralTree(Vec3(xmin + hx, ymin + hy, zmin + hz),
                                  Vec3(hx, hy, hz));

  std::cerr << "Tree instantiated.\n";

  // insert all mesh nodes in the tree
  for (unsigned int i = 0; i < nodes.size(); i++) {
    const Node& n = nodes[i];
    m_tetTree->InsertMeshNode(Vec3(n.x, n.y, n.z), i);
  }

  std::cerr << m_className << "::InitializeTetrahedralTree:\n";
  std::cerr << "    Tetrahedral tree nodes initialized successfully.\n";

  // insert all mesh elements (tetrahedrons) in the tree
  for (unsigned int i = 0; i < elements.size(); i++) {
    const Element& e = elements[i];
    const double bb[6] = {e.xmin, e.ymin, e.zmin, e.xmax, e.ymax, e.zmax};
    m_tetTree->InsertTetrahedron(bb, i);
  }

  std::cerr << m_className << "::InitializeTetrahedralTree:\n";
  std::cerr << "    Tetrahedral tree initialized successfully.\n";

  m_isTreeInitialized = true;
  return true;
}

void ComponentFieldMap::PrintElement(const std::string& header, const double x,
                                     const double y, const double z,
                                     const double t1, const double t2,
                                     const double t3, const double t4,
                                     const unsigned int i, const unsigned int n,
                                     const int iw) const {

  const Element& element = elements[i];
  std::cout << m_className << "::" << header << ":\n"
            << "    Global = (" << x << ", " << y << ", " << z << ")\n"
            << "    Local = (" << t1 << ", " << t2 << ", " << t3 << ", " << t4
            << ")\n"
            << "    Element = " << i << " (degenerate: " << element.degenerate
            << ")\n     "
            << " Node             x            y            z            V\n";
  for (unsigned int ii = 0; ii < n; ++ii) {
    const Node& node = nodes[element.emap[ii]];
    const double v = iw < 0 ? node.v : node.w[iw];
    printf("      %-5d %12g %12g %12g %12g\n", element.emap[i], node.x, node.y,
           node.z, v);
  }
}
}
