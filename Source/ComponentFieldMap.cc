#include <stdio.h>
#include <string.h>
#include <iostream>
#include <fstream>

#include <stdlib.h>
#include <math.h>
#include <string>

#include "ComponentFieldMap.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

ComponentFieldMap::ComponentFieldMap()
    : is3d(true),
      nElements(-1),
      lastElement(-1),
      cacheElemBoundingBoxes(false),
      nNodes(-1),
      nMaterials(-1),
      nWeightingFields(0),
      hasBoundingBox(false),
      deleteBackground(true),
      checkMultipleElement(false),
      warning(false),
      tetTree(NULL),
      useTetrahedralTreeForSearch(false),
      isTreeInitialized(false){

  m_className = "ComponentFieldMap";

  materials.clear();
  elements.clear();
  nodes.clear();
  wfields.clear();
  wfieldsOk.clear();
}

ComponentFieldMap::~ComponentFieldMap() {
  if(tetTree)
      delete tetTree;
}

void ComponentFieldMap::PrintMaterials() {

  // Do not proceed if not properly initialised.
  if (!ready) {
    std::cerr << m_className << "::PrintMaterials:\n";
    std::cerr << "    Field map not yet initialised.\n";
    return;
  }

  if (nMaterials < 0) {
    std::cerr << m_className << "::PrintMaterials:\n";
    std::cerr << "    No materials are currently defined.\n";
    return;
  }

  std::cout << m_className << "::PrintMaterials:\n";
  std::cout << "    Currently " << nMaterials << " materials are defined.\n";
  std::cout << "      Index Permittivity  Resistivity Notes\n";
  for (int i = 0; i < nMaterials; ++i) {
    printf("      %5d %12g %12g", i, materials[i].eps, materials[i].ohm);
    if (materials[i].medium != 0) {
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

void ComponentFieldMap::DriftMedium(int imat) {

  // Do not proceed if not properly initialised.
  if (!ready) {
    std::cerr << m_className << "::DriftMedium:\n";
    std::cerr << "    Field map not yet initialised.\n";
    std::cerr << "    Drift medium cannot be selected.\n";
    return;
  }

  // Check value
  if (imat < 0 || imat >= nMaterials) {
    std::cerr << m_className << "::DriftMedium:\n";
    std::cerr << "    Material index " << imat << " is out of range.\n";
    return;
  }

  // Make drift medium
  materials[imat].driftmedium = true;
}

void ComponentFieldMap::NotDriftMedium(const int imat) {

  // Do not proceed if not properly initialised.
  if (!ready) {
    std::cerr << m_className << "::NotDriftMedium:\n";
    std::cerr << "    Field map not yet initialised.\n";
    std::cerr << "    Drift medium cannot be selected.\n";
    return;
  }

  // Check value
  if (imat < 0 || imat >= nMaterials) {
    std::cerr << m_className << "::NotDriftMedium:\n";
    std::cerr << "    Material index " << imat << " is out of range.\n";
    return;
  }

  // Make drift medium
  materials[imat].driftmedium = false;
}

double ComponentFieldMap::GetPermittivity(const int imat) {

  if (imat < 0 || imat >= nMaterials) {
    std::cerr << m_className << "::GetPermittivity:\n";
    std::cerr << "    Material index " << imat << " is out of range.\n";
    return -1.;
  }

  return materials[imat].eps;
}

double ComponentFieldMap::GetConductivity(const int imat) {

  if (imat < 0 || imat >= nMaterials) {
    std::cerr << m_className << "::GetConductivity:\n";
    std::cerr << "    Material index " << imat << " is out of range.\n";
    return -1.;
  }

  return materials[imat].ohm;
}

void ComponentFieldMap::SetMedium(const int imat, Medium* m) {

  if (imat < 0 || imat >= nMaterials) {
    std::cerr << m_className << "::SetMedium:\n";
    std::cerr << "    Material index " << imat << " is out of range.\n";
    return;
  }

  if (!m) {
    std::cerr << m_className << "::SetMedium:\n";
    std::cerr << "    Medium pointer is null.\n";
    return;
  }

  if (debug) {
    std::string name = m->GetName();
    std::cout << m_className << "::SetMedium:\n";
    std::cout << "    Associated material " << imat << " with medium " << name
              << ".\n";
  }

  materials[imat].medium = m;
}

Medium* ComponentFieldMap::GetMedium(const unsigned int& imat) const {

  if (imat >= (unsigned int)nMaterials) {
    std::cerr << m_className << "::GetMedium:\n";
    std::cerr << "    Material index " << imat << " is out of range.\n";
    return NULL;
  }

  return materials[imat].medium;
}

bool ComponentFieldMap::GetElement(const int i, double& vol, double& dmin,
                                   double& dmax) {

  if (i < 0 || i >= nElements) {
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
  if(!cacheElemBoundingBoxes) {
    std::cerr << m_className << "::FindElement5:\n";
    std::cerr << "    Caching the bounding box calculations of all elements.\n";

    CalculateElementBoundingBoxes();
    cacheElemBoundingBoxes = true;
  }

  // this variable tracks how many elements to scan. with tetra tree disabled, all elements are scanned
  int numElemToSearch = nElements;
  // the list to store the tetra list in the block that contains the input 3D point.
  std::vector<int> tetList;

  // Check if the tetrahedral is enabled
  if(useTetrahedralTreeForSearch) {
    if(!isTreeInitialized) {
      if(!InitializeTetrahedralTree()) {
        std::cerr << m_className << "::FindElement5:\n";
        std::cerr << "    Tetrahedral tree initialization failed.\n";
        return -1;
      }
    }
    tetList = tetTree->GetTetListInBlock( Vec3(x, y, z) );
    numElemToSearch = tetList.size();
  }

  // Backup
  double jacbak[4][4], detbak = 1.;
  double t1bak = 0., t2bak = 0., t3bak = 0., t4bak = 0.;
  int imapbak = -1;

  // Initial values.
  t1 = t2 = t3 = t4 = 0;

  // Check previously used element
  int rc;
  if (lastElement > -1 && !checkMultipleElement) {
    if (elements[lastElement].degenerate) {
      rc = Coordinates3(x, y, z, t1, t2, t3, t4, jac, det, lastElement);
      if (rc == 0 && t1 >= 0 && t1 <= +1 && t2 >= 0 && t2 <= +1 && t3 >= 0 &&
          t3 <= +1)
        return lastElement;
    } else {
      rc = Coordinates5(x, y, z, t1, t2, t3, t4, jac, det, lastElement);
      if (rc == 0 && t1 >= -1 && t1 <= +1 && t2 >= -1 && t2 <= +1)
        return lastElement;
    }
  }

  // Verify the count of volumes that contain the point.
  int nfound = 0;
  int imap = -1;

  // Tolerance
  const double f = 0.2;

  // Scan all elements
  for (int i = 0; i < numElemToSearch; ++i) {
    int idxToElemList;
    
    if(useTetrahedralTreeForSearch)
      idxToElemList = tetList[i];
    else
      idxToElemList = i;

    element& e = elements[idxToElemList];
    if (x < e.xmin - f * (e.xmax - e.xmin) || x > e.xmax + f * (e.xmax - e.xmin) ||
        y < e.ymin - f * (e.ymax - e.ymin) || y > e.ymax + f * (e.ymax - e.ymin) ||
        z < e.zmin - f * (e.zmax - e.zmin) || z > e.zmax + f * (e.zmax - e.zmin))
      continue;

    if (elements[idxToElemList].degenerate) {
      // Degenerate element
      rc = Coordinates3(x, y, z, t1, t2, t3, t4, jac, det, idxToElemList);
      if (rc == 0 && t1 >= 0 && t1 <= +1 && t2 >= 0 && t2 <= +1 && t3 >= 0 &&
          t3 <= +1) {
        ++nfound;
        imap = idxToElemList;
        lastElement = idxToElemList;
        if (debug) {
          std::cout << m_className << "::FindElement5:\n";
          std::cout << "    Found matching degenerate element " << idxToElemList << ".\n";
        }
        if (!checkMultipleElement) return idxToElemList;
        for (int j = 0; j < 4; ++j) {
          for (int k = 0; k < 4; ++k) jacbak[j][k] = jac[j][k];
        }
        detbak = det;
        t1bak = t1;
        t2bak = t2;
        t3bak = t3;
        t4bak = t4;
        imapbak = imap;
        if (debug) {
          std::cout << m_className << "::FindElement5:\n";
          std::cout << "    Global = (" << x << ", " << y << ")\n";
          std::cout << "    Local = (" << t1 << ", " << t2 << ", " << t3 << ", "
                    << t4 << ")\n";
          std::cout << "    Element = " << imap
                    << " (degenerate: " << elements[imap].degenerate << ")\n";
          std::cout << "                          Node             x           "
                       " y            V\n";
          for (int ii = 0; ii < 6; ++ii) {
            printf("                          %-5d %12g %12g %12g\n",
                   elements[imap].emap[ii], nodes[elements[imap].emap[ii]].x,
                   nodes[elements[imap].emap[ii]].y,
                   nodes[elements[imap].emap[ii]].v);
          }
        }
      }
    } else {
      // Non-degenerate element
      rc = Coordinates5(x, y, z, t1, t2, t3, t4, jac, det, idxToElemList);
      if (rc == 0 && t1 >= -1 && t1 <= +1 && t2 >= -1 && t2 <= +1) {
        ++nfound;
        imap = idxToElemList;
        lastElement = idxToElemList;
        if (debug) {
          std::cout << m_className << "::FindElement5:\n";
          std::cout << "    Found matching non-degenerate element " << idxToElemList
                    << ".\n";
        }
        if (!checkMultipleElement) return idxToElemList;
        for (int j = 0; j < 4; ++j) {
          for (int k = 0; k < 4; ++k) jacbak[j][k] = jac[j][k];
        }
        detbak = det;
        t1bak = t1;
        t2bak = t2;
        t3bak = t3;
        t4bak = t4;
        imapbak = imap;
        if (debug) {
          std::cout << m_className << "::FindElement5:\n";
          std::cout << "    Global = (" << x << ", " << y << ")\n";
          std::cout << "    Local = (" << t1 << ", " << t2 << ", " << t3 << ", "
                    << t4 << ")\n";
          std::cout << "    Element = " << imap
                    << " (degenerate: " << elements[imap].degenerate << ")\n";
          std::cout << "                          Node             x           "
                       " y            V\n";
          for (int ii = 0; ii < 8; ++ii) {
            printf("                          %-5d %12g %12g %12g\n",
                   elements[imap].emap[ii], nodes[elements[imap].emap[ii]].x,
                   nodes[elements[imap].emap[ii]].y,
                   nodes[elements[imap].emap[ii]].v);
          }
        }
      }
    }
  }

  // In checking mode, verify the tetrahedron/triangle count.
  if (checkMultipleElement) {
    if (nfound < 1) {
      if (debug) {
        std::cout << m_className << "::FindElement5:\n";
        std::cout << "    No element matching point (" << x << ", " << y
                  << ") found.\n";
      }
      lastElement = -1;
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
      lastElement = imap;
      return imap;
    }
  }

  if (debug) {
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
  if(!cacheElemBoundingBoxes) {
    std::cerr << m_className << "::FindElement13:\n";
    std::cerr << "    Caching the bounding box calculations of all elements.\n";
  
    CalculateElementBoundingBoxes();
    cacheElemBoundingBoxes = true;
  }
  
  // Backup
  double jacbak[4][4];
  double detbak = 1.;
  double t1bak = 0., t2bak = 0., t3bak = 0., t4bak = 0.;
  int imapbak = -1;

  // Initial values.
  t1 = t2 = t3 = t4 = 0.;

  // Check previously used element
  int rc;
  if (lastElement > -1 && !checkMultipleElement) {
    rc = Coordinates13(x, y, z, t1, t2, t3, t4, jac, det, lastElement);
    if (rc == 0 && t1 >= 0 && t1 <= +1 && t2 >= 0 && t2 <= +1 && t3 >= 0 &&
        t3 <= +1 && t4 >= 0 && t4 <= +1)
      return lastElement;
  }

  // this variable tracks how many elements to scan. with tetra tree disabled, all elements are scanned
  int numElemToSearch = nElements;
  // the list to store the tetra list in the block that contains the input 3D point.
  std::vector<int> tetList;

  // Check if the tetrahedral is enabled
  if(useTetrahedralTreeForSearch) {
    if(!isTreeInitialized) {
      if(!InitializeTetrahedralTree()) {
        std::cerr << m_className << "::FindElement13:\n";
        std::cerr << "    Tetrahedral tree initialization failed.\n";
        return -1;
      }
    }
    tetList = tetTree->GetTetListInBlock( Vec3(x, y, z) );
    numElemToSearch = tetList.size();
  }

  // Verify the count of volumes that contain the point.
  int nfound = 0;
  int imap = -1;

  // Tolerance
  const double f = 0.2;

  // Scan all elements
  for (int i = 0; i < numElemToSearch; i++) {
    int idxToElemList;
    
    if(useTetrahedralTreeForSearch)
      idxToElemList = tetList[i];
    else
      idxToElemList = i;

    element& e = elements[idxToElemList];

    if (x < e.xmin - f * (e.xmax - e.xmin) || x > e.xmax + f * (e.xmax - e.xmin) ||
        y < e.ymin - f * (e.ymax - e.ymin) || y > e.ymax + f * (e.ymax - e.ymin) ||
        z < e.zmin - f * (e.zmax - e.zmin) || z > e.zmax + f * (e.zmax - e.zmin))
      continue;

    rc = Coordinates13(x, y, z, t1, t2, t3, t4, jac, det, idxToElemList);

    if (rc == 0 && t1 >= 0 && t1 <= +1 && t2 >= 0 && t2 <= +1 && t3 >= 0 &&
        t3 <= +1 && t4 >= 0 && t4 <= +1) {
      ++nfound;
      imap = idxToElemList;
      lastElement = idxToElemList;
      if (debug) {
        std::cout << m_className << "::FindElement13:\n";
        std::cout << "    Found matching element " << i << ".\n";
      }
      if (!checkMultipleElement) return idxToElemList;
      for (int j = 0; j < 4; ++j) {
        for (int k = 0; k < 4; ++k) jacbak[j][k] = jac[j][k];
      }
      detbak = det;
      t1bak = t1;
      t2bak = t2;
      t3bak = t3;
      t4bak = t4;
      imapbak = imap;
      if (debug) {
        std::cout << m_className << "::FindElement13:\n";
        std::cout << "    Global = (" << x << ", " << y << ")\n";
        std::cout << "    Local = (" << t1 << ", " << t2 << ", " << t3 << ", "
                  << t4 << ")\n";
        std::cout << "    Element = " << imap << "\n";
        std::cout << "                          Node             x            "
                     "y            z            V\n";
        for (int ii = 0; ii < 10; ++ii) {
          printf("                          %-5d %12g %12g %12g %12g\n",
                 elements[imap].emap[ii], nodes[elements[imap].emap[ii]].x,
                 nodes[elements[imap].emap[ii]].y,
                 nodes[elements[imap].emap[ii]].z,
                 nodes[elements[imap].emap[ii]].v);
        }
      }
    }
  }

  // In checking mode, verify the tetrahedron/triangle count.
  if (checkMultipleElement) {
    if (nfound < 1) {
      if (debug) {
        std::cout << m_className << "::FindElement13:\n";
        std::cout << "    No element matching point (" << x << ", " << y << ", "
                  << z << ") found.\n";
      }
      lastElement = -1;
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
      lastElement = imap;
      return imap;
    }
  }

  if (debug) {
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

  if (x >= nodes[elements[lastElement].emap[3]].x &&
      y >= nodes[elements[lastElement].emap[3]].y &&
      z >= nodes[elements[lastElement].emap[3]].z &&
      x < nodes[elements[lastElement].emap[0]].x &&
      y < nodes[elements[lastElement].emap[2]].y &&
      z < nodes[elements[lastElement].emap[7]].z) {
    imap = lastElement;
  }

  // Default element loop
  if (imap == -1) {
    for (int i = 0; i < nElements; ++i) {
      if (x >= nodes[elements[i].emap[3]].x &&
          y >= nodes[elements[i].emap[3]].y &&
          z >= nodes[elements[i].emap[3]].z &&
          x < nodes[elements[i].emap[0]].x &&
          y < nodes[elements[i].emap[2]].y &&
          z < nodes[elements[i].emap[7]].z) {
        imap = i;
        break;
      }
    }
  }

  if (imap < 0) {
    if (debug) {
      std::cout << m_className << "::FindElementCube:\n";
      std::cout << "    Point (" << x << "," << y << "," << z
                << ") not in the mesh, it is background or PEC.\n";
      std::cout << "    First node (" << nodes[elements[0].emap[3]].x << ","
                << nodes[elements[0].emap[3]].y << ","
                << nodes[elements[0].emap[3]].z << ") in the mesh.\n";
      std::cout << "    dx= "
                << (nodes[elements[0].emap[0]].x - nodes[elements[0].emap[3]].x)
                << ", dy= "
                << (nodes[elements[0].emap[2]].y - nodes[elements[0].emap[3]].y)
                << ", dz= "
                << (nodes[elements[0].emap[7]].z - nodes[elements[0].emap[3]].z)
                << "\n";
      std::cout << "    Last node (" << nodes[elements[nElements - 1].emap[5]].x
                << "," << nodes[elements[nElements - 1].emap[5]].y << ","
                << nodes[elements[nElements - 1].emap[5]].z
                << ") in the mesh.\n";
      std::cout << "  dx= " << (nodes[elements[nElements - 1].emap[0]].x -
                                nodes[elements[nElements - 1].emap[3]].x)
                << ", dy= " << (nodes[elements[nElements - 1].emap[2]].y -
                                nodes[elements[nElements - 1].emap[3]].y)
                << ", dz= " << (nodes[elements[nElements - 1].emap[7]].z -
                                nodes[elements[nElements - 1].emap[3]].z)
                << "\n";
    }
    return -1;
  }
  CoordinatesCube(x, y, z, t1, t2, t3, jac, dN, imap);
  if (debug) {
    std::cout << m_className << "::FindElementCube:\n";
    std::cout << "Global: (" << x << "," << y << "," << z << ") in element "
              << imap << " (degenerate: " << elements[imap].degenerate << ")\n";
    std::cout << "      Node xyzV\n";
    for (int i = 0; i < 8; i++) {
      std::cout << "  " << elements[imap].emap[i] << " "
                << nodes[elements[imap].emap[i]].x << " "
                << nodes[elements[imap].emap[i]].y << " "
                << nodes[elements[imap].emap[i]].z << " "
                << nodes[elements[imap].emap[i]].v << "\n";
    }
  }
  return imap;
}

void ComponentFieldMap::Jacobian3(int i, double u, double v, double w,
                                  double& det, double jac[4][4]) {

  // Initial values
  det = 0;
  jac[0][0] = 0;
  jac[0][1] = 0;
  jac[1][0] = 0;
  jac[1][1] = 0;

  // Be sure that the element is within range
  if (i < 0 || i >= nElements) {
    std::cerr << m_className << "::Jacobian3:\n";
    std::cerr << "    Element " << i << " out of range.\n";
    return;
  }

  // Determinant of the quadratic triangular Jacobian
  det =
      -(((-1 + 4 * v) * nodes[elements[i].emap[1]].x +
         nodes[elements[i].emap[2]].x - 4 * w * nodes[elements[i].emap[2]].x +
         4 * u * nodes[elements[i].emap[3]].x -
         4 * u * nodes[elements[i].emap[4]].x -
         4 * v * nodes[elements[i].emap[5]].x +
         4 * w * nodes[elements[i].emap[5]].x) *
        (-nodes[elements[i].emap[0]].y + 4 * u * nodes[elements[i].emap[0]].y +
         4 * v * nodes[elements[i].emap[3]].y +
         4 * w * nodes[elements[i].emap[4]].y)) -
      ((-1 + 4 * u) * nodes[elements[i].emap[0]].x +
       nodes[elements[i].emap[1]].x - 4 * v * nodes[elements[i].emap[1]].x -
       4 * u * nodes[elements[i].emap[3]].x +
       4 * v * nodes[elements[i].emap[3]].x +
       4 * w * nodes[elements[i].emap[4]].x -
       4 * w * nodes[elements[i].emap[5]].x) *
          (-nodes[elements[i].emap[2]].y +
           4 * w * nodes[elements[i].emap[2]].y +
           4 * u * nodes[elements[i].emap[4]].y +
           4 * v * nodes[elements[i].emap[5]].y) +
      ((-1 + 4 * u) * nodes[elements[i].emap[0]].x +
       nodes[elements[i].emap[2]].x - 4 * w * nodes[elements[i].emap[2]].x +
       4 * v * nodes[elements[i].emap[3]].x -
       4 * u * nodes[elements[i].emap[4]].x +
       4 * w * nodes[elements[i].emap[4]].x -
       4 * v * nodes[elements[i].emap[5]].x) *
          (-nodes[elements[i].emap[1]].y +
           4 * v * nodes[elements[i].emap[1]].y +
           4 * u * nodes[elements[i].emap[3]].y +
           4 * w * nodes[elements[i].emap[5]].y);

  // Terms of the quadratic triangular Jacobian
  jac[0][0] =
      (-nodes[elements[i].emap[1]].x + 4 * v * nodes[elements[i].emap[1]].x +
       4 * u * nodes[elements[i].emap[3]].x +
       4 * w * nodes[elements[i].emap[5]].x) *
          (-nodes[elements[i].emap[2]].y +
           4 * w * nodes[elements[i].emap[2]].y +
           4 * u * nodes[elements[i].emap[4]].y +
           4 * v * nodes[elements[i].emap[5]].y) -
      (-nodes[elements[i].emap[2]].x + 4 * w * nodes[elements[i].emap[2]].x +
       4 * u * nodes[elements[i].emap[4]].x +
       4 * v * nodes[elements[i].emap[5]].x) *
          (-nodes[elements[i].emap[1]].y +
           4 * v * nodes[elements[i].emap[1]].y +
           4 * u * nodes[elements[i].emap[3]].y +
           4 * w * nodes[elements[i].emap[5]].y);
  jac[0][1] = (-1 + 4 * v) * nodes[elements[i].emap[1]].y +
              nodes[elements[i].emap[2]].y -
              4 * w * nodes[elements[i].emap[2]].y +
              4 * u * nodes[elements[i].emap[3]].y -
              4 * u * nodes[elements[i].emap[4]].y -
              4 * v * nodes[elements[i].emap[5]].y +
              4 * w * nodes[elements[i].emap[5]].y;
  jac[0][2] = nodes[elements[i].emap[1]].x -
              4 * v * nodes[elements[i].emap[1]].x +
              (-1 + 4 * w) * nodes[elements[i].emap[2]].x -
              4 * u * nodes[elements[i].emap[3]].x +
              4 * u * nodes[elements[i].emap[4]].x +
              4 * v * nodes[elements[i].emap[5]].x -
              4 * w * nodes[elements[i].emap[5]].x;
  jac[1][0] =
      (-nodes[elements[i].emap[2]].x + 4 * w * nodes[elements[i].emap[2]].x +
       4 * u * nodes[elements[i].emap[4]].x +
       4 * v * nodes[elements[i].emap[5]].x) *
          (-nodes[elements[i].emap[0]].y +
           4 * u * nodes[elements[i].emap[0]].y +
           4 * v * nodes[elements[i].emap[3]].y +
           4 * w * nodes[elements[i].emap[4]].y) -
      (-nodes[elements[i].emap[0]].x + 4 * u * nodes[elements[i].emap[0]].x +
       4 * v * nodes[elements[i].emap[3]].x +
       4 * w * nodes[elements[i].emap[4]].x) *
          (-nodes[elements[i].emap[2]].y +
           4 * w * nodes[elements[i].emap[2]].y +
           4 * u * nodes[elements[i].emap[4]].y +
           4 * v * nodes[elements[i].emap[5]].y);
  jac[1][1] =
      nodes[elements[i].emap[0]].y - 4 * u * nodes[elements[i].emap[0]].y -
      nodes[elements[i].emap[2]].y + 4 * w * nodes[elements[i].emap[2]].y -
      4 * v * nodes[elements[i].emap[3]].y +
      4 * u * nodes[elements[i].emap[4]].y -
      4 * w * nodes[elements[i].emap[4]].y +
      4 * v * nodes[elements[i].emap[5]].y;
  jac[1][2] = (-1 + 4 * u) * nodes[elements[i].emap[0]].x +
              nodes[elements[i].emap[2]].x -
              4 * w * nodes[elements[i].emap[2]].x +
              4 * v * nodes[elements[i].emap[3]].x -
              4 * u * nodes[elements[i].emap[4]].x +
              4 * w * nodes[elements[i].emap[4]].x -
              4 * v * nodes[elements[i].emap[5]].x;
  jac[2][0] =
      -((-nodes[elements[i].emap[1]].x + 4 * v * nodes[elements[i].emap[1]].x +
         4 * u * nodes[elements[i].emap[3]].x +
         4 * w * nodes[elements[i].emap[5]].x) *
        (-nodes[elements[i].emap[0]].y + 4 * u * nodes[elements[i].emap[0]].y +
         4 * v * nodes[elements[i].emap[3]].y +
         4 * w * nodes[elements[i].emap[4]].y)) +
      (-nodes[elements[i].emap[0]].x + 4 * u * nodes[elements[i].emap[0]].x +
       4 * v * nodes[elements[i].emap[3]].x +
       4 * w * nodes[elements[i].emap[4]].x) *
          (-nodes[elements[i].emap[1]].y +
           4 * v * nodes[elements[i].emap[1]].y +
           4 * u * nodes[elements[i].emap[3]].y +
           4 * w * nodes[elements[i].emap[5]].y);
  jac[2][1] = (-1 + 4 * u) * nodes[elements[i].emap[0]].y +
              nodes[elements[i].emap[1]].y -
              4 * v * nodes[elements[i].emap[1]].y -
              4 * u * nodes[elements[i].emap[3]].y +
              4 * v * nodes[elements[i].emap[3]].y +
              4 * w * nodes[elements[i].emap[4]].y -
              4 * w * nodes[elements[i].emap[5]].y;
  jac[2][2] =
      nodes[elements[i].emap[0]].x - 4 * u * nodes[elements[i].emap[0]].x -
      nodes[elements[i].emap[1]].x + 4 * v * nodes[elements[i].emap[1]].x +
      4 * u * nodes[elements[i].emap[3]].x -
      4 * v * nodes[elements[i].emap[3]].x -
      4 * w * nodes[elements[i].emap[4]].x +
      4 * w * nodes[elements[i].emap[5]].x;
}

void ComponentFieldMap::Jacobian5(int i, double u, double v, double& det,
                                  double jac[4][4]) {

  // Initial values
  det = 0;
  jac[0][0] = 0;
  jac[0][1] = 0;
  jac[1][0] = 0;
  jac[1][1] = 0;

  // Be sure that the element is within range
  if (i < 0 || i >= nElements) {
    std::cerr << m_className << "::Jacobian5:\n";
    std::cerr << "    Element " << i << " out of range.\n";
    return;
  }

  // Determinant of the quadrilateral serendipity Jacobian
  det =
      (-2 * u * u * u *
           ((nodes[elements[i].emap[2]].x + nodes[elements[i].emap[3]].x -
             2 * nodes[elements[i].emap[6]].x) *
                (nodes[elements[i].emap[0]].y + nodes[elements[i].emap[1]].y -
                 2 * nodes[elements[i].emap[4]].y) -
            (nodes[elements[i].emap[0]].x + nodes[elements[i].emap[1]].x -
             2 * nodes[elements[i].emap[4]].x) *
                (nodes[elements[i].emap[2]].y + nodes[elements[i].emap[3]].y -
                 2 * nodes[elements[i].emap[6]].y)) +
       2 * v * v * v *
           (-((nodes[elements[i].emap[0]].x + nodes[elements[i].emap[3]].x -
               2 * nodes[elements[i].emap[7]].x) *
              (nodes[elements[i].emap[1]].y + nodes[elements[i].emap[2]].y -
               2 * nodes[elements[i].emap[5]].y)) +
            (nodes[elements[i].emap[1]].x + nodes[elements[i].emap[2]].x -
             2 * nodes[elements[i].emap[5]].x) *
                (nodes[elements[i].emap[0]].y + nodes[elements[i].emap[3]].y -
                 2 * nodes[elements[i].emap[7]].y)) +
       2 * (-((nodes[elements[i].emap[5]].x - nodes[elements[i].emap[7]].x) *
              (nodes[elements[i].emap[4]].y - nodes[elements[i].emap[6]].y)) +
            (nodes[elements[i].emap[4]].x - nodes[elements[i].emap[6]].x) *
                (nodes[elements[i].emap[5]].y - nodes[elements[i].emap[7]].y)) +
       v * (-(nodes[elements[i].emap[6]].x * nodes[elements[i].emap[0]].y) -
            2 * nodes[elements[i].emap[7]].x * nodes[elements[i].emap[0]].y +
            nodes[elements[i].emap[6]].x * nodes[elements[i].emap[1]].y -
            2 * nodes[elements[i].emap[7]].x * nodes[elements[i].emap[1]].y -
            nodes[elements[i].emap[6]].x * nodes[elements[i].emap[2]].y -
            2 * nodes[elements[i].emap[7]].x * nodes[elements[i].emap[2]].y +
            nodes[elements[i].emap[4]].x *
                (nodes[elements[i].emap[0]].y - nodes[elements[i].emap[1]].y +
                 nodes[elements[i].emap[2]].y - nodes[elements[i].emap[3]].y) +
            nodes[elements[i].emap[6]].x * nodes[elements[i].emap[3]].y -
            2 * nodes[elements[i].emap[7]].x * nodes[elements[i].emap[3]].y -
            nodes[elements[i].emap[0]].x * nodes[elements[i].emap[4]].y +
            nodes[elements[i].emap[1]].x * nodes[elements[i].emap[4]].y -
            nodes[elements[i].emap[2]].x * nodes[elements[i].emap[4]].y +
            nodes[elements[i].emap[3]].x * nodes[elements[i].emap[4]].y -
            2 * nodes[elements[i].emap[0]].x * nodes[elements[i].emap[5]].y -
            2 * nodes[elements[i].emap[1]].x * nodes[elements[i].emap[5]].y -
            2 * nodes[elements[i].emap[2]].x * nodes[elements[i].emap[5]].y -
            2 * nodes[elements[i].emap[3]].x * nodes[elements[i].emap[5]].y +
            8 * nodes[elements[i].emap[7]].x * nodes[elements[i].emap[5]].y +
            nodes[elements[i].emap[0]].x * nodes[elements[i].emap[6]].y -
            nodes[elements[i].emap[1]].x * nodes[elements[i].emap[6]].y +
            nodes[elements[i].emap[2]].x * nodes[elements[i].emap[6]].y -
            nodes[elements[i].emap[3]].x * nodes[elements[i].emap[6]].y +
            2 * nodes[elements[i].emap[5]].x *
                (nodes[elements[i].emap[0]].y + nodes[elements[i].emap[1]].y +
                 nodes[elements[i].emap[2]].y + nodes[elements[i].emap[3]].y -
                 4 * nodes[elements[i].emap[7]].y) +
            2 * (nodes[elements[i].emap[0]].x + nodes[elements[i].emap[1]].x +
                 nodes[elements[i].emap[2]].x + nodes[elements[i].emap[3]].x) *
                nodes[elements[i].emap[7]].y) +
       v * v *
           (-(nodes[elements[i].emap[4]].x * nodes[elements[i].emap[0]].y) +
            2 * nodes[elements[i].emap[5]].x * nodes[elements[i].emap[0]].y +
            nodes[elements[i].emap[6]].x * nodes[elements[i].emap[0]].y +
            2 * nodes[elements[i].emap[7]].x * nodes[elements[i].emap[0]].y +
            nodes[elements[i].emap[4]].x * nodes[elements[i].emap[1]].y -
            2 * nodes[elements[i].emap[5]].x * nodes[elements[i].emap[1]].y -
            nodes[elements[i].emap[6]].x * nodes[elements[i].emap[1]].y -
            2 * nodes[elements[i].emap[7]].x * nodes[elements[i].emap[1]].y +
            nodes[elements[i].emap[4]].x * nodes[elements[i].emap[2]].y +
            2 * nodes[elements[i].emap[5]].x * nodes[elements[i].emap[2]].y -
            nodes[elements[i].emap[6]].x * nodes[elements[i].emap[2]].y +
            2 * nodes[elements[i].emap[7]].x * nodes[elements[i].emap[2]].y -
            nodes[elements[i].emap[4]].x * nodes[elements[i].emap[3]].y -
            2 * nodes[elements[i].emap[5]].x * nodes[elements[i].emap[3]].y +
            nodes[elements[i].emap[6]].x * nodes[elements[i].emap[3]].y -
            2 * nodes[elements[i].emap[7]].x * nodes[elements[i].emap[3]].y +
            2 * nodes[elements[i].emap[2]].x *
                (nodes[elements[i].emap[1]].y + nodes[elements[i].emap[3]].y) -
            nodes[elements[i].emap[2]].x * nodes[elements[i].emap[4]].y +
            2 * nodes[elements[i].emap[5]].x * nodes[elements[i].emap[4]].y -
            2 * nodes[elements[i].emap[7]].x * nodes[elements[i].emap[4]].y -
            2 * nodes[elements[i].emap[2]].x * nodes[elements[i].emap[5]].y -
            2 * nodes[elements[i].emap[4]].x * nodes[elements[i].emap[5]].y +
            2 * nodes[elements[i].emap[6]].x * nodes[elements[i].emap[5]].y +
            nodes[elements[i].emap[2]].x * nodes[elements[i].emap[6]].y -
            2 * nodes[elements[i].emap[5]].x * nodes[elements[i].emap[6]].y +
            2 * nodes[elements[i].emap[7]].x * nodes[elements[i].emap[6]].y +
            nodes[elements[i].emap[0]].x * (2 * nodes[elements[i].emap[1]].y +
                                            2 * nodes[elements[i].emap[3]].y +
                                            nodes[elements[i].emap[4]].y -
                                            2 * nodes[elements[i].emap[5]].y -
                                            nodes[elements[i].emap[6]].y -
                                            2 * nodes[elements[i].emap[7]].y) -
            2 * (nodes[elements[i].emap[2]].x - nodes[elements[i].emap[4]].x +
                 nodes[elements[i].emap[6]].x) *
                nodes[elements[i].emap[7]].y +
            nodes[elements[i].emap[3]].x * (-2 * nodes[elements[i].emap[0]].y -
                                            2 * nodes[elements[i].emap[2]].y +
                                            nodes[elements[i].emap[4]].y +
                                            2 * nodes[elements[i].emap[5]].y -
                                            nodes[elements[i].emap[6]].y +
                                            2 * nodes[elements[i].emap[7]].y) +
            nodes[elements[i].emap[1]].x * (-2 * nodes[elements[i].emap[0]].y -
                                            2 * nodes[elements[i].emap[2]].y -
                                            nodes[elements[i].emap[4]].y +
                                            2 * nodes[elements[i].emap[5]].y +
                                            nodes[elements[i].emap[6]].y +
                                            2 * nodes[elements[i].emap[7]].y)) +
       u * (nodes[elements[i].emap[5]].x * nodes[elements[i].emap[0]].y -
            2 * nodes[elements[i].emap[6]].x * nodes[elements[i].emap[0]].y -
            nodes[elements[i].emap[7]].x * nodes[elements[i].emap[0]].y -
            nodes[elements[i].emap[5]].x * nodes[elements[i].emap[1]].y -
            2 * nodes[elements[i].emap[6]].x * nodes[elements[i].emap[1]].y +
            nodes[elements[i].emap[7]].x * nodes[elements[i].emap[1]].y +
            nodes[elements[i].emap[5]].x * nodes[elements[i].emap[2]].y -
            2 * nodes[elements[i].emap[6]].x * nodes[elements[i].emap[2]].y -
            nodes[elements[i].emap[7]].x * nodes[elements[i].emap[2]].y -
            nodes[elements[i].emap[5]].x * nodes[elements[i].emap[3]].y -
            2 * nodes[elements[i].emap[6]].x * nodes[elements[i].emap[3]].y +
            nodes[elements[i].emap[7]].x * nodes[elements[i].emap[3]].y -
            2 * nodes[elements[i].emap[1]].x * nodes[elements[i].emap[4]].y -
            2 * nodes[elements[i].emap[2]].x * nodes[elements[i].emap[4]].y -
            2 * nodes[elements[i].emap[3]].x * nodes[elements[i].emap[4]].y +
            8 * nodes[elements[i].emap[6]].x * nodes[elements[i].emap[4]].y +
            nodes[elements[i].emap[1]].x * nodes[elements[i].emap[5]].y -
            nodes[elements[i].emap[2]].x * nodes[elements[i].emap[5]].y +
            nodes[elements[i].emap[3]].x * nodes[elements[i].emap[5]].y +
            2 * nodes[elements[i].emap[4]].x *
                (nodes[elements[i].emap[0]].y + nodes[elements[i].emap[1]].y +
                 nodes[elements[i].emap[2]].y + nodes[elements[i].emap[3]].y -
                 4 * nodes[elements[i].emap[6]].y) +
            2 * nodes[elements[i].emap[1]].x * nodes[elements[i].emap[6]].y +
            2 * nodes[elements[i].emap[2]].x * nodes[elements[i].emap[6]].y +
            2 * nodes[elements[i].emap[3]].x * nodes[elements[i].emap[6]].y -
            (nodes[elements[i].emap[1]].x - nodes[elements[i].emap[2]].x +
             nodes[elements[i].emap[3]].x) *
                nodes[elements[i].emap[7]].y +
            nodes[elements[i].emap[0]].x * (-2 * nodes[elements[i].emap[4]].y -
                                            nodes[elements[i].emap[5]].y +
                                            2 * nodes[elements[i].emap[6]].y +
                                            nodes[elements[i].emap[7]].y) +
            v * v * (4 * nodes[elements[i].emap[4]].x *
                         nodes[elements[i].emap[0]].y -
                     3 * nodes[elements[i].emap[5]].x *
                         nodes[elements[i].emap[0]].y -
                     4 * nodes[elements[i].emap[6]].x *
                         nodes[elements[i].emap[0]].y -
                     5 * nodes[elements[i].emap[7]].x *
                         nodes[elements[i].emap[0]].y +
                     4 * nodes[elements[i].emap[4]].x *
                         nodes[elements[i].emap[1]].y -
                     5 * nodes[elements[i].emap[5]].x *
                         nodes[elements[i].emap[1]].y -
                     4 * nodes[elements[i].emap[6]].x *
                         nodes[elements[i].emap[1]].y -
                     3 * nodes[elements[i].emap[7]].x *
                         nodes[elements[i].emap[1]].y +
                     4 * nodes[elements[i].emap[4]].x *
                         nodes[elements[i].emap[2]].y +
                     5 * nodes[elements[i].emap[5]].x *
                         nodes[elements[i].emap[2]].y -
                     4 * nodes[elements[i].emap[6]].x *
                         nodes[elements[i].emap[2]].y +
                     3 * nodes[elements[i].emap[7]].x *
                         nodes[elements[i].emap[2]].y +
                     4 * nodes[elements[i].emap[4]].x *
                         nodes[elements[i].emap[3]].y +
                     3 * nodes[elements[i].emap[5]].x *
                         nodes[elements[i].emap[3]].y -
                     4 * nodes[elements[i].emap[6]].x *
                         nodes[elements[i].emap[3]].y +
                     5 * nodes[elements[i].emap[7]].x *
                         nodes[elements[i].emap[3]].y +
                     8 * nodes[elements[i].emap[5]].x *
                         nodes[elements[i].emap[4]].y +
                     8 * nodes[elements[i].emap[7]].x *
                         nodes[elements[i].emap[4]].y -
                     8 * nodes[elements[i].emap[4]].x *
                         nodes[elements[i].emap[5]].y +
                     8 * nodes[elements[i].emap[6]].x *
                         nodes[elements[i].emap[5]].y -
                     8 * nodes[elements[i].emap[5]].x *
                         nodes[elements[i].emap[6]].y -
                     8 * nodes[elements[i].emap[7]].x *
                         nodes[elements[i].emap[6]].y +
                     nodes[elements[i].emap[3]].x *
                         (5 * nodes[elements[i].emap[0]].y +
                          3 * nodes[elements[i].emap[1]].y -
                          4 * nodes[elements[i].emap[4]].y -
                          3 * nodes[elements[i].emap[5]].y +
                          4 * nodes[elements[i].emap[6]].y -
                          5 * nodes[elements[i].emap[7]].y) +
                     nodes[elements[i].emap[2]].x *
                         (3 * nodes[elements[i].emap[0]].y +
                          5 * nodes[elements[i].emap[1]].y -
                          4 * nodes[elements[i].emap[4]].y -
                          5 * nodes[elements[i].emap[5]].y +
                          4 * nodes[elements[i].emap[6]].y -
                          3 * nodes[elements[i].emap[7]].y) -
                     8 * nodes[elements[i].emap[4]].x *
                         nodes[elements[i].emap[7]].y +
                     8 * nodes[elements[i].emap[6]].x *
                         nodes[elements[i].emap[7]].y +
                     nodes[elements[i].emap[1]].x *
                         (-5 * nodes[elements[i].emap[2]].y -
                          3 * nodes[elements[i].emap[3]].y -
                          4 * nodes[elements[i].emap[4]].y +
                          5 * nodes[elements[i].emap[5]].y +
                          4 * nodes[elements[i].emap[6]].y +
                          3 * nodes[elements[i].emap[7]].y) +
                     nodes[elements[i].emap[0]].x *
                         (-3 * nodes[elements[i].emap[2]].y -
                          5 * nodes[elements[i].emap[3]].y -
                          4 * nodes[elements[i].emap[4]].y +
                          3 * nodes[elements[i].emap[5]].y +
                          4 * nodes[elements[i].emap[6]].y +
                          5 * nodes[elements[i].emap[7]].y)) -
            2 * v *
                (nodes[elements[i].emap[6]].x * nodes[elements[i].emap[0]].y -
                 3 * nodes[elements[i].emap[7]].x *
                     nodes[elements[i].emap[0]].y +
                 nodes[elements[i].emap[6]].x * nodes[elements[i].emap[1]].y -
                 nodes[elements[i].emap[7]].x * nodes[elements[i].emap[1]].y +
                 3 * nodes[elements[i].emap[6]].x *
                     nodes[elements[i].emap[2]].y -
                 nodes[elements[i].emap[7]].x * nodes[elements[i].emap[2]].y +
                 3 * nodes[elements[i].emap[6]].x *
                     nodes[elements[i].emap[3]].y -
                 3 * nodes[elements[i].emap[7]].x *
                     nodes[elements[i].emap[3]].y -
                 3 * nodes[elements[i].emap[0]].x *
                     nodes[elements[i].emap[4]].y -
                 3 * nodes[elements[i].emap[1]].x *
                     nodes[elements[i].emap[4]].y -
                 nodes[elements[i].emap[2]].x * nodes[elements[i].emap[4]].y -
                 nodes[elements[i].emap[3]].x * nodes[elements[i].emap[4]].y +
                 4 * nodes[elements[i].emap[7]].x *
                     nodes[elements[i].emap[4]].y +
                 nodes[elements[i].emap[0]].x * nodes[elements[i].emap[5]].y +
                 3 * nodes[elements[i].emap[1]].x *
                     nodes[elements[i].emap[5]].y +
                 3 * nodes[elements[i].emap[2]].x *
                     nodes[elements[i].emap[5]].y +
                 nodes[elements[i].emap[3]].x * nodes[elements[i].emap[5]].y -
                 4 * nodes[elements[i].emap[6]].x *
                     nodes[elements[i].emap[5]].y -
                 nodes[elements[i].emap[0]].x * nodes[elements[i].emap[6]].y -
                 nodes[elements[i].emap[1]].x * nodes[elements[i].emap[6]].y -
                 3 * nodes[elements[i].emap[2]].x *
                     nodes[elements[i].emap[6]].y -
                 3 * nodes[elements[i].emap[3]].x *
                     nodes[elements[i].emap[6]].y +
                 4 * nodes[elements[i].emap[7]].x *
                     nodes[elements[i].emap[6]].y -
                 nodes[elements[i].emap[5]].x *
                     (nodes[elements[i].emap[0]].y +
                      3 * nodes[elements[i].emap[1]].y +
                      3 * nodes[elements[i].emap[2]].y +
                      nodes[elements[i].emap[3]].y -
                      4 * (nodes[elements[i].emap[4]].y +
                           nodes[elements[i].emap[6]].y)) +
                 (3 * nodes[elements[i].emap[0]].x +
                  nodes[elements[i].emap[1]].x + nodes[elements[i].emap[2]].x +
                  3 * nodes[elements[i].emap[3]].x -
                  4 * nodes[elements[i].emap[6]].x) *
                     nodes[elements[i].emap[7]].y +
                 nodes[elements[i].emap[4]].x *
                     (3 * nodes[elements[i].emap[0]].y +
                      3 * nodes[elements[i].emap[1]].y +
                      nodes[elements[i].emap[2]].y +
                      nodes[elements[i].emap[3]].y -
                      4 * (nodes[elements[i].emap[5]].y +
                           nodes[elements[i].emap[7]].y)))) +
       u * u *
           (2 * nodes[elements[i].emap[3]].x * nodes[elements[i].emap[0]].y -
            2 * nodes[elements[i].emap[4]].x * nodes[elements[i].emap[0]].y -
            nodes[elements[i].emap[5]].x * nodes[elements[i].emap[0]].y -
            2 * nodes[elements[i].emap[6]].x * nodes[elements[i].emap[0]].y +
            nodes[elements[i].emap[7]].x * nodes[elements[i].emap[0]].y -
            2 * nodes[elements[i].emap[0]].x * nodes[elements[i].emap[1]].y +
            2 * nodes[elements[i].emap[4]].x * nodes[elements[i].emap[1]].y -
            nodes[elements[i].emap[5]].x * nodes[elements[i].emap[1]].y +
            2 * nodes[elements[i].emap[6]].x * nodes[elements[i].emap[1]].y +
            nodes[elements[i].emap[7]].x * nodes[elements[i].emap[1]].y +
            2 * nodes[elements[i].emap[3]].x * nodes[elements[i].emap[2]].y -
            2 * nodes[elements[i].emap[4]].x * nodes[elements[i].emap[2]].y +
            nodes[elements[i].emap[5]].x * nodes[elements[i].emap[2]].y -
            2 * nodes[elements[i].emap[6]].x * nodes[elements[i].emap[2]].y -
            nodes[elements[i].emap[7]].x * nodes[elements[i].emap[2]].y +
            2 * nodes[elements[i].emap[4]].x * nodes[elements[i].emap[3]].y +
            nodes[elements[i].emap[5]].x * nodes[elements[i].emap[3]].y +
            2 * nodes[elements[i].emap[6]].x * nodes[elements[i].emap[3]].y -
            nodes[elements[i].emap[7]].x * nodes[elements[i].emap[3]].y -
            2 * nodes[elements[i].emap[3]].x * nodes[elements[i].emap[4]].y +
            2 * nodes[elements[i].emap[5]].x * nodes[elements[i].emap[4]].y -
            2 * nodes[elements[i].emap[7]].x * nodes[elements[i].emap[4]].y -
            nodes[elements[i].emap[3]].x * nodes[elements[i].emap[5]].y -
            2 * nodes[elements[i].emap[4]].x * nodes[elements[i].emap[5]].y +
            2 * nodes[elements[i].emap[6]].x * nodes[elements[i].emap[5]].y -
            2 * nodes[elements[i].emap[3]].x * nodes[elements[i].emap[6]].y -
            2 * nodes[elements[i].emap[5]].x * nodes[elements[i].emap[6]].y +
            2 * nodes[elements[i].emap[7]].x * nodes[elements[i].emap[6]].y +
            nodes[elements[i].emap[0]].x * (-2 * nodes[elements[i].emap[3]].y +
                                            2 * nodes[elements[i].emap[4]].y +
                                            nodes[elements[i].emap[5]].y +
                                            2 * nodes[elements[i].emap[6]].y -
                                            nodes[elements[i].emap[7]].y) +
            (nodes[elements[i].emap[3]].x + 2 * nodes[elements[i].emap[4]].x -
             2 * nodes[elements[i].emap[6]].x) *
                nodes[elements[i].emap[7]].y +
            nodes[elements[i].emap[2]].x * (-2 * nodes[elements[i].emap[1]].y -
                                            2 * nodes[elements[i].emap[3]].y +
                                            2 * nodes[elements[i].emap[4]].y -
                                            nodes[elements[i].emap[5]].y +
                                            2 * nodes[elements[i].emap[6]].y +
                                            nodes[elements[i].emap[7]].y) -
            3 * v * v *
                (nodes[elements[i].emap[5]].x * nodes[elements[i].emap[0]].y -
                 nodes[elements[i].emap[6]].x * nodes[elements[i].emap[0]].y -
                 nodes[elements[i].emap[7]].x * nodes[elements[i].emap[0]].y +
                 nodes[elements[i].emap[5]].x * nodes[elements[i].emap[1]].y +
                 nodes[elements[i].emap[6]].x * nodes[elements[i].emap[1]].y -
                 nodes[elements[i].emap[7]].x * nodes[elements[i].emap[1]].y -
                 nodes[elements[i].emap[5]].x * nodes[elements[i].emap[2]].y +
                 nodes[elements[i].emap[6]].x * nodes[elements[i].emap[2]].y +
                 nodes[elements[i].emap[7]].x * nodes[elements[i].emap[2]].y -
                 nodes[elements[i].emap[5]].x * nodes[elements[i].emap[3]].y -
                 nodes[elements[i].emap[6]].x * nodes[elements[i].emap[3]].y +
                 nodes[elements[i].emap[7]].x * nodes[elements[i].emap[3]].y -
                 2 * nodes[elements[i].emap[5]].x *
                     nodes[elements[i].emap[4]].y +
                 2 * nodes[elements[i].emap[7]].x *
                     nodes[elements[i].emap[4]].y -
                 2 * nodes[elements[i].emap[6]].x *
                     nodes[elements[i].emap[5]].y +
                 2 * nodes[elements[i].emap[5]].x *
                     nodes[elements[i].emap[6]].y -
                 2 * nodes[elements[i].emap[7]].x *
                     nodes[elements[i].emap[6]].y +
                 nodes[elements[i].emap[4]].x *
                     (nodes[elements[i].emap[0]].y -
                      nodes[elements[i].emap[1]].y -
                      nodes[elements[i].emap[2]].y +
                      nodes[elements[i].emap[3]].y +
                      2 * nodes[elements[i].emap[5]].y -
                      2 * nodes[elements[i].emap[7]].y) +
                 nodes[elements[i].emap[3]].x * (nodes[elements[i].emap[0]].y -
                                                 nodes[elements[i].emap[2]].y -
                                                 nodes[elements[i].emap[4]].y +
                                                 nodes[elements[i].emap[5]].y +
                                                 nodes[elements[i].emap[6]].y -
                                                 nodes[elements[i].emap[7]].y) +
                 2 * nodes[elements[i].emap[6]].x *
                     nodes[elements[i].emap[7]].y +
                 (nodes[elements[i].emap[0]].x - nodes[elements[i].emap[2]].x) *
                     (nodes[elements[i].emap[1]].y -
                      nodes[elements[i].emap[3]].y -
                      nodes[elements[i].emap[4]].y -
                      nodes[elements[i].emap[5]].y +
                      nodes[elements[i].emap[6]].y +
                      nodes[elements[i].emap[7]].y)) +
            v * (4 * nodes[elements[i].emap[5]].x *
                     nodes[elements[i].emap[0]].y +
                 3 * nodes[elements[i].emap[6]].x *
                     nodes[elements[i].emap[0]].y -
                 4 * nodes[elements[i].emap[7]].x *
                     nodes[elements[i].emap[0]].y +
                 4 * nodes[elements[i].emap[5]].x *
                     nodes[elements[i].emap[1]].y -
                 3 * nodes[elements[i].emap[6]].x *
                     nodes[elements[i].emap[1]].y -
                 4 * nodes[elements[i].emap[7]].x *
                     nodes[elements[i].emap[1]].y +
                 4 * nodes[elements[i].emap[5]].x *
                     nodes[elements[i].emap[2]].y -
                 5 * nodes[elements[i].emap[6]].x *
                     nodes[elements[i].emap[2]].y -
                 4 * nodes[elements[i].emap[7]].x *
                     nodes[elements[i].emap[2]].y +
                 4 * nodes[elements[i].emap[5]].x *
                     nodes[elements[i].emap[3]].y +
                 5 * nodes[elements[i].emap[6]].x *
                     nodes[elements[i].emap[3]].y -
                 4 * nodes[elements[i].emap[7]].x *
                     nodes[elements[i].emap[3]].y -
                 8 * nodes[elements[i].emap[5]].x *
                     nodes[elements[i].emap[4]].y +
                 8 * nodes[elements[i].emap[7]].x *
                     nodes[elements[i].emap[4]].y +
                 8 * nodes[elements[i].emap[6]].x *
                     nodes[elements[i].emap[5]].y -
                 8 * nodes[elements[i].emap[5]].x *
                     nodes[elements[i].emap[6]].y +
                 8 * nodes[elements[i].emap[7]].x *
                     nodes[elements[i].emap[6]].y +
                 nodes[elements[i].emap[4]].x *
                     (5 * nodes[elements[i].emap[0]].y -
                      5 * nodes[elements[i].emap[1]].y -
                      3 * nodes[elements[i].emap[2]].y +
                      3 * nodes[elements[i].emap[3]].y +
                      8 * nodes[elements[i].emap[5]].y -
                      8 * nodes[elements[i].emap[7]].y) -
                 8 * nodes[elements[i].emap[6]].x *
                     nodes[elements[i].emap[7]].y +
                 nodes[elements[i].emap[3]].x *
                     (3 * nodes[elements[i].emap[1]].y +
                      5 * nodes[elements[i].emap[2]].y -
                      3 * nodes[elements[i].emap[4]].y -
                      4 * nodes[elements[i].emap[5]].y -
                      5 * nodes[elements[i].emap[6]].y +
                      4 * nodes[elements[i].emap[7]].y) +
                 nodes[elements[i].emap[0]].x *
                     (5 * nodes[elements[i].emap[1]].y +
                      3 * nodes[elements[i].emap[2]].y -
                      5 * nodes[elements[i].emap[4]].y -
                      4 * nodes[elements[i].emap[5]].y -
                      3 * nodes[elements[i].emap[6]].y +
                      4 * nodes[elements[i].emap[7]].y) +
                 nodes[elements[i].emap[2]].x *
                     (-3 * nodes[elements[i].emap[0]].y -
                      5 * nodes[elements[i].emap[3]].y +
                      3 * nodes[elements[i].emap[4]].y -
                      4 * nodes[elements[i].emap[5]].y +
                      5 * nodes[elements[i].emap[6]].y +
                      4 * nodes[elements[i].emap[7]].y)) +
            nodes[elements[i].emap[1]].x *
                ((-1 + v) * (-2 + 3 * v) * nodes[elements[i].emap[0]].y +
                 2 * nodes[elements[i].emap[2]].y -
                 2 * nodes[elements[i].emap[4]].y +
                 nodes[elements[i].emap[5]].y -
                 2 * nodes[elements[i].emap[6]].y -
                 nodes[elements[i].emap[7]].y +
                 v * (-3 * nodes[elements[i].emap[3]].y +
                      5 * nodes[elements[i].emap[4]].y -
                      4 * nodes[elements[i].emap[5]].y +
                      3 * nodes[elements[i].emap[6]].y +
                      4 * nodes[elements[i].emap[7]].y -
                      3 * v * (nodes[elements[i].emap[2]].y +
                               nodes[elements[i].emap[4]].y -
                               nodes[elements[i].emap[5]].y -
                               nodes[elements[i].emap[6]].y +
                               nodes[elements[i].emap[7]].y))))) /
      8;
  // Jacobian terms
  jac[0][0] =
      (u * u * (-nodes[elements[i].emap[0]].y - nodes[elements[i].emap[1]].y +
                nodes[elements[i].emap[2]].y + nodes[elements[i].emap[3]].y +
                2 * nodes[elements[i].emap[4]].y -
                2 * nodes[elements[i].emap[6]].y) +
       2 * (-nodes[elements[i].emap[4]].y + nodes[elements[i].emap[6]].y +
            v * (nodes[elements[i].emap[0]].y + nodes[elements[i].emap[1]].y +
                 nodes[elements[i].emap[2]].y + nodes[elements[i].emap[3]].y -
                 2 * nodes[elements[i].emap[5]].y -
                 2 * nodes[elements[i].emap[7]].y)) +
       u * (nodes[elements[i].emap[0]].y -
            2 * v * nodes[elements[i].emap[0]].y -
            nodes[elements[i].emap[1]].y +
            2 * v * nodes[elements[i].emap[1]].y +
            nodes[elements[i].emap[2]].y +
            2 * v * nodes[elements[i].emap[2]].y -
            nodes[elements[i].emap[3]].y -
            2 * v * nodes[elements[i].emap[3]].y -
            4 * v * nodes[elements[i].emap[5]].y +
            4 * v * nodes[elements[i].emap[7]].y)) /
      4;
  jac[0][1] =
      (u * u * (nodes[elements[i].emap[0]].x + nodes[elements[i].emap[1]].x -
                nodes[elements[i].emap[2]].x - nodes[elements[i].emap[3]].x -
                2 * nodes[elements[i].emap[4]].x +
                2 * nodes[elements[i].emap[6]].x) -
       2 * (-nodes[elements[i].emap[4]].x + nodes[elements[i].emap[6]].x +
            v * (nodes[elements[i].emap[0]].x + nodes[elements[i].emap[1]].x +
                 nodes[elements[i].emap[2]].x + nodes[elements[i].emap[3]].x -
                 2 * nodes[elements[i].emap[5]].x -
                 2 * nodes[elements[i].emap[7]].x)) +
       u * ((-1 + 2 * v) * nodes[elements[i].emap[0]].x +
            nodes[elements[i].emap[1]].x -
            2 * v * nodes[elements[i].emap[1]].x -
            nodes[elements[i].emap[2]].x -
            2 * v * nodes[elements[i].emap[2]].x +
            nodes[elements[i].emap[3]].x +
            2 * v * nodes[elements[i].emap[3]].x +
            4 * v * nodes[elements[i].emap[5]].x -
            4 * v * nodes[elements[i].emap[7]].x)) /
      4;
  jac[1][0] =
      (v * (-nodes[elements[i].emap[0]].y + nodes[elements[i].emap[1]].y -
            nodes[elements[i].emap[2]].y + nodes[elements[i].emap[3]].y) -
       2 * nodes[elements[i].emap[5]].y +
       2 * u *
           ((-1 + v) * nodes[elements[i].emap[0]].y +
            (-1 + v) * nodes[elements[i].emap[1]].y -
            nodes[elements[i].emap[2]].y - v * nodes[elements[i].emap[2]].y -
            nodes[elements[i].emap[3]].y - v * nodes[elements[i].emap[3]].y +
            2 * nodes[elements[i].emap[4]].y -
            2 * v * nodes[elements[i].emap[4]].y +
            2 * nodes[elements[i].emap[6]].y +
            2 * v * nodes[elements[i].emap[6]].y) +
       v * v * (nodes[elements[i].emap[0]].y - nodes[elements[i].emap[1]].y -
                nodes[elements[i].emap[2]].y + nodes[elements[i].emap[3]].y +
                2 * nodes[elements[i].emap[5]].y -
                2 * nodes[elements[i].emap[7]].y) +
       2 * nodes[elements[i].emap[7]].y) /
      4;
  jac[1][1] =
      (v * (nodes[elements[i].emap[0]].x - nodes[elements[i].emap[1]].x +
            nodes[elements[i].emap[2]].x - nodes[elements[i].emap[3]].x) +
       2 * u *
           (nodes[elements[i].emap[0]].x - v * nodes[elements[i].emap[0]].x +
            nodes[elements[i].emap[1]].x - v * nodes[elements[i].emap[1]].x +
            nodes[elements[i].emap[2]].x + v * nodes[elements[i].emap[2]].x +
            nodes[elements[i].emap[3]].x + v * nodes[elements[i].emap[3]].x -
            2 * nodes[elements[i].emap[4]].x +
            2 * v * nodes[elements[i].emap[4]].x -
            2 * nodes[elements[i].emap[6]].x -
            2 * v * nodes[elements[i].emap[6]].x) +
       2 * (nodes[elements[i].emap[5]].x - nodes[elements[i].emap[7]].x) +
       v * v * (-nodes[elements[i].emap[0]].x + nodes[elements[i].emap[1]].x +
                nodes[elements[i].emap[2]].x - nodes[elements[i].emap[3]].x -
                2 * nodes[elements[i].emap[5]].x +
                2 * nodes[elements[i].emap[7]].x)) /
      4;
}

void ComponentFieldMap::Jacobian13(int i, double t, double u, double v,
                                   double w, double& det, double jac[4][4]) {

  // Initial values
  det = 0;
  for (int j = 0; j < 4; ++j) {
    for (int k = 0; k < 4; ++k) jac[j][k] = 0;
  }

  // Be sure that the element is within range
  if (i < 0 || i >= nElements) {
    std::cerr << m_className << "::Jacobian13:\n";
    std::cerr << "    Element " << i << " out of range.\n";
    return;
  }

  // Determinant of the quadrilateral serendipity Jacobian
  det =
      -(((-4 * v * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[1]].x +
          4 * u * nodes[elements[i].emap[1]].x + nodes[elements[i].emap[3]].x -
          4 * w * nodes[elements[i].emap[3]].x +
          4 * t * nodes[elements[i].emap[4]].x -
          4 * t * nodes[elements[i].emap[6]].x +
          4 * v * nodes[elements[i].emap[7]].x -
          4 * u * nodes[elements[i].emap[8]].x +
          4 * w * nodes[elements[i].emap[8]].x) *
             (4 * w * nodes[elements[i].emap[9]].y -
              nodes[elements[i].emap[2]].y +
              4 * v * nodes[elements[i].emap[2]].y +
              4 * t * nodes[elements[i].emap[5]].y +
              4 * u * nodes[elements[i].emap[7]].y) +
         (nodes[elements[i].emap[1]].x - 4 * u * nodes[elements[i].emap[1]].x -
          nodes[elements[i].emap[2]].x + 4 * v * nodes[elements[i].emap[2]].x -
          4 * t * nodes[elements[i].emap[4]].x +
          4 * t * nodes[elements[i].emap[5]].x +
          4 * u * nodes[elements[i].emap[7]].x -
          4 * v * nodes[elements[i].emap[7]].x +
          4 * w *
              (nodes[elements[i].emap[9]].x - nodes[elements[i].emap[8]].x)) *
             (4 * v * nodes[elements[i].emap[9]].y -
              nodes[elements[i].emap[3]].y +
              4 * w * nodes[elements[i].emap[3]].y +
              4 * t * nodes[elements[i].emap[6]].y +
              4 * u * nodes[elements[i].emap[8]].y) +
         (-4 * w * nodes[elements[i].emap[9]].x +
          4 * v *
              (nodes[elements[i].emap[9]].x - nodes[elements[i].emap[2]].x) +
          nodes[elements[i].emap[2]].x - nodes[elements[i].emap[3]].x +
          4 * w * nodes[elements[i].emap[3]].x -
          4 * t * nodes[elements[i].emap[5]].x +
          4 * t * nodes[elements[i].emap[6]].x -
          4 * u * nodes[elements[i].emap[7]].x +
          4 * u * nodes[elements[i].emap[8]].x) *
             ((-1 + 4 * u) * nodes[elements[i].emap[1]].y +
              4 * (t * nodes[elements[i].emap[4]].y +
                   v * nodes[elements[i].emap[7]].y +
                   w * nodes[elements[i].emap[8]].y))) *
        ((-1 + 4 * t) * nodes[elements[i].emap[0]].z +
         4 * (u * nodes[elements[i].emap[4]].z +
              v * nodes[elements[i].emap[5]].z +
              w * nodes[elements[i].emap[6]].z))) -
      ((nodes[elements[i].emap[1]].x - 4 * u * nodes[elements[i].emap[1]].x -
        nodes[elements[i].emap[3]].x + 4 * w * nodes[elements[i].emap[3]].x -
        4 * t * nodes[elements[i].emap[4]].x +
        4 * t * nodes[elements[i].emap[6]].x +
        4 * v * (nodes[elements[i].emap[9]].x - nodes[elements[i].emap[7]].x) +
        4 * u * nodes[elements[i].emap[8]].x -
        4 * w * nodes[elements[i].emap[8]].x) *
           ((-1 + 4 * t) * nodes[elements[i].emap[0]].y +
            4 * (u * nodes[elements[i].emap[4]].y +
                 v * nodes[elements[i].emap[5]].y +
                 w * nodes[elements[i].emap[6]].y)) -
       ((-1 + 4 * t) * nodes[elements[i].emap[0]].x +
        nodes[elements[i].emap[1]].x - 4 * u * nodes[elements[i].emap[1]].x +
        4 * (-(t * nodes[elements[i].emap[4]].x) +
             u * nodes[elements[i].emap[4]].x +
             v * nodes[elements[i].emap[5]].x +
             w * nodes[elements[i].emap[6]].x -
             v * nodes[elements[i].emap[7]].x -
             w * nodes[elements[i].emap[8]].x)) *
           (4 * v * nodes[elements[i].emap[9]].y -
            nodes[elements[i].emap[3]].y +
            4 * w * nodes[elements[i].emap[3]].y +
            4 * t * nodes[elements[i].emap[6]].y +
            4 * u * nodes[elements[i].emap[8]].y) +
       ((-1 + 4 * t) * nodes[elements[i].emap[0]].x -
        4 * v * nodes[elements[i].emap[9]].x + nodes[elements[i].emap[3]].x -
        4 * w * nodes[elements[i].emap[3]].x +
        4 * u * nodes[elements[i].emap[4]].x +
        4 * v * nodes[elements[i].emap[5]].x -
        4 * t * nodes[elements[i].emap[6]].x +
        4 * w * nodes[elements[i].emap[6]].x -
        4 * u * nodes[elements[i].emap[8]].x) *
           ((-1 + 4 * u) * nodes[elements[i].emap[1]].y +
            4 * (t * nodes[elements[i].emap[4]].y +
                 v * nodes[elements[i].emap[7]].y +
                 w * nodes[elements[i].emap[8]].y))) *
          (4 * w * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[2]].z +
           4 * v * nodes[elements[i].emap[2]].z +
           4 * t * nodes[elements[i].emap[5]].z +
           4 * u * nodes[elements[i].emap[7]].z) +
      ((nodes[elements[i].emap[1]].x - 4 * u * nodes[elements[i].emap[1]].x -
        nodes[elements[i].emap[2]].x + 4 * v * nodes[elements[i].emap[2]].x -
        4 * t * nodes[elements[i].emap[4]].x +
        4 * t * nodes[elements[i].emap[5]].x +
        4 * u * nodes[elements[i].emap[7]].x -
        4 * v * nodes[elements[i].emap[7]].x +
        4 * w * (nodes[elements[i].emap[9]].x - nodes[elements[i].emap[8]].x)) *
           ((-1 + 4 * t) * nodes[elements[i].emap[0]].y +
            4 * (u * nodes[elements[i].emap[4]].y +
                 v * nodes[elements[i].emap[5]].y +
                 w * nodes[elements[i].emap[6]].y)) -
       ((-1 + 4 * t) * nodes[elements[i].emap[0]].x +
        nodes[elements[i].emap[1]].x - 4 * u * nodes[elements[i].emap[1]].x +
        4 * (-(t * nodes[elements[i].emap[4]].x) +
             u * nodes[elements[i].emap[4]].x +
             v * nodes[elements[i].emap[5]].x +
             w * nodes[elements[i].emap[6]].x -
             v * nodes[elements[i].emap[7]].x -
             w * nodes[elements[i].emap[8]].x)) *
           (4 * w * nodes[elements[i].emap[9]].y -
            nodes[elements[i].emap[2]].y +
            4 * v * nodes[elements[i].emap[2]].y +
            4 * t * nodes[elements[i].emap[5]].y +
            4 * u * nodes[elements[i].emap[7]].y) +
       ((-1 + 4 * t) * nodes[elements[i].emap[0]].x -
        4 * w * nodes[elements[i].emap[9]].x + nodes[elements[i].emap[2]].x -
        4 * v * nodes[elements[i].emap[2]].x +
        4 * u * nodes[elements[i].emap[4]].x -
        4 * t * nodes[elements[i].emap[5]].x +
        4 * v * nodes[elements[i].emap[5]].x +
        4 * w * nodes[elements[i].emap[6]].x -
        4 * u * nodes[elements[i].emap[7]].x) *
           ((-1 + 4 * u) * nodes[elements[i].emap[1]].y +
            4 * (t * nodes[elements[i].emap[4]].y +
                 v * nodes[elements[i].emap[7]].y +
                 w * nodes[elements[i].emap[8]].y))) *
          (4 * v * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[3]].z +
           4 * w * nodes[elements[i].emap[3]].z +
           4 * t * nodes[elements[i].emap[6]].z +
           4 * u * nodes[elements[i].emap[8]].z) +
      ((-4 * w * nodes[elements[i].emap[9]].x +
        4 * v * (nodes[elements[i].emap[9]].x - nodes[elements[i].emap[2]].x) +
        nodes[elements[i].emap[2]].x - nodes[elements[i].emap[3]].x +
        4 * w * nodes[elements[i].emap[3]].x -
        4 * t * nodes[elements[i].emap[5]].x +
        4 * t * nodes[elements[i].emap[6]].x -
        4 * u * nodes[elements[i].emap[7]].x +
        4 * u * nodes[elements[i].emap[8]].x) *
           ((-1 + 4 * t) * nodes[elements[i].emap[0]].y +
            4 * (u * nodes[elements[i].emap[4]].y +
                 v * nodes[elements[i].emap[5]].y +
                 w * nodes[elements[i].emap[6]].y)) +
       ((-1 + 4 * t) * nodes[elements[i].emap[0]].x -
        4 * v * nodes[elements[i].emap[9]].x + nodes[elements[i].emap[3]].x -
        4 * w * nodes[elements[i].emap[3]].x +
        4 * u * nodes[elements[i].emap[4]].x +
        4 * v * nodes[elements[i].emap[5]].x -
        4 * t * nodes[elements[i].emap[6]].x +
        4 * w * nodes[elements[i].emap[6]].x -
        4 * u * nodes[elements[i].emap[8]].x) *
           (4 * w * nodes[elements[i].emap[9]].y -
            nodes[elements[i].emap[2]].y +
            4 * v * nodes[elements[i].emap[2]].y +
            4 * t * nodes[elements[i].emap[5]].y +
            4 * u * nodes[elements[i].emap[7]].y) -
       ((-1 + 4 * t) * nodes[elements[i].emap[0]].x -
        4 * w * nodes[elements[i].emap[9]].x + nodes[elements[i].emap[2]].x -
        4 * v * nodes[elements[i].emap[2]].x +
        4 * u * nodes[elements[i].emap[4]].x -
        4 * t * nodes[elements[i].emap[5]].x +
        4 * v * nodes[elements[i].emap[5]].x +
        4 * w * nodes[elements[i].emap[6]].x -
        4 * u * nodes[elements[i].emap[7]].x) *
           (4 * v * nodes[elements[i].emap[9]].y -
            nodes[elements[i].emap[3]].y +
            4 * w * nodes[elements[i].emap[3]].y +
            4 * t * nodes[elements[i].emap[6]].y +
            4 * u * nodes[elements[i].emap[8]].y)) *
          ((-1 + 4 * u) * nodes[elements[i].emap[1]].z +
           4 * (t * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[7]].z +
                w * nodes[elements[i].emap[8]].z));

  jac[0][0] =
      -((((-1 + 4 * u) * nodes[elements[i].emap[1]].x +
          4 * (t * nodes[elements[i].emap[4]].x +
               v * nodes[elements[i].emap[7]].x +
               w * nodes[elements[i].emap[8]].x)) *
             (4 * v * nodes[elements[i].emap[9]].y -
              nodes[elements[i].emap[3]].y +
              4 * w * nodes[elements[i].emap[3]].y +
              4 * t * nodes[elements[i].emap[6]].y +
              4 * u * nodes[elements[i].emap[8]].y) -
         (4 * v * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[3]].x +
          4 * w * nodes[elements[i].emap[3]].x +
          4 * t * nodes[elements[i].emap[6]].x +
          4 * u * nodes[elements[i].emap[8]].x) *
             ((-1 + 4 * u) * nodes[elements[i].emap[1]].y +
              4 * (t * nodes[elements[i].emap[4]].y +
                   v * nodes[elements[i].emap[7]].y +
                   w * nodes[elements[i].emap[8]].y))) *
        (4 * w * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[2]].z +
         4 * v * nodes[elements[i].emap[2]].z +
         4 * t * nodes[elements[i].emap[5]].z +
         4 * u * nodes[elements[i].emap[7]].z)) +
      (((-1 + 4 * u) * nodes[elements[i].emap[1]].x +
        4 * (t * nodes[elements[i].emap[4]].x +
             v * nodes[elements[i].emap[7]].x +
             w * nodes[elements[i].emap[8]].x)) *
           (4 * w * nodes[elements[i].emap[9]].y -
            nodes[elements[i].emap[2]].y +
            4 * v * nodes[elements[i].emap[2]].y +
            4 * t * nodes[elements[i].emap[5]].y +
            4 * u * nodes[elements[i].emap[7]].y) -
       (4 * w * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[2]].x +
        4 * v * nodes[elements[i].emap[2]].x +
        4 * t * nodes[elements[i].emap[5]].x +
        4 * u * nodes[elements[i].emap[7]].x) *
           ((-1 + 4 * u) * nodes[elements[i].emap[1]].y +
            4 * (t * nodes[elements[i].emap[4]].y +
                 v * nodes[elements[i].emap[7]].y +
                 w * nodes[elements[i].emap[8]].y))) *
          (4 * v * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[3]].z +
           4 * w * nodes[elements[i].emap[3]].z +
           4 * t * nodes[elements[i].emap[6]].z +
           4 * u * nodes[elements[i].emap[8]].z) +
      (-((4 * v * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[3]].x +
          4 * w * nodes[elements[i].emap[3]].x +
          4 * t * nodes[elements[i].emap[6]].x +
          4 * u * nodes[elements[i].emap[8]].x) *
         (4 * w * nodes[elements[i].emap[9]].y - nodes[elements[i].emap[2]].y +
          4 * v * nodes[elements[i].emap[2]].y +
          4 * t * nodes[elements[i].emap[5]].y +
          4 * u * nodes[elements[i].emap[7]].y)) +
       (4 * w * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[2]].x +
        4 * v * nodes[elements[i].emap[2]].x +
        4 * t * nodes[elements[i].emap[5]].x +
        4 * u * nodes[elements[i].emap[7]].x) *
           (4 * v * nodes[elements[i].emap[9]].y -
            nodes[elements[i].emap[3]].y +
            4 * w * nodes[elements[i].emap[3]].y +
            4 * t * nodes[elements[i].emap[6]].y +
            4 * u * nodes[elements[i].emap[8]].y)) *
          ((-1 + 4 * u) * nodes[elements[i].emap[1]].z +
           4 * (t * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[7]].z +
                w * nodes[elements[i].emap[8]].z));

  jac[0][1] =
      (nodes[elements[i].emap[1]].y - 4 * u * nodes[elements[i].emap[1]].y -
       nodes[elements[i].emap[3]].y + 4 * w * nodes[elements[i].emap[3]].y -
       4 * t * nodes[elements[i].emap[4]].y +
       4 * t * nodes[elements[i].emap[6]].y +
       4 * v * (nodes[elements[i].emap[9]].y - nodes[elements[i].emap[7]].y) +
       4 * u * nodes[elements[i].emap[8]].y -
       4 * w * nodes[elements[i].emap[8]].y) *
          (4 * w * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[2]].z +
           4 * v * nodes[elements[i].emap[2]].z +
           4 * t * nodes[elements[i].emap[5]].z +
           4 * u * nodes[elements[i].emap[7]].z) +
      (-4 * w * nodes[elements[i].emap[9]].y - nodes[elements[i].emap[1]].y +
       4 * u * nodes[elements[i].emap[1]].y + nodes[elements[i].emap[2]].y -
       4 * v * nodes[elements[i].emap[2]].y +
       4 * t * nodes[elements[i].emap[4]].y -
       4 * t * nodes[elements[i].emap[5]].y -
       4 * u * nodes[elements[i].emap[7]].y +
       4 * v * nodes[elements[i].emap[7]].y +
       4 * w * nodes[elements[i].emap[8]].y) *
          (4 * v * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[3]].z +
           4 * w * nodes[elements[i].emap[3]].z +
           4 * t * nodes[elements[i].emap[6]].z +
           4 * u * nodes[elements[i].emap[8]].z) +
      (-4 * v * nodes[elements[i].emap[9]].y +
       4 * w * nodes[elements[i].emap[9]].y - nodes[elements[i].emap[2]].y +
       4 * v * nodes[elements[i].emap[2]].y + nodes[elements[i].emap[3]].y -
       4 * w * nodes[elements[i].emap[3]].y +
       4 * t * nodes[elements[i].emap[5]].y -
       4 * t * nodes[elements[i].emap[6]].y +
       4 * u * nodes[elements[i].emap[7]].y -
       4 * u * nodes[elements[i].emap[8]].y) *
          ((-1 + 4 * u) * nodes[elements[i].emap[1]].z +
           4 * (t * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[7]].z +
                w * nodes[elements[i].emap[8]].z));

  jac[0][2] =
      (-4 * v * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[1]].x +
       4 * u * nodes[elements[i].emap[1]].x + nodes[elements[i].emap[3]].x -
       4 * w * nodes[elements[i].emap[3]].x +
       4 * t * nodes[elements[i].emap[4]].x -
       4 * t * nodes[elements[i].emap[6]].x +
       4 * v * nodes[elements[i].emap[7]].x -
       4 * u * nodes[elements[i].emap[8]].x +
       4 * w * nodes[elements[i].emap[8]].x) *
          (4 * w * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[2]].z +
           4 * v * nodes[elements[i].emap[2]].z +
           4 * t * nodes[elements[i].emap[5]].z +
           4 * u * nodes[elements[i].emap[7]].z) +
      (nodes[elements[i].emap[1]].x - 4 * u * nodes[elements[i].emap[1]].x -
       nodes[elements[i].emap[2]].x + 4 * v * nodes[elements[i].emap[2]].x -
       4 * t * nodes[elements[i].emap[4]].x +
       4 * t * nodes[elements[i].emap[5]].x +
       4 * u * nodes[elements[i].emap[7]].x -
       4 * v * nodes[elements[i].emap[7]].x +
       4 * w * (nodes[elements[i].emap[9]].x - nodes[elements[i].emap[8]].x)) *
          (4 * v * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[3]].z +
           4 * w * nodes[elements[i].emap[3]].z +
           4 * t * nodes[elements[i].emap[6]].z +
           4 * u * nodes[elements[i].emap[8]].z) +
      (-4 * w * nodes[elements[i].emap[9]].x +
       4 * v * (nodes[elements[i].emap[9]].x - nodes[elements[i].emap[2]].x) +
       nodes[elements[i].emap[2]].x - nodes[elements[i].emap[3]].x +
       4 * w * nodes[elements[i].emap[3]].x -
       4 * t * nodes[elements[i].emap[5]].x +
       4 * t * nodes[elements[i].emap[6]].x -
       4 * u * nodes[elements[i].emap[7]].x +
       4 * u * nodes[elements[i].emap[8]].x) *
          ((-1 + 4 * u) * nodes[elements[i].emap[1]].z +
           4 * (t * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[7]].z +
                w * nodes[elements[i].emap[8]].z));

  jac[0][3] =
      (nodes[elements[i].emap[1]].x - 4 * u * nodes[elements[i].emap[1]].x -
       nodes[elements[i].emap[3]].x + 4 * w * nodes[elements[i].emap[3]].x -
       4 * t * nodes[elements[i].emap[4]].x +
       4 * t * nodes[elements[i].emap[6]].x +
       4 * v * (nodes[elements[i].emap[9]].x - nodes[elements[i].emap[7]].x) +
       4 * u * nodes[elements[i].emap[8]].x -
       4 * w * nodes[elements[i].emap[8]].x) *
          (4 * w * nodes[elements[i].emap[9]].y - nodes[elements[i].emap[2]].y +
           4 * v * nodes[elements[i].emap[2]].y +
           4 * t * nodes[elements[i].emap[5]].y +
           4 * u * nodes[elements[i].emap[7]].y) +
      (-4 * w * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[1]].x +
       4 * u * nodes[elements[i].emap[1]].x + nodes[elements[i].emap[2]].x -
       4 * v * nodes[elements[i].emap[2]].x +
       4 * t * nodes[elements[i].emap[4]].x -
       4 * t * nodes[elements[i].emap[5]].x -
       4 * u * nodes[elements[i].emap[7]].x +
       4 * v * nodes[elements[i].emap[7]].x +
       4 * w * nodes[elements[i].emap[8]].x) *
          (4 * v * nodes[elements[i].emap[9]].y - nodes[elements[i].emap[3]].y +
           4 * w * nodes[elements[i].emap[3]].y +
           4 * t * nodes[elements[i].emap[6]].y +
           4 * u * nodes[elements[i].emap[8]].y) +
      (-4 * v * nodes[elements[i].emap[9]].x +
       4 * w * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[2]].x +
       4 * v * nodes[elements[i].emap[2]].x + nodes[elements[i].emap[3]].x -
       4 * w * nodes[elements[i].emap[3]].x +
       4 * t * nodes[elements[i].emap[5]].x -
       4 * t * nodes[elements[i].emap[6]].x +
       4 * u * nodes[elements[i].emap[7]].x -
       4 * u * nodes[elements[i].emap[8]].x) *
          ((-1 + 4 * u) * nodes[elements[i].emap[1]].y +
           4 * (t * nodes[elements[i].emap[4]].y +
                v * nodes[elements[i].emap[7]].y +
                w * nodes[elements[i].emap[8]].y));

  jac[1][0] =
      -((-((4 * v * nodes[elements[i].emap[9]].x -
            nodes[elements[i].emap[3]].x +
            4 * w * nodes[elements[i].emap[3]].x +
            4 * t * nodes[elements[i].emap[6]].x +
            4 * u * nodes[elements[i].emap[8]].x) *
           (4 * w * nodes[elements[i].emap[9]].y -
            nodes[elements[i].emap[2]].y +
            4 * v * nodes[elements[i].emap[2]].y +
            4 * t * nodes[elements[i].emap[5]].y +
            4 * u * nodes[elements[i].emap[7]].y)) +
         (4 * w * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[2]].x +
          4 * v * nodes[elements[i].emap[2]].x +
          4 * t * nodes[elements[i].emap[5]].x +
          4 * u * nodes[elements[i].emap[7]].x) *
             (4 * v * nodes[elements[i].emap[9]].y -
              nodes[elements[i].emap[3]].y +
              4 * w * nodes[elements[i].emap[3]].y +
              4 * t * nodes[elements[i].emap[6]].y +
              4 * u * nodes[elements[i].emap[8]].y)) *
        ((-1 + 4 * t) * nodes[elements[i].emap[0]].z +
         4 * (u * nodes[elements[i].emap[4]].z +
              v * nodes[elements[i].emap[5]].z +
              w * nodes[elements[i].emap[6]].z))) +
      (-((4 * v * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[3]].x +
          4 * w * nodes[elements[i].emap[3]].x +
          4 * t * nodes[elements[i].emap[6]].x +
          4 * u * nodes[elements[i].emap[8]].x) *
         ((-1 + 4 * t) * nodes[elements[i].emap[0]].y +
          4 * (u * nodes[elements[i].emap[4]].y +
               v * nodes[elements[i].emap[5]].y +
               w * nodes[elements[i].emap[6]].y))) +
       ((-1 + 4 * t) * nodes[elements[i].emap[0]].x +
        4 * (u * nodes[elements[i].emap[4]].x +
             v * nodes[elements[i].emap[5]].x +
             w * nodes[elements[i].emap[6]].x)) *
           (4 * v * nodes[elements[i].emap[9]].y -
            nodes[elements[i].emap[3]].y +
            4 * w * nodes[elements[i].emap[3]].y +
            4 * t * nodes[elements[i].emap[6]].y +
            4 * u * nodes[elements[i].emap[8]].y)) *
          (4 * w * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[2]].z +
           4 * v * nodes[elements[i].emap[2]].z +
           4 * t * nodes[elements[i].emap[5]].z +
           4 * u * nodes[elements[i].emap[7]].z) -
      (-((4 * w * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[2]].x +
          4 * v * nodes[elements[i].emap[2]].x +
          4 * t * nodes[elements[i].emap[5]].x +
          4 * u * nodes[elements[i].emap[7]].x) *
         ((-1 + 4 * t) * nodes[elements[i].emap[0]].y +
          4 * (u * nodes[elements[i].emap[4]].y +
               v * nodes[elements[i].emap[5]].y +
               w * nodes[elements[i].emap[6]].y))) +
       ((-1 + 4 * t) * nodes[elements[i].emap[0]].x +
        4 * (u * nodes[elements[i].emap[4]].x +
             v * nodes[elements[i].emap[5]].x +
             w * nodes[elements[i].emap[6]].x)) *
           (4 * w * nodes[elements[i].emap[9]].y -
            nodes[elements[i].emap[2]].y +
            4 * v * nodes[elements[i].emap[2]].y +
            4 * t * nodes[elements[i].emap[5]].y +
            4 * u * nodes[elements[i].emap[7]].y)) *
          (4 * v * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[3]].z +
           4 * w * nodes[elements[i].emap[3]].z +
           4 * t * nodes[elements[i].emap[6]].z +
           4 * u * nodes[elements[i].emap[8]].z);

  jac[1][1] =
      (-4 * w * nodes[elements[i].emap[9]].y +
       4 * v * (nodes[elements[i].emap[9]].y - nodes[elements[i].emap[2]].y) +
       nodes[elements[i].emap[2]].y - nodes[elements[i].emap[3]].y +
       4 * w * nodes[elements[i].emap[3]].y -
       4 * t * nodes[elements[i].emap[5]].y +
       4 * t * nodes[elements[i].emap[6]].y -
       4 * u * nodes[elements[i].emap[7]].y +
       4 * u * nodes[elements[i].emap[8]].y) *
          ((-1 + 4 * t) * nodes[elements[i].emap[0]].z +
           4 * (u * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[5]].z +
                w * nodes[elements[i].emap[6]].z)) +
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].y -
       4 * v * nodes[elements[i].emap[9]].y + nodes[elements[i].emap[3]].y -
       4 * w * nodes[elements[i].emap[3]].y +
       4 * u * nodes[elements[i].emap[4]].y +
       4 * v * nodes[elements[i].emap[5]].y -
       4 * t * nodes[elements[i].emap[6]].y +
       4 * w * nodes[elements[i].emap[6]].y -
       4 * u * nodes[elements[i].emap[8]].y) *
          (4 * w * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[2]].z +
           4 * v * nodes[elements[i].emap[2]].z +
           4 * t * nodes[elements[i].emap[5]].z +
           4 * u * nodes[elements[i].emap[7]].z) -
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].y -
       4 * w * nodes[elements[i].emap[9]].y + nodes[elements[i].emap[2]].y -
       4 * v * nodes[elements[i].emap[2]].y +
       4 * u * nodes[elements[i].emap[4]].y -
       4 * t * nodes[elements[i].emap[5]].y +
       4 * v * nodes[elements[i].emap[5]].y +
       4 * w * nodes[elements[i].emap[6]].y -
       4 * u * nodes[elements[i].emap[7]].y) *
          (4 * v * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[3]].z +
           4 * w * nodes[elements[i].emap[3]].z +
           4 * t * nodes[elements[i].emap[6]].z +
           4 * u * nodes[elements[i].emap[8]].z);

  jac[1][2] =
      (-4 * v * nodes[elements[i].emap[9]].x +
       4 * w * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[2]].x +
       4 * v * nodes[elements[i].emap[2]].x + nodes[elements[i].emap[3]].x -
       4 * w * nodes[elements[i].emap[3]].x +
       4 * t * nodes[elements[i].emap[5]].x -
       4 * t * nodes[elements[i].emap[6]].x +
       4 * u * nodes[elements[i].emap[7]].x -
       4 * u * nodes[elements[i].emap[8]].x) *
          ((-1 + 4 * t) * nodes[elements[i].emap[0]].z +
           4 * (u * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[5]].z +
                w * nodes[elements[i].emap[6]].z)) -
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].x -
       4 * v * nodes[elements[i].emap[9]].x + nodes[elements[i].emap[3]].x -
       4 * w * nodes[elements[i].emap[3]].x +
       4 * u * nodes[elements[i].emap[4]].x +
       4 * v * nodes[elements[i].emap[5]].x -
       4 * t * nodes[elements[i].emap[6]].x +
       4 * w * nodes[elements[i].emap[6]].x -
       4 * u * nodes[elements[i].emap[8]].x) *
          (4 * w * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[2]].z +
           4 * v * nodes[elements[i].emap[2]].z +
           4 * t * nodes[elements[i].emap[5]].z +
           4 * u * nodes[elements[i].emap[7]].z) +
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].x -
       4 * w * nodes[elements[i].emap[9]].x + nodes[elements[i].emap[2]].x -
       4 * v * nodes[elements[i].emap[2]].x +
       4 * u * nodes[elements[i].emap[4]].x -
       4 * t * nodes[elements[i].emap[5]].x +
       4 * v * nodes[elements[i].emap[5]].x +
       4 * w * nodes[elements[i].emap[6]].x -
       4 * u * nodes[elements[i].emap[7]].x) *
          (4 * v * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[3]].z +
           4 * w * nodes[elements[i].emap[3]].z +
           4 * t * nodes[elements[i].emap[6]].z +
           4 * u * nodes[elements[i].emap[8]].z);

  jac[1][3] =
      (-4 * w * nodes[elements[i].emap[9]].x +
       4 * v * (nodes[elements[i].emap[9]].x - nodes[elements[i].emap[2]].x) +
       nodes[elements[i].emap[2]].x - nodes[elements[i].emap[3]].x +
       4 * w * nodes[elements[i].emap[3]].x -
       4 * t * nodes[elements[i].emap[5]].x +
       4 * t * nodes[elements[i].emap[6]].x -
       4 * u * nodes[elements[i].emap[7]].x +
       4 * u * nodes[elements[i].emap[8]].x) *
          ((-1 + 4 * t) * nodes[elements[i].emap[0]].y +
           4 * (u * nodes[elements[i].emap[4]].y +
                v * nodes[elements[i].emap[5]].y +
                w * nodes[elements[i].emap[6]].y)) +
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].x -
       4 * v * nodes[elements[i].emap[9]].x + nodes[elements[i].emap[3]].x -
       4 * w * nodes[elements[i].emap[3]].x +
       4 * u * nodes[elements[i].emap[4]].x +
       4 * v * nodes[elements[i].emap[5]].x -
       4 * t * nodes[elements[i].emap[6]].x +
       4 * w * nodes[elements[i].emap[6]].x -
       4 * u * nodes[elements[i].emap[8]].x) *
          (4 * w * nodes[elements[i].emap[9]].y - nodes[elements[i].emap[2]].y +
           4 * v * nodes[elements[i].emap[2]].y +
           4 * t * nodes[elements[i].emap[5]].y +
           4 * u * nodes[elements[i].emap[7]].y) -
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].x -
       4 * w * nodes[elements[i].emap[9]].x + nodes[elements[i].emap[2]].x -
       4 * v * nodes[elements[i].emap[2]].x +
       4 * u * nodes[elements[i].emap[4]].x -
       4 * t * nodes[elements[i].emap[5]].x +
       4 * v * nodes[elements[i].emap[5]].x +
       4 * w * nodes[elements[i].emap[6]].x -
       4 * u * nodes[elements[i].emap[7]].x) *
          (4 * v * nodes[elements[i].emap[9]].y - nodes[elements[i].emap[3]].y +
           4 * w * nodes[elements[i].emap[3]].y +
           4 * t * nodes[elements[i].emap[6]].y +
           4 * u * nodes[elements[i].emap[8]].y);

  jac[2][0] =
      (((-1 + 4 * u) * nodes[elements[i].emap[1]].x +
        4 * (t * nodes[elements[i].emap[4]].x +
             v * nodes[elements[i].emap[7]].x +
             w * nodes[elements[i].emap[8]].x)) *
           (4 * v * nodes[elements[i].emap[9]].y -
            nodes[elements[i].emap[3]].y +
            4 * w * nodes[elements[i].emap[3]].y +
            4 * t * nodes[elements[i].emap[6]].y +
            4 * u * nodes[elements[i].emap[8]].y) -
       (4 * v * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[3]].x +
        4 * w * nodes[elements[i].emap[3]].x +
        4 * t * nodes[elements[i].emap[6]].x +
        4 * u * nodes[elements[i].emap[8]].x) *
           ((-1 + 4 * u) * nodes[elements[i].emap[1]].y +
            4 * (t * nodes[elements[i].emap[4]].y +
                 v * nodes[elements[i].emap[7]].y +
                 w * nodes[elements[i].emap[8]].y))) *
          ((-1 + 4 * t) * nodes[elements[i].emap[0]].z +
           4 * (u * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[5]].z +
                w * nodes[elements[i].emap[6]].z)) +
      (-(((-1 + 4 * u) * nodes[elements[i].emap[1]].x +
          4 * (t * nodes[elements[i].emap[4]].x +
               v * nodes[elements[i].emap[7]].x +
               w * nodes[elements[i].emap[8]].x)) *
         ((-1 + 4 * t) * nodes[elements[i].emap[0]].y +
          4 * (u * nodes[elements[i].emap[4]].y +
               v * nodes[elements[i].emap[5]].y +
               w * nodes[elements[i].emap[6]].y))) +
       ((-1 + 4 * t) * nodes[elements[i].emap[0]].x +
        4 * (u * nodes[elements[i].emap[4]].x +
             v * nodes[elements[i].emap[5]].x +
             w * nodes[elements[i].emap[6]].x)) *
           ((-1 + 4 * u) * nodes[elements[i].emap[1]].y +
            4 * (t * nodes[elements[i].emap[4]].y +
                 v * nodes[elements[i].emap[7]].y +
                 w * nodes[elements[i].emap[8]].y))) *
          (4 * v * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[3]].z +
           4 * w * nodes[elements[i].emap[3]].z +
           4 * t * nodes[elements[i].emap[6]].z +
           4 * u * nodes[elements[i].emap[8]].z) -
      (-((4 * v * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[3]].x +
          4 * w * nodes[elements[i].emap[3]].x +
          4 * t * nodes[elements[i].emap[6]].x +
          4 * u * nodes[elements[i].emap[8]].x) *
         ((-1 + 4 * t) * nodes[elements[i].emap[0]].y +
          4 * (u * nodes[elements[i].emap[4]].y +
               v * nodes[elements[i].emap[5]].y +
               w * nodes[elements[i].emap[6]].y))) +
       ((-1 + 4 * t) * nodes[elements[i].emap[0]].x +
        4 * (u * nodes[elements[i].emap[4]].x +
             v * nodes[elements[i].emap[5]].x +
             w * nodes[elements[i].emap[6]].x)) *
           (4 * v * nodes[elements[i].emap[9]].y -
            nodes[elements[i].emap[3]].y +
            4 * w * nodes[elements[i].emap[3]].y +
            4 * t * nodes[elements[i].emap[6]].y +
            4 * u * nodes[elements[i].emap[8]].y)) *
          ((-1 + 4 * u) * nodes[elements[i].emap[1]].z +
           4 * (t * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[7]].z +
                w * nodes[elements[i].emap[8]].z));

  jac[2][1] =
      (-4 * v * nodes[elements[i].emap[9]].y - nodes[elements[i].emap[1]].y +
       4 * u * nodes[elements[i].emap[1]].y + nodes[elements[i].emap[3]].y -
       4 * w * nodes[elements[i].emap[3]].y +
       4 * t * nodes[elements[i].emap[4]].y -
       4 * t * nodes[elements[i].emap[6]].y +
       4 * v * nodes[elements[i].emap[7]].y -
       4 * u * nodes[elements[i].emap[8]].y +
       4 * w * nodes[elements[i].emap[8]].y) *
          ((-1 + 4 * t) * nodes[elements[i].emap[0]].z +
           4 * (u * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[5]].z +
                w * nodes[elements[i].emap[6]].z)) +
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].y +
       nodes[elements[i].emap[1]].y - 4 * u * nodes[elements[i].emap[1]].y +
       4 * (-(t * nodes[elements[i].emap[4]].y) +
            u * nodes[elements[i].emap[4]].y +
            v * nodes[elements[i].emap[5]].y +
            w * nodes[elements[i].emap[6]].y -
            v * nodes[elements[i].emap[7]].y -
            w * nodes[elements[i].emap[8]].y)) *
          (4 * v * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[3]].z +
           4 * w * nodes[elements[i].emap[3]].z +
           4 * t * nodes[elements[i].emap[6]].z +
           4 * u * nodes[elements[i].emap[8]].z) -
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].y -
       4 * v * nodes[elements[i].emap[9]].y + nodes[elements[i].emap[3]].y -
       4 * w * nodes[elements[i].emap[3]].y +
       4 * u * nodes[elements[i].emap[4]].y +
       4 * v * nodes[elements[i].emap[5]].y -
       4 * t * nodes[elements[i].emap[6]].y +
       4 * w * nodes[elements[i].emap[6]].y -
       4 * u * nodes[elements[i].emap[8]].y) *
          ((-1 + 4 * u) * nodes[elements[i].emap[1]].z +
           4 * (t * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[7]].z +
                w * nodes[elements[i].emap[8]].z));

  jac[2][2] =
      (nodes[elements[i].emap[1]].x - 4 * u * nodes[elements[i].emap[1]].x -
       nodes[elements[i].emap[3]].x + 4 * w * nodes[elements[i].emap[3]].x -
       4 * t * nodes[elements[i].emap[4]].x +
       4 * t * nodes[elements[i].emap[6]].x +
       4 * v * (nodes[elements[i].emap[9]].x - nodes[elements[i].emap[7]].x) +
       4 * u * nodes[elements[i].emap[8]].x -
       4 * w * nodes[elements[i].emap[8]].x) *
          ((-1 + 4 * t) * nodes[elements[i].emap[0]].z +
           4 * (u * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[5]].z +
                w * nodes[elements[i].emap[6]].z)) -
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].x +
       nodes[elements[i].emap[1]].x - 4 * u * nodes[elements[i].emap[1]].x +
       4 * (-(t * nodes[elements[i].emap[4]].x) +
            u * nodes[elements[i].emap[4]].x +
            v * nodes[elements[i].emap[5]].x +
            w * nodes[elements[i].emap[6]].x -
            v * nodes[elements[i].emap[7]].x -
            w * nodes[elements[i].emap[8]].x)) *
          (4 * v * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[3]].z +
           4 * w * nodes[elements[i].emap[3]].z +
           4 * t * nodes[elements[i].emap[6]].z +
           4 * u * nodes[elements[i].emap[8]].z) +
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].x -
       4 * v * nodes[elements[i].emap[9]].x + nodes[elements[i].emap[3]].x -
       4 * w * nodes[elements[i].emap[3]].x +
       4 * u * nodes[elements[i].emap[4]].x +
       4 * v * nodes[elements[i].emap[5]].x -
       4 * t * nodes[elements[i].emap[6]].x +
       4 * w * nodes[elements[i].emap[6]].x -
       4 * u * nodes[elements[i].emap[8]].x) *
          ((-1 + 4 * u) * nodes[elements[i].emap[1]].z +
           4 * (t * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[7]].z +
                w * nodes[elements[i].emap[8]].z));

  jac[2][3] =
      (-4 * v * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[1]].x +
       4 * u * nodes[elements[i].emap[1]].x + nodes[elements[i].emap[3]].x -
       4 * w * nodes[elements[i].emap[3]].x +
       4 * t * nodes[elements[i].emap[4]].x -
       4 * t * nodes[elements[i].emap[6]].x +
       4 * v * nodes[elements[i].emap[7]].x -
       4 * u * nodes[elements[i].emap[8]].x +
       4 * w * nodes[elements[i].emap[8]].x) *
          ((-1 + 4 * t) * nodes[elements[i].emap[0]].y +
           4 * (u * nodes[elements[i].emap[4]].y +
                v * nodes[elements[i].emap[5]].y +
                w * nodes[elements[i].emap[6]].y)) +
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].x +
       nodes[elements[i].emap[1]].x - 4 * u * nodes[elements[i].emap[1]].x +
       4 * (-(t * nodes[elements[i].emap[4]].x) +
            u * nodes[elements[i].emap[4]].x +
            v * nodes[elements[i].emap[5]].x +
            w * nodes[elements[i].emap[6]].x -
            v * nodes[elements[i].emap[7]].x -
            w * nodes[elements[i].emap[8]].x)) *
          (4 * v * nodes[elements[i].emap[9]].y - nodes[elements[i].emap[3]].y +
           4 * w * nodes[elements[i].emap[3]].y +
           4 * t * nodes[elements[i].emap[6]].y +
           4 * u * nodes[elements[i].emap[8]].y) -
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].x -
       4 * v * nodes[elements[i].emap[9]].x + nodes[elements[i].emap[3]].x -
       4 * w * nodes[elements[i].emap[3]].x +
       4 * u * nodes[elements[i].emap[4]].x +
       4 * v * nodes[elements[i].emap[5]].x -
       4 * t * nodes[elements[i].emap[6]].x +
       4 * w * nodes[elements[i].emap[6]].x -
       4 * u * nodes[elements[i].emap[8]].x) *
          ((-1 + 4 * u) * nodes[elements[i].emap[1]].y +
           4 * (t * nodes[elements[i].emap[4]].y +
                v * nodes[elements[i].emap[7]].y +
                w * nodes[elements[i].emap[8]].y));

  jac[3][0] =
      -((((-1 + 4 * u) * nodes[elements[i].emap[1]].x +
          4 * (t * nodes[elements[i].emap[4]].x +
               v * nodes[elements[i].emap[7]].x +
               w * nodes[elements[i].emap[8]].x)) *
             (4 * w * nodes[elements[i].emap[9]].y -
              nodes[elements[i].emap[2]].y +
              4 * v * nodes[elements[i].emap[2]].y +
              4 * t * nodes[elements[i].emap[5]].y +
              4 * u * nodes[elements[i].emap[7]].y) -
         (4 * w * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[2]].x +
          4 * v * nodes[elements[i].emap[2]].x +
          4 * t * nodes[elements[i].emap[5]].x +
          4 * u * nodes[elements[i].emap[7]].x) *
             ((-1 + 4 * u) * nodes[elements[i].emap[1]].y +
              4 * (t * nodes[elements[i].emap[4]].y +
                   v * nodes[elements[i].emap[7]].y +
                   w * nodes[elements[i].emap[8]].y))) *
        ((-1 + 4 * t) * nodes[elements[i].emap[0]].z +
         4 * (u * nodes[elements[i].emap[4]].z +
              v * nodes[elements[i].emap[5]].z +
              w * nodes[elements[i].emap[6]].z))) -
      (-(((-1 + 4 * u) * nodes[elements[i].emap[1]].x +
          4 * (t * nodes[elements[i].emap[4]].x +
               v * nodes[elements[i].emap[7]].x +
               w * nodes[elements[i].emap[8]].x)) *
         ((-1 + 4 * t) * nodes[elements[i].emap[0]].y +
          4 * (u * nodes[elements[i].emap[4]].y +
               v * nodes[elements[i].emap[5]].y +
               w * nodes[elements[i].emap[6]].y))) +
       ((-1 + 4 * t) * nodes[elements[i].emap[0]].x +
        4 * (u * nodes[elements[i].emap[4]].x +
             v * nodes[elements[i].emap[5]].x +
             w * nodes[elements[i].emap[6]].x)) *
           ((-1 + 4 * u) * nodes[elements[i].emap[1]].y +
            4 * (t * nodes[elements[i].emap[4]].y +
                 v * nodes[elements[i].emap[7]].y +
                 w * nodes[elements[i].emap[8]].y))) *
          (4 * w * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[2]].z +
           4 * v * nodes[elements[i].emap[2]].z +
           4 * t * nodes[elements[i].emap[5]].z +
           4 * u * nodes[elements[i].emap[7]].z) +
      (-((4 * w * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[2]].x +
          4 * v * nodes[elements[i].emap[2]].x +
          4 * t * nodes[elements[i].emap[5]].x +
          4 * u * nodes[elements[i].emap[7]].x) *
         ((-1 + 4 * t) * nodes[elements[i].emap[0]].y +
          4 * (u * nodes[elements[i].emap[4]].y +
               v * nodes[elements[i].emap[5]].y +
               w * nodes[elements[i].emap[6]].y))) +
       ((-1 + 4 * t) * nodes[elements[i].emap[0]].x +
        4 * (u * nodes[elements[i].emap[4]].x +
             v * nodes[elements[i].emap[5]].x +
             w * nodes[elements[i].emap[6]].x)) *
           (4 * w * nodes[elements[i].emap[9]].y -
            nodes[elements[i].emap[2]].y +
            4 * v * nodes[elements[i].emap[2]].y +
            4 * t * nodes[elements[i].emap[5]].y +
            4 * u * nodes[elements[i].emap[7]].y)) *
          ((-1 + 4 * u) * nodes[elements[i].emap[1]].z +
           4 * (t * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[7]].z +
                w * nodes[elements[i].emap[8]].z));

  jac[3][1] =
      (nodes[elements[i].emap[1]].y - 4 * u * nodes[elements[i].emap[1]].y -
       nodes[elements[i].emap[2]].y + 4 * v * nodes[elements[i].emap[2]].y -
       4 * t * nodes[elements[i].emap[4]].y +
       4 * t * nodes[elements[i].emap[5]].y +
       4 * u * nodes[elements[i].emap[7]].y -
       4 * v * nodes[elements[i].emap[7]].y +
       4 * w * (nodes[elements[i].emap[9]].y - nodes[elements[i].emap[8]].y)) *
          ((-1 + 4 * t) * nodes[elements[i].emap[0]].z +
           4 * (u * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[5]].z +
                w * nodes[elements[i].emap[6]].z)) -
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].y +
       nodes[elements[i].emap[1]].y - 4 * u * nodes[elements[i].emap[1]].y +
       4 * (-(t * nodes[elements[i].emap[4]].y) +
            u * nodes[elements[i].emap[4]].y +
            v * nodes[elements[i].emap[5]].y +
            w * nodes[elements[i].emap[6]].y -
            v * nodes[elements[i].emap[7]].y -
            w * nodes[elements[i].emap[8]].y)) *
          (4 * w * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[2]].z +
           4 * v * nodes[elements[i].emap[2]].z +
           4 * t * nodes[elements[i].emap[5]].z +
           4 * u * nodes[elements[i].emap[7]].z) +
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].y -
       4 * w * nodes[elements[i].emap[9]].y + nodes[elements[i].emap[2]].y -
       4 * v * nodes[elements[i].emap[2]].y +
       4 * u * nodes[elements[i].emap[4]].y -
       4 * t * nodes[elements[i].emap[5]].y +
       4 * v * nodes[elements[i].emap[5]].y +
       4 * w * nodes[elements[i].emap[6]].y -
       4 * u * nodes[elements[i].emap[7]].y) *
          ((-1 + 4 * u) * nodes[elements[i].emap[1]].z +
           4 * (t * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[7]].z +
                w * nodes[elements[i].emap[8]].z));

  jac[3][2] =
      (-4 * w * nodes[elements[i].emap[9]].x - nodes[elements[i].emap[1]].x +
       4 * u * nodes[elements[i].emap[1]].x + nodes[elements[i].emap[2]].x -
       4 * v * nodes[elements[i].emap[2]].x +
       4 * t * nodes[elements[i].emap[4]].x -
       4 * t * nodes[elements[i].emap[5]].x -
       4 * u * nodes[elements[i].emap[7]].x +
       4 * v * nodes[elements[i].emap[7]].x +
       4 * w * nodes[elements[i].emap[8]].x) *
          ((-1 + 4 * t) * nodes[elements[i].emap[0]].z +
           4 * (u * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[5]].z +
                w * nodes[elements[i].emap[6]].z)) +
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].x +
       nodes[elements[i].emap[1]].x - 4 * u * nodes[elements[i].emap[1]].x +
       4 * (-(t * nodes[elements[i].emap[4]].x) +
            u * nodes[elements[i].emap[4]].x +
            v * nodes[elements[i].emap[5]].x +
            w * nodes[elements[i].emap[6]].x -
            v * nodes[elements[i].emap[7]].x -
            w * nodes[elements[i].emap[8]].x)) *
          (4 * w * nodes[elements[i].emap[9]].z - nodes[elements[i].emap[2]].z +
           4 * v * nodes[elements[i].emap[2]].z +
           4 * t * nodes[elements[i].emap[5]].z +
           4 * u * nodes[elements[i].emap[7]].z) -
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].x -
       4 * w * nodes[elements[i].emap[9]].x + nodes[elements[i].emap[2]].x -
       4 * v * nodes[elements[i].emap[2]].x +
       4 * u * nodes[elements[i].emap[4]].x -
       4 * t * nodes[elements[i].emap[5]].x +
       4 * v * nodes[elements[i].emap[5]].x +
       4 * w * nodes[elements[i].emap[6]].x -
       4 * u * nodes[elements[i].emap[7]].x) *
          ((-1 + 4 * u) * nodes[elements[i].emap[1]].z +
           4 * (t * nodes[elements[i].emap[4]].z +
                v * nodes[elements[i].emap[7]].z +
                w * nodes[elements[i].emap[8]].z));

  jac[3][3] =
      (nodes[elements[i].emap[1]].x - 4 * u * nodes[elements[i].emap[1]].x -
       nodes[elements[i].emap[2]].x + 4 * v * nodes[elements[i].emap[2]].x -
       4 * t * nodes[elements[i].emap[4]].x +
       4 * t * nodes[elements[i].emap[5]].x +
       4 * u * nodes[elements[i].emap[7]].x -
       4 * v * nodes[elements[i].emap[7]].x +
       4 * w * (nodes[elements[i].emap[9]].x - nodes[elements[i].emap[8]].x)) *
          ((-1 + 4 * t) * nodes[elements[i].emap[0]].y +
           4 * (u * nodes[elements[i].emap[4]].y +
                v * nodes[elements[i].emap[5]].y +
                w * nodes[elements[i].emap[6]].y)) -
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].x +
       nodes[elements[i].emap[1]].x - 4 * u * nodes[elements[i].emap[1]].x +
       4 * (-(t * nodes[elements[i].emap[4]].x) +
            u * nodes[elements[i].emap[4]].x +
            v * nodes[elements[i].emap[5]].x +
            w * nodes[elements[i].emap[6]].x -
            v * nodes[elements[i].emap[7]].x -
            w * nodes[elements[i].emap[8]].x)) *
          (4 * w * nodes[elements[i].emap[9]].y - nodes[elements[i].emap[2]].y +
           4 * v * nodes[elements[i].emap[2]].y +
           4 * t * nodes[elements[i].emap[5]].y +
           4 * u * nodes[elements[i].emap[7]].y) +
      ((-1 + 4 * t) * nodes[elements[i].emap[0]].x -
       4 * w * nodes[elements[i].emap[9]].x + nodes[elements[i].emap[2]].x -
       4 * v * nodes[elements[i].emap[2]].x +
       4 * u * nodes[elements[i].emap[4]].x -
       4 * t * nodes[elements[i].emap[5]].x +
       4 * v * nodes[elements[i].emap[5]].x +
       4 * w * nodes[elements[i].emap[6]].x -
       4 * u * nodes[elements[i].emap[7]].x) *
          ((-1 + 4 * u) * nodes[elements[i].emap[1]].y +
           4 * (t * nodes[elements[i].emap[4]].y +
                v * nodes[elements[i].emap[7]].y +
                w * nodes[elements[i].emap[8]].y));
}

void ComponentFieldMap::JacobianCube(int element, double t1, double t2,
                                     double t3, TMatrixD*& jac,
                                     std::vector<TMatrixD*>& dN) {
  if (jac == 0) {
    std::cerr << m_className << "::JacobianCube:\n";
    std::cerr << "    Pointer to Jacobian matrix is empty!\n";
    return;
  }
  dN.clear();
  // Be sure that the element is within range
  if (element < 0 || element >= nElements) {
    std::cerr << m_className << "::JacobianCube:\n";
    std::cerr << "    Element " << element << " out of range.\n";
    return;
  }
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
  for (int node = 0; node < 8; node++) {
    (*jac)(0, 0) +=
        nodes[elements[element].emap[node]].x * ((*dN.at(node))(0, 0));
    (*jac)(0, 1) +=
        nodes[elements[element].emap[node]].y * ((*dN.at(node))(0, 0));
    (*jac)(0, 2) +=
        nodes[elements[element].emap[node]].z * ((*dN.at(node))(0, 0));
    (*jac)(1, 0) +=
        nodes[elements[element].emap[node]].x * ((*dN.at(node))(1, 0));
    (*jac)(1, 1) +=
        nodes[elements[element].emap[node]].y * ((*dN.at(node))(1, 0));
    (*jac)(1, 2) +=
        nodes[elements[element].emap[node]].z * ((*dN.at(node))(1, 0));
    (*jac)(2, 0) +=
        nodes[elements[element].emap[node]].x * ((*dN.at(node))(2, 0));
    (*jac)(2, 1) +=
        nodes[elements[element].emap[node]].y * ((*dN.at(node))(2, 0));
    (*jac)(2, 2) +=
        nodes[elements[element].emap[node]].z * ((*dN.at(node))(2, 0));
  }

  // compute determinant
  if (debug) {
    std::cout << m_className << "::JacobianCube:" << std::endl;
    std::cout << "   Det.: " << jac->Determinant() << std::endl;
    std::cout << "   Jacobian matrix.: " << std::endl;
    jac->Print("%11.10g");
    std::cout << "   Hexahedral coordinates (t, u, v) = (" << t1 << "," << t2
              << "," << t3 << ")" << std::endl;
    std::cout << "   Node xyzV" << std::endl;
    for (int node = 0; node < 8; node++) {
      std::cout << "         " << elements[element].emap[node] << "          "
                << nodes[elements[element].emap[node]].x << "         "
                << nodes[elements[element].emap[node]].y << "         "
                << nodes[elements[element].emap[node]].z << "         "
                << nodes[elements[element].emap[node]].v << std::endl;
    }
  }
}

int ComponentFieldMap::Coordinates3(double x, double y, double z, double& t1,
                                    double& t2, double& t3, double& t4,
                                    double jac[4][4], double& det, int imap) {

  if (debug) {
    std::cout << m_className << "::Coordinates3:\n";
    std::cout << "   Point (" << x << ", " << y << ", " << z << ")\n";
  }

  // Failure flag
  int ifail = 1;

  // Provisional values
  t1 = t2 = t3 = t4 = 0;

  // Make a first order approximation, using the linear triangle.
  double tt1 =
      (x - nodes[elements[imap].emap[1]].x) *
          (nodes[elements[imap].emap[2]].y - nodes[elements[imap].emap[1]].y) -
      (y - nodes[elements[imap].emap[1]].y) *
          (nodes[elements[imap].emap[2]].x - nodes[elements[imap].emap[1]].x);
  double tt2 =
      (x - nodes[elements[imap].emap[2]].x) *
          (nodes[elements[imap].emap[0]].y - nodes[elements[imap].emap[2]].y) -
      (y - nodes[elements[imap].emap[2]].y) *
          (nodes[elements[imap].emap[0]].x - nodes[elements[imap].emap[2]].x);
  double tt3 =
      (x - nodes[elements[imap].emap[0]].x) *
          (nodes[elements[imap].emap[1]].y - nodes[elements[imap].emap[0]].y) -
      (y - nodes[elements[imap].emap[0]].y) *
          (nodes[elements[imap].emap[1]].x - nodes[elements[imap].emap[0]].x);
  if ((nodes[elements[imap].emap[0]].x - nodes[elements[imap].emap[1]].x) *
                  (nodes[elements[imap].emap[2]].y -
                   nodes[elements[imap].emap[1]].y) -
              (nodes[elements[imap].emap[2]].x -
               nodes[elements[imap].emap[1]].x) *
                  (nodes[elements[imap].emap[0]].y -
                   nodes[elements[imap].emap[1]].y) ==
          0 ||
      (nodes[elements[imap].emap[1]].x - nodes[elements[imap].emap[2]].x) *
                  (nodes[elements[imap].emap[0]].y -
                   nodes[elements[imap].emap[2]].y) -
              (nodes[elements[imap].emap[0]].x -
               nodes[elements[imap].emap[2]].x) *
                  (nodes[elements[imap].emap[1]].y -
                   nodes[elements[imap].emap[2]].y) ==
          0 ||
      (nodes[elements[imap].emap[2]].x - nodes[elements[imap].emap[0]].x) *
                  (nodes[elements[imap].emap[1]].y -
                   nodes[elements[imap].emap[0]].y) -
              (nodes[elements[imap].emap[1]].x -
               nodes[elements[imap].emap[0]].x) *
                  (nodes[elements[imap].emap[2]].y -
                   nodes[elements[imap].emap[0]].y) ==
          0) {
    std::cerr << m_className << "::Coordinates3:\n";
    std::cerr << "    Calculation of linear coordinates failed; abandoned.\n";
    return ifail;
  } else {
    t1 = tt1 /
         ((nodes[elements[imap].emap[0]].x - nodes[elements[imap].emap[1]].x) *
              (nodes[elements[imap].emap[2]].y -
               nodes[elements[imap].emap[1]].y) -
          (nodes[elements[imap].emap[2]].x - nodes[elements[imap].emap[1]].x) *
              (nodes[elements[imap].emap[0]].y -
               nodes[elements[imap].emap[1]].y));
    t2 = tt2 /
         ((nodes[elements[imap].emap[1]].x - nodes[elements[imap].emap[2]].x) *
              (nodes[elements[imap].emap[0]].y -
               nodes[elements[imap].emap[2]].y) -
          (nodes[elements[imap].emap[0]].x - nodes[elements[imap].emap[2]].x) *
              (nodes[elements[imap].emap[1]].y -
               nodes[elements[imap].emap[2]].y));
    t3 = tt3 /
         ((nodes[elements[imap].emap[2]].x - nodes[elements[imap].emap[0]].x) *
              (nodes[elements[imap].emap[1]].y -
               nodes[elements[imap].emap[0]].y) -
          (nodes[elements[imap].emap[1]].x - nodes[elements[imap].emap[0]].x) *
              (nodes[elements[imap].emap[2]].y -
               nodes[elements[imap].emap[0]].y));
    t4 = 0;
  }

  // Start iterative refinement
  double td1 = t1, td2 = t2, td3 = t3;
  if (debug) {
    std::cout << m_className << "::Coordinates3:\n";
    std::cout << "    Linear estimate:   (u, v, w) = (" << td1 << ", " << td2
              << ", " << td3 << "), sum = " << td1 + td2 + td3 << ".\n";
  }

  // Loop
  bool converged = false;
  double diff[3] = {0., 0., 0.};
  double corr[3] = {0., 0., 0.};
  for (int iter = 0; iter < 10; iter++) {
    if (debug) {
      std::cout << m_className << "::Coordinates3:\n";
      std::cout << "    Iteration " << iter << ":     (u, v, w) = (" << td1
                << ", " << td2 << ", " << td3 << "), sum = " << td1 + td2 + td3
                << "\n";
    }
    // Re-compute the (x,y,z) position for this coordinate.
    double xr = nodes[elements[imap].emap[0]].x * td1 * (2 * td1 - 1) +
                nodes[elements[imap].emap[1]].x * td2 * (2 * td2 - 1) +
                nodes[elements[imap].emap[2]].x * td3 * (2 * td3 - 1) +
                nodes[elements[imap].emap[3]].x * 4 * td1 * td2 +
                nodes[elements[imap].emap[4]].x * 4 * td1 * td3 +
                nodes[elements[imap].emap[5]].x * 4 * td2 * td3;
    double yr = nodes[elements[imap].emap[0]].y * td1 * (2 * td1 - 1) +
                nodes[elements[imap].emap[1]].y * td2 * (2 * td2 - 1) +
                nodes[elements[imap].emap[2]].y * td3 * (2 * td3 - 1) +
                nodes[elements[imap].emap[3]].y * 4 * td1 * td2 +
                nodes[elements[imap].emap[4]].y * 4 * td1 * td3 +
                nodes[elements[imap].emap[5]].y * 4 * td2 * td3;
    double sr = td1 + td2 + td3;
    // Compute the Jacobian
    Jacobian3(imap, td1, td2, td3, det, jac);
    // Compute the difference vector
    diff[0] = 1 - sr;
    diff[1] = x - xr;
    diff[2] = y - yr;
    // Update the estimate
    for (int l = 0; l < 3; l++) {
      corr[l] = 0;
      for (int k = 0; k < 3; k++) {
        corr[l] += jac[l][k] * diff[k] / det;
      }
    }
    // Debugging
    if (debug) {
      std::cout << m_className << "::Coordinates3:\n";
      std::cout << "    Difference vector:  (1, x, y)  = (" << diff[0] << ", "
                << diff[1] << ", " << diff[2] << ").\n";
      std::cout << "    Correction vector:  (u, v, w) = (" << corr[0] << ", "
                << corr[1] << ", " << corr[2] << ").\n";
    }
    // Update the vector.
    td1 += corr[0];
    td2 += corr[1];
    td3 += corr[2];
    // Check for convergence.
    if (fabs(corr[0]) < 1.0e-5 && fabs(corr[1]) < 1.0e-5 &&
        fabs(corr[2]) < 1.0e-5) {
      if (debug) {
        std::cout << m_className << "::Coordinates3:\n";
        std::cout << "    Convergence reached.\n";
      }
      converged = true;
      break;
    }
  }
  // No convergence reached
  if (!converged) {
    double xmin, ymin, xmax, ymax;
    xmin = nodes[elements[imap].emap[0]].x;
    xmax = nodes[elements[imap].emap[0]].x;
    if (nodes[elements[imap].emap[1]].x < xmin) {
      xmin = nodes[elements[imap].emap[1]].x;
    }
    if (nodes[elements[imap].emap[1]].x > xmax) {
      xmax = nodes[elements[imap].emap[1]].x;
    }
    if (nodes[elements[imap].emap[2]].x < xmin) {
      xmin = nodes[elements[imap].emap[2]].x;
    }
    if (nodes[elements[imap].emap[2]].x > xmax) {
      xmax = nodes[elements[imap].emap[2]].x;
    }
    ymin = nodes[elements[imap].emap[0]].y;
    ymax = nodes[elements[imap].emap[0]].y;
    if (nodes[elements[imap].emap[1]].y < ymin) {
      ymin = nodes[elements[imap].emap[1]].y;
    }
    if (nodes[elements[imap].emap[1]].y > ymax) {
      ymax = nodes[elements[imap].emap[1]].y;
    }
    if (nodes[elements[imap].emap[2]].y < ymin) {
      ymin = nodes[elements[imap].emap[2]].y;
    }
    if (nodes[elements[imap].emap[2]].y > ymax) {
      ymax = nodes[elements[imap].emap[2]].y;
    }

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
  if (debug) {
    std::cout << m_className << "::Coordinates3:\n";
    std::cout << "    Convergence reached at (t1, t2, t3) = (" << t1 << ", "
              << t2 << ", " << t3 << ").\n";
  }

  // For debugging purposes, show position
  if (debug) {
    double xr = nodes[elements[imap].emap[0]].x * td1 * (2 * td1 - 1) +
                nodes[elements[imap].emap[1]].x * td2 * (2 * td2 - 1) +
                nodes[elements[imap].emap[2]].x * td3 * (2 * td3 - 1) +
                nodes[elements[imap].emap[3]].x * 4 * td1 * td2 +
                nodes[elements[imap].emap[4]].x * 4 * td1 * td3 +
                nodes[elements[imap].emap[5]].x * 4 * td2 * td3;
    double yr = nodes[elements[imap].emap[0]].y * td1 * (2 * td1 - 1) +
                nodes[elements[imap].emap[1]].y * td2 * (2 * td2 - 1) +
                nodes[elements[imap].emap[2]].y * td3 * (2 * td3 - 1) +
                nodes[elements[imap].emap[3]].y * 4 * td1 * td2 +
                nodes[elements[imap].emap[4]].y * 4 * td1 * td3 +
                nodes[elements[imap].emap[5]].y * 4 * td2 * td3;
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

int ComponentFieldMap::Coordinates4(double x, double y, double z, double& t1,
                                    double& t2, double& t3, double& t4,
                                    double jac[4][4], double& det, int imap) {

  // Debugging
  if (debug) {
    std::cout << m_className << "::Coordinates4:\n";
    std::cout << "   Point (" << x << ", " << y << ", " << z << ")\n";
  }

  // Failure flag
  int ifail = 1;

  // Provisional values
  t1 = t2 = t3 = t4 = 0.;

  // Compute determinant.
  det =
      -(-((nodes[elements[imap].emap[0]].x - nodes[elements[imap].emap[3]].x) *
          (nodes[elements[imap].emap[1]].y - nodes[elements[imap].emap[2]].y)) +
        (nodes[elements[imap].emap[1]].x - nodes[elements[imap].emap[2]].x) *
            (nodes[elements[imap].emap[0]].y -
             nodes[elements[imap].emap[3]].y)) *
          (2 * x * (-nodes[elements[imap].emap[0]].y +
                    nodes[elements[imap].emap[1]].y +
                    nodes[elements[imap].emap[2]].y -
                    nodes[elements[imap].emap[3]].y) -
           (nodes[elements[imap].emap[0]].x + nodes[elements[imap].emap[3]].x) *
               (nodes[elements[imap].emap[1]].y +
                nodes[elements[imap].emap[2]].y - 2 * y) +
           nodes[elements[imap].emap[1]].x *
               (nodes[elements[imap].emap[0]].y +
                nodes[elements[imap].emap[3]].y - 2 * y) +
           nodes[elements[imap].emap[2]].x *
               (nodes[elements[imap].emap[0]].y +
                nodes[elements[imap].emap[3]].y - 2 * y)) +
      pow(-(nodes[elements[imap].emap[0]].x * nodes[elements[imap].emap[1]].y) +
              nodes[elements[imap].emap[3]].x *
                  nodes[elements[imap].emap[2]].y -
              nodes[elements[imap].emap[2]].x *
                  nodes[elements[imap].emap[3]].y +
              x * (-nodes[elements[imap].emap[0]].y +
                   nodes[elements[imap].emap[1]].y -
                   nodes[elements[imap].emap[2]].y +
                   nodes[elements[imap].emap[3]].y) +
              nodes[elements[imap].emap[1]].x *
                  (nodes[elements[imap].emap[0]].y - y) +
              (nodes[elements[imap].emap[0]].x +
               nodes[elements[imap].emap[2]].x -
               nodes[elements[imap].emap[3]].x) *
                  y,
          2);

  // Check that the determinant is non-negative
  // (this can happen if the point is out of range).
  if (det < 0) {
    if (debug) {
      std::cerr << m_className << "::Coordinates4:\n";
      std::cerr << "    No solution found for isoparametric coordinates\n";
      std::cerr << "    in element " << imap << " because the determinant "
                << det << " is < 0.\n";
    }
    return ifail;
  }

  // Vector products for evaluation of T1.
  double prod =
      ((nodes[elements[imap].emap[2]].x - nodes[elements[imap].emap[3]].x) *
           (nodes[elements[imap].emap[0]].y - nodes[elements[imap].emap[1]].y) -
       (nodes[elements[imap].emap[0]].x - nodes[elements[imap].emap[1]].x) *
           (nodes[elements[imap].emap[2]].y - nodes[elements[imap].emap[3]].y));
  if (prod * prod >
      1.0e-12 *
          ((nodes[elements[imap].emap[0]].x - nodes[elements[imap].emap[1]].x) *
               (nodes[elements[imap].emap[0]].x -
                nodes[elements[imap].emap[1]].x) +
           (nodes[elements[imap].emap[0]].y - nodes[elements[imap].emap[1]].y) *
               (nodes[elements[imap].emap[0]].y -
                nodes[elements[imap].emap[1]].y)) *
          ((nodes[elements[imap].emap[2]].x - nodes[elements[imap].emap[3]].x) *
               (nodes[elements[imap].emap[2]].x -
                nodes[elements[imap].emap[3]].x) +
           (nodes[elements[imap].emap[2]].y - nodes[elements[imap].emap[3]].y) *
               (nodes[elements[imap].emap[2]].y -
                nodes[elements[imap].emap[3]].y))) {
    t1 = (-(nodes[elements[imap].emap[3]].x * nodes[elements[imap].emap[0]].y) +
          x * nodes[elements[imap].emap[0]].y +
          nodes[elements[imap].emap[2]].x * nodes[elements[imap].emap[1]].y -
          x * nodes[elements[imap].emap[1]].y -
          nodes[elements[imap].emap[1]].x * nodes[elements[imap].emap[2]].y +
          x * nodes[elements[imap].emap[2]].y +
          nodes[elements[imap].emap[0]].x * nodes[elements[imap].emap[3]].y -
          x * nodes[elements[imap].emap[3]].y -
          nodes[elements[imap].emap[0]].x * y +
          nodes[elements[imap].emap[1]].x * y -
          nodes[elements[imap].emap[2]].x * y +
          nodes[elements[imap].emap[3]].x * y + sqrt(det)) /
         prod;
  } else {
    double xp =
        nodes[elements[imap].emap[0]].y - nodes[elements[imap].emap[1]].y;
    double yp =
        nodes[elements[imap].emap[1]].x - nodes[elements[imap].emap[0]].x;
    double dn = sqrt(xp * xp + yp * yp);
    if (dn <= 0) {
      std::cerr << m_className << "::Coordinates4:\n";
      std::cerr << "    Element " << imap
                << " appears to be degenerate in the 1 - 2 axis.\n";
      return ifail;
    }
    xp = xp / dn;
    yp = yp / dn;
    double dpoint = xp * (x - nodes[elements[imap].emap[0]].x) +
                    yp * (y - nodes[elements[imap].emap[0]].y);
    double dbox = xp * (nodes[elements[imap].emap[3]].x -
                        nodes[elements[imap].emap[0]].x) +
                  yp * (nodes[elements[imap].emap[3]].y -
                        nodes[elements[imap].emap[0]].y);
    if (dbox == 0) {
      std::cerr << m_className << "::Coordinates4:\n";
      std::cerr << "    Element " << imap
                << " appears to be degenerate in the 1 - 3 axis.\n";
      return ifail;
    }
    double t = -1 + 2 * dpoint / dbox;
    double xt1 = nodes[elements[imap].emap[0]].x +
                 0.5 * (t + 1) * (nodes[elements[imap].emap[3]].x -
                                  nodes[elements[imap].emap[0]].x);
    double yt1 = nodes[elements[imap].emap[0]].y +
                 0.5 * (t + 1) * (nodes[elements[imap].emap[3]].y -
                                  nodes[elements[imap].emap[0]].y);
    double xt2 = nodes[elements[imap].emap[1]].x +
                 0.5 * (t + 1) * (nodes[elements[imap].emap[2]].x -
                                  nodes[elements[imap].emap[1]].x);
    double yt2 = nodes[elements[imap].emap[1]].y +
                 0.5 * (t + 1) * (nodes[elements[imap].emap[2]].y -
                                  nodes[elements[imap].emap[1]].y);
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
  prod =
      ((nodes[elements[imap].emap[0]].x - nodes[elements[imap].emap[3]].x) *
           (nodes[elements[imap].emap[1]].y - nodes[elements[imap].emap[2]].y) -
       (nodes[elements[imap].emap[1]].x - nodes[elements[imap].emap[2]].x) *
           (nodes[elements[imap].emap[0]].y - nodes[elements[imap].emap[3]].y));
  if (prod * prod >
      1.0e-12 *
          ((nodes[elements[imap].emap[0]].x - nodes[elements[imap].emap[3]].x) *
               (nodes[elements[imap].emap[0]].x -
                nodes[elements[imap].emap[3]].x) +
           (nodes[elements[imap].emap[0]].y - nodes[elements[imap].emap[3]].y) *
               (nodes[elements[imap].emap[0]].y -
                nodes[elements[imap].emap[3]].y)) *
          ((nodes[elements[imap].emap[1]].x - nodes[elements[imap].emap[2]].x) *
               (nodes[elements[imap].emap[1]].x -
                nodes[elements[imap].emap[2]].x) +
           (nodes[elements[imap].emap[1]].y - nodes[elements[imap].emap[2]].y) *
               (nodes[elements[imap].emap[1]].y -
                nodes[elements[imap].emap[2]].y))) {
    t2 = (-(nodes[elements[imap].emap[1]].x * nodes[elements[imap].emap[0]].y) +
          x * nodes[elements[imap].emap[0]].y +
          nodes[elements[imap].emap[0]].x * nodes[elements[imap].emap[1]].y -
          x * nodes[elements[imap].emap[1]].y -
          nodes[elements[imap].emap[3]].x * nodes[elements[imap].emap[2]].y +
          x * nodes[elements[imap].emap[2]].y +
          nodes[elements[imap].emap[2]].x * nodes[elements[imap].emap[3]].y -
          x * nodes[elements[imap].emap[3]].y -
          nodes[elements[imap].emap[0]].x * y +
          nodes[elements[imap].emap[1]].x * y -
          nodes[elements[imap].emap[2]].x * y +
          nodes[elements[imap].emap[3]].x * y - sqrt(det)) /
         prod;
  } else {
    double xp =
        nodes[elements[imap].emap[0]].y - nodes[elements[imap].emap[3]].y;
    double yp =
        nodes[elements[imap].emap[3]].x - nodes[elements[imap].emap[0]].x;
    double dn = sqrt(xp * xp + yp * yp);
    if (dn <= 0) {
      std::cerr << m_className << "Coordinates4:\n";
      std::cerr << "    Element " << imap
                << " appears to be degenerate in the 1 - 4 axis.\n";
      return ifail;
    }
    xp = xp / dn;
    yp = yp / dn;
    double dpoint = xp * (x - nodes[elements[imap].emap[0]].x) +
                    yp * (y - nodes[elements[imap].emap[0]].y);
    double dbox = xp * (nodes[elements[imap].emap[1]].x -
                        nodes[elements[imap].emap[0]].x) +
                  yp * (nodes[elements[imap].emap[1]].y -
                        nodes[elements[imap].emap[0]].y);
    if (dbox == 0) {
      std::cerr << m_className << "::Coordinates4:\n";
      std::cerr << "    Element " << imap
                << " appears to be degenerate in the 1 - 2 axis.\n";
      return ifail;
    }
    double t = -1 + 2 * dpoint / dbox;
    double xt1 = nodes[elements[imap].emap[0]].x +
                 0.5 * (t + 1) * (nodes[elements[imap].emap[1]].x -
                                  nodes[elements[imap].emap[0]].x);
    double yt1 = nodes[elements[imap].emap[0]].y +
                 0.5 * (t + 1) * (nodes[elements[imap].emap[1]].y -
                                  nodes[elements[imap].emap[0]].y);
    double xt2 = nodes[elements[imap].emap[3]].x +
                 0.5 * (t + 1) * (nodes[elements[imap].emap[2]].x -
                                  nodes[elements[imap].emap[3]].x);
    double yt2 = nodes[elements[imap].emap[3]].y +
                 0.5 * (t + 1) * (nodes[elements[imap].emap[2]].y -
                                  nodes[elements[imap].emap[3]].y);
    dn = (xt1 - xt2) * (xt1 - xt2) + (yt1 - yt2) * (yt1 - yt2);
    if (dn <= 0) {
      std::cout << m_className << "::Coordinates4:\n";
      std::cout << "    Coordinate requested at convergence point of element "
                << imap << ".\n";
      return ifail;
    }
    t2 = -1 + 2 * ((x - xt1) * (xt2 - xt1) + (y - yt1) * (yt2 - yt1)) / dn;
  }
  if (debug) {
    std::cout << m_className << "::Coordinates4:\n";
    std::cout << "    Isoparametric (u, v):   (" << t1 << ", " << t2 << ").\n";
  }

  // Re-compute the (x,y,z) position for this coordinate.
  if (debug) {
    double xr = nodes[elements[imap].emap[0]].x * (1 - t1) * (1 - t2) / 4 +
                nodes[elements[imap].emap[1]].x * (1 + t1) * (1 - t2) / 4 +
                nodes[elements[imap].emap[2]].x * (1 + t1) * (1 + t2) / 4 +
                nodes[elements[imap].emap[3]].x * (1 - t1) * (1 + t2) / 4;
    double yr = nodes[elements[imap].emap[0]].y * (1 - t1) * (1 - t2) / 4 +
                nodes[elements[imap].emap[1]].y * (1 + t1) * (1 - t2) / 4 +
                nodes[elements[imap].emap[2]].y * (1 + t1) * (1 + t2) / 4 +
                nodes[elements[imap].emap[3]].y * (1 - t1) * (1 + t2) / 4;
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

int ComponentFieldMap::Coordinates5(double x, double y, double z, double& t1,
                                    double& t2, double& t3, double& t4,
                                    double jac[4][4], double& det, int imap) {

  // Debugging
  if (debug) {
    std::cout << m_className << "::Coordinates5:\n";
    std::cout << "   Point (" << x << ", " << y << ", " << z << ")\n";
  }

  // Failure flag
  int ifail = 1;

  // Provisional values
  t1 = t2 = t3 = t4 = 0;

  // Degenerate elements should have been treated as triangles.
  if (elements[imap].degenerate) {
    std::cerr << m_className << "::Coordinates5:\n";
    std::cerr << "    Received degenerate element " << imap << ".\n";
    return ifail;
  }

  // Set tolerance parameter.
  double f = 0.5;

  // Make a first order approximation.
  int rc = Coordinates4(x, y, z, t1, t2, t3, t4, jac, det, imap);
  if (rc > 0) {
    if (debug) {
      std::cout << m_className << "::Coordinates5:\n";
      std::cout << "    Failure to obtain linear estimate of isoparametric "
                   "coordinates\n";
      std::cout << "    in element " << imap << ".\n";
    }
    return ifail;
  }

  // Check whether the point is far outside.
  if (t1 < -(1 + f) || t1 > (1 + f) || t2 < -(1 + f) || t2 > (1 + f)) {
    if (debug) {
      std::cout << m_className << "::Coordinates5:\n";
      std::cout << "    Point far outside, (t1,t2) = (" << t1 << ", " << t2
                << ").\n";
    }
    return ifail;
  }

  // Start iteration
  double td1 = t1, td2 = t2;
  if (debug) {
    std::cout << m_className << "::Coordinates5:\n";
    std::cout << "    Iteration starts at (t1,t2) = (" << td1 << ", " << td2
              << ").\n";
  }
  // Loop
  bool converged = false;
  double diff[2], corr[2];
  for (int iter = 0; iter < 10; iter++) {
    if (debug) {
      std::cout << m_className << "::Coordinates5:\n";
      std::cout << "    Iteration " << iter << ":     (t1, t2) = (" << td1
                << ", " << td2 << ").\n";
    }
    // Re-compute the (x,y,z) position for this coordinate.
    double xr =
        nodes[elements[imap].emap[0]].x *
            (-(1 - td1) * (1 - td2) * (1 + td1 + td2)) / 4 +
        nodes[elements[imap].emap[1]].x *
            (-(1 + td1) * (1 - td2) * (1 - td1 + td2)) / 4 +
        nodes[elements[imap].emap[2]].x *
            (-(1 + td1) * (1 + td2) * (1 - td1 - td2)) / 4 +
        nodes[elements[imap].emap[3]].x *
            (-(1 - td1) * (1 + td2) * (1 + td1 - td2)) / 4 +
        nodes[elements[imap].emap[4]].x * (1 - td1) * (1 + td1) * (1 - td2) /
            2 +
        nodes[elements[imap].emap[5]].x * (1 + td1) * (1 + td2) * (1 - td2) /
            2 +
        nodes[elements[imap].emap[6]].x * (1 - td1) * (1 + td1) * (1 + td2) /
            2 +
        nodes[elements[imap].emap[7]].x * (1 - td1) * (1 + td2) * (1 - td2) / 2;
    double yr =
        nodes[elements[imap].emap[0]].y *
            (-(1 - td1) * (1 - td2) * (1 + td1 + td2)) / 4 +
        nodes[elements[imap].emap[1]].y *
            (-(1 + td1) * (1 - td2) * (1 - td1 + td2)) / 4 +
        nodes[elements[imap].emap[2]].y *
            (-(1 + td1) * (1 + td2) * (1 - td1 - td2)) / 4 +
        nodes[elements[imap].emap[3]].y *
            (-(1 - td1) * (1 + td2) * (1 + td1 - td2)) / 4 +
        nodes[elements[imap].emap[4]].y * (1 - td1) * (1 + td1) * (1 - td2) /
            2 +
        nodes[elements[imap].emap[5]].y * (1 + td1) * (1 + td2) * (1 - td2) /
            2 +
        nodes[elements[imap].emap[6]].y * (1 - td1) * (1 + td1) * (1 + td2) /
            2 +
        nodes[elements[imap].emap[7]].y * (1 - td1) * (1 + td2) * (1 - td2) / 2;
    // Compute the Jacobian.
    Jacobian5(imap, td1, td2, det, jac);
    // Compute the difference vector.
    diff[0] = x - xr;
    diff[1] = y - yr;
    // Update the estimate.
    for (int l = 0; l < 2; ++l) {
      corr[l] = 0;
      for (int k = 0; k < 2; ++k) {
        corr[l] += jac[l][k] * diff[k] / det;
      }
    }
    // Debugging
    if (debug) {
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
      if (debug) {
        std::cout << m_className << "::Coordinates5:\n";
        std::cout << "    Convergence reached.\n";
      }
      converged = true;
      break;
    }
  }
  // No convergence reached.
  if (!converged) {
    double xmin, ymin, xmax, ymax;
    xmin = nodes[elements[imap].emap[0]].x;
    xmax = nodes[elements[imap].emap[0]].x;
    if (nodes[elements[imap].emap[1]].x < xmin) {
      xmin = nodes[elements[imap].emap[1]].x;
    }
    if (nodes[elements[imap].emap[1]].x > xmax) {
      xmax = nodes[elements[imap].emap[1]].x;
    }
    if (nodes[elements[imap].emap[2]].x < xmin) {
      xmin = nodes[elements[imap].emap[2]].x;
    }
    if (nodes[elements[imap].emap[2]].x > xmax) {
      xmax = nodes[elements[imap].emap[2]].x;
    }
    if (nodes[elements[imap].emap[3]].x < xmin) {
      xmin = nodes[elements[imap].emap[3]].x;
    }
    if (nodes[elements[imap].emap[3]].x > xmax) {
      xmax = nodes[elements[imap].emap[3]].x;
    }
    if (nodes[elements[imap].emap[4]].x < xmin) {
      xmin = nodes[elements[imap].emap[4]].x;
    }
    if (nodes[elements[imap].emap[4]].x > xmax) {
      xmax = nodes[elements[imap].emap[4]].x;
    }
    if (nodes[elements[imap].emap[5]].x < xmin) {
      xmin = nodes[elements[imap].emap[5]].x;
    }
    if (nodes[elements[imap].emap[5]].x > xmax) {
      xmax = nodes[elements[imap].emap[5]].x;
    }
    if (nodes[elements[imap].emap[6]].x < xmin) {
      xmin = nodes[elements[imap].emap[6]].x;
    }
    if (nodes[elements[imap].emap[6]].x > xmax) {
      xmax = nodes[elements[imap].emap[6]].x;
    }
    if (nodes[elements[imap].emap[7]].x < xmin) {
      xmin = nodes[elements[imap].emap[7]].x;
    }
    if (nodes[elements[imap].emap[7]].x > xmax) {
      xmax = nodes[elements[imap].emap[7]].x;
    }
    ymin = nodes[elements[imap].emap[0]].y;
    ymax = nodes[elements[imap].emap[0]].y;
    if (nodes[elements[imap].emap[1]].y < ymin) {
      ymin = nodes[elements[imap].emap[1]].y;
    }
    if (nodes[elements[imap].emap[1]].y > ymax) {
      ymax = nodes[elements[imap].emap[1]].y;
    }
    if (nodes[elements[imap].emap[2]].y < ymin) {
      ymin = nodes[elements[imap].emap[2]].y;
    }
    if (nodes[elements[imap].emap[2]].y > ymax) {
      ymax = nodes[elements[imap].emap[2]].y;
    }
    if (nodes[elements[imap].emap[3]].y < ymin) {
      ymin = nodes[elements[imap].emap[3]].y;
    }
    if (nodes[elements[imap].emap[3]].y > ymax) {
      ymax = nodes[elements[imap].emap[3]].y;
    }
    if (nodes[elements[imap].emap[4]].y < ymin) {
      ymin = nodes[elements[imap].emap[4]].y;
    }
    if (nodes[elements[imap].emap[4]].y > ymax) {
      ymax = nodes[elements[imap].emap[4]].y;
    }
    if (nodes[elements[imap].emap[5]].y < ymin) {
      ymin = nodes[elements[imap].emap[5]].y;
    }
    if (nodes[elements[imap].emap[5]].y > ymax) {
      ymax = nodes[elements[imap].emap[5]].y;
    }
    if (nodes[elements[imap].emap[6]].y < ymin) {
      ymin = nodes[elements[imap].emap[6]].y;
    }
    if (nodes[elements[imap].emap[6]].y > ymax) {
      ymax = nodes[elements[imap].emap[6]].y;
    }
    if (nodes[elements[imap].emap[7]].y < ymin) {
      ymin = nodes[elements[imap].emap[7]].y;
    }
    if (nodes[elements[imap].emap[7]].y > ymax) {
      ymax = nodes[elements[imap].emap[7]].y;
    }

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
  if (debug) {
    std::cout << m_className << "::Coordinates5:\n";
    std::cout << "    Convergence reached at (t1, t2) = (" << t1 << ", " << t2
              << ").\n";
  }

  // For debugging purposes, show position.
  if (debug) {
    double xr =
        nodes[elements[imap].emap[0]].x *
            (-(1 - t1) * (1 - t2) * (1 + t1 + t2)) / 4 +
        nodes[elements[imap].emap[1]].x *
            (-(1 + t1) * (1 - t2) * (1 - t1 + t2)) / 4 +
        nodes[elements[imap].emap[2]].x *
            (-(1 + t1) * (1 + t2) * (1 - t1 - t2)) / 4 +
        nodes[elements[imap].emap[3]].x *
            (-(1 - t1) * (1 + t2) * (1 + t1 - t2)) / 4 +
        nodes[elements[imap].emap[4]].x * (1 - t1) * (1 + t1) * (1 - t2) / 2 +
        nodes[elements[imap].emap[5]].x * (1 + t1) * (1 + t2) * (1 - t2) / 2 +
        nodes[elements[imap].emap[6]].x * (1 - t1) * (1 + t1) * (1 + t2) / 2 +
        nodes[elements[imap].emap[7]].x * (1 - t1) * (1 + t2) * (1 - t2) / 2;
    double yr =
        nodes[elements[imap].emap[0]].y *
            (-(1 - t1) * (1 - t2) * (1 + t1 + t2)) / 4 +
        nodes[elements[imap].emap[1]].y *
            (-(1 + t1) * (1 - t2) * (1 - t1 + t2)) / 4 +
        nodes[elements[imap].emap[2]].y *
            (-(1 + t1) * (1 + t2) * (1 - t1 - t2)) / 4 +
        nodes[elements[imap].emap[3]].y *
            (-(1 - t1) * (1 + t2) * (1 + t1 - t2)) / 4 +
        nodes[elements[imap].emap[4]].y * (1 - t1) * (1 + t1) * (1 - t2) / 2 +
        nodes[elements[imap].emap[5]].y * (1 + t1) * (1 + t2) * (1 - t2) / 2 +
        nodes[elements[imap].emap[6]].y * (1 - t1) * (1 + t1) * (1 + t2) / 2 +
        nodes[elements[imap].emap[7]].y * (1 - t1) * (1 + t2) * (1 - t2) / 2;
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

int ComponentFieldMap::Coordinates12(double x, double y, double z, double& t1,
                                     double& t2, double& t3, double& t4,
                                     int imap) {

  if (debug) {
    std::cout << m_className << "::Coordinates12:\n";
    std::cout << "   Point (" << x << ", " << y << ", " << z << ") for element "
              << imap << "\n";
  }

  // Failure flag
  int ifail = 1;

  // Compute tetrahedral coordinates.
  t1 =
      (x - nodes[elements[imap].emap[1]].x) *
          ((nodes[elements[imap].emap[2]].y - nodes[elements[imap].emap[1]].y) *
               (nodes[elements[imap].emap[3]].z -
                nodes[elements[imap].emap[1]].z) -
           (nodes[elements[imap].emap[3]].y - nodes[elements[imap].emap[1]].y) *
               (nodes[elements[imap].emap[2]].z -
                nodes[elements[imap].emap[1]].z)) +
      (y - nodes[elements[imap].emap[1]].y) *
          ((nodes[elements[imap].emap[2]].z - nodes[elements[imap].emap[1]].z) *
               (nodes[elements[imap].emap[3]].x -
                nodes[elements[imap].emap[1]].x) -
           (nodes[elements[imap].emap[3]].z - nodes[elements[imap].emap[1]].z) *
               (nodes[elements[imap].emap[2]].x -
                nodes[elements[imap].emap[1]].x)) +
      (z - nodes[elements[imap].emap[1]].z) *
          ((nodes[elements[imap].emap[2]].x - nodes[elements[imap].emap[1]].x) *
               (nodes[elements[imap].emap[3]].y -
                nodes[elements[imap].emap[1]].y) -
           (nodes[elements[imap].emap[3]].x - nodes[elements[imap].emap[1]].x) *
               (nodes[elements[imap].emap[2]].y -
                nodes[elements[imap].emap[1]].y));
  t2 =
      (x - nodes[elements[imap].emap[2]].x) *
          ((nodes[elements[imap].emap[0]].y - nodes[elements[imap].emap[2]].y) *
               (nodes[elements[imap].emap[3]].z -
                nodes[elements[imap].emap[2]].z) -
           (nodes[elements[imap].emap[3]].y - nodes[elements[imap].emap[2]].y) *
               (nodes[elements[imap].emap[0]].z -
                nodes[elements[imap].emap[2]].z)) +
      (y - nodes[elements[imap].emap[2]].y) *
          ((nodes[elements[imap].emap[0]].z - nodes[elements[imap].emap[2]].z) *
               (nodes[elements[imap].emap[3]].x -
                nodes[elements[imap].emap[2]].x) -
           (nodes[elements[imap].emap[3]].z - nodes[elements[imap].emap[2]].z) *
               (nodes[elements[imap].emap[0]].x -
                nodes[elements[imap].emap[2]].x)) +
      (z - nodes[elements[imap].emap[2]].z) *
          ((nodes[elements[imap].emap[0]].x - nodes[elements[imap].emap[2]].x) *
               (nodes[elements[imap].emap[3]].y -
                nodes[elements[imap].emap[2]].y) -
           (nodes[elements[imap].emap[3]].x - nodes[elements[imap].emap[2]].x) *
               (nodes[elements[imap].emap[0]].y -
                nodes[elements[imap].emap[2]].y));
  t3 =
      (x - nodes[elements[imap].emap[3]].x) *
          ((nodes[elements[imap].emap[0]].y - nodes[elements[imap].emap[3]].y) *
               (nodes[elements[imap].emap[1]].z -
                nodes[elements[imap].emap[3]].z) -
           (nodes[elements[imap].emap[1]].y - nodes[elements[imap].emap[3]].y) *
               (nodes[elements[imap].emap[0]].z -
                nodes[elements[imap].emap[3]].z)) +
      (y - nodes[elements[imap].emap[3]].y) *
          ((nodes[elements[imap].emap[0]].z - nodes[elements[imap].emap[3]].z) *
               (nodes[elements[imap].emap[1]].x -
                nodes[elements[imap].emap[3]].x) -
           (nodes[elements[imap].emap[1]].z - nodes[elements[imap].emap[3]].z) *
               (nodes[elements[imap].emap[0]].x -
                nodes[elements[imap].emap[3]].x)) +
      (z - nodes[elements[imap].emap[3]].z) *
          ((nodes[elements[imap].emap[0]].x - nodes[elements[imap].emap[3]].x) *
               (nodes[elements[imap].emap[1]].y -
                nodes[elements[imap].emap[3]].y) -
           (nodes[elements[imap].emap[1]].x - nodes[elements[imap].emap[3]].x) *
               (nodes[elements[imap].emap[0]].y -
                nodes[elements[imap].emap[3]].y));
  t4 =
      (x - nodes[elements[imap].emap[0]].x) *
          ((nodes[elements[imap].emap[2]].y - nodes[elements[imap].emap[0]].y) *
               (nodes[elements[imap].emap[1]].z -
                nodes[elements[imap].emap[0]].z) -
           (nodes[elements[imap].emap[1]].y - nodes[elements[imap].emap[0]].y) *
               (nodes[elements[imap].emap[2]].z -
                nodes[elements[imap].emap[0]].z)) +
      (y - nodes[elements[imap].emap[0]].y) *
          ((nodes[elements[imap].emap[2]].z - nodes[elements[imap].emap[0]].z) *
               (nodes[elements[imap].emap[1]].x -
                nodes[elements[imap].emap[0]].x) -
           (nodes[elements[imap].emap[1]].z - nodes[elements[imap].emap[0]].z) *
               (nodes[elements[imap].emap[2]].x -
                nodes[elements[imap].emap[0]].x)) +
      (z - nodes[elements[imap].emap[0]].z) *
          ((nodes[elements[imap].emap[2]].x - nodes[elements[imap].emap[0]].x) *
               (nodes[elements[imap].emap[1]].y -
                nodes[elements[imap].emap[0]].y) -
           (nodes[elements[imap].emap[1]].x - nodes[elements[imap].emap[0]].x) *
               (nodes[elements[imap].emap[2]].y -
                nodes[elements[imap].emap[0]].y));
  t1 = t1 /
       ((nodes[elements[imap].emap[0]].x - nodes[elements[imap].emap[1]].x) *
            ((nodes[elements[imap].emap[2]].y -
              nodes[elements[imap].emap[1]].y) *
                 (nodes[elements[imap].emap[3]].z -
                  nodes[elements[imap].emap[1]].z) -
             (nodes[elements[imap].emap[3]].y -
              nodes[elements[imap].emap[1]].y) *
                 (nodes[elements[imap].emap[2]].z -
                  nodes[elements[imap].emap[1]].z)) +
        (nodes[elements[imap].emap[0]].y - nodes[elements[imap].emap[1]].y) *
            ((nodes[elements[imap].emap[2]].z -
              nodes[elements[imap].emap[1]].z) *
                 (nodes[elements[imap].emap[3]].x -
                  nodes[elements[imap].emap[1]].x) -
             (nodes[elements[imap].emap[3]].z -
              nodes[elements[imap].emap[1]].z) *
                 (nodes[elements[imap].emap[2]].x -
                  nodes[elements[imap].emap[1]].x)) +
        (nodes[elements[imap].emap[0]].z - nodes[elements[imap].emap[1]].z) *
            ((nodes[elements[imap].emap[2]].x -
              nodes[elements[imap].emap[1]].x) *
                 (nodes[elements[imap].emap[3]].y -
                  nodes[elements[imap].emap[1]].y) -
             (nodes[elements[imap].emap[3]].x -
              nodes[elements[imap].emap[1]].x) *
                 (nodes[elements[imap].emap[2]].y -
                  nodes[elements[imap].emap[1]].y)));
  t2 = t2 /
       ((nodes[elements[imap].emap[1]].x - nodes[elements[imap].emap[2]].x) *
            ((nodes[elements[imap].emap[0]].y -
              nodes[elements[imap].emap[2]].y) *
                 (nodes[elements[imap].emap[3]].z -
                  nodes[elements[imap].emap[2]].z) -
             (nodes[elements[imap].emap[3]].y -
              nodes[elements[imap].emap[2]].y) *
                 (nodes[elements[imap].emap[0]].z -
                  nodes[elements[imap].emap[2]].z)) +
        (nodes[elements[imap].emap[1]].y - nodes[elements[imap].emap[2]].y) *
            ((nodes[elements[imap].emap[0]].z -
              nodes[elements[imap].emap[2]].z) *
                 (nodes[elements[imap].emap[3]].x -
                  nodes[elements[imap].emap[2]].x) -
             (nodes[elements[imap].emap[3]].z -
              nodes[elements[imap].emap[2]].z) *
                 (nodes[elements[imap].emap[0]].x -
                  nodes[elements[imap].emap[2]].x)) +
        (nodes[elements[imap].emap[1]].z - nodes[elements[imap].emap[2]].z) *
            ((nodes[elements[imap].emap[0]].x -
              nodes[elements[imap].emap[2]].x) *
                 (nodes[elements[imap].emap[3]].y -
                  nodes[elements[imap].emap[2]].y) -
             (nodes[elements[imap].emap[3]].x -
              nodes[elements[imap].emap[2]].x) *
                 (nodes[elements[imap].emap[0]].y -
                  nodes[elements[imap].emap[2]].y)));
  t3 = t3 /
       ((nodes[elements[imap].emap[2]].x - nodes[elements[imap].emap[3]].x) *
            ((nodes[elements[imap].emap[0]].y -
              nodes[elements[imap].emap[3]].y) *
                 (nodes[elements[imap].emap[1]].z -
                  nodes[elements[imap].emap[3]].z) -
             (nodes[elements[imap].emap[1]].y -
              nodes[elements[imap].emap[3]].y) *
                 (nodes[elements[imap].emap[0]].z -
                  nodes[elements[imap].emap[3]].z)) +
        (nodes[elements[imap].emap[2]].y - nodes[elements[imap].emap[3]].y) *
            ((nodes[elements[imap].emap[0]].z -
              nodes[elements[imap].emap[3]].z) *
                 (nodes[elements[imap].emap[1]].x -
                  nodes[elements[imap].emap[3]].x) -
             (nodes[elements[imap].emap[1]].z -
              nodes[elements[imap].emap[3]].z) *
                 (nodes[elements[imap].emap[0]].x -
                  nodes[elements[imap].emap[3]].x)) +
        (nodes[elements[imap].emap[2]].z - nodes[elements[imap].emap[3]].z) *
            ((nodes[elements[imap].emap[0]].x -
              nodes[elements[imap].emap[3]].x) *
                 (nodes[elements[imap].emap[1]].y -
                  nodes[elements[imap].emap[3]].y) -
             (nodes[elements[imap].emap[1]].x -
              nodes[elements[imap].emap[3]].x) *
                 (nodes[elements[imap].emap[0]].y -
                  nodes[elements[imap].emap[3]].y)));
  t4 = t4 /
       ((nodes[elements[imap].emap[3]].x - nodes[elements[imap].emap[0]].x) *
            ((nodes[elements[imap].emap[2]].y -
              nodes[elements[imap].emap[0]].y) *
                 (nodes[elements[imap].emap[1]].z -
                  nodes[elements[imap].emap[0]].z) -
             (nodes[elements[imap].emap[1]].y -
              nodes[elements[imap].emap[0]].y) *
                 (nodes[elements[imap].emap[2]].z -
                  nodes[elements[imap].emap[0]].z)) +
        (nodes[elements[imap].emap[3]].y - nodes[elements[imap].emap[0]].y) *
            ((nodes[elements[imap].emap[2]].z -
              nodes[elements[imap].emap[0]].z) *
                 (nodes[elements[imap].emap[1]].x -
                  nodes[elements[imap].emap[0]].x) -
             (nodes[elements[imap].emap[1]].z -
              nodes[elements[imap].emap[0]].z) *
                 (nodes[elements[imap].emap[2]].x -
                  nodes[elements[imap].emap[0]].x)) +
        (nodes[elements[imap].emap[3]].z - nodes[elements[imap].emap[0]].z) *
            ((nodes[elements[imap].emap[2]].x -
              nodes[elements[imap].emap[0]].x) *
                 (nodes[elements[imap].emap[1]].y -
                  nodes[elements[imap].emap[0]].y) -
             (nodes[elements[imap].emap[1]].x -
              nodes[elements[imap].emap[0]].x) *
                 (nodes[elements[imap].emap[2]].y -
                  nodes[elements[imap].emap[0]].y)));

  // Result
  if (debug) {
    std::cout << m_className << "::Coordinates12:\n";
    std::cout << "    Tetrahedral coordinates (t, u, v, w) = (" << t1 << ", "
              << t2 << ", " << t3 << ", " << t4
              << ") sum = " << t1 + t2 + t3 + t4 << ".\n";
  }
  // Re-compute the (x,y,z) position for this coordinate.
  if (debug) {
    double xr = nodes[elements[imap].emap[0]].x * t1 +
                nodes[elements[imap].emap[1]].x * t2 +
                nodes[elements[imap].emap[2]].x * t3 +
                nodes[elements[imap].emap[3]].x * t4;
    double yr = nodes[elements[imap].emap[0]].y * t1 +
                nodes[elements[imap].emap[1]].y * t2 +
                nodes[elements[imap].emap[2]].y * t3 +
                nodes[elements[imap].emap[3]].y * t4;
    double zr = nodes[elements[imap].emap[0]].z * t1 +
                nodes[elements[imap].emap[1]].z * t2 +
                nodes[elements[imap].emap[2]].z * t3 +
                nodes[elements[imap].emap[3]].z * t4;
    double sr = t1 + t2 + t3 + t4;
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

int ComponentFieldMap::Coordinates13(double x, double y, double z, double& t1,
                                     double& t2, double& t3, double& t4,
                                     double jac[4][4], double& det, int imap) {

  if (debug) {
    std::cout << m_className << "::Coordinates13:\n";
    std::cout << "   Point (" << x << ", " << y << ", " << z << ")\n";
  }

  // Failure flag
  int ifail = 1;

  // Provisional values
  t1 = t2 = t3 = t4 = 0.;

  // Set tolerance parameter.
  double f = 0.5;

  // Make a first order approximation.
  int rc = Coordinates12(x, y, z, t1, t2, t3, t4, imap);
  if (rc > 0) {
    if (debug) {
      std::cout << m_className << "::Coordinates13:\n";
      std::cout << "    Failure to obtain linear estimate of isoparametric "
                   "coordinates\n";
      std::cout << "    in element " << imap << ".\n";
    }
    return ifail;
  }
  if (t1 < -f || t2 < -f || t3 < -f || t4 < -f || t1 > 1 + f || t2 > 1 + f ||
      t3 > 1 + f || t4 > 1 + f) {
    if (debug) {
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
  if (debug) {
    std::cout << m_className << "::Coordinates13:\n";
    std::cout << "    Iteration starts at (t1,t2,t3,t4) = (" << td1 << ", "
              << td2 << ", " << td3 << ", " << td4 << ").\n";
  }
  // Loop
  bool converged = false;
  double diff[4], corr[4];
  for (int iter = 0; iter < 10; iter++) {
    if (debug) {
      std::cout << m_className << "::Coordinates13:\n";
      std::cout << "    Iteration " << iter << ":      (t1,t2,t3,t4) = (" << td1
                << ", " << td2 << ", " << td3 << ", " << td4 << ").\n";
    }
    // Re-compute the (x,y,z) position for this coordinate.
    double xr = nodes[elements[imap].emap[0]].x * td1 * (2 * td1 - 1) +
                nodes[elements[imap].emap[1]].x * td2 * (2 * td2 - 1) +
                nodes[elements[imap].emap[2]].x * td3 * (2 * td3 - 1) +
                nodes[elements[imap].emap[3]].x * td4 * (2 * td4 - 1) +
                nodes[elements[imap].emap[4]].x * 4 * td1 * td2 +
                nodes[elements[imap].emap[5]].x * 4 * td1 * td3 +
                nodes[elements[imap].emap[6]].x * 4 * td1 * td4 +
                nodes[elements[imap].emap[7]].x * 4 * td2 * td3 +
                nodes[elements[imap].emap[8]].x * 4 * td2 * td4 +
                nodes[elements[imap].emap[9]].x * 4 * td3 * td4;
    double yr = nodes[elements[imap].emap[0]].y * td1 * (2 * td1 - 1) +
                nodes[elements[imap].emap[1]].y * td2 * (2 * td2 - 1) +
                nodes[elements[imap].emap[2]].y * td3 * (2 * td3 - 1) +
                nodes[elements[imap].emap[3]].y * td4 * (2 * td4 - 1) +
                nodes[elements[imap].emap[4]].y * 4 * td1 * td2 +
                nodes[elements[imap].emap[5]].y * 4 * td1 * td3 +
                nodes[elements[imap].emap[6]].y * 4 * td1 * td4 +
                nodes[elements[imap].emap[7]].y * 4 * td2 * td3 +
                nodes[elements[imap].emap[8]].y * 4 * td2 * td4 +
                nodes[elements[imap].emap[9]].y * 4 * td3 * td4;
    double zr = nodes[elements[imap].emap[0]].z * td1 * (2 * td1 - 1) +
                nodes[elements[imap].emap[1]].z * td2 * (2 * td2 - 1) +
                nodes[elements[imap].emap[2]].z * td3 * (2 * td3 - 1) +
                nodes[elements[imap].emap[3]].z * td4 * (2 * td4 - 1) +
                nodes[elements[imap].emap[4]].z * 4 * td1 * td2 +
                nodes[elements[imap].emap[5]].z * 4 * td1 * td3 +
                nodes[elements[imap].emap[6]].z * 4 * td1 * td4 +
                nodes[elements[imap].emap[7]].z * 4 * td2 * td3 +
                nodes[elements[imap].emap[8]].z * 4 * td2 * td4 +
                nodes[elements[imap].emap[9]].z * 4 * td3 * td4;
    double sr = td1 + td2 + td3 + td4;

    // Compute the Jacobian.
    Jacobian13(imap, td1, td2, td3, td4, det, jac);
    // Compute the difference vector.
    diff[0] = 1 - sr;
    diff[1] = x - xr;
    diff[2] = y - yr;
    diff[3] = z - zr;

    // Update the estimate.
    for (int l = 0; l < 4; ++l) {
      corr[l] = 0;
      for (int k = 0; k < 4; ++k) {
        corr[l] += jac[l][k] * diff[k] / det;
      }
    }

    // Debugging
    if (debug) {
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
      if (debug) {
        std::cout << m_className << "::Coordinates13:\n";
        std::cout << "    Convergence reached.\n";
      }
      converged = true;
      break;
    }
  }

  // No convergence reached.
  if (!converged) {
    double xmin, ymin, zmin, xmax, ymax, zmax;
    xmin = nodes[elements[imap].emap[0]].x;
    xmax = nodes[elements[imap].emap[0]].x;
    if (nodes[elements[imap].emap[1]].x < xmin) {
      xmin = nodes[elements[imap].emap[1]].x;
    }
    if (nodes[elements[imap].emap[1]].x > xmax) {
      xmax = nodes[elements[imap].emap[1]].x;
    }
    if (nodes[elements[imap].emap[2]].x < xmin) {
      xmin = nodes[elements[imap].emap[2]].x;
    }
    if (nodes[elements[imap].emap[2]].x > xmax) {
      xmax = nodes[elements[imap].emap[2]].x;
    }
    if (nodes[elements[imap].emap[3]].x < xmin) {
      xmin = nodes[elements[imap].emap[3]].x;
    }
    if (nodes[elements[imap].emap[3]].x > xmax) {
      xmax = nodes[elements[imap].emap[3]].x;
    }
    ymin = nodes[elements[imap].emap[0]].y;
    ymax = nodes[elements[imap].emap[0]].y;
    if (nodes[elements[imap].emap[1]].y < ymin) {
      ymin = nodes[elements[imap].emap[1]].y;
    }
    if (nodes[elements[imap].emap[1]].y > ymax) {
      ymax = nodes[elements[imap].emap[1]].y;
    }
    if (nodes[elements[imap].emap[2]].y < ymin) {
      ymin = nodes[elements[imap].emap[2]].y;
    }
    if (nodes[elements[imap].emap[2]].y > ymax) {
      ymax = nodes[elements[imap].emap[2]].y;
    }
    if (nodes[elements[imap].emap[3]].y < ymin) {
      ymin = nodes[elements[imap].emap[3]].y;
    }
    if (nodes[elements[imap].emap[3]].y > ymax) {
      ymax = nodes[elements[imap].emap[3]].y;
    }
    zmin = nodes[elements[imap].emap[0]].z;
    zmax = nodes[elements[imap].emap[0]].z;
    if (nodes[elements[imap].emap[1]].z < zmin) {
      zmin = nodes[elements[imap].emap[1]].z;
    }
    if (nodes[elements[imap].emap[1]].z > zmax) {
      zmax = nodes[elements[imap].emap[1]].z;
    }
    if (nodes[elements[imap].emap[2]].z < zmin) {
      zmin = nodes[elements[imap].emap[2]].z;
    }
    if (nodes[elements[imap].emap[2]].z > zmax) {
      zmax = nodes[elements[imap].emap[2]].z;
    }
    if (nodes[elements[imap].emap[3]].z < zmin) {
      zmin = nodes[elements[imap].emap[3]].z;
    }
    if (nodes[elements[imap].emap[3]].z > zmax) {
      zmax = nodes[elements[imap].emap[3]].z;
    }

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
  if (debug) {
    std::cout << m_className << "::Coordinates13:\n";
    std::cout << "    Convergence reached at (t1, t2, t3, t4) = (" << t1 << ", "
              << t2 << ", " << t3 << ", " << t4 << ").\n";
  }

  // For debugging purposes, show position.
  if (debug) {
    // Re-compute the (x,y,z) position for this coordinate.
    double xr = nodes[elements[imap].emap[0]].x * td1 * (2 * td1 - 1) +
                nodes[elements[imap].emap[1]].x * td2 * (2 * td2 - 1) +
                nodes[elements[imap].emap[2]].x * td3 * (2 * td3 - 1) +
                nodes[elements[imap].emap[3]].x * td4 * (2 * td4 - 1) +
                nodes[elements[imap].emap[4]].x * 4 * td1 * td2 +
                nodes[elements[imap].emap[5]].x * 4 * td1 * td3 +
                nodes[elements[imap].emap[6]].x * 4 * td1 * td4 +
                nodes[elements[imap].emap[7]].x * 4 * td2 * td3 +
                nodes[elements[imap].emap[8]].x * 4 * td2 * td4 +
                nodes[elements[imap].emap[9]].x * 4 * td3 * td4;
    double yr = nodes[elements[imap].emap[0]].y * td1 * (2 * td1 - 1) +
                nodes[elements[imap].emap[1]].y * td2 * (2 * td2 - 1) +
                nodes[elements[imap].emap[2]].y * td3 * (2 * td3 - 1) +
                nodes[elements[imap].emap[3]].y * td4 * (2 * td4 - 1) +
                nodes[elements[imap].emap[4]].y * 4 * td1 * td2 +
                nodes[elements[imap].emap[5]].y * 4 * td1 * td3 +
                nodes[elements[imap].emap[6]].y * 4 * td1 * td4 +
                nodes[elements[imap].emap[7]].y * 4 * td2 * td3 +
                nodes[elements[imap].emap[8]].y * 4 * td2 * td4 +
                nodes[elements[imap].emap[9]].y * 4 * td3 * td4;
    double zr = nodes[elements[imap].emap[0]].z * td1 * (2 * td1 - 1) +
                nodes[elements[imap].emap[1]].z * td2 * (2 * td2 - 1) +
                nodes[elements[imap].emap[2]].z * td3 * (2 * td3 - 1) +
                nodes[elements[imap].emap[3]].z * td4 * (2 * td4 - 1) +
                nodes[elements[imap].emap[4]].z * 4 * td1 * td2 +
                nodes[elements[imap].emap[5]].z * 4 * td1 * td3 +
                nodes[elements[imap].emap[6]].z * 4 * td1 * td4 +
                nodes[elements[imap].emap[7]].z * 4 * td2 * td3 +
                nodes[elements[imap].emap[8]].z * 4 * td2 * td4 +
                nodes[elements[imap].emap[9]].z * 4 * td3 * td4;
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

int ComponentFieldMap::CoordinatesCube(double x, double y, double z, double& t1,
                                       double& t2, double& t3, TMatrixD*& jac,
                                       std::vector<TMatrixD*>& dN, int imap) {

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

  // Compute hexahedral coordinates (t1->[-1,1],t2->[-1,1],t3->[-1,1]) and
  // t1 (zeta) is in y-direction
  // t2 (eta)  is in opposite x-direction
  // t3 (mu)   is in z-direction
  // Nodes are set in that way, that node [0] has always lowest x,y,z!
  t2 =
      (2. * (x - nodes[elements[imap].emap[3]].x) /
           (nodes[elements[imap].emap[0]].x - nodes[elements[imap].emap[3]].x) -
       1) *
      -1.;
  t1 = 2. * (y - nodes[elements[imap].emap[3]].y) /
           (nodes[elements[imap].emap[2]].y - nodes[elements[imap].emap[3]].y) -
       1;
  t3 = 2. * (z - nodes[elements[imap].emap[3]].z) /
           (nodes[elements[imap].emap[7]].z - nodes[elements[imap].emap[3]].z) -
       1;
  // Re-compute the (x,y,z) position for this coordinate.
  if (debug) {
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
      xr += nodes[elements[imap].emap[i]].x * n[i];
      yr += nodes[elements[imap].emap[i]].y * n[i];
      zr += nodes[elements[imap].emap[i]].z * n[i];
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
  if (!ready) {
    std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
    std::cerr << "    No valid field map available.\n";
    return;
  }

  // No regular and mirror periodicity at the same time.
  if (xPeriodic && xMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along x requested; reset.\n";
    xPeriodic = false;
    xMirrorPeriodic = false;
    warning = true;
  }
  if (yPeriodic && yMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along y requested; reset.\n";
    yPeriodic = false;
    yMirrorPeriodic = false;
    warning = true;
  }
  if (zPeriodic && zMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along z requested; reset.\n";
    zPeriodic = false;
    zMirrorPeriodic = false;
    warning = true;
  }

  // In case of axial periodicity,
  // the range must be an integral part of 2 pi.
  if (xAxiallyPeriodic) {
    if (mapxamin >= mapxamax) {
      mapnxa = 0;
    } else {
      mapnxa = TwoPi / (mapxamax - mapxamin);
    }
    if (fabs(mapnxa - int(0.5 + mapnxa)) > 0.001 || mapnxa < 1.5) {
      std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
      std::cerr << "    X-axial symmetry has been requested but the map\n";
      std::cerr << "    does not cover an integral fraction of 2 pi; reset.\n";
      xAxiallyPeriodic = false;
      warning = true;
    }
  }

  if (yAxiallyPeriodic) {
    if (mapyamin >= mapyamax) {
      mapnya = 0;
    } else {
      mapnya = TwoPi / (mapyamax - mapyamin);
    }
    if (fabs(mapnya - int(0.5 + mapnya)) > 0.001 || mapnya < 1.5) {
      std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
      std::cerr << "    Y-axial symmetry has been requested but the map\n";
      std::cerr << "    does not cover an integral fraction of 2 pi; reset.\n";
      yAxiallyPeriodic = false;
      warning = true;
    }
  }

  if (zAxiallyPeriodic) {
    if (mapzamin >= mapzamax) {
      mapnza = 0;
    } else {
      mapnza = TwoPi / (mapzamax - mapzamin);
    }
    if (fabs(mapnza - int(0.5 + mapnza)) > 0.001 || mapnza < 1.5) {
      std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
      std::cerr << "    Z-axial symmetry has been requested but the map\n";
      std::cerr << "    does not cover an integral fraction of 2 pi; reset.\n";
      zAxiallyPeriodic = false;
      warning = true;
    }
  }

  // Not more than 1 rotational symmetry
  if ((xRotationSymmetry && yRotationSymmetry) ||
      (xRotationSymmetry && zRotationSymmetry) ||
      (yRotationSymmetry && zRotationSymmetry)) {
    std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
    std::cerr << "    Only 1 rotational symmetry allowed; reset.\n";
    xRotationSymmetry = false;
    yRotationSymmetry = false;
    zRotationSymmetry = false;
    warning = true;
  }

  // No rotational symmetry as well as axial periodicity
  if ((xRotationSymmetry || yRotationSymmetry || zRotationSymmetry) &&
      (xAxiallyPeriodic || yAxiallyPeriodic || zAxiallyPeriodic)) {
    std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
    std::cerr << "    Not allowed to combine rotational symmetry\n";
    std::cerr << "    and axial periodicity; reset.\n";
    xAxiallyPeriodic = false;
    yAxiallyPeriodic = false;
    zAxiallyPeriodic = false;
    xRotationSymmetry = false;
    yRotationSymmetry = false;
    zRotationSymmetry = false;
    warning = true;
  }

  // In case of rotational symmetry, the x-range should not straddle 0.
  if (xRotationSymmetry || yRotationSymmetry || zRotationSymmetry) {
    if (mapxmin * mapxmax < 0) {
      std::cerr << m_className << "::UpdatePeriodicityCommon:\n";
      std::cerr << "    Rotational symmetry requested, \n";
      std::cerr << "    but x-range straddles 0; reset.\n";
      xRotationSymmetry = false;
      yRotationSymmetry = false;
      zRotationSymmetry = false;
      warning = true;
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
  if (xRotationSymmetry) {
    xMinBoundingBox = mapymin;
    xMaxBoundingBox = mapymax;
    yMinBoundingBox = -std::max(fabs(mapxmin), fabs(mapxmax));
    yMaxBoundingBox = +std::max(fabs(mapxmin), fabs(mapxmax));
    zMinBoundingBox = -std::max(fabs(mapxmin), fabs(mapxmax));
    zMaxBoundingBox = +std::max(fabs(mapxmin), fabs(mapxmax));
  } else if (yRotationSymmetry) {
    xMinBoundingBox = -std::max(fabs(mapxmin), fabs(mapxmax));
    xMaxBoundingBox = +std::max(fabs(mapxmin), fabs(mapxmax));
    yMinBoundingBox = mapymin;
    yMaxBoundingBox = mapymax;
    zMinBoundingBox = -std::max(fabs(mapxmin), fabs(mapxmax));
    zMaxBoundingBox = +std::max(fabs(mapxmin), fabs(mapxmax));
  } else if (zRotationSymmetry) {
    xMinBoundingBox = -std::max(fabs(mapxmin), fabs(mapxmax));
    xMaxBoundingBox = +std::max(fabs(mapxmin), fabs(mapxmax));
    yMinBoundingBox = -std::max(fabs(mapxmin), fabs(mapxmax));
    yMaxBoundingBox = +std::max(fabs(mapxmin), fabs(mapxmax));
    zMinBoundingBox = mapymin;
    zMaxBoundingBox = mapymax;
  }

  if (xAxiallyPeriodic) {
    yMinBoundingBox = -std::max(std::max(fabs(mapymin), fabs(mapymax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
    yMaxBoundingBox = +std::max(std::max(fabs(mapymin), fabs(mapymax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
    zMinBoundingBox = -std::max(std::max(fabs(mapymin), fabs(mapymax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
    zMaxBoundingBox = +std::max(std::max(fabs(mapymin), fabs(mapymax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
  } else if (yAxiallyPeriodic) {
    xMinBoundingBox = -std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
    xMaxBoundingBox = +std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
    zMinBoundingBox = -std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
    zMaxBoundingBox = +std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapzmin), fabs(mapzmax)));
  } else if (zAxiallyPeriodic) {
    xMinBoundingBox = -std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapymin), fabs(mapymax)));
    xMaxBoundingBox = +std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapymin), fabs(mapymax)));
    yMinBoundingBox = -std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapymin), fabs(mapymax)));
    yMaxBoundingBox = +std::max(std::max(fabs(mapxmin), fabs(mapxmax)),
                                std::max(fabs(mapymin), fabs(mapymax)));
  }

  if (xPeriodic || xMirrorPeriodic) {
    xMinBoundingBox = -INFINITY;
    xMaxBoundingBox = +INFINITY;
  }
  if (yPeriodic || yMirrorPeriodic) {
    yMinBoundingBox = -INFINITY;
    yMaxBoundingBox = +INFINITY;
  }
  if (zPeriodic || zMirrorPeriodic) {
    zMinBoundingBox = -INFINITY;
    zMaxBoundingBox = +INFINITY;
  }

  // Display the range if requested.
  if (debug) PrintRange();
}

void ComponentFieldMap::UpdatePeriodicity2d() {

  // Check the required data is available.
  if (!ready) {
    std::cerr << m_className << "::UpdatePeriodicity2d:\n";
    std::cerr << "    No valid field map available.\n";
    return;
  }

  // No z-periodicity in 2d
  if (zPeriodic || zMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity2d:\n";
    std::cerr << "    Simple or mirror periodicity along z\n";
    std::cerr << "    requested for a 2d map; reset.\n";
    zPeriodic = false;
    zMirrorPeriodic = false;
    warning = true;
  }

  // Only z-axial periodicity in 2d maps
  if (xAxiallyPeriodic || yAxiallyPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity2d:\n";
    std::cerr << "    Axial symmetry has been requested \n";
    std::cerr << "    around x or y for a 2D map; reset.\n";
    xAxiallyPeriodic = false;
    yAxiallyPeriodic = false;
    warning = true;
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
  if (!ready || nNodes < 1) {
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
  if (is3d) {
    zMinBoundingBox = mapzmin;
    zMaxBoundingBox = mapzmax;
  } else {
    mapzmin = zMinBoundingBox;
    mapzmax = zMaxBoundingBox;
  }
  hasBoundingBox = true;

  // Display the range if requested.
  if (debug) PrintRange();
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
  if (xPeriodic) {
    std::cout << " simple with length " << cellsx << " cm";
  }
  if (xMirrorPeriodic) {
    std::cout << " mirror with length " << cellsx << " cm";
  }
  if (xAxiallyPeriodic) {
    std::cout << " axial " << int(0.5 + mapnxa) << "-fold repetition";
  }
  if (xRotationSymmetry) std::cout << " rotational symmetry";
  if (!(xPeriodic || xMirrorPeriodic || xAxiallyPeriodic || xRotationSymmetry))
    std::cout << " none";
  std::cout << "\n";

  std::cout << "            y:";
  if (yPeriodic) {
    std::cout << " simple with length " << cellsy << " cm";
  }
  if (yMirrorPeriodic) {
    std::cout << " mirror with length " << cellsy << " cm";
  }
  if (yAxiallyPeriodic) {
    std::cout << " axial " << int(0.5 + mapnya) << "-fold repetition";
  }
  if (yRotationSymmetry) {
    std::cout << " rotational symmetry";
  }
  if (!(yPeriodic || yMirrorPeriodic || yAxiallyPeriodic || yRotationSymmetry))
    std::cout << " none";
  std::cout << "\n";

  std::cout << "            z:";
  if (zPeriodic) {
    std::cout << " simple with length " << cellsz << " cm";
  }
  if (zMirrorPeriodic) {
    std::cout << " mirror with length " << cellsz << " cm";
  }
  if (zAxiallyPeriodic) {
    std::cout << " axial " << int(0.5 + mapnza) << "-fold repetition";
  }
  if (zRotationSymmetry) {
    std::cout << " rotational symmetry";
  }
  if (!(zPeriodic || zMirrorPeriodic || zAxiallyPeriodic || zRotationSymmetry))
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

  if (!ready) return false;

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
  if (xPeriodic) {
    xpos = mapxmin + fmod(xpos - mapxmin, mapxmax - mapxmin);
    if (xpos < mapxmin) xpos += mapxmax - mapxmin;
  } else if (xMirrorPeriodic) {
    double xnew = mapxmin + fmod(xpos - mapxmin, mapxmax - mapxmin);
    if (xnew < mapxmin) xnew += mapxmax - mapxmin;
    int nx = int(floor(0.5 + (xnew - xpos) / (mapxmax - mapxmin)));
    if (nx != 2 * (nx / 2)) {
      xnew = mapxmin + mapxmax - xnew;
      xmirrored = true;
    }
    xpos = xnew;
  }
  if (xAxiallyPeriodic && (zpos != 0 || ypos != 0)) {
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
  if (yPeriodic) {
    ypos = mapymin + fmod(ypos - mapymin, mapymax - mapymin);
    if (ypos < mapymin) ypos += mapymax - mapymin;
  } else if (yMirrorPeriodic) {
    double ynew = mapymin + fmod(ypos - mapymin, mapymax - mapymin);
    if (ynew < mapymin) ynew += mapymax - mapymin;
    int ny = int(floor(0.5 + (ynew - ypos) / (mapymax - mapymin)));
    if (ny != 2 * (ny / 2)) {
      ynew = mapymin + mapymax - ynew;
      ymirrored = true;
    }
    ypos = ynew;
  }
  if (yAxiallyPeriodic && (xpos != 0 || zpos != 0)) {
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
  if (zPeriodic) {
    zpos = mapzmin + fmod(zpos - mapzmin, mapzmax - mapzmin);
    if (zpos < mapzmin) zpos += mapzmax - mapzmin;
  } else if (zMirrorPeriodic) {
    double znew = mapzmin + fmod(zpos - mapzmin, mapzmax - mapzmin);
    if (znew < mapzmin) znew += mapzmax - mapzmin;
    int nz = int(floor(0.5 + (znew - zpos) / (mapzmax - mapzmin)));
    if (nz != 2 * (nz / 2)) {
      znew = mapzmin + mapzmax - znew;
      zmirrored = true;
    }
    zpos = znew;
  }
  if (zAxiallyPeriodic && (ypos != 0 || xpos != 0)) {
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
  if (xRotationSymmetry) {
    rcoordinate = sqrt(ypos * ypos + zpos * zpos);
    zcoordinate = xpos;
  } else if (yRotationSymmetry) {
    rcoordinate = sqrt(xpos * xpos + zpos * zpos);
    zcoordinate = ypos;
  } else if (zRotationSymmetry) {
    rcoordinate = sqrt(xpos * xpos + ypos * ypos);
    zcoordinate = zpos;
  }

  if (xRotationSymmetry || yRotationSymmetry || zRotationSymmetry) {
    xpos = rcoordinate;
    ypos = zcoordinate;
    zpos = 0;
  }
}

void ComponentFieldMap::UnmapFields(double& ex, double& ey, double& ez,
                                    double& xpos, double& ypos, double& zpos,
                                    bool& xmirrored, bool& ymirrored,
                                    bool& zmirrored, double& rcoordinate,
                                    double& rotation) {

  // Apply mirror imaging.
  if (xmirrored) ex = -ex;
  if (ymirrored) ey = -ey;
  if (zmirrored) ez = -ez;

  // Rotate the field.
  double er, theta;
  if (xAxiallyPeriodic) {
    er = sqrt(ey * ey + ez * ez);
    theta = atan2(ez, ey);
    theta += rotation;
    ey = er * cos(theta);
    ez = er * sin(theta);
  }
  if (yAxiallyPeriodic) {
    er = sqrt(ez * ez + ex * ex);
    theta = atan2(ex, ez);
    theta += rotation;
    ez = er * cos(theta);
    ex = er * sin(theta);
  }
  if (zAxiallyPeriodic) {
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
  if (xRotationSymmetry) {
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
  if (yRotationSymmetry) {
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
  if (zRotationSymmetry) {
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
  if (!ready) {
    std::cerr << m_className << "::CalculateElementBoundingBoxes:\n";
    std::cerr << "    Field map not yet initialised.\n";
    std::cerr << "    Bounding boxes of elements cannot be computed.\n";
    return;
  }

  // Calculate the bounding boxes of all elements
  for (int i = 0; i < nElements; ++i) {
    element& elem = elements[i];
    elem.xmin = std::min(
        std::min(nodes[elem.emap[0]].x, nodes[elem.emap[1]].x),
        std::min(nodes[elem.emap[2]].x, nodes[elem.emap[3]].x));
    elem.xmax = std::max(
        std::max(nodes[elem.emap[0]].x, nodes[elem.emap[1]].x),
        std::max(nodes[elem.emap[2]].x, nodes[elem.emap[3]].x));
    elem.ymin = std::min(
        std::min(nodes[elem.emap[0]].y, nodes[elem.emap[1]].y),
        std::min(nodes[elem.emap[2]].y, nodes[elem.emap[3]].y));
    elem.ymax = std::max(
        std::max(nodes[elem.emap[0]].y, nodes[elem.emap[1]].y),
        std::max(nodes[elem.emap[2]].y, nodes[elem.emap[3]].y));
    elem.zmin = std::min(
        std::min(nodes[elem.emap[0]].z, nodes[elem.emap[1]].z),
        std::min(nodes[elem.emap[2]].z, nodes[elem.emap[3]].z));
    elem.zmax = std::max(
        std::max(nodes[elem.emap[0]].z, nodes[elem.emap[1]].z),
        std::max(nodes[elem.emap[2]].z, nodes[elem.emap[3]].z));
  }
}

bool ComponentFieldMap::InitializeTetrahedralTree(void) {

  // Do not proceed if not properly initialised.
  if (!ready) {
    std::cerr << m_className << "::InitializeTetrahedralTree:\n";
    std::cerr << "    Field map not yet initialised.\n";
    std::cerr << "    Tetrahedral tree cannot be initialized.\n";
    return false;
  }

  std::cerr << m_className << "::InitializeTetrahedralTree:\n";
  std::cerr << "    About to initialize the tetrahedral tree.\n";

  // check if the caching has not been done before
  if(!cacheElemBoundingBoxes) {
    CalculateElementBoundingBoxes();
  }

  // determine the bounding box
  double xmin=0., ymin=0., zmin=0., xmax=0., ymax=0., zmax=0.;
  for(unsigned int i=0; i<nodes.size(); i++) {
    const node& n = nodes[i];

    // adjust the bounding box
    if(n.x <= xmin) xmin = n.x;
    if(n.x > xmax) xmax = n.x;
    if(n.y <= ymin) ymin = n.y;
    if(n.y > ymax) ymax = n.y;
    if(n.z <= zmin) zmin = n.z;
    if(n.z > zmax) zmax = n.z;
  }

  std::cout << "::Bounding box:\n ";
	std::cout << std::scientific << "\tx: " << xmin << " -> " << xmax << std::endl;
	std::cout << std::scientific << "\ty: " << ymin << " -> " << ymax << std::endl;
	std::cout << std::scientific << "\tz: " << zmin << " -> " << zmax << std::endl;

  tetTree = new TetrahedralTree(Vec3(xmin+(xmax-xmin)/2,ymin+(ymax-ymin)/2, zmin+(zmax-zmin)/2), Vec3((xmax-xmin)/2,(ymax-ymin)/2,(zmax-zmin)/2));

  std::cerr << "Tree instantiated.\n";

  // insert all mesh nodes in the tree
  for(unsigned int i=0; i<nodes.size(); i++) {
    const node& n = nodes[i];
    //std::cerr << i << std::endl;
    tetTree->InsertMeshNode(Vec3(n.x, n.y, n.z), i);
  }

  std::cerr << m_className << "::InitializeTetrahedralTree:\n";
  std::cerr << "    Tetrahedral tree nodes initialized successfully.\n";

  // insert all mesh elements (tetrahedrons) in the tree
  for(unsigned int i=0; i<elements.size(); i++)
  {
    element& e = elements[i];
      
    double bb[6] = {e.xmin, e.ymin, e.zmin, e.xmax, e.ymax, e.zmax};
    tetTree->InsertTetrahedron(bb, i);
  }

  std::cerr << m_className << "::InitializeTetrahedralTree:\n";
  std::cerr << "    Tetrahedral tree initialized successfully.\n";

  isTreeInitialized = true;
  return true;
}

}

