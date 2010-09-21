#include <stdio.h>
#include <string.h>
#include <fstream>

#include <stdlib.h>
#include <math.h>
#include <string>

#include "ComponentFieldMap.hh"
#include "FundamentalConstants.hh"

namespace Garfield {

ComponentFieldMap::ComponentFieldMap() :
  nElements(-1), lastElement(-1), 
  nNodes(-1), nMaterials(-1),
  hasWeightingField(false),
  hasBoundingBox(false), 
  deleteBackground(true), checkMultipleElement(false),
  warning(false) {
  
  className = "ComponentFieldMap";
  
  materials.clear();
  elements.clear();
  nodes.clear();

}

void 
ComponentFieldMap::PrintMaterials() {

  // Do not proceed if not properly initialised.
  if (!ready) {
    printf("ComponentFieldMap::PrintMaterials:\n");
    printf("    Field map not yet initialised, drift medium can not be selected.\n");
    return;
  }

  if (nMaterials < 0) {
    printf("ComponentFieldMap::PrintMaterials:\n");
    printf("    No materials are currently defined.\n");
    return;
  }
  
  printf("ComponentFieldMap::PrintMaterials:\n");
  printf("    Currently %d materials are defined\n", nMaterials);
  printf("                Index Permittivity  Resistivity Notes\n");
  for (int i = 0; i < nMaterials; ++i) {
    printf("                %5d %12g %12g", i, materials[i].eps, materials[i].ohm);
    if (materials[i].medium != 0) {
      std::string name = materials[i].medium->GetName();
      printf(" %s", name.c_str());
      if (materials[i].medium->IsDriftable()) printf(", drift medium");
      if (materials[i].medium->IsIonisable()) printf(", ionisable");
    }
    if (materials[i].driftmedium) {
      printf(" (old flag drift medium)\n");
    } else {
      printf("\n");
    }
  }
  
}

void 
ComponentFieldMap::DriftMedium(int imat) {

  // Do not proceed if not properly initialised.
  if (!ready) {
    printf("ComponentFieldMap::DriftMedium:\n");
    printf("    Field map not yet initialised, drift medium cannot be selected.\n");
    return;
  }

  // Check value
  if (imat < 0 || imat >= nMaterials) {
    printf("ComponentFieldMap::DriftMedium:\n");
    printf("    Material index %d is out of range.\n", imat);
    return;
  }

  // Make drift medium
  materials[imat].driftmedium = true;
  
}

void 
ComponentFieldMap::NotDriftMedium(const int imat) {

  // Do not proceed if not properly initialised.
  if (!ready) {
    printf("ComponentFieldMap::NotDriftMedium:\n");
    printf("    Field map not yet initialised, drift medium cannot be selected.\n");
    return;
  }

  // Check value
  if (imat < 0 || imat >= nMaterials) {
    printf("ComponentFieldMap::NotDriftMedium:\n");
    printf("    Material index %d is out of range.\n", imat);
    return;
  }

  // Make drift medium
  materials[imat].driftmedium = false;

}

double 
ComponentFieldMap::GetPermittivity(const int imat) {

  if (imat < 0 || imat >= nMaterials) {
    printf("ComponentFieldMap::GetPermittivity:\n");
    printf("    Material index %d is out of range.\n", imat);
    return -1.;
  }

  return materials[imat].eps;

}

double 
ComponentFieldMap::GetConductivity(const int imat) {

  if (imat < 0 || imat >= nMaterials) {
    printf("ComponentFieldMap::GetConductivity:\n");
    printf("    Material index %d is out of range.\n", imat);
    return -1;
  }

  return materials[imat].ohm;

}

void 
ComponentFieldMap::SetMedium(const int imat, Medium* m) {

  if (imat < 0 || imat >= nMaterials) {
    printf("ComponentFieldMap::SetMedium:\n");
    printf("    Material index %d is out of range.\n", imat);
    return;
  }
  
  if (m == 0) {
    printf("ComponentFieldMap::SetMedium:\n");
    printf("    Medium is not defined.\n");
    return;
  }
  
  if (debug) {
    std::string name = m->GetName();
    printf("ComponentFieldMap::SetMedium:\n");
    printf("    Associated material %d with medium %s.\n", imat, name.c_str());
  }
  
  materials[imat].medium = m;
  
}

bool 
ComponentFieldMap::GetMedium(const int imat, Medium*& m) const {

  if (imat < 0 || imat >= nMaterials) {
    printf("ComponentFieldMap::GetMedium:\n");
    printf("    Material index %d is out of range.\n", imat);
    m = 0;
    return false;
  }
  
  m = materials[imat].medium;
  if (m == 0) return false;  
  return true;

}

int 
ComponentFieldMap::FindElement5(const double x, const double y, double const z,
                                double& t1, double& t2, double& t3, double& t4,
                                double jac[4][4], double& det) {
                
  // Backup
  double jacbak[4][4], detbak = 1.;
  double  t1bak = 0., t2bak = 0., t3bak = 0., t4bak = 0.;
  int imapbak = -1;

  // Initial values.
  t1 = t2 = t3 = t4 = 0;

  // Check previously used element
  int rc;
  if (lastElement > -1 && !checkMultipleElement) {
    if (elements[lastElement].degenerate) {
      rc = Coordinates3(x, y, z, t1, t2, t3, t4, jac, det, lastElement);
      if (rc == 0 && t1 >= 0 && t1 <= +1 && 
                     t2 >= 0 && t2 <= +1 && 
                     t3 >= 0 && t3 <= +1) return lastElement;
    } else {
      rc = Coordinates5(x, y, z, t1, t2, t3, t4, jac, det, lastElement);
      if (rc == 0 && t1 >= -1 && t1 <= +1 && 
                     t2 >= -1 && t2 <= +1) return lastElement;
    }
  }
  
  // Verify the count of volumes that contain the point.
  int nfound = 0;
  int imap = -1;

  // Tolerance
  const double f = 0.2;
  
  // Scan all elements
  double xmin, ymin, zmin, xmax, ymax, zmax;
  for (int i = 0; i < nElements; ++i) {
    xmin = std::min(std::min(nodes[elements[i].emap[0]].xmap, nodes[elements[i].emap[1]].xmap),
                    std::min(nodes[elements[i].emap[2]].xmap, nodes[elements[i].emap[3]].xmap));
    xmax = std::max(std::max(nodes[elements[i].emap[0]].xmap, nodes[elements[i].emap[1]].xmap),
                    std::max(nodes[elements[i].emap[2]].xmap, nodes[elements[i].emap[3]].xmap));
    ymin = std::min(std::min(nodes[elements[i].emap[0]].ymap, nodes[elements[i].emap[1]].ymap),
                    std::min(nodes[elements[i].emap[2]].ymap, nodes[elements[i].emap[3]].ymap));
    ymax = std::max(std::max(nodes[elements[i].emap[0]].ymap, nodes[elements[i].emap[1]].ymap),
                    std::max(nodes[elements[i].emap[2]].ymap, nodes[elements[i].emap[3]].ymap));
    zmin = std::min(std::min(nodes[elements[i].emap[0]].zmap, nodes[elements[i].emap[1]].zmap),
                    std::min(nodes[elements[i].emap[2]].zmap, nodes[elements[i].emap[3]].zmap));
    zmax = std::max(std::max(nodes[elements[i].emap[0]].zmap, nodes[elements[i].emap[1]].zmap),
                    std::max(nodes[elements[i].emap[2]].zmap, nodes[elements[i].emap[3]].zmap)); 
    if (x < xmin - f * (xmax - xmin) || x > xmax + f * (xmax - xmin) ||
        y < ymin - f * (ymax - ymin) || y > ymax + f * (ymax - ymin) || 
        z < zmin - f * (zmax - zmin) || z > zmax + f * (zmax - zmin)) continue;
        
    if (elements[i].degenerate) {
      // Degenerate element
      rc = Coordinates3(x, y, z, t1, t2, t3, t4, jac, det, i);
      if (rc == 0 && t1 >= 0 && t1 <= +1 && t2 >= 0 && t2 <= +1 && t3 >= 0 && t3 <= +1) {
        ++nfound;
        imap = i;
        lastElement = i;
        if (debug) {
          printf("ComponentFieldMap::FindElement5:\n");
          printf("    Found matching degenerate element %d.\n", i);
        }
        if (!checkMultipleElement) return i;
        for (int j = 0; j < 4; ++j) {
          for (int k = 0; k < 4; ++k) jacbak[j][k] = jac[j][k];
        }
        detbak = det; t1bak = t1; t2bak = t2; t3bak = t3; t4bak = t4; imapbak = imap;
        if (debug) {
          printf("ComponentFieldMap::FindElement5:\n");
          printf("    Global = (%g,%g), Local = (%g,%g,%g,%g), Element = %d (degenerate: %d)\n",
                 x, y, t1, t2, t3, t4, imap, elements[imap].degenerate);
          printf("                          Node             x            y            V\n");
          for (int ii = 0; ii < 6; ++ii) {
            printf("                          %-5d %12g %12g %12g\n",
                   elements[imap].emap[ii], 
                   nodes[elements[imap].emap[ii]].xmap, 
                   nodes[elements[imap].emap[ii]].ymap, 
                   nodes[elements[imap].emap[ii]].vmap);
          }
        }
      }
    } else {
      // Non-degenerate element
      rc = Coordinates5(x, y, z, t1, t2, t3, t4, jac, det, i);
      if (rc == 0 && t1 >= -1 && t1 <= +1 && t2 >= -1 && t2 <= +1) {
        ++nfound;
        imap = i;
        lastElement = i;
        if (debug) {
          printf("ComponentFieldMap::FindElement5:\n");
          printf("    Found matching non-degenerate element %d.\n", i);
        }
        if (!checkMultipleElement) return i;
        for (int j = 0; j < 4; ++j) {
          for (int k = 0; k < 4; ++k) jacbak[j][k] = jac[j][k];
        }
        detbak = det; t1bak = t1; t2bak = t2; t3bak = t3; t4bak = t4; imapbak = imap;
        if (debug) {
          printf("ComponentFieldMap::FindElement5:\n");
          printf("    Global = (%g,%g), Local = (%g,%g,%g,%g), Element = %d (degenerate: %d)\n",
                 x, y, t1, t2, t3, t4, imap, elements[imap].degenerate);
          printf("                          Node             x            y            V\n");
          for (int ii = 0; ii < 8; ++ii) {
            printf("                          %-5d %12g %12g %12g\n",
                   elements[imap].emap[ii],
                   nodes[elements[imap].emap[ii]].xmap,
                   nodes[elements[imap].emap[ii]].ymap,
                   nodes[elements[imap].emap[ii]].vmap);
          }
        }
      }
    }
  }
  
  // In checking mode, verify the tetrahedron/triangle count.
  if (checkMultipleElement) {
    if (nfound < 1) {
      if (debug) {
        printf("ComponentFieldMap::FindElement5:\n"); 
        printf("    No element matching point (%g,%g) found.\n", x, y);
      }
      lastElement = -1;
      return -1;
    }
    if (nfound > 1) {
      printf("ComponentFieldMap::FindElement5:\n");
      printf("    Found %d elements matching point (%g,%g).\n", nfound, x, y);
    }
    if (nfound > 0) {
      for (int j = 0; j < 4; ++j) {
        for (int k = 0; k < 4; ++k) jac[j][k] = jacbak[j][k];
      }
      det = detbak; t1 = t1bak; t2 = t2bak; t3 = t3bak; t4 = t4bak; imap = imapbak;
      lastElement = imap;
      return imap;
    }
  }
  
  if (debug) {
    printf("ComponentFieldMap::FindElement5:\n");
    printf("    No element matching point (%g,%g) found.\n", x, y);
  }
  return -1;
  
}

int 
ComponentFieldMap::FindElement13(const double x, const double y, const double z,
                                 double& t1, double& t2, double& t3, double& t4,
                                 double jac[4][4], double& det) {

  // Backup
  double jacbak[4][4], detbak = 1.;
  double  t1bak = 0., t2bak = 0., t3bak = 0., t4bak = 0.;
  int imapbak = -1;

  // Initial values.
  t1 = t2 = t3 = t4 = 0.;

  // Check previously used element
  int rc;
  if (lastElement > -1 && !checkMultipleElement) {
    rc = Coordinates13(x, y, z, t1, t2, t3, t4, jac, det, lastElement);
    if (rc == 0 &&
	      t1 >= 0 && t1 <= +1 &&
	      t2 >= 0 && t2 <= +1 &&
	      t3 >= 0 && t3 <= +1 &&
	      t4 >= 0 && t4 <= +1) return lastElement;
  }

  // Verify the count of volumes that contain the point.
  int nfound = 0;
  int imap = -1;

  // Tolerance
  const double f = 0.2;

  // Scan all elements
  double xmin, ymin, zmin, xmax, ymax, zmax;
  for (int i = 0; i < nElements; i++) {
    xmin = std::min(std::min(nodes[elements[i].emap[0]].xmap, nodes[elements[i].emap[1]].xmap),
                    std::min(nodes[elements[i].emap[2]].xmap, nodes[elements[i].emap[3]].xmap));
    xmax = std::max(std::max(nodes[elements[i].emap[0]].xmap, nodes[elements[i].emap[1]].xmap),
                    std::max(nodes[elements[i].emap[2]].xmap, nodes[elements[i].emap[3]].xmap));
    ymin = std::min(std::min(nodes[elements[i].emap[0]].ymap, nodes[elements[i].emap[1]].ymap),
                    std::min(nodes[elements[i].emap[2]].ymap, nodes[elements[i].emap[3]].ymap));
    ymax = std::max(std::max(nodes[elements[i].emap[0]].ymap, nodes[elements[i].emap[1]].ymap),
                    std::max(nodes[elements[i].emap[2]].ymap, nodes[elements[i].emap[3]].ymap));
    zmin = std::min(std::min(nodes[elements[i].emap[0]].zmap, nodes[elements[i].emap[1]].zmap),
                    std::min(nodes[elements[i].emap[2]].zmap, nodes[elements[i].emap[3]].zmap));
    zmax = std::max(std::max(nodes[elements[i].emap[0]].zmap, nodes[elements[i].emap[1]].zmap),
                    std::max(nodes[elements[i].emap[2]].zmap, nodes[elements[i].emap[3]].zmap));
    if (x < xmin - f * (xmax - xmin) || x > xmax + f * (xmax - xmin) ||
        y < ymin - f * (ymax - ymin) || y > ymax + f * (ymax - ymin) || 
        z < zmin - f * (zmax - zmin) || z > zmax + f * (zmax - zmin)) continue;
    rc = Coordinates13(x, y, z, t1, t2, t3, t4, jac, det, i);
    
    if (rc == 0 &&
	    t1 >= 0 && t1 <= +1 &&
	    t2 >= 0 && t2 <= +1 &&
	    t3 >= 0 && t3 <= +1 &&
	    t4 >= 0 && t4 <= +1) {
      ++nfound;
      imap = i;
      lastElement = i;
      if (debug) {
        printf("ComponentFieldMap::FindElement13:\n");
        printf("    Found matching element %d.\n", i);
      }
      if (!checkMultipleElement) return i;
      for (int j = 0; j < 4; ++j) {
        for (int k = 0; k < 4; ++k) jacbak[j][k] = jac[j][k];
      }
      detbak = det; t1bak = t1; t2bak = t2; t3bak = t3; t4bak = t4; 
      imapbak = imap;
      if (debug) {
        printf("ComponentFieldMap::FindElement13:\n");
        printf("    Global = (%g,%g,%g), Local = (%g,%g,%g,%g), Element = %d\n",
               x, y, z, t1, t2, t3, t4, imap);
        printf("                          Node             x            y            z            V\n");
        for (int ii = 0; ii < 10; ++ii) {
          printf("                          %-5d %12g %12g %12g %12g\n",
                 elements[imap].emap[ii], 
                 nodes[elements[imap].emap[ii]].xmap, 
                 nodes[elements[imap].emap[ii]].ymap, 
                 nodes[elements[imap].emap[ii]].zmap,
                 nodes[elements[imap].emap[ii]].vmap);
        }
      }
    }
  }

  // In checking mode, verify the tetrahedron/triangle count.
  if (checkMultipleElement) {
    if (nfound < 1) {
      if (debug) {
        printf("ComponentFieldMap::FindElement13:\n");
        printf("    No element matching point (%g,%g,%g) found.\n", x, y, z);
      }
      lastElement = -1;
      return -1;
    }
    if (nfound > 1) {
      printf("ComponentFieldMap::FindElement13:\n");
      printf("    Found %d elements matching point (%g,%g,%g).\n", nfound, x, y, z);
    }
    if (nfound > 0) {
      for (int j = 0; j < 4; ++j) {
        for (int k = 0; k < 4; ++k) jac[j][k] = jacbak[j][k];
      }
      det = detbak; t1 = t1bak; t2 = t2bak; t3 = t3bak; t4 = t4bak; imap = imapbak;
      lastElement = imap;
      return imap;
    }
  }

  if (debug) {
    printf("ComponentFieldMap::FindElement13:\n");
    printf("   No element matching point (%g,%g,%g) found.\n", x, y, z);
  }
  return -1;

}

void 
ComponentFieldMap::Jacobian3(int i, double u, double v, double w, double& det, double jac[4][4]) {

  // Initial values
  det = 0;
  jac[0][0] = 0; jac[0][1] = 0;
  jac[1][0] = 0; jac[1][1] = 0;

  // Be sure that the element is within range
  if (i < 0 || i >= nElements) {
    printf("ComponentFieldMap::Jacobian3:\n");
    printf("    Element %d out of range.\n", i);
    return;
  }
  
  // Determinant of the quadratic triangular Jacobian
  det =
    -(((-1+4*v)*nodes[elements[i].emap[1]].xmap+nodes[elements[i].emap[2]].xmap-4*w*nodes[elements[i].emap[2]].xmap+
       4*u*nodes[elements[i].emap[3]].xmap-4*u*nodes[elements[i].emap[4]].xmap-4*v*nodes[elements[i].emap[5]].xmap+4*w*nodes[elements[i].emap[5]].xmap)*
      (-nodes[elements[i].emap[0]].ymap+4*u*nodes[elements[i].emap[0]].ymap+4*v*nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[4]].ymap))-
    ((-1+4*u)*nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[1]].xmap-4*v*nodes[elements[i].emap[1]].xmap-
     4*u*nodes[elements[i].emap[3]].xmap+4*v*nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[4]].xmap-4*w*nodes[elements[i].emap[5]].xmap)*
    (-nodes[elements[i].emap[2]].ymap+4*w*nodes[elements[i].emap[2]].ymap+4*u*nodes[elements[i].emap[4]].ymap+4*v*nodes[elements[i].emap[5]].ymap)+
    ((-1+4*u)*nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[2]].xmap-4*w*nodes[elements[i].emap[2]].xmap+
     4*v*nodes[elements[i].emap[3]].xmap-4*u*nodes[elements[i].emap[4]].xmap+4*w*nodes[elements[i].emap[4]].xmap-4*v*nodes[elements[i].emap[5]].xmap)*
    (-nodes[elements[i].emap[1]].ymap+4*v*nodes[elements[i].emap[1]].ymap+4*u*nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[5]].ymap);

  // Terms of the quadratic triangular Jacobian
  jac[0][0] =
    (-nodes[elements[i].emap[1]].xmap+4*v*nodes[elements[i].emap[1]].xmap+4*u*nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[5]].xmap)*
    (-nodes[elements[i].emap[2]].ymap+4*w*nodes[elements[i].emap[2]].ymap+4*u*nodes[elements[i].emap[4]].ymap+4*v*nodes[elements[i].emap[5]].ymap)-
    (-nodes[elements[i].emap[2]].xmap+4*w*nodes[elements[i].emap[2]].xmap+4*u*nodes[elements[i].emap[4]].xmap+4*v*nodes[elements[i].emap[5]].xmap)*
    (-nodes[elements[i].emap[1]].ymap+4*v*nodes[elements[i].emap[1]].ymap+4*u*nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[5]].ymap);
  jac[0][1] = 
    (-1+4*v)*nodes[elements[i].emap[1]].ymap+nodes[elements[i].emap[2]].ymap-4*w*nodes[elements[i].emap[2]].ymap+
    4*u*nodes[elements[i].emap[3]].ymap-4*u*nodes[elements[i].emap[4]].ymap-4*v*nodes[elements[i].emap[5]].ymap+4*w*nodes[elements[i].emap[5]].ymap;
  jac[0][2] = 
    nodes[elements[i].emap[1]].xmap-4*v*nodes[elements[i].emap[1]].xmap+(-1+4*w)*nodes[elements[i].emap[2]].xmap-
    4*u*nodes[elements[i].emap[3]].xmap+4*u*nodes[elements[i].emap[4]].xmap+4*v*nodes[elements[i].emap[5]].xmap-4*w*nodes[elements[i].emap[5]].xmap;
  jac[1][0] =
    (-nodes[elements[i].emap[2]].xmap+4*w*nodes[elements[i].emap[2]].xmap+4*u*nodes[elements[i].emap[4]].xmap+4*v*nodes[elements[i].emap[5]].xmap)*
    (-nodes[elements[i].emap[0]].ymap+4*u*nodes[elements[i].emap[0]].ymap+4*v*nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[4]].ymap)-
    (-nodes[elements[i].emap[0]].xmap+4*u*nodes[elements[i].emap[0]].xmap+4*v*nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[4]].xmap)*
    (-nodes[elements[i].emap[2]].ymap+4*w*nodes[elements[i].emap[2]].ymap+4*u*nodes[elements[i].emap[4]].ymap+4*v*nodes[elements[i].emap[5]].ymap);
  jac[1][1] =
    nodes[elements[i].emap[0]].ymap-4*u*nodes[elements[i].emap[0]].ymap-nodes[elements[i].emap[2]].ymap+4*w*nodes[elements[i].emap[2]].ymap-
    4*v*nodes[elements[i].emap[3]].ymap+4*u*nodes[elements[i].emap[4]].ymap-4*w*nodes[elements[i].emap[4]].ymap+4*v*nodes[elements[i].emap[5]].ymap;
  jac[1][2] =
    (-1+4*u)*nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[2]].xmap-4*w*nodes[elements[i].emap[2]].xmap+
    4*v*nodes[elements[i].emap[3]].xmap-4*u*nodes[elements[i].emap[4]].xmap+4*w*nodes[elements[i].emap[4]].xmap-4*v*nodes[elements[i].emap[5]].xmap;
  jac[2][0] =
    -((-nodes[elements[i].emap[1]].xmap+4*v*nodes[elements[i].emap[1]].xmap+4*u*nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[5]].xmap)*
      (-nodes[elements[i].emap[0]].ymap+4*u*nodes[elements[i].emap[0]].ymap+4*v*nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[4]].ymap))+
    (-nodes[elements[i].emap[0]].xmap+4*u*nodes[elements[i].emap[0]].xmap+4*v*nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[4]].xmap)*
    (-nodes[elements[i].emap[1]].ymap+4*v*nodes[elements[i].emap[1]].ymap+4*u*nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[5]].ymap);
  jac[2][1] =
    (-1+4*u)*nodes[elements[i].emap[0]].ymap+nodes[elements[i].emap[1]].ymap-4*v*nodes[elements[i].emap[1]].ymap-
    4*u*nodes[elements[i].emap[3]].ymap+4*v*nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[4]].ymap-4*w*nodes[elements[i].emap[5]].ymap;
  jac[2][2] =
    nodes[elements[i].emap[0]].xmap-4*u*nodes[elements[i].emap[0]].xmap-nodes[elements[i].emap[1]].xmap+4*v*nodes[elements[i].emap[1]].xmap+
    4*u*nodes[elements[i].emap[3]].xmap-4*v*nodes[elements[i].emap[3]].xmap-4*w*nodes[elements[i].emap[4]].xmap+4*w*nodes[elements[i].emap[5]].xmap;

}

void ComponentFieldMap::Jacobian5(int i, double u, double v, double& det, double jac[4][4]) {

  // Initial values
  det = 0;
  jac[0][0] = 0; jac[0][1] = 0;
  jac[1][0] = 0; jac[1][1] = 0;

  // Be sure that the element is within range
  if (i < 0 || i >= nElements) {
    printf("ComponentFieldMap::Jacobian5:\n");
    printf("    Element %d out of range.\n", i);
    return;
  }  

  // Determinant of the quadrilateral serendipity Jacobian
  det = 
     (-2*u*u*u*((nodes[elements[i].emap[2]].xmap+nodes[elements[i].emap[3]].xmap-2*nodes[elements[i].emap[6]].xmap)*
     (nodes[elements[i].emap[0]].ymap+nodes[elements[i].emap[1]].ymap-2*nodes[elements[i].emap[4]].ymap)-(nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[1]].xmap-
     2*nodes[elements[i].emap[4]].xmap)*(nodes[elements[i].emap[2]].ymap+nodes[elements[i].emap[3]].ymap-2*nodes[elements[i].emap[6]].ymap))+
     2*v*v*v*(-((nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[3]].xmap-2*nodes[elements[i].emap[7]].xmap)*
     (nodes[elements[i].emap[1]].ymap+nodes[elements[i].emap[2]].ymap-2*nodes[elements[i].emap[5]].ymap))+
     (nodes[elements[i].emap[1]].xmap+nodes[elements[i].emap[2]].xmap-2*nodes[elements[i].emap[5]].xmap)*(nodes[elements[i].emap[0]].ymap+nodes[elements[i].emap[3]].ymap-
     2*nodes[elements[i].emap[7]].ymap))+2*(-((nodes[elements[i].emap[5]].xmap-nodes[elements[i].emap[7]].xmap)*
     (nodes[elements[i].emap[4]].ymap-nodes[elements[i].emap[6]].ymap))+(nodes[elements[i].emap[4]].xmap-nodes[elements[i].emap[6]].xmap)*
     (nodes[elements[i].emap[5]].ymap-nodes[elements[i].emap[7]].ymap))+v*(-(nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[0]].ymap)-
     2*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[0]].ymap+nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[1]].ymap-
     2*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[1]].ymap-nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[2]].ymap-
     2*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[2]].ymap+nodes[elements[i].emap[4]].xmap*(nodes[elements[i].emap[0]].ymap-nodes[elements[i].emap[1]].ymap+
     nodes[elements[i].emap[2]].ymap-nodes[elements[i].emap[3]].ymap)+nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[3]].ymap-
     2*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[3]].ymap-nodes[elements[i].emap[0]].xmap*nodes[elements[i].emap[4]].ymap+
     nodes[elements[i].emap[1]].xmap*nodes[elements[i].emap[4]].ymap-nodes[elements[i].emap[2]].xmap*nodes[elements[i].emap[4]].ymap+
     nodes[elements[i].emap[3]].xmap*nodes[elements[i].emap[4]].ymap-2*nodes[elements[i].emap[0]].xmap*nodes[elements[i].emap[5]].ymap-
     2*nodes[elements[i].emap[1]].xmap*nodes[elements[i].emap[5]].ymap-2*nodes[elements[i].emap[2]].xmap*nodes[elements[i].emap[5]].ymap-
     2*nodes[elements[i].emap[3]].xmap*nodes[elements[i].emap[5]].ymap+8*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[5]].ymap+
     nodes[elements[i].emap[0]].xmap*nodes[elements[i].emap[6]].ymap-nodes[elements[i].emap[1]].xmap*nodes[elements[i].emap[6]].ymap+
     nodes[elements[i].emap[2]].xmap*nodes[elements[i].emap[6]].ymap-nodes[elements[i].emap[3]].xmap*nodes[elements[i].emap[6]].ymap+
     2*nodes[elements[i].emap[5]].xmap*(nodes[elements[i].emap[0]].ymap+nodes[elements[i].emap[1]].ymap+nodes[elements[i].emap[2]].ymap+nodes[elements[i].emap[3]].ymap-
     4*nodes[elements[i].emap[7]].ymap)+2*(nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[1]].xmap+nodes[elements[i].emap[2]].xmap+
     nodes[elements[i].emap[3]].xmap)*nodes[elements[i].emap[7]].ymap)+v*v*(-(nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[0]].ymap)+
     2*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[0]].ymap+nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[0]].ymap+
     2*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[0]].ymap+nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[1]].ymap-
     2*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[1]].ymap-nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[1]].ymap-
     2*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[1]].ymap+nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[2]].ymap+
     2*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[2]].ymap-nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[2]].ymap+
     2*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[2]].ymap-nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[3]].ymap-
     2*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[3]].ymap+nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[3]].ymap-
     2*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[3]].ymap+
     2*nodes[elements[i].emap[2]].xmap*(nodes[elements[i].emap[1]].ymap+nodes[elements[i].emap[3]].ymap)-nodes[elements[i].emap[2]].xmap*nodes[elements[i].emap[4]].ymap+
     2*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[4]].ymap-2*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[4]].ymap-
     2*nodes[elements[i].emap[2]].xmap*nodes[elements[i].emap[5]].ymap-2*nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[5]].ymap+
     2*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[5]].ymap+nodes[elements[i].emap[2]].xmap*nodes[elements[i].emap[6]].ymap-
     2*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[6]].ymap+2*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[6]].ymap+
     nodes[elements[i].emap[0]].xmap*(2*nodes[elements[i].emap[1]].ymap+2*nodes[elements[i].emap[3]].ymap+nodes[elements[i].emap[4]].ymap-
     2*nodes[elements[i].emap[5]].ymap-nodes[elements[i].emap[6]].ymap-2*nodes[elements[i].emap[7]].ymap)-
     2*(nodes[elements[i].emap[2]].xmap-nodes[elements[i].emap[4]].xmap+nodes[elements[i].emap[6]].xmap)*nodes[elements[i].emap[7]].ymap+nodes[elements[i].emap[3]].xmap*
     (-2*nodes[elements[i].emap[0]].ymap-2*nodes[elements[i].emap[2]].ymap+nodes[elements[i].emap[4]].ymap+
     2*nodes[elements[i].emap[5]].ymap-nodes[elements[i].emap[6]].ymap+2*nodes[elements[i].emap[7]].ymap)+
     nodes[elements[i].emap[1]].xmap*(-2*nodes[elements[i].emap[0]].ymap-2*nodes[elements[i].emap[2]].ymap-nodes[elements[i].emap[4]].ymap+
     2*nodes[elements[i].emap[5]].ymap+nodes[elements[i].emap[6]].ymap+2*nodes[elements[i].emap[7]].ymap))+
     u*(nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[0]].ymap-2*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[0]].ymap-
     nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[0]].ymap-nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[1]].ymap-
     2*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[1]].ymap+nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[1]].ymap+
     nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[2]].ymap-2*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[2]].ymap-
     nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[2]].ymap-nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[3]].ymap-
     2*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[3]].ymap+nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[3]].ymap-
     2*nodes[elements[i].emap[1]].xmap*nodes[elements[i].emap[4]].ymap-2*nodes[elements[i].emap[2]].xmap*nodes[elements[i].emap[4]].ymap-
     2*nodes[elements[i].emap[3]].xmap*nodes[elements[i].emap[4]].ymap+8*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[4]].ymap+
     nodes[elements[i].emap[1]].xmap*nodes[elements[i].emap[5]].ymap-nodes[elements[i].emap[2]].xmap*nodes[elements[i].emap[5]].ymap+
     nodes[elements[i].emap[3]].xmap*nodes[elements[i].emap[5]].ymap+2*nodes[elements[i].emap[4]].xmap*(nodes[elements[i].emap[0]].ymap+
     nodes[elements[i].emap[1]].ymap+nodes[elements[i].emap[2]].ymap+nodes[elements[i].emap[3]].ymap-4*nodes[elements[i].emap[6]].ymap)+
     2*nodes[elements[i].emap[1]].xmap*nodes[elements[i].emap[6]].ymap+2*nodes[elements[i].emap[2]].xmap*nodes[elements[i].emap[6]].ymap+
     2*nodes[elements[i].emap[3]].xmap*nodes[elements[i].emap[6]].ymap-(nodes[elements[i].emap[1]].xmap-nodes[elements[i].emap[2]].xmap+
     nodes[elements[i].emap[3]].xmap)*nodes[elements[i].emap[7]].ymap+nodes[elements[i].emap[0]].xmap*
     (-2*nodes[elements[i].emap[4]].ymap-nodes[elements[i].emap[5]].ymap+2*nodes[elements[i].emap[6]].ymap+nodes[elements[i].emap[7]].ymap)+
     v*v*(4*nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[0]].ymap-3*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[0]].ymap-
     4*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[0]].ymap-5*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[0]].ymap+
     4*nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[1]].ymap-5*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[1]].ymap-
     4*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[1]].ymap-3*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[1]].ymap+
     4*nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[2]].ymap+5*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[2]].ymap-
     4*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[2]].ymap+3*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[2]].ymap+
     4*nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[3]].ymap+3*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[3]].ymap-
     4*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[3]].ymap+5*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[3]].ymap+
     8*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[4]].ymap+8*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[4]].ymap-
     8*nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[5]].ymap+8*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[5]].ymap-
     8*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[6]].ymap-8*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[6]].ymap+
     nodes[elements[i].emap[3]].xmap*(5*nodes[elements[i].emap[0]].ymap+3*nodes[elements[i].emap[1]].ymap-4*nodes[elements[i].emap[4]].ymap-
     3*nodes[elements[i].emap[5]].ymap+4*nodes[elements[i].emap[6]].ymap-5*nodes[elements[i].emap[7]].ymap)+
     nodes[elements[i].emap[2]].xmap*(3*nodes[elements[i].emap[0]].ymap+5*nodes[elements[i].emap[1]].ymap-4*nodes[elements[i].emap[4]].ymap-
     5*nodes[elements[i].emap[5]].ymap+4*nodes[elements[i].emap[6]].ymap-3*nodes[elements[i].emap[7]].ymap)-
     8*nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[7]].ymap+8*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[7]].ymap+
     nodes[elements[i].emap[1]].xmap*(-5*nodes[elements[i].emap[2]].ymap-3*nodes[elements[i].emap[3]].ymap-4*nodes[elements[i].emap[4]].ymap+
     5*nodes[elements[i].emap[5]].ymap+4*nodes[elements[i].emap[6]].ymap+3*nodes[elements[i].emap[7]].ymap)+
     nodes[elements[i].emap[0]].xmap*(-3*nodes[elements[i].emap[2]].ymap-5*nodes[elements[i].emap[3]].ymap-4*nodes[elements[i].emap[4]].ymap+
     3*nodes[elements[i].emap[5]].ymap+4*nodes[elements[i].emap[6]].ymap+5*nodes[elements[i].emap[7]].ymap))-
     2*v*(nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[0]].ymap-3*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[0]].ymap+
     nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[1]].ymap-nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[1]].ymap+
     3*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[2]].ymap-nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[2]].ymap+
     3*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[3]].ymap-3*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[3]].ymap-
     3*nodes[elements[i].emap[0]].xmap*nodes[elements[i].emap[4]].ymap-3*nodes[elements[i].emap[1]].xmap*nodes[elements[i].emap[4]].ymap-
     nodes[elements[i].emap[2]].xmap*nodes[elements[i].emap[4]].ymap-nodes[elements[i].emap[3]].xmap*nodes[elements[i].emap[4]].ymap+
     4*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[4]].ymap+nodes[elements[i].emap[0]].xmap*nodes[elements[i].emap[5]].ymap+
     3*nodes[elements[i].emap[1]].xmap*nodes[elements[i].emap[5]].ymap+3*nodes[elements[i].emap[2]].xmap*nodes[elements[i].emap[5]].ymap+
     nodes[elements[i].emap[3]].xmap*nodes[elements[i].emap[5]].ymap-4*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[5]].ymap-
     nodes[elements[i].emap[0]].xmap*nodes[elements[i].emap[6]].ymap-nodes[elements[i].emap[1]].xmap*nodes[elements[i].emap[6]].ymap-
     3*nodes[elements[i].emap[2]].xmap*nodes[elements[i].emap[6]].ymap-3*nodes[elements[i].emap[3]].xmap*nodes[elements[i].emap[6]].ymap+
     4*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[6]].ymap-nodes[elements[i].emap[5]].xmap*(nodes[elements[i].emap[0]].ymap+
     3*nodes[elements[i].emap[1]].ymap+3*nodes[elements[i].emap[2]].ymap+nodes[elements[i].emap[3]].ymap-
     4*(nodes[elements[i].emap[4]].ymap+nodes[elements[i].emap[6]].ymap))+(3*nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[1]].xmap+
     nodes[elements[i].emap[2]].xmap+3*nodes[elements[i].emap[3]].xmap-4*nodes[elements[i].emap[6]].xmap)*nodes[elements[i].emap[7]].ymap+
     nodes[elements[i].emap[4]].xmap*(3*nodes[elements[i].emap[0]].ymap+3*nodes[elements[i].emap[1]].ymap+nodes[elements[i].emap[2]].ymap+
     nodes[elements[i].emap[3]].ymap-4*(nodes[elements[i].emap[5]].ymap+nodes[elements[i].emap[7]].ymap))))+
     u*u*(2*nodes[elements[i].emap[3]].xmap*nodes[elements[i].emap[0]].ymap-2*nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[0]].ymap-
     nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[0]].ymap-2*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[0]].ymap+
     nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[0]].ymap-2*nodes[elements[i].emap[0]].xmap*nodes[elements[i].emap[1]].ymap+
     2*nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[1]].ymap-nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[1]].ymap+
     2*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[1]].ymap+nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[1]].ymap+
     2*nodes[elements[i].emap[3]].xmap*nodes[elements[i].emap[2]].ymap-2*nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[2]].ymap+
     nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[2]].ymap-2*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[2]].ymap-
     nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[2]].ymap+2*nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[3]].ymap+
     nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[3]].ymap+2*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[3]].ymap-
     nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[3]].ymap-2*nodes[elements[i].emap[3]].xmap*nodes[elements[i].emap[4]].ymap+
     2*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[4]].ymap-2*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[4]].ymap-
     nodes[elements[i].emap[3]].xmap*nodes[elements[i].emap[5]].ymap-2*nodes[elements[i].emap[4]].xmap*nodes[elements[i].emap[5]].ymap+
     2*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[5]].ymap-2*nodes[elements[i].emap[3]].xmap*nodes[elements[i].emap[6]].ymap-
     2*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[6]].ymap+2*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[6]].ymap+
     nodes[elements[i].emap[0]].xmap*(-2*nodes[elements[i].emap[3]].ymap+2*nodes[elements[i].emap[4]].ymap+nodes[elements[i].emap[5]].ymap+
     2*nodes[elements[i].emap[6]].ymap-nodes[elements[i].emap[7]].ymap)+(nodes[elements[i].emap[3]].xmap+2*nodes[elements[i].emap[4]].xmap-
     2*nodes[elements[i].emap[6]].xmap)*nodes[elements[i].emap[7]].ymap+nodes[elements[i].emap[2]].xmap*(-2*nodes[elements[i].emap[1]].ymap-
     2*nodes[elements[i].emap[3]].ymap+2*nodes[elements[i].emap[4]].ymap-nodes[elements[i].emap[5]].ymap+
     2*nodes[elements[i].emap[6]].ymap+nodes[elements[i].emap[7]].ymap)-3*v*v*(nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[0]].ymap-
     nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[0]].ymap-nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[0]].ymap+
     nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[1]].ymap+nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[1]].ymap-
     nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[1]].ymap-nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[2]].ymap+
     nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[2]].ymap+nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[2]].ymap-
     nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[3]].ymap-nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[3]].ymap+
     nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[3]].ymap-2*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[4]].ymap+
     2*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[4]].ymap-2*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[5]].ymap+
     2*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[6]].ymap-2*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[6]].ymap+
     nodes[elements[i].emap[4]].xmap*(nodes[elements[i].emap[0]].ymap-nodes[elements[i].emap[1]].ymap-nodes[elements[i].emap[2]].ymap+
     nodes[elements[i].emap[3]].ymap+2*nodes[elements[i].emap[5]].ymap-2*nodes[elements[i].emap[7]].ymap)+
     nodes[elements[i].emap[3]].xmap*(nodes[elements[i].emap[0]].ymap-nodes[elements[i].emap[2]].ymap-nodes[elements[i].emap[4]].ymap+
     nodes[elements[i].emap[5]].ymap+nodes[elements[i].emap[6]].ymap-nodes[elements[i].emap[7]].ymap)+2*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[7]].ymap+
     (nodes[elements[i].emap[0]].xmap-nodes[elements[i].emap[2]].xmap)*(nodes[elements[i].emap[1]].ymap-nodes[elements[i].emap[3]].ymap-nodes[elements[i].emap[4]].ymap-
     nodes[elements[i].emap[5]].ymap+nodes[elements[i].emap[6]].ymap+nodes[elements[i].emap[7]].ymap))+
     v*(4*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[0]].ymap+3*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[0]].ymap-
     4*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[0]].ymap+4*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[1]].ymap-
     3*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[1]].ymap-4*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[1]].ymap+
     4*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[2]].ymap-5*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[2]].ymap-
     4*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[2]].ymap+4*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[3]].ymap+
     5*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[3]].ymap-4*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[3]].ymap-
     8*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[4]].ymap+8*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[4]].ymap+
     8*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[5]].ymap-8*nodes[elements[i].emap[5]].xmap*nodes[elements[i].emap[6]].ymap+
     8*nodes[elements[i].emap[7]].xmap*nodes[elements[i].emap[6]].ymap+nodes[elements[i].emap[4]].xmap*(5*nodes[elements[i].emap[0]].ymap-
     5*nodes[elements[i].emap[1]].ymap-3*nodes[elements[i].emap[2]].ymap+3*nodes[elements[i].emap[3]].ymap+8*nodes[elements[i].emap[5]].ymap-
     8*nodes[elements[i].emap[7]].ymap)-8*nodes[elements[i].emap[6]].xmap*nodes[elements[i].emap[7]].ymap+
     nodes[elements[i].emap[3]].xmap*(3*nodes[elements[i].emap[1]].ymap+5*nodes[elements[i].emap[2]].ymap-3*nodes[elements[i].emap[4]].ymap-
     4*nodes[elements[i].emap[5]].ymap-5*nodes[elements[i].emap[6]].ymap+4*nodes[elements[i].emap[7]].ymap)+
     nodes[elements[i].emap[0]].xmap*(5*nodes[elements[i].emap[1]].ymap+3*nodes[elements[i].emap[2]].ymap-5*nodes[elements[i].emap[4]].ymap-
     4*nodes[elements[i].emap[5]].ymap-3*nodes[elements[i].emap[6]].ymap+4*nodes[elements[i].emap[7]].ymap)+nodes[elements[i].emap[2]].xmap*
     (-3*nodes[elements[i].emap[0]].ymap-5*nodes[elements[i].emap[3]].ymap+3*nodes[elements[i].emap[4]].ymap-4*nodes[elements[i].emap[5]].ymap+
     5*nodes[elements[i].emap[6]].ymap+4*nodes[elements[i].emap[7]].ymap))+nodes[elements[i].emap[1]].xmap*((-1+v)*(-2+3*v)*
     nodes[elements[i].emap[0]].ymap+2*nodes[elements[i].emap[2]].ymap-2*nodes[elements[i].emap[4]].ymap+nodes[elements[i].emap[5]].ymap-
     2*nodes[elements[i].emap[6]].ymap-nodes[elements[i].emap[7]].ymap+v*(-3*nodes[elements[i].emap[3]].ymap+5*nodes[elements[i].emap[4]].ymap-
     4*nodes[elements[i].emap[5]].ymap+3*nodes[elements[i].emap[6]].ymap+4*nodes[elements[i].emap[7]].ymap-
     3*v*(nodes[elements[i].emap[2]].ymap+nodes[elements[i].emap[4]].ymap-nodes[elements[i].emap[5]].ymap-nodes[elements[i].emap[6]].ymap+
	  nodes[elements[i].emap[7]].ymap)))))/8;
  // Jacobian terms
  jac[0][0] = (u*u*(-nodes[elements[i].emap[0]].ymap-nodes[elements[i].emap[1]].ymap+nodes[elements[i].emap[2]].ymap+
     nodes[elements[i].emap[3]].ymap+2*nodes[elements[i].emap[4]].ymap-2*nodes[elements[i].emap[6]].ymap)+2*(-nodes[elements[i].emap[4]].ymap+
     nodes[elements[i].emap[6]].ymap+v*(nodes[elements[i].emap[0]].ymap+nodes[elements[i].emap[1]].ymap+nodes[elements[i].emap[2]].ymap+nodes[elements[i].emap[3]].ymap-
     2*nodes[elements[i].emap[5]].ymap-2*nodes[elements[i].emap[7]].ymap))+u*(nodes[elements[i].emap[0]].ymap-2*v*nodes[elements[i].emap[0]].ymap-
     nodes[elements[i].emap[1]].ymap+2*v*nodes[elements[i].emap[1]].ymap+nodes[elements[i].emap[2]].ymap+2*v*nodes[elements[i].emap[2]].ymap-
				      nodes[elements[i].emap[3]].ymap-2*v*nodes[elements[i].emap[3]].ymap-4*v*nodes[elements[i].emap[5]].ymap+4*v*nodes[elements[i].emap[7]].ymap))/4;
  jac[0][1] = (u*u*(nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[1]].xmap-nodes[elements[i].emap[2]].xmap-
     nodes[elements[i].emap[3]].xmap-2*nodes[elements[i].emap[4]].xmap+2*nodes[elements[i].emap[6]].xmap)-2*(-nodes[elements[i].emap[4]].xmap+
     nodes[elements[i].emap[6]].xmap+v*(nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[1]].xmap+nodes[elements[i].emap[2]].xmap+nodes[elements[i].emap[3]].xmap-
     2*nodes[elements[i].emap[5]].xmap-2*nodes[elements[i].emap[7]].xmap))+u*((-1+2*v)*nodes[elements[i].emap[0]].xmap+
     nodes[elements[i].emap[1]].xmap-2*v*nodes[elements[i].emap[1]].xmap-nodes[elements[i].emap[2]].xmap-2*v*nodes[elements[i].emap[2]].xmap+
				      nodes[elements[i].emap[3]].xmap+2*v*nodes[elements[i].emap[3]].xmap+4*v*nodes[elements[i].emap[5]].xmap-4*v*nodes[elements[i].emap[7]].xmap))/4;
  jac[1][0] = (v*(-nodes[elements[i].emap[0]].ymap+nodes[elements[i].emap[1]].ymap-nodes[elements[i].emap[2]].ymap+nodes[elements[i].emap[3]].ymap)-
     2*nodes[elements[i].emap[5]].ymap+2*u*((-1+v)*nodes[elements[i].emap[0]].ymap+(-1+v)*nodes[elements[i].emap[1]].ymap-
     nodes[elements[i].emap[2]].ymap-v*nodes[elements[i].emap[2]].ymap-nodes[elements[i].emap[3]].ymap-v*nodes[elements[i].emap[3]].ymap+
     2*nodes[elements[i].emap[4]].ymap-2*v*nodes[elements[i].emap[4]].ymap+2*nodes[elements[i].emap[6]].ymap+2*v*nodes[elements[i].emap[6]].ymap)+
     v*v*(nodes[elements[i].emap[0]].ymap-nodes[elements[i].emap[1]].ymap-nodes[elements[i].emap[2]].ymap+nodes[elements[i].emap[3]].ymap+
	  2*nodes[elements[i].emap[5]].ymap-2*nodes[elements[i].emap[7]].ymap)+2*nodes[elements[i].emap[7]].ymap)/4;
  jac[1][1] =(v*(nodes[elements[i].emap[0]].xmap-nodes[elements[i].emap[1]].xmap+nodes[elements[i].emap[2]].xmap-nodes[elements[i].emap[3]].xmap)+
     2*u*(nodes[elements[i].emap[0]].xmap-v*nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[1]].xmap-v*nodes[elements[i].emap[1]].xmap+
     nodes[elements[i].emap[2]].xmap+v*nodes[elements[i].emap[2]].xmap+nodes[elements[i].emap[3]].xmap+v*nodes[elements[i].emap[3]].xmap-
     2*nodes[elements[i].emap[4]].xmap+2*v*nodes[elements[i].emap[4]].xmap-2*nodes[elements[i].emap[6]].xmap-2*v*nodes[elements[i].emap[6]].xmap)+
     2*(nodes[elements[i].emap[5]].xmap-nodes[elements[i].emap[7]].xmap)+v*v*(-nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[1]].xmap+
				      nodes[elements[i].emap[2]].xmap-nodes[elements[i].emap[3]].xmap-2*nodes[elements[i].emap[5]].xmap+2*nodes[elements[i].emap[7]].xmap))/4;

}

void 
ComponentFieldMap::Jacobian13(int i, double t, double u, double v, double w, double& det, double jac[4][4]) {

  // Initial values
  det = 0;
  for (int j = 0; j < 4; ++j) {
    for (int k = 0; k < 4; ++k) jac[j][k] = 0;
  }

  // Be sure that the element is within range
  if (i < 0 || i >= nElements) {
    printf("ComponentFieldMap::Jacobian13:\n");
    printf("    Element %d out of range.\n", i);
    return;
  }

  // Determinant of the quadrilateral serendipity Jacobian
det = 
-(((-4*v*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[1]].xmap+4*u*nodes[elements[i].emap[1]].xmap+nodes[elements[i].emap[3]].xmap-
4*w*nodes[elements[i].emap[3]].xmap+4*t*nodes[elements[i].emap[4]].xmap-4*t*nodes[elements[i].emap[6]].xmap+4*v*nodes[elements[i].emap[7]].xmap-
4*u*nodes[elements[i].emap[8]].xmap+4*w*nodes[elements[i].emap[8]].xmap)*
(4*w*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[2]].ymap+4*v*nodes[elements[i].emap[2]].ymap+4*t*nodes[elements[i].emap[5]].ymap+
4*u*nodes[elements[i].emap[7]].ymap)+
(nodes[elements[i].emap[1]].xmap-4*u*nodes[elements[i].emap[1]].xmap-nodes[elements[i].emap[2]].xmap+4*v*nodes[elements[i].emap[2]].xmap-
4*t*nodes[elements[i].emap[4]].xmap+4*t*nodes[elements[i].emap[5]].xmap+4*u*nodes[elements[i].emap[7]].xmap-4*v*nodes[elements[i].emap[7]].xmap+
4*w*(nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[8]].xmap))*
(4*v*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[3]].ymap+4*t*nodes[elements[i].emap[6]].ymap+
4*u*nodes[elements[i].emap[8]].ymap)+
(-4*w*nodes[elements[i].emap[9]].xmap+4*v*(nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[2]].xmap)+nodes[elements[i].emap[2]].xmap-
nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[3]].xmap-4*t*nodes[elements[i].emap[5]].xmap+4*t*nodes[elements[i].emap[6]].xmap-
4*u*nodes[elements[i].emap[7]].xmap+4*u*nodes[elements[i].emap[8]].xmap)*
((-1+4*u)*nodes[elements[i].emap[1]].ymap+4*(t*nodes[elements[i].emap[4]].ymap+v*nodes[elements[i].emap[7]].ymap+
w*nodes[elements[i].emap[8]].ymap)))*((-1+4*t)*nodes[elements[i].emap[0]].zmap+4*(u*nodes[elements[i].emap[4]].zmap+
v*nodes[elements[i].emap[5]].zmap+w*nodes[elements[i].emap[6]].zmap)))-
((nodes[elements[i].emap[1]].xmap-4*u*nodes[elements[i].emap[1]].xmap-nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[3]].xmap-
4*t*nodes[elements[i].emap[4]].xmap+4*t*nodes[elements[i].emap[6]].xmap+4*v*(nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[7]].xmap)+
4*u*nodes[elements[i].emap[8]].xmap-4*w*nodes[elements[i].emap[8]].xmap)*
((-1+4*t)*nodes[elements[i].emap[0]].ymap+4*(u*nodes[elements[i].emap[4]].ymap+v*nodes[elements[i].emap[5]].ymap+
w*nodes[elements[i].emap[6]].ymap))-
((-1+4*t)*nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[1]].xmap-4*u*nodes[elements[i].emap[1]].xmap+
4*(-(t*nodes[elements[i].emap[4]].xmap)+u*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[5]].xmap+w*nodes[elements[i].emap[6]].xmap-
v*nodes[elements[i].emap[7]].xmap-w*nodes[elements[i].emap[8]].xmap))*
(4*v*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[3]].ymap+4*t*nodes[elements[i].emap[6]].ymap+
4*u*nodes[elements[i].emap[8]].ymap)+
((-1+4*t)*nodes[elements[i].emap[0]].xmap-4*v*nodes[elements[i].emap[9]].xmap+nodes[elements[i].emap[3]].xmap-
4*w*nodes[elements[i].emap[3]].xmap+4*u*nodes[elements[i].emap[4]].xmap+4*v*nodes[elements[i].emap[5]].xmap-4*t*nodes[elements[i].emap[6]].xmap+
4*w*nodes[elements[i].emap[6]].xmap-4*u*nodes[elements[i].emap[8]].xmap)*
((-1+4*u)*nodes[elements[i].emap[1]].ymap+4*(t*nodes[elements[i].emap[4]].ymap+v*nodes[elements[i].emap[7]].ymap+
w*nodes[elements[i].emap[8]].ymap)))*(4*w*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[2]].zmap+4*v*nodes[elements[i].emap[2]].zmap+
4*t*nodes[elements[i].emap[5]].zmap+4*u*nodes[elements[i].emap[7]].zmap)+
((nodes[elements[i].emap[1]].xmap-4*u*nodes[elements[i].emap[1]].xmap-nodes[elements[i].emap[2]].xmap+4*v*nodes[elements[i].emap[2]].xmap-
4*t*nodes[elements[i].emap[4]].xmap+4*t*nodes[elements[i].emap[5]].xmap+4*u*nodes[elements[i].emap[7]].xmap-4*v*nodes[elements[i].emap[7]].xmap+
4*w*(nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[8]].xmap))*
((-1+4*t)*nodes[elements[i].emap[0]].ymap+4*(u*nodes[elements[i].emap[4]].ymap+v*nodes[elements[i].emap[5]].ymap+
w*nodes[elements[i].emap[6]].ymap))-
((-1+4*t)*nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[1]].xmap-4*u*nodes[elements[i].emap[1]].xmap+
4*(-(t*nodes[elements[i].emap[4]].xmap)+u*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[5]].xmap+w*nodes[elements[i].emap[6]].xmap-
v*nodes[elements[i].emap[7]].xmap-w*nodes[elements[i].emap[8]].xmap))*
(4*w*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[2]].ymap+4*v*nodes[elements[i].emap[2]].ymap+4*t*nodes[elements[i].emap[5]].ymap+
4*u*nodes[elements[i].emap[7]].ymap)+
((-1+4*t)*nodes[elements[i].emap[0]].xmap-4*w*nodes[elements[i].emap[9]].xmap+nodes[elements[i].emap[2]].xmap-
4*v*nodes[elements[i].emap[2]].xmap+4*u*nodes[elements[i].emap[4]].xmap-4*t*nodes[elements[i].emap[5]].xmap+4*v*nodes[elements[i].emap[5]].xmap+
4*w*nodes[elements[i].emap[6]].xmap-4*u*nodes[elements[i].emap[7]].xmap)*
((-1+4*u)*nodes[elements[i].emap[1]].ymap+4*(t*nodes[elements[i].emap[4]].ymap+v*nodes[elements[i].emap[7]].ymap+
w*nodes[elements[i].emap[8]].ymap)))*(4*v*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[3]].zmap+4*w*nodes[elements[i].emap[3]].zmap+
4*t*nodes[elements[i].emap[6]].zmap+4*u*nodes[elements[i].emap[8]].zmap)+
((-4*w*nodes[elements[i].emap[9]].xmap+4*v*(nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[2]].xmap)+nodes[elements[i].emap[2]].xmap-
nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[3]].xmap-4*t*nodes[elements[i].emap[5]].xmap+4*t*nodes[elements[i].emap[6]].xmap-
4*u*nodes[elements[i].emap[7]].xmap+4*u*nodes[elements[i].emap[8]].xmap)*
((-1+4*t)*nodes[elements[i].emap[0]].ymap+4*(u*nodes[elements[i].emap[4]].ymap+v*nodes[elements[i].emap[5]].ymap+
w*nodes[elements[i].emap[6]].ymap))+
((-1+4*t)*nodes[elements[i].emap[0]].xmap-4*v*nodes[elements[i].emap[9]].xmap+nodes[elements[i].emap[3]].xmap-
4*w*nodes[elements[i].emap[3]].xmap+4*u*nodes[elements[i].emap[4]].xmap+4*v*nodes[elements[i].emap[5]].xmap-4*t*nodes[elements[i].emap[6]].xmap+
4*w*nodes[elements[i].emap[6]].xmap-4*u*nodes[elements[i].emap[8]].xmap)*
(4*w*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[2]].ymap+4*v*nodes[elements[i].emap[2]].ymap+4*t*nodes[elements[i].emap[5]].ymap+
4*u*nodes[elements[i].emap[7]].ymap)-
((-1+4*t)*nodes[elements[i].emap[0]].xmap-4*w*nodes[elements[i].emap[9]].xmap+nodes[elements[i].emap[2]].xmap-
4*v*nodes[elements[i].emap[2]].xmap+4*u*nodes[elements[i].emap[4]].xmap-4*t*nodes[elements[i].emap[5]].xmap+4*v*nodes[elements[i].emap[5]].xmap+
4*w*nodes[elements[i].emap[6]].xmap-4*u*nodes[elements[i].emap[7]].xmap)*
(4*v*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[3]].ymap+4*t*nodes[elements[i].emap[6]].ymap+
4*u*nodes[elements[i].emap[8]].ymap))*((-1+4*u)*nodes[elements[i].emap[1]].zmap+4*(t*nodes[elements[i].emap[4]].zmap+
						     v*nodes[elements[i].emap[7]].zmap+w*nodes[elements[i].emap[8]].zmap));

jac[0][0] =
-((((-1+4*u)*nodes[elements[i].emap[1]].xmap+4*(t*nodes[elements[i].emap[4]].xmap+
v*nodes[elements[i].emap[7]].xmap+w*nodes[elements[i].emap[8]].xmap))*
(4*v*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[3]].ymap+
4*t*nodes[elements[i].emap[6]].ymap+4*u*nodes[elements[i].emap[8]].ymap)-
(4*v*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[3]].xmap+4*t*nodes[elements[i].emap[6]].xmap+
4*u*nodes[elements[i].emap[8]].xmap)*((-1+4*u)*nodes[elements[i].emap[1]].ymap+4*(t*nodes[elements[i].emap[4]].ymap+
v*nodes[elements[i].emap[7]].ymap+w*nodes[elements[i].emap[8]].ymap)))*
(4*w*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[2]].zmap+4*v*nodes[elements[i].emap[2]].zmap+4*t*nodes[elements[i].emap[5]].zmap+
4*u*nodes[elements[i].emap[7]].zmap))+
(((-1+4*u)*nodes[elements[i].emap[1]].xmap+4*(t*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[7]].xmap+
w*nodes[elements[i].emap[8]].xmap))*(4*w*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[2]].ymap+4*v*nodes[elements[i].emap[2]].ymap+
4*t*nodes[elements[i].emap[5]].ymap+4*u*nodes[elements[i].emap[7]].ymap)-
(4*w*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[2]].xmap+4*v*nodes[elements[i].emap[2]].xmap+4*t*nodes[elements[i].emap[5]].xmap+
4*u*nodes[elements[i].emap[7]].xmap)*((-1+4*u)*nodes[elements[i].emap[1]].ymap+4*(t*nodes[elements[i].emap[4]].ymap+
v*nodes[elements[i].emap[7]].ymap+w*nodes[elements[i].emap[8]].ymap)))*
(4*v*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[3]].zmap+4*w*nodes[elements[i].emap[3]].zmap+4*t*nodes[elements[i].emap[6]].zmap+
4*u*nodes[elements[i].emap[8]].zmap)+
(-((4*v*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[3]].xmap+4*t*nodes[elements[i].emap[6]].xmap+
4*u*nodes[elements[i].emap[8]].xmap)*(4*w*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[2]].ymap+4*v*nodes[elements[i].emap[2]].ymap+
4*t*nodes[elements[i].emap[5]].ymap+4*u*nodes[elements[i].emap[7]].ymap))+
(4*w*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[2]].xmap+4*v*nodes[elements[i].emap[2]].xmap+4*t*nodes[elements[i].emap[5]].xmap+
4*u*nodes[elements[i].emap[7]].xmap)*(4*v*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[3]].ymap+
4*t*nodes[elements[i].emap[6]].ymap+4*u*nodes[elements[i].emap[8]].ymap))*
((-1+4*u)*nodes[elements[i].emap[1]].zmap+4*(t*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[7]].zmap+
			      w*nodes[elements[i].emap[8]].zmap));

jac[0][1] =
(nodes[elements[i].emap[1]].ymap-4*u*nodes[elements[i].emap[1]].ymap-nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[3]].ymap-
4*t*nodes[elements[i].emap[4]].ymap+4*t*nodes[elements[i].emap[6]].ymap+4*v*(nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[7]].ymap)+
4*u*nodes[elements[i].emap[8]].ymap-4*w*nodes[elements[i].emap[8]].ymap)*
(4*w*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[2]].zmap+4*v*nodes[elements[i].emap[2]].zmap+4*t*nodes[elements[i].emap[5]].zmap+
4*u*nodes[elements[i].emap[7]].zmap)+
(-4*w*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[1]].ymap+4*u*nodes[elements[i].emap[1]].ymap+nodes[elements[i].emap[2]].ymap-
4*v*nodes[elements[i].emap[2]].ymap+4*t*nodes[elements[i].emap[4]].ymap-4*t*nodes[elements[i].emap[5]].ymap-4*u*nodes[elements[i].emap[7]].ymap+
4*v*nodes[elements[i].emap[7]].ymap+4*w*nodes[elements[i].emap[8]].ymap)*
(4*v*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[3]].zmap+4*w*nodes[elements[i].emap[3]].zmap+4*t*nodes[elements[i].emap[6]].zmap+
4*u*nodes[elements[i].emap[8]].zmap)+
(-4*v*nodes[elements[i].emap[9]].ymap+4*w*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[2]].ymap+4*v*nodes[elements[i].emap[2]].ymap+
nodes[elements[i].emap[3]].ymap-4*w*nodes[elements[i].emap[3]].ymap+4*t*nodes[elements[i].emap[5]].ymap-4*t*nodes[elements[i].emap[6]].ymap+
4*u*nodes[elements[i].emap[7]].ymap-4*u*nodes[elements[i].emap[8]].ymap)*
((-1+4*u)*nodes[elements[i].emap[1]].zmap+4*(t*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[7]].zmap+
			      w*nodes[elements[i].emap[8]].zmap));

jac[0][2] =
(-4*v*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[1]].xmap+4*u*nodes[elements[i].emap[1]].xmap+nodes[elements[i].emap[3]].xmap-
4*w*nodes[elements[i].emap[3]].xmap+4*t*nodes[elements[i].emap[4]].xmap-4*t*nodes[elements[i].emap[6]].xmap+4*v*nodes[elements[i].emap[7]].xmap-
4*u*nodes[elements[i].emap[8]].xmap+4*w*nodes[elements[i].emap[8]].xmap)*
(4*w*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[2]].zmap+4*v*nodes[elements[i].emap[2]].zmap+4*t*nodes[elements[i].emap[5]].zmap+
4*u*nodes[elements[i].emap[7]].zmap)+
(nodes[elements[i].emap[1]].xmap-4*u*nodes[elements[i].emap[1]].xmap-nodes[elements[i].emap[2]].xmap+4*v*nodes[elements[i].emap[2]].xmap-
4*t*nodes[elements[i].emap[4]].xmap+4*t*nodes[elements[i].emap[5]].xmap+4*u*nodes[elements[i].emap[7]].xmap-4*v*nodes[elements[i].emap[7]].xmap+
4*w*(nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[8]].xmap))*
(4*v*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[3]].zmap+4*w*nodes[elements[i].emap[3]].zmap+4*t*nodes[elements[i].emap[6]].zmap+
4*u*nodes[elements[i].emap[8]].zmap)+
(-4*w*nodes[elements[i].emap[9]].xmap+4*v*(nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[2]].xmap)+nodes[elements[i].emap[2]].xmap-
nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[3]].xmap-4*t*nodes[elements[i].emap[5]].xmap+4*t*nodes[elements[i].emap[6]].xmap-
4*u*nodes[elements[i].emap[7]].xmap+4*u*nodes[elements[i].emap[8]].xmap)*
((-1+4*u)*nodes[elements[i].emap[1]].zmap+4*(t*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[7]].zmap+
			      w*nodes[elements[i].emap[8]].zmap));

jac[0][3] =
(nodes[elements[i].emap[1]].xmap-4*u*nodes[elements[i].emap[1]].xmap-nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[3]].xmap-
4*t*nodes[elements[i].emap[4]].xmap+4*t*nodes[elements[i].emap[6]].xmap+4*v*(nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[7]].xmap)+
4*u*nodes[elements[i].emap[8]].xmap-4*w*nodes[elements[i].emap[8]].xmap)*
(4*w*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[2]].ymap+4*v*nodes[elements[i].emap[2]].ymap+4*t*nodes[elements[i].emap[5]].ymap+
4*u*nodes[elements[i].emap[7]].ymap)+
(-4*w*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[1]].xmap+4*u*nodes[elements[i].emap[1]].xmap+nodes[elements[i].emap[2]].xmap-
4*v*nodes[elements[i].emap[2]].xmap+4*t*nodes[elements[i].emap[4]].xmap-4*t*nodes[elements[i].emap[5]].xmap-4*u*nodes[elements[i].emap[7]].xmap+
4*v*nodes[elements[i].emap[7]].xmap+4*w*nodes[elements[i].emap[8]].xmap)*
(4*v*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[3]].ymap+4*t*nodes[elements[i].emap[6]].ymap+
4*u*nodes[elements[i].emap[8]].ymap)+
(-4*v*nodes[elements[i].emap[9]].xmap+4*w*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[2]].xmap+4*v*nodes[elements[i].emap[2]].xmap+
nodes[elements[i].emap[3]].xmap-4*w*nodes[elements[i].emap[3]].xmap+4*t*nodes[elements[i].emap[5]].xmap-4*t*nodes[elements[i].emap[6]].xmap+
4*u*nodes[elements[i].emap[7]].xmap-4*u*nodes[elements[i].emap[8]].xmap)*
((-1+4*u)*nodes[elements[i].emap[1]].ymap+4*(t*nodes[elements[i].emap[4]].ymap+v*nodes[elements[i].emap[7]].ymap+
			      w*nodes[elements[i].emap[8]].ymap));

jac[1][0] =
-((-((4*v*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[3]].xmap+
4*w*nodes[elements[i].emap[3]].xmap+4*t*nodes[elements[i].emap[6]].xmap+4*u*nodes[elements[i].emap[8]].xmap)*
(4*w*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[2]].ymap+4*v*nodes[elements[i].emap[2]].ymap+
4*t*nodes[elements[i].emap[5]].ymap+4*u*nodes[elements[i].emap[7]].ymap))+
(4*w*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[2]].xmap+4*v*nodes[elements[i].emap[2]].xmap+4*t*nodes[elements[i].emap[5]].xmap+
4*u*nodes[elements[i].emap[7]].xmap)*(4*v*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[3]].ymap+
4*t*nodes[elements[i].emap[6]].ymap+4*u*nodes[elements[i].emap[8]].ymap))*
((-1+4*t)*nodes[elements[i].emap[0]].zmap+4*(u*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[5]].zmap+
w*nodes[elements[i].emap[6]].zmap)))+
(-((4*v*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[3]].xmap+4*t*nodes[elements[i].emap[6]].xmap+
4*u*nodes[elements[i].emap[8]].xmap)*((-1+4*t)*nodes[elements[i].emap[0]].ymap+4*(u*nodes[elements[i].emap[4]].ymap+
v*nodes[elements[i].emap[5]].ymap+w*nodes[elements[i].emap[6]].ymap)))+
((-1+4*t)*nodes[elements[i].emap[0]].xmap+4*(u*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[5]].xmap+
w*nodes[elements[i].emap[6]].xmap))*(4*v*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[3]].ymap+
4*t*nodes[elements[i].emap[6]].ymap+4*u*nodes[elements[i].emap[8]].ymap))*
(4*w*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[2]].zmap+4*v*nodes[elements[i].emap[2]].zmap+4*t*nodes[elements[i].emap[5]].zmap+
4*u*nodes[elements[i].emap[7]].zmap)-
(-((4*w*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[2]].xmap+4*v*nodes[elements[i].emap[2]].xmap+4*t*nodes[elements[i].emap[5]].xmap+
4*u*nodes[elements[i].emap[7]].xmap)*((-1+4*t)*nodes[elements[i].emap[0]].ymap+4*(u*nodes[elements[i].emap[4]].ymap+
v*nodes[elements[i].emap[5]].ymap+w*nodes[elements[i].emap[6]].ymap)))+
((-1+4*t)*nodes[elements[i].emap[0]].xmap+4*(u*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[5]].xmap+
w*nodes[elements[i].emap[6]].xmap))*(4*w*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[2]].ymap+4*v*nodes[elements[i].emap[2]].ymap+
4*t*nodes[elements[i].emap[5]].ymap+4*u*nodes[elements[i].emap[7]].ymap))*
(4*v*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[3]].zmap+4*w*nodes[elements[i].emap[3]].zmap+4*t*nodes[elements[i].emap[6]].zmap+
 4*u*nodes[elements[i].emap[8]].zmap);

jac[1][1] =
(-4*w*nodes[elements[i].emap[9]].ymap+4*v*(nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[2]].ymap)+nodes[elements[i].emap[2]].ymap-
nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[3]].ymap-4*t*nodes[elements[i].emap[5]].ymap+4*t*nodes[elements[i].emap[6]].ymap-
4*u*nodes[elements[i].emap[7]].ymap+4*u*nodes[elements[i].emap[8]].ymap)*
((-1+4*t)*nodes[elements[i].emap[0]].zmap+4*(u*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[5]].zmap+
w*nodes[elements[i].emap[6]].zmap))+
((-1+4*t)*nodes[elements[i].emap[0]].ymap-4*v*nodes[elements[i].emap[9]].ymap+nodes[elements[i].emap[3]].ymap-4*w*nodes[elements[i].emap[3]].ymap+
4*u*nodes[elements[i].emap[4]].ymap+4*v*nodes[elements[i].emap[5]].ymap-4*t*nodes[elements[i].emap[6]].ymap+4*w*nodes[elements[i].emap[6]].ymap-
4*u*nodes[elements[i].emap[8]].ymap)*
(4*w*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[2]].zmap+4*v*nodes[elements[i].emap[2]].zmap+4*t*nodes[elements[i].emap[5]].zmap+
4*u*nodes[elements[i].emap[7]].zmap)-
((-1+4*t)*nodes[elements[i].emap[0]].ymap-4*w*nodes[elements[i].emap[9]].ymap+nodes[elements[i].emap[2]].ymap-4*v*nodes[elements[i].emap[2]].ymap+
4*u*nodes[elements[i].emap[4]].ymap-4*t*nodes[elements[i].emap[5]].ymap+4*v*nodes[elements[i].emap[5]].ymap+4*w*nodes[elements[i].emap[6]].ymap-
4*u*nodes[elements[i].emap[7]].ymap)*
(4*v*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[3]].zmap+4*w*nodes[elements[i].emap[3]].zmap+4*t*nodes[elements[i].emap[6]].zmap+
 4*u*nodes[elements[i].emap[8]].zmap);

jac[1][2] =
(-4*v*nodes[elements[i].emap[9]].xmap+4*w*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[2]].xmap+4*v*nodes[elements[i].emap[2]].xmap+
nodes[elements[i].emap[3]].xmap-4*w*nodes[elements[i].emap[3]].xmap+4*t*nodes[elements[i].emap[5]].xmap-4*t*nodes[elements[i].emap[6]].xmap+
4*u*nodes[elements[i].emap[7]].xmap-4*u*nodes[elements[i].emap[8]].xmap)*
((-1+4*t)*nodes[elements[i].emap[0]].zmap+4*(u*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[5]].zmap+
w*nodes[elements[i].emap[6]].zmap))-
((-1+4*t)*nodes[elements[i].emap[0]].xmap-4*v*nodes[elements[i].emap[9]].xmap+nodes[elements[i].emap[3]].xmap-4*w*nodes[elements[i].emap[3]].xmap+
4*u*nodes[elements[i].emap[4]].xmap+4*v*nodes[elements[i].emap[5]].xmap-4*t*nodes[elements[i].emap[6]].xmap+4*w*nodes[elements[i].emap[6]].xmap-
4*u*nodes[elements[i].emap[8]].xmap)*
(4*w*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[2]].zmap+4*v*nodes[elements[i].emap[2]].zmap+4*t*nodes[elements[i].emap[5]].zmap+
4*u*nodes[elements[i].emap[7]].zmap)+
((-1+4*t)*nodes[elements[i].emap[0]].xmap-4*w*nodes[elements[i].emap[9]].xmap+nodes[elements[i].emap[2]].xmap-4*v*nodes[elements[i].emap[2]].xmap+
4*u*nodes[elements[i].emap[4]].xmap-4*t*nodes[elements[i].emap[5]].xmap+4*v*nodes[elements[i].emap[5]].xmap+4*w*nodes[elements[i].emap[6]].xmap-
4*u*nodes[elements[i].emap[7]].xmap)*
(4*v*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[3]].zmap+4*w*nodes[elements[i].emap[3]].zmap+4*t*nodes[elements[i].emap[6]].zmap+
 4*u*nodes[elements[i].emap[8]].zmap);

jac[1][3] =
(-4*w*nodes[elements[i].emap[9]].xmap+4*v*(nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[2]].xmap)+nodes[elements[i].emap[2]].xmap-
nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[3]].xmap-4*t*nodes[elements[i].emap[5]].xmap+4*t*nodes[elements[i].emap[6]].xmap-
4*u*nodes[elements[i].emap[7]].xmap+4*u*nodes[elements[i].emap[8]].xmap)*
((-1+4*t)*nodes[elements[i].emap[0]].ymap+4*(u*nodes[elements[i].emap[4]].ymap+v*nodes[elements[i].emap[5]].ymap+
w*nodes[elements[i].emap[6]].ymap))+
((-1+4*t)*nodes[elements[i].emap[0]].xmap-4*v*nodes[elements[i].emap[9]].xmap+nodes[elements[i].emap[3]].xmap-4*w*nodes[elements[i].emap[3]].xmap+
4*u*nodes[elements[i].emap[4]].xmap+4*v*nodes[elements[i].emap[5]].xmap-4*t*nodes[elements[i].emap[6]].xmap+4*w*nodes[elements[i].emap[6]].xmap-
4*u*nodes[elements[i].emap[8]].xmap)*
(4*w*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[2]].ymap+4*v*nodes[elements[i].emap[2]].ymap+4*t*nodes[elements[i].emap[5]].ymap+
4*u*nodes[elements[i].emap[7]].ymap)-
((-1+4*t)*nodes[elements[i].emap[0]].xmap-4*w*nodes[elements[i].emap[9]].xmap+nodes[elements[i].emap[2]].xmap-4*v*nodes[elements[i].emap[2]].xmap+
4*u*nodes[elements[i].emap[4]].xmap-4*t*nodes[elements[i].emap[5]].xmap+4*v*nodes[elements[i].emap[5]].xmap+4*w*nodes[elements[i].emap[6]].xmap-
4*u*nodes[elements[i].emap[7]].xmap)*
(4*v*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[3]].ymap+4*t*nodes[elements[i].emap[6]].ymap+
 4*u*nodes[elements[i].emap[8]].ymap);

jac[2][0] =
(((-1+4*u)*nodes[elements[i].emap[1]].xmap+4*(t*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[7]].xmap+
w*nodes[elements[i].emap[8]].xmap))*(4*v*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[3]].ymap+
4*t*nodes[elements[i].emap[6]].ymap+4*u*nodes[elements[i].emap[8]].ymap)-
(4*v*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[3]].xmap+4*t*nodes[elements[i].emap[6]].xmap+
4*u*nodes[elements[i].emap[8]].xmap)*((-1+4*u)*nodes[elements[i].emap[1]].ymap+4*(t*nodes[elements[i].emap[4]].ymap+
v*nodes[elements[i].emap[7]].ymap+w*nodes[elements[i].emap[8]].ymap)))*
((-1+4*t)*nodes[elements[i].emap[0]].zmap+4*(u*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[5]].zmap+
w*nodes[elements[i].emap[6]].zmap))+
(-(((-1+4*u)*nodes[elements[i].emap[1]].xmap+4*(t*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[7]].xmap+
w*nodes[elements[i].emap[8]].xmap))*((-1+4*t)*nodes[elements[i].emap[0]].ymap+4*(u*nodes[elements[i].emap[4]].ymap+
v*nodes[elements[i].emap[5]].ymap+w*nodes[elements[i].emap[6]].ymap)))+
((-1+4*t)*nodes[elements[i].emap[0]].xmap+4*(u*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[5]].xmap+
w*nodes[elements[i].emap[6]].xmap))*((-1+4*u)*nodes[elements[i].emap[1]].ymap+4*(t*nodes[elements[i].emap[4]].ymap+
v*nodes[elements[i].emap[7]].ymap+w*nodes[elements[i].emap[8]].ymap)))*
(4*v*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[3]].zmap+4*w*nodes[elements[i].emap[3]].zmap+4*t*nodes[elements[i].emap[6]].zmap+
4*u*nodes[elements[i].emap[8]].zmap)-
(-((4*v*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[3]].xmap+4*t*nodes[elements[i].emap[6]].xmap+
4*u*nodes[elements[i].emap[8]].xmap)*((-1+4*t)*nodes[elements[i].emap[0]].ymap+4*(u*nodes[elements[i].emap[4]].ymap+
v*nodes[elements[i].emap[5]].ymap+w*nodes[elements[i].emap[6]].ymap)))+
((-1+4*t)*nodes[elements[i].emap[0]].xmap+4*(u*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[5]].xmap+
w*nodes[elements[i].emap[6]].xmap))*(4*v*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[3]].ymap+
4*t*nodes[elements[i].emap[6]].ymap+4*u*nodes[elements[i].emap[8]].ymap))*
((-1+4*u)*nodes[elements[i].emap[1]].zmap+4*(t*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[7]].zmap+
			      w*nodes[elements[i].emap[8]].zmap));

jac[2][1] =
(-4*v*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[1]].ymap+4*u*nodes[elements[i].emap[1]].ymap+nodes[elements[i].emap[3]].ymap-
4*w*nodes[elements[i].emap[3]].ymap+4*t*nodes[elements[i].emap[4]].ymap-4*t*nodes[elements[i].emap[6]].ymap+4*v*nodes[elements[i].emap[7]].ymap-
4*u*nodes[elements[i].emap[8]].ymap+4*w*nodes[elements[i].emap[8]].ymap)*
((-1+4*t)*nodes[elements[i].emap[0]].zmap+4*(u*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[5]].zmap+
w*nodes[elements[i].emap[6]].zmap))+
((-1+4*t)*nodes[elements[i].emap[0]].ymap+nodes[elements[i].emap[1]].ymap-4*u*nodes[elements[i].emap[1]].ymap+
4*(-(t*nodes[elements[i].emap[4]].ymap)+u*nodes[elements[i].emap[4]].ymap+v*nodes[elements[i].emap[5]].ymap+w*nodes[elements[i].emap[6]].ymap-
v*nodes[elements[i].emap[7]].ymap-w*nodes[elements[i].emap[8]].ymap))*
(4*v*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[3]].zmap+4*w*nodes[elements[i].emap[3]].zmap+4*t*nodes[elements[i].emap[6]].zmap+
4*u*nodes[elements[i].emap[8]].zmap)-
((-1+4*t)*nodes[elements[i].emap[0]].ymap-4*v*nodes[elements[i].emap[9]].ymap+nodes[elements[i].emap[3]].ymap-4*w*nodes[elements[i].emap[3]].ymap+
4*u*nodes[elements[i].emap[4]].ymap+4*v*nodes[elements[i].emap[5]].ymap-4*t*nodes[elements[i].emap[6]].ymap+4*w*nodes[elements[i].emap[6]].ymap-
4*u*nodes[elements[i].emap[8]].ymap)*
((-1+4*u)*nodes[elements[i].emap[1]].zmap+4*(t*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[7]].zmap+
			      w*nodes[elements[i].emap[8]].zmap));

jac[2][2] =
(nodes[elements[i].emap[1]].xmap-4*u*nodes[elements[i].emap[1]].xmap-nodes[elements[i].emap[3]].xmap+4*w*nodes[elements[i].emap[3]].xmap-
4*t*nodes[elements[i].emap[4]].xmap+4*t*nodes[elements[i].emap[6]].xmap+4*v*(nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[7]].xmap)+
4*u*nodes[elements[i].emap[8]].xmap-4*w*nodes[elements[i].emap[8]].xmap)*
((-1+4*t)*nodes[elements[i].emap[0]].zmap+4*(u*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[5]].zmap+
w*nodes[elements[i].emap[6]].zmap))-
((-1+4*t)*nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[1]].xmap-4*u*nodes[elements[i].emap[1]].xmap+
4*(-(t*nodes[elements[i].emap[4]].xmap)+u*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[5]].xmap+w*nodes[elements[i].emap[6]].xmap-
v*nodes[elements[i].emap[7]].xmap-w*nodes[elements[i].emap[8]].xmap))*
(4*v*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[3]].zmap+4*w*nodes[elements[i].emap[3]].zmap+4*t*nodes[elements[i].emap[6]].zmap+
4*u*nodes[elements[i].emap[8]].zmap)+
((-1+4*t)*nodes[elements[i].emap[0]].xmap-4*v*nodes[elements[i].emap[9]].xmap+nodes[elements[i].emap[3]].xmap-4*w*nodes[elements[i].emap[3]].xmap+
4*u*nodes[elements[i].emap[4]].xmap+4*v*nodes[elements[i].emap[5]].xmap-4*t*nodes[elements[i].emap[6]].xmap+4*w*nodes[elements[i].emap[6]].xmap-
4*u*nodes[elements[i].emap[8]].xmap)*
((-1+4*u)*nodes[elements[i].emap[1]].zmap+4*(t*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[7]].zmap+
			      w*nodes[elements[i].emap[8]].zmap));

jac[2][3] =
(-4*v*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[1]].xmap+4*u*nodes[elements[i].emap[1]].xmap+nodes[elements[i].emap[3]].xmap-
4*w*nodes[elements[i].emap[3]].xmap+4*t*nodes[elements[i].emap[4]].xmap-4*t*nodes[elements[i].emap[6]].xmap+4*v*nodes[elements[i].emap[7]].xmap-
4*u*nodes[elements[i].emap[8]].xmap+4*w*nodes[elements[i].emap[8]].xmap)*
((-1+4*t)*nodes[elements[i].emap[0]].ymap+4*(u*nodes[elements[i].emap[4]].ymap+v*nodes[elements[i].emap[5]].ymap+
w*nodes[elements[i].emap[6]].ymap))+
((-1+4*t)*nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[1]].xmap-4*u*nodes[elements[i].emap[1]].xmap+
4*(-(t*nodes[elements[i].emap[4]].xmap)+u*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[5]].xmap+w*nodes[elements[i].emap[6]].xmap-
v*nodes[elements[i].emap[7]].xmap-w*nodes[elements[i].emap[8]].xmap))*
(4*v*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[3]].ymap+4*w*nodes[elements[i].emap[3]].ymap+4*t*nodes[elements[i].emap[6]].ymap+
4*u*nodes[elements[i].emap[8]].ymap)-
((-1+4*t)*nodes[elements[i].emap[0]].xmap-4*v*nodes[elements[i].emap[9]].xmap+nodes[elements[i].emap[3]].xmap-4*w*nodes[elements[i].emap[3]].xmap+
4*u*nodes[elements[i].emap[4]].xmap+4*v*nodes[elements[i].emap[5]].xmap-4*t*nodes[elements[i].emap[6]].xmap+4*w*nodes[elements[i].emap[6]].xmap-
4*u*nodes[elements[i].emap[8]].xmap)*
((-1+4*u)*nodes[elements[i].emap[1]].ymap+4*(t*nodes[elements[i].emap[4]].ymap+v*nodes[elements[i].emap[7]].ymap+
			      w*nodes[elements[i].emap[8]].ymap));

jac[3][0] =
-((((-1+4*u)*nodes[elements[i].emap[1]].xmap+4*(t*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[7]].xmap+
w*nodes[elements[i].emap[8]].xmap))*
(4*w*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[2]].ymap+4*v*nodes[elements[i].emap[2]].ymap+
4*t*nodes[elements[i].emap[5]].ymap+4*u*nodes[elements[i].emap[7]].ymap)-
(4*w*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[2]].xmap+4*v*nodes[elements[i].emap[2]].xmap+4*t*nodes[elements[i].emap[5]].xmap+
4*u*nodes[elements[i].emap[7]].xmap)*((-1+4*u)*nodes[elements[i].emap[1]].ymap+4*(t*nodes[elements[i].emap[4]].ymap+
v*nodes[elements[i].emap[7]].ymap+w*nodes[elements[i].emap[8]].ymap)))*
((-1+4*t)*nodes[elements[i].emap[0]].zmap+4*(u*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[5]].zmap+
w*nodes[elements[i].emap[6]].zmap)))-
(-(((-1+4*u)*nodes[elements[i].emap[1]].xmap+4*(t*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[7]].xmap+
w*nodes[elements[i].emap[8]].xmap))*((-1+4*t)*nodes[elements[i].emap[0]].ymap+4*(u*nodes[elements[i].emap[4]].ymap+
v*nodes[elements[i].emap[5]].ymap+w*nodes[elements[i].emap[6]].ymap)))+
((-1+4*t)*nodes[elements[i].emap[0]].xmap+4*(u*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[5]].xmap+
w*nodes[elements[i].emap[6]].xmap))*((-1+4*u)*nodes[elements[i].emap[1]].ymap+4*(t*nodes[elements[i].emap[4]].ymap+
v*nodes[elements[i].emap[7]].ymap+w*nodes[elements[i].emap[8]].ymap)))*
(4*w*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[2]].zmap+4*v*nodes[elements[i].emap[2]].zmap+4*t*nodes[elements[i].emap[5]].zmap+
4*u*nodes[elements[i].emap[7]].zmap)+
(-((4*w*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[2]].xmap+4*v*nodes[elements[i].emap[2]].xmap+4*t*nodes[elements[i].emap[5]].xmap+
4*u*nodes[elements[i].emap[7]].xmap)*((-1+4*t)*nodes[elements[i].emap[0]].ymap+4*(u*nodes[elements[i].emap[4]].ymap+
v*nodes[elements[i].emap[5]].ymap+w*nodes[elements[i].emap[6]].ymap)))+
((-1+4*t)*nodes[elements[i].emap[0]].xmap+4*(u*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[5]].xmap+
w*nodes[elements[i].emap[6]].xmap))*(4*w*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[2]].ymap+4*v*nodes[elements[i].emap[2]].ymap+
4*t*nodes[elements[i].emap[5]].ymap+4*u*nodes[elements[i].emap[7]].ymap))*
((-1+4*u)*nodes[elements[i].emap[1]].zmap+4*(t*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[7]].zmap+
			      w*nodes[elements[i].emap[8]].zmap));

jac[3][1] = 
(nodes[elements[i].emap[1]].ymap-4*u*nodes[elements[i].emap[1]].ymap-nodes[elements[i].emap[2]].ymap+4*v*nodes[elements[i].emap[2]].ymap-
4*t*nodes[elements[i].emap[4]].ymap+4*t*nodes[elements[i].emap[5]].ymap+4*u*nodes[elements[i].emap[7]].ymap-4*v*nodes[elements[i].emap[7]].ymap+
4*w*(nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[8]].ymap))*
((-1+4*t)*nodes[elements[i].emap[0]].zmap+4*(u*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[5]].zmap+
w*nodes[elements[i].emap[6]].zmap))-
((-1+4*t)*nodes[elements[i].emap[0]].ymap+nodes[elements[i].emap[1]].ymap-4*u*nodes[elements[i].emap[1]].ymap+
4*(-(t*nodes[elements[i].emap[4]].ymap)+u*nodes[elements[i].emap[4]].ymap+v*nodes[elements[i].emap[5]].ymap+w*nodes[elements[i].emap[6]].ymap-
v*nodes[elements[i].emap[7]].ymap-w*nodes[elements[i].emap[8]].ymap))*
(4*w*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[2]].zmap+4*v*nodes[elements[i].emap[2]].zmap+4*t*nodes[elements[i].emap[5]].zmap+
4*u*nodes[elements[i].emap[7]].zmap)+
((-1+4*t)*nodes[elements[i].emap[0]].ymap-4*w*nodes[elements[i].emap[9]].ymap+nodes[elements[i].emap[2]].ymap-4*v*nodes[elements[i].emap[2]].ymap+
4*u*nodes[elements[i].emap[4]].ymap-4*t*nodes[elements[i].emap[5]].ymap+4*v*nodes[elements[i].emap[5]].ymap+4*w*nodes[elements[i].emap[6]].ymap-
4*u*nodes[elements[i].emap[7]].ymap)*
((-1+4*u)*nodes[elements[i].emap[1]].zmap+4*(t*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[7]].zmap+
			      w*nodes[elements[i].emap[8]].zmap));

jac[3][2] =
(-4*w*nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[1]].xmap+4*u*nodes[elements[i].emap[1]].xmap+nodes[elements[i].emap[2]].xmap-
4*v*nodes[elements[i].emap[2]].xmap+4*t*nodes[elements[i].emap[4]].xmap-4*t*nodes[elements[i].emap[5]].xmap-4*u*nodes[elements[i].emap[7]].xmap+
4*v*nodes[elements[i].emap[7]].xmap+4*w*nodes[elements[i].emap[8]].xmap)*
((-1+4*t)*nodes[elements[i].emap[0]].zmap+4*(u*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[5]].zmap+
w*nodes[elements[i].emap[6]].zmap))+
((-1+4*t)*nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[1]].xmap-4*u*nodes[elements[i].emap[1]].xmap+
4*(-(t*nodes[elements[i].emap[4]].xmap)+u*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[5]].xmap+w*nodes[elements[i].emap[6]].xmap-
v*nodes[elements[i].emap[7]].xmap-w*nodes[elements[i].emap[8]].xmap))*
(4*w*nodes[elements[i].emap[9]].zmap-nodes[elements[i].emap[2]].zmap+4*v*nodes[elements[i].emap[2]].zmap+4*t*nodes[elements[i].emap[5]].zmap+
4*u*nodes[elements[i].emap[7]].zmap)-
((-1+4*t)*nodes[elements[i].emap[0]].xmap-4*w*nodes[elements[i].emap[9]].xmap+nodes[elements[i].emap[2]].xmap-4*v*nodes[elements[i].emap[2]].xmap+
4*u*nodes[elements[i].emap[4]].xmap-4*t*nodes[elements[i].emap[5]].xmap+4*v*nodes[elements[i].emap[5]].xmap+
4*w*nodes[elements[i].emap[6]].xmap-4*u*nodes[elements[i].emap[7]].xmap)*
((-1+4*u)*nodes[elements[i].emap[1]].zmap+4*(t*nodes[elements[i].emap[4]].zmap+v*nodes[elements[i].emap[7]].zmap+
			      w*nodes[elements[i].emap[8]].zmap));

jac[3][3] = 
(nodes[elements[i].emap[1]].xmap-4*u*nodes[elements[i].emap[1]].xmap-nodes[elements[i].emap[2]].xmap+4*v*nodes[elements[i].emap[2]].xmap-
4*t*nodes[elements[i].emap[4]].xmap+4*t*nodes[elements[i].emap[5]].xmap+4*u*nodes[elements[i].emap[7]].xmap-4*v*nodes[elements[i].emap[7]].xmap+
4*w*(nodes[elements[i].emap[9]].xmap-nodes[elements[i].emap[8]].xmap))*
((-1+4*t)*nodes[elements[i].emap[0]].ymap+4*(u*nodes[elements[i].emap[4]].ymap+v*nodes[elements[i].emap[5]].ymap+
w*nodes[elements[i].emap[6]].ymap))-
((-1+4*t)*nodes[elements[i].emap[0]].xmap+nodes[elements[i].emap[1]].xmap-4*u*nodes[elements[i].emap[1]].xmap+
4*(-(t*nodes[elements[i].emap[4]].xmap)+u*nodes[elements[i].emap[4]].xmap+v*nodes[elements[i].emap[5]].xmap+w*nodes[elements[i].emap[6]].xmap-
v*nodes[elements[i].emap[7]].xmap-w*nodes[elements[i].emap[8]].xmap))*
(4*w*nodes[elements[i].emap[9]].ymap-nodes[elements[i].emap[2]].ymap+4*v*nodes[elements[i].emap[2]].ymap+4*t*nodes[elements[i].emap[5]].ymap+
4*u*nodes[elements[i].emap[7]].ymap)+
((-1+4*t)*nodes[elements[i].emap[0]].xmap-4*w*nodes[elements[i].emap[9]].xmap+nodes[elements[i].emap[2]].xmap-4*v*nodes[elements[i].emap[2]].xmap+
4*u*nodes[elements[i].emap[4]].xmap-4*t*nodes[elements[i].emap[5]].xmap+4*v*nodes[elements[i].emap[5]].xmap+4*w*nodes[elements[i].emap[6]].xmap-
4*u*nodes[elements[i].emap[7]].xmap)*
((-1+4*u)*nodes[elements[i].emap[1]].ymap+4*(t*nodes[elements[i].emap[4]].ymap+v*nodes[elements[i].emap[7]].ymap+
			      w*nodes[elements[i].emap[8]].ymap));

}

int 
ComponentFieldMap::Coordinates3(double x, double y, double z,
                       double& t1, double& t2, double& t3, double& t4,
                       double jac[4][4], double& det, int imap) {

  if (debug) {
    printf("ComponentFieldMap::Coordinates3:\n");
    printf("   Point (%g,%g).\n", x, y);
  }

  // Failure flag
  int ifail = 1;

  // Provisional values
  t1 = t2 = t3 = t4 = 0;

  // Make a first order approximation, using the linear triangle.
  double tt1 = (x - nodes[elements[imap].emap[1]].xmap) * (nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[1]].ymap) -
               (y - nodes[elements[imap].emap[1]].ymap) * (nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[1]].xmap);
  double tt2 = (x - nodes[elements[imap].emap[2]].xmap) * (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[2]].ymap) -
               (y - nodes[elements[imap].emap[2]].ymap) * (nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[2]].xmap);
  double tt3 = (x - nodes[elements[imap].emap[0]].xmap) * (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[0]].ymap) -
               (y - nodes[elements[imap].emap[0]].ymap) * (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[0]].xmap);
  if ((nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[1]].xmap) * (nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[1]].ymap) -
      (nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[1]].xmap) * (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[1]].ymap) == 0 ||
      (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[2]].xmap) * (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[2]].ymap) -
      (nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[2]].xmap) * (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[2]].ymap) == 0 ||
      (nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[0]].xmap) * (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[0]].ymap) -
      (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[0]].xmap) * (nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[0]].ymap) == 0) {
    printf("ComponentFieldMap::Coordinates3:\n");
    printf("    Calculation of linear coordinates failed; abandoned.\n");
    return ifail;
  } else {
    t1 = tt1 / ((nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[1]].xmap) * (nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[1]].ymap) -
      (nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[1]].xmap) * (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[1]].ymap));
    t2 = tt2 / ((nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[2]].xmap) * (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[2]].ymap) -
      (nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[2]].xmap) * (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[2]].ymap));
    t3 = tt3 / ((nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[0]].xmap) * (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[0]].ymap) -
      (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[0]].xmap) * (nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[0]].ymap));
    t4 = 0;
  }

  // Start iterative refinement
  double td1 = t1, td2 = t2, td3 = t3;
  if (debug) {
    printf("ComponentFieldMap::Coordinates3:\n");
    printf("    Linear estimate:   (u, v, w) = (%g,%g,%g), sum = %g.\n",
           td1,td2,td3,td1 + td2 + td3);
  }

  // Loop
  bool converged = false;
  double diff[2], corr[2];
  for (int iter=0; iter < 10; iter++) {
    if (debug) {
      printf("ComponentFieldMap::Coordinates3:\n");
      printf("    Iteration %3d:     (u, v, w) = (%g,%g,%g), sum = %g.\n",
             iter,td1,td2,td3,td1 + td2 + td3);
    }
    // Re-compute the (x,y,z) position for this coordinate.
    double xr = nodes[elements[imap].emap[0]].xmap * td1 * (2 * td1 - 1) +
      nodes[elements[imap].emap[1]].xmap * td2 * (2 * td2 - 1) +
      nodes[elements[imap].emap[2]].xmap * td3 * (2 * td3 - 1) +
      nodes[elements[imap].emap[3]].xmap * 4 * td1 * td2 +
      nodes[elements[imap].emap[4]].xmap * 4 * td1 * td3 +
      nodes[elements[imap].emap[5]].xmap * 4 * td2 * td3;
    double yr=nodes[elements[imap].emap[0]].ymap * td1 * (2 * td1 - 1) +
      nodes[elements[imap].emap[1]].ymap * td2 * (2 * td2 - 1) +
      nodes[elements[imap].emap[2]].ymap * td3 * (2 * td3 - 1) +
      nodes[elements[imap].emap[3]].ymap * 4 * td1 * td2 +
      nodes[elements[imap].emap[4]].ymap * 4 * td1 * td3 +
      nodes[elements[imap].emap[5]].ymap * 4 * td2 * td3;
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
      printf("ComponentFieldMap::Coordinates3:\n");
      printf("    Difference vector: (1, x, y) = (%g,%g,%g).\n", diff[0], diff[1], diff[2]);
      printf("    Correction vector: (u, v, w) = (%g,%g,%g).\n", corr[0], corr[1], corr[2]);
    }
    // Update the vector.
    td1 += corr[0];
    td2 += corr[1];
    td3 += corr[2];
    // Check for convergence.
    if (fabs(corr[0]) < 1.0e-5 && fabs(corr[1]) < 1.0e-5 && fabs(corr[2]) < 1.0e-5) {
      if (debug) {
        printf("ComponentFieldMap::Coordinates3:\n");
        printf("    Convergence reached.\n");
      }
      converged = true;
      break;
    }
  }
  // No convergence reached
  if (!converged) {
    double xmin, ymin, xmax, ymax;
    xmin = nodes[elements[imap].emap[0]].xmap;
    xmax = nodes[elements[imap].emap[0]].xmap;
    if (nodes[elements[imap].emap[1]].xmap < xmin) {xmin = nodes[elements[imap].emap[1]].xmap;} 
    if (nodes[elements[imap].emap[1]].xmap > xmax) {xmax = nodes[elements[imap].emap[1]].xmap;}
    if (nodes[elements[imap].emap[2]].xmap < xmin) {xmin = nodes[elements[imap].emap[2]].xmap;} 
    if (nodes[elements[imap].emap[2]].xmap > xmax) {xmax = nodes[elements[imap].emap[2]].xmap;}
    ymin = nodes[elements[imap].emap[0]].ymap;
    ymax = nodes[elements[imap].emap[0]].ymap;
    if (nodes[elements[imap].emap[1]].ymap < ymin) {ymin = nodes[elements[imap].emap[1]].ymap;}
    if (nodes[elements[imap].emap[1]].ymap > ymax) {ymax = nodes[elements[imap].emap[1]].ymap;}
    if (nodes[elements[imap].emap[2]].ymap < ymin) {ymin = nodes[elements[imap].emap[2]].ymap;}
    if (nodes[elements[imap].emap[2]].ymap > ymax) {ymax = nodes[elements[imap].emap[2]].ymap;}

    if (x >= xmin && x <= xmax && y >= ymin && y <= ymax) {
      printf("ComponentFieldMap::Coordinates3:\n");
      printf("    No convergence achieved when refining internal isoparametric coordinates\n");
      printf("    in element %d at position (%g,%g).\n",
             imap,x,y);
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
    printf("ComponentFieldMap::Coordinates3:\n");
    printf("    Convergence reached at (t1, t2, t3) = (%g,%g,%g).\n", t1, t2, t3);
  }

  // For debugging purposes, show position
  if (debug) {
    double xr = nodes[elements[imap].emap[0]].xmap * td1 * (2 * td1 - 1) +
                nodes[elements[imap].emap[1]].xmap * td2 * (2 * td2-1) +
                nodes[elements[imap].emap[2]].xmap * td3 * (2 * td3-1) +
                nodes[elements[imap].emap[3]].xmap * 4 * td1 * td2 +
                nodes[elements[imap].emap[4]].xmap * 4 * td1 * td3 +
                nodes[elements[imap].emap[5]].xmap * 4 * td2 * td3;
    double yr = nodes[elements[imap].emap[0]].ymap * td1 * (2 * td1 - 1) +
                nodes[elements[imap].emap[1]].ymap * td2 * (2 * td2 - 1) +
                nodes[elements[imap].emap[2]].ymap * td3 * (2 * td3 - 1) +
                nodes[elements[imap].emap[3]].ymap * 4 * td1 * td2 +    
                nodes[elements[imap].emap[4]].ymap * 4 * td1 * td3 +
                nodes[elements[imap].emap[5]].ymap * 4 * td2 * td3;
    double sr = td1 + td2 + td3;
    printf("ComponentFieldMap::Coordinates3:\n");
    printf("    Position requested:     (%g,%g),\n", x, y);
    printf("    Reconstructed:          (%g,%g),\n", xr, yr);
    printf("    Difference:             (%g,%g).\n", x - xr, y - yr);
    printf("    Checksum - 1:           %g.\n", sr - 1);
  }
  
  // Success
  ifail = 0;
  return ifail;
  
}

int 
ComponentFieldMap::Coordinates4(double x, double y, double z,
			   double& t1, double& t2, double& t3, double& t4,
			   double jac[4][4], double& det, int imap) {

  // Debugging
  if (debug) {
    printf("ComponentFieldMap::Coordinates4:\n");
    printf("    Point (%g,%g).\n", x, y);
  }

  // Failure flag
  int ifail=1;

  // Provisional values
  t1 = 0;
  t2 = 0;
  t3 = 0;
  t4 = 0;

  // Compute determinant.
  det =
    -(-((nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[3]].xmap) * (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[2]].ymap)) + 
      (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[2]].xmap) * (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[3]].ymap))*
    (2*x*(-nodes[elements[imap].emap[0]].ymap+nodes[elements[imap].emap[1]].ymap+nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[3]].ymap) - 
     (nodes[elements[imap].emap[0]].xmap+nodes[elements[imap].emap[3]].xmap) * (nodes[elements[imap].emap[1]].ymap+nodes[elements[imap].emap[2]].ymap-2*y) + 
     nodes[elements[imap].emap[1]].xmap*(nodes[elements[imap].emap[0]].ymap+nodes[elements[imap].emap[3]].ymap-2*y) + nodes[elements[imap].emap[2]].xmap*
     (nodes[elements[imap].emap[0]].ymap+nodes[elements[imap].emap[3]].ymap-2*y)) + 
    pow(-(nodes[elements[imap].emap[0]].xmap*nodes[elements[imap].emap[1]].ymap) + 
	nodes[elements[imap].emap[3]].xmap*nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[2]].xmap*nodes[elements[imap].emap[3]].ymap+
	x*(-nodes[elements[imap].emap[0]].ymap+
           nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[2]].ymap+nodes[elements[imap].emap[3]].ymap) + 
	nodes[elements[imap].emap[1]].xmap*(nodes[elements[imap].emap[0]].ymap-y) + 
	(nodes[elements[imap].emap[0]].xmap+nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[3]].xmap)*y,2);

  // Check that the determinant is non-negative (this can happen if the point is out of range)
  if (det < 0) {
    if (debug) {
      printf("ComponentFieldMap::Coordinates4:\n");
      printf("    No solution found for isoparametric coordinates\n");
      printf("    in element %d because the determinant %g is < 0.\n", imap, det);
    }
    return ifail;
  }

  // Vector products for evaluation of T1.
  double prod=((nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[3]].xmap) * (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[1]].ymap) - 
	       (nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[1]].xmap) * (nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[3]].ymap));
  if (prod*prod > 1.0e-12*
      ((nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[1]].xmap) * (nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[1]].xmap) + 
       (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[1]].ymap) * (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[1]].ymap))*
      ((nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[3]].xmap) * (nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[3]].xmap) + 
       (nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[3]].ymap) * (nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[3]].ymap))) {
    t1 = (-(nodes[elements[imap].emap[3]].xmap*nodes[elements[imap].emap[0]].ymap) + x*nodes[elements[imap].emap[0]].ymap+
	nodes[elements[imap].emap[2]].xmap*nodes[elements[imap].emap[1]].ymap-x*nodes[elements[imap].emap[1]].ymap-
	nodes[elements[imap].emap[1]].xmap*nodes[elements[imap].emap[2]].ymap+
	x*nodes[elements[imap].emap[2]].ymap+nodes[elements[imap].emap[0]].xmap*nodes[elements[imap].emap[3]].ymap-
	x*nodes[elements[imap].emap[3]].ymap - nodes[elements[imap].emap[0]].xmap*y+nodes[elements[imap].emap[1]].xmap*y-
	nodes[elements[imap].emap[2]].xmap*y+nodes[elements[imap].emap[3]].xmap*y+sqrt(det)) / prod;
  } else {
    double xp=nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[1]].ymap;
    double yp=nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[0]].xmap;
    double dn=sqrt(xp*xp+yp*yp);
    if(dn <= 0) {
      printf("ComponentFieldMap::Coordinates4:\n");
      printf("    Element %d appears to be degenerate in the 1 - 2 axis.\n", imap);
      return ifail;
    }
    xp=xp/dn;
    yp=yp/dn;
    double dpoint=xp*(x - nodes[elements[imap].emap[0]].xmap) + yp*(y - nodes[elements[imap].emap[0]].ymap);
    double dbox=  xp*(nodes[elements[imap].emap[3]].xmap - nodes[elements[imap].emap[0]].xmap) + yp*(nodes[elements[imap].emap[3]].ymap - nodes[elements[imap].emap[0]].ymap);
    if (dbox == 0) {
      printf("ComponentFieldMap::Coordinates4:\n");
      printf("    Element %d appears to be degenerate in the 1 - 3 axis.\n", imap);
      return ifail;
    }
    double t=-1 + 2*dpoint/dbox;
    double xt1 = nodes[elements[imap].emap[0]].xmap+0.5*(t+1) * (nodes[elements[imap].emap[3]].xmap - nodes[elements[imap].emap[0]].xmap);
    double yt1 = nodes[elements[imap].emap[0]].ymap+0.5*(t+1) * (nodes[elements[imap].emap[3]].ymap - nodes[elements[imap].emap[0]].ymap);
    double xt2 = nodes[elements[imap].emap[1]].xmap+0.5*(t+1) * (nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[1]].xmap);
    double yt2 = nodes[elements[imap].emap[1]].ymap+0.5*(t+1) * (nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[1]].ymap);
    dn=(xt1 - xt2) * (xt1 - xt2) + (yt1 - yt2) * (yt1 - yt2);
    if(dn <= 0) {
      printf("ComponentFieldMap::Coordinates4:\n");
      printf("    Coordinate requested at convergence point of element %d.\n", imap);
      return ifail;
    }
    t1 = -1 + 2*((x-xt1) * (xt2-xt1) + (y-yt1) * (yt2-yt1)) / dn;
  }

  // Vector products for evaluation of T2.
  prod=((nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[3]].xmap) * (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[2]].ymap) - 
	(nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[2]].xmap) * (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[3]].ymap));
  if (prod*prod > 1.0e-12*
      ((nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[3]].xmap) * (nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[3]].xmap) + 
       (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[3]].ymap) * (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[3]].ymap))*
      ((nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[2]].xmap) * (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[2]].xmap) + 
       (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[2]].ymap) * (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[2]].ymap))) {
    t2 = (-(nodes[elements[imap].emap[1]].xmap*nodes[elements[imap].emap[0]].ymap) + x*nodes[elements[imap].emap[0]].ymap+
	  nodes[elements[imap].emap[0]].xmap*nodes[elements[imap].emap[1]].ymap-x*nodes[elements[imap].emap[1]].ymap-
	  nodes[elements[imap].emap[3]].xmap*nodes[elements[imap].emap[2]].ymap+
	  x*nodes[elements[imap].emap[2]].ymap+nodes[elements[imap].emap[2]].xmap*nodes[elements[imap].emap[3]].ymap-
	  x*nodes[elements[imap].emap[3]].ymap - nodes[elements[imap].emap[0]].xmap*y+nodes[elements[imap].emap[1]].xmap*y-
	  nodes[elements[imap].emap[2]].xmap*y+nodes[elements[imap].emap[3]].xmap*y-sqrt(det)) / prod;
  } else {
    double xp = nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[3]].ymap;
    double yp = nodes[elements[imap].emap[3]].xmap - nodes[elements[imap].emap[0]].xmap;
    double dn = sqrt(xp * xp + yp * yp);
    if(dn <= 0) {
      printf("ComponentFieldMap::Coordinates4:\n");
      printf("    Element %d appears to be degenerate in the 1 - 4 axis.\n", imap);
      return ifail;
    }
    xp = xp / dn;
    yp = yp / dn;
    double dpoint = xp * (x - nodes[elements[imap].emap[0]].xmap) + yp*(y - nodes[elements[imap].emap[0]].ymap);
    double dbox =  xp * (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[0]].xmap) + yp*(nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[0]].ymap);
    if (dbox == 0) {
      printf("ComponentFieldMap::Coordinates4:\n");
      printf("    Element %d appears to be degenerate in the 1 - 2 axis.\n", imap);
      return ifail;
    }
    double t = -1 + 2 * dpoint / dbox;
    double xt1 = nodes[elements[imap].emap[0]].xmap+0.5*(t+1) * (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[0]].xmap);
    double yt1 = nodes[elements[imap].emap[0]].ymap+0.5*(t+1) * (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[0]].ymap);
    double xt2 = nodes[elements[imap].emap[3]].xmap+0.5*(t+1) * (nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[3]].xmap);
    double yt2 = nodes[elements[imap].emap[3]].ymap+0.5*(t+1) * (nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[3]].ymap);
    dn = (xt1 - xt2) * (xt1 - xt2) + (yt1 - yt2) * (yt1 - yt2);
    if(dn <= 0) {
      printf("ComponentFieldMap::Coordinates4:\n");
      printf("    Coordinate requested at convergence point of element %d.\n", imap);
      return ifail;
    }
    t2 = -1 + 2 * ((x - xt1) * (xt2 - xt1) + (y - yt1) * (yt2 - yt1)) / dn;
  }
  if (debug) {
    printf("ComponentFieldMap::Coordinates4:\n");
    printf("    Isoparametric (u, v):   (%g,%g).\n", t1, t2);
  }

  // Re-compute the (x,y,z) position for this coordinate.
  if (debug) {
    double xr=nodes[elements[imap].emap[0]].xmap*(1 - t1) * (1 - t2) / 4+
      nodes[elements[imap].emap[1]].xmap*(1 + t1) * (1 - t2) / 4+
      nodes[elements[imap].emap[2]].xmap*(1 + t1) * (1 + t2) / 4+
      nodes[elements[imap].emap[3]].xmap*(1 - t1) * (1 + t2) / 4;
    double yr=nodes[elements[imap].emap[0]].ymap*(1 - t1) * (1 - t2) / 4+
      nodes[elements[imap].emap[1]].ymap*(1 + t1) * (1 - t2) / 4+
      nodes[elements[imap].emap[2]].ymap*(1 + t1) * (1 + t2) / 4+
      nodes[elements[imap].emap[3]].ymap*(1 - t1) * (1 + t2) / 4;
    printf("ComponentFieldMap::Coordinates4: \n");
    printf("    Position requested:     (%g,%g),\n", x, y);
    printf("    Reconstructed:          (%g,%g),\n", xr, yr);
    printf("    Difference:             (%g,%g).\n", x - xr, y - yr);
  }

  // This should have worked if we get this far.
  ifail=0;
  return ifail;

}

int 
ComponentFieldMap::Coordinates5(double x, double y, double z,
                                double& t1, double& t2, double& t3, double& t4,
                                double jac[4][4], double& det, int imap) {
               
  // Debugging
  if (debug) {
    printf("ComponentFieldMap::Coordinates5:\n");
    printf("    Point (%g,%g).\n", x, y);
  }

  // Failure flag
  int ifail = 1;

  // Provisional values
  t1 = 0;
  t2 = 0;
  t3 = 0;
  t4 = 0;

  // Degenerate elements should have been treated as triangles
  if (elements[imap].degenerate) {
    printf("ComponentFieldMap::Coordinates5:\n");
    printf("    Received degenerate element %d.\n", imap);
    return ifail;
  }

  // Set tolerance parameter.
  double f = 0.5;

  // Make a first order approximation.
  int rc = Coordinates4(x, y, z, t1, t2, t3, t4, jac, det, imap);
  if (rc>0) {
    if (debug) {
      printf("ComponentFieldMap::Coordinates5:\n");
      printf("    Failure to obtain linear estimate of isoparametric coordinates\n");
      printf("    in element %d.\n", imap);}
    return ifail;
  }

  // Check whether the point is far outside.
  if (t1 < -(1 + f) || t1 > (1 + f) || t2 < -(1 + f) || t2 > (1 + f)) {
    if (debug) {
      printf("ComponentFieldMap::Coordinates5:\n");
      printf("    Point far outside, (t1,t2) = (%g,%g).\n", t1, t2);
    }
    return ifail;
  }

  // Start iteration
  double td1 = t1, td2 = t2;
  if (debug) {
    printf("ComponentFieldMap::Coordinates5:\n");
    printf("    Iteration starts at (t1,t2) = (%g,%g).\n", td1,td2);
  }
  // Loop
  bool converged = false;
  double diff[2], corr[2];
  for (int iter = 0; iter < 10; iter++) {
    if (debug) {
      printf("ComponentFieldMap::Coordinates5:\n");
      printf("    Iteration %3d:     (t1, t2) = (%g,%g).\n", iter, td1, td2);
    }
    // Re-compute the (x,y,z) position for this coordinate.
    double xr =
      nodes[elements[imap].emap[0]].xmap * (-(1 - td1) * (1 - td2) * (1 + td1 + td2)) / 4 +
      nodes[elements[imap].emap[1]].xmap * (-(1 + td1) * (1 - td2) * (1 - td1 + td2)) / 4 +
      nodes[elements[imap].emap[2]].xmap * (-(1 + td1) * (1 + td2) * (1 - td1 - td2)) / 4 +
      nodes[elements[imap].emap[3]].xmap * (-(1 - td1) * (1 + td2) * (1 + td1 - td2)) / 4 +
      nodes[elements[imap].emap[4]].xmap * (1 - td1) * (1 + td1) * (1 - td2) / 2 +
      nodes[elements[imap].emap[5]].xmap * (1 + td1) * (1 + td2) * (1 - td2) / 2 +
      nodes[elements[imap].emap[6]].xmap * (1 - td1) * (1 + td1) * (1 + td2) / 2 +
      nodes[elements[imap].emap[7]].xmap * (1 - td1) * (1 + td2) * (1 - td2) / 2;
    double yr =
      nodes[elements[imap].emap[0]].ymap * (-(1 - td1) * (1 - td2) * (1 + td1 + td2)) / 4 +
      nodes[elements[imap].emap[1]].ymap * (-(1 + td1) * (1 - td2) * (1 - td1 + td2)) / 4 +
      nodes[elements[imap].emap[2]].ymap * (-(1 + td1) * (1 + td2) * (1 - td1 - td2)) / 4 +
      nodes[elements[imap].emap[3]].ymap * (-(1 - td1) * (1 + td2) * (1 + td1 - td2)) / 4 +
      nodes[elements[imap].emap[4]].ymap * (1 - td1) * (1 + td1) * (1 - td2) / 2 +
      nodes[elements[imap].emap[5]].ymap * (1 + td1) * (1 + td2) * (1 - td2) / 2 +
      nodes[elements[imap].emap[6]].ymap * (1 - td1) * (1 + td1) * (1 + td2) / 2 +
      nodes[elements[imap].emap[7]].ymap * (1 - td1) * (1 + td2) * (1 - td2) / 2;
    // Compute the Jacobian
    Jacobian5(imap, td1, td2, det, jac);
    // Compute the difference vector
    diff[0] = x - xr;
    diff[1] = y - yr;
    // Update the estimate
    for (int l = 0; l < 2; ++l) {
      corr[l] = 0;
      for (int k = 0; k < 2; ++k) {
        corr[l] += jac[l][k] * diff[k] / det;
      }
    }
    // Debugging
    if (debug) {
      printf("ComponentFieldMap::Coordinates5:\n");
      printf("    Difference vector: (x, y)   = (%g,%g).\n", diff[0], diff[1]);
      printf("    Correction vector: (t1, t2) = (%g,%g).\n", corr[0], corr[1]);
    }
    // Update the vector.
    td1 += corr[0];
    td2 += corr[1];
    // Check for convergence.
    if (fabs(corr[0]) < 1.0e-5 && fabs(corr[1]) < 1.0e-5) {
      if (debug) {
        printf("ComponentFieldMap::Coordinates5:\n");
        printf("    Convergence reached.\n");
      }
      converged = true;
      break;
    }
  }
  // No convergence reached
  if (!converged) {
    double xmin, ymin, xmax, ymax;
    xmin = nodes[elements[imap].emap[0]].xmap;
    xmax = nodes[elements[imap].emap[0]].xmap;
    if (nodes[elements[imap].emap[1]].xmap < xmin) {xmin = nodes[elements[imap].emap[1]].xmap;} 
    if (nodes[elements[imap].emap[1]].xmap > xmax) {xmax = nodes[elements[imap].emap[1]].xmap;}
    if (nodes[elements[imap].emap[2]].xmap < xmin) {xmin = nodes[elements[imap].emap[2]].xmap;} 
    if (nodes[elements[imap].emap[2]].xmap > xmax) {xmax = nodes[elements[imap].emap[2]].xmap;}
    if (nodes[elements[imap].emap[3]].xmap < xmin) {xmin = nodes[elements[imap].emap[3]].xmap;} 
    if (nodes[elements[imap].emap[3]].xmap > xmax) {xmax = nodes[elements[imap].emap[3]].xmap;}
    if (nodes[elements[imap].emap[4]].xmap < xmin) {xmin = nodes[elements[imap].emap[4]].xmap;} 
    if (nodes[elements[imap].emap[4]].xmap > xmax) {xmax = nodes[elements[imap].emap[4]].xmap;}
    if (nodes[elements[imap].emap[5]].xmap < xmin) {xmin = nodes[elements[imap].emap[5]].xmap;} 
    if (nodes[elements[imap].emap[5]].xmap > xmax) {xmax = nodes[elements[imap].emap[5]].xmap;}
    if (nodes[elements[imap].emap[6]].xmap < xmin) {xmin = nodes[elements[imap].emap[6]].xmap;} 
    if (nodes[elements[imap].emap[6]].xmap > xmax) {xmax = nodes[elements[imap].emap[6]].xmap;}
    if (nodes[elements[imap].emap[7]].xmap < xmin) {xmin = nodes[elements[imap].emap[7]].xmap;} 
    if (nodes[elements[imap].emap[7]].xmap > xmax) {xmax = nodes[elements[imap].emap[7]].xmap;}
    ymin = nodes[elements[imap].emap[0]].ymap;
    ymax = nodes[elements[imap].emap[0]].ymap;
    if (nodes[elements[imap].emap[1]].ymap < ymin) {ymin = nodes[elements[imap].emap[1]].ymap;} 
    if (nodes[elements[imap].emap[1]].ymap > ymax) {ymax = nodes[elements[imap].emap[1]].ymap;}
    if (nodes[elements[imap].emap[2]].ymap < ymin) {ymin = nodes[elements[imap].emap[2]].ymap;} 
    if (nodes[elements[imap].emap[2]].ymap > ymax) {ymax = nodes[elements[imap].emap[2]].ymap;}
    if (nodes[elements[imap].emap[3]].ymap < ymin) {ymin = nodes[elements[imap].emap[3]].ymap;} 
    if (nodes[elements[imap].emap[3]].ymap > ymax) {ymax = nodes[elements[imap].emap[3]].ymap;}
    if (nodes[elements[imap].emap[4]].ymap < ymin) {ymin = nodes[elements[imap].emap[4]].ymap;} 
    if (nodes[elements[imap].emap[4]].ymap > ymax) {ymax = nodes[elements[imap].emap[4]].ymap;}
    if (nodes[elements[imap].emap[5]].ymap < ymin) {ymin = nodes[elements[imap].emap[5]].ymap;} 
    if (nodes[elements[imap].emap[5]].ymap > ymax) {ymax = nodes[elements[imap].emap[5]].ymap;}
    if (nodes[elements[imap].emap[6]].ymap < ymin) {ymin = nodes[elements[imap].emap[6]].ymap;} 
    if (nodes[elements[imap].emap[6]].ymap > ymax) {ymax = nodes[elements[imap].emap[6]].ymap;}
    if (nodes[elements[imap].emap[7]].ymap < ymin) {ymin = nodes[elements[imap].emap[7]].ymap;} 
    if (nodes[elements[imap].emap[7]].ymap > ymax) {ymax = nodes[elements[imap].emap[7]].ymap;}

    if (x >= xmin && x <= xmax && y >= ymin && y <= ymax) {
      printf("ComponentFieldMap::Coordinates5:\n");
      printf("    No convergence achieved when refining internal isoparametric coordinates\n");
      printf("    in element %d at position (%g,%g).\n", imap,x,y);
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
    printf("ComponentFieldMap::Coordinates5:\n");
    printf("    Convergence reached at (t1, t2) = (%g,%g).\n", t1, t2);
  }
  
  // For debugging purposes, show position
  if (debug) {
    double xr =
      nodes[elements[imap].emap[0]].xmap * (-(1 - t1) * (1 - t2) * (1 + t1 + t2)) / 4+
      nodes[elements[imap].emap[1]].xmap * (-(1 + t1) * (1 - t2) * (1 - t1 + t2)) / 4+
      nodes[elements[imap].emap[2]].xmap * (-(1 + t1) * (1 + t2) * (1 - t1 - t2)) / 4+
      nodes[elements[imap].emap[3]].xmap * (-(1 - t1) * (1 + t2) * (1 + t1 - t2)) / 4+
      nodes[elements[imap].emap[4]].xmap * (1 - t1) * (1 + t1) * (1 - t2) / 2 + 
      nodes[elements[imap].emap[5]].xmap * (1 + t1) * (1 + t2) * (1 - t2) / 2 + 
      nodes[elements[imap].emap[6]].xmap * (1 - t1) * (1 + t1) * (1 + t2) / 2 + 
      nodes[elements[imap].emap[7]].xmap * (1 - t1) * (1 + t2) * (1 - t2) / 2;
    double yr=
      nodes[elements[imap].emap[0]].ymap*(-(1 - t1) * (1 - t2) * (1 + t1 + t2)) / 4+
      nodes[elements[imap].emap[1]].ymap*(-(1 + t1) * (1 - t2) * (1 - t1 + t2)) / 4+
      nodes[elements[imap].emap[2]].ymap*(-(1 + t1) * (1 + t2) * (1 - t1 - t2)) / 4+
      nodes[elements[imap].emap[3]].ymap*(-(1 - t1) * (1 + t2) * (1 + t1 - t2)) / 4+
      nodes[elements[imap].emap[4]].ymap*(1 - t1) * (1 + t1) * (1 - t2) / 2 + 
      nodes[elements[imap].emap[5]].ymap*(1 + t1) * (1 + t2) * (1 - t2) / 2 + 
      nodes[elements[imap].emap[6]].ymap*(1 - t1) * (1 + t1) * (1 + t2) / 2 + 
      nodes[elements[imap].emap[7]].ymap*(1 - t1) * (1 + t2) * (1 - t2) / 2;
    printf("ComponentFieldMap::Coordinates5:\n");
    printf("    Position requested:     (%g,%g),\n", x, y);
    printf("    Reconstructed:          (%g,%g),\n", xr, yr);
    printf("    Difference:             (%g,%g).\n", x - xr, y - yr);
  }

  // Success
  ifail = 0;
  return ifail;
  
}

int 
ComponentFieldMap::Coordinates12(double x, double y, double z,
                        double& t1, double& t2, double& t3, double& t4,
                        double jac[4][4], double& det, int imap) {

  // Debugging
  //  bool debug; if (imap==1024) {debug=true;} else {debug = false;}
  if (debug) {
    printf("ComponentFieldMap::Coordinates12:\n");
    printf("    Point (%g,%g,%g) for element %d.\n", x, y, z, imap);
  }

  // Failure flag
  int ifail = 1;
  
  // Compute tetrahedral coordinates.
  t1 = (x - nodes[elements[imap].emap[1]].xmap) * ((nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[1]].ymap) * (nodes[elements[imap].emap[3]].zmap - nodes[elements[imap].emap[1]].zmap) - 
                                                   (nodes[elements[imap].emap[3]].ymap - nodes[elements[imap].emap[1]].ymap) * (nodes[elements[imap].emap[2]].zmap - nodes[elements[imap].emap[1]].zmap)) +            
       (y - nodes[elements[imap].emap[1]].ymap) * ((nodes[elements[imap].emap[2]].zmap - nodes[elements[imap].emap[1]].zmap) * (nodes[elements[imap].emap[3]].xmap - nodes[elements[imap].emap[1]].xmap) - 
                                                   (nodes[elements[imap].emap[3]].zmap - nodes[elements[imap].emap[1]].zmap) * (nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[1]].xmap)) + 
       (z - nodes[elements[imap].emap[1]].zmap) * ((nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[1]].xmap) * (nodes[elements[imap].emap[3]].ymap - nodes[elements[imap].emap[1]].ymap) - 
                                                   (nodes[elements[imap].emap[3]].xmap - nodes[elements[imap].emap[1]].xmap) * (nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[1]].ymap));
  t2 = (x - nodes[elements[imap].emap[2]].xmap) * ((nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[2]].ymap) * (nodes[elements[imap].emap[3]].zmap - nodes[elements[imap].emap[2]].zmap) - 
                                                   (nodes[elements[imap].emap[3]].ymap - nodes[elements[imap].emap[2]].ymap) * (nodes[elements[imap].emap[0]].zmap - nodes[elements[imap].emap[2]].zmap)) + 
       (y - nodes[elements[imap].emap[2]].ymap) * ((nodes[elements[imap].emap[0]].zmap - nodes[elements[imap].emap[2]].zmap) * (nodes[elements[imap].emap[3]].xmap - nodes[elements[imap].emap[2]].xmap) - 
                                                   (nodes[elements[imap].emap[3]].zmap - nodes[elements[imap].emap[2]].zmap) * (nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[2]].xmap)) + 
       (z - nodes[elements[imap].emap[2]].zmap) * ((nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[2]].xmap) * (nodes[elements[imap].emap[3]].ymap - nodes[elements[imap].emap[2]].ymap) - 
                                                   (nodes[elements[imap].emap[3]].xmap - nodes[elements[imap].emap[2]].xmap) * (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[2]].ymap));
  t3 = (x - nodes[elements[imap].emap[3]].xmap) * ((nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[3]].ymap) * (nodes[elements[imap].emap[1]].zmap - nodes[elements[imap].emap[3]].zmap) - 
                                                   (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[3]].ymap) * (nodes[elements[imap].emap[0]].zmap - nodes[elements[imap].emap[3]].zmap)) + 
       (y - nodes[elements[imap].emap[3]].ymap) * ((nodes[elements[imap].emap[0]].zmap - nodes[elements[imap].emap[3]].zmap) * (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[3]].xmap) - 
                                                   (nodes[elements[imap].emap[1]].zmap - nodes[elements[imap].emap[3]].zmap) * (nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[3]].xmap)) + 
       (z - nodes[elements[imap].emap[3]].zmap) * ((nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[3]].xmap) * (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[3]].ymap) - 
                                                   (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[3]].xmap) * (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[3]].ymap));
  t4 = (x - nodes[elements[imap].emap[0]].xmap) * ((nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[0]].ymap) * (nodes[elements[imap].emap[1]].zmap - nodes[elements[imap].emap[0]].zmap) - 
                                                   (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[0]].ymap) * (nodes[elements[imap].emap[2]].zmap - nodes[elements[imap].emap[0]].zmap)) + 
       (y - nodes[elements[imap].emap[0]].ymap) * ((nodes[elements[imap].emap[2]].zmap - nodes[elements[imap].emap[0]].zmap) * (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[0]].xmap) - 
                                                   (nodes[elements[imap].emap[1]].zmap - nodes[elements[imap].emap[0]].zmap) * (nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[0]].xmap)) +                                            
       (z - nodes[elements[imap].emap[0]].zmap) * ((nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[0]].xmap) * (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[0]].ymap) - 
                                                   (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[0]].xmap) * (nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[0]].ymap));
  t1 = t1 / ((nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[1]].xmap) * ((nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[1]].ymap) * (nodes[elements[imap].emap[3]].zmap - nodes[elements[imap].emap[1]].zmap) - 
             (nodes[elements[imap].emap[3]].ymap - nodes[elements[imap].emap[1]].ymap) * (nodes[elements[imap].emap[2]].zmap - nodes[elements[imap].emap[1]].zmap)) + 
             (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[1]].ymap) * ((nodes[elements[imap].emap[2]].zmap - nodes[elements[imap].emap[1]].zmap) * (nodes[elements[imap].emap[3]].xmap - nodes[elements[imap].emap[1]].xmap) - 
             (nodes[elements[imap].emap[3]].zmap - nodes[elements[imap].emap[1]].zmap) * (nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[1]].xmap)) + 
             (nodes[elements[imap].emap[0]].zmap - nodes[elements[imap].emap[1]].zmap) * ((nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[1]].xmap) * (nodes[elements[imap].emap[3]].ymap - nodes[elements[imap].emap[1]].ymap) - 
             (nodes[elements[imap].emap[3]].xmap - nodes[elements[imap].emap[1]].xmap) * (nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[1]].ymap)));
  t2 = t2 / ((nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[2]].xmap) * ((nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[2]].ymap) * (nodes[elements[imap].emap[3]].zmap - nodes[elements[imap].emap[2]].zmap) - 
             (nodes[elements[imap].emap[3]].ymap - nodes[elements[imap].emap[2]].ymap) * (nodes[elements[imap].emap[0]].zmap - nodes[elements[imap].emap[2]].zmap)) + 
             (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[2]].ymap) * ((nodes[elements[imap].emap[0]].zmap - nodes[elements[imap].emap[2]].zmap) * (nodes[elements[imap].emap[3]].xmap - nodes[elements[imap].emap[2]].xmap) - 
             (nodes[elements[imap].emap[3]].zmap - nodes[elements[imap].emap[2]].zmap) * (nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[2]].xmap)) + 
             (nodes[elements[imap].emap[1]].zmap - nodes[elements[imap].emap[2]].zmap) * ((nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[2]].xmap) * (nodes[elements[imap].emap[3]].ymap - nodes[elements[imap].emap[2]].ymap) - 
             (nodes[elements[imap].emap[3]].xmap - nodes[elements[imap].emap[2]].xmap) * (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[2]].ymap)));
  t3 = t3 / ((nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[3]].xmap) * ((nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[3]].ymap) * (nodes[elements[imap].emap[1]].zmap - nodes[elements[imap].emap[3]].zmap) - 
             (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[3]].ymap) * (nodes[elements[imap].emap[0]].zmap - nodes[elements[imap].emap[3]].zmap)) + 
             (nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[3]].ymap) * ((nodes[elements[imap].emap[0]].zmap - nodes[elements[imap].emap[3]].zmap) * (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[3]].xmap) - 
             (nodes[elements[imap].emap[1]].zmap - nodes[elements[imap].emap[3]].zmap) * (nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[3]].xmap)) + 
             (nodes[elements[imap].emap[2]].zmap - nodes[elements[imap].emap[3]].zmap) * ((nodes[elements[imap].emap[0]].xmap - nodes[elements[imap].emap[3]].xmap) * (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[3]].ymap) - 
             (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[3]].xmap) * (nodes[elements[imap].emap[0]].ymap - nodes[elements[imap].emap[3]].ymap)));
  t4 = t4 / ((nodes[elements[imap].emap[3]].xmap - nodes[elements[imap].emap[0]].xmap) * ((nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[0]].ymap) * (nodes[elements[imap].emap[1]].zmap - nodes[elements[imap].emap[0]].zmap) - 
             (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[0]].ymap) * (nodes[elements[imap].emap[2]].zmap - nodes[elements[imap].emap[0]].zmap)) + 
             (nodes[elements[imap].emap[3]].ymap - nodes[elements[imap].emap[0]].ymap) * ((nodes[elements[imap].emap[2]].zmap - nodes[elements[imap].emap[0]].zmap) * (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[0]].xmap) - 
             (nodes[elements[imap].emap[1]].zmap - nodes[elements[imap].emap[0]].zmap) * (nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[0]].xmap)) + 
             (nodes[elements[imap].emap[3]].zmap - nodes[elements[imap].emap[0]].zmap) * ((nodes[elements[imap].emap[2]].xmap - nodes[elements[imap].emap[0]].xmap) * (nodes[elements[imap].emap[1]].ymap - nodes[elements[imap].emap[0]].ymap) - 
             (nodes[elements[imap].emap[1]].xmap - nodes[elements[imap].emap[0]].xmap) * (nodes[elements[imap].emap[2]].ymap - nodes[elements[imap].emap[0]].ymap)));

  // Result
  if (debug) {
    printf("ComponentFieldMap::Coordinates12:\n");
    printf("    Tetrahedral coordinates (t, u, v, w) = (%g,%g,%g,%g), sum = %g.\n",
           t1, t2, t3, t4, t1 + t2 + t3 + t4);
  }
  // Re-compute the (x,y,z) position for this coordinate.
  if (debug) {
    double xr = 
      nodes[elements[imap].emap[0]].xmap * t1 + 
      nodes[elements[imap].emap[1]].xmap * t2 + 
      nodes[elements[imap].emap[2]].xmap * t3 +
      nodes[elements[imap].emap[3]].xmap * t4;
    double yr =
      nodes[elements[imap].emap[0]].ymap * t1 + 
      nodes[elements[imap].emap[1]].ymap * t2 + 
      nodes[elements[imap].emap[2]].ymap * t3 +
      nodes[elements[imap].emap[3]].ymap * t4;
    double zr =
      nodes[elements[imap].emap[0]].zmap * t1 + 
      nodes[elements[imap].emap[1]].zmap * t2 + 
      nodes[elements[imap].emap[2]].zmap * t3 +
      nodes[elements[imap].emap[3]].zmap * t4;
    double sr = t1 + t2 + t3 + t4;
    printf("ComponentFieldMap::Coordinates12:\n");
    printf("    Position requested:     (%g,%g,%g)\n", x, y, z);
    printf("    Position reconstructed: (%g,%g,%g)\n", xr, yr, zr);
    printf("    Difference:             (%g,%g,%g)\n", x - xr, y - yr, z - zr);
    printf("    Checksum - 1:           %g\n", sr - 1);
  }

  // This should always work
  ifail = 0;
  return ifail;

}

int 
ComponentFieldMap::Coordinates13(double x, double y, double z,
                        double& t1, double& t2, double& t3, double& t4,
                        double jac[4][4], double& det, int imap) {
  // Debugging
  // if (imap == 1024) {debug=true;} else {debug = false;}
  if (debug) {
    printf("ComponentFieldMap::Coordinates13:\n");
    printf("    Point (%g,%g,%g).\n", x, y, z);
  }

  // Failure flag
  int ifail = 1;

  // Provisional values
  t1 = t2 = t3 = t4 = 0.;

  // Set tolerance parameter.
  double f = 0.5;

  // Make a first order approximation.
  int rc = Coordinates12(x, y, z, t1, t2, t3, t4, jac, det, imap);
  if (rc > 0) {
    if (debug) {
      printf("ComponentFieldMap::Coordinates13:\n");
      printf("    Failure to obtain linear estimate of isoparametric coordinates\n");
      printf("    in element %d.\n", imap);
    }
    return ifail;
  }
  if (t1 <    -f || t2 <    -f || t3 <    -f || t4 <    -f ||
      t1 > 1 + f || t2 > 1 + f || t3 > 1 + f || t4 > 1 + f) {
    if (debug) {
      printf("ComponentFieldMap::Coordinates13:\n");
      printf("    Linear isoparametric coordinates more than\n");
      printf("    f (%g) out of range in element %d.\n", f, imap);
    }
    ifail = 0;
    return ifail;
  }

  // Start iteration
  double td1 = t1, td2 = t2, td3 = t3, td4 = t4;
  if (debug) {
    printf("ComponentFieldMap::Coordinates13:\n");
    printf("    Iteration starts at (t1,t2,t3,t4) = (%g,%g,%g,%g).\n", td1, td2, td3, td4);
  }
  // Loop
  bool converged = false;
  double diff[4], corr[4];
  for (int iter = 0; iter < 10; iter++) {
    if (debug) {
      printf("ComponentFieldMap::Coordinates13:\n");
      printf("    Iteration %3d:      (t1,t2,t3,t4) = (%g,%g,%g,%g).\n", 
             iter, td1, td2, td3, td4);
    }
    // Re-compute the (x,y,z) position for this coordinate.
    double xr = 
      nodes[elements[imap].emap[0]].xmap * td1 * (2 * td1 - 1) +
      nodes[elements[imap].emap[1]].xmap * td2 * (2 * td2 - 1) +
      nodes[elements[imap].emap[2]].xmap * td3 * (2 * td3 - 1) +
      nodes[elements[imap].emap[3]].xmap * td4 * (2 * td4 - 1) +
      nodes[elements[imap].emap[4]].xmap * 4 * td1 * td2 +
      nodes[elements[imap].emap[5]].xmap * 4 * td1 * td3 +
      nodes[elements[imap].emap[6]].xmap * 4 * td1 * td4 +
      nodes[elements[imap].emap[7]].xmap * 4 * td2 * td3 +
      nodes[elements[imap].emap[8]].xmap * 4 * td2 * td4 +
      nodes[elements[imap].emap[9]].xmap * 4 * td3 * td4;
    double yr = 
      nodes[elements[imap].emap[0]].ymap *td1*(2*td1 - 1) + 
      nodes[elements[imap].emap[1]].ymap *td2*(2*td2-1) + 
      nodes[elements[imap].emap[2]].ymap *td3*(2*td3-1) + 
      nodes[elements[imap].emap[3]].ymap *td4*(2*td4-1) + 
      nodes[elements[imap].emap[4]].ymap *4*td1*td2 + 
      nodes[elements[imap].emap[5]].ymap *4*td1*td3+
      nodes[elements[imap].emap[6]].ymap *4*td1*td4+
      nodes[elements[imap].emap[7]].ymap *4*td2*td3+
      nodes[elements[imap].emap[8]].ymap *4*td2*td4+
      nodes[elements[imap].emap[9]].ymap *4*td3*td4;
    double zr = 
      nodes[elements[imap].emap[0]].zmap * td1 * (2 * td1 - 1) +
      nodes[elements[imap].emap[1]].zmap * td2 * (2 * td2 - 1) +
      nodes[elements[imap].emap[2]].zmap * td3 * (2 * td3 - 1) +
      nodes[elements[imap].emap[3]].zmap * td4 * (2 * td4 - 1) +
      nodes[elements[imap].emap[4]].zmap * 4 * td1 * td2 +
      nodes[elements[imap].emap[5]].zmap * 4 * td1 * td3 +
      nodes[elements[imap].emap[6]].zmap * 4 * td1 * td4 +
      nodes[elements[imap].emap[7]].zmap * 4 * td2 * td3 +
      nodes[elements[imap].emap[8]].zmap * 4 * td2 * td4 +
      nodes[elements[imap].emap[9]].zmap * 4 * td3 * td4;
    double sr = td1 + td2 + td3 + td4;

    // Compute the Jacobian
    Jacobian13(imap, td1, td2, td3, td4, det, jac);
    // Compute the difference vector
    diff[0] = 1 - sr;
    diff[1] = x - xr;
    diff[2] = y - yr;
    diff[3] = z - zr;

    // Update the estimate
    for (int l = 0; l < 4; ++l) {
      corr[l] = 0;
      for (int k = 0; k < 4; ++k) {
        corr[l] += jac[l][k] * diff[k] / det;
      }
    }

    // Debugging
    if (debug) {
      printf("ComponentFieldMap::Coordinates13:\n");
      printf("    Difference vector:  (1, x, y, z)  = (%g,%g,%g,%g).\n", diff[0], diff[1], diff[2], diff[3]);
      printf("    Correction vector:  (t1,t2,t3,t4) = (%g,%g,%g,%g).\n", corr[0], corr[1], corr[2], corr[3]);
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
        printf("ComponentFieldMap::Coordinates13:\n");
        printf("    Convergence reached.\n");
      }
      converged = true;
      break;
    }
  }

  // No convergence reached
  if (!converged) {
    double xmin, ymin, zmin, xmax, ymax, zmax;
    xmin = nodes[elements[imap].emap[0]].xmap; 
    xmax = nodes[elements[imap].emap[0]].xmap;
    if (nodes[elements[imap].emap[1]].xmap < xmin) {xmin = nodes[elements[imap].emap[1]].xmap;} 
    if (nodes[elements[imap].emap[1]].xmap > xmax) {xmax = nodes[elements[imap].emap[1]].xmap;}
    if (nodes[elements[imap].emap[2]].xmap < xmin) {xmin = nodes[elements[imap].emap[2]].xmap;} 
    if (nodes[elements[imap].emap[2]].xmap > xmax) {xmax = nodes[elements[imap].emap[2]].xmap;}
    if (nodes[elements[imap].emap[3]].xmap < xmin) {xmin = nodes[elements[imap].emap[3]].xmap;} 
    if (nodes[elements[imap].emap[3]].xmap > xmax) {xmax = nodes[elements[imap].emap[3]].xmap;}
    ymin = nodes[elements[imap].emap[0]].ymap;
    ymax = nodes[elements[imap].emap[0]].ymap;
    if (nodes[elements[imap].emap[1]].ymap < ymin) {ymin = nodes[elements[imap].emap[1]].ymap;} 
    if (nodes[elements[imap].emap[1]].ymap > ymax) {ymax = nodes[elements[imap].emap[1]].ymap;}
    if (nodes[elements[imap].emap[2]].ymap < ymin) {ymin = nodes[elements[imap].emap[2]].ymap;} 
    if (nodes[elements[imap].emap[2]].ymap > ymax) {ymax = nodes[elements[imap].emap[2]].ymap;}
    if (nodes[elements[imap].emap[3]].ymap < ymin) {ymin = nodes[elements[imap].emap[3]].ymap;} 
    if (nodes[elements[imap].emap[3]].ymap > ymax) {ymax = nodes[elements[imap].emap[3]].ymap;}
    zmin = nodes[elements[imap].emap[0]].zmap;
    zmax = nodes[elements[imap].emap[0]].zmap;
    if (nodes[elements[imap].emap[1]].zmap < zmin) {zmin = nodes[elements[imap].emap[1]].zmap;}
    if (nodes[elements[imap].emap[1]].zmap > zmax) {zmax = nodes[elements[imap].emap[1]].zmap;}
    if (nodes[elements[imap].emap[2]].zmap < zmin) {zmin = nodes[elements[imap].emap[2]].zmap;}
    if (nodes[elements[imap].emap[2]].zmap > zmax) {zmax = nodes[elements[imap].emap[2]].zmap;}
    if (nodes[elements[imap].emap[3]].zmap < zmin) {zmin = nodes[elements[imap].emap[3]].zmap;}
    if (nodes[elements[imap].emap[3]].zmap > zmax) {zmax = nodes[elements[imap].emap[3]].zmap;}

    if (x >= xmin && x <= xmax && y >= ymin && 
        y <= ymax && z >= zmin && z <= zmax) {
      printf("ComponentFieldMap::Coordinates13:\n");
      printf("    No convergence achieved when refining internal isoparametric coordinates\n");
      printf("    in element %d at position (%g,%g,%g).\n", imap, x, y, z);
      t1 = -1;
      t2 = -1;
      t3 = -1;
      t4 = -1;
      return ifail;
    }
  }

  // Convergence reached.
  t1 = td1;
  t2 = td2;
  t3 = td3;
  t4 = td4;
  if (debug) {
    printf("ComponentFieldMap::Coordinates13:\n");
    printf("    Convergence reached at (t1, t2, t3, t4) = (%g,%g,%g,%g).\n", t1, t2, t3, t4);
  }
  
  // For debugging purposes, show position
  if (debug) {
    // Re-compute the (x,y,z) position for this coordinate.
    double xr = 
      nodes[elements[imap].emap[0]].xmap * td1 * (2 * td1 - 1) +
      nodes[elements[imap].emap[1]].xmap * td2 * (2 * td2 - 1) +
      nodes[elements[imap].emap[2]].xmap * td3 * (2 * td3 - 1) +
      nodes[elements[imap].emap[3]].xmap * td4 * (2 * td4 - 1) +
      nodes[elements[imap].emap[4]].xmap * 4 * td1 * td2 +
      nodes[elements[imap].emap[5]].xmap * 4 * td1 * td3 +
      nodes[elements[imap].emap[6]].xmap * 4 * td1 * td4 +
      nodes[elements[imap].emap[7]].xmap * 4 * td2 * td3 +
      nodes[elements[imap].emap[8]].xmap * 4 * td2 * td4 +
      nodes[elements[imap].emap[9]].xmap * 4 * td3 * td4;
    double yr = 
      nodes[elements[imap].emap[0]].ymap * td1 * (2 * td1 - 1) +
      nodes[elements[imap].emap[1]].ymap * td2 * (2 * td2 - 1) +
      nodes[elements[imap].emap[2]].ymap * td3 * (2 * td3 - 1) +
      nodes[elements[imap].emap[3]].ymap * td4 * (2 * td4 - 1) +
      nodes[elements[imap].emap[4]].ymap * 4 * td1 * td2 +
      nodes[elements[imap].emap[5]].ymap * 4 * td1 * td3 +
      nodes[elements[imap].emap[6]].ymap * 4 * td1 * td4 +
      nodes[elements[imap].emap[7]].ymap * 4 * td2 * td3 +
      nodes[elements[imap].emap[8]].ymap * 4 * td2 * td4 +
      nodes[elements[imap].emap[9]].ymap * 4 * td3 * td4;
    double zr = 
      nodes[elements[imap].emap[0]].zmap * td1 * (2 * td1 - 1) +
      nodes[elements[imap].emap[1]].zmap * td2 * (2 * td2 - 1) +
      nodes[elements[imap].emap[2]].zmap * td3 * (2 * td3 - 1) +
      nodes[elements[imap].emap[3]].zmap * td4 * (2 * td4 - 1) +
      nodes[elements[imap].emap[4]].zmap * 4 * td1 * td2 +
      nodes[elements[imap].emap[5]].zmap * 4 * td1 * td3 +
      nodes[elements[imap].emap[6]].zmap * 4 * td1 * td4 +
      nodes[elements[imap].emap[7]].zmap * 4 * td2 * td3 +
      nodes[elements[imap].emap[8]].zmap * 4 * td2 * td4 +
      nodes[elements[imap].emap[9]].zmap * 4 * td3 * td4;
    double sr = td1 + td2 + td3 + td4;
    printf("ComponentFieldMap::Coordinates13:\n");
    printf("    Position requested:     (%g,%g,%g),\n", x, y, z);
    printf("    Reconstructed:          (%g,%g,%g),\n", xr, yr, zr);
    printf("    Difference:             (%g,%g,%g),\n", x - xr, y - yr, z - zr);
    printf("    Checksum - 1:           %g.\n", sr - 1);
  }

  // Success
  ifail = 0;
  return ifail;

}

void 
ComponentFieldMap::UpdatePeriodicityCommon() {

  // Check the required data is available
  if (!ready) {
    printf("ComponentFieldMap::UpdatePeriodicityCommon:\n");
    printf("    No valid field map available.\n");
    return;
  }

  // No regular and mirror periodicity at the same time
  if (xPeriodic && xMirrorPeriodic) {
    printf("ComponentFieldMap::UpdatePeriodicityCommon:\n");
    printf("    Both simple and mirror periodicity along x requested; reset.\n");
    xPeriodic = false;
    xMirrorPeriodic = false;
    warning = true;
  }
  if (yPeriodic && yMirrorPeriodic) {
    printf("ComponentFieldMap::UpdatePeriodicityCommon:\n");
    printf("    Both simple and mirror periodicity along y requested; reset.\n");
    yPeriodic = false;
    yMirrorPeriodic = false;
    warning = true;
  }
  if (zPeriodic && zMirrorPeriodic) {
    printf("ComponentFieldMap::UpdatePeriodicityCommon:\n");
    printf("    Both simple and mirror periodicity along z requested; reset.\n");
    zPeriodic = false;
    zMirrorPeriodic = false;
    warning = true;
  }

  // In case of axial periodicity, the range must be an integral part of 2 pi
  if (xAxiallyPeriodic) {
    if (mapxamin >= mapxamax) {
      mapnxa = 0;
    } else {
      mapnxa = TwoPi / (mapxamax - mapxamin);
    }
    if (fabs(mapnxa - int(0.5+mapnxa)) > 0.001 || mapnxa < 1.5) {
      printf("ComponentFieldMap::UpdatePeriodicityCommon:\n");
      printf("    X-axial symmetry has been requested but the map\n");
      printf("    does not cover an integral fraction of 2 pi; reset.\n");
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
      printf("ComponentFieldMap::UpdatePeriodicityCommon:\n");
      printf("    Y-axial symmetry has been requested but the map\n");
      printf("    does not cover an integral fraction of 2 pi; reset.\n");
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
      printf("ComponentFieldMap::UpdatePeriodicityCommon:\n");
      printf("    Z-axial symmetry has been requested but the map\n");
      printf("    does not cover an integral fraction of 2 pi; reset.\n");
      zAxiallyPeriodic = false;
      warning = true;
    }
  }

  // Not more than 1 rotational symmetry
  if (xRotationSymmetry && yRotationSymmetry ||
      xRotationSymmetry && zRotationSymmetry ||
      yRotationSymmetry && zRotationSymmetry) {
    printf("ComponentFieldMap::UpdatePeriodicityCommon:\n");
    printf("    Only 1 rotational symmetry allowed; reset.\n");
    xRotationSymmetry = false;
    yRotationSymmetry = false;
    zRotationSymmetry = false;
    warning = true;
  }

  // No rotational symmetry as well as axial periodicity
  if ((xRotationSymmetry || yRotationSymmetry || zRotationSymmetry) && 
      (xAxiallyPeriodic || yAxiallyPeriodic || zAxiallyPeriodic)) {
    printf("ComponentFieldMap::UpdatePeriodicityCommon:\n");
    printf("    Not allowed to combine rotational symmetry and axial periodicity; reset.\n");
    xAxiallyPeriodic = false;
    yAxiallyPeriodic = false;
    zAxiallyPeriodic = false;
    xRotationSymmetry = false;
    yRotationSymmetry = false;
    zRotationSymmetry = false;
    warning = true;
  }

  // In case of rotational symmetry, the x-range should not straddle 0
  if (xRotationSymmetry || yRotationSymmetry || zRotationSymmetry) {
    if (mapxmin * mapxmax < 0) {
      printf("ComponentFieldMap::UpdatePeriodicityCommon:\n");
      printf("    Rotational symmetry requested, but x-range straddles 0; reset.\n");
      xRotationSymmetry = false;
      yRotationSymmetry = false;
      zRotationSymmetry = false;
      warning = true;
    }
  }

  // Recompute the cell ranges
  xMinBoundingBox = mapxmin;
  xMaxBoundingBox = mapxmax;
  yMinBoundingBox = mapymin;
  yMaxBoundingBox = mapymax;
  zMinBoundingBox = mapzmin;
  zMaxBoundingBox = mapzmax;
  cellsx = fabs(mapxmax-mapxmin);
  cellsy = fabs(mapymax-mapymin);
  cellsz = fabs(mapzmax-mapzmin);
  if (xRotationSymmetry) {
    xMinBoundingBox = mapymin;
    xMaxBoundingBox = mapymax;
    yMinBoundingBox = -std::max(fabs(mapxmin),fabs(mapxmax));
    yMaxBoundingBox = +std::max(fabs(mapxmin),fabs(mapxmax));
    zMinBoundingBox = -std::max(fabs(mapxmin),fabs(mapxmax));
    zMaxBoundingBox = +std::max(fabs(mapxmin),fabs(mapxmax));
  } else if (yRotationSymmetry) {
    xMinBoundingBox = -std::max(fabs(mapxmin),fabs(mapxmax));
    xMaxBoundingBox = +std::max(fabs(mapxmin),fabs(mapxmax));
    yMinBoundingBox = mapymin;
    yMaxBoundingBox = mapymax;
    zMinBoundingBox = -std::max(fabs(mapxmin),fabs(mapxmax));
    zMaxBoundingBox = +std::max(fabs(mapxmin),fabs(mapxmax));
  } else if (zRotationSymmetry) {
    xMinBoundingBox = -std::max(fabs(mapxmin),fabs(mapxmax));
    xMaxBoundingBox = +std::max(fabs(mapxmin),fabs(mapxmax));
    yMinBoundingBox = -std::max(fabs(mapxmin),fabs(mapxmax));
    yMaxBoundingBox = +std::max(fabs(mapxmin),fabs(mapxmax));
    zMinBoundingBox = mapymin;
    zMaxBoundingBox = mapymax;
  }
  
  if (xAxiallyPeriodic) {
    yMinBoundingBox = -std::max(std::max(fabs(mapymin),fabs(mapymax)),std::max(fabs(mapzmin),fabs(mapzmax)));
    yMaxBoundingBox = +std::max(std::max(fabs(mapymin),fabs(mapymax)),std::max(fabs(mapzmin),fabs(mapzmax)));
    zMinBoundingBox = -std::max(std::max(fabs(mapymin),fabs(mapymax)),std::max(fabs(mapzmin),fabs(mapzmax)));
    zMaxBoundingBox = +std::max(std::max(fabs(mapymin),fabs(mapymax)),std::max(fabs(mapzmin),fabs(mapzmax)));
  } else if (yAxiallyPeriodic) {
    xMinBoundingBox = -std::max(std::max(fabs(mapxmin),fabs(mapxmax)),std::max(fabs(mapzmin),fabs(mapzmax)));
    xMaxBoundingBox = +std::max(std::max(fabs(mapxmin),fabs(mapxmax)),std::max(fabs(mapzmin),fabs(mapzmax)));
    zMinBoundingBox = -std::max(std::max(fabs(mapxmin),fabs(mapxmax)),std::max(fabs(mapzmin),fabs(mapzmax)));
    zMaxBoundingBox = +std::max(std::max(fabs(mapxmin),fabs(mapxmax)),std::max(fabs(mapzmin),fabs(mapzmax)));
  } else if (zAxiallyPeriodic) {
    xMinBoundingBox = -std::max(std::max(fabs(mapxmin),fabs(mapxmax)),std::max(fabs(mapymin),fabs(mapymax)));
    xMaxBoundingBox = +std::max(std::max(fabs(mapxmin),fabs(mapxmax)),std::max(fabs(mapymin),fabs(mapymax)));
    yMinBoundingBox = -std::max(std::max(fabs(mapxmin),fabs(mapxmax)),std::max(fabs(mapymin),fabs(mapymax)));
    yMaxBoundingBox = +std::max(std::max(fabs(mapxmin),fabs(mapxmax)),std::max(fabs(mapymin),fabs(mapymax)));
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

  // Display the range if requested
  if (debug) PrintRange();
  
}

void
ComponentFieldMap::UpdatePeriodicity2d() {

  // Check the required data is available
  if (!ready) {
    printf("ComponentFieldMap::UpdatePeriodicity2d:\n");
    printf("    No valid field map available.\n");
    return;
  }

  // No z-periodicity in 2d
  if (zPeriodic || zMirrorPeriodic) {
    printf("ComponentFieldMap::UpdatePeriodicity2d:\n");
    printf("    Simple or mirror periodicity along z requested for a 2D map; reset.\n");
    zPeriodic = false;
    zMirrorPeriodic = false;
    warning = true;
  }

  // Only z-axial periodicity in 2d maps
  if (xAxiallyPeriodic || yAxiallyPeriodic ) {
    printf("ComponentFieldMap::UpdatePeriodicity2d:\n");
    printf("    Axial symmetry has been requested around x or y for a 2D map; reset.\n");
    xAxiallyPeriodic = false;
    yAxiallyPeriodic = false;
    warning = true;
  }
  
}

void 
ComponentFieldMap::SetRange() {

  // Initial values
  mapxmin = mapymin = mapzmin = 0;
  mapxmax = mapymax = mapzmax = 0;
  mapxamin = mapyamin = mapzamin = 0;
  mapxamax = mapyamax = mapzamax = 0;
  mapvmin = mapvmax = 0;
  setangx = setangy = setangz = false;

  // Check the required data is available
  if (!ready || nNodes < 1) {
    printf("ComponentFieldMap::SetRange:\n");
    printf("    Field map not yet set or number of nodes < 1.\n");
    return;
  }
  
  // Loop over the nodes
  mapxmin = nodes[0].xmap;
  mapymin = nodes[0].xmap;
  mapzmin = nodes[0].ymap;
  mapxmax = nodes[0].ymap;
  mapymax = nodes[0].zmap;
  mapzmax = nodes[0].zmap;
  mapvmin = nodes[0].vmap;
  mapvmax = nodes[0].vmap;
    
  double ang;
  for (int i = 1; i < nNodes; i++) {
    if (mapxmin > nodes[i].xmap) mapxmin = nodes[i].xmap;
    if (mapxmax < nodes[i].xmap) mapxmax = nodes[i].xmap;
    if (mapymin > nodes[i].ymap) mapymin = nodes[i].ymap;
    if (mapymax < nodes[i].ymap) mapymax = nodes[i].ymap;
    if (mapzmin > nodes[i].zmap) mapzmin = nodes[i].zmap;
    if (mapzmax < nodes[i].zmap) mapzmax = nodes[i].zmap;
    if (mapvmin > nodes[i].vmap) mapvmin = nodes[i].vmap;
    if (mapvmax < nodes[i].vmap) mapvmax = nodes[i].vmap;
    
    if (nodes[i].ymap != 0 || nodes[i].zmap != 0) {
      ang = atan2(nodes[i].zmap, nodes[i].ymap);
      if (setangx) {
        if (ang < mapxamin) mapxamin = ang;
        if (ang > mapxamax) mapxamax = ang;
      } else {
        mapxamin = mapxamax = ang;
        setangx = true;
      }
    }
    
    if (nodes[i].zmap != 0 || nodes[i].xmap != 0) {
      ang = atan2(nodes[i].xmap, nodes[i].zmap);
      if (setangy) {
        if (ang < mapyamin) mapyamin = ang;
        if (ang > mapyamax) mapyamax = ang;
      } else {
        mapyamin = mapyamax = ang;
        setangy = true;
      }
    }
        
    if (nodes[i].xmap != 0 || nodes[i].ymap != 0) {
      ang = atan2(nodes[i].ymap, nodes[i].xmap);
      if (setangz) {
        if (ang < mapzamin) mapzamin = ang;
        if (ang > mapzamax) mapzamax = ang;
      } else {
        mapzamin = mapzamax = ang;
        setangz = true;
      }
    }
  }

  // Fix the angular ranges
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
  
  if (mapzamax-mapzamin > Pi) {
    double aux = mapzamin;
    mapzamin = mapzamax;
    mapzamax = aux + TwoPi;
  }

  // Set the periodicity length (maybe not needed)
  mapsx = fabs(mapxmax - mapxmin);
  mapsy = fabs(mapymax - mapymin);
  mapsz = fabs(mapzmax - mapzmin);

  // Set provisional cell dimensions
  xMinBoundingBox = mapxmin;
  xMaxBoundingBox = mapxmax;
  yMinBoundingBox = mapymin;
  yMaxBoundingBox = mapymax;
  zMinBoundingBox = mapzmin;
  zMaxBoundingBox = mapzmax;
  hasBoundingBox = true;

  // Display the range if requested
  if (debug) PrintRange();
  
}

void 
ComponentFieldMap::PrintRange() {

  printf("ComponentFieldMap::PrintRange:\n");
  printf("        Dimensions of the elementary block\n");
  printf("            %15g < x < %-15g cm,\n", xMinBoundingBox, xMaxBoundingBox);
  printf("            %15g < y < %-15g cm,\n", yMinBoundingBox, yMaxBoundingBox);
  printf("            %15g < z < %-15g cm,\n", zMinBoundingBox, zMaxBoundingBox);
  printf("            %15g < V < %-15g V.\n",  mapvmin, mapvmax);
  
  printf("        Periodicities\n");
  
  printf("            x:");  
  if (xPeriodic)         {printf(" simple with length %g cm", cellsx);}
  if (xMirrorPeriodic)   {printf(" mirror with length %g cm", cellsx);}
  if (xAxiallyPeriodic)  {printf(" axial %d-fold repetition", int(0.5 + mapnxa));}
  if (xRotationSymmetry) {printf(" rotational symmetry");}
  if (!(xPeriodic || xMirrorPeriodic || xAxiallyPeriodic || xRotationSymmetry)) {printf(" none");}
  printf("\n");
  
  printf("            y:");
  if (yPeriodic)         {printf(" simple with length %g cm", cellsy);}
  if (yMirrorPeriodic)   {printf(" mirror with length %g cm", cellsy);}
  if (yAxiallyPeriodic)  {printf(" axial %d-fold repetition", int(0.5 + mapnya));}
  if (yRotationSymmetry) {printf(" rotational symmetry");}
  if (!(yPeriodic || yMirrorPeriodic || yAxiallyPeriodic || yRotationSymmetry)) {printf(" none");}
  printf("\n");
  
  printf("            z:");
  if (zPeriodic)         {printf(" simple with length %g cm", cellsz);}
  if (zMirrorPeriodic)   {printf(" mirror with length %g cm", cellsz);}
  if (zAxiallyPeriodic)  {printf(" axial %d-fold repetition", int(0.5 + mapnza));}
  if (zRotationSymmetry) {printf(" rotational symmetry");}
  if (!(zPeriodic || zMirrorPeriodic || zAxiallyPeriodic || zRotationSymmetry)) {printf(" none");}
  printf("\n");
  
}

bool
ComponentFieldMap::IsInBoundingBox(const double x, 
                                   const double y, const double z) {

  return false;

}

void 
ComponentFieldMap::MapCoordinates(double& xpos, double& ypos, double& zpos,
                         bool& xmirrored, bool& ymirrored, bool& zmirrored,
                         double& rcoordinate, double& rotation) {

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
    int nx = int(floor(0.5 + (xnew - xpos)  / (mapxmax - mapxmin)));
    if (nx != 2 * (nx / 2)) {
      xnew = mapxmin + mapxmax - xnew;
      xmirrored = true;
    }
    xpos = xnew;
  }
  if (xAxiallyPeriodic && (zpos != 0 || ypos != 0)) {
    auxr = sqrt(zpos * zpos + ypos * ypos);
    auxphi = atan2(zpos, ypos);
    rotation = (mapxamax - mapxamin) * floor(0.5 + (auxphi - 0.5 * (mapxamin + mapxamax)) / (mapxamax - mapxamin));
    if (auxphi - rotation < mapxamin) rotation = rotation - (mapxamax - mapxamin);
    if (auxphi - rotation > mapxamax) rotation = rotation + (mapxamax - mapxamin);
    auxphi = auxphi - rotation;
    ypos = auxr * cos(auxphi);
    zpos = auxr * sin(auxphi);
  }
  
  ymirrored = false;
  if (yPeriodic) {
    ypos = mapymin + fmod(ypos - mapymin, mapymax - mapymin);
    if (ypos < mapymin) ypos += mapymax-mapymin;
  } else if (yMirrorPeriodic) {
    double ynew = mapymin + fmod(ypos - mapymin, mapymax - mapymin);
    if (ynew < mapymin) ynew += mapymax-mapymin;
    int ny = int(floor(0.5 + (ynew - ypos) / (mapymax - mapymin)));
    if (ny != 2 * (ny / 2)) {
      ynew = mapymin + mapymax - ynew;
      ymirrored = true;
    }
    ypos = ynew;
  }
  if (yAxiallyPeriodic && (xpos != 0 || zpos != 0)) {
    auxr = sqrt(xpos * xpos + zpos * zpos);
    auxphi = atan2(xpos,zpos);
    rotation = (mapyamax - mapyamin) * floor(0.5 + (auxphi - 0.5 * (mapyamin + mapyamax)) / (mapyamax - mapyamin));
    if (auxphi - rotation < mapyamin) rotation = rotation - (mapyamax - mapyamin);
    if (auxphi - rotation > mapyamax) rotation = rotation + (mapyamax - mapyamin);
    auxphi = auxphi-rotation;
    zpos = auxr * cos(auxphi);
    xpos = auxr * sin(auxphi);
  }
  
  zmirrored = false;
  if (zPeriodic) {
    zpos = mapzmin + fmod(zpos - mapzmin, mapzmax - mapzmin);
    if (zpos < mapzmin) zpos += mapzmax - mapzmin;
  } else if (zMirrorPeriodic) {
    double znew = mapzmin + fmod(zpos - mapzmin, mapzmax - mapzmin);
    if (znew < mapzmin) znew += mapzmax-mapzmin;
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
    rotation = (mapzamax - mapzamin) * floor(0.5 + (auxphi - 0.5 * (mapzamin + mapzamax)) / (mapzamax - mapzamin));
    if (auxphi - rotation < mapzamin) rotation = rotation - (mapzamax - mapzamin);
    if (auxphi - rotation > mapzamax) rotation = rotation + (mapzamax - mapzamin);
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

void 
ComponentFieldMap::UnmapFields(double& ex, double& ey, double& ez,
                      double& xpos, double& ypos, double& zpos,
                      bool& xmirrored, bool& ymirrored, bool& zmirrored,
                      double& rcoordinate, double& rotation) {                                            

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

  // And take care of symmetry
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
    if (rcoordinate <=  0) {
      ex = 0;
      ey = eaxis;
      ez = 0;
    } else {
      ex = er*xpos/rcoordinate;
      ey = eaxis;
      ez = er*zpos/rcoordinate;
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

int 
ComponentFieldMap::ReadInteger(char* token, int def, bool& error) {
  
  if (!token) {
    error = true;
    return def;
  } 
  
  return atoi(token);

}
  
double 
ComponentFieldMap::ReadDouble(char* token, double def, bool& error) {
  
  if (!token) {
    error = true;
    return def;
  }
  return atof(token);
    
}


}
