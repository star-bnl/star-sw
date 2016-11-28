// Copied and modified ComponentAnsys123.cc

#include <stdio.h>
#include <string.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <stdlib.h>
#include <math.h>
#include <map>

#include "ComponentComsol.hh"

namespace Garfield {

ComponentComsol::ComponentComsol() : ComponentFieldMap() {

  m_className = "ComponentComsol";
  m_ready = false;
}

ComponentComsol::ComponentComsol(std::string mesh, std::string mplist,
                                 std::string field)
    : ComponentFieldMap() {

  m_className = "ComponentComsol";
  Initialise(mesh, mplist, field);
}

bool ends_with(std::string s, std::string t) {
  return s.size() >= t.size() && s.substr(s.size() - t.size(), t.size()) == t;
}

int readInt(std::string s) {
  std::istringstream iss(s);
  int ret;
  iss >> ret;
  return ret;
}

bool ComponentComsol::Initialise(std::string mesh, std::string mplist,
                                 std::string field) {
  double unit = 100.0;  // m

  std::string line;

  // Open the materials file.
  std::ifstream fmplist;
  fmplist.open(mplist.c_str(), std::ios::in);
  if (fmplist.fail()) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Could not open result file " << mplist
              << " for reading.\n";
    return false;
  }
  fmplist >> m_nMaterials;
  for (unsigned int i = 0; i < m_nMaterials; ++i) {
    material newMaterial;
    newMaterial.driftmedium = true;
    newMaterial.medium = nullptr;
    newMaterial.ohm = -1;
    fmplist >> newMaterial.eps;
    materials.push_back(newMaterial);
  }
  {
    // add default material
    material newMaterial;
    newMaterial.driftmedium = false;
    newMaterial.medium = nullptr;
    newMaterial.eps = newMaterial.ohm = -1;
    materials.push_back(newMaterial);
    m_nMaterials++;
  }
  std::map<int, int> domain2material;
  int d2msize;
  fmplist >> d2msize;
  for (int i = 0; i < d2msize; ++i) {
    int domain;
    fmplist >> domain;
    fmplist >> domain2material[domain];
  }
  fmplist.close();

  std::ifstream fmesh;
  fmesh.open(mesh.c_str(), std::ios::in);
  if (fmesh.fail()) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Could not open nodes file " << mesh << " for reading.\n";
    return false;
  }

  do {
    std::getline(fmesh, line);
  } while (!ends_with(line, "# number of mesh points"));
  nNodes = readInt(line);

  std::cout << m_className << "::Initialise:\n";
  std::cout << "    Read " << nNodes << " nodes from file " << mesh << ".\n";
  do {
    std::getline(fmesh, line);
  } while (line != "# Mesh point coordinates");
  double minx = 1e100, miny = 1e100, minz = 1e100, maxx = -1e100, maxy = -1e100,
         maxz = -1e100;
  for (int i = 0; i < nNodes; ++i) {
    node newNode;
    fmesh >> newNode.x >> newNode.y >> newNode.z;
    newNode.x *= unit;
    newNode.y *= unit;
    newNode.z *= unit;
    nodes.push_back(newNode);
    minx = std::min(minx, newNode.x);
    maxx = std::max(maxx, newNode.x);
    miny = std::min(miny, newNode.y);
    maxy = std::max(maxy, newNode.y);
    minz = std::min(minz, newNode.z);
    maxz = std::max(maxz, newNode.z);
  }
  std::cout << minx << " < x < " << maxx << "\n";
  std::cout << miny << " < y < " << maxy << "\n";
  std::cout << minz << " < z < " << maxz << "\n";

  do {
    std::getline(fmesh, line);
  } while (line != "4 tet2 # type name");
  do {
    std::getline(fmesh, line);
  } while (!ends_with(line, "# number of elements"));
  nElements = readInt(line);

  std::cout << m_className << "::Initialise:\n";
  std::cout << "    Read " << nElements << " elements from file " << mesh
            << ".\n";
  std::getline(fmesh, line);
  // elements 6 & 7 are swapped due to differences in COMSOL and ANSYS
  // representation
  int perm[10] = {0, 1, 2, 3, 4, 5, 7, 6, 8, 9};
  for (int i = 0; i < nElements; ++i) {
    element newElement;
    newElement.degenerate = false;
    for (int j = 0; j < 10; ++j) {
      fmesh >> newElement.emap[perm[j]];
    }
    elements.push_back(newElement);
  }

  do {
    std::getline(fmesh, line);
  } while (line != "# Geometric entity indices");
  for (int i = 0; i < nElements; ++i) {
    int domain;
    fmesh >> domain;
    elements[i].matmap = domain2material.count(domain) ? domain2material[domain]
                                                       : m_nMaterials - 1;
  }
  fmesh.close();

  std::map<node, std::vector<int>, nodeCmp> nodeIdx;
  for (int i = 0; i < nNodes; ++i) {
    nodeIdx[nodes[i]].push_back(i);
  }
  std::cout << "Map size: " << nodeIdx.size() << std::endl;

  std::ifstream ffield;
  ffield.open(field.c_str(), std::ios::in);
  if (ffield.fail()) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Could not open field potentials file " << field
              << " for reading.\n";
    return false;
  }
  do {
    std::getline(ffield, line);
  } while (line.substr(0, 81) !=
           "% x                       y                        z               "
           "         V (V)");
  {
    std::istringstream sline(line);
    std::string token;
    sline >> token;  // %
    sline >> token;  // x
    sline >> token;  // y
    sline >> token;  // z
    sline >> token;  // V
    sline >> token;  // (V)
    while (sline >> token) {
      std::cout << m_className << "::Initialise:\n";
      std::cout << "    Reading data for weighting field " << token << ".\n";
      nWeightingFields++;
      wfields.push_back(token);
      wfieldsOk.push_back(true);
      sline >> token;  // (V)
    }
  }
  for (int i = 0; i < nNodes; ++i) {
    node tmp;
    ffield >> tmp.x >> tmp.y >> tmp.z >> tmp.v;
    tmp.x *= unit;
    tmp.y *= unit;
    tmp.z *= unit;
    for (int j = 0; j < nWeightingFields; ++j) {
      double w;
      ffield >> w;
      tmp.w.push_back(w);
    }
    int closest = -1;
    double closestDist = 1;
    for (int j : nodeIdx[tmp]) {
      double dist = (tmp.x - nodes[j].x) * (tmp.x - nodes[j].x) +
                    (tmp.y - nodes[j].y) * (tmp.y - nodes[j].y) +
                    (tmp.z - nodes[j].z) * (tmp.z - nodes[j].z);
      if (dist < closestDist) {
        closestDist = dist;
        closest = j;
      }
    }
    if (closest == -1) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Could not match the node from field potentials file: "
                << tmp.x << " " << tmp.y << " " << tmp.z << "\n.";
      return false;
    }
    nodes[closest].v = tmp.v;
    nodes[closest].w = tmp.w;
  }

  m_ready = true;

  //  for (int i = 0; i < nNodes; ++i) {
  //    double ex, ey, ez, v;
  //    Medium* m;
  //    int status;
  //    ElectricField(nodes[i].x, nodes[i].y, nodes[i].z, ex, ey, ez, v, m,
  // status);
  //    std::cout << "Field at " << nodes[i].x << " " << nodes[i].y << " " <<
  // nodes[i].z << ": " << ex << " " << ey << " " << ez << " " << v << "\n";
  //  }

  // Establish the ranges.
  SetRange();
  UpdatePeriodicity();
  return true;
}

bool ComponentComsol::SetWeightingField(std::string field, std::string label) {
  double unit = 100.0;  // m;

  if (!m_ready) {
    std::cerr << m_className << "::SetWeightingField:\n";
    std::cerr << "    No valid field map is present.\n";
    std::cerr << "    Weighting field cannot be added.\n";
    return false;
  }

  // Open the voltage list.
  std::ifstream ffield;
  ffield.open(field.c_str(), std::ios::in);
  if (ffield.fail()) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Could not open field potentials file " << field
              << " for reading.\n";
    return false;
  }

  // Check if a weighting field with the same label alm_ready exists.
  int iw = nWeightingFields;
  for (int i = nWeightingFields; i--;) {
    if (wfields[i] == label) {
      iw = i;
      break;
    }
  }
  if (iw == nWeightingFields) {
    ++nWeightingFields;
    wfields.resize(nWeightingFields);
    wfieldsOk.resize(nWeightingFields);
    for (int j = 0; j < nNodes; ++j) {
      nodes[j].w.resize(nWeightingFields);
    }
  } else {
    std::cout << m_className << "::SetWeightingField:\n";
    std::cout << "    Replacing existing weighting field " << label << ".\n";
  }
  wfields[iw] = label;
  wfieldsOk[iw] = false;
  std::map<node, std::vector<int>, nodeCmp> nodeIdx;
  for (int i = 0; i < nNodes; ++i) {
    nodeIdx[nodes[i]].push_back(i);
  }
  std::cout << "Map size: " << nodeIdx.size() << std::endl;

  std::string line;
  do {
    std::getline(ffield, line);
  } while (line !=
           "% x                       y                        z               "
           "         V (V)");
  for (int i = 0; i < nNodes; ++i) {
    node tmp;
    ffield >> tmp.x >> tmp.y >> tmp.z >> tmp.v;
    tmp.x *= unit;
    tmp.y *= unit;
    tmp.z *= unit;
    int closest = -1;
    double closestDist = 1;
    for (int j : nodeIdx[tmp]) {
      double dist = (tmp.x - nodes[j].x) * (tmp.x - nodes[j].x) +
                    (tmp.y - nodes[j].y) * (tmp.y - nodes[j].y) +
                    (tmp.z - nodes[j].z) * (tmp.z - nodes[j].z);
      if (dist < closestDist) {
        closestDist = dist;
        closest = j;
      }
    }
    if (closest == -1) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Could not match the node from field potentials file: "
                << tmp.x << " " << tmp.y << " " << tmp.z << "\n.";
      return false;
    }
    nodes[closest].w[iw] = tmp.v;
  }

  return true;
}

void ComponentComsol::ElectricField(const double x, const double y,
                                    const double z, double& ex, double& ey,
                                    double& ez, Medium*& m, int& status) {

  double v = 0.;
  ElectricField(x, y, z, ex, ey, ez, v, m, status);
}

void ComponentComsol::ElectricField(const double xin, const double yin,
                                    const double zin, double& ex, double& ey,
                                    double& ez, double& volt, Medium*& m,
                                    int& status) {

  // Copy the coordinates
  double x = xin, y = yin, z = zin;

  // Map the coordinates onto field map coordinates
  bool xmirrored, ymirrored, zmirrored;
  double rcoordinate, rotation;
  MapCoordinates(x, y, z, xmirrored, ymirrored, zmirrored, rcoordinate,
                 rotation);

  // Initial values
  ex = ey = ez = volt = 0.;
  status = 0;
  m = 0;

  // Do not proceed if not properly initialised.
  if (!m_ready) {
    status = -10;
    std::cerr << m_className << "::ElectricField:\n";
    std::cerr << "    Field map not available for interpolation.\n";
    return;
  }

  if (warning) {
    std::cerr << m_className << "::ElectricField:\n";
    std::cerr << "    Warnings have been issued for this field map.\n";
  }

  // Find the element that contains this point
  double t1, t2, t3, t4, jac[4][4], det;
  int imap = FindElement13(x, y, z, t1, t2, t3, t4, jac, det);
  if (imap < 0) {
    //    std::cout << "ef @(" << xin << ", " << yin << ", " << zin << ") = not
    // in mesh" << "\n";
    if (m_debug) {
      std::cout << m_className << "::ElectricField:\n";
      std::cout << "    Point (" << x << ", " << y << ", " << z
                << " not in the mesh.\n";
    }
    status = -6;
    return;
  }

  if (m_debug) {
    std::cout << m_className << "::ElectricField:\n";
    std::cout << "    Global: (" << x << ", " << y << ", " << z << "),\n";
    std::cout << "    Local: (" << t1 << ", " << t2 << ", " << t3 << ", " << t4
              << " in element " << imap << "\n";
    std::cout
        << "      Node             x            y            z            V\n";
    for (int i = 0; i < 10; i++) {
      printf("      %-5d %12g %12g %12g %12g\n", elements[imap].emap[i],
             nodes[elements[imap].emap[i]].x, nodes[elements[imap].emap[i]].y,
             nodes[elements[imap].emap[i]].z, nodes[elements[imap].emap[i]].v);
    }
  }

  // Tetrahedral field
  volt = nodes[elements[imap].emap[0]].v * t1 * (2 * t1 - 1) +
         nodes[elements[imap].emap[1]].v * t2 * (2 * t2 - 1) +
         nodes[elements[imap].emap[2]].v * t3 * (2 * t3 - 1) +
         nodes[elements[imap].emap[3]].v * t4 * (2 * t4 - 1) +
         4 * nodes[elements[imap].emap[4]].v * t1 * t2 +
         4 * nodes[elements[imap].emap[5]].v * t1 * t3 +
         4 * nodes[elements[imap].emap[6]].v * t1 * t4 +
         4 * nodes[elements[imap].emap[7]].v * t2 * t3 +
         4 * nodes[elements[imap].emap[8]].v * t2 * t4 +
         4 * nodes[elements[imap].emap[9]].v * t3 * t4;
  ex = -(nodes[elements[imap].emap[0]].v * (4 * t1 - 1) * jac[0][1] +
         nodes[elements[imap].emap[1]].v * (4 * t2 - 1) * jac[1][1] +
         nodes[elements[imap].emap[2]].v * (4 * t3 - 1) * jac[2][1] +
         nodes[elements[imap].emap[3]].v * (4 * t4 - 1) * jac[3][1] +
         nodes[elements[imap].emap[4]].v *
             (4 * t2 * jac[0][1] + 4 * t1 * jac[1][1]) +
         nodes[elements[imap].emap[5]].v *
             (4 * t3 * jac[0][1] + 4 * t1 * jac[2][1]) +
         nodes[elements[imap].emap[6]].v *
             (4 * t4 * jac[0][1] + 4 * t1 * jac[3][1]) +
         nodes[elements[imap].emap[7]].v *
             (4 * t3 * jac[1][1] + 4 * t2 * jac[2][1]) +
         nodes[elements[imap].emap[8]].v *
             (4 * t4 * jac[1][1] + 4 * t2 * jac[3][1]) +
         nodes[elements[imap].emap[9]].v *
             (4 * t4 * jac[2][1] + 4 * t3 * jac[3][1])) /
       det;
  ey = -(nodes[elements[imap].emap[0]].v * (4 * t1 - 1) * jac[0][2] +
         nodes[elements[imap].emap[1]].v * (4 * t2 - 1) * jac[1][2] +
         nodes[elements[imap].emap[2]].v * (4 * t3 - 1) * jac[2][2] +
         nodes[elements[imap].emap[3]].v * (4 * t4 - 1) * jac[3][2] +
         nodes[elements[imap].emap[4]].v *
             (4 * t2 * jac[0][2] + 4 * t1 * jac[1][2]) +
         nodes[elements[imap].emap[5]].v *
             (4 * t3 * jac[0][2] + 4 * t1 * jac[2][2]) +
         nodes[elements[imap].emap[6]].v *
             (4 * t4 * jac[0][2] + 4 * t1 * jac[3][2]) +
         nodes[elements[imap].emap[7]].v *
             (4 * t3 * jac[1][2] + 4 * t2 * jac[2][2]) +
         nodes[elements[imap].emap[8]].v *
             (4 * t4 * jac[1][2] + 4 * t2 * jac[3][2]) +
         nodes[elements[imap].emap[9]].v *
             (4 * t4 * jac[2][2] + 4 * t3 * jac[3][2])) /
       det;
  ez = -(nodes[elements[imap].emap[0]].v * (4 * t1 - 1) * jac[0][3] +
         nodes[elements[imap].emap[1]].v * (4 * t2 - 1) * jac[1][3] +
         nodes[elements[imap].emap[2]].v * (4 * t3 - 1) * jac[2][3] +
         nodes[elements[imap].emap[3]].v * (4 * t4 - 1) * jac[3][3] +
         nodes[elements[imap].emap[4]].v *
             (4 * t2 * jac[0][3] + 4 * t1 * jac[1][3]) +
         nodes[elements[imap].emap[5]].v *
             (4 * t3 * jac[0][3] + 4 * t1 * jac[2][3]) +
         nodes[elements[imap].emap[6]].v *
             (4 * t4 * jac[0][3] + 4 * t1 * jac[3][3]) +
         nodes[elements[imap].emap[7]].v *
             (4 * t3 * jac[1][3] + 4 * t2 * jac[2][3]) +
         nodes[elements[imap].emap[8]].v *
             (4 * t4 * jac[1][3] + 4 * t2 * jac[3][3]) +
         nodes[elements[imap].emap[9]].v *
             (4 * t4 * jac[2][3] + 4 * t3 * jac[3][3])) /
       det;

  // Transform field to global coordinates
  UnmapFields(ex, ey, ez, x, y, z, xmirrored, ymirrored, zmirrored, rcoordinate,
              rotation);
  //  std::cout << "ef @(" << xin << ", " << yin << ", " << zin << ") = " <<
  // volt << "\n";

  // Drift medium?
  if (m_debug) {
    std::cout << m_className << "::ElectricField:\n";
    std::cout << "    Material " << elements[imap].matmap << ", drift flag "
              << materials[elements[imap].matmap].driftmedium << "\n";
  }
  m = materials[elements[imap].matmap].medium;
  status = -5;
  if (materials[elements[imap].matmap].driftmedium) {
    if (m != 0) {
      if (m->IsDriftable()) status = 0;
    }
  }
}

void ComponentComsol::WeightingField(const double xin, const double yin,
                                     const double zin, double& wx, double& wy,
                                     double& wz, const std::string& label) {

  // Initial values
  wx = wy = wz = 0;

  // Do not proceed if not properly initialised.
  if (!m_ready) return;

  // Look for the label.
  int iw = 0;
  bool found = false;
  for (int i = nWeightingFields; i--;) {
    if (wfields[i] == label) {
      iw = i;
      found = true;
      break;
    }
  }

  // Do not proceed if the requested weighting field does not exist.
  if (!found) return;
  // Check if the weighting field is properly initialised.
  if (!wfieldsOk[iw]) return;

  // Copy the coordinates.
  double x = xin, y = yin, z = zin;

  // Map the coordinates onto field map coordinates
  bool xmirrored, ymirrored, zmirrored;
  double rcoordinate, rotation;
  MapCoordinates(x, y, z, xmirrored, ymirrored, zmirrored, rcoordinate,
                 rotation);

  if (warning) {
    std::cerr << m_className << "::WeightingField:\n";
    std::cerr << "    Warnings have been issued for this field map.\n";
  }

  // Find the element that contains this point.
  double t1, t2, t3, t4, jac[4][4], det;
  int imap = FindElement13(x, y, z, t1, t2, t3, t4, jac, det);
  // Check if the point is in the mesh.
  if (imap < 0) return;

  if (m_debug) {
    std::cout << m_className << "::WeightingField:\n";
    std::cout << "    Global: (" << x << ", " << y << ", " << z << "),\n";
    std::cout << "    Local: (" << t1 << ", " << t2 << ", " << t3 << ", " << t4
              << " in element " << imap << "\n";
    std::cout
        << "      Node             x            y            z            V\n";
    for (int i = 0; i < 10; i++) {
      printf("      %-5d %12g %12g %12g %12g\n", elements[imap].emap[i],
             nodes[elements[imap].emap[i]].x, nodes[elements[imap].emap[i]].y,
             nodes[elements[imap].emap[i]].z,
             nodes[elements[imap].emap[i]].w[iw]);
    }
  }

  // Tetrahedral field
  wx = -(nodes[elements[imap].emap[0]].w[iw] * (4 * t1 - 1) * jac[0][1] +
         nodes[elements[imap].emap[1]].w[iw] * (4 * t2 - 1) * jac[1][1] +
         nodes[elements[imap].emap[2]].w[iw] * (4 * t3 - 1) * jac[2][1] +
         nodes[elements[imap].emap[3]].w[iw] * (4 * t4 - 1) * jac[3][1] +
         nodes[elements[imap].emap[4]].w[iw] *
             (4 * t2 * jac[0][1] + 4 * t1 * jac[1][1]) +
         nodes[elements[imap].emap[5]].w[iw] *
             (4 * t3 * jac[0][1] + 4 * t1 * jac[2][1]) +
         nodes[elements[imap].emap[6]].w[iw] *
             (4 * t4 * jac[0][1] + 4 * t1 * jac[3][1]) +
         nodes[elements[imap].emap[7]].w[iw] *
             (4 * t3 * jac[1][1] + 4 * t2 * jac[2][1]) +
         nodes[elements[imap].emap[8]].w[iw] *
             (4 * t4 * jac[1][1] + 4 * t2 * jac[3][1]) +
         nodes[elements[imap].emap[9]].w[iw] *
             (4 * t4 * jac[2][1] + 4 * t3 * jac[3][1])) /
       det;

  wy = -(nodes[elements[imap].emap[0]].w[iw] * (4 * t1 - 1) * jac[0][2] +
         nodes[elements[imap].emap[1]].w[iw] * (4 * t2 - 1) * jac[1][2] +
         nodes[elements[imap].emap[2]].w[iw] * (4 * t3 - 1) * jac[2][2] +
         nodes[elements[imap].emap[3]].w[iw] * (4 * t4 - 1) * jac[3][2] +
         nodes[elements[imap].emap[4]].w[iw] *
             (4 * t2 * jac[0][2] + 4 * t1 * jac[1][2]) +
         nodes[elements[imap].emap[5]].w[iw] *
             (4 * t3 * jac[0][2] + 4 * t1 * jac[2][2]) +
         nodes[elements[imap].emap[6]].w[iw] *
             (4 * t4 * jac[0][2] + 4 * t1 * jac[3][2]) +
         nodes[elements[imap].emap[7]].w[iw] *
             (4 * t3 * jac[1][2] + 4 * t2 * jac[2][2]) +
         nodes[elements[imap].emap[8]].w[iw] *
             (4 * t4 * jac[1][2] + 4 * t2 * jac[3][2]) +
         nodes[elements[imap].emap[9]].w[iw] *
             (4 * t4 * jac[2][2] + 4 * t3 * jac[3][2])) /
       det;

  wz = -(nodes[elements[imap].emap[0]].w[iw] * (4 * t1 - 1) * jac[0][3] +
         nodes[elements[imap].emap[1]].w[iw] * (4 * t2 - 1) * jac[1][3] +
         nodes[elements[imap].emap[2]].w[iw] * (4 * t3 - 1) * jac[2][3] +
         nodes[elements[imap].emap[3]].w[iw] * (4 * t4 - 1) * jac[3][3] +
         nodes[elements[imap].emap[4]].w[iw] *
             (4 * t2 * jac[0][3] + 4 * t1 * jac[1][3]) +
         nodes[elements[imap].emap[5]].w[iw] *
             (4 * t3 * jac[0][3] + 4 * t1 * jac[2][3]) +
         nodes[elements[imap].emap[6]].w[iw] *
             (4 * t4 * jac[0][3] + 4 * t1 * jac[3][3]) +
         nodes[elements[imap].emap[7]].w[iw] *
             (4 * t3 * jac[1][3] + 4 * t2 * jac[2][3]) +
         nodes[elements[imap].emap[8]].w[iw] *
             (4 * t4 * jac[1][3] + 4 * t2 * jac[3][3]) +
         nodes[elements[imap].emap[9]].w[iw] *
             (4 * t4 * jac[2][3] + 4 * t3 * jac[3][3])) /
       det;

  // Transform field to global coordinates
  UnmapFields(wx, wy, wz, x, y, z, xmirrored, ymirrored, zmirrored, rcoordinate,
              rotation);
}

double ComponentComsol::WeightingPotential(const double xin, const double yin,
                                           const double zin,
                                           const std::string& label) {

  // Do not proceed if not properly initialised.
  if (!m_ready) return 0.;

  // Look for the label.
  int iw = 0;
  bool found = false;
  for (int i = nWeightingFields; i--;) {
    if (wfields[i] == label) {
      iw = i;
      found = true;
      break;
    }
  }

  // Do not proceed if the requested weighting field does not exist.
  if (!found) return 0.;
  // Check if the weighting field is properly initialised.
  if (!wfieldsOk[iw]) return 0.;

  // Copy the coordinates.
  double x = xin, y = yin, z = zin;

  // Map the coordinates onto field map coordinates.
  bool xmirrored, ymirrored, zmirrored;
  double rcoordinate, rotation;
  MapCoordinates(x, y, z, xmirrored, ymirrored, zmirrored, rcoordinate,
                 rotation);

  if (warning) {
    std::cerr << m_className << "::WeightingPotential:\n";
    std::cerr << "    Warnings have been issued for this field map.\n";
  }

  // Find the element that contains this point.
  double t1, t2, t3, t4, jac[4][4], det;
  int imap = FindElement13(x, y, z, t1, t2, t3, t4, jac, det);
  if (imap < 0) return 0.;

  if (m_debug) {
    std::cout << m_className << "::WeightingPotential:\n";
    std::cout << "    Global: (" << x << ", " << y << ", " << z << "),\n";
    std::cout << "    Local: (" << t1 << ", " << t2 << ", " << t3 << ", " << t4
              << " in element " << imap << "\n";
    std::cout
        << "      Node             x            y            z            V\n";
    for (int i = 0; i < 10; i++) {
      printf("      %-5d %12g %12g %12g %12g\n", elements[imap].emap[i],
             nodes[elements[imap].emap[i]].x, nodes[elements[imap].emap[i]].y,
             nodes[elements[imap].emap[i]].z, nodes[elements[imap].emap[i]].v);
    }
  }

  // Tetrahedral field
  return nodes[elements[imap].emap[0]].w[iw] * t1 * (2 * t1 - 1) +
         nodes[elements[imap].emap[1]].w[iw] * t2 * (2 * t2 - 1) +
         nodes[elements[imap].emap[2]].w[iw] * t3 * (2 * t3 - 1) +
         nodes[elements[imap].emap[3]].w[iw] * t4 * (2 * t4 - 1) +
         4 * nodes[elements[imap].emap[4]].w[iw] * t1 * t2 +
         4 * nodes[elements[imap].emap[5]].w[iw] * t1 * t3 +
         4 * nodes[elements[imap].emap[6]].w[iw] * t1 * t4 +
         4 * nodes[elements[imap].emap[7]].w[iw] * t2 * t3 +
         4 * nodes[elements[imap].emap[8]].w[iw] * t2 * t4 +
         4 * nodes[elements[imap].emap[9]].w[iw] * t3 * t4;
}

Medium* ComponentComsol::GetMedium(const double xin, const double yin,
                                   const double zin) {

  // Copy the coordinates
  double x = xin, y = yin, z = zin;

  // Map the coordinates onto field map coordinates
  bool xmirrored, ymirrored, zmirrored;
  double rcoordinate, rotation;
  MapCoordinates(x, y, z, xmirrored, ymirrored, zmirrored, rcoordinate,
                 rotation);

  // Do not proceed if not properly initialised.
  if (!m_ready) {
    std::cerr << m_className << "::GetMedium:\n";
    std::cerr << "    Field map not available for interpolation.\n";
    return nullptr;
  }
  if (warning) {
    std::cerr << m_className << "::GetMedium:\n";
    std::cerr << "    Warnings have been issued for this field map.\n";
  }

  // Find the element that contains this point
  double t1, t2, t3, t4, jac[4][4], det;
  int imap = FindElement13(x, y, z, t1, t2, t3, t4, jac, det);
  if (imap < 0) {
    if (m_debug) {
      std::cout << m_className << "::GetMedium:\n";
      std::cout << "    Point (" << x << ", " << y << ", " << z
                << ") not in the mesh.\n";
    }
    return nullptr;
  }
  if (elements[imap].matmap >= m_nMaterials) {
    if (m_debug) {
      std::cerr << m_className << "::GetMedium:\n";
      std::cerr << "    Point (" << x << ", " << y
                << ") has out of range material number " << imap << ".\n";
    }
    return nullptr;
  }

  if (m_debug) {
    std::cout << m_className << "::GetMedium:\n";
    std::cout << "    Global: (" << x << ", " << y << ", " << z << "),\n";
    std::cout << "    Local: (" << t1 << ", " << t2 << ", " << t3 << ", " << t4
              << " in element " << imap << "\n";
    std::cout
        << "      Node             x            y            z            V\n";
    for (int ii = 0; ii < 10; ++ii) {
      printf("      %-5d %12g %12g %12g %12g\n", elements[imap].emap[ii],
             nodes[elements[imap].emap[ii]].x, nodes[elements[imap].emap[ii]].y,
             nodes[elements[imap].emap[ii]].z,
             nodes[elements[imap].emap[ii]].v);
    }
  }

  return materials[elements[imap].matmap].medium;
}

double ComponentComsol::GetElementVolume(const int i) {

  if (i < 0 || i >= nElements) return 0.;

  // Uses formula V = |a (dot) b x c|/6
  // with a => "3", b => "1", c => "2" and origin "0"
  const double vol =
      fabs((nodes[elements[i].emap[3]].x - nodes[elements[i].emap[0]].x) *
               ((nodes[elements[i].emap[1]].y - nodes[elements[i].emap[0]].y) *
                    (nodes[elements[i].emap[2]].z -
                     nodes[elements[i].emap[0]].z) -
                (nodes[elements[i].emap[2]].y - nodes[elements[i].emap[0]].y) *
                    (nodes[elements[i].emap[1]].z -
                     nodes[elements[i].emap[0]].z)) +
           (nodes[elements[i].emap[3]].y - nodes[elements[i].emap[0]].y) *
               ((nodes[elements[i].emap[1]].z - nodes[elements[i].emap[0]].z) *
                    (nodes[elements[i].emap[2]].x -
                     nodes[elements[i].emap[0]].x) -
                (nodes[elements[i].emap[2]].z - nodes[elements[i].emap[0]].z) *
                    (nodes[elements[i].emap[1]].x -
                     nodes[elements[i].emap[0]].x)) +
           (nodes[elements[i].emap[3]].z - nodes[elements[i].emap[0]].z) *
               ((nodes[elements[i].emap[1]].x - nodes[elements[i].emap[0]].x) *
                    (nodes[elements[i].emap[2]].y -
                     nodes[elements[i].emap[0]].y) -
                (nodes[elements[i].emap[3]].x - nodes[elements[i].emap[0]].x) *
                    (nodes[elements[i].emap[1]].y -
                     nodes[elements[i].emap[0]].y))) /
      6.;
  return vol;
}

void ComponentComsol::GetAspectRatio(const int i, double& dmin, double& dmax) {

  if (i < 0 || i >= nElements) {
    dmin = dmax = 0.;
    return;
  }

  const int np = 4;
  // Loop over all pairs of vertices.
  for (int j = 0; j < np - 1; ++j) {
    for (int k = j + 1; k < np; ++k) {
      // Compute distance.
      const double dist = sqrt(
          pow(nodes[elements[i].emap[j]].x - nodes[elements[i].emap[k]].x, 2) +
          pow(nodes[elements[i].emap[j]].y - nodes[elements[i].emap[k]].y, 2) +
          pow(nodes[elements[i].emap[j]].z - nodes[elements[i].emap[k]].z, 2));
      if (k == 1) {
        dmin = dist;
        dmax = dist;
      } else {
        if (dist < dmin) dmin = dist;
        if (dist > dmax) dmax = dist;
      }
    }
  }
}

}  // namespace Garfield
