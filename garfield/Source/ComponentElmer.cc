// Copied and modified ComponentAnsys123.cc

#include <stdio.h>
#include <string.h>
#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <math.h>

#include "ComponentElmer.hh"

namespace Garfield {

ComponentElmer::ComponentElmer() : ComponentFieldMap() {

  m_className = "ComponentElmer";
  m_ready = false;
}

ComponentElmer::ComponentElmer(std::string header, std::string elist,
                               std::string nlist, std::string mplist,
                               std::string volt, std::string unit)
    : ComponentFieldMap() {

  m_className = "ComponentElmer";
  Initialise(header, elist, nlist, mplist, volt, unit);
}

bool ComponentElmer::Initialise(std::string header, std::string elist,
                                std::string nlist, std::string mplist,
                                std::string volt, std::string unit) {

  m_debug = false;
  m_ready = false;

  // Keep track of the success.
  bool ok = true;

  // Buffer for reading
  const int size = 100;
  char line[size];

  // Open the header.
  std::ifstream fheader;
  fheader.open(header.c_str(), std::ios::in);
  if (fheader.fail()) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Could not open header file " << header
              << " for reading.\n";
  }

  // Temporary variables for use in file reading
  char* token = NULL;
  bool readerror = false;
  bool readstop = false;
  int il = 0;

  // Read the header to get the number of nodes and elements.
  fheader.getline(line, size, '\n');
  token = strtok(line, " ");
  nNodes = ReadInteger(token, 0, readerror);
  token = strtok(NULL, " ");
  nElements = ReadInteger(token, 0, readerror);
  std::cout << "ComponentElmer::Initialise:\n";
  std::cout << "    Read " << nNodes << " nodes and " << nElements
            << " elements from file " << header << ".\n";
  if (readerror) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Error reading file " << header << " (line " << il
              << ").\n";
    fheader.close();
    ok = false;
    return false;
  }

  // Close the header file.
  fheader.close();

  // Open the nodes list.
  std::ifstream fnodes;
  fnodes.open(nlist.c_str(), std::ios::in);
  if (fnodes.fail()) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Could not open nodes file " << nlist << " for reading.\n";
  }

  // Check the value of the unit.
  double funit;
  if (strcmp(unit.c_str(), "mum") == 0 || strcmp(unit.c_str(), "micron") == 0 ||
      strcmp(unit.c_str(), "micrometer") == 0) {
    funit = 0.0001;
  } else if (strcmp(unit.c_str(), "mm") == 0 ||
             strcmp(unit.c_str(), "millimeter") == 0) {
    funit = 0.1;
  } else if (strcmp(unit.c_str(), "cm") == 0 ||
             strcmp(unit.c_str(), "centimeter") == 0) {
    funit = 1.0;
  } else if (strcmp(unit.c_str(), "m") == 0 ||
             strcmp(unit.c_str(), "meter") == 0) {
    funit = 100.0;
  } else {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Unknown length unit " << unit << ".\n";
    ok = false;
    funit = 1.0;
  }
  if (m_debug) {
    std::cout << "ComponentElmer::Initialise:\n";
    std::cout << "    Unit scaling factor = " << funit << ".\n";
  }

  // Read the nodes from the file.
  node newNode;
  newNode.w.clear();
  for (il = 0; il < nNodes; il++) {

    // Get a line from the nodes file.
    fnodes.getline(line, size, '\n');

    // Ignore the first two characters.
    token = strtok(line, " ");
    token = strtok(NULL, " ");

    // Get the node coordinates.
    token = strtok(NULL, " ");
    double xnode = ReadDouble(token, -1, readerror);
    token = strtok(NULL, " ");
    double ynode = ReadDouble(token, -1, readerror);
    token = strtok(NULL, " ");
    double znode = ReadDouble(token, -1, readerror);
    if (readerror) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Error reading file " << nlist << " (line " << il
                << ").\n";
      fnodes.close();
      ok = false;
      return false;
    }

    // Set up and create a new node.
    newNode.x = xnode * funit;
    newNode.y = ynode * funit;
    newNode.z = znode * funit;
    nodes.push_back(newNode);
  }

  // Close the nodes file.
  fnodes.close();

  // Open the potential file.
  std::ifstream fvolt;
  fvolt.open(volt.c_str(), std::ios::in);
  if (fvolt.fail()) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Could not open result file " << volt << " for reading.\n";
  }

  // Reset the line counter.
  il = 1;

  // Read past the header.
  while (!readstop && fvolt.getline(line, size, '\n')) {
    token = strtok(line, " ");
    if (strcmp(token, "Perm:") == 0) readstop = true;
    il++;
  }

  // Should have stopped: if not, print error message.
  if (!readstop) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Error reading past header of potentials file " << volt
              << ".\n";
    fvolt.close();
    ok = false;
    return false;
  }

  // Read past the permutation map (number of lines = nNodes).
  for (int tl = 0; tl < nNodes; tl++) {
    fvolt.getline(line, size, '\n');
    il++;
  }

  // Read the potentials.
  for (int tl = 0; tl < nNodes; tl++) {
    double v;
    fvolt.getline(line, size, '\n');
    token = strtok(line, " ");
    v = ReadDouble(token, -1, readerror);
    if (readerror) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Error reading file " << volt << " (line " << il
                << ").\n";
      fvolt.close();
      ok = false;
      return false;
    }
    // Place the voltage in its appropriate node.
    nodes[tl].v = v;
  }

  // Close the potentials file.
  fvolt.close();

  // Open the materials file.
  std::ifstream fmplist;
  fmplist.open(mplist.c_str(), std::ios::in);
  if (fmplist.fail()) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Could not open result file " << mplist
              << " for reading.\n";
  }

  // Read the dielectric constants from the materials file.
  fmplist.getline(line, size, '\n');
  token = strtok(line, " ");
  if (readerror) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Error reading number of materials from " << mplist
              << ".\n";
    fmplist.close();
    ok = false;
    return false;
  }
  m_nMaterials = ReadInteger(token, 0, readerror);
  materials.resize(m_nMaterials);
  for (unsigned int i = 0; i < m_nMaterials; ++i) {
    materials[i].ohm = -1;
    materials[i].eps = -1;
    materials[i].medium = NULL;
  }
  for (il = 2; il < ((int)m_nMaterials + 2); il++) {
    fmplist.getline(line, size, '\n');
    token = strtok(line, " ");
    ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    double dc = ReadDouble(token, -1.0, readerror);
    if (readerror) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Error reading file " << mplist << " (line " << il
                << ").\n";
      fmplist.close();
      ok = false;
      return false;
    }
    materials[il - 2].eps = dc;
    std::cout << m_className << "::Initialise:\n";
    std::cout << "    Set material " << il - 2 << " of " << m_nMaterials
              << " to eps " << dc << ".\n";
  }

  // Close the materials file.
  fmplist.close();

  // Find the lowest epsilon, check for eps = 0, set default drift media.
  double epsmin = -1.;
  unsigned int iepsmin = 0;
  for (unsigned int imat = 0; imat < m_nMaterials; ++imat) {
    if (materials[imat].eps < 0) continue;
    if (materials[imat].eps == 0) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Material " << imat
                << " has been assigned a permittivity\n";
      std::cerr << "    equal to zero in " << mplist << ".\n";
      ok = false;
    } else if (epsmin < 0. || epsmin > materials[imat].eps) {
      epsmin = materials[imat].eps;
      iepsmin = imat;
    }
  }

  if (epsmin < 0.) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    No material with positive permittivity found \n";
    std::cerr << "    in material list " << mplist << ".\n";
    ok = false;
  } else {
    for (unsigned int imat = 0; imat < m_nMaterials; ++imat) {
      if (imat == iepsmin) {
        materials[imat].driftmedium = true;
      } else {
        materials[imat].driftmedium = false;
      }
    }
  }

  // Open the elements file.
  std::ifstream felems;
  felems.open(elist.c_str(), std::ios::in);
  if (felems.fail()) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Could not open result file " << elist
              << " for reading.\n";
  }

  // Read the elements and their material indices.
  elements.clear();
  int highestnode = 0;
  element newElement;
  for (il = 0; il < nElements; il++) {

    // Get a line
    felems.getline(line, size, '\n');

    // Split into tokens.
    token = strtok(line, " ");
    // Read the 2nd-order element
    // Note: Ordering of Elmer elements can be described in the
    // ElmerSolver manual (appendix D. at the time of this comment)
    // If the order read below is compared to the shape functions used
    // eg. in ElectricField, the order is wrong, but note at the
    // end of this function the order of elements 5,6,7 will change to
    // 7,5,6 when actually recorded in newElement.emap to correct for this
    token = strtok(NULL, " ");
    int imat = ReadInteger(token, -1, readerror) - 1;
    token = strtok(NULL, " ");
    token = strtok(NULL, " ");
    int in0 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    int in1 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    int in2 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    int in3 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    int in4 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    int in5 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    int in6 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    int in7 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    int in8 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    int in9 = ReadInteger(token, -1, readerror);

    if (m_debug && il < 10) {
      std::cout << "    Read nodes " << in0 << ", " << in1 << ", " << in2
                << ", " << in3 << ", ... from element " << il + 1 << " of "
                << nElements << " with mat " << imat << ".\n";
    }

    // Check synchronisation.
    if (readerror) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Error reading file " << elist << " (line " << il
                << ").\n";
      felems.close();
      ok = false;
      return false;
    }

    // Check the material number and ensure that epsilon is non-negative.
    if (imat < 0 || imat > (int)m_nMaterials) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Out-of-range material number on file " << elist
                << " (line " << il << ").\n";
      std::cerr << "    Element: " << il << ", material: " << imat << "\n";
      std::cerr << "    nodes: (" << in0 << ", " << in1 << ", " << in2 << ", "
                << in3 << ", " << in4 << ", " << in5 << ", " << in6 << ", "
                << in7 << ", " << in8 << ", " << in9 << ")\n";
      ok = false;
    }
    if (materials[imat].eps < 0) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Element " << il << " in element list " << elist << "\n";
      std::cerr << "    uses material " << imat
                << " which has not been assigned\n";
      std::cerr << "    a positive permittivity in material list " << mplist
                << ".\n";
      ok = false;
    }

    // Check the node numbers.
    if (in0 < 1 || in1 < 1 || in2 < 1 || in3 < 1 || in4 < 1 || in5 < 1 ||
        in6 < 1 || in7 < 1 || in8 < 1 || in9 < 1) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Found a node number < 1 on file " << elist << " (line "
                << il << ").\n";
      std::cerr << "    Element: " << il << ", material: " << imat << "\n";
      std::cerr << "    nodes: (" << in0 << ", " << in1 << ", " << in2 << ", "
                << in3 << ", " << in4 << ", " << in5 << ", " << in6 << ", "
                << in7 << ", " << in8 << ", " << in9 << ")\n";
      ok = false;
    }
    if (in0 > highestnode) highestnode = in0;
    if (in1 > highestnode) highestnode = in1;
    if (in2 > highestnode) highestnode = in2;
    if (in3 > highestnode) highestnode = in3;
    if (in4 > highestnode) highestnode = in4;
    if (in5 > highestnode) highestnode = in5;
    if (in6 > highestnode) highestnode = in6;
    if (in7 > highestnode) highestnode = in7;
    if (in8 > highestnode) highestnode = in8;
    if (in9 > highestnode) highestnode = in9;

    // These elements must not be degenerate.
    if (in0 == in1 || in0 == in2 || in0 == in3 || in0 == in4 || in0 == in5 ||
        in0 == in6 || in0 == in7 || in0 == in8 || in0 == in9 || in1 == in2 ||
        in1 == in3 || in1 == in4 || in1 == in5 || in1 == in6 || in1 == in7 ||
        in1 == in8 || in1 == in9 || in2 == in3 || in2 == in4 || in2 == in5 ||
        in2 == in6 || in2 == in7 || in2 == in8 || in2 == in9 || in3 == in4 ||
        in3 == in5 || in3 == in6 || in3 == in7 || in3 == in8 || in3 == in9 ||
        in4 == in5 || in4 == in6 || in4 == in7 || in4 == in8 || in4 == in9 ||
        in5 == in6 || in5 == in7 || in5 == in8 || in5 == in9 || in6 == in7 ||
        in6 == in8 || in6 == in9 || in7 == in8 || in7 == in9 || in8 == in9) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Element " << il << " of file " << elist
                << " is degenerate,\n";
      std::cerr << "    no such elements allowed in this type of map.\n";
      ok = false;
    }

    newElement.degenerate = false;

    // Store the material reference.
    newElement.matmap = imat;

    // Node references
    newElement.emap[0] = in0 - 1;
    newElement.emap[1] = in1 - 1;
    newElement.emap[2] = in2 - 1;
    newElement.emap[3] = in3 - 1;
    newElement.emap[4] = in4 - 1;
    newElement.emap[7] = in5 - 1;
    newElement.emap[5] = in6 - 1;
    newElement.emap[6] = in7 - 1;
    newElement.emap[8] = in8 - 1;
    newElement.emap[9] = in9 - 1;
    elements.push_back(newElement);
  }

  // Close the elements file.
  felems.close();

  // Set the ready flag.
  if (ok) {
    m_ready = true;
  } else {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr
        << "    Field map could not be read and can not be interpolated.\n";
    return false;
  }

  std::cout << "ComponentElmer::Initialise:\n";
  std::cout << "    Finished.\n";

  // Remove weighting fields (if any).
  wfields.clear();
  wfieldsOk.clear();
  nWeightingFields = 0;

  // Establish the ranges.
  SetRange();
  UpdatePeriodicity();
  return true;
}

bool ComponentElmer::SetWeightingField(std::string wvolt, std::string label) {

  if (!m_ready) {
    std::cerr << m_className << "::SetWeightingField:\n";
    std::cerr << "    No valid field map is present.\n";
    std::cerr << "    Weighting field cannot be added.\n";
    return false;
  }

  // Keep track of the success.
  bool ok = true;

  // Open the voltage list.
  std::ifstream fwvolt;
  fwvolt.open(wvolt.c_str(), std::ios::in);
  if (fwvolt.fail()) {
    std::cerr << m_className << "::SetWeightingField:\n";
    std::cerr << "    Could not open potential file " << wvolt
              << " for reading.\n";
    std::cerr << "    The file perhaps does not exist.\n";
    return false;
  }

  // Check if a weighting field with the same label already exists.
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
    for (int j = nNodes; j--;) {
      nodes[j].w.resize(nWeightingFields);
    }
  } else {
    std::cout << m_className << "::SetWeightingField:\n";
    std::cout << "    Replacing existing weighting field " << label << ".\n";
  }
  wfields[iw] = label;
  wfieldsOk[iw] = false;

  // Temporary variables for use in file reading
  const int size = 100;
  char line[size];
  char* token = NULL;
  bool readerror = false;
  bool readstop = false;
  int il = 1;

  // Read past the header.
  while (!readstop && fwvolt.getline(line, size, '\n')) {
    token = strtok(line, " ");
    if (strcmp(token, "Perm:") == 0) readstop = true;
    il++;
  }

  // Should have stopped: if not, print error message.
  if (!readstop) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Error reading past header of potentials file " << wvolt
              << ".\n";
    fwvolt.close();
    ok = false;
    return false;
  }

  // Read past the permutation map (number of lines = nNodes).
  for (int tl = 0; tl < nNodes; tl++) {
    fwvolt.getline(line, size, '\n');
    il++;
  }

  // Read the potentials.
  for (int tl = 0; tl < nNodes; tl++) {
    double v;
    fwvolt.getline(line, size, '\n');
    token = strtok(line, " ");
    v = ReadDouble(token, -1, readerror);
    if (readerror) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Error reading file " << wvolt << " (line " << il
                << ").\n";
      fwvolt.close();
      ok = false;
      return false;
    }
    // Place the weighting potential at its appropriate node and index.
    nodes[tl].w[iw] = v;
  }

  // Close the potentials file.
  fwvolt.close();
  std::cout << m_className << "::SetWeightingField:\n";
  std::cout << "    Read potentials from file " << wvolt.c_str() << ".\n";

  // Set the ready flag.
  wfieldsOk[iw] = ok;
  if (!ok) {
    std::cerr << m_className << "::SetWeightingField:\n";
    std::cerr << "    Field map could not be read "
              << "and cannot be interpolated.\n";
    return false;
  }
  return true;
}

void ComponentElmer::ElectricField(const double x, const double y,
                                   const double z, double& ex, double& ey,
                                   double& ez, Medium*& m, int& status) {

  double v = 0.;
  ElectricField(x, y, z, ex, ey, ez, v, m, status);
}

void ComponentElmer::ElectricField(const double xin, const double yin,
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

void ComponentElmer::WeightingField(const double xin, const double yin,
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

double ComponentElmer::WeightingPotential(const double xin, const double yin,
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

Medium* ComponentElmer::GetMedium(const double xin, const double yin,
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
    return NULL;
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
    return NULL;
  }
  if (elements[imap].matmap >= m_nMaterials) {
    if (m_debug) {
      std::cerr << m_className << "::GetMedium:\n";
      std::cerr << "    Point (" << x << ", " << y
                << ") has out of range material number " << imap << ".\n";
    }
    return NULL;
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

double ComponentElmer::GetElementVolume(const int i) {

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

void ComponentElmer::GetAspectRatio(const int i, double& dmin, double& dmax) {

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
