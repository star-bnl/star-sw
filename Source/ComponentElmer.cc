// Copied and modified ComponentAnsys123.cc

#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <math.h>

#include "ComponentElmer.hh"

namespace {

void PrintErrorReadingFile(const std::string& hdr, const std::string& file,
                           const int line) { 
  std::cerr << hdr << "\n    Error reading file " << file << " (line " 
            << line << ").\n";
}

void PrintErrorOpeningFile(const std::string& hdr, 
                           const std::string& filetype, 
                           const std::string& filename) {

  std::cerr << hdr << "\n    Could not open " << filetype << " file "
            << filename << " for reading.\n";
  std::cerr << "    The file perhaps does not exist.\n";
}
}

namespace Garfield {

ComponentElmer::ComponentElmer() : ComponentFieldMap() {

  m_className = "ComponentElmer";
}

ComponentElmer::ComponentElmer(const std::string& header, 
                               const std::string& elist,
                               const std::string& nlist, 
                               const std::string& mplist,
                               const std::string& volt, 
                               const std::string& unit)
    : ComponentFieldMap() {

  m_className = "ComponentElmer";
  Initialise(header, elist, nlist, mplist, volt, unit);
}

bool ComponentElmer::Initialise(const std::string& header, 
                                const std::string& elist,
                                const std::string& nlist, 
                                const std::string& mplist,
                                const std::string& volt, 
                                const std::string& unit) {

  const std::string hdr = m_className + "::Initialise:";
  m_debug = false;
  m_ready = false;
  m_warning = false;
  m_nWarnings = 0;

  // Keep track of the success.
  bool ok = true;

  // Buffer for reading
  const int size = 100;
  char line[size];

  // Open the header.
  std::ifstream fheader;
  fheader.open(header.c_str(), std::ios::in);
  if (fheader.fail()) {
    PrintErrorOpeningFile(hdr, "header", header);
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
  std::cout << hdr << "\n    Read " << nNodes << " nodes and " << nElements
            << " elements from file " << header << ".\n";
  if (readerror) {
    PrintErrorReadingFile(hdr, header, il);
    fheader.close();
    return false;
  }

  // Close the header file.
  fheader.close();

  // Open the nodes list.
  std::ifstream fnodes;
  fnodes.open(nlist.c_str(), std::ios::in);
  if (fnodes.fail()) {
    PrintErrorOpeningFile(hdr, "nodes", nlist);
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
    std::cerr << hdr << " Unknown length unit " << unit << ".\n";
    ok = false;
    funit = 1.0;
  }
  if (m_debug) std::cout << hdr << " Unit scaling factor = " << funit << ".\n";

  // Read the nodes from the file.
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
      PrintErrorReadingFile(hdr, nlist, il);
      fnodes.close();
      return false;
    }

    // Set up and create a new node.
    Node newNode;
    newNode.w.clear();
    newNode.x = xnode * funit;
    newNode.y = ynode * funit;
    newNode.z = znode * funit;
    nodes.push_back(std::move(newNode));
  }

  // Close the nodes file.
  fnodes.close();

  // Open the potential file.
  std::ifstream fvolt;
  fvolt.open(volt.c_str(), std::ios::in);
  if (fvolt.fail()) {
    PrintErrorOpeningFile(hdr, "potentials", volt);
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
    std::cerr << hdr << "\n    Error reading past header of potentials file " 
              << volt << ".\n";
    fvolt.close();
    return false;
  }

  // Read past the permutation map (number of lines = nNodes).
  for (int tl = 0; tl < nNodes; tl++) {
    fvolt.getline(line, size, '\n');
    il++;
  }

  // Read the potentials.
  for (int tl = 0; tl < nNodes; tl++) {
    fvolt.getline(line, size, '\n');
    token = strtok(line, " ");
    double v = ReadDouble(token, -1, readerror);
    if (readerror) {
      PrintErrorReadingFile(hdr, volt, il);
      fvolt.close();
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
    PrintErrorOpeningFile(hdr, "materials", mplist);
  }

  // Read the dielectric constants from the materials file.
  fmplist.getline(line, size, '\n');
  token = strtok(line, " ");
  if (readerror) {
    std::cerr << hdr << "\n    Error reading number of materials from " 
              << mplist << ".\n";
    fmplist.close();
    ok = false;
    return false;
  }
  m_nMaterials = ReadInteger(token, 0, readerror);
  materials.resize(m_nMaterials);
  for (unsigned int i = 0; i < m_nMaterials; ++i) {
    materials[i].ohm = -1;
    materials[i].eps = -1;
    materials[i].medium = nullptr;
  }
  for (il = 2; il < ((int)m_nMaterials + 2); il++) {
    fmplist.getline(line, size, '\n');
    token = strtok(line, " ");
    ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    double dc = ReadDouble(token, -1.0, readerror);
    if (readerror) {
      PrintErrorReadingFile(hdr, mplist, il);
      fmplist.close();
      ok = false;
      return false;
    }
    materials[il - 2].eps = dc;
    std::cout << hdr << "\n    Set material " << il - 2 << " of " 
              << m_nMaterials << " to eps " << dc << ".\n";
  }

  // Close the materials file.
  fmplist.close();

  // Find the lowest epsilon, check for eps = 0, set default drift media.
  double epsmin = -1.;
  unsigned int iepsmin = 0;
  for (unsigned int imat = 0; imat < m_nMaterials; ++imat) {
    if (materials[imat].eps < 0) continue;
    if (materials[imat].eps == 0) {
      std::cerr << hdr << "\n    Material " << imat
                << " has been assigned a permittivity equal to zero in\n    "
                << mplist << ".\n";
      ok = false;
    } else if (epsmin < 0. || epsmin > materials[imat].eps) {
      epsmin = materials[imat].eps;
      iepsmin = imat;
    }
  }

  if (epsmin < 0.) {
    std::cerr << hdr << "\n    No material with positive permittivity found \n"
              << "    in material list " << mplist << ".\n";
    ok = false;
  } else {
    for (unsigned int imat = 0; imat < m_nMaterials; ++imat) {
      materials[imat].driftmedium = imat == iepsmin ? true : false;
    }
  }

  // Open the elements file.
  std::ifstream felems;
  felems.open(elist.c_str(), std::ios::in);
  if (felems.fail()) {
    PrintErrorOpeningFile(hdr, "elements", elist);
  }

  // Read the elements and their material indices.
  elements.clear();
  int highestnode = 0;
  Element newElement;
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
      PrintErrorReadingFile(hdr, elist, il);
      felems.close();
      ok = false;
      return false;
    }

    // Check the material number and ensure that epsilon is non-negative.
    if (imat < 0 || imat > (int)m_nMaterials) {
      std::cerr << hdr << "\n    Out-of-range material number on file " << elist
                << " (line " << il << ").\n";
      std::cerr << "    Element: " << il << ", material: " << imat << "\n";
      std::cerr << "    nodes: (" << in0 << ", " << in1 << ", " << in2 << ", "
                << in3 << ", " << in4 << ", " << in5 << ", " << in6 << ", "
                << in7 << ", " << in8 << ", " << in9 << ")\n";
      ok = false;
    }
    if (materials[imat].eps < 0) {
      std::cerr << hdr << "\n    Element " << il << " in element list " 
                << elist << "\n    uses material " << imat
                << " which has not been assigned a positive permittivity in "
                << mplist << ".\n";
      ok = false;
    }

    // Check the node numbers.
    if (in0 < 1 || in1 < 1 || in2 < 1 || in3 < 1 || in4 < 1 || in5 < 1 ||
        in6 < 1 || in7 < 1 || in8 < 1 || in9 < 1) {
      std::cerr << hdr << "\n    Found a node number < 1 on file " << elist 
                << " (line " << il << ").\n    Element: " << il 
                << ", material: " << imat << "\n    nodes: (" 
                << in0 << ", " << in1 << ", " << in2 << ", " << in3 << ", " 
                << in4 << ", " << in5 << ", " << in6 << ", " << in7 << ", " 
                << in8 << ", " << in9 << ")\n";
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
      std::cerr << hdr << "\n    Element " << il << " of file " << elist 
                << " is degenerate,\n"
                << "    no such elements are allowed in this type of map.\n";
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
    std::cerr << hdr << "\n    Field map could not be "
              << "read and cannot be interpolated.\n";
    return false;
  }

  std::cout << hdr << " Finished.\n";

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

  const std::string hdr = m_className + "::SetWeightingField:"; 
  if (!m_ready) {
    PrintNotReady("SetWeightingField");
    std::cerr << "    Weighting field cannot be added.\n";
    return false;
  }

  // Keep track of the success.
  bool ok = true;

  // Open the voltage list.
  std::ifstream fwvolt;
  fwvolt.open(wvolt.c_str(), std::ios::in);
  if (fwvolt.fail()) {
    PrintErrorOpeningFile(hdr, "potential", wvolt);
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
    std::cout << hdr << "\n    Replacing existing weighting field " 
              << label << ".\n";
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
    std::cerr << hdr << "\n    Error reading past header of potentials file " 
              << wvolt << ".\n";
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
      PrintErrorReadingFile(hdr, wvolt, il);
      fwvolt.close();
      ok = false;
      return false;
    }
    // Place the weighting potential at its appropriate node and index.
    nodes[tl].w[iw] = v;
  }

  // Close the potentials file.
  fwvolt.close();
  std::cout << hdr << "\n    Read potentials from file " << wvolt << ".\n";

  // Set the ready flag.
  wfieldsOk[iw] = ok;
  if (!ok) {
    std::cerr << hdr << "\n    Field map could not "
              << "be read and cannot be interpolated.\n";
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
  bool xmirr, ymirr, zmirr;
  double rcoordinate, rotation;
  MapCoordinates(x, y, z, xmirr, ymirr, zmirr, rcoordinate, rotation);

  // Initial values
  ex = ey = ez = volt = 0.;
  status = 0;
  m = nullptr;

  // Do not proceed if not properly initialised.
  if (!m_ready) {
    status = -10;
    PrintNotReady("ElectricField");
    return;
  }

  if (m_warning) PrintWarning("ElectricField");

  // Find the element that contains this point
  double t1, t2, t3, t4, jac[4][4], det;
  const int imap = FindElement13(x, y, z, t1, t2, t3, t4, jac, det);
  if (imap < 0) {
    if (m_debug) {
      std::cout << m_className << "::ElectricField:\n    Point ("
                << x << ", " << y << ", " << z << ") is not in the mesh.\n";
    }
    status = -6;
    return;
  }

  const Element& element = elements[imap];
  if (m_debug) {
    PrintElement("ElectricField", x, y, z, t1, t2, t3, t4, element, 10);
  }
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
  // Shorthands.
  const double fourt1 = 4 * t1;
  const double fourt2 = 4 * t2;
  const double fourt3 = 4 * t3;
  const double fourt4 = 4 * t4;
  const double invdet = 1. / det;
  // Tetrahedral field
  volt = n0.v * t1 * (2 * t1 - 1) + n1.v * t2 * (2 * t2 - 1) +
         n2.v * t3 * (2 * t3 - 1) + n3.v * t4 * (2 * t4 - 1) +
         4 * n4.v * t1 * t2 + 4 * n5.v * t1 * t3 + 4 * n6.v * t1 * t4 +
         4 * n7.v * t2 * t3 + 4 * n8.v * t2 * t4 + 4 * n9.v * t3 * t4;
  ex = -(n0.v * (fourt1 - 1) * jac[0][1] + n1.v * (fourt2 - 1) * jac[1][1] +
         n2.v * (fourt3 - 1) * jac[2][1] + n3.v * (fourt4 - 1) * jac[3][1] +
         n4.v * (fourt2 * jac[0][1] + fourt1 * jac[1][1]) +
         n5.v * (fourt3 * jac[0][1] + fourt1 * jac[2][1]) +
         n6.v * (fourt4 * jac[0][1] + fourt1 * jac[3][1]) +
         n7.v * (fourt3 * jac[1][1] + fourt2 * jac[2][1]) +
         n8.v * (fourt4 * jac[1][1] + fourt2 * jac[3][1]) +
         n9.v * (fourt4 * jac[2][1] + fourt3 * jac[3][1])) * invdet;
  ey = -(n0.v * (fourt1 - 1) * jac[0][2] + n1.v * (fourt2 - 1) * jac[1][2] +
         n2.v * (fourt3 - 1) * jac[2][2] + n3.v * (fourt4 - 1) * jac[3][2] +
         n4.v * (fourt2 * jac[0][2] + fourt1 * jac[1][2]) +
         n5.v * (fourt3 * jac[0][2] + fourt1 * jac[2][2]) +
         n6.v * (fourt4 * jac[0][2] + fourt1 * jac[3][2]) +
         n7.v * (fourt3 * jac[1][2] + fourt2 * jac[2][2]) +
         n8.v * (fourt4 * jac[1][2] + fourt2 * jac[3][2]) +
         n9.v * (fourt4 * jac[2][2] + fourt3 * jac[3][2])) * invdet;
  ez = -(n0.v * (fourt1 - 1) * jac[0][3] + n1.v * (fourt2 - 1) * jac[1][3] +
         n2.v * (fourt3 - 1) * jac[2][3] + n3.v * (fourt4 - 1) * jac[3][3] +
         n4.v * (fourt2 * jac[0][3] + fourt1 * jac[1][3]) +
         n5.v * (fourt3 * jac[0][3] + fourt1 * jac[2][3]) +
         n6.v * (fourt4 * jac[0][3] + fourt1 * jac[3][3]) +
         n7.v * (fourt3 * jac[1][3] + fourt2 * jac[2][3]) +
         n8.v * (fourt4 * jac[1][3] + fourt2 * jac[3][3]) +
         n9.v * (fourt4 * jac[2][3] + fourt3 * jac[3][3])) * invdet;

  // Transform field to global coordinates
  UnmapFields(ex, ey, ez, x, y, z, xmirr, ymirr, zmirr, rcoordinate, rotation);

  // Drift medium?
  const Material& mat = materials[element.matmap]; 
  if (m_debug) {
    std::cout << m_className << "::ElectricField:\n    Material "
              << element.matmap << ", drift flag " << mat.driftmedium << ".\n";
  }
  m = mat.medium;
  status = -5;
  if (mat.driftmedium && m && m->IsDriftable()) status = 0;
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
  bool xmirr, ymirr, zmirr;
  double rcoordinate, rotation;
  MapCoordinates(x, y, z, xmirr, ymirr, zmirr, rcoordinate, rotation);

  if (m_warning) PrintWarning("WeightingField");

  // Find the element that contains this point.
  double t1, t2, t3, t4, jac[4][4], det;
  const int imap = FindElement13(x, y, z, t1, t2, t3, t4, jac, det);
  // Check if the point is in the mesh.
  if (imap < 0) return;

  const Element& element = elements[imap];
  if (m_debug) {
    PrintElement("WeightingField", x, y, z, t1, t2, t3, t4, element, 10, iw);
  }
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
  // Shorthands.
  const double fourt1 = 4 * t1;
  const double fourt2 = 4 * t2;
  const double fourt3 = 4 * t3;
  const double fourt4 = 4 * t4;
  const double invdet = 1. / det;
  // Tetrahedral field
  wx = -(n0.w[iw] * (fourt1 - 1) * jac[0][1] +
         n1.w[iw] * (fourt2 - 1) * jac[1][1] +
         n2.w[iw] * (fourt3 - 1) * jac[2][1] +
         n3.w[iw] * (fourt4 - 1) * jac[3][1] +
         n4.w[iw] * (fourt2 * jac[0][1] + fourt1 * jac[1][1]) +
         n5.w[iw] * (fourt3 * jac[0][1] + fourt1 * jac[2][1]) +
         n6.w[iw] * (fourt4 * jac[0][1] + fourt1 * jac[3][1]) +
         n7.w[iw] * (fourt3 * jac[1][1] + fourt2 * jac[2][1]) +
         n8.w[iw] * (fourt4 * jac[1][1] + fourt2 * jac[3][1]) +
         n9.w[iw] * (fourt4 * jac[2][1] + fourt3 * jac[3][1])) * invdet;
  wy = -(n0.w[iw] * (fourt1 - 1) * jac[0][2] +
         n1.w[iw] * (fourt2 - 1) * jac[1][2] +
         n2.w[iw] * (fourt3 - 1) * jac[2][2] +
         n3.w[iw] * (fourt4 - 1) * jac[3][2] +
         n4.w[iw] * (fourt2 * jac[0][2] + fourt1 * jac[1][2]) +
         n5.w[iw] * (fourt3 * jac[0][2] + fourt1 * jac[2][2]) +
         n6.w[iw] * (fourt4 * jac[0][2] + fourt1 * jac[3][2]) +
         n7.w[iw] * (fourt3 * jac[1][2] + fourt2 * jac[2][2]) +
         n8.w[iw] * (fourt4 * jac[1][2] + fourt2 * jac[3][2]) +
         n9.w[iw] * (fourt4 * jac[2][2] + fourt3 * jac[3][2])) * invdet;
  wz = -(n0.w[iw] * (fourt1 - 1) * jac[0][3] +
         n1.w[iw] * (fourt2 - 1) * jac[1][3] +
         n2.w[iw] * (fourt3 - 1) * jac[2][3] +
         n3.w[iw] * (fourt4 - 1) * jac[3][3] +
         n4.w[iw] * (fourt2 * jac[0][3] + fourt1 * jac[1][3]) +
         n5.w[iw] * (fourt3 * jac[0][3] + fourt1 * jac[2][3]) +
         n6.w[iw] * (fourt4 * jac[0][3] + fourt1 * jac[3][3]) +
         n7.w[iw] * (fourt3 * jac[1][3] + fourt2 * jac[2][3]) +
         n8.w[iw] * (fourt4 * jac[1][3] + fourt2 * jac[3][3]) +
         n9.w[iw] * (fourt4 * jac[2][3] + fourt3 * jac[3][3])) * invdet;

  // Transform field to global coordinates
  UnmapFields(wx, wy, wz, x, y, z, xmirr, ymirr, zmirr, rcoordinate, rotation);
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
  bool xmirr, ymirr, zmirr;
  double rcoordinate, rotation;
  MapCoordinates(x, y, z, xmirr, ymirr, zmirr, rcoordinate, rotation);

  if (m_warning) PrintWarning("WeightingPotential");

  // Find the element that contains this point.
  double t1, t2, t3, t4, jac[4][4], det;
  const int imap = FindElement13(x, y, z, t1, t2, t3, t4, jac, det);
  if (imap < 0) return 0.;

  const Element& element = elements[imap];
  if (m_debug) {
    PrintElement("WeightingPotential", x, y, z, t1, t2, t3, t4, element, 10, iw);
  }
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
  // Tetrahedral field
  return n0.w[iw] * t1 * (2 * t1 - 1) + n1.w[iw] * t2 * (2 * t2 - 1) +
         n2.w[iw] * t3 * (2 * t3 - 1) + n3.w[iw] * t4 * (2 * t4 - 1) +
         4 * n4.w[iw] * t1 * t2 + 4 * n5.w[iw] * t1 * t3 +
         4 * n6.w[iw] * t1 * t4 + 4 * n7.w[iw] * t2 * t3 +
         4 * n8.w[iw] * t2 * t4 + 4 * n9.w[iw] * t3 * t4;
}

Medium* ComponentElmer::GetMedium(const double xin, const double yin,
                                  const double zin) {

  // Copy the coordinates
  double x = xin, y = yin, z = zin;

  // Map the coordinates onto field map coordinates
  bool xmirr, ymirr, zmirr;
  double rcoordinate, rotation;
  MapCoordinates(x, y, z, xmirr, ymirr, zmirr, rcoordinate, rotation);

  // Do not proceed if not properly initialised.
  if (!m_ready) {
    PrintNotReady("GetMedium");
    return nullptr;
  }
  if (m_warning) PrintWarning("GetMedium");

  // Find the element that contains this point
  double t1, t2, t3, t4, jac[4][4], det;
  const int imap = FindElement13(x, y, z, t1, t2, t3, t4, jac, det);
  if (imap < 0) {
    if (m_debug) {
      std::cout << m_className << "::GetMedium:\n    Point ("
                << x << ", " << y << ", " << z << ") is not in the mesh.\n";
    }
    return nullptr;
  }
  const Element& element = elements[imap];
  if (element.matmap >= m_nMaterials) {
    if (m_debug) {
      std::cerr << m_className << "::GetMedium:\n    Point (" 
                << x << ", " << y << ", " << z 
                << ") has out of range material number " << imap << ".\n";
    }
    return nullptr;
  }

  if (m_debug) PrintElement("GetMedium", x, y, z, t1, t2, t3, t4, element, 10);

  return materials[element.matmap].medium;
}

double ComponentElmer::GetElementVolume(const unsigned int i) {

  if (i >= elements.size()) return 0.;
  const Element& element = elements[i];
  const Node& n0 = nodes[element.emap[0]];
  const Node& n1 = nodes[element.emap[1]];
  const Node& n2 = nodes[element.emap[2]];
  const Node& n3 = nodes[element.emap[3]];

  // Uses formula V = |a (dot) b x c|/6
  // with a => "3", b => "1", c => "2" and origin "0"
  const double vol =
      fabs((n3.x - n0.x) *
               ((n1.y - n0.y) * (n2.z - n0.z) - (n2.y - n0.y) * (n1.z - n0.z)) +
           (n3.y - n0.y) *
               ((n1.z - n0.z) * (n2.x - n0.x) - (n2.z - n0.z) * (n1.x - n0.x)) +
           (n3.z - n0.z) * ((n1.x - n0.x) * (n2.y - n0.y) -
                            (n3.x - n0.x) * (n1.y - n0.y))) /
      6.;
  return vol;
}

void ComponentElmer::GetAspectRatio(const unsigned int i, double& dmin,
                                    double& dmax) {

  if (i >= elements.size()) {
    dmin = dmax = 0.;
    return;
  }

  const Element& element = elements[i];
  const int np = 4;
  // Loop over all pairs of vertices.
  for (int j = 0; j < np - 1; ++j) {
    const Node& nj = nodes[element.emap[j]];
    for (int k = j + 1; k < np; ++k) {
      const Node& nk = nodes[element.emap[k]];
      // Compute distance.
      const double dx = nj.x - nk.x;
      const double dy = nj.y - nk.y;
      const double dz = nj.z - nk.z;
      const double dist = sqrt(dx * dx + dy * dy + dz * dz);
      if (k == 1) {
        dmin = dmax = dist;
      } else {
        if (dist < dmin) dmin = dist;
        if (dist > dmax) dmax = dist;
      }
    }
  }
}

}  // namespace Garfield
