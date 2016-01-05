#include <stdio.h>
#include <string.h>
#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <math.h>

#include "ComponentAnsys121.hh"

namespace Garfield {

ComponentAnsys121::ComponentAnsys121() : ComponentFieldMap() {

  m_className = "ComponentAnsys121";
  m_ready = false;
  // Default bounding box
  is3d = false;
  zMinBoundingBox = -50;
  zMaxBoundingBox = 50;
}

bool ComponentAnsys121::Initialise(std::string elist, std::string nlist,
                                   std::string mplist, std::string prnsol,
                                   std::string unit) {

  m_ready = false;
  // Keep track of the success.
  bool ok = true;

  // Buffer for reading
  const int size = 100;
  char line[size];

  // Open the material list.
  std::ifstream fmplist;
  fmplist.open(mplist.c_str(), std::ios::in);
  if (fmplist.fail()) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Could not open material file " << mplist
              << " for reading.\n",
        std::cerr << "    The file perhaps does not exist.\n";
    return false;
  }

  // Read the material list.
  m_nMaterials = 0;
  int il = 0;
  unsigned int icurrmat = 0;
  bool readerror = false;
  while (fmplist.getline(line, size, '\n')) {
    il++;
    // Skip page feed
    if (strcmp(line, "1") == 0) {
      fmplist.getline(line, size, '\n');
      il++;
      fmplist.getline(line, size, '\n');
      il++;
      fmplist.getline(line, size, '\n');
      il++;
      fmplist.getline(line, size, '\n');
      il++;
      fmplist.getline(line, size, '\n');
      il++;
      continue;
    }
    // Split the line in tokens
    char* token = NULL;
    token = strtok(line, " ");
    // Skip blank lines and headers
    if (!token || strcmp(token, " ") == 0 || strcmp(token, "\n") == 0 ||
        strcmp(token, "TEMPERATURE") == 0 || strcmp(token, "PROPERTY=") == 0 ||
        int(token[0]) == 10 || int(token[0]) == 13)
      continue;
    // Read number of materials,
    // ensure it does not exceed the maximum and initialise the list
    if (strcmp(token, "LIST") == 0) {
      token = strtok(NULL, " ");
      token = strtok(NULL, " ");
      token = strtok(NULL, " ");
      token = strtok(NULL, " ");
      m_nMaterials = ReadInteger(token, -1, readerror);
      if (readerror) {
        std::cerr << m_className << "::Initialise:\n";
        std::cerr << "    Error reading file " << mplist << " (line " << il
                  << ").\n";
        fmplist.close();
        ok = false;
        return false;
      }
      materials.resize(m_nMaterials);
      for (unsigned int i = 0; i < m_nMaterials; ++i) {
        materials[i].ohm = -1;
        materials[i].eps = -1;
        materials[i].medium = NULL;
      }
      if (m_debug) {
        std::cout << m_className << "::Initialise:\n";
        std::cout << "    Number of materials: " << m_nMaterials << "\n";
      }
    } else if (strcmp(token, "MATERIAL") == 0) {
      // Version 12 format: read material number
      token = strtok(NULL, " ");
      token = strtok(NULL, " ");
      const int imat = ReadInteger(token, -1, readerror);
      if (readerror || imat < 0) {
        std::cerr << m_className << "::Initialise:\n";
        std::cerr << "    Error reading file " << mplist << " (line " << il
                  << ").\n";
        fmplist.close();
        ok = false;
        return false;
      }
      icurrmat = imat;
    } else if (strcmp(token, "TEMP") == 0) {
      // Version 12 format: read property tag and value
      token = strtok(NULL, " ");
      int itype = 0;
      if (strncmp(token, "PERX", 4) == 0) {
        itype = 1;
      } else if (strncmp(token, "RSVX", 4) == 0) {
        itype = 2;
      } else {
        std::cerr << m_className << "::Initialise:\n";
        std::cerr << "    Found unknown material property flag " << token
                  << "\n";
        std::cerr << "    on material properties file " << mplist << " (line "
                  << il << ").\n";
        ok = false;
      }
      fmplist.getline(line, size, '\n');
      il++;
      token = NULL;
      token = strtok(line, " ");
      if (icurrmat < 1 || icurrmat > m_nMaterials) {
        std::cerr << m_className << "::Initialise:\n";
        std::cerr << "    Found out-of-range current material index "
                  << icurrmat << "\n";
        std::cerr << "    in material properties file " << mplist << ".\n";
        ok = false;
        readerror = false;
      } else if (itype == 1) {
        materials[icurrmat - 1].eps = ReadDouble(token, -1, readerror);
      } else if (itype == 2) {
        materials[icurrmat - 1].ohm = ReadDouble(token, -1, readerror);
      }
      if (readerror) {
        std::cerr << m_className << "::Initialise:\n";
        std::cerr << "    Error reading file " << mplist << " (line " << il
                  << ").\n";
        fmplist.close();
        ok = false;
        return false;
      }
    } else if (strcmp(token, "PROPERTY") == 0) {
      // Version 11 format
      token = strtok(NULL, " ");
      token = strtok(NULL, " ");
      int itype = 0;
      if (strcmp(token, "PERX") == 0) {
        itype = 1;
      } else if (strcmp(token, "RSVX") == 0) {
        itype = 2;
      } else {
        std::cerr << m_className << "::Initialise:\n";
        std::cerr << m_className << "::Initialise:\n";
        std::cerr << "    Found unknown material property flag " << token
                  << "\n";
        std::cerr << "    on material properties file " << mplist << " (line "
                  << il << ").\n";
        ok = false;
      }
      token = strtok(NULL, " ");
      token = strtok(NULL, " ");
      int imat = ReadInteger(token, -1, readerror);
      if (readerror) {
        std::cerr << m_className << "::Initialise:\n";
        std::cerr << "    Error reading file " << mplist << " (line " << il
                  << ").\n";
        fmplist.close();
        ok = false;
        return false;
      } else if (imat < 1 || imat > (int)m_nMaterials) {
        std::cerr << m_className << "::Initialise:\n";
        std::cerr << "    Found out-of-range current material index " << imat
                  << "\n";
        std::cerr << "    in material properties file " << mplist << ".\n";
        ok = false;
      } else {
        fmplist.getline(line, size, '\n');
        il++;
        fmplist.getline(line, size, '\n');
        il++;
        token = NULL;
        token = strtok(line, " ");
        token = strtok(NULL, " ");
        if (itype == 1) {
          materials[imat - 1].eps = ReadDouble(token, -1, readerror);
        } else if (itype == 2) {
          materials[imat - 1].ohm = ReadDouble(token, -1, readerror);
        }
        if (readerror) {
          std::cerr << m_className << "::Initialise:\n";
          std::cerr << "    Error reading file " << mplist << " (line " << il
                    << ").\n";
          fmplist.close();
          ok = false;
          return false;
        }
      }
    }
  }

  // Close the file
  fmplist.close();

  // Find the lowest epsilon, check for eps = 0, set default drift media
  double epsmin = -1;
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

  // Tell how many lines read
  std::cout << m_className << "::Initialise:\n";
  std::cout << "    Read properties of " << m_nMaterials
            << " materials from file " << mplist << ".\n";
  if (m_debug) PrintMaterials();

  // Open the element list
  std::ifstream felist;
  felist.open(elist.c_str(), std::ios::in);
  if (felist.fail()) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Could not open element file " << elist
              << " for reading.\n";
    std::cerr << "    The file perhaps does not exist.\n";
    return false;
  }
  // Read the element list
  elements.clear();
  nElements = 0;
  element newElement;
  int ndegenerate = 0;
  int nbackground = 0;
  il = 0;
  int highestnode = 0;
  while (felist.getline(line, size, '\n')) {
    il++;
    // Split the line in tokens
    char* token = NULL;
    // Split into tokens
    token = strtok(line, " ");
    // Skip blank lines and headers
    if (!token || strcmp(token, " ") == 0 || strcmp(token, "\n") == 0 ||
        int(token[0]) == 10 || int(token[0]) == 13 ||
        strcmp(token, "LIST") == 0 || strcmp(token, "ELEM") == 0)
      continue;
    // Read the element
    int ielem = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    int imat = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    token = strtok(NULL, " ");
    token = strtok(NULL, " ");
    token = strtok(NULL, " ");
    token = strtok(NULL, " ");
    if (!token) std::cerr << "No token available\n";
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

    // Check synchronisation
    if (readerror) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Error reading file " << elist << " (line " << il
                << ").\n";
      felist.close();
      ok = false;
      return false;
    } else if (ielem - 1 != nElements + nbackground) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Synchronisation lost on file " << elist << " (line "
                << il << ").\n";
      std::cerr << "    Element: " << ielem << " (expected " << nElements
                << "), material: " << imat << ", nodes: (" << in0 << ", " << in1
                << ", " << in2 << ", " << in3 << ", " << in4 << ", " << in5
                << ", " << in6 << ", " << in7 << ")\n";
      ok = false;
    }
    // Check the material number and ensure that epsilon is non-negative
    if (imat < 1 || imat > (int)m_nMaterials) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "   Out-of-range material number on file " << elist
                << " (line " << il << ").\n";
      std::cerr << "    Element: " << ielem << ", material: " << imat
                << ", nodes: (" << in0 << ", " << in1 << ", " << in2 << ", "
                << in3 << ", " << in4 << ", " << in5 << ", " << in6 << ", "
                << in7 << ")\n";
      ok = false;
    }
    if (materials[imat - 1].eps < 0) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Element " << ielem << " in element list " << elist
                << " uses material " << imat << " which\n",
          std::cerr << "    has not been assigned a positive permittivity\n";
      std::cerr << "    in material list " << mplist << ".\n";
      ok = false;
    }
    // Check the node numbers
    if (in0 < 1 || in1 < 1 || in2 < 1 || in3 < 1 || in4 < 1 || in5 < 1 ||
        in6 < 1 || in7 < 1) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Found a node number < 1 on file " << elist << " (line "
                << il << ").\n";
      std::cerr << "    Element: " << ielem << ", material: " << imat << "\n";
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
    // Skip quadrilaterals which are background.
    if (deleteBackground && materials[imat - 1].ohm == 0) {
      nbackground++;
      continue;
    }
    // Store the element, degeneracy
    if (in2 == in3 && in3 == in6) {
      ndegenerate++;
      newElement.degenerate = true;
    } else {
      newElement.degenerate = false;
    }
    // Store the material reference
    newElement.matmap = imat - 1;
    // Node references
    if (newElement.degenerate) {
      newElement.emap[0] = in0 - 1;
      newElement.emap[1] = in1 - 1;
      newElement.emap[2] = in2 - 1;
      newElement.emap[3] = in4 - 1;
      newElement.emap[4] = in7 - 1;
      newElement.emap[5] = in5 - 1;
      newElement.emap[6] = in3 - 1;
      newElement.emap[7] = in6 - 1;
    } else {
      newElement.emap[0] = in0 - 1;
      newElement.emap[1] = in1 - 1;
      newElement.emap[2] = in2 - 1;
      newElement.emap[3] = in3 - 1;
      newElement.emap[4] = in4 - 1;
      newElement.emap[5] = in5 - 1;
      newElement.emap[6] = in6 - 1;
      newElement.emap[7] = in7 - 1;
    }
    elements.push_back(newElement);
    ++nElements;
  }
  // Close the file
  felist.close();
  // Tell how many lines read
  std::cout << m_className << "::Initialise:\n";
  std::cout << "    Read " << nElements << " elements from file " << elist
            << ",\n";
  std::cout << "    highest node number: " << highestnode << ",\n";
  std::cout << "    degenerate elements: " << ndegenerate << ",\n";
  std::cout << "    background elements skipped: " << nbackground << ".\n";
  // Check the value of the unit
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
    std::cout << m_className << "::Initialise:\n";
    std::cout << "    Unit scaling factor = " << funit << ".\n";
  }

  // Open the node list
  std::ifstream fnlist;
  fnlist.open(nlist.c_str(), std::ios::in);
  if (fnlist.fail()) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Could not open nodes file " << nlist << " for reading.\n";
    std::cerr << "    The file perhaps does not exist.\n";
    return false;
  }
  // Read the node list
  nodes.clear();
  nNodes = 0;
  node newNode;
  newNode.w.clear();
  il = 0;
  while (fnlist.getline(line, size, '\n')) {
    il++;
    // Split the line in tokens
    char* token = NULL;
    // Split into tokens
    token = strtok(line, " ");
    // Skip blank lines and headers
    if (!token || strcmp(token, " ") == 0 || strcmp(token, "\n") == 0 ||
        int(token[0]) == 10 || int(token[0]) == 13 ||
        strcmp(token, "LIST") == 0 || strcmp(token, "NODE") == 0)
      continue;
    // Read the element
    int inode = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    double xnode = ReadDouble(token, -1, readerror);
    token = strtok(NULL, " ");
    double ynode = ReadDouble(token, -1, readerror);
    token = strtok(NULL, " ");
    double znode = ReadDouble(token, -1, readerror);
    // Check syntax
    if (readerror) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Error reading file " << nlist << " (line " << il
                << ").\n";
      fnlist.close();
      ok = false;
      return false;
    }
    // Check synchronisation
    if (inode - 1 != nNodes) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Synchronisation lost on file " << nlist << " (line "
                << il << ").\n";
      std::cerr << "    Node: " << inode << " (expected " << nNodes
                << "), (x,y,z) = (" << xnode << ", " << ynode << ", " << znode
                << ")\n";
      ok = false;
    }

    // Store the point coordinates
    newNode.x = xnode * funit;
    newNode.y = ynode * funit;
    newNode.z = znode * funit;
    nodes.push_back(newNode);
    ++nNodes;
  }
  // Close the file
  fnlist.close();
  // Tell how many lines read
  std::cout << m_className << "::Initialise:\n";
  std::cout << "    Read " << nNodes << " nodes from file " << nlist << ".\n";
  // Check number of nodes
  if (nNodes != highestnode) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Number of nodes read (" << nNodes << ") on " << nlist
              << "\n";
    std::cerr << "    does not match element list (" << highestnode << ").\n";
    ok = false;
  }

  // Open the voltage list
  std::ifstream fprnsol;
  fprnsol.open(prnsol.c_str(), std::ios::in);
  if (fprnsol.fail()) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Could not open potential file " << prnsol
              << " for reading.\n";
    std::cerr << "    The file perhaps does not exist.\n";
    return false;
  }
  // Read the voltage list
  il = 0;
  int nread = 0;
  readerror = false;
  while (fprnsol.getline(line, size, '\n')) {
    il++;
    // Split the line in tokens
    char* token = NULL;
    token = strtok(line, " ");
    // Skip blank lines and headers
    if (!token || strcmp(token, " ") == 0 || strcmp(token, "\n") == 0 ||
        int(token[0]) == 10 || int(token[0]) == 13 ||
        strcmp(token, "PRINT") == 0 || strcmp(token, "*****") == 0 ||
        strcmp(token, "LOAD") == 0 || strcmp(token, "TIME=") == 0 ||
        strcmp(token, "MAXIMUM") == 0 || strcmp(token, "VALUE") == 0 ||
        strcmp(token, "NODE") == 0)
      continue;
    // Read the element
    int inode = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    double volt = ReadDouble(token, -1, readerror);
    // Check syntax
    if (readerror) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Error reading file " << prnsol << " (line " << il
                << ").\n";
      fprnsol.close();
      ok = false;
      return false;
    }
    // Check node number and store if OK
    if (inode < 1 || inode > nNodes) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Node number " << inode << " out of range\n";
      std::cerr << "    on potential file " << prnsol << " (line " << il
                << ").\n";
      ok = false;
    } else {
      nodes[inode - 1].v = volt;
      nread++;
    }
  }
  // Close the file
  fprnsol.close();
  // Tell how many lines read
  std::cout << m_className << "::Initialise:\n";
  std::cout << "    Read " << nread << " potentials from file " << prnsol
            << ".\n";
  // Check number of nodes
  if (nread != nNodes) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Number of nodes read (" << nread << ") on potential file "
              << prnsol << " does not\n",
        std::cerr << "    match the node list (" << nNodes << ").\n";
    ok = false;
  }
  // Set the ready flag
  if (ok) {
    m_ready = true;
  } else {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr
        << "    Field map could not be read and cannot be interpolated.\n";
    return false;
  }

  // Remove weighting fields (if any).
  wfields.clear();
  wfieldsOk.clear();
  nWeightingFields = 0;

  // Establish the ranges
  SetRange();
  UpdatePeriodicity();
  return true;
}

bool ComponentAnsys121::SetWeightingField(std::string prnsol,
                                          std::string label) {

  if (!m_ready) {
    std::cerr << m_className << "::SetWeightingField:\n";
    std::cerr << "    No valid field map is present.\n";
    std::cerr << "    Weighting field cannot be added.\n";
    return false;
  }

  // Open the voltage list.
  std::ifstream fprnsol;
  fprnsol.open(prnsol.c_str(), std::ios::in);
  if (fprnsol.fail()) {
    std::cerr << m_className << "::SetWeightingField:\n";
    std::cerr << "    Could not open potential file " << prnsol
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

  // Buffer for reading
  const int size = 100;
  char line[size];

  bool ok = true;
  // Read the voltage list.
  int il = 0;
  int nread = 0;
  bool readerror = false;
  while (fprnsol.getline(line, size, '\n')) {
    il++;
    // Split the line in tokens.
    char* token = NULL;
    token = strtok(line, " ");
    // Skip blank lines and headers.
    if (!token || strcmp(token, " ") == 0 || strcmp(token, "\n") == 0 ||
        int(token[0]) == 10 || int(token[0]) == 13 ||
        strcmp(token, "PRINT") == 0 || strcmp(token, "*****") == 0 ||
        strcmp(token, "LOAD") == 0 || strcmp(token, "TIME=") == 0 ||
        strcmp(token, "MAXIMUM") == 0 || strcmp(token, "VALUE") == 0 ||
        strcmp(token, "NODE") == 0)
      continue;
    // Read the element.
    int inode = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    double volt = ReadDouble(token, -1, readerror);
    // Check the syntax.
    if (readerror) {
      std::cerr << m_className << "::SetWeightingField:\n";
      std::cerr << "    Error reading file " << prnsol << " (line " << il
                << ").\n";
      fprnsol.close();
      return false;
    }
    // Check node number and store if OK.
    if (inode < 1 || inode > nNodes) {
      std::cerr << m_className << "::SetWeightingField:\n";
      std::cerr << "    Node number " << inode << " out of range\n";
      std::cerr << "    on potential file " << prnsol << " (line " << il
                << ").\n";
      ok = false;
    } else {
      nodes[inode - 1].w[iw] = volt;
      nread++;
    }
  }
  // Close the file.
  fprnsol.close();
  // Tell how many lines read.
  std::cout << m_className << "::SetWeightingField:\n";
  std::cout << "    Read " << nread << " potentials from file " << prnsol
            << ".\n";
  // Check the number of nodes.
  if (nread != nNodes) {
    std::cerr << m_className << "::SetWeightingField:\n";
    std::cerr << "    Number of nodes read (" << nread << ") "
              << "    on potential file " << prnsol << "\n";
    std::cerr << "    does not match the node list (" << nNodes << ").\n";
    ok = false;
  }

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

void ComponentAnsys121::ElectricField(const double x, const double y,
                                      const double z, double& ex, double& ey,
                                      double& ez, Medium*& m, int& status) {

  double v;
  ElectricField(x, y, z, ex, ey, ez, v, m, status);
}

void ComponentAnsys121::ElectricField(const double xin, const double yin,
                                      const double zin, double& ex, double& ey,
                                      double& ez, double& volt, Medium*& m,
                                      int& status) {

  // Copy the coordinates
  double x = xin, y = yin, z = 0.;

  // Map the coordinates onto field map coordinates
  bool xmirrored, ymirrored, zmirrored;
  double rcoordinate, rotation;
  MapCoordinates(x, y, z, xmirrored, ymirrored, zmirrored, rcoordinate,
                 rotation);

  // Initial values
  ex = ey = ez = volt = 0;
  m = 0;
  status = 0;

  // Do not proceed if not properly initialised.
  if (!m_ready) {
    status = -10;
    std::cerr << m_className << "::ElectricField:\n";
    std::cerr << "     Field map not available for interpolation.\n";
    return;
  }

  if (warning) {
    std::cerr << m_className << "::ElectricField:\n";
    std::cerr << "    Warnings have been issued for this field map.\n";
  }

  if (zin < zMinBoundingBox || zin > zMaxBoundingBox) {
    status = -5;
    return;
  }

  // Find the element that contains this point
  double t1, t2, t3, t4, jac[4][4], det;
  int imap = FindElement5(x, y, z, t1, t2, t3, t4, jac, det);
  if (imap < 0) {
    if (m_debug) {
      std::cout << m_className << "::ElectricField:\n";
      std::cout << "    Point (" << x << ", " << y << ") not in the mesh.\n";
    }
    status = -6;
    return;
  }

  if (m_debug) {
    std::cout << m_className << "::ElectricField:\n";
    std::cout << "    Global: (" << x << ", " << y << "),\n";
    std::cout << "    Local: (" << t1 << ", " << t2 << ", " << t3 << ", " << t4
              << ") in element " << imap
              << " (degenerate: " << elements[imap].degenerate << ")\n";
    std::cout
        << "                  Node             x            y            V\n";
    for (int i = 0; i < 8; i++) {
      printf("                  %-5d %12g %12g %12g\n", elements[imap].emap[i],
             nodes[elements[imap].emap[i]].x, nodes[elements[imap].emap[i]].y,
             nodes[elements[imap].emap[i]].v);
    }
  }

  // Calculate quadrilateral field, which can degenerate to a triangular field
  if (elements[imap].degenerate) {
    volt = nodes[elements[imap].emap[0]].v * t1 * (2 * t1 - 1) +
           nodes[elements[imap].emap[1]].v * t2 * (2 * t2 - 1) +
           nodes[elements[imap].emap[2]].v * t3 * (2 * t3 - 1) +
           4 * nodes[elements[imap].emap[3]].v * t1 * t2 +
           4 * nodes[elements[imap].emap[4]].v * t1 * t3 +
           4 * nodes[elements[imap].emap[5]].v * t2 * t3;
    ex = -(nodes[elements[imap].emap[0]].v * (4 * t1 - 1) * jac[0][1] +
           nodes[elements[imap].emap[1]].v * (4 * t2 - 1) * jac[1][1] +
           nodes[elements[imap].emap[2]].v * (4 * t3 - 1) * jac[2][1] +
           nodes[elements[imap].emap[3]].v *
               (4 * t2 * jac[0][1] + 4 * t1 * jac[1][1]) +
           nodes[elements[imap].emap[4]].v *
               (4 * t3 * jac[0][1] + 4 * t1 * jac[2][1]) +
           nodes[elements[imap].emap[5]].v *
               (4 * t3 * jac[1][1] + 4 * t2 * jac[2][1])) /
         det;
    ey = -(nodes[elements[imap].emap[0]].v * (4 * t1 - 1) * jac[0][2] +
           nodes[elements[imap].emap[1]].v * (4 * t2 - 1) * jac[1][2] +
           nodes[elements[imap].emap[2]].v * (4 * t3 - 1) * jac[2][2] +
           nodes[elements[imap].emap[3]].v *
               (4 * t2 * jac[0][2] + 4 * t1 * jac[1][2]) +
           nodes[elements[imap].emap[4]].v *
               (4 * t3 * jac[0][2] + 4 * t1 * jac[2][2]) +
           nodes[elements[imap].emap[5]].v *
               (4 * t3 * jac[1][2] + 4 * t2 * jac[2][2])) /
         det;
  } else {
    volt =
        -nodes[elements[imap].emap[0]].v * (1 - t1) * (1 - t2) * (1 + t1 + t2) /
            4 -
        nodes[elements[imap].emap[1]].v * (1 + t1) * (1 - t2) * (1 - t1 + t2) /
            4 -
        nodes[elements[imap].emap[2]].v * (1 + t1) * (1 + t2) * (1 - t1 - t2) /
            4 -
        nodes[elements[imap].emap[3]].v * (1 - t1) * (1 + t2) * (1 + t1 - t2) /
            4 +
        nodes[elements[imap].emap[4]].v * (1 - t1) * (1 + t1) * (1 - t2) / 2 +
        nodes[elements[imap].emap[5]].v * (1 + t1) * (1 + t2) * (1 - t2) / 2 +
        nodes[elements[imap].emap[6]].v * (1 - t1) * (1 + t1) * (1 + t2) / 2 +
        nodes[elements[imap].emap[7]].v * (1 - t1) * (1 + t2) * (1 - t2) / 2;
    ex = -(nodes[elements[imap].emap[0]].v *
               ((1 - t2) * (2 * t1 + t2) * jac[0][0] +
                (1 - t1) * (t1 + 2 * t2) * jac[1][0]) /
               4 +
           nodes[elements[imap].emap[1]].v *
               ((1 - t2) * (2 * t1 - t2) * jac[0][0] -
                (1 + t1) * (t1 - 2 * t2) * jac[1][0]) /
               4 +
           nodes[elements[imap].emap[2]].v *
               ((1 + t2) * (2 * t1 + t2) * jac[0][0] +
                (1 + t1) * (t1 + 2 * t2) * jac[1][0]) /
               4 +
           nodes[elements[imap].emap[3]].v *
               ((1 + t2) * (2 * t1 - t2) * jac[0][0] -
                (1 - t1) * (t1 - 2 * t2) * jac[1][0]) /
               4 +
           nodes[elements[imap].emap[4]].v *
               (t1 * (t2 - 1) * jac[0][0] +
                (t1 - 1) * (t1 + 1) * jac[1][0] / 2) +
           nodes[elements[imap].emap[5]].v *
               ((1 - t2) * (1 + t2) * jac[0][0] / 2 -
                (1 + t1) * t2 * jac[1][0]) +
           nodes[elements[imap].emap[6]].v *
               (-t1 * (1 + t2) * jac[0][0] +
                (1 - t1) * (1 + t1) * jac[1][0] / 2) +
           nodes[elements[imap].emap[7]].v *
               ((t2 - 1) * (t2 + 1) * jac[0][0] / 2 +
                (t1 - 1) * t2 * jac[1][0])) /
         det;
    ey = -(nodes[elements[imap].emap[0]].v *
               ((1 - t2) * (2 * t1 + t2) * jac[0][1] +
                (1 - t1) * (t1 + 2 * t2) * jac[1][1]) /
               4 +
           nodes[elements[imap].emap[1]].v *
               ((1 - t2) * (2 * t1 - t2) * jac[0][1] -
                (1 + t1) * (t1 - 2 * t2) * jac[1][1]) /
               4 +
           nodes[elements[imap].emap[2]].v *
               ((1 + t2) * (2 * t1 + t2) * jac[0][1] +
                (1 + t1) * (t1 + 2 * t2) * jac[1][1]) /
               4 +
           nodes[elements[imap].emap[3]].v *
               ((1 + t2) * (2 * t1 - t2) * jac[0][1] -
                (1 - t1) * (t1 - 2 * t2) * jac[1][1]) /
               4 +
           nodes[elements[imap].emap[4]].v *
               (t1 * (t2 - 1) * jac[0][1] +
                (t1 - 1) * (t1 + 1) * jac[1][1] / 2) +
           nodes[elements[imap].emap[5]].v *
               ((1 - t2) * (1 + t2) * jac[0][1] / 2 -
                (1 + t1) * t2 * jac[1][1]) +
           nodes[elements[imap].emap[6]].v *
               (-t1 * (1 + t2) * jac[0][1] +
                (1 - t1) * (1 + t1) * jac[1][1] / 2) +
           nodes[elements[imap].emap[7]].v *
               ((t2 - 1) * (t2 + 1) * jac[0][1] / 2 +
                (t1 - 1) * t2 * jac[1][1])) /
         det;
  }

  // Transform field to global coordinates
  UnmapFields(ex, ey, ez, x, y, z, xmirrored, ymirrored, zmirrored, rcoordinate,
              rotation);

  // Drift medium?
  if (m_debug) {
    std::cout << m_className << "::ElectricField:\n";
    std::cout << "    Material " << elements[imap].matmap << ", drift flag "
              << materials[elements[imap].matmap].driftmedium << ".\n";
  }
  m = materials[elements[imap].matmap].medium;
  status = -5;
  if (materials[elements[imap].matmap].driftmedium) {
    if (m != 0) {
      if (m->IsDriftable()) status = 0;
    }
  }
}

void ComponentAnsys121::WeightingField(const double xin, const double yin,
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
  int imap = FindElement5(x, y, z, t1, t2, t3, t4, jac, det);
  // Check if the point is in the mesh.
  if (imap < 0) return;

  if (m_debug) {
    std::cout << m_className << "::WeightingField:\n";
    std::cout << "    Global: (" << x << ", " << y << "),\n";
    std::cout << "    Local: (" << t1 << ", " << t2 << ", " << t3 << ", " << t4
              << ") in element " << imap
              << " (degenerate: " << elements[imap].degenerate << ")\n";
    std::cout
        << "                  Node             x            y            V\n";
    for (int i = 0; i < 8; i++) {
      printf("                  %-5d %12g %12g %12g\n", elements[imap].emap[i],
             nodes[elements[imap].emap[i]].x, nodes[elements[imap].emap[i]].y,
             nodes[elements[imap].emap[i]].w[iw]);
    }
  }

  // Calculate quadrilateral field, which can degenerate to a triangular field
  if (elements[imap].degenerate) {
    wx = -(nodes[elements[imap].emap[0]].w[iw] * (4 * t1 - 1) * jac[0][1] +
           nodes[elements[imap].emap[1]].w[iw] * (4 * t2 - 1) * jac[1][1] +
           nodes[elements[imap].emap[2]].w[iw] * (4 * t3 - 1) * jac[2][1] +
           nodes[elements[imap].emap[3]].w[iw] *
               (4 * t2 * jac[0][1] + 4 * t1 * jac[1][1]) +
           nodes[elements[imap].emap[4]].w[iw] *
               (4 * t3 * jac[0][1] + 4 * t1 * jac[2][1]) +
           nodes[elements[imap].emap[5]].w[iw] *
               (4 * t3 * jac[1][1] + 4 * t2 * jac[2][1])) /
         det;
    wy = -(nodes[elements[imap].emap[0]].w[iw] * (4 * t1 - 1) * jac[0][2] +
           nodes[elements[imap].emap[1]].w[iw] * (4 * t2 - 1) * jac[1][2] +
           nodes[elements[imap].emap[2]].w[iw] * (4 * t3 - 1) * jac[2][2] +
           nodes[elements[imap].emap[3]].w[iw] *
               (4 * t2 * jac[0][2] + 4 * t1 * jac[1][2]) +
           nodes[elements[imap].emap[4]].w[iw] *
               (4 * t3 * jac[0][2] + 4 * t1 * jac[2][2]) +
           nodes[elements[imap].emap[5]].w[iw] *
               (4 * t3 * jac[1][2] + 4 * t2 * jac[2][2])) /
         det;
  } else {
    wx = -(nodes[elements[imap].emap[0]].w[iw] *
               ((1 - t2) * (2 * t1 + t2) * jac[0][0] +
                (1 - t1) * (t1 + 2 * t2) * jac[1][0]) /
               4 +
           nodes[elements[imap].emap[1]].w[iw] *
               ((1 - t2) * (2 * t1 - t2) * jac[0][0] -
                (1 + t1) * (t1 - 2 * t2) * jac[1][0]) /
               4 +
           nodes[elements[imap].emap[2]].w[iw] *
               ((1 + t2) * (2 * t1 + t2) * jac[0][0] +
                (1 + t1) * (t1 + 2 * t2) * jac[1][0]) /
               4 +
           nodes[elements[imap].emap[3]].w[iw] *
               ((1 + t2) * (2 * t1 - t2) * jac[0][0] -
                (1 - t1) * (t1 - 2 * t2) * jac[1][0]) /
               4 +
           nodes[elements[imap].emap[4]].w[iw] *
               (t1 * (t2 - 1) * jac[0][0] +
                (t1 - 1) * (t1 + 1) * jac[1][0] / 2) +
           nodes[elements[imap].emap[5]].w[iw] *
               ((1 - t2) * (1 + t2) * jac[0][0] / 2 -
                (1 + t1) * t2 * jac[1][0]) +
           nodes[elements[imap].emap[6]].w[iw] *
               (-t1 * (1 + t2) * jac[0][0] +
                (1 - t1) * (1 + t1) * jac[1][0] / 2) +
           nodes[elements[imap].emap[7]].w[iw] *
               ((t2 - 1) * (1 + t2) * jac[0][0] / 2 +
                (t1 - 1) * t2 * jac[1][0])) /
         det;
    wy = -(nodes[elements[imap].emap[0]].w[iw] *
               ((1 - t2) * (2 * t1 + t2) * jac[0][1] +
                (1 - t1) * (t1 + 2 * t2) * jac[1][1]) /
               4 +
           nodes[elements[imap].emap[1]].w[iw] *
               ((1 - t2) * (2 * t1 - t2) * jac[0][1] -
                (1 + t1) * (t1 - 2 * t2) * jac[1][1]) /
               4 +
           nodes[elements[imap].emap[2]].w[iw] *
               ((1 + t2) * (2 * t1 + t2) * jac[0][1] +
                (1 + t1) * (t1 + 2 * t2) * jac[1][1]) /
               4 +
           nodes[elements[imap].emap[3]].w[iw] *
               ((1 + t2) * (2 * t1 - t2) * jac[0][1] -
                (1 - t1) * (t1 - 2 * t2) * jac[1][1]) /
               4 +
           nodes[elements[imap].emap[4]].w[iw] *
               (t1 * (t2 - 1) * jac[0][1] +
                (t1 - 1) * (t1 + 1) * jac[1][1] / 2) +
           nodes[elements[imap].emap[5]].w[iw] *
               ((1 - t2) * (1 + t2) * jac[0][1] / 2 -
                (1 + t1) * t2 * jac[1][1]) +
           nodes[elements[imap].emap[6]].w[iw] *
               (-t1 * (1 + t2) * jac[0][1] +
                (1 - t1) * (1 + t1) * jac[1][1] / 2) +
           nodes[elements[imap].emap[7]].w[iw] *
               ((t2 - 1) * (t2 + 1) * jac[0][1] / 2 +
                (t1 - 1) * t2 * jac[1][1])) /
         det;
  }

  // Transform field to global coordinates
  UnmapFields(wx, wy, wz, x, y, z, xmirrored, ymirrored, zmirrored, rcoordinate,
              rotation);
}

double ComponentAnsys121::WeightingPotential(const double xin, const double yin,
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
  int imap = FindElement5(x, y, z, t1, t2, t3, t4, jac, det);
  // Check if the point is in the mesh
  if (imap < 0) return 0.;

  if (m_debug) {
    std::cerr << m_className << "::WeightingPotential:\n";
    std::cout << "    Global: (" << x << ", " << y << "),\n";
    std::cout << "    Local: (" << t1 << ", " << t2 << ", " << t3 << ", " << t4
              << ") in element " << imap
              << " (degenerate: " << elements[imap].degenerate << ")\n";
    std::cout
        << "                  Node             x            y            V\n";
    for (int i = 0; i < 8; ++i) {
      printf("                  %-5d %12g %12g %12g\n", elements[imap].emap[i],
             nodes[elements[imap].emap[i]].x, nodes[elements[imap].emap[i]].y,
             nodes[elements[imap].emap[i]].w[iw]);
    }
  }

  // Calculate quadrilateral field, which can degenerate to a triangular field
  if (elements[imap].degenerate) {
    return nodes[elements[imap].emap[0]].w[iw] * t1 * (2 * t1 - 1) +
           nodes[elements[imap].emap[1]].w[iw] * t2 * (2 * t2 - 1) +
           nodes[elements[imap].emap[2]].w[iw] * t3 * (2 * t3 - 1) +
           4 * nodes[elements[imap].emap[3]].w[iw] * t1 * t2 +
           4 * nodes[elements[imap].emap[4]].w[iw] * t1 * t3 +
           4 * nodes[elements[imap].emap[5]].w[iw] * t2 * t3;
  }

  return -nodes[elements[imap].emap[0]].w[iw] * (1 - t1) * (1 - t2) *
             (1 + t1 + t2) / 4 -
         nodes[elements[imap].emap[1]].w[iw] * (1 + t1) * (1 - t2) *
             (1 - t1 + t2) / 4 -
         nodes[elements[imap].emap[2]].w[iw] * (1 + t1) * (1 + t2) *
             (1 - t1 - t2) / 4 -
         nodes[elements[imap].emap[3]].w[iw] * (1 - t1) * (1 + t2) *
             (1 + t1 - t2) / 4 +
         nodes[elements[imap].emap[4]].w[iw] * (1 - t1) * (1 + t1) * (1 - t2) /
             2 +
         nodes[elements[imap].emap[5]].w[iw] * (1 + t1) * (1 + t2) * (1 - t2) /
             2 +
         nodes[elements[imap].emap[6]].w[iw] * (1 - t1) * (1 + t1) * (1 + t2) /
             2 +
         nodes[elements[imap].emap[7]].w[iw] * (1 - t1) * (1 + t2) * (1 - t2) /
             2;
}

Medium* ComponentAnsys121::GetMedium(const double xin, const double yin,
                                     const double zin) {

  // Copy the coordinates.
  double x = xin, y = yin, z = 0.;

  // Map the coordinates onto field map coordinates.
  bool xmirrored, ymirrored, zmirrored;
  double rcoordinate, rotation;
  MapCoordinates(x, y, z, xmirrored, ymirrored, zmirrored, rcoordinate,
                 rotation);

  if (zin < zMinBoundingBox || z > zMaxBoundingBox) {
    return NULL;
  }

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

  // Find the element that contains this point.
  double t1, t2, t3, t4, jac[4][4], det;
  int imap = FindElement5(x, y, z, t1, t2, t3, t4, jac, det);
  if (imap < 0) {
    if (m_debug) {
      std::cerr << m_className << "::GetMedium:\n";
      std::cerr << "    Point (" << x << ", " << y << ") not in the mesh.\n";
    }
    return NULL;
  }
  if (elements[imap].matmap >= m_nMaterials) {
    if (m_debug) {
      std::cerr << m_className << "::GetMedium:\n";
      std::cerr << "    Point (" << x << ", " << y << ")"
                << " has out of range material number " << imap << ".\n";
    }
    return NULL;
  }

  if (m_debug) {
    std::cout << m_className << "::GetMedium:\n";
    std::cout << "    Global: (" << x << ", " << y << "),\n";
    std::cout << "    Local: (" << t1 << ", " << t2 << ", " << t3 << ", " << t4
              << ") in element " << imap
              << " (degenerate: " << elements[imap].degenerate << ")\n";
    std::cout
        << "                  Node             x            y            V\n";
    for (int i = 0; i < 8; i++) {
      printf("                 %-5d %12g %12g %12g\n", elements[imap].emap[i],
             nodes[elements[imap].emap[i]].x, nodes[elements[imap].emap[i]].y,
             nodes[elements[imap].emap[i]].v);
    }
  }

  // Assign a medium.
  return materials[elements[imap].matmap].medium;
}

void ComponentAnsys121::SetRangeZ(const double zmin, const double zmax) {

  if (fabs(zmax - zmin) <= 0.) {
    std::cerr << m_className << "::SetRangeZ:\n";
    std::cerr << "    Zero range is not permitted.\n";
    return;
  }
  zMinBoundingBox = mapzmin = std::min(zmin, zmax);
  zMaxBoundingBox = mapzmax = std::max(zmin, zmax);
}

void ComponentAnsys121::UpdatePeriodicity() {

  UpdatePeriodicity2d();
  UpdatePeriodicityCommon();
}

double ComponentAnsys121::GetElementVolume(const int i) {

  if (i < 0 || i >= nElements) return 0.;
  const double surf =
      (fabs((nodes[elements[i].emap[1]].x - nodes[elements[i].emap[0]].x) *
                (nodes[elements[i].emap[2]].y - nodes[elements[i].emap[0]].y) -
            (nodes[elements[i].emap[2]].x - nodes[elements[i].emap[0]].x) *
                (nodes[elements[i].emap[1]].y - nodes[elements[i].emap[0]].y)) +
       fabs(
           (nodes[elements[i].emap[3]].x - nodes[elements[i].emap[0]].x) *
               (nodes[elements[i].emap[2]].y - nodes[elements[i].emap[0]].y) -
           (nodes[elements[i].emap[2]].x - nodes[elements[i].emap[0]].x) *
               (nodes[elements[i].emap[3]].y - nodes[elements[i].emap[0]].y))) /
      2.;
  return surf;
}

void ComponentAnsys121::GetAspectRatio(const int i, double& dmin,
                                       double& dmax) {

  if (i < 0 || i >= nElements) {
    dmin = dmax = 0.;
    return;
  }

  const int np = 8;
  // Loop over all pairs of vertices.
  for (int j = 0; j < np - 1; ++j) {
    for (int k = j + 1; k < np; ++k) {
      // Compute distance.
      const double dist = sqrt(
          pow(nodes[elements[i].emap[j]].x - nodes[elements[i].emap[k]].x, 2) +
          pow(nodes[elements[i].emap[j]].y - nodes[elements[i].emap[k]].y, 2));
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
}
