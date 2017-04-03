#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <math.h>

#include "ComponentAnsys123.hh"

namespace Garfield {

ComponentAnsys123::ComponentAnsys123() : ComponentFieldMap() {

  m_className = "ComponentAnsys123";
}

bool ComponentAnsys123::Initialise(std::string elist, std::string nlist,
                                   std::string mplist, std::string prnsol,
                                   std::string unit) {

  m_ready = false;
  m_warning = false;
  m_nWarnings = 10;
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
  long il = 0;
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
        std::cerr << "    Error reading file " << mplist << " line " << il
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
        std::cerr << "    Found out-of-range material index " << imat << "\n";
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
  Element newElement;
  int nbackground = 0;
  il = 0;
  int highestnode = 0;
  while (felist.getline(line, size, '\n')) {
    il++;
    // Skip page feed
    if (strcmp(line, "1") == 0) {
      felist.getline(line, size, '\n');
      il++;
      felist.getline(line, size, '\n');
      il++;
      felist.getline(line, size, '\n');
      il++;
      felist.getline(line, size, '\n');
      il++;
      felist.getline(line, size, '\n');
      il++;
      continue;
    }
    // Split the line in tokens
    char* token = NULL;
    // Split into tokens
    token = strtok(line, " ");
    // Skip blank lines and headers
    if (!token || strcmp(token, " ") == 0 || strcmp(token, "\n") == 0 ||
        int(token[0]) == 10 || int(token[0]) == 13 ||
        strcmp(token, "LIST") == 0 || strcmp(token, "ELEM") == 0) {
      continue;
    }
    // Read the element
    int ielem = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    int imat = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    token = strtok(NULL, " ");
    token = strtok(NULL, " ");
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
    if (!felist.getline(line, size, '\n')) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Error reading element " << ielem << ".\n";
      ok = false;
      break;
    }
    ++il;
    token = NULL;
    token = strtok(line, " ");
    int in8 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    int in9 = ReadInteger(token, -1, readerror);

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
                << "), material: " << imat << ",\n";
      std::cerr << "    nodes: (" << in0 << ", " << in1 << ", " << in2 << ", "
                << in3 << ", " << in4 << ", " << in5 << ", " << in6 << ", "
                << in7 << ", " << in8 << ", " << in9 << ")\n";
      ok = false;
    }

    // Check the material number and ensure that epsilon is non-negative
    if (imat < 1 || imat > (int)m_nMaterials) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Out-of-range material number on file " << elist
                << " (line " << il << ").\n";
      std::cerr << "    Element: " << ielem << ", material: " << imat << ",\n";
      std::cerr << "    nodes: (" << in0 << ", " << in1 << ", " << in2 << ", "
                << in3 << ", " << in4 << ", " << in5 << ", " << in6 << ", "
                << in7 << ", " << in8 << ", " << in9 << ")\n";
      ok = false;
    }
    if (materials[imat - 1].eps < 0) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Element " << ielem << " in element list " << elist
                << "\n";
      std::cerr << "    uses material " << imat
                << " which has not been assigned\n";
      std::cerr << "    a positive permittivity in material list " << mplist
                << ".\n";
      ok = false;
    }

    // Check the node numbers
    if (in0 < 1 || in1 < 1 || in2 < 1 || in3 < 1 || in4 < 1 || in5 < 1 ||
        in6 < 1 || in7 < 1 || in8 < 1 || in9 < 1) {
      std::cerr << m_className << "::Initialise:\n";
      std::cerr << "    Found a node number < 1 on file " << elist << " (line "
                << il << ").\n";
      std::cerr << "    Element: " << ielem << ", material: " << imat << ",\n";
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

    // Skip quadrilaterals which are background.
    if (m_deleteBackground && materials[imat - 1].ohm == 0) {
      nbackground++;
      continue;
    }

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
      std::cerr << "    Element " << ielem << " of file " << elist
                << " is degenerate,\n";
      std::cerr << "    no such elements allowed in this type of map.\n";
      ok = false;
    }

    newElement.degenerate = false;

    // Store the material reference
    newElement.matmap = imat - 1;

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
    nElements++;
  }
  // Close the file
  felist.close();

  // Tell how many lines read.
  std::cout << m_className << "::Initialise:\n";
  std::cout << "    Read " << nElements << " elements from file " << elist
            << ",\n";
  std::cout << "    highest node number: " << highestnode << ",\n";
  std::cout << "    background elements skipped: " << nbackground << "\n";
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
    std::cout << m_className << ":Initialise:\n";
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
  Node newNode;
  newNode.w.clear();
  il = 0;
  while (fnlist.getline(line, size, '\n')) {
    il++;
    // Skip page feed
    if (strcmp(line, "1") == 0) {
      fnlist.getline(line, size, '\n');
      il++;
      fnlist.getline(line, size, '\n');
      il++;
      fnlist.getline(line, size, '\n');
      il++;
      fnlist.getline(line, size, '\n');
      il++;
      fnlist.getline(line, size, '\n');
      il++;
      continue;
    }
    // Split the line in tokens
    char* token = NULL;
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
    nNodes++;
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
  while (fprnsol.getline(line, size, '\n')) {
    il++;
    // Skip page feed
    if (strcmp(line, "1") == 0) {
      fprnsol.getline(line, size, '\n');
      il++;
      fprnsol.getline(line, size, '\n');
      il++;
      fprnsol.getline(line, size, '\n');
      il++;
      fprnsol.getline(line, size, '\n');
      il++;
      fprnsol.getline(line, size, '\n');
      il++;
      continue;
    }
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
      std::cerr << "    Error reading file " << prnsol << " (line << " << il
                << ").\n";
      fprnsol.close();
      ok = false;
      return false;
    }
    // Check node number and store if OK
    if (inode < 1 || inode > highestnode) {
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
              << prnsol << "\n";
    std::cerr << "    does not match the node list (" << nNodes << ").\n";
    ok = false;
  }

  // Set the ready flag
  if (ok) {
    m_ready = true;
  } else {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr
        << "    Field map could not be read and can not be interpolated.\n";
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

bool ComponentAnsys123::SetWeightingField(std::string prnsol,
                                          std::string label) {

  if (!m_ready) {
    PrintNotReady("SetWeightingField");
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
    // Skip page feed
    if (strcmp(line, "1") == 0) {
      fprnsol.getline(line, size, '\n');
      il++;
      fprnsol.getline(line, size, '\n');
      il++;
      fprnsol.getline(line, size, '\n');
      il++;
      fprnsol.getline(line, size, '\n');
      il++;
      fprnsol.getline(line, size, '\n');
      il++;
      continue;
    }
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
      std::cerr << "    Error reading file " << prnsol.c_str() << " (line "
                << il << ").\n";
      fprnsol.close();
      return false;
    }
    // Check node number and store if OK.
    if (inode < 1 || inode > nNodes) {
      std::cerr << m_className << "::SetWeightingField:\n";
      std::cerr << "    Node number " << inode << " out of range\n";
      std::cerr << "    on potential file " << prnsol.c_str() << " (line " << il
                << ").\n";
      ok = false;
    } else {
      nodes[inode - 1].w[iw] = volt;
      nread++;
    }
  }
  // Close the file.
  fprnsol.close();

  std::cout << m_className << "::SetWeightingField:\n";
  std::cout << "    Read " << nread << " potentials from file "
            << prnsol.c_str() << ".\n";
  // Check the number of nodes.
  if (nread != nNodes) {
    std::cerr << m_className << "::SetWeightingField:\n";
    std::cerr << "    Number of nodes read (" << nread << ") "
              << "    on potential file " << prnsol.c_str() << "\n";
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

void ComponentAnsys123::ElectricField(const double x, const double y,
                                      const double z, double& ex, double& ey,
                                      double& ez, Medium*& m, int& status) {

  double v = 0.;
  ElectricField(x, y, z, ex, ey, ez, v, m, status);
}

void ComponentAnsys123::ElectricField(const double xin, const double yin,
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
  m = NULL;

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
      std::cerr << m_className << "::ElectricField:\n";
      std::cerr << "    Point (" << x << ", " << y << ", " << z
                << ") not in the mesh.\n";
    }
    status = -6;
    return;
  }

  if (m_debug) {
    PrintElement("ElectricField", x, y, z, t1, t2, t3, t4, imap, 10);
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
  // Tetrahedral field
  const double invdet = 1. / det;
  volt = n0.v * t1 * (2 * t1 - 1) + n1.v * t2 * (2 * t2 - 1) +
         n2.v * t3 * (2 * t3 - 1) + n3.v * t4 * (2 * t4 - 1) +
         4 * n4.v * t1 * t2 + 4 * n5.v * t1 * t3 + 4 * n6.v * t1 * t4 +
         4 * n7.v * t2 * t3 + 4 * n8.v * t2 * t4 + 4 * n9.v * t3 * t4;

  ex = -(n0.v * (4 * t1 - 1) * jac[0][1] + n1.v * (4 * t2 - 1) * jac[1][1] +
         n2.v * (4 * t3 - 1) * jac[2][1] + n3.v * (4 * t4 - 1) * jac[3][1] +
         n4.v * (4 * t2 * jac[0][1] + 4 * t1 * jac[1][1]) +
         n5.v * (4 * t3 * jac[0][1] + 4 * t1 * jac[2][1]) +
         n6.v * (4 * t4 * jac[0][1] + 4 * t1 * jac[3][1]) +
         n7.v * (4 * t3 * jac[1][1] + 4 * t2 * jac[2][1]) +
         n8.v * (4 * t4 * jac[1][1] + 4 * t2 * jac[3][1]) +
         n9.v * (4 * t4 * jac[2][1] + 4 * t3 * jac[3][1])) * invdet;

  ey = -(n0.v * (4 * t1 - 1) * jac[0][2] + n1.v * (4 * t2 - 1) * jac[1][2] +
         n2.v * (4 * t3 - 1) * jac[2][2] + n3.v * (4 * t4 - 1) * jac[3][2] +
         n4.v * (4 * t2 * jac[0][2] + 4 * t1 * jac[1][2]) +
         n5.v * (4 * t3 * jac[0][2] + 4 * t1 * jac[2][2]) +
         n6.v * (4 * t4 * jac[0][2] + 4 * t1 * jac[3][2]) +
         n7.v * (4 * t3 * jac[1][2] + 4 * t2 * jac[2][2]) +
         n8.v * (4 * t4 * jac[1][2] + 4 * t2 * jac[3][2]) +
         n9.v * (4 * t4 * jac[2][2] + 4 * t3 * jac[3][2])) * invdet;

  ez = -(n0.v * (4 * t1 - 1) * jac[0][3] + n1.v * (4 * t2 - 1) * jac[1][3] +
         n2.v * (4 * t3 - 1) * jac[2][3] + n3.v * (4 * t4 - 1) * jac[3][3] +
         n4.v * (4 * t2 * jac[0][3] + 4 * t1 * jac[1][3]) +
         n5.v * (4 * t3 * jac[0][3] + 4 * t1 * jac[2][3]) +
         n6.v * (4 * t4 * jac[0][3] + 4 * t1 * jac[3][3]) +
         n7.v * (4 * t3 * jac[1][3] + 4 * t2 * jac[2][3]) +
         n8.v * (4 * t4 * jac[1][3] + 4 * t2 * jac[3][3]) +
         n9.v * (4 * t4 * jac[2][3] + 4 * t3 * jac[3][3])) * invdet;

  // Transform field to global coordinates
  UnmapFields(ex, ey, ez, x, y, z, xmirr, ymirr, zmirr, rcoordinate, rotation);

  // Drift medium?
  if (m_debug) {
    std::cout << m_className << "::ElectricField:\n";
    std::cout << "    Material " << element.matmap << ", drift flag "
              << materials[element.matmap].driftmedium << ".\n";
  }
  m = materials[element.matmap].medium;
  status = -5;
  if (materials[element.matmap].driftmedium) {
    if (m && m->IsDriftable()) status = 0;
  }
}

void ComponentAnsys123::WeightingField(const double xin, const double yin,
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

  if (m_debug) {
    PrintElement("WeightingField", x, y, z, t1, t2, t3, t4, imap, 10, iw);
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
  // Tetrahedral field
  const double invdet = 1. / det;
  wx = -(n0.w[iw] * (4 * t1 - 1) * jac[0][1] +
         n1.w[iw] * (4 * t2 - 1) * jac[1][1] +
         n2.w[iw] * (4 * t3 - 1) * jac[2][1] +
         n3.w[iw] * (4 * t4 - 1) * jac[3][1] +
         n4.w[iw] * (4 * t2 * jac[0][1] + 4 * t1 * jac[1][1]) +
         n5.w[iw] * (4 * t3 * jac[0][1] + 4 * t1 * jac[2][1]) +
         n6.w[iw] * (4 * t4 * jac[0][1] + 4 * t1 * jac[3][1]) +
         n7.w[iw] * (4 * t3 * jac[1][1] + 4 * t2 * jac[2][1]) +
         n8.w[iw] * (4 * t4 * jac[1][1] + 4 * t2 * jac[3][1]) +
         n9.w[iw] * (4 * t4 * jac[2][1] + 4 * t3 * jac[3][1])) * invdet;

  wy = -(n0.w[iw] * (4 * t1 - 1) * jac[0][2] +
         n1.w[iw] * (4 * t2 - 1) * jac[1][2] +
         n2.w[iw] * (4 * t3 - 1) * jac[2][2] +
         n3.w[iw] * (4 * t4 - 1) * jac[3][2] +
         n4.w[iw] * (4 * t2 * jac[0][2] + 4 * t1 * jac[1][2]) +
         n5.w[iw] * (4 * t3 * jac[0][2] + 4 * t1 * jac[2][2]) +
         n6.w[iw] * (4 * t4 * jac[0][2] + 4 * t1 * jac[3][2]) +
         n7.w[iw] * (4 * t3 * jac[1][2] + 4 * t2 * jac[2][2]) +
         n8.w[iw] * (4 * t4 * jac[1][2] + 4 * t2 * jac[3][2]) +
         n9.w[iw] * (4 * t4 * jac[2][2] + 4 * t3 * jac[3][2])) * invdet;

  wz = -(n0.w[iw] * (4 * t1 - 1) * jac[0][3] +
         n1.w[iw] * (4 * t2 - 1) * jac[1][3] +
         n2.w[iw] * (4 * t3 - 1) * jac[2][3] +
         n3.w[iw] * (4 * t4 - 1) * jac[3][3] +
         n4.w[iw] * (4 * t2 * jac[0][3] + 4 * t1 * jac[1][3]) +
         n5.w[iw] * (4 * t3 * jac[0][3] + 4 * t1 * jac[2][3]) +
         n6.w[iw] * (4 * t4 * jac[0][3] + 4 * t1 * jac[3][3]) +
         n7.w[iw] * (4 * t3 * jac[1][3] + 4 * t2 * jac[2][3]) +
         n8.w[iw] * (4 * t4 * jac[1][3] + 4 * t2 * jac[3][3]) +
         n9.w[iw] * (4 * t4 * jac[2][3] + 4 * t3 * jac[3][3])) * invdet;

  // Transform field to global coordinates
  UnmapFields(wx, wy, wz, x, y, z, xmirr, ymirr, zmirr, rcoordinate, rotation);
}

double ComponentAnsys123::WeightingPotential(const double xin, const double yin,
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

  if (m_debug) {
    PrintElement("WeightingPotential", x, y, z, t1, t2, t3, t4, imap, 10, iw);
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
  return n0.w[iw] * t1 * (2 * t1 - 1) + n1.w[iw] * t2 * (2 * t2 - 1) +
         n2.w[iw] * t3 * (2 * t3 - 1) + n3.w[iw] * t4 * (2 * t4 - 1) +
         4 * n4.w[iw] * t1 * t2 + 4 * n5.w[iw] * t1 * t3 +
         4 * n6.w[iw] * t1 * t4 + 4 * n7.w[iw] * t2 * t3 +
         4 * n8.w[iw] * t2 * t4 + 4 * n9.w[iw] * t3 * t4;
}

Medium* ComponentAnsys123::GetMedium(const double xin, const double yin,
                                     const double zin) {

  // Copy the coordinates.
  double x = xin, y = yin, z = zin;

  // Map the coordinates onto field map coordinates.
  bool xmirr, ymirr, zmirr;
  double rcoordinate, rotation;
  MapCoordinates(x, y, z, xmirr, ymirr, zmirr, rcoordinate, rotation);

  // Do not proceed if not properly initialised.
  if (!m_ready) {
    std::cerr << m_className << "::GetMedium:\n";
    std::cerr << "    Field map not available for interpolation.\n";
    return NULL;
  }
  if (m_warning) PrintWarning("GetMedium");

  // Find the element that contains this point.
  double t1, t2, t3, t4, jac[4][4], det;
  const int imap = FindElement13(x, y, z, t1, t2, t3, t4, jac, det);
  if (imap < 0) {
    if (m_debug) {
      std::cerr << m_className << "::GetMedium:\n";
      std::cerr << "    Point (" << x << ", " << y << ", " << z
                << ") not in the mesh.\n";
    }
    return NULL;
  }
  const Element& element = elements[imap];
  if (element.matmap >= m_nMaterials) {
    if (m_debug) {
      std::cerr << m_className << "::GetMedium:\n";
      std::cerr << "    Point (" << x << ", " << y << ", " << z << ")"
                << " has out of range material number " << imap << ".\n";
    }
    return NULL;
  }

  if (m_debug) {
    PrintElement("GetMedium", x, y, z, t1, t2, t3, t4, imap, 10);
  }

  // Assign a medium.
  return materials[element.matmap].medium;
}

double ComponentAnsys123::GetElementVolume(const unsigned int i) {

  if (i >= elements.size()) return 0.;
  const Element& element = elements[i];
  const Node& n0 = nodes[element.emap[0]];
  const Node& n1 = nodes[element.emap[1]];
  const Node& n2 = nodes[element.emap[2]];
  const Node& n3 = nodes[element.emap[3]];
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

void ComponentAnsys123::GetAspectRatio(const unsigned int i, double& dmin,
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
}
