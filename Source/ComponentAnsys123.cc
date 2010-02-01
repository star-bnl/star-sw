#include <stdio.h>
#include <string.h>
#include <fstream>
#include <stdlib.h>
#include <math.h>

#include "ComponentAnsys123.hh"
#include "Input.hh"

namespace Garfield {

ComponentAnsys123::ComponentAnsys123(char* elist, char* nlist, 
                                     char* mplist, char* prnsol, char* unit) {

  // Keep track of the success
  bool ok = true;

  // Buffer for reading
  int il;
  const int size = 100;
  char line[size];

  // Open the material list
  std::ifstream fmplist;
  fmplist.open(mplist, std::ios::in);
  if (fmplist.fail()) {
    printf("ComponentAnsys123: Could not open material file %s for reading; the file perhaps does not exist.\n", mplist);
    return;
  }

  // Read the material list
  nMaterials = 0;
  il = 0;
  bool readerror = false;
  while (fmplist.getline(line, size, '\n')) {
    il++;
    // Split the line in tokens
    char* token = NULL;
    // Split into tokens
    token = strtok(line, " ");
    // Skip blank lines and headers
    if (!token ||
        strcmp(token," ") == 0 || 
        strcmp(token,"\n") == 0 || 
        strcmp(token,"TEMPERATURE") == 0 ||
        strcmp(token,"PROPERTY=") == 0 ||
        int(token[0]) == 10 || 
        int(token[0]) == 13) continue;
    // Read number of materials, ensure it does not exceed the maximum and initialise the list
    if (strcmp(token,"LIST") == 0) {
      token = strtok(NULL, " ");
      token = strtok(NULL, " ");
      token = strtok(NULL, " ");
      token = strtok(NULL, " "); 
      nMaterials = ReadInteger(token, -1, readerror);
      if (readerror) {
        printf("ComponentAnsys123: Error reading file %s (line %d).\n", mplist, il);
        fmplist.close();
        ok = false;
        return;
      }
      materials.resize(nMaterials);
      for (int i = 0; i < nMaterials; i++) {
        materials[i].ohm = -1;
        materials[i].eps = -1;
        materials[i].medium = NULL;
      }
      if (debug) {
        printf("ComponentAnsys123: Number of materials: %d\n", nMaterials);
      }
    } else if (strcmp(token,"PROPERTY") == 0) {
      token = strtok(NULL, " ");
      token = strtok(NULL, " ");
      int itype = 0;
      if (strcmp(token,"PERX") == 0) {
        itype = 1;
      } else if (strcmp(token,"RSVX") == 0) {
        itype = 2;
      } else {
        printf("ComponentAnsys123:\n");
        printf("    Found unknown material property flag %s on material properties file %s (line %d).\n",
               token, mplist, il);
        ok = false;
      }
      token = strtok(NULL, " ");
      token = strtok(NULL, " "); 
      int imat = ReadInteger(token, -1, readerror);
      if (readerror) {
        printf("ComponentAnsys123: Error reading file %s (line %d).\n", mplist, il);
        fmplist.close();
        ok = false;
        return;
      } else if (imat < 1 || imat > nMaterials) {
        printf("ComponentAnsys123:\n");
        printf("    Found out-of-range material index %d in material properties file %s.\n", imat, mplist);
        ok = false;
      } else {
        fmplist.getline(line, size, '\n'); il++;
        fmplist.getline(line, size, '\n'); il++;
        token = NULL;
        token = strtok(line, " ");
        token = strtok(NULL, " ");
        if (itype == 1) {
          materials[imat - 1].eps = ReadDouble(token, -1, readerror);
        } else if (itype == 2) {
          materials[imat - 1].ohm = ReadDouble(token, -1, readerror);
        }
        if (readerror) {
          printf("ComponentAnsys123: Error reading file %s (line %d).\n", mplist, il);
          fmplist.close();
          ok = false;
          return;
        }
      }
    }
  }
  
  // Close the file
  fmplist.close();

  // Find the lowest epsilon, check for eps = 0, set default drift media
  double epsmin = -1; int iepsmin = -1;
  for (int imat = 0; imat < nMaterials; imat++) {
    if (materials[imat].eps < 0) {
      continue;
    } else if (materials[imat].eps == 0) {
      printf("ComponentAnsys123:\n");
      printf("    Material %d has been assigned a permittivity equal to zero in %s.\n", imat, mplist);
      ok = false;
    } else if (iepsmin < 0 || epsmin > materials[imat].eps) {
      epsmin = materials[imat].eps;
      iepsmin = imat;
    }
  }
  
  if (iepsmin < 0) {
    printf("ComponentAnsys123: No material with positive permittivity found in material list %s.\n", mplist);
    ok = false;
  } else {
    for (int imat = 0; imat < nMaterials; imat++) {
      if (imat == iepsmin) {
        materials[imat].driftmedium = true;
      } else {
        materials[imat].driftmedium = false;
      }
    }
  }
  
  // Tell how many lines read
  printf("ComponentAnsys123: Read properties of %d materials from file %s.\n", nMaterials, mplist);
  if (debug) PrintMaterials();
  
  // Open the element list
  std::ifstream felist;
  felist.open(elist, std::ios::in);
  if (felist.fail()) {
    printf("ComponentAnsys123:\n");
    printf("    Could not open element file %s for reading.\n", elist);
    printf("    The file perhaps does not exist.\n");
    return;
  }

  // Read the element list
  elements.clear();  
  nElements = 0;
  element newElement;
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
    if (!token ||
        strcmp(token," ") == 0 || 
        strcmp(token,"\n") == 0 || 
        int(token[0]) == 10 || 
        int(token[0]) == 13 ||
        strcmp(token,"LIST") == 0 ||
        strcmp(token,"ELEM") == 0) continue;
    // Read the element
    int ielem = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); 
    int imat = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    token = strtok(NULL, " ");
    token = strtok(NULL, " ");
    token = strtok(NULL, " ");
    token = strtok(NULL, " "); int in0 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in1 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in2 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in3 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in4 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in5 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in6 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in7 = ReadInteger(token, -1, readerror);
    felist.getline(line, size, '\n');
    if (!line) {
      printf("ComponentAnsys123: Error reading element %d.\n", ielem);
      ok = false;
      break;
    }
    token = NULL;
    token = strtok(line, " "); int in8 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in9 = ReadInteger(token, -1, readerror);
    
    // Check synchronisation
    if (readerror) {
      printf("ComponentAnsys123: Error reading file %s (line %d).\n", elist, il);
      felist.close();
      ok = false;
      return;
    } else if (ielem - 1 != nElements + nbackground) {
      printf("ComponentAnsys123:\n");
      printf("    Synchronisation lost on file %s (line %d).\n", elist, il);
      printf("                  Element: %d (expected %d), material: %d, nodes: (%d %d %d %d %d %d %d %d %d %d)\n",
             ielem, nElements, imat, in0, in1, in2, in3, in4, in5, in6, in7, in8, in9);
      ok = false;
    }
    
    // Check the material number and ensure that epsilon is non-negative
    if (imat < 1 || imat > nMaterials) {
	  printf("ComponentAnsys123: Out-of-range material number on file %s (line %d).\n", elist, il);
	  printf("                  Element: %d, material: %d, nodes: (%d %d %d %d %d %d %d %d %d %d)\n",
             ielem, imat, in0, in1, in2, in3, in4, in5, in6, in7, in8, in9);
      ok = false;
    }
    if (materials[imat - 1].eps < 0) {
      printf("ComponentAnsys123: \n");
      printf("    Element %d in element list %s uses material %d which has not\n    been assigned a positive permittivity in material list %s.\n",
             ielem, elist, imat, mplist);
      ok = false;
    }

    // Check the node numbers
    if (in0 < 1 || in1 < 1 || in2 < 1 || in3 < 1 || in4 < 1 || in5 < 1 || in6 < 1 || in7 < 1 || in8 < 1 || in9 < 1) {
	  printf("ComponentAnsys123: Found a node number < 1 on file %s (line %d).\n", elist, il);
	  printf("                  Element: %d, material: %d, nodes: (%d %d %d %d %d %d %d %d %d %d).\n",
             ielem, imat, in0, in1, in2, in3, in4, in5, in6, in7, in8, in9);
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
    if (deleteBackground && materials[imat - 1].ohm == 0) {
      nbackground++;
      continue;
    }    
    
    // These elements must not be degenerate
    if (in0 == in1 || in0 == in2 || in0 == in3 || in0 == in4 || in0 == in5 || in0 == in6 || in0 == in7 || in0 == in8 || in0 == in9 || 
        in1 == in2 || in1 == in3 || in1 == in4 || in1 == in5 || in1 == in6 || in1 == in7 || in1 == in8 || in1 == in9 || 
        in2 == in3 || in2 == in4 || in2 == in5 || in2 == in6 || in2 == in7 || in2 == in8 || in2 == in9 || 
        in3 == in4 || in3 == in5 || in3 == in6 || in3 == in7 || in3 == in8 || in3 == in9 || 
        in4 == in5 || in4 == in6 || in4 == in7 || in4 == in8 || in4 == in9 || 
        in5 == in6 || in5 == in7 || in5 == in8 || in5 == in9 || 
        in6 == in7 || in6 == in8 || in6 == in9 || 
        in7 == in8 || in7 == in9 || 
        in8 == in9) {
      printf("ComponentAnsys123:\n");
      printf("    Element %d of file %s is degenerate, no such elements allowed in this type of map.\n", ielem, elist);
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
  
  // Tell how many lines read
  printf("ComponentAnsys123: Read %d elements from file %s,\n", nElements, elist);
  printf("                  highest node number: %d, background elements skipped: %d.\n",
         highestnode, nbackground);
  // Check the value of the unit
  double funit;
  if (strcmp(unit,"mum") == 0 || strcmp(unit,"micron") == 0 || strcmp(unit,"micrometer") == 0) {
    funit = 0.0001;
  } else if (strcmp(unit,"mm") == 0 || strcmp(unit,"millimeter") == 0) {
    funit = 0.1;
  } else if (strcmp(unit,"cm") == 0 || strcmp(unit,"centimeter") == 0) {
    funit = 1.0;
  } else if (strcmp(unit,"m") == 0 || strcmp(unit,"meter") == 0) {
    funit = 100.0;
  } else {
    printf("ComponentAnsys123: Unknown length unit %s.\n", unit);
    ok = false;
    funit = 1.0;    
  }
  if (debug) printf("ComponentAnsys123: Unit scaling factor = %g.\n", funit);
                         
  // Open the node list
  std::ifstream fnlist;
  fnlist.open(nlist, std::ios::in);
  if (fnlist.fail()) {
    printf("ComponentAnsys123: Could not open nodes file %s for reading; the file perhaps does not exist.\n", nlist);
    return;
  }

  // Read the node list
  nodes.clear();
  nNodes = 0;
  node newNode;
  il = 0;
  while (fnlist.getline(line, size, '\n')) {
    il++;
    // Split the line in tokens
    char* token = NULL;
    // Split into tokens
    token = strtok(line, " ");
    // Skip blank lines and headers
    if (!token ||
        strcmp(token," ") == 0 || 
        strcmp(token,"\n") == 0 || 
        int(token[0]) == 10 || 
        int(token[0]) == 13 ||
        strcmp(token,"LIST") == 0 ||
        strcmp(token,"NODE") == 0) continue;
    // Read the element
    int inode = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); double xnode = ReadDouble(token, -1, readerror);
    token = strtok(NULL, " "); double ynode = ReadDouble(token, -1, readerror);
    token = strtok(NULL, " "); double znode = ReadDouble(token, -1, readerror);
    // Check syntax
    if (readerror) {
      printf("ComponentAnsys123: Error reading file %s (line %d).\n", nlist, il);
      fnlist.close();
      ok = false;
      return;
    }
    // Check synchronisation
    if (inode - 1 != nNodes) {
      printf("ComponentAnsys123: Synchronisation lost on file %s (line %d).\n", nlist, il);
      printf("                  Node: %d (expected %d), (x,y,z) = (%g,%g,%g)\n", inode, nNodes, xnode, ynode, znode);
      ok = false;
    }    
    // Store the point coordinates
    newNode.xmap = xnode * funit;
    newNode.ymap = ynode * funit;
    newNode.zmap = znode * funit;
    nodes.push_back(newNode);
    nNodes++;
  }
  // Close the file
  fnlist.close();
  // Tell how many lines read
  printf("ComponentAnsys123: Read %d nodes from file %s.\n", nNodes, nlist);
  // Check number of nodes
  if (nNodes != highestnode) {
    printf("ComponentAnsys123: Number of nodes read (%d) on %s does not match element list (%d).\n",
           nNodes, nlist, highestnode);
    ok = false;
  }

  // Open the voltage list
  std::ifstream fprnsol;
  fprnsol.open(prnsol, std::ios::in);
  if (fprnsol.fail()) {
    printf("ComponentAnsys123: Could not open potential file %s for reading; the file perhaps does not exist.\n", prnsol);
    return;
  }

  // Read the voltage list
  il = 0;
  int nread = 0;
  while (fprnsol.getline(line, size, '\n')) {
    il++;
    // Split the line in tokens
    char* token = NULL;
    // Split into tokens
    token = strtok(line, " ");
    // Skip blank lines and headers
    if (!token ||
        strcmp(token," ") == 0 || 
        strcmp(token,"\n") == 0 || 
        int(token[0]) == 10 || 
        int(token[0]) == 13 ||
        strcmp(token,"PRINT")   == 0 ||
        strcmp(token,"*****")   == 0 ||
        strcmp(token,"LOAD")    == 0 ||
        strcmp(token,"TIME=")   == 0 ||
        strcmp(token,"MAXIMUM") == 0 ||
        strcmp(token,"VALUE")   == 0 ||
        strcmp(token,"NODE")    == 0) continue;
    // Read the element
    int inode = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); double volt = ReadDouble(token, -1, readerror);
    // Check syntax
    if (readerror) {
      printf("ComponentAnsys123: Error reading file %s (line %d).\n", prnsol, il);
      fprnsol.close();
      ok = false;
      return;
    }
    // Check node number and store if OK
    if (inode < 1 || inode > highestnode) {
      printf("ComponentAnsys123: Node number %d out of range on potential file %s (line %d).\n", inode, prnsol, il);
      ok = false;
    } else {
      nodes[inode - 1].vmap = volt;
      nread++;
    }
  }
  // Close the file
  fprnsol.close();
  // Tell how many lines read
  printf("ComponentAnsys123: Read %d potentials from file %s.\n", nread, prnsol);
  // Check number of nodes
  if (nread != nNodes) {
    printf("ComponentAnsys123: Number of nodes read (%d) on potential file %s does not match the node list (%d).\n",
           nread, prnsol, nNodes);
    ok = false;
  }

  // Set the ready flag
  if (ok) {
    ready = true;
  } else {
    printf("ComponentAnsys123: Field map could not be read and can not be interpolated.\n");
  }

  // Establish the ranges
  SetRange();
  UpdatePeriodicity();

}

void 
ComponentAnsys123::ElectricField(const double x, const double y, const double z,
                                 double& ex, double& ey, double& ez,
                                 Medium*& m, int& status) {

  double v;
  ElectricField(x, y, z, ex, ey, ez, v, m, status);

}

void 
ComponentAnsys123::ElectricField(const double xin, const double yin, const double zin,
                                 double& ex, double& ey, double& ez, double& volt, 
                                 Medium*& m, int& status) {

  // Copy the coordinates
  double x = xin, y = yin, z = zin;

  // Map the coordinates onto field map coordinates
  bool xmirrored, ymirrored, zmirrored;
  double rcoordinate, rotation;
  MapCoordinates(x, y, z, 
                 xmirrored, ymirrored, zmirrored,
                 rcoordinate, rotation);

  // Initial values
  ex = ey = ez = volt = 0;
  status = 0;
  m = 0;
  
  // Do not proceed if not properly initialised.
  if (!ready) {
    status = -10;
    printf("ComponentAnsys123::ElectricField: Field map not available for interpolation.\n");
    return;
  }
  
  if (warning) {
    printf("ComponentAnsys123::ElectricField: Warnings have been issued for this field map.\n");
  }

  // Find the element that contains this point
  double t1, t2, t3, t4, jac[4][4], det;
  int imap = FindElement13(x, y, z, t1, t2, t3, t4, jac, det);
  if (imap < 0) {
    if (debug) {
      printf("ComponentAnsys123::ElectricField: Point (%g,%g,%g) not in the mesh.\n", x, y, z);
    }
    status = -6;
    return;
  }
  
  if (debug) {
    printf("ComponentAnsys123::ElectricField: Global: (%g,%g,%g), Local: (%g,%g,%g,%g) in element %d\n",
           x, y, z, t1, t2, t3, t4, imap);
    printf("                          Node             x            y            z            V\n");
    for (int i = 0; i < 10; i++) {
      printf("                          %-5d %12g %12g %12g %12g\n",
             elements[imap].emap[i],
             nodes[elements[imap].emap[i]].xmap,
             nodes[elements[imap].emap[i]].ymap,
             nodes[elements[imap].emap[i]].zmap,
             nodes[elements[imap].emap[i]].vmap);
    }
  }

  // Tetrahedral field
  volt =
    nodes[elements[imap].emap[0]].vmap * t1 * (2 * t1 - 1) + nodes[elements[imap].emap[1]].vmap * t2 * (2 * t2 - 1) + 
    nodes[elements[imap].emap[2]].vmap * t3 * (2 * t3 - 1) + nodes[elements[imap].emap[3]].vmap * t4 * (2 * t4 - 1) + 
    4 * nodes[elements[imap].emap[4]].vmap * t1 * t2 +     4 * nodes[elements[imap].emap[5]].vmap * t1 * t3 + 
    4 * nodes[elements[imap].emap[6]].vmap * t1 * t4 +     4 * nodes[elements[imap].emap[7]].vmap * t2 * t3 + 
    4 * nodes[elements[imap].emap[8]].vmap * t2 * t4 +     4 * nodes[elements[imap].emap[9]].vmap * t3 * t4;
  ex = -(nodes[elements[imap].emap[0]].vmap * (4 * t1 - 1) * jac[0][1] + 
         nodes[elements[imap].emap[1]].vmap * (4 * t2 - 1) * jac[1][1] + 
         nodes[elements[imap].emap[2]].vmap * (4 * t3 - 1) * jac[2][1] + 
         nodes[elements[imap].emap[3]].vmap * (4 * t4 - 1) * jac[3][1] + 
         nodes[elements[imap].emap[4]].vmap * (4 * t2 * jac[0][1] + 4 * t1 * jac[1][1]) + 
         nodes[elements[imap].emap[5]].vmap * (4 * t3 * jac[0][1] + 4 * t1 * jac[2][1]) + 
         nodes[elements[imap].emap[6]].vmap * (4 * t4 * jac[0][1] + 4 * t1 * jac[3][1]) + 
         nodes[elements[imap].emap[7]].vmap * (4 * t3 * jac[1][1] + 4 * t2 * jac[2][1]) + 
         nodes[elements[imap].emap[8]].vmap * (4 * t4 * jac[1][1] + 4 * t2 * jac[3][1]) + 
         nodes[elements[imap].emap[9]].vmap * (4 * t4 * jac[2][1] + 4 * t3 * jac[3][1])) / det;
  ey = -(nodes[elements[imap].emap[0]].vmap * (4 * t1 - 1) * jac[0][2] + 
         nodes[elements[imap].emap[1]].vmap * (4 * t2 - 1) * jac[1][2] + 
         nodes[elements[imap].emap[2]].vmap * (4 * t3 - 1) * jac[2][2] + 
         nodes[elements[imap].emap[3]].vmap * (4 * t4 - 1) * jac[3][2] + 
         nodes[elements[imap].emap[4]].vmap * (4 * t2 * jac[0][2] + 4 * t1 * jac[1][2]) + 
         nodes[elements[imap].emap[5]].vmap * (4 * t3 * jac[0][2] + 4 * t1 * jac[2][2]) + 
         nodes[elements[imap].emap[6]].vmap * (4 * t4 * jac[0][2] + 4 * t1 * jac[3][2]) + 
         nodes[elements[imap].emap[7]].vmap * (4 * t3 * jac[1][2] + 4 * t2 * jac[2][2]) + 
         nodes[elements[imap].emap[8]].vmap * (4 * t4 * jac[1][2] + 4 * t2 * jac[3][2]) + 
         nodes[elements[imap].emap[9]].vmap * (4 * t4 * jac[2][2] + 4 * t3 * jac[3][2])) / det;
  ez = -(nodes[elements[imap].emap[0]].vmap * (4 * t1 - 1) * jac[0][3] + 
         nodes[elements[imap].emap[1]].vmap * (4 * t2 - 1) * jac[1][3] + 
         nodes[elements[imap].emap[2]].vmap * (4 * t3 - 1) * jac[2][3] + 
         nodes[elements[imap].emap[3]].vmap * (4 * t4 - 1) * jac[3][3] + 
         nodes[elements[imap].emap[4]].vmap * (4 * t2 * jac[0][3] + 4 * t1 * jac[1][3]) + 
         nodes[elements[imap].emap[5]].vmap * (4 * t3 * jac[0][3] + 4 * t1 * jac[2][3]) + 
         nodes[elements[imap].emap[6]].vmap * (4 * t4 * jac[0][3] + 4 * t1 * jac[3][3]) + 
         nodes[elements[imap].emap[7]].vmap * (4 * t3 * jac[1][3] + 4 * t2 * jac[2][3]) + 
         nodes[elements[imap].emap[8]].vmap * (4 * t4 * jac[1][3] + 4 * t2 * jac[3][3]) + 
         nodes[elements[imap].emap[9]].vmap * (4 * t4 * jac[2][3] + 4 * t3 * jac[3][3])) / det;

  // Transform field to global coordinates
  UnmapFields(ex, ey, ez,
              x, y, z,
              xmirrored, ymirrored, zmirrored,
              rcoordinate, rotation);

  // Drift medium ?
  if (debug) {
    printf("ComponentAnsys123::ElectricField: Material %d, drift flag %d.\n",
           elements[imap].matmap, materials[elements[imap].matmap].driftmedium);
  }
  m = materials[elements[imap].matmap].medium;
  status = -5;
  if (materials[elements[imap].matmap].driftmedium) {
    if (m != 0) {
      if (m->IsDriftable()) status = 0;
    }
  }

}

bool 
ComponentAnsys123::GetMedium(const double xin, const double yin, const double zin, 
                             Medium*& m) {

  // Copy the coordinates
  double x = xin, y = yin, z = zin;

  // Map the coordinates onto field map coordinates
  bool xmirrored, ymirrored, zmirrored;
  double rcoordinate, rotation;
  MapCoordinates(x, y, z,
                 xmirrored, ymirrored, zmirrored,
                 rcoordinate, rotation);

  // Initial values
  m = NULL;

  // Do not proceed if not properly initialised.
  if (!ready) {
    printf("ComponentAnsys123::GetMedium: Field map not available for interpolation.\n");
    return false;
  }
  if (warning) {printf("ComponentAnsys123::GetMedium: Warnings have been issued for this field map.\n");}

  // Find the element that contains this point
  double t1, t2, t3, t4, jac[4][4], det;
  int imap = FindElement13(x, y, z, t1, t2, t3, t4, jac, det);
  if (imap < 0) {
    if (debug) {
      printf("ComponentAnsys123::GetMedium: Point (%g,%g,%g) not in the mesh.\n", x, y, z);      
    }
    return false;
  }
  if (elements[imap].matmap < 0 || elements[imap].matmap >= nMaterials ) {
    if (debug) {
      printf("ComponentAnsys123::GetMedium: Point (%g,%g) has out of range material number %d.\n", x, y, imap);
    }
    return false;
  }
  
  if (debug) {
    printf("ComponentAnsys123::ElectricField:\n");
    printf("    Global: (%g,%g,%g), Local: (%g,%g,%g,%g) in element %d\n",
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
  
  // Assign a medium
  m = materials[elements[imap].matmap].medium;
  return true;
    
}

}
