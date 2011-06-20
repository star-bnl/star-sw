// Copied and modified ComponentAnsys123.cc

#include <stdio.h>
#include <string.h>
#include <fstream>
#include <stdlib.h>
#include <math.h>

#include "ComponentElmer.hh"

namespace Garfield {

ComponentElmer::ComponentElmer() : ComponentFieldMap() {

  ready = false;

}

ComponentElmer::ComponentElmer(std::string header, std::string elist, std::string nlist,
                                     std::string mplist, std::string volt, 
                                     std::string unit) :
  ComponentFieldMap() {

  Initialise(header, elist, nlist, mplist, volt, unit);

}

bool
ComponentElmer::Initialise(std::string header, std::string elist,  std::string nlist,
	std::string mplist, std::string volt, std::string unit) {

  debug = false;
  ready = false;

  // Keep track of the success
  bool ok = true;

  // Buffer for reading
  const int size = 100;
  char line[size];

  printf("ComponentElmer::Initialise:\n");
  // Open the header
  std::ifstream fheader;
  fheader.open(header.c_str(), std::ios::in);
  if (fheader.fail()) {
    printf("ComponentElmer::Initialise:\n");
    printf("    Could not open header file %s for reading.\n",
           header.c_str());
  }

  // Temporary variables for use in file reading
  char* token = NULL;
  bool readerror = false;
  bool readstop = false;
  int il = 0;

  // Read the header to get the number of nodes and elements
  fheader.getline(line, size, '\n');
  token = strtok(line, " ");
  nNodes = ReadInteger(token, 0, readerror);
  token = strtok(NULL, " ");
  nElements = ReadInteger(token, 0, readerror);
  printf("Read %i nodes and %i elements from file %s.\n",
         nNodes, nElements, header.c_str());
  if (readerror) {
    printf("ComponentElmer::Initialise:\n");
    printf("    Error reading file %s (line %d).\n", header.c_str(), il);
    fheader.close();
    ok = false;
    return false;
  }

  // Close the header file
  fheader.close();

  // Open the nodes list
  std::ifstream fnodes;
  fnodes.open(nlist.c_str(), std::ios::in);
  if (fnodes.fail()) {
    printf("ComponentElmer::Initialise:\n");
    printf("    Could not open nodes file %s for reading.\n",
           nlist.c_str());
  }

  // Check the value of the unit
  double funit;
  if (strcmp(unit.c_str(),"mum") == 0 || strcmp(unit.c_str(),"micron") == 0 || 
      strcmp(unit.c_str(),"micrometer") == 0) {
    funit = 0.0001;
  } else if (strcmp(unit.c_str(),"mm") == 0 || 
             strcmp(unit.c_str(),"millimeter") == 0) {
    funit = 0.1;
  } else if (strcmp(unit.c_str(),"cm") == 0 || 
             strcmp(unit.c_str(),"centimeter") == 0) {
    funit = 1.0;
  } else if (strcmp(unit.c_str(),"m") == 0 || 
             strcmp(unit.c_str(),"meter") == 0) {
    funit = 100.0;
  } else {
    printf("ComponentElmer::Initialise:\n");
    printf("    Unknown length unit %s.\n", unit.c_str());
    ok = false;
    funit = 1.0;    
  }
  if (debug) {
    printf("ComponentElmer::Initialise:\n");
    printf("    Unit scaling factor = %g.\n", funit);
  }

  // Read the nodes from the file
  node newNode;
  for (il = 0; il < nNodes; il++) {

    // Get a line from the nodes file
    fnodes.getline(line, size, '\n');

    // Ignore the first two characters
    token = strtok(line, " ");
    token = strtok(NULL, " ");
    
    // Get the node coordinates
    token = strtok(NULL, " "); double xnode = ReadDouble(token, -1, readerror);
    token = strtok(NULL, " "); double ynode = ReadDouble(token, -1, readerror);
    token = strtok(NULL, " "); double znode = ReadDouble(token, -1, readerror);
    if (readerror) {
      printf("ComponentElmer::Initialise:\n");
      printf("    Error reading file %s (line %d).\n", nlist.c_str(), il);
      fnodes.close();
      ok = false;
      return false;
    }
    
    //if(il < 60) printf("Got node %i with x=%f, y=%f, z=%f and funit=%f.\n",il,xnode,ynode,znode,funit);

    // Set up and create a new node
    newNode.x = xnode * funit;
    newNode.y = ynode * funit;
    newNode.z = znode * funit;
    nodes.push_back(newNode);
  }

  // Close the nodes file
  fnodes.close();

  // Open the potential file
  std::ifstream fvolt;
  fvolt.open(volt.c_str(), std::ios::in);
  if (fvolt.fail()) {
    printf("ComponentElmer::Initialise:\n");
    printf("    Could not open result file %s for reading.\n",
           volt.c_str());
  }

  // Reset the line counter
  il = 1;

  // Read past the header
  while (!readstop && fvolt.getline(line, size, '\n')) {
    token = strtok(line, " ");
    if (strcmp(token, "Perm:") == 0) readstop = true;
    il++;
  }

  // Should have stopped: if not, print error message
  if (!readstop) {
    printf("ComponentElmer::Initialise:\n");
    printf("    Error reading past header of potentials file %s.\n", 
           volt.c_str());  
    fnodes.close();
    ok = false;
    return false;  
  }

  // Read past the permutation map (number of lines = nNodes)
  for (int tl = 0; tl < nNodes; tl++) { 
    fvolt.getline(line, size, '\n'); 
    il++;
  }

  // Read the potentials
  for (int tl = 0; tl < nNodes; tl++) {
    double v;
    fvolt.getline(line, size, '\n');
    token = strtok(line, " ");
    v = ReadDouble(token, -1, readerror);
    if(readerror) {
      printf("ComponentElmer::Initialise:\n");
      printf("    Error reading file %s (line %d).\n", volt.c_str(), il);
      fnodes.close();
      ok = false;
      return false;  
    }
    // Place the voltage in its appropriate node
    nodes[tl].v = v;
  }

  // Close the potentials file
  fvolt.close();

  // Open the materials file
  std::ifstream fmplist;
  fmplist.open(mplist.c_str(), std::ios::in);
  if (fmplist.fail()) {
    printf("ComponentElmer::Initialise:\n");
    printf("    Could not open result file %s for reading.\n",
           mplist.c_str());
  }

  // Read the dielectric constants from the materials file
  fmplist.getline(line, size, '\n');
  token = strtok(line, " ");
  if (readerror) {
    printf("ComponentElmer::Initialise:\n");
    printf("    Error reading number of materials from %s.\n", 
           mplist.c_str());
    fmplist.close();
    ok = false;
    return false;
  }
  nMaterials = ReadInteger(token, 0, readerror);
  materials.resize(nMaterials);
  for (int i = 0; i < nMaterials; ++i) {
    materials[i].ohm = -1;
    materials[i].eps = -1;
    materials[i].medium = NULL;
  }
  for (il = 2; il < (nMaterials + 2); il++) {
    fmplist.getline(line, size, '\n');
    token = strtok(line, " "); ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); double dc = ReadDouble(token, -1.0, readerror);
    if (readerror) {
      printf("ComponentElmer::Initialise:\n");
      printf("    Error reading file %s (line %d).\n", mplist.c_str(), il);
      fmplist.close();
      ok = false;
      return false;
    } 
    materials[il - 2].eps = dc;
    printf("ComponentElmer::Initialise:\n");
    printf("    Set material %i of %i to eps %f.\n", il - 2, nMaterials, dc);
  }

  // Close the materials file
  fmplist.close();

  // Find the lowest epsilon, check for eps = 0, set default drift media
  double epsmin = -1; int iepsmin = -1;
  for (int imat = 0; imat < nMaterials; ++imat) {
    if (materials[imat].eps < 0) continue;
    if (materials[imat].eps == 0) {
      printf("ComponentElmer::Initialise:\n");
      printf("    Material %d has been assigned a permittivity\n", imat);
      printf("    equal to zero in %s.\n", mplist.c_str());
      ok = false;
    } else if (iepsmin < 0 || epsmin > materials[imat].eps) {
      epsmin = materials[imat].eps;
      iepsmin = imat;
    }
  }
  
  if (iepsmin < 0) {
    printf("ComponentElmer::Initialise:\n");
    printf("    No material with positive permittivity found \n");
    printf("    in material list %s.\n", mplist.c_str());
    ok = false;
  } else {
    for (int imat = 0; imat < nMaterials; ++imat) {
      if (imat == iepsmin) {
        materials[imat].driftmedium = true;
      } else {
        materials[imat].driftmedium = false;
      }
    }
  }

  // Open the elements file
  std::ifstream felems;
  felems.open(elist.c_str(), std::ios::in);
  if (felems.fail()) {
    printf("ComponentElmer::Initialise:\n");
    printf("    Could not open result file %s for reading.\n",
           elist.c_str());
  }

  // Read the elements and their material indices
  elements.clear();  
  int highestnode = 0;
  int nbackground = 0;
  element newElement;
  for (il = 0; il < nElements; il++)	{

    // Get a line
    felems.getline(line, size, '\n');

    // Split into tokens
    token = strtok(line, " ");
    // Read the 2nd-order element
    // Note: Ordering of Elmer elements can be described in the 
    // ElmerSolver manual (appendix D. at the time of this comment)
    // If the order read below is compared to the shape functions used 
    // eg. in ElectricField, the order is wrong, but note at the
    // end of this function the order of elements 5,6,7 will change to 
    // 7,5,6 when actually recorded in newElement.emap to correct for this
    token = strtok(NULL, " "); int imat = ReadInteger(token, -1, readerror) - 1;
    token = strtok(NULL, " ");
    token = strtok(NULL, " "); int in0 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in1 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in2 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in3 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in4 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in5 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in6 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in7 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in8 = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); int in9 = ReadInteger(token, -1, readerror);
   
    if (debug && il < 10) {
      printf("Read nodes %i, %i, %i, %i... from element %i of %i with mat %i.\n",
             in0,in1,in2,in3,il+1,nElements,imat);
    }

    // Check synchronisation
    if (readerror) {
      printf("ComponentElmer::Initialise:\n");
      printf("    Error reading file %s (line %d).\n", elist.c_str(), il);
      felems.close();
      ok = false;
      return false;
    } 
    
    // Check the material number and ensure that epsilon is non-negative
    if (imat < 0 || imat > nMaterials) {
       printf("ComponentElmer::Initialise:\n");
       printf("    Out-of-range material number on file %s (line %d).\n", 
              elist.c_str(), il);
      printf("    Element: %d, material: %d,\n", il, imat);
      printf("    nodes: (%d %d %d %d %d %d %d %d %d %d)\n",
             in0, in1, in2, in3, in4, in5, in6, in7, in8, in9);
      ok = false;
    }
    if (materials[imat].eps < 0) {
      printf("ComponentElmer::Initialise:\n");
      printf("    Element %d in element list %s \n", il, elist.c_str());
      printf("    uses material %d which has not been assigned\n", imat);
      printf("    a positive permittivity in material list %s.\n", 
             mplist.c_str());
      ok = false;
    }

    // Check the node numbers
    if (in0 < 1 || in1 < 1 || in2 < 1 || in3 < 1 || in4 < 1 || 
        in5 < 1 || in6 < 1 || in7 < 1 || in8 < 1 || in9 < 1) {
      printf("ComponentElmer::Initialise:\n");
      printf("    Found a node number < 1 on file %s (line %d).\n",
             elist.c_str(), il);
      printf("    Element: %d, material: %d,\n", il, imat);
      printf("    nodes: (%d %d %d %d %d %d %d %d %d %d).\n",
             in0, in1, in2, in3, in4, in5, in6, in7, in8, in9);
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
    if (in0 == in1 || in0 == in2 || in0 == in3 || in0 == in4 || in0 == in5 || 
        in0 == in6 || in0 == in7 || in0 == in8 || in0 == in9 || 
        in1 == in2 || in1 == in3 || in1 == in4 || in1 == in5 || 
        in1 == in6 || in1 == in7 || in1 == in8 || in1 == in9 || 
        in2 == in3 || in2 == in4 || in2 == in5 || in2 == in6 || 
        in2 == in7 || in2 == in8 || in2 == in9 || 
        in3 == in4 || in3 == in5 || in3 == in6 || 
        in3 == in7 || in3 == in8 || in3 == in9 || 
        in4 == in5 || in4 == in6 || in4 == in7 || in4 == in8 || in4 == in9 || 
        in5 == in6 || in5 == in7 || in5 == in8 || in5 == in9 || 
        in6 == in7 || in6 == in8 || in6 == in9 || 
        in7 == in8 || in7 == in9 || 
        in8 == in9) {
      printf("ComponentElmer::Initialise:\n");
      printf("    Element %d of file %s is degenerate,\n", 
             il, elist.c_str());
      printf("    no such elements allowed in this type of map.\n");
      ok = false;
    }
    
    newElement.degenerate = false;
    
    // Store the material reference
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

  // Close the elements file
  felems.close();

  // Set the ready flag
  if (ok) {
    ready = true;
  } else {
    printf("ComponentElmer::Initialise:\n");
    printf("    Field map could not be read and can not be interpolated.\n");
    return false;
  }

  printf("END ComponentElmer::Initialise:\n");

  // Establish the ranges
  SetRange();
  UpdatePeriodicity();
  return true;

}

bool
ComponentElmer::SetWeightingField(std::string prnsol, std::string label) {

 if (!ready) {
    printf("ComponentElmer::SetWeightingField:\n");
    printf("    No valid field map is present.\n");
    printf("    Weighting field cannot be added.\n");
    return false;
  }

  // Open the voltage list
  std::ifstream fprnsol;
  fprnsol.open(prnsol.c_str(), std::ios::in);
  if (fprnsol.fail()) {
    printf("ComponentElmer::SetWeightingField:\n");
    printf("    Could not open potential file %s for reading.\n",
           prnsol.c_str());
    printf("    The file perhaps does not exist.\n");
    return false;
  }

  // Buffer for reading
  const int size = 100;
  char line[size];

  bool ok = true;
  // Read the voltage list
  int il = 0;
  int nread = 0;
  bool readerror = false;

  while (fprnsol.getline(line, size, '\n')) {
    il++;
    // Split the line in tokens
    char* token = NULL;
    token = strtok(line, " ");
    // Skip blank lines and headers
    if (!token || strcmp(token," ") == 0 || strcmp(token,"\n") == 0 || 
        int(token[0]) == 10 || int(token[0]) == 13 ||
        strcmp(token,"PRINT")   == 0 || strcmp(token,"*****")   == 0 ||
        strcmp(token,"LOAD")    == 0 || strcmp(token,"TIME=")   == 0 ||
        strcmp(token,"MAXIMUM") == 0 || strcmp(token,"VALUE")   == 0 ||
        strcmp(token,"NODE")    == 0) continue;
    // Read the element
    int inode = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " "); double volt = ReadDouble(token, -1, readerror);
    // Check syntax
    if (readerror) {
      printf("ComponentElmer::SetWeightingField:\n");
      printf("    Error reading file %s (line %d).\n", prnsol.c_str(), il);
      fprnsol.close();
      return false;
    }
    // Check node number and store if OK
    if (inode < 1 || inode > nNodes) {
      printf("ComponentElmer::SetWeightingField:\n");
      printf("    Node number %d out of range\n", inode);
      printf("    on potential file %s (line %d).\n", prnsol.c_str(), il);
      ok = false;
    } else {
      nodes[inode - 1].w = volt;
      nread++;
    }
  }
  // Close the file
  fprnsol.close();
  
  // Tell how many lines read
  printf("ComponentElmer::SetWeightingField:\n");
  printf("    Read %d potentials from file %s.\n", nread, prnsol.c_str());
  // Check number of nodes
  if (nread != nNodes) {
    printf("ComponentElmer::SetWeightingField:\n");
    printf("    Number of nodes read (%d) on potential file %s \n", 
           nread, prnsol.c_str());
    printf("    does not match the node list (%d).\n", nNodes);
    ok = false;
  }

  // Set the ready flag
  if (!ok) {
    printf("ComponentElmer::SetWeightingField:\n");
    printf("    Field map could not be read and can not be interpolated.\n");
    return false;
  }
  
  hasWeightingField = true;
  wfield = label;
  return true;

}

void 
ComponentElmer::ElectricField(const double x, const double y, const double z,
                                 double& ex, double& ey, double& ez,
                                 Medium*& m, int& status) {

  double v = 0.;
  ElectricField(x, y, z, ex, ey, ez, v, m, status);

}

void 
ComponentElmer::ElectricField(
                   const double xin, const double yin, const double zin,
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
  ex = ey = ez = volt = 0.;
  status = 0;
  m = 0;
  
  // Do not proceed if not properly initialised.
  if (!ready) {
    status = -10;
    printf("ComponentElmer::ElectricField:\n");
    printf("    Field map not available for interpolation.\n");
    return;
  }
  
  if (warning) {
    printf("ComponentElmer::ElectricField:\n");
    printf("    Warnings have been issued for this field map.\n");
  }

  // Find the element that contains this point
  double t1, t2, t3, t4, jac[4][4], det;
  int imap = FindElement13(x, y, z, t1, t2, t3, t4, jac, det);
  if (imap < 0) {
    if (debug) {
      printf("ComponentElmer::ElectricField:\n");
      printf("    Point (%g,%g,%g) not in the mesh.\n", x, y, z);
    }
    status = -6;
    return;
  }
  
  if (debug) {
    printf("ComponentElmer::ElectricField:\n");
    printf("    Global: (%g,%g,%g), Local: (%g,%g,%g,%g) in element %d\n",
           x, y, z, t1, t2, t3, t4, imap);
    printf("      Node             x            y            z            V\n");
    for (int i = 0; i < 10; i++) {
      printf("      %-5d %12g %12g %12g %12g\n",
             elements[imap].emap[i],
             nodes[elements[imap].emap[i]].x,
             nodes[elements[imap].emap[i]].y,
             nodes[elements[imap].emap[i]].z,
             nodes[elements[imap].emap[i]].v);
    }
  }

  // Tetrahedral field
  volt =
        nodes[elements[imap].emap[0]].v * t1 * (2 * t1 - 1) + 
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
         nodes[elements[imap].emap[4]].v * (4 * t2 * jac[0][1] + 
                                               4 * t1 * jac[1][1]) + 
         nodes[elements[imap].emap[5]].v * (4 * t3 * jac[0][1] + 
                                               4 * t1 * jac[2][1]) + 
         nodes[elements[imap].emap[6]].v * (4 * t4 * jac[0][1] + 
                                               4 * t1 * jac[3][1]) + 
         nodes[elements[imap].emap[7]].v * (4 * t3 * jac[1][1] + 
                                               4 * t2 * jac[2][1]) + 
         nodes[elements[imap].emap[8]].v * (4 * t4 * jac[1][1] + 
                                               4 * t2 * jac[3][1]) + 
         nodes[elements[imap].emap[9]].v * (4 * t4 * jac[2][1] + 
                                               4 * t3 * jac[3][1])) / det;
  ey = -(nodes[elements[imap].emap[0]].v * (4 * t1 - 1) * jac[0][2] + 
         nodes[elements[imap].emap[1]].v * (4 * t2 - 1) * jac[1][2] + 
         nodes[elements[imap].emap[2]].v * (4 * t3 - 1) * jac[2][2] + 
         nodes[elements[imap].emap[3]].v * (4 * t4 - 1) * jac[3][2] + 
         nodes[elements[imap].emap[4]].v * (4 * t2 * jac[0][2] + 
                                               4 * t1 * jac[1][2]) + 
         nodes[elements[imap].emap[5]].v * (4 * t3 * jac[0][2] + 
                                               4 * t1 * jac[2][2]) + 
         nodes[elements[imap].emap[6]].v * (4 * t4 * jac[0][2] + 
                                               4 * t1 * jac[3][2]) + 
         nodes[elements[imap].emap[7]].v * (4 * t3 * jac[1][2] +
                                               4 * t2 * jac[2][2]) + 
         nodes[elements[imap].emap[8]].v * (4 * t4 * jac[1][2] + 
                                               4 * t2 * jac[3][2]) + 
         nodes[elements[imap].emap[9]].v * (4 * t4 * jac[2][2] + 
                                               4 * t3 * jac[3][2])) / det;
  ez = -(nodes[elements[imap].emap[0]].v * (4 * t1 - 1) * jac[0][3] + 
         nodes[elements[imap].emap[1]].v * (4 * t2 - 1) * jac[1][3] + 
         nodes[elements[imap].emap[2]].v * (4 * t3 - 1) * jac[2][3] + 
         nodes[elements[imap].emap[3]].v * (4 * t4 - 1) * jac[3][3] + 
         nodes[elements[imap].emap[4]].v * (4 * t2 * jac[0][3] + 
                                               4 * t1 * jac[1][3]) + 
         nodes[elements[imap].emap[5]].v * (4 * t3 * jac[0][3] + 
                                               4 * t1 * jac[2][3]) + 
         nodes[elements[imap].emap[6]].v * (4 * t4 * jac[0][3] + 
                                               4 * t1 * jac[3][3]) + 
         nodes[elements[imap].emap[7]].v * (4 * t3 * jac[1][3] +
                                               4 * t2 * jac[2][3]) + 
         nodes[elements[imap].emap[8]].v * (4 * t4 * jac[1][3] + 
                                               4 * t2 * jac[3][3]) + 
         nodes[elements[imap].emap[9]].v * (4 * t4 * jac[2][3] + 
                                               4 * t3 * jac[3][3])) / det;

  // Transform field to global coordinates
  UnmapFields(ex, ey, ez,
              x, y, z,
              xmirrored, ymirrored, zmirrored,
              rcoordinate, rotation);

  // Drift medium ?
  if (debug) {
    printf("ComponentElmer::ElectricField:\n");
    printf("    Material %d, drift flag %d.\n",
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

void
ComponentElmer::WeightingField(
                    const double xin, const double yin, const double zin,
                    double& wx, double& wy, double& wz,
                    const std::string label) {

  // Initial values
  wx = wy = wz = 0.;
  
  // Do not proceed if not properly initialised.
  if (!ready || !hasWeightingField || label != wfield) return;

  // Copy the coordinates
  double x = xin, y = yin, z = zin;

  // Map the coordinates onto field map coordinates
  bool xmirrored, ymirrored, zmirrored;
  double rcoordinate, rotation;
  MapCoordinates(x, y, z, 
                 xmirrored, ymirrored, zmirrored,
                 rcoordinate, rotation);
  
  if (warning) {
    printf("ComponentElmer::WeightingField:\n");
    printf("    Warnings have been issued for this field map.\n");
  }

  // Find the element that contains this point
  double t1, t2, t3, t4, jac[4][4], det;
  int imap = FindElement13(x, y, z, t1, t2, t3, t4, jac, det);
  // Check if the point is in the mesh
  if (imap < 0) return;
  
  if (debug) {
    printf("ComponentElmer::WeightingField:\n");
    printf("    Global: (%g,%g,%g),\n", x, y, z);
    printf("    Local: (%g,%g,%g,%g) in element %d\n",
           t1, t2, t3, t4, imap);
    printf("      Node             x            y            z            V\n");
    for (int i = 0; i < 10; i++) {
      printf("      %-5d %12g %12g %12g %12g\n",
             elements[imap].emap[i],
             nodes[elements[imap].emap[i]].x,
             nodes[elements[imap].emap[i]].y,
             nodes[elements[imap].emap[i]].z,
             nodes[elements[imap].emap[i]].w);
    }
  }

  // Tetrahedral field
  wx = -(nodes[elements[imap].emap[0]].w * (4 * t1 - 1) * jac[0][1] + 
         nodes[elements[imap].emap[1]].w * (4 * t2 - 1) * jac[1][1] + 
         nodes[elements[imap].emap[2]].w * (4 * t3 - 1) * jac[2][1] + 
         nodes[elements[imap].emap[3]].w * (4 * t4 - 1) * jac[3][1] + 
         nodes[elements[imap].emap[4]].w * (4 * t2 * jac[0][1] + 
                                               4 * t1 * jac[1][1]) + 
         nodes[elements[imap].emap[5]].w * (4 * t3 * jac[0][1] + 
                                               4 * t1 * jac[2][1]) + 
         nodes[elements[imap].emap[6]].w * (4 * t4 * jac[0][1] + 
                                               4 * t1 * jac[3][1]) + 
         nodes[elements[imap].emap[7]].w * (4 * t3 * jac[1][1] + 
                                               4 * t2 * jac[2][1]) + 
         nodes[elements[imap].emap[8]].w * (4 * t4 * jac[1][1] + 
                                               4 * t2 * jac[3][1]) + 
         nodes[elements[imap].emap[9]].w * (4 * t4 * jac[2][1] + 
                                               4 * t3 * jac[3][1])) / det;
  wy = -(nodes[elements[imap].emap[0]].w * (4 * t1 - 1) * jac[0][2] + 
         nodes[elements[imap].emap[1]].w * (4 * t2 - 1) * jac[1][2] + 
         nodes[elements[imap].emap[2]].w * (4 * t3 - 1) * jac[2][2] + 
         nodes[elements[imap].emap[3]].w * (4 * t4 - 1) * jac[3][2] + 
         nodes[elements[imap].emap[4]].w * (4 * t2 * jac[0][2] + 
                                               4 * t1 * jac[1][2]) + 
         nodes[elements[imap].emap[5]].w * (4 * t3 * jac[0][2] + 
                                               4 * t1 * jac[2][2]) + 
         nodes[elements[imap].emap[6]].w * (4 * t4 * jac[0][2] + 
                                               4 * t1 * jac[3][2]) + 
         nodes[elements[imap].emap[7]].w * (4 * t3 * jac[1][2] + 
                                               4 * t2 * jac[2][2]) + 
         nodes[elements[imap].emap[8]].w * (4 * t4 * jac[1][2] + 
                                               4 * t2 * jac[3][2]) + 
         nodes[elements[imap].emap[9]].w * (4 * t4 * jac[2][2] + 
                                               4 * t3 * jac[3][2])) / det;
  wz = -(nodes[elements[imap].emap[0]].w * (4 * t1 - 1) * jac[0][3] + 
         nodes[elements[imap].emap[1]].w * (4 * t2 - 1) * jac[1][3] + 
         nodes[elements[imap].emap[2]].w * (4 * t3 - 1) * jac[2][3] + 
         nodes[elements[imap].emap[3]].w * (4 * t4 - 1) * jac[3][3] + 
         nodes[elements[imap].emap[4]].w * (4 * t2 * jac[0][3] + 
                                               4 * t1 * jac[1][3]) + 
         nodes[elements[imap].emap[5]].w * (4 * t3 * jac[0][3] + 
                                               4 * t1 * jac[2][3]) + 
         nodes[elements[imap].emap[6]].w * (4 * t4 * jac[0][3] + 
                                               4 * t1 * jac[3][3]) + 
         nodes[elements[imap].emap[7]].w * (4 * t3 * jac[1][3] + 
                                               4 * t2 * jac[2][3]) + 
         nodes[elements[imap].emap[8]].w * (4 * t4 * jac[1][3] + 
                                               4 * t2 * jac[3][3]) + 
         nodes[elements[imap].emap[9]].w * (4 * t4 * jac[2][3] + 
                                               4 * t3 * jac[3][3])) / det;

  // Transform field to global coordinates
  UnmapFields(wx, wy, wz,
              x, y, z,
              xmirrored, ymirrored, zmirrored,
              rcoordinate, rotation);
	      
}

double
ComponentElmer::WeightingPotential(
                    const double xin, const double yin, const double zin,
                    const std::string label) {

  // Do not proceed if not properly initialised.
  if (!ready || !hasWeightingField || label != wfield) return 0.;

  // Copy the coordinates
  double x = xin, y = yin, z = zin;

  // Map the coordinates onto field map coordinates
  bool xmirrored, ymirrored, zmirrored;
  double rcoordinate, rotation;
  MapCoordinates(x, y, z,
                 xmirrored, ymirrored, zmirrored,
                 rcoordinate, rotation);

  if (warning) {
    printf("ComponentElmer::WeightingPotential:\n");
    printf("    Warnings have been issued for this field map.\n");
  } 
   
  // Find the element that contains this point
  double t1, t2, t3, t4, jac[4][4], det;
  int imap = FindElement13(x, y, z, t1, t2, t3, t4, jac, det);
  if (imap < 0) return 0.;
  
  if (debug) {
    printf("ComponentElmer::WeightingPotential:\n");
    printf("    Global: (%g,%g,%g),\n", x, y, z);
    printf("    Local: (%g,%g,%g,%g) in element %d\n",
           t1, t2, t3, t4, imap);
    printf("      Node             x            y            z            V\n");
    for (int i = 0; i < 10; i++) {
      printf("      %-5d %12g %12g %12g %12g\n",
             elements[imap].emap[i],
             nodes[elements[imap].emap[i]].x,
             nodes[elements[imap].emap[i]].y,
             nodes[elements[imap].emap[i]].z,
             nodes[elements[imap].emap[i]].v);
    }
  }

  // Tetrahedral field
  return nodes[elements[imap].emap[0]].v * t1 * (2 * t1 - 1) + 
         nodes[elements[imap].emap[1]].v * t2 * (2 * t2 - 1) + 
         nodes[elements[imap].emap[2]].v * t3 * (2 * t3 - 1) + 
         nodes[elements[imap].emap[3]].v * t4 * (2 * t4 - 1) + 
     4 * nodes[elements[imap].emap[4]].v * t1 * t2 +
     4 * nodes[elements[imap].emap[5]].v * t1 * t3 + 
     4 * nodes[elements[imap].emap[6]].v * t1 * t4 + 
     4 * nodes[elements[imap].emap[7]].v * t2 * t3 + 
     4 * nodes[elements[imap].emap[8]].v * t2 * t4 +  
     4 * nodes[elements[imap].emap[9]].v * t3 * t4;

}

bool 
ComponentElmer::GetMedium(
            const double xin, const double yin, const double zin, 
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
  m = 0;

  // Do not proceed if not properly initialised.
  if (!ready) {
    printf("ComponentElmer::GetMedium:\n");
    printf("    Field map not available for interpolation.\n");
    return false;
  }
  if (warning) {
    printf("ComponentElmer::GetMedium:\n");
    printf("    Warnings have been issued for this field map.\n");
  }

  // Find the element that contains this point
  double t1, t2, t3, t4, jac[4][4], det;
  int imap = FindElement13(x, y, z, t1, t2, t3, t4, jac, det);
  if (imap < 0) {
    if (debug) {
      printf("ComponentElmer::GetMedium:\n");
      printf("    Point (%g,%g,%g) not in the mesh.\n", x, y, z);      
    }
    return false;
  }
  if (elements[imap].matmap < 0 || elements[imap].matmap >= nMaterials ) {
    if (debug) {
      printf("ComponentElmer::GetMedium:\n");
      printf("    Point (%g,%g) has out of range material number %d.\n", 
             x, y, imap);
    }
    return false;
  }
  
  if (debug) {
    printf("ComponentElmer::ElectricField:\n");
    printf("    Global: (%g,%g,%g),\n", x, y, z);
    printf("    Local: (%g,%g,%g,%g) in element %d\n",
           t1, t2, t3, t4, imap);
    printf("      Node             x            y            z            V\n");
    for (int ii = 0; ii < 10; ++ii) {
      printf("      %-5d %12g %12g %12g %12g\n",
             elements[imap].emap[ii],
             nodes[elements[imap].emap[ii]].x,
             nodes[elements[imap].emap[ii]].y,
             nodes[elements[imap].emap[ii]].z,
             nodes[elements[imap].emap[ii]].v);
    }
  }
  
  // Assign a medium
  m = materials[elements[imap].matmap].medium;
  return true;
    
}

double
ComponentElmer::GetElementVolume(const int i) {

  if (i < 0 || i >= nElements) return 0.;
  
  // Uses formula V = |a (dot) b x c|/6 
  // with a => "3", b => "1", c => "2" and origin "0"
  const double vol = fabs(
    (nodes[elements[i].emap[3]].x - nodes[elements[i].emap[0]].x) * (
      (nodes[elements[i].emap[1]].y - nodes[elements[i].emap[0]].y) * 
      (nodes[elements[i].emap[2]].z - nodes[elements[i].emap[0]].z) - 
      (nodes[elements[i].emap[2]].y - nodes[elements[i].emap[0]].y) * 
      (nodes[elements[i].emap[1]].z - nodes[elements[i].emap[0]].z)) +
    (nodes[elements[i].emap[3]].y - nodes[elements[i].emap[0]].y) * (
      (nodes[elements[i].emap[1]].z - nodes[elements[i].emap[0]].z) * 
      (nodes[elements[i].emap[2]].x - nodes[elements[i].emap[0]].x) -
      (nodes[elements[i].emap[2]].z - nodes[elements[i].emap[0]].z) * 
      (nodes[elements[i].emap[1]].x - nodes[elements[i].emap[0]].x)) +
    (nodes[elements[i].emap[3]].z - nodes[elements[i].emap[0]].z) * (
      (nodes[elements[i].emap[1]].x - nodes[elements[i].emap[0]].x) * 
      (nodes[elements[i].emap[2]].y - nodes[elements[i].emap[0]].y) -
      (nodes[elements[i].emap[3]].x - nodes[elements[i].emap[0]].x) * 
      (nodes[elements[i].emap[1]].y - nodes[elements[i].emap[0]].y))) / 6.;
  return vol;

}

void
ComponentElmer::GetAspectRatio(const int i, double& dmin, double& dmax) {

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

} // namespace Garfield
