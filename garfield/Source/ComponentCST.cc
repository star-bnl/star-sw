// Copied and modified ComponentAnsys123.cc
#include <stdio.h>
#include <string.h>
#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <math.h>
#include <algorithm>
#include <functional>
#include <vector>
#include <iomanip>
#include <sys/stat.h>
#include <sstream>

#include "TMath.h"
#include "ComponentCST.hh"

namespace Garfield {

ComponentCST::ComponentCST() : ComponentFieldMap() {

  m_className = "ComponentCST";
  m_ready = false;
  // Default bounding box
  zMinBoundingBox = -50.;
  zMaxBoundingBox = 50.;
  m_xlines.clear();
  m_ylines.clear();
  m_zlines.clear();
  deleteBackground = false;
  disableFieldComponent[0] = false;
  disableFieldComponent[1] = false;
  disableFieldComponent[2] = false;
  doShaping = false;
}

bool ComponentCST::Initialise(std::string elist, std::string nlist,
                              std::string mplist, std::string prnsol,
                              std::string unit) {
  m_ready = false;

//Keep track of the success
  bool ok = true;

  // Buffer for reading
  const int size = 200;
  char line[size];
  // Open the material list
  std::ifstream fmplist;
  fmplist.open(mplist.c_str(), std::ios::in);
  if (fmplist.fail()) {
    std::cerr << m_className << "::Initialise:" << std::endl;
    std::cerr << "    Could not open material file " << mplist
              << " for reading." << std::endl,
        std::cerr << "    The file perhaps does not exist." << std::endl;
    return false;
  }

  // Read the material list
  m_nMaterials = 0;
  int il = 0;
  bool readerror = false;
  while (fmplist.getline(line, size, '\n')) {
    il++;
    // Split the line in tokens
    char* token = NULL;
    token = strtok(line, " ");
    // Skip blank lines and headers
    if (!token || strcmp(token, " ") == 0 || strcmp(token, "\n") == 0 ||
        int(token[0]) == 10 || int(token[0]) == 13)
      continue;
    // Read number of materials,
    // ensure it does not exceed the maximum and initialize the list
    if (strcmp(token, "Materials") == 0) {
      token = strtok(NULL, " ");
      m_nMaterials = ReadInteger(token, -1, readerror);
      if (readerror) {
        std::cerr << m_className << "::Initialise:" << std::endl;
        std::cerr << "    Error reading file " << mplist << " (line " << il
                  << ")." << std::endl;
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
        std::cout << m_className << "::Initialise:" << std::endl;
        std::cout << "    Number of materials: " << m_nMaterials << ""
                  << std::endl;
      }
    } else if (strcmp(token, "Material") == 0) {
      token = strtok(NULL, " ");
      int imat = ReadInteger(token, -1, readerror);
      if (readerror) {
        std::cerr << m_className << "::Initialise:" << std::endl;
        std::cerr << "     Error reading file " << mplist << " (line " << il
                  << "." << std::endl;
        fmplist.close();
        ok = false;
        return false;
      } else if (imat < 1 || imat > (int)m_nMaterials) {
        std::cerr << m_className << "::Initialise:" << std::endl;
        std::cerr << "    Found out-of-range material index " << imat << "in"
                  << std::endl;
        std::cerr << "    material properties file " << mplist << "."
                  << std::endl;
        ok = false;
      } else {
        token = strtok(NULL, " ");
        int itype = 0;
        if (strcmp(token, "PERX") == 0) {
          itype = 1;
        } else if (strcmp(token, "RSVX") == 0) {
          itype = 2;
        } else {
          std::cerr << m_className << "::Initialise:" << std::endl;
          std::cerr << "    Found unknown material property flag " << token
                    << "" << std::endl;
          std::cerr << "    on material properties file " << mplist << "(line "
                    << il << ")." << std::endl;
          ok = false;
        }
        token = strtok(NULL, " ");
        if (itype == 1) {
          materials[imat - 1].eps = ReadDouble(token, -1, readerror);
        } else if (itype == 2) {
          materials[imat - 1].ohm = ReadDouble(token, -1, readerror);
          token = strtok(NULL, " ");
          if (strcmp(token, "PERX") != 0) {
            std::cerr << m_className << "::Initialise:" << std::endl;
            std::cerr << "   Found unknown material property falg " << token
                      << "" << std::endl;
            std::cerr << "   on material file " << mplist << " (material "
                      << imat << ").\n)";
            ok = false;
          } else {
            token = strtok(NULL, " ");
            materials[imat - 1].eps = ReadDouble(token, -1, readerror);
          }
        }
        if (readerror) {
          std::cerr << m_className << "::Initialise:" << std::endl;
          std::cerr << "     Error reading file " << mplist << "(line " << il
                    << ")." << std::endl;
          fmplist.close();
          ok = false;
          return false;
        }
        if (m_debug) {
          std::cout << m_className << "::Initialise:" << std::endl;
          std::cout << "    Read material properties for material "
                    << (imat - 1) << "" << std::endl;
          if (itype == 2) {
            std::cout << "    eps = " << materials[imat - 1].eps
                      << " ohm = " << materials[imat - 1].ohm << ""
                      << std::endl;
          } else {
            std::cout << "    eps = " << materials[imat - 1].eps << ""
                      << std::endl;
          }
        }
      }
    }
  }
  // Close the file
  fmplist.close();
  // Find the lowest epsilon, check for eps = 0, set default drift media
  double epsmin = -1.;
  unsigned int iepsmin = 0;
  for (unsigned int imat = 0; imat < m_nMaterials; ++imat) {
    if (materials[imat].eps < 0) continue;
    if (materials[imat].eps == 0) {
      std::cout << m_className << "::Initialise:" << std::endl;
      std::cout << "    Material " << imat
                << " has been assigned a permittivity" << std::endl;
      std::cout << "    equal to zero in " << mplist << "." << std::endl;
      ok = false;
    } else if (epsmin < 0. || epsmin > materials[imat].eps) {
      epsmin = materials[imat].eps;
      iepsmin = imat;
    }
  }
  if (epsmin < 0.) {
    std::cerr << m_className << "::Initialise:" << std::endl;
    std::cerr << "     No material with positive permittivity found in"
              << std::endl;
    std::cerr << "     material list " << mplist.c_str() << "." << std::endl;
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
  std::cout << m_className << "::Initialise:" << std::endl;
  std::cout << "    Read properties of " << m_nMaterials << " materials"
            << std::endl;
  std::cout << "    from file " << mplist << "." << std::endl;
  if (m_debug) PrintMaterials();

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
    std::cerr << m_className << "::Initialise:" << std::endl;
    std::cerr << "    Unknown length unit " << unit << "." << std::endl;
    ok = false;
    funit = 1.0;
  }
  if (m_debug) {
    std::cout << m_className << "::Initialise:" << std::endl;
    std::cout << "    Unit scaling factor = " << funit << "." << std::endl;
  }

  // Open the node list
  std::ifstream fnlist;
  fnlist.open(nlist.c_str(), std::ios::in);
  if (fnlist.fail()) {
    std::cerr << m_className << "::Initialise:" << std::endl;
    std::cerr << "    Could not open nodes file " << nlist << " for reading."
              << std::endl;
    std::cerr << "    The file perhaps does not exist." << std::endl;
    return false;
  }
  // Read the node list
  nNodes = 0;
  il = 0;
  int xlines = 0, ylines = 0, zlines = 0;
  int lines_type = -1;
  double line_tmp;
  while (fnlist.getline(line, size, '\n')) {
    il++;
    // Split the line in tokens
    char* token = NULL;
    // Split into tokens
    token = strtok(line, " ");
    // Skip blank lines and headers
    if (!token || strcmp(token, " ") == 0 || strcmp(token, "\n") == 0 ||
        int(token[0]) == 10 || int(token[0]) == 13)
      continue;
    // Read max sizes
    if (strcmp(token, "xmax") == 0) {
      token = strtok(NULL, " ");
      xlines = ReadInteger(token, -1, readerror);
      token = strtok(NULL, " ");
      token = strtok(NULL, " ");
      ylines = ReadInteger(token, -1, readerror);
      token = strtok(NULL, " ");
      token = strtok(NULL, " ");
      zlines = ReadInteger(token, -1, readerror);
      if (readerror) break;
      continue;
    }
    if (strcmp(token, "x-lines\n") == 0 || strcmp(token, "x-lines") == 0) {
      lines_type = 1;
      if (m_debug) {
        std::cout << m_className << "::Initialise:" << std::endl;
        std::cout << "    Reading x-lines from file  " << nlist << "."
                  << std::endl;
      }
      continue;
    }
    if (strcmp(token, "y-lines\n") == 0 || strcmp(token, "y-lines") == 0) {
      lines_type = 2;
      if (m_debug) {
        std::cout << m_className << "::Initialise:" << std::endl;
        std::cout << "    Reading y-lines from file  " << nlist << "."
                  << std::endl;
      }
      continue;
    }
    if (strcmp(token, "z-lines\n") == 0 || strcmp(token, "z-lines") == 0) {
      lines_type = 3;
      if (m_debug) {
        std::cout << m_className << "::Initialise:" << std::endl;
        std::cout << "    Reading z-lines from file  " << nlist << "."
                  << std::endl;
      }
      continue;
    }
    line_tmp = ReadDouble(token, -1, readerror);
    if (lines_type == 1)
      m_xlines.push_back(line_tmp * funit);
    else if (lines_type == 2)
      m_ylines.push_back(line_tmp * funit);
    else if (lines_type == 3)
      m_zlines.push_back(line_tmp * funit);
    else {
      std::cerr << m_className << "::Initialise:" << std::endl;
      std::cerr << "    Line type was not set in  " << nlist << " (line " << il
                << ", token = " << token << ")." << std::endl;
      std::cerr << "    Maybe it is in the wrong format" << std::endl;
      std::cerr << "    e.g. missing tailing space after x-lines." << std::endl;
      ok = false;
      break;
    }
    if (readerror) break;
  }
  // Check syntax
  if (readerror) {
    std::cerr << m_className << "::Initialise:" << std::endl;
    std::cerr << "    Error reading file " << nlist << " (line " << il << ")."
              << std::endl;
    fnlist.close();
    ok = false;
    return false;
  }
  // Close the file
  fnlist.close();

  if ((unsigned)xlines == m_xlines.size() &&
      (unsigned)ylines == m_ylines.size() &&
      (unsigned)zlines == m_zlines.size()) {
    std::cout << m_className << "::Initialise:" << std::endl;
    std::cout << "    Found in file " << nlist << "\n    " << xlines
              << " x-lines\n    " << ylines << " y-lines\n    " << zlines
              << " z-lines" << std::endl;
  } else {
    std::cerr << m_className << "::Initialise:" << std::endl;
    std::cerr << "    There should be " << xlines << " x-lines, " << ylines
              << " y-lines and " << zlines << " z-lines in file " << nlist
              << " but I found :\n    " << m_xlines.size() << " x-lines, "
              << m_ylines.size() << " x-lines, " << m_zlines.size()
              << " z-lines." << std::endl;
  }
  m_nx = m_xlines.size();
  m_ny = m_ylines.size();
  m_nz = m_zlines.size();
  nNodes = m_nx*m_ny*m_nz;
  nElements = (m_nx-1)*(m_ny-1)*(m_nz-1);

  // Tell how many lines read
  std::cout << m_className << "::Initialise:" << std::endl;
  std::cout << "    Read " << nNodes << " nodes from file " << nlist << "."
            << std::endl;
  // Check number of nodes

  // Open the element list
  std::ifstream felist;
  felist.open(elist.c_str(), std::ios::in);
  if (felist.fail()) {
    std::cerr << m_className << "::Initialise:" << std::endl;
    std::cerr << "    Could not open element file " << elist << " for reading."
              << std::endl;
    std::cerr << "    The file perhaps does not exist." << std::endl;
    return false;
  }
  // Read the element list
  m_elementMaterial.resize(nElements);
  il = 0;
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
    unsigned char imat = atoi(token);
    // construct node numbers
    std::vector<int> node_nb;
    try{
      // Read element material - the number of the material is stored (1, 2, ...) but we need the index (0, 1, ...)
      m_elementMaterial.at(ielem) = (imat-1);
    } catch(...) {
      std::cerr << m_className << "::Initialise:" << std::endl;
      std::cerr << "    Error reading file " << elist << " (line " << il << ")." << std::endl;
      std::cerr << "    The element index (" << ielem << ") is not in the expected range: 0 - " << nElements << std::endl;
      ok = false;
    }
    // Check the material number and ensure that epsilon is non-negative
//    int check_mat = imat;
    if (imat < 1 || imat > m_nMaterials) {
      std::cerr << m_className << "::Initialise:" << std::endl;
      std::cerr << "   Out-of-range material number on file " << elist
                << " (line " << il << ")." << std::endl;
      std::cerr << "    Element: " << ielem << ", material: " << imat << std::endl;
      ok = false;
    }
    if (materials[imat - 1].eps < 0) {
      std::cerr << m_className << "::Initialise:" << std::endl;
      std::cerr << "    Element " << ielem << " in element list " << elist
                << " uses material " << imat << " which" << std::endl;
      std::cerr << "    has not been assigned a positive permittivity"
                << std::endl;
      std::cerr << "    in material list " << mplist << "." << std::endl;
      ok = false;
    }
  }
  // Close the file
  felist.close();
  // Tell how many lines read
  std::cout << m_className << "::Initialise:" << std::endl;
  std::cout << "    Read " << nElements << " elements from file " << elist
            << "," << std::endl;

  // Open the voltage list
  m_potential.resize(nNodes);
  std::ifstream fprnsol;
  fprnsol.open(prnsol.c_str(), std::ios::in);
  if (fprnsol.fail()) {
    std::cerr << m_className << "::Initialise:" << std::endl;
    std::cerr << "    Could not open potential file " << prnsol
              << " for reading." << std::endl;
    std::cerr << "    The file perhaps does not exist." << std::endl;
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
        int(token[0]) == 10 || int(token[0]) == 13 || strcmp(token, "Max") == 0)
      continue;
    // Read node potential (in prnsol node id starts with 1 and here we will use 0 as first node...)
    int inode = ReadInteger(token, -1, readerror);
    token = strtok(NULL, " ");
    double volt = ReadDouble(token, -1, readerror);

    try{
      m_potential.at(inode-1) = volt;
      nread++;
    } catch (...){
      std::cerr << m_className << "::Initialise:" << std::endl;
      std::cerr << "    Error reading file " << prnsol << " (line " << il << ")." << std::endl;
      std::cerr << "    The node index (" << inode-1 << ") is not in the expected range: 0 - " << nNodes << std::endl;
      ok = false;
    }
  }
  // Close the file
  fprnsol.close();
  // Tell how many lines read
  std::cout << m_className << "::Initialise:" << std::endl;
  std::cout << "    Read " << nread << "/" << nNodes << " (expected) potentials from file " << prnsol << "."
            << std::endl;
  // Check number of nodes
  if (nread != nNodes) {
    std::cerr << m_className << "::Initialise:" << std::endl;
    std::cerr << "    Number of nodes read (" << nread << ") on potential file "
              << prnsol << " does not" << std::endl;
    std::cerr << "    match the node list (" << nNodes << ")." << std::endl;
    ok = false;
  }
  // Set the ready flag
  if (ok) {
    m_ready = true;
  } else {
    std::cerr << m_className << "::Initialise:" << std::endl;
    std::cerr << "    Field map could not be read and cannot be interpolated."
              << std::endl;
    return false;
  }

  // Establish the ranges
  SetRange();
  UpdatePeriodicity();
  return true;
}

bool ComponentCST::Initialise(std::string dataFile, std::string unit){
  m_ready = false;

  // Keep track of the success
  bool ok = true;
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
    std::cerr << m_className << "::Initialise:" << std::endl;
    std::cerr << "    Unknown length unit " << unit << "." << std::endl;
    ok = false;
    funit = 1.0;
  }
  if (m_debug) {
    std::cout << m_className << "::Initialise:" << std::endl;
    std::cout << "    Unit scaling factor = " << funit << "." << std::endl;
  }
	FILE* f = fopen(dataFile.c_str(), "rb");
	if (f == nullptr) {
		std::cerr << m_className << "::Initilise:"  << std::endl;
		std::cerr << "    Could not open file:" << dataFile.c_str() << std::endl;
		return false;
	}

	struct stat fileStatus;
	stat(dataFile.c_str(), &fileStatus);
	int fileSize = fileStatus.st_size;

	if (fileSize < 1000) {
		fclose(f);
		std::cerr << m_className << "::Initilise:"  << std::endl;
		std::cerr << "     Error. The file is extremely short and does not seem to contain a header or data." << std::endl;
		ok = false;
	}

	char header[headerSize];
	size_t result;
	result = fread(header, sizeof(char), headerSize, f);
	if (result != headerSize) {fputs ("Reading error while reading header.",stderr); exit (3);}

	int nx = 0, ny = 0, nz = 0;
	int m_x = 0, m_y = 0, m_z = 0;
	int n_s = 0, n_x = 0, n_y = 0, n_z = 0;
	int e_s = 0, e_x = 0, e_y = 0, e_z = 0;
	int e_m = 0;

	int filled = 0;
	filled = std::sscanf(header, (std::string("mesh_nx=%d mesh_ny=%d mesh_nz=%d\n")+
			std::string("mesh_xlines=%d mesh_ylines=%d mesh_zlines=%d\n")+
			std::string("nodes_scalar=%d nodes_vector_x=%d nodes_vector_y=%d nodes_vector_z=%d\n")+
			std::string("elements_scalar=%d elements_vector_x=%d elements_vector_y=%d elements_vector_z=%d\n")+
			std::string("elements_material=%d\n")+
			std::string("n_materials=%d\n")).c_str(),
			&nx, &ny, &nz,
			&m_x, &m_y, &m_z,
			&n_s, &n_x, &n_y, &n_z,
			&e_s, &e_x, &e_y, &e_z,
			&e_m,	&m_nMaterials);
	if (filled != 16){
		fclose(f);
		std::cerr << m_className << "::Initilise:"  << std::endl;
		std::cerr << "    Error. File header of " << dataFile.c_str() << " is broken."  << std::endl;
		ok = false;
	}
	if (fileSize < 1000+(m_x+m_y+m_z)*8+(n_s+n_x+n_y+n_z+e_s+e_x+e_y+e_z)*4+e_m*1+(int)m_nMaterials*20)	{
		fclose(f);
		ok = false;
	}
	if (m_debug){
	    std::cout << m_className << "::Initialise:" << std::endl;
	    std::cout << "  Information about the data stored in the given binary file:" << std::endl;
		std::cout << "  Mesh (nx): " << nx << "\t Mesh (ny): " << ny << "\t Mesh (nz): " << nz << std::endl;
		std::cout << "  Mesh (x_lines): " << m_x << "\t Mesh (y_lines): " << m_y << "\t Mesh (z_lines): " << m_z << std::endl;
		std::cout << "  Nodes (scalar): " << n_s << "\t Nodes (x): " << n_x << "\t Nodes (y): " << n_y << "\t Nodes (z): " << n_z << std::endl;
		std::cout << "  Field (scalar): " << e_s << "\t Field (x): " << e_x << "\t Field (y): " << e_y << "\t Field (z): " << e_z << std::endl;
		std::cout << "  Elements: " << e_m << "\t Materials: " << m_nMaterials << std::endl;
	}
  m_nx = m_x;
  m_ny = m_y;
  m_nz = m_z;
  nNodes = m_nx*m_ny*m_nz;
  nElements = (m_nx-1)*(m_ny-1)*(m_nz-1);

	m_xlines.resize(m_x);
	m_ylines.resize(m_y);
	m_zlines.resize(m_z);
	m_potential.resize(n_s);
	m_elementMaterial.resize(e_m);
//	elements_scalar.resize(e_s);
	materials.resize(m_nMaterials);
	result = fread(m_xlines.data(), sizeof(double), m_xlines.size(), f);
	if (result != m_xlines.size()) {fputs ("Reading error while reading xlines.",stderr); exit (3);}
	else if (result == 0) {fputs ("No xlines are stored in the data file.",stderr); exit (3);}
	result = fread(m_ylines.data(), sizeof(double), m_ylines.size(), f);
	if (result != m_ylines.size()) {fputs ("Reading error while reading ylines",stderr); exit (3);}
	else if (result == 0) {fputs ("No ylines are stored in the data file.",stderr); exit (3);}
	result = fread(m_zlines.data(), sizeof(double), m_zlines.size(), f);
	if (result != m_zlines.size()) {fputs ("Reading error while reasing zlines",stderr); exit (3);}
	else if (result == 0) {fputs ("No zlines are stored in the data file.",stderr); exit (3);}
	result = fread(m_potential.data(), sizeof(float), m_potential.size(), f);
	if (result != m_potential.size()) {fputs ("Reading error while reading nodes.",stderr); exit (3);}
	else if (result == 0) {fputs ("No potentials are stored in the data file.",stderr); exit (3);}
	fseek(f,  e_s*sizeof(float), SEEK_CUR);
	// not needed in principle - thus it is ok if nothing is read
	result = fread(m_elementMaterial.data(), sizeof(unsigned char), m_elementMaterial.size(), f);
	if (result != m_elementMaterial.size()) {fputs ("Reading error while reading element material",stderr); exit (3);}
	std::stringstream st;
	st << m_className << "::Initialise:" << std::endl;
	/*
	 *  The material vector is filled according to the material id!
	 *  Thus material.at(0) is material with id 0.
	 */
	for (unsigned int i = 0; i<materials.size(); i++) {
		float id;
		result = fread(&(id), sizeof(float), 1, f);
		if (result != 1) {fputs ("Input error while reading material id.",stderr); exit (3);}
		unsigned int description_size = 0;
		result = fread(&(description_size), sizeof(int), 1, f);
		if (result != 1) {fputs ("Input error while reading material description size.",stderr); exit (3);}
		char* c = new char[description_size];
		result = fread(c, sizeof(char), description_size, f);
		if (result != description_size) {fputs ("Input error while reading material description.",stderr); exit (3);}
		std::string name = c;
		st << "  Read material: " << name.c_str();
		if (name.compare("gas") == 0){
		  st << " (considered as drift medium)";
		  materials.at(id).driftmedium = true;
		} else {
		  materials.at(id).driftmedium = false;
		}
		delete[] c;
		float tmp_eps;
		result = fread(&(tmp_eps), sizeof(float), 1, f);
		materials.at(id).eps = tmp_eps;
		if (result != 1) {fputs ("Reading error while reading eps.",stderr); exit (3);}
//		float mue, rho;
//		result = fread(&(mue), sizeof(float), 1, f);
//		if (result != 1) {fputs ("Reading error while reading mue.",stderr); exit (3);}
//		result = fread(&(rho), sizeof(float), 1, f);
//		if (result != 1) {fputs ("Reading error while reading rho.",stderr); exit (3);}
		st << "; eps is: " << materials.at(id).eps <<
//				"\t mue is: " << mue <<
//				"\t rho is: " << rho <<
				"\t id is: " <<  id << std::endl;
		// skip mue and rho
		fseek(f, 2*sizeof(float),SEEK_CUR);
		//ToDo: Check if rho should be used to decide, which material is driftable

	}
	if (m_debug){
	  std::cout << st.str();
	  for(std::vector<material>::iterator it = materials.begin(), it_end = materials.end(); it!=it_end;it++){
	    std::cout << "Material id: " << std::distance(materials.begin(),it) << " \t driftable: " << (*it).driftmedium << std::endl;
	  }
	}
	// To be sure that they are sorted (should be already be the case)
	std::sort(m_xlines.begin(),m_xlines.end());
	std::sort(m_ylines.begin(),m_ylines.end());
	std::sort(m_zlines.begin(),m_zlines.end());
	if (funit != 1){
      std::transform(m_xlines.begin(), m_xlines.end(), m_xlines.begin(), std::bind1st(std::multiplies<double>(),funit));
      std::transform(m_ylines.begin(), m_ylines.end(), m_ylines.begin(), std::bind1st(std::multiplies<double>(),funit));
      std::transform(m_zlines.begin(), m_zlines.end(), m_zlines.begin(), std::bind1st(std::multiplies<double>(),funit));
	}

	std::cout << m_className << "::Initialise" << std::endl;
	std::cout << "    x range: " << *(m_xlines.begin()) << " - " << *(m_xlines.end()-1) << std::endl;
	std::cout << "    y range: " << *(m_ylines.begin()) << " - " << *(m_ylines.end()-1) << std::endl;
	std::cout << "    z range: " << *(m_zlines.begin()) << " - " << *(m_zlines.end()-1) << std::endl;
	fclose(f);
  // Set the ready flag
  if (ok) {
    m_ready = true;
  } else {
    std::cerr << m_className << "::Initialise:" << std::endl;
    std::cerr << "    Field map could not be read and cannot be interpolated."
              << std::endl;
    return false;
  }

  SetRange();
  UpdatePeriodicity();
	return true;
}

bool ComponentCST::SetWeightingField(std::string prnsol, std::string label, bool isBinary) {
  std::vector<float> potentials(nNodes);
  if (!m_ready) {
    std::cerr << m_className << "::SetWeightingField:" << std::endl;
    std::cerr << "    No valid field map is present." << std::endl;
    std::cerr << "    Weighting field cannot be added." << std::endl;
    return false;
  }

  // Open the voltage list
  std::ifstream fprnsol;
  fprnsol.open(prnsol.c_str(), std::ios::in);
  if (fprnsol.fail()) {
    std::cerr << m_className << "::SetWeightingField:" << std::endl;
    std::cerr << "    Could not open potential file " << prnsol
              << " for reading." << std::endl;
    std::cerr << "    The file perhaps does not exist." << std::endl;
    return false;
  }
  // Check if a weighting field with the same label already exists.
  std::map<std::string, std::vector<float> >::iterator it = m_weightingFields.find(label);
  if (it != m_weightingFields.end()) {
    std::cout << m_className << "::SetWeightingField:" << std::endl;
    std::cout << "    Replacing existing weighting field " << label << "."
              << std::endl;
  } else {
    wfields.push_back(label);
    wfieldsOk.push_back(false);
  }

  if(std::distance(m_weightingFields.begin(),it) != std::distance(wfields.begin(),find(wfields.begin(),wfields.end(),label))){
    std::cerr << m_className << "::SetWeightingField:" << std::endl;
    std::cerr << "    Indexes of the weighting fields and the weighting field counter are not equal!" <<  std::endl;
    return false;
  }
  unsigned int iField = std::distance(m_weightingFields.begin(),it);
  int nread = 0;
  bool ok = true;

  if(isBinary) {
    std::cout <<  m_className << "::SetWeightingField:" << std::endl;
    std::cout <<  "    Reading weighting field from binary file:" << prnsol.c_str() << std::endl;
    FILE* f = fopen(prnsol.c_str(), "rb");
    if (f == nullptr) {
      std::cerr << m_className << "::Initilise:"  << std::endl;
      std::cerr << "    Could not open file:" << prnsol.c_str() << std::endl;
      return false;
    }

    struct stat fileStatus;
    stat(prnsol.c_str(), &fileStatus);
    int fileSize = fileStatus.st_size;

    if (fileSize < 1000) {
      fclose(f);
      std::cerr << m_className << "::SetWeightingField:"  << std::endl;
      std::cerr << "     Error. The file is extremely short and does not seem to contain a header or data." << std::endl;
      ok = false;
    }

    char header[headerSize];
    size_t result;
    result = fread(header, sizeof(char), headerSize, f);
    if (result != headerSize) {fputs ("Reading error while reading header.",stderr); exit (3);}

    int nx = 0, ny = 0, nz = 0;
    int m_x = 0, m_y = 0, m_z = 0;
    int n_x = 0, n_y = 0, n_z = 0;
    int e_s = 0, e_x = 0, e_y = 0, e_z = 0;
    int e_m = 0;

    int filled = 0;
    filled = std::sscanf(header, (std::string("mesh_nx=%d mesh_ny=%d mesh_nz=%d\n")+
        std::string("mesh_xlines=%d mesh_ylines=%d mesh_zlines=%d\n")+
        std::string("nodes_scalar=%d nodes_vector_x=%d nodes_vector_y=%d nodes_vector_z=%d\n")+
        std::string("elements_scalar=%d elements_vector_x=%d elements_vector_y=%d elements_vector_z=%d\n")+
        std::string("elements_material=%d\n")+
        std::string("n_materials=%d\n")).c_str(),
        &nx, &ny, &nz,
        &m_x, &m_y, &m_z,
        &nread, &n_x, &n_y, &n_z,
        &e_s, &e_x, &e_y, &e_z,
        &e_m, &m_nMaterials);
    if (filled != 16){
      fclose(f);
      std::cerr << m_className << "::SetWeightingField:"  << std::endl;
      std::cerr << "    Error. File header of " << prnsol.c_str() << " is broken."  << std::endl;
      ok = false;
    }
    if (fileSize < 1000+(m_x+m_y+m_z)*8+(nread+n_x+n_y+n_z+e_s+e_x+e_y+e_z)*4+e_m*1+(int)m_nMaterials*20)  {
      fclose(f);
      ok = false;
    }
    if(m_debug){
      std::cout << m_className << "::SetWeightingField:" << std::endl;
      std::cout << "  Information about the data stored in the given binary file:" << std::endl;
      std::cout << "  Mesh (nx): " << nx << "\t Mesh (ny): " << ny << "\t Mesh (nz): " << nz << std::endl;
      std::cout << "  Mesh (x_lines): " << m_x << "\t Mesh (y_lines): " << m_y << "\t Mesh (z_lines): " << m_z << std::endl;
      std::cout << "  Nodes (scalar): " << nread << "\t Nodes (x): " << n_x << "\t Nodes (y): " << n_y << "\t Nodes (z): " << n_z << std::endl;
      std::cout << "  Field (scalar): " << e_s << "\t Field (x): " << e_x << "\t Field (y): " << e_y << "\t Field (z): " << e_z << std::endl;
      std::cout << "  Elements: " << e_m << "\t Materials: " << m_nMaterials << std::endl;
    }
    // skip everything, but the potential
    fseek(f, m_x*sizeof(double), SEEK_CUR);
    fseek(f, m_y*sizeof(double), SEEK_CUR);
    fseek(f, m_z*sizeof(double), SEEK_CUR);
    result = fread(potentials.data(), sizeof(float), potentials.size(), f);
    if (result != potentials.size()) {fputs ("Reading error while reading nodes.",stderr); exit (3);}
    else if (result == 0) {fputs ("No wighting potentials are stored in the data file.",stderr); exit (3);}
    fprnsol.close();
  } else {
    std::cout <<  m_className << "::SetWeightingField:" << std::endl;
    std::cout <<  "    Reading weighting field from text file:" << prnsol.c_str() << std::endl;
    // Buffer for reading
    const int size = 100;
    char line[size];


    // Read the voltage list
    int il = 0;

    bool readerror = false;
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
      try{
        potentials.at(inode-1) =  volt;
        nread++;
      } catch (...){
        std::cerr << m_className << "::SetWeightingField:" << std::endl;
        std::cerr << "    Node number " << inode << " out of range." << std::endl;
        std::cerr << "    on potential file " << prnsol << " (line " << il << ")." << std::endl;
        std::cerr << "    Size of the potential vector is: " << potentials.size() << std::endl;
        ok = false;
      }
    }
    // Close the file
    fprnsol.close();
  }
  // Tell how many lines read
  std::cout << m_className << "::SetWeightingField:" << std::endl;
  std::cout << "    Read " << nread << "/" << nNodes << " (expected) potentials from file " << prnsol << "."
            << std::endl;
  // Check number of nodes
  if (nread != nNodes) {
    std::cerr << m_className << "::SetWeightingField:" << std::endl;
    std::cerr << "    Number of nodes read (" << nread << ")"
              << " on potential file (" << prnsol << ")" << std::endl;
    std::cerr << "     does not match the node list (" << nNodes << ")."
              << std::endl;
    ok = false;
  }
  if (!ok) {
    std::cerr << m_className << "::SetWeightingField:" << std::endl;
    std::cerr << "    Field map could not be read "
              << "and cannot be interpolated." << std::endl;
    return false;
  }

  m_weightingFields[label] = potentials;

  // Set the ready flag.
  wfieldsOk[iField] = ok;
  return true;
}

void ComponentCST::ShiftComponent(const double xShift, const double yShift, const double zShift){
  std::transform(m_xlines.begin(), m_xlines.end(), m_xlines.begin(), std::bind1st(std::plus<double>(),xShift));
  std::transform(m_ylines.begin(), m_ylines.end(), m_ylines.begin(), std::bind1st(std::plus<double>(),yShift));
  std::transform(m_zlines.begin(), m_zlines.end(), m_zlines.begin(), std::bind1st(std::plus<double>(),zShift));
  SetRange();
  UpdatePeriodicity();

  std::cout << m_className << "::ShiftComponent:" << std::endl;
  std::cout << "    Shifted component in x-direction: " << xShift
      << "\t y-direction: " << yShift
      << "\t z-direction: " << zShift << std::endl;
}

void ComponentCST::ElectricField(const double xin, const double yin, const double zin,
                                 double& ex, double& ey, double& ez, Medium*& m,
                                 int& status) {
  double volt;
  ElectricFieldBinary(xin, yin, zin, ex, ey, ez, volt, m, status);
}

void ComponentCST::ElectricField(const double xin, const double yin, const double zin,
                                 double& ex, double& ey, double& ez, double & volt, Medium*& m,
                                 int& status) {
  ElectricFieldBinary(xin, yin, zin, ex, ey, ez, volt, m, status, true);
}

void ComponentCST::WeightingField(const double xin, const double yin,
                                  const double zin, double& wx, double& wy,
                                  double& wz, const std::string& label) {

  // Initial values
  wx = wy = wz = 0;

  // Do not proceed if not properly initialised.
  if (!m_ready) return;

  // Look for the label.
  std::map<std::string, std::vector<float> >::iterator it = m_weightingFields.find(label);
  if(it == m_weightingFields.end()){
    // Do not proceed if the requested weighting field does not exist.
    std::cerr << "No weighting field named " << label.c_str() << " found!" << std::endl;
    return;
  }

  // Check if the weighting field is properly initialised.
  if (!wfieldsOk[std::distance(m_weightingFields.begin(),it)]) return;

  // Copy the coordinates
  double x = xin, y = yin, z = zin;

  // Map the coordinates onto field map coordinates and get indexes
  bool mirrored[3];
  double rcoordinate, rotation;
  unsigned int i,j,k;
  double position_mapped[3] = {0., 0., 0.};
  if(!Coordinate2Index(x, y, z, i, j, k, position_mapped, mirrored, rcoordinate, rotation))
    return;

  double rx = (position_mapped[0] - m_xlines.at(i))/(m_xlines.at(i+1) - m_xlines.at(i));
  double ry = (position_mapped[1] - m_ylines.at(j))/(m_ylines.at(j+1) - m_ylines.at(j));
  double rz = (position_mapped[2] - m_zlines.at(k))/(m_zlines.at(k+1) - m_zlines.at(k));

  float fwx, fwy, fwz;
  if(!disableFieldComponent[0])
    fwx = GetFieldComponent(i, j, k, rx, ry, rz, 'x', &((*it).second));
  if(!disableFieldComponent[1])
    fwy = GetFieldComponent(i, j, k, rx, ry, rz, 'y', &((*it).second));
  if(!disableFieldComponent[2])
    fwz = GetFieldComponent(i, j, k, rx, ry, rz, 'z', &((*it).second));

  if (m_elementMaterial.size()>0 && doShaping) {
    ShapeField(fwx, fwy, fwz, rx, ry, rz, i, j, k, &((*it).second));
  }
  if(mirrored[0])
    fwx *= -1.;
  if(mirrored[1])
    fwy *= -1.;
  if(mirrored[2])
    fwz *= -1.;
  if (warning) {
    std::cout << m_className << "::WeightingField:" << std::endl;
    std::cout << "    Warnings have been issued for this field map."
              << std::endl;
  }
  if (materials.at(m_elementMaterial.at(Index2Element(i,j,k))).driftmedium) {
    if (!disableFieldComponent[0]) wx = fwx;
    if (!disableFieldComponent[1]) wy = fwy;
    if (!disableFieldComponent[2]) wz = fwz;
  }
}

double ComponentCST::WeightingPotential(const double xin, const double yin,
                                        const double zin,
                                        const std::string& label) {

  // Do not proceed if not properly initialised.
  if (!m_ready) return 0.;

  // Look for the label.
  std::map<std::string, std::vector<float> >::iterator it = m_weightingFields.find(label);
  if(it == m_weightingFields.end()){
    // Do not proceed if the requested weighting field does not exist.
    std::cerr << "No weighting field named " << label.c_str() << " found!" << std::endl;
    return 0.;
  }

  // Check if the weighting field is properly initialised.
  if (!wfieldsOk[std::distance(m_weightingFields.begin(),it)]) return 0.;

  // Copy the coordinates
  double x = xin, y = yin, z = zin;

  // Map the coordinates onto field map coordinates
  bool mirrored[3];
  double rcoordinate, rotation;
  unsigned int i,j,k;
  double position_mapped[3] = {0., 0., 0.};
  if(!Coordinate2Index(x, y, z, i, j, k, position_mapped, mirrored, rcoordinate, rotation))
    return 0.;

  double rx = (position_mapped[0] - m_xlines.at(i))/(m_xlines.at(i+1) - m_xlines.at(i));
  double ry = (position_mapped[1] - m_ylines.at(j))/(m_ylines.at(j+1) - m_ylines.at(j));
  double rz = (position_mapped[2] - m_zlines.at(k))/(m_zlines.at(k+1) - m_zlines.at(k));

  double potential = GetPotential(i, j, k, rx, ry, rz, &((*it).second));

  if (m_debug) {
    std::cout << m_className << "::WeightingPotential:" << std::endl;
    std::cout << "    Global: (" << x << "," << y << "," << z << "),"
              << std::endl;
    std::cout << "    Local: (" << rx << "," << ry << "," << rz
              << ") in element with indexes: i=" << i << ", j=" << j << ", k=" << k << std::endl;
    std::cout << "  Node xyzV:" << std::endl;
    std::cout << "Node 0 position: " << Index2Node(i+1, j  , k  ) << "\t potential: " << ((*it).second).at(Index2Node(i+1, j  , k  ))
              << "Node 1 position: " << Index2Node(i+1, j+1, k  ) << "\t potential: " << ((*it).second).at(Index2Node(i+1, j+1, k  ))
              << "Node 2 position: " << Index2Node(i  , j+1, k  ) << "\t potential: " << ((*it).second).at(Index2Node(i  , j+1, k  ))
              << "Node 3 position: " << Index2Node(i  , j  , k  ) << "\t potential: " << ((*it).second).at(Index2Node(i  , j  , k  ))
              << "Node 4 position: " << Index2Node(i+1, j  , k+1) << "\t potential: " << ((*it).second).at(Index2Node(i+1, j  , k+1))
              << "Node 5 position: " << Index2Node(i+1, j+1, k+1) << "\t potential: " << ((*it).second).at(Index2Node(i+1, j+1, k+1))
              << "Node 6 position: " << Index2Node(i  , j+1, k+1) << "\t potential: " << ((*it).second).at(Index2Node(i  , j+1, k+1))
              << "Node 7 position: " << Index2Node(i  , j  , k+1) << "\t potential: " << ((*it).second).at(Index2Node(i  , j  , k  ))
              << std::endl;
  }
  return potential;
}

void ComponentCST::GetNumberOfMeshLines(unsigned int &n_x, unsigned int &n_y, unsigned int &n_z){
  n_x = m_xlines.size();
  n_y = m_ylines.size();
  n_z = m_zlines.size();
}

void ComponentCST::GetElementBoundaries(unsigned int element, double &xmin, double &xmax,
    double &ymin, double &ymax, double &zmin, double &zmax){
  unsigned int i,j,k;
  Element2Index(element, i, j, k);
  xmin = m_xlines.at(i);
  xmax = m_xlines.at(i+1);
  ymin = m_ylines.at(j);
  ymax = m_ylines.at(j+1);
  zmin = m_zlines.at(k);
  zmax = m_zlines.at(k+1);
}

Medium* ComponentCST::GetMedium(const double xin, const double yin,
                                const double zin) {
  unsigned int i, j, k;
  Coordinate2Index(xin,yin,zin,i,j,k);
  if(m_debug){
      std::cout << m_className << "::GetMedium:" << std::endl;
      std::cout << "    Found position (" << xin << ", " << yin << ", " << zin << "): " << std:: endl;
      std::cout << "    Indexes are: x: " << i << "/" << m_xlines.size()
          << "\t y: " << j << "/" << m_ylines.size()
          << "\t z: " << k << "/" << m_zlines.size() << std::endl;
      std::cout << "    Element material index: " << Index2Element(i, j, k) << std::endl;
      std::cout << "    Element index: " << (int)m_elementMaterial.at(Index2Element(i,j,k)) << std::endl;
  }
  return materials.at(m_elementMaterial.at(Index2Element(i,j,k))).medium;
}

void ComponentCST::SetRange(){
  // Establish the ranges
  mapxmin = *m_xlines.begin();
  mapxmax = *(m_xlines.end()-1);
  mapymin = *m_ylines.begin();
  mapymax = *(m_ylines.end()-1);
  mapzmin = *m_zlines.begin();
  mapzmax = *(m_zlines.end()-1);
  mapvmin = *std::min_element(m_potential.begin(),m_potential.end());
  mapvmax = *std::max_element(m_potential.begin(),m_potential.end());
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

}

void ComponentCST::SetRangeZ(const double zmin, const double zmax) {

  if (fabs(zmax - zmin) <= 0.) {
    std::cerr << m_className << "::SetRangeZ:" << std::endl;
    std::cerr << "    Zero range is not permitted." << std::endl;
    return;
  }
  zMinBoundingBox = std::min(zmin, zmax);
  zMaxBoundingBox = std::max(zmin, zmax);
}

bool ComponentCST::Coordinate2Index(const double x, const double y, const double z,
      unsigned int &i, unsigned int &j, unsigned int &k){
  bool mirrored[3] = {false, false, false};
  double position_mapped[3] = {0., 0., 0.};
  double rcoordinate, rotation;
  return Coordinate2Index(x ,y, z, i, j, k, position_mapped, mirrored, rcoordinate, rotation);
}


int ComponentCST::Index2Element(const unsigned int i, const unsigned int j, const unsigned int k){

  if (i>m_nx-2 || j>m_ny-2 || k>m_nz-2) {
    throw "FieldMap::ElementByIndex: Error. Element indexes out of bounds.";
  }
  return i+j*(m_nx-1)+k*(m_nx-1)*(m_ny-1);
}

bool ComponentCST::Coordinate2Index(const double xin, const double yin, const double zin,
      unsigned int &i, unsigned int &j, unsigned int &k,
      double *position_mapped, bool *mirrored,
      double &rcoordinate, double &rotation){
  // Map the coordinates onto field map coordinates
  position_mapped[0] = xin;
  position_mapped[1] = yin;
  position_mapped[2] = zin;
  MapCoordinates(position_mapped[0], position_mapped[1], position_mapped[2],
      mirrored[0], mirrored[1], mirrored[2], rcoordinate, rotation);

  std::vector<double>::iterator it_x, it_y, it_z;
  it_x = std::lower_bound(m_xlines.begin(),m_xlines.end(),position_mapped[0]);
  it_y = std::lower_bound(m_ylines.begin(),m_ylines.end(),position_mapped[1]);
  it_z = std::lower_bound(m_zlines.begin(),m_zlines.end(),position_mapped[2]);
  if(it_x == m_xlines.end() || it_y == m_ylines.end() || it_z == m_zlines.end() ||
     position_mapped[0] < m_xlines.at(0) || position_mapped[1] < m_ylines.at(0) || position_mapped[2] < m_zlines.at(0) ){
    if(m_debug){
      std::cerr << m_className << "::ElectricFieldBinary:" << std::endl;
      std::cerr << "    Could not find the given coordinate!" << std::endl;
      std::cerr << "    You ask for the following position: " << xin << ", " << yin << ", " << zin << std::endl;
      std::cerr << "    The mapped position is: " << position_mapped[0] << ", " << position_mapped[1] << ", " << position_mapped[2] << std::endl;
      PrintRange();
    }
    return false;
  }
  /* Lower bound returns the next mesh line behind the position in question.
   * If the position in question is on a mesh line this mesh line is returned.
   * Since we are interested in the mesh line before the position in question we need to move the
   * iterator to the left except for the very first mesh line!
   */
  if(it_x == m_xlines.begin())
    i = 0;
  else
    i  = std::distance(m_xlines.begin(),it_x-1);
  if(it_y == m_ylines.begin())
    j = 0;
  else
    j  = std::distance(m_ylines.begin(),it_y-1);
  if(it_z == m_zlines.begin())
    k = 0;
  else
    k  = std::distance(m_zlines.begin(),it_z-1);
  return true;
}

void ComponentCST::UpdatePeriodicity() {

  UpdatePeriodicity2d();
  UpdatePeriodicityCommon();
}

void ComponentCST::GetAspectRatio(const int element, double& dmin, double& dmax) {

  if (element < 0 || element >= nElements) {
    dmin = dmax = 0.;
    return;
  }
  unsigned int i, j, k;
  Element2Index(element, i, j, k);
  std::vector<double> distances;
  distances.push_back(m_xlines.at(i+1) - m_xlines.at(i));
  distances.push_back(m_ylines.at(j+1) - m_ylines.at(j));
  distances.push_back(m_zlines.at(k+1) - m_zlines.at(k));
  std::sort(distances.begin(), distances.end());
  dmin = distances.at(0);
  dmax = distances.at(2);
}

double ComponentCST::GetElementVolume(const int element) {

  if (element < 0 || element >= nElements) return 0.;
  unsigned int i,j,k;
  Element2Index(element, i, j, k);
  const double volume =
      fabs((m_xlines.at(i+1) - m_xlines.at(i)) *
           (m_xlines.at(j+1) - m_ylines.at(j)) *
           (m_xlines.at(k+1) - m_zlines.at(k)));
  return volume;
}

void ComponentCST::ElectricFieldBinary(const double xin, const double yin, const double zin,
                                 double& ex, double& ey, double& ez, double & volt, Medium*& m,
                                 int& status, bool calculatePotential) {
  // Copy the coordinates
  double x = xin, y = yin, z = zin;

  ex = ey = ez = 0;

  bool mirrored[3];
  double rcoordinate, rotation;
  unsigned int i,j,k;
  double position_mapped[3] = {0., 0., 0.};
  if(!Coordinate2Index(x, y, z, i, j, k, position_mapped, mirrored, rcoordinate, rotation))
    return;

  double rx = (position_mapped[0] - m_xlines.at(i))/(m_xlines.at(i+1) - m_xlines.at(i));
  double ry = (position_mapped[1] - m_ylines.at(j))/(m_ylines.at(j+1) - m_ylines.at(j));
  double rz = (position_mapped[2] - m_zlines.at(k))/(m_zlines.at(k+1) - m_zlines.at(k));

  float fex = GetFieldComponent(i, j, k, rx, ry, rz, 'x', &m_potential);
  float fey = GetFieldComponent(i, j, k, rx, ry, rz, 'y', &m_potential);
  float fez = GetFieldComponent(i, j, k, rx, ry, rz, 'z', &m_potential);

  if (m_elementMaterial.size()>0 && doShaping) {
    ShapeField(fex, fey, fez, rx, ry, rz, i, j, k, &m_potential);
  }
  if(mirrored[0])
    fex *= -1.;
  if(mirrored[1])
    fey *= -1.;
  if(mirrored[2])
    fez *= -1.;
  if(m_debug){
    std::cout << m_className << "::ElectricFieldBinary:" << std::endl;
    std::cout << "    Found position (" << x << ", " << y << ", " << z << "): " << std:: endl;
    std::cout << "    Indexes are: x: " << i << "/" << m_xlines.size()
        << "\t y: " << j << "/" << m_ylines.size()
        << "\t z: " << k << "/" << m_zlines.size() << std::endl;
    if(i != 0 && j != 0 && k != 0){
      std::cout << "    index: " << i << "\t x before: " << m_xlines.at(i-1) << "\t x behind: " << m_xlines.at(i) <<  "\t r = " << rx
        << "\n    index: " << j << "\t y before: " << m_ylines.at(j-1) << "\t y behind: " << m_ylines.at(j) << "\t r = " << ry
        << "\n    index: " << k << "\t z before: " << m_zlines.at(k-1) << "\t z behind: " << m_zlines.at(k) << "\t r = " << rz << std::endl;
    }
    std::cout << "    Electric field is: " << fex << ", " << fey << ", " << fez << "): " << std:: endl;
  }
  // get the material index of the element and return the medium taken from the materials (since the material id is equal to the material vector position)
  m = materials.at(m_elementMaterial.at(Index2Element(i,j,k))).medium;
  //  m = materials[elements[imap].matmap].medium;
  status = -5;
  if (materials.at(m_elementMaterial.at(Index2Element(i,j,k))).driftmedium) {
    if (m != 0) {
      if (m->IsDriftable()) status = 0;
    }
  }
  if(!disableFieldComponent[0])
    ex = fex;
  if(!disableFieldComponent[1])
    ey = fey;
  if(!disableFieldComponent[2])
    ez = fez;
  if(calculatePotential)
    volt = GetPotential(i,j,k,rx,ry,rz,&m_potential);

}

float ComponentCST::GetFieldComponent(const unsigned int i,const unsigned  int j,const unsigned int k,
    const double rx,const double ry ,const double rz,const char component, const std::vector<float>* potentials){
  float dv1 = 0, dv2 = 0, dv3 = 0, dv4 = 0;
  float dv11 = 0, dv21 = 0, dv = 0;
  float e = 0;
  if (component=='x')
  {
      dv1 = potentials->at(Index2Node(i+1, j, k)) - potentials->at(Index2Node(i, j, k));
      dv2 = potentials->at(Index2Node(i+1, j+1, k)) - potentials->at(Index2Node(i, j+1, k));
      dv3 = potentials->at(Index2Node(i+1, j+1, k+1)) - potentials->at(Index2Node(i, j+1, k+1));
      dv4 = potentials->at(Index2Node(i+1, j, k+1)) - potentials->at(Index2Node(i, j, k+1));

      dv11 = dv1 + (dv4-dv1)*rz;
      dv21 = dv2 + (dv3-dv2)*rz;
      dv = dv11 + (dv21-dv11)*ry;
      e = -1*dv/(m_xlines.at(i+1)-m_xlines.at(i));
  }
  if (component=='y')
  {
      dv1 = potentials->at(Index2Node(i, j+1, k)) - potentials->at(Index2Node(i, j, k));
      dv2 = potentials->at(Index2Node(i, j+1, k+1)) - potentials->at(Index2Node(i, j, k+1));
      dv3 = potentials->at(Index2Node(i+1, j+1, k+1)) - potentials->at(Index2Node(i+1, j, k+1));
      dv4 = potentials->at(Index2Node(i+1, j+1, k)) - potentials->at(Index2Node(i+1, j, k));

      dv11 = dv1 + (dv4-dv1)*rx;
      dv21 = dv2 + (dv3-dv2)*rx;
      dv = dv11 + (dv21-dv11)*rz;
      e = -1*dv/(m_ylines.at(j+1)-m_ylines.at(j));
  }
  if (component=='z')
  {
      dv1 = potentials->at(Index2Node(i, j, k+1)) - potentials->at(Index2Node(i, j, k));
      dv2 = potentials->at(Index2Node(i+1, j, k+1)) - potentials->at(Index2Node(i+1, j, k));
      dv3 = potentials->at(Index2Node(i+1, j+1, k+1)) - potentials->at(Index2Node(i+1, j+1, k));
      dv4 = potentials->at(Index2Node(i, j+1, k+1)) - potentials->at(Index2Node(i, j+1, k));

      dv11 = dv1 + (dv4-dv1)*ry;
      dv21 = dv2 + (dv3-dv2)*ry;
      dv = dv11 + (dv21-dv11)*rx;
      e = -1*dv/(m_zlines.at(k+1)-m_zlines.at(k));
  }
  return e;
}

float ComponentCST::GetPotential(const unsigned int i,const unsigned  int j,const unsigned int k,
    const double rx,const double ry ,const double rz, const std::vector<float>* potentials){
  double t1 = rx*2. - 1;
  double t2 = ry*2. - 1;
  double t3 = rz*2. - 1;
  return (potentials->at(Index2Node(i+1, j  , k  )) * (1 - t1) * (1 - t2) * (1 - t3) +
          potentials->at(Index2Node(i+1, j+1, k  )) * (1 + t1) * (1 - t2) * (1 - t3) +
          potentials->at(Index2Node(i  , j+1, k  )) * (1 + t1) * (1 + t2) * (1 - t3) +
          potentials->at(Index2Node(i  , j  , k  )) * (1 - t1) * (1 + t2) * (1 - t3) +
          potentials->at(Index2Node(i+1, j  , k+1)) * (1 - t1) * (1 - t2) * (1 + t3) +
          potentials->at(Index2Node(i+1, j+1, k+1)) * (1 + t1) * (1 - t2) * (1 + t3) +
          potentials->at(Index2Node(i  , j+1, k+1)) * (1 + t1) * (1 + t2) * (1 + t3) +
          potentials->at(Index2Node(i  , j  , k+1)) * (1 - t1) * (1 + t2) * (1 + t3)) /
         8.;
}

void ComponentCST::ShapeField(float &ex, float &ey, float &ez,
    const double rx, const double ry, const double rz,
    const unsigned int i, const unsigned int j, const unsigned int k,
    std::vector<float>* potentials){
  int m1 = 0, m2 = 0;
  if ((i==0 && rx>=0.5) || (i==m_xlines.size()-2 && rx<0.5) || (i>0 && i<m_xlines.size()-2))
  {
    m1 = m_elementMaterial.at(Index2Element(i, j, k));
    if (rx>=0.5)
    {
      m2 = m_elementMaterial.at(Index2Element(i+1, j, k));
      if (m1==m2)
      {
        float ex_next = GetFieldComponent(i+1, j, k, 0.5, ry, rz, 'x', potentials);
        ex = ex + (rx-0.5) * (ex_next - ex) * (m_xlines.at(i+1) - m_xlines.at(i)) / (m_xlines.at(i+2) - m_xlines.at(i+1));
      }
    }
    else
    {
      m2 = m_elementMaterial.at(Index2Element(i-1, j, k));
      if (m1==m2)
      {
        float ex_before = GetFieldComponent(i-1, j, k, 0.5, ry, rz, 'x', potentials);
        ex = ex_before + (rx+0.5) * (ex - ex_before) * (m_xlines.at(i) - m_xlines.at(i-1)) / (m_xlines.at(i+1) - m_xlines.at(i));
      }
    }
  }

  if ((j==0 && ry>=0.5) || (j==m_ylines.size()-2 && ry<0.5) || (j>0 && j<m_ylines.size()-2))
  {
    m1 = m_elementMaterial.at(Index2Element(i, j, k));
    if (ry>=0.5)
    {
      m2 = m_elementMaterial.at(Index2Element(i, j+1, k));
      if (m1==m2)
      {
        float ey_next = GetFieldComponent(i, j+1, k, rx, 0.5, rz, 'y', potentials);
        ey = ey + (ry-0.5) * (ey_next - ey) * (m_ylines.at(j+1) - m_ylines.at(j)) / (m_ylines.at(j+2) - m_ylines.at(j+1));
      }
    }
    else
    {
      m2 = m_elementMaterial.at(Index2Element(i, j-1, k));
      if (m1==m2)
      {
        float ey_next = GetFieldComponent(i, j-1, k, rx, 0.5, rz, 'y', potentials);
        ey = ey_next + (ry+0.5) * (ey - ey_next) * (m_ylines.at(j) - m_ylines.at(j-1)) / (m_ylines.at(j+1) - m_ylines.at(j));
      }
    }
  }

  if ((k==0 && rz>=0.5) || (k==m_zlines.size()-2 && rz<0.5) || (k>0 && k<m_zlines.size()-2))
  {
    m1 = m_elementMaterial.at(Index2Element(i, j, k));
    if (rz>=0.5)
    {
      m2 = m_elementMaterial.at(Index2Element(i, j, k+1));
      if (m1==m2)
      {
        float ez_next = GetFieldComponent(i, j, k+1, rx, ry, 0.5, 'z', potentials);
        ez = ez + (rz-0.5) * (ez_next - ez) * (m_zlines.at(k+1) - m_zlines.at(k)) / (m_zlines.at(k+2) - m_zlines.at(k+1));
      }
    }
    else
    {
      m2 = m_elementMaterial.at(Index2Element(i, j, k-1));
      if (m1==m2)
      {
        float ez_next = GetFieldComponent(i, j, k-1, rx, ry, 0.5, 'z', potentials);
        ez = ez_next + (rz+0.5) * (ez - ez_next) * (m_zlines.at(k) - m_zlines.at(k-1)) / (m_zlines.at(k+1) - m_zlines.at(k));
      }
    }
  }
}
//
void ComponentCST::Element2Index(int element, unsigned int& i, unsigned int& j,unsigned  int& k) {
  int tmp = element;
  k = element / ((m_xlines.size() - 1) * (m_ylines.size() - 1));
  tmp -= k * (m_xlines.size() - 1) * (m_ylines.size() - 1);
  j = tmp / (m_xlines.size() - 1);
  i = element - j * (m_xlines.size() - 1) -
      k * (m_xlines.size() - 1) * (m_ylines.size() - 1);
}

int ComponentCST::Index2Node(const unsigned int i, const unsigned int j, const unsigned int k){

  if (i>m_nx-1 || j>m_ny-1 || k>m_nz-1) {
    throw "FieldMap::NodeByIndex: Error. Node indexes out of bounds.";
  }
  return i+j*m_nx+k*m_nx*m_ny;
}
}
