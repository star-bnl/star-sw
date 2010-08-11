#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <algorithm>

#include "ComponentTcad2d.hh"

namespace Garfield {

ComponentTcad2d::ComponentTcad2d() : 
  ComponentBase(), 
  nRegions(0), nVertices(0), nElements(0),
  hasBoundingBox(false), 
  lastElement(0), a(0.), b(0.), c(0.), d(0.) {

  className = "ComponentTcad2d";
    
  regions.clear();
  vertices.clear();
  elements.clear();

}

void 
ComponentTcad2d::ElectricField(const double x, const double y, const double z,
                               double& ex, double& ey, double& ez, double& p,
                               Medium*& m, int& status) {
 
  m = 0; 
  // Make sure the field map has been loaded properly
  if (!ready) {
    std::cerr << className << "::ElectricField:\n";
    std::cerr << "    Field map not available for interpolation.\n";
    status = -10;
    return;
  }  
  
  // Check if point is inside the bounding box
  if (x < xMinBoundingBox || x > xMaxBoundingBox ||
      y < yMinBoundingBox || y > yMaxBoundingBox) {
    status = -11;
    return;
  }
  
  // Set z-component to zero
  ez = 0.;
  // Assume that the point is inside a drift medium
  status = 0;
  // Check if point is still located in the previously found element
  int i = lastElement;
  switch (elements[i].type) {
    case 1:
      if (CheckLine(x, y, i)) {
        ex = a * vertices[elements[i].vertex[0]].ex + 
             b * vertices[elements[i].vertex[1]].ex;
        ey = a * vertices[elements[i].vertex[0]].ey + 
             b * vertices[elements[i].vertex[1]].ey;
        p  = a * vertices[elements[i].vertex[0]].p + 
             b * vertices[elements[i].vertex[1]].p;
        m = regions[elements[i].region].medium;
        if (!regions[elements[i].region].drift || m == 0) status = -5;
        return;
      } 
      break;
    case 2:
      if (CheckTriangle(x, y, i)) {
        ex = a * vertices[elements[i].vertex[0]].ex + 
             b * vertices[elements[i].vertex[1]].ex + 
             c * vertices[elements[i].vertex[2]].ex;
        ey = a * vertices[elements[i].vertex[0]].ey + 
             b * vertices[elements[i].vertex[1]].ey + 
             c * vertices[elements[i].vertex[2]].ey;
        p  = a * vertices[elements[i].vertex[0]].p + 
             b * vertices[elements[i].vertex[1]].p + 
             c * vertices[elements[i].vertex[2]].p;
        m = regions[elements[i].region].medium;
        if (!regions[elements[i].region].drift || m == 0) status = -5;
        return;
      }
      break;  
    case 3:
      if (CheckRectangle(x, y, i)) {
        ex = a * vertices[elements[i].vertex[0]].ex + 
             b * vertices[elements[i].vertex[1]].ex + 
             c * vertices[elements[i].vertex[2]].ex + 
             d * vertices[elements[i].vertex[3]].ex;
        ey = a * vertices[elements[i].vertex[0]].ey + 
             b * vertices[elements[i].vertex[1]].ey + 
             c * vertices[elements[i].vertex[2]].ey + 
             d * vertices[elements[i].vertex[3]].ey;
        p  = a * vertices[elements[i].vertex[0]].p + 
             b * vertices[elements[i].vertex[1]].p + 
             c * vertices[elements[i].vertex[2]].p + 
             d * vertices[elements[i].vertex[3]].p;
        m = regions[elements[i].region].medium;
        if (!regions[elements[i].region].drift || m == 0) status = -5;
        return;
      }
      break;
    default: 
      std::cerr << className << "::ElectricField:\n";
      std::cerr << "    Invalid element type (" 
                << elements[i].type << ").\n";
      status = -11;
      return;
      break;
  }
  
  // The point is not in the previous element
  // We have to loop over all elements
  for (i = nElements; i--;) {
    if (x < vertices[elements[i].vertex[0]].x) continue;
    switch (elements[i].type) {
      case 1:
        if (CheckLine(x, y, i)) {
          ex = a * vertices[elements[i].vertex[0]].ex + 
               b * vertices[elements[i].vertex[1]].ex;
          ey = a * vertices[elements[i].vertex[0]].ey + 
               b * vertices[elements[i].vertex[1]].ey;
          p  = a * vertices[elements[i].vertex[0]].p + 
               b * vertices[elements[i].vertex[1]].p;
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (!regions[elements[i].region].drift || m == 0) status = -5; 
          return;
        }
        break;
      case 2:
        if (CheckTriangle(x, y, i)) {
          ex = a * vertices[elements[i].vertex[0]].ex + 
               b * vertices[elements[i].vertex[1]].ex + 
               c * vertices[elements[i].vertex[2]].ex;
          ey = a * vertices[elements[i].vertex[0]].ey + 
               b * vertices[elements[i].vertex[1]].ey + 
               c * vertices[elements[i].vertex[2]].ey;
          p  = a * vertices[elements[i].vertex[0]].p + 
               b * vertices[elements[i].vertex[1]].p + 
               c * vertices[elements[i].vertex[2]].p;               
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (!regions[elements[i].region].drift || m == 0) status =  -5; 
          return;
        }
        break;    
      case 3:
        if (CheckRectangle(x, y, i)) {
          ex = a * vertices[elements[i].vertex[0]].ex + 
               b * vertices[elements[i].vertex[1]].ex + 
               c * vertices[elements[i].vertex[2]].ex + 
               d * vertices[elements[i].vertex[3]].ex;
          ey = a * vertices[elements[i].vertex[0]].ey + 
               b * vertices[elements[i].vertex[1]].ey + 
               c * vertices[elements[i].vertex[2]].ey + 
               d * vertices[elements[i].vertex[3]].ey;
          p  = a * vertices[elements[i].vertex[0]].p + 
               b * vertices[elements[i].vertex[1]].p + 
               c * vertices[elements[i].vertex[2]].p + 
               d * vertices[elements[i].vertex[3]].p;               
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (!regions[elements[i].region].drift || m == 0) status = -5;
          return;
        }
        break;
      default:
        std::cerr << className << "::ElectricField:\n";
        std::cerr << "    Invalid element type (" 
                  << elements[i].type << ").\n";
        status = -11;
        return;
        break;
    }
  }
  // Point is outside the mesh
  if (debug) {
    std::cerr << className << "::ElectricField:\n";
    std::cerr << "    Point (" << x << ", " << y 
              << ") is outside the mesh.\n";
  }
  status = -6;
  return;

}

void 
ComponentTcad2d::ElectricField(const double x, const double y, const double z,
                               double& ex, double& ey, double& ez,
                               Medium*& m, int& status) {

  double v = 0.;
  ElectricField(x, y, z, ex, ey, ez, v, m, status);
  
}

bool 
ComponentTcad2d::GetMedium(const double x, const double y, const double z,
                           Medium*& m) {

  m = 0;
  // Make sure the field map has been loaded properly
  if (!ready) {
    std::cerr << className << "::GetMedium:\n";
    std::cerr << "    Field map not available for interpolation.\n";
    return false;
  }  
  
  // Check if point is inside the bounding box
  if (x < xMinBoundingBox || x > xMaxBoundingBox || 
      y < yMinBoundingBox || y > yMaxBoundingBox) {
    return false;
  }
  
  // Check if point is still located in the previously found element
  int i = lastElement;
  switch (elements[i].type) {
    case 1:
      if (CheckLine(x, y, i)) {
        m = regions[elements[i].region].medium;
        if (m == 0) return false;
        return true;
      } 
      break;
    case 2:
      if (CheckTriangle(x, y, i)) {
        m = regions[elements[i].region].medium;
        if (m == 0) return false;
        return true;
      }
      break;  
    case 3:
      if (CheckRectangle(x, y, i)) {
        m = regions[elements[i].region].medium;
        if (m == 0) return false;
        return true;
      }
      break;
    default: 
      std::cerr << className << "::GetMedium:\n";
      std::cerr << "    Invalid element type (" 
                << elements[i].type << ").\n";
      return false;
      break;
  }
  
  // The point is not in the previous element
  // We have to loop over all elements
  for (i = nElements; i--;) {
    if (x < vertices[elements[i].vertex[0]].x) continue;
    switch (elements[i].type) {
      case 1:
        if (CheckLine(x, y, i)) {
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (m == 0) return false;
          return true;
        }
        break;
      case 2:
        if (CheckTriangle(x, y, i)) {
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (m == 0) return false;
          return true;
        }
        break;    
      case 3:
        if (CheckRectangle(x, y, i)) {
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (m == 0) return false;
          return true;
        }
        break;
      default:
        std::cerr << className << "::GetMedium:\n";
        std::cerr << "    Invalid element type (" 
                  << elements[i].type << ").\n";
        return false;
        break;
    }
  }
  // Point is outside the mesh
  return false;

}
 
bool 
ComponentTcad2d::Initialise(const std::string gridfilename, 
                            const std::string datafilename) {
  
  ready = false;
  // Import mesh data
  if (!LoadGrid(gridfilename)) {
    std::cerr << className << "::Initialise:\n";
    std::cerr << "    Importing mesh data failed.\n";
    return false;
  }
  
  // Import electric field and potential 
  if (!LoadData(datafilename)) {
    std::cerr << className << "::Initialise:\n";
    std::cerr << "    Importing electric field and potential failed.\n"; 
    return false;
  }
  
  // Find min. and max. coordinates and potentials
  xMaxBoundingBox = xMinBoundingBox = vertices[elements[0].vertex[0]].x;
  yMaxBoundingBox = yMinBoundingBox = vertices[elements[0].vertex[0]].y;  
  pMax = pMin = vertices[elements[0].vertex[0]].p;
  for (int i = nElements; i--;) {
    for (int j = 0; j <= elements[i].type; ++j) {
      if (vertices[elements[i].vertex[j]].x < xMinBoundingBox) {
        xMinBoundingBox = vertices[elements[i].vertex[j]].x;
      } else if (vertices[elements[i].vertex[j]].x > xMaxBoundingBox) {
        xMaxBoundingBox = vertices[elements[i].vertex[j]].x;
      }
      if (vertices[elements[i].vertex[j]].y < yMinBoundingBox) {
        yMinBoundingBox = vertices[elements[i].vertex[j]].y;
      } else if (vertices[elements[i].vertex[j]].y > yMaxBoundingBox) {
        yMaxBoundingBox = vertices[elements[i].vertex[j]].y;
      }
      if (vertices[elements[i].vertex[j]].p < pMin) {
        pMin = vertices[elements[i].vertex[j]].p;
      } else if (vertices[elements[i].vertex[j]].p > pMax) {
        pMax = vertices[elements[i].vertex[j]].p;
      }
    }
  }
  
  ready = true;
  return true;

}

bool 
ComponentTcad2d::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                                double& xmax, double& ymax, double& zmax) {

  if (!ready) return false;
  xmin = xMinBoundingBox; ymin = yMinBoundingBox; 
  xmax = xMaxBoundingBox; ymax = yMaxBoundingBox;
  return true;

}

bool 
ComponentTcad2d::GetVoltageRange(double& vmin, double& vmax) {

  if (!ready) return false;
  vmin = pMin; vmax = pMax;
  return true;
  
}

void 
ComponentTcad2d::GetRegion(const int i, std::string& name, bool& active) {

  if (i < 0 || i >= nRegions) {
    std::cerr << className << "::GetRegion:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }  
  name = regions[i].name;
  active = regions[i].drift;

}

void 
ComponentTcad2d::SetDriftRegion(const int i, const bool active) {

  if (i < 0 || i >= nRegions) {
    std::cerr << className << "::SetDriftRegion:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }
  regions[i].drift = active;

}

void 
ComponentTcad2d::SetMedium(const int i, Medium* medium) {

  if (i < 0 || i >= nRegions) {
    std::cerr << className << "::SetMedium:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }
  
  if (medium == 0) {
    std::cerr << className << "::SetMedium:\n";
    std::cerr << "    Medium pointer is null.\n";
    return;
  }
  
  regions[i].medium = medium;
  
}

bool
ComponentTcad2d::GetMedium(const int i, Medium*& m) const {

  if (i < 0 || i >= nRegions) {
    std::cerr << className << "::GetMedium:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return false;
  }
  
  m = regions[i].medium;
  if (m == 0) return false;
  return true;

}

bool 
ComponentTcad2d::LoadData(const std::string datafilename) {

  std::ifstream datafile(datafilename.c_str(), std::ios::in);
  std::string line;
  std::istringstream data;
  
  std::vector<bool> isInRegion;
  isInRegion.resize(nVertices);
  std::vector<int> fillCount;
  fillCount.resize(nVertices);
  
  for (int i = nVertices; i--;) {
    fillCount[i] = 0;
    vertices[i].p = 0.;
    vertices[i].ex = 0.;
    vertices[i].ey = 0.;
    vertices[i].isShared = false;
  }
  
  std::string::size_type pBra, pKet, pEq;

  while (!datafile.fail()) {
    // Read one line
    std::getline(datafile, line);
    // Strip white space from beginning of line
    line.erase(line.begin(), std::find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    //Find data section
    if (line.substr(0, 8) == "function") {
      // Read type of data set
      pEq = line.find('=');
      if (pEq == std::string::npos) {
        // No "=" found
        std::cerr << className << "::LoadData:\n";
        std::cerr << "    Error reading file " << datafilename << "\n";
        datafile.close();
        Cleanup();  
        return false;
      }
      line = line.substr(pEq + 1);
      std::string dataset;
      data.str(line); data >> dataset; data.clear();
      if (dataset == "ElectrostaticPotential") {
        std::getline(datafile, line); std::getline(datafile, line);
        std::getline(datafile, line); std::getline(datafile, line);
        pBra = line.find('['); pKet = line.find(']');
        if (pKet < pBra || 
            pBra == std::string::npos || pKet == std::string::npos) {
          std::cerr << className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        std::string name; 
        data.str(line); data >> name; data.clear();
        int index = -1;
        for (int j = 0; j < nRegions; ++j) {
          if (name == regions[j].name) {
            index = j;
            break;
          }
        }
        if (index == -1) {
          std::cerr << className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Unknown region " << name << ".\n";
          continue;
        }
        std::getline(datafile, line);
        pBra = line.find('('); pKet = line.find(')');
        if (pKet < pBra || 
            pBra == std::string::npos || pKet == std::string::npos) {
          std::cerr << className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          datafile.close(); Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        int nValues;
        data.str(line); data >> nValues; data.clear();
        for (int j = nVertices; j--;) isInRegion[j] = false;
        for (int j = 0; j < nElements; ++j) {
          if (elements[j].region != index) continue;
          for (int k = 0; k <= elements[j].type; ++k) {
            isInRegion[elements[j].vertex[k]] = true;
          } 
        }
        int ivertex = 0;
        double val;
        for (int j = 0; j < nValues; ++j) {
          datafile >> val;
          while (ivertex < nVertices) {
            if (isInRegion[ivertex]) break;
            ++ivertex;
          }
          if (ivertex >= nVertices) {
            std::cerr << className << "::LoadData:\n";
            std::cerr << "    Error reading file " << datafilename << "\n";
            std::cerr << "    Dataset contains more values than "
                      << "there are vertices in region " 
                      << name << "\n";
          }
          vertices[ivertex].p = val;
          ++fillCount[ivertex];
          ++ivertex;
        }
      } else if (dataset == "ElectricField") {
        std::getline(datafile, line); std::getline(datafile, line);
        std::getline(datafile, line); std::getline(datafile, line);
        pBra = line.find('['); pKet = line.find(']');
        if (pKet < pBra || 
            pBra == std::string::npos || pKet == std::string::npos) {
          std::cerr << className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          datafile.close(); Cleanup();          
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        std::string name; 
        data.str(line); data >> name; data.clear();
        int index = -1;
        for (int j = 0; j < nRegions; ++j) {
          if (name == regions[j].name) {
            index = j;
            break;
          }
        }
        if (index == -1) {
          std::cerr << className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Unknown region " << name << ".\n";
          continue;
        }
        std::getline(datafile, line);
        pBra = line.find('('); pKet = line.find(')');
        if (pKet < pBra || 
            pBra == std::string::npos || pKet == std::string::npos) {
          std::cerr << className << "::LoadData\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          datafile.close(); Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        int nValues;
        data.str(line); data >> nValues; data.clear();
        nValues = nValues / 2;
        for (int j = nVertices; j--;) isInRegion[j] = false;
        for (int j = 0; j < nElements; ++j) {
          if (elements[j].region != index) continue;
          for (int k = 0; k <= elements[j].type; ++k) {
            isInRegion[elements[j].vertex[k]] = true;
          } 
        }
        int ivertex = 0;
        double val1, val2;
        for (int j = 0; j < nValues; ++j) {
          datafile >> val1 >> val2;
          while (ivertex < nVertices) {
            if (isInRegion[ivertex]) break;
            ++ivertex;
          }
          if (ivertex >= nVertices) {
            std::cerr << className << "::LoadData\n"
                      << "    Error reading file " << datafilename << "\n"
                      << "    Dataset contains more values than" 
                      << " there are vertices in region " 
                      << name << ".\n";
          }
          vertices[ivertex].ex = val1;
          vertices[ivertex].ey = val2;
          ++ivertex;
        }
      }
    }
  }
  if (datafile.fail() && !datafile.eof()) {
    std::cerr << className << "::LoadData\n";
    std::cerr << "    Error reading file " << datafilename << "\n";
    datafile.close(); Cleanup();
    return false;
  }
  
  for (int i = nVertices; i--;) {
    if (fillCount[i] > 1) vertices[i].isShared = true;
  }
    
  datafile.close();

  return true;
  
}

bool 
ComponentTcad2d::LoadGrid(const std::string gridfilename) {

  // Open the file containing the mesh description
  std::ifstream gridfile(gridfilename.c_str(), std::ios::in);
  std::string line;
  std::istringstream data;

  // Delete existing mesh information
  Cleanup();
  std::string::size_type pBra, pKet, pEq;
  // Count line numbers
  int i = 0;

  // Get the number of regions
  while (!gridfile.fail()) {
    // Read one line
    std::getline(gridfile, line); ++i;
    // Strip white space from beginning of line
    line.erase(line.begin(), find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Find entry 'nb_regions'
    if (line.substr(0, 10) == "nb_regions") {
      pEq = line.find('=');
      if (pEq == std::string::npos) {
        // No "=" sign found
        std::cerr << className << "::LoadGrid:\n"
                  << "    Could not read number of regions.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pEq + 1);
      data.str(line); data >> nRegions; data.clear();
      break;
    }
    if (gridfile.fail()) break;
  }
  if (gridfile.eof()) {
    // Reached end of file
    std::cerr << className << "::LoadGrid:\n"
              << "    Could not find entry 'nb_regions' in file " 
              << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    // Error reading from the file
    std::cerr << className << "::LoadGrid:\n"
              << "    Error reading file " << gridfilename 
              << " (line " << i << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }
  regions.resize(nRegions);
  
  // Get region names
  while (!gridfile.fail()) {
    std::getline(gridfile, line); ++i;
    line.erase(line.begin(), find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Find entry 'regions'
    if (line.substr(0, 7) == "regions") {
      // Get region names (given in brackets)
      pBra = line.find('['); pKet = line.find(']');
      if (pKet < pBra || 
          pBra == std::string::npos || pKet == std::string::npos) {
        // No closed brackets []
        std::cerr << className << "::LoadGrid:\n" 
                  << "    Could not read region names.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line);
      for (int j = 0; j < nRegions; ++j) {
        data >> regions[j].name;
        data.clear();
        // Assume by default that all regions are active
        regions[j].drift = true;
        regions[j].medium = 0;
      }
      break;
    }
  }
  if (gridfile.eof()) {
    // Reached end of file
    std::cerr << className << "::LoadGrid:\n"
              << "    Could not find entry 'regions' in file " 
              << gridfilename << ".\n";
    Cleanup();        
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    // Error reading from the file
    std::cerr << className << "::LoadGrid:\n"
              << "    Error reading file " << gridfilename 
              << " (line " << i << ").\n";
    Cleanup();        
    gridfile.close();
    return false;
  }
  
  // Get vertices
  while (!gridfile.fail()) {
    std::getline(gridfile, line); ++i;
    line.erase(line.begin(), find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Vertices'
    if (line.substr(0, 8) == "Vertices") {
      // Get number of vertices (given in brackets)
      pBra = line.find('('); pKet = line.find(')');
      if (pKet < pBra || 
          pBra == std::string::npos || pKet == std::string::npos) {
        // No closed brackets []
        std::cerr << className << "::LoadGrid:\n"
                  << "    Could not read number of vertices.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line); data >> nVertices; data.clear();
      vertices.resize(nVertices);
      // Get vertex coordinates
      for (int j = 0; j < nVertices; ++j) {
        gridfile >> vertices[j].x >> vertices[j].y;
        // Change units from micron to cm
        vertices[j].x *= 1.e-4;
        vertices[j].y *= 1.e-4;
        ++i;
      }
      break;
    }
  }
  if (gridfile.eof()) {
    // Reached end of file
    std::cerr << className << "::LoadGrid:\n"
              << "    Could not find section 'Vertices' in file " 
              << gridfilename << ".\n";
    Cleanup();        
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    // Error reading from file
    std::cerr << className << "::LoadGrid:\n"
              << "    Error reading file " << gridfilename 
              << " (line " << i << ").\n";
    Cleanup();        
    gridfile.close();
    return false;
  }
  
  // Get edges (connections between vertices)
  int nEdges = 0;
  // Temporary arrays for storing edge points
  std::vector<int> edgeP1;
  std::vector<int> edgeP2;
  while (!gridfile.fail()) {
    std::getline(gridfile, line); ++i;
    line.erase(line.begin(), find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Edges'
    if (line.substr(0, 5) == "Edges") {
      // Get number of edges (given in brackets)
      pBra = line.find('('); pKet = line.find(')');
      if (pKet < pBra || 
          pBra == std::string::npos || pKet == std::string::npos) {
        // No closed brackets ()
        std::cerr << className << "::LoadGrid:\n"
                  << "    Could not read number of edges.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line); data >> nEdges; data.clear();
      edgeP1.resize(nEdges);
      edgeP2.resize(nEdges);
      for (int j = 0; j < nEdges; ++j) {
        gridfile >> edgeP1[j] >> edgeP2[j];
        ++i;
      }
      break;
    }
  }
  if (gridfile.eof()) {
    // Reached end of file
    std::cerr << className << "::LoadGrid:\n"
              << "    Could not find section 'Edges' in file " 
              << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    // Error reading from file
    std::cerr << className << "::LoadGrid:\n" 
              << "    Error reading file " << gridfilename 
              << " (line " << i << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }  
  
  // Read elements
  int edge1, edge2, edge3, edge4;
  int type;
  while (!gridfile.fail()) {
    std::getline(gridfile, line); ++i;
    line.erase(line.begin(), find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Elements'
    if (line.substr(0, 8) == "Elements") {
      // Get number of elements (given in brackets)
      pBra = line.find('('); pKet = line.find(')');
      if (pKet < pBra || 
          pBra == std::string::npos || pKet == std::string::npos) {
        // No closed brackets ()
        std::cerr << className << "::LoadGrid:\n"
                  << "    Could not read number of elements.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line); data >> nElements; data.clear();
      // Resize vector of elements
      elements.resize(nElements);
      // Get type and constituting edges of each element
      for (int j = 0; j < nElements; ++j) {
        ++i;
        gridfile >> type;
        switch (type) {
          case 1:
            // Line
            gridfile >> edge1 >> edge2;
            break;
          case 2:
            // Triangle
            gridfile >> edge1 >> edge2 >> edge3;
            break;
          case 3:
            // Rectangle
            gridfile >> edge1 >> edge2 >> edge3 >> edge4;
            break;
          default:
            std::cerr << className << "::LoadGrid:\n" 
                      << "    Error reading file " << gridfilename 
                      << " (line " << i << ").\n";                      
            std::cerr << "    Invalid element type for 2d mesh.\n";
            Cleanup();        
            gridfile.close();
            return false;
            break;
        }
        elements[j].type = type;
        elements[j].region = -1;              
        if (edge1 >= 0) {
          elements[j].vertex[0] = edgeP1[edge1];
        } else {
          elements[j].vertex[0] = edgeP2[-edge1 - 1];
        }
        if (edge2 >= 0) {
          elements[j].vertex[1] = edgeP1[edge2];
        } else {
          elements[j].vertex[1] = edgeP2[-edge2 - 1];
        }
        if (type < 2) {
          while (vertices[elements[j].vertex[0]].x > 
                 vertices[elements[j].vertex[1]].x) {
            // Rearrange vertices such that point 0 is on the left
            edge1 = elements[j].vertex[0]; 
            elements[j].vertex[0] = elements[j].vertex[1]; 
            elements[j].vertex[1] = edge1;
          }
          continue;
        }
        if (edge3 >= 0) {
          elements[j].vertex[2] = edgeP1[edge3];
        } else {
          elements[j].vertex[2] = edgeP2[-edge3 - 1];
        }
        if (type < 3) {
          while (vertices[elements[j].vertex[0]].x > 
                 vertices[elements[j].vertex[1]].x || 
                 vertices[elements[j].vertex[0]].x > 
                 vertices[elements[j].vertex[2]].x) {
            // Rearrange vertices such that point 0 is on the left
            edge1 = elements[j].vertex[0];
            elements[j].vertex[0] = elements[j].vertex[1];
            elements[j].vertex[1] = elements[j].vertex[2];
            elements[j].vertex[2] = edge1;
          }
          continue;
       }
        if (edge4 >= 0) {
          elements[j].vertex[3] = edgeP1[edge4];
        } else {
          elements[j].vertex[3] = edgeP2[-edge4 - 1];
        }
        while (vertices[elements[j].vertex[0]].x > 
               vertices[elements[j].vertex[1]].x || 
               vertices[elements[j].vertex[0]].x > 
               vertices[elements[j].vertex[2]].x ||
               vertices[elements[j].vertex[0]].x > 
               vertices[elements[j].vertex[3]].x ) {
          // Rearrange vertices such that point 0 is on the left               
          edge1 = elements[j].vertex[0];
          elements[j].vertex[0] = elements[j].vertex[1];
          elements[j].vertex[1] = elements[j].vertex[2];
          elements[j].vertex[2] = elements[j].vertex[3];
          elements[j].vertex[3] = edge1;
        }
      }
      break;
    }
  }
  if (gridfile.eof()) {
    // Reached end of file
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Could not find section 'Elements' in file " 
              << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename 
              << " (line " << i << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }
  
  // Assign regions to elements
  std::string name; 
  while (!gridfile.fail()) {
    std::getline(gridfile, line);
    line.erase(line.begin(), find_if(line.begin(), line.end(), 
               not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Region'
    if (line.substr(0, 6) == "Region") {
      // Get region name (given in brackets)
      pBra = line.find('('); pKet = line.find(')');
      if (pKet < pBra || 
          pBra == std::string::npos || pKet == std::string::npos) {
        std::cerr << className << "::LoadGrid:\n";
        std::cerr << "    Could not read region name.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line); data >> name; data.clear();
      int index = -1;
      for (int j = 0; j < nRegions; ++j) {
        if (name == regions[j].name) {
          index = j;
          break;
        }
      }
      if (index == -1) {
        // Specified region name is not in the list
        std::cerr << className << "::LoadGrid:\n";
        std::cerr << "    Error reading file " 
                  << gridfilename << ".\n";
        std::cerr << "    Unknown region " << name << ".\n";
        continue;
      }
      std::getline(gridfile, line); std::getline(gridfile, line);
      pBra = line.find('('); pKet = line.find(')');
      if (pKet < pBra || 
          pBra == std::string::npos || pKet == std::string::npos) {
        // No closed brackets ()
        std::cerr << className << "::LoadGrid:\n";
        std::cerr << "    Error reading file " 
                  << gridfilename << ".\n";
        std::cerr << "    Could not read number of elements in region " 
                  << name << ".\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      int nelements;
      int ielement;
      data.str(line); data >> nelements; data.clear();
      for (int j = 0; j < nelements; ++j) {
        gridfile >> ielement;
        elements[ielement].region = index;
      } 
    }
  }
  
  gridfile.close();  
  if (gridfile.fail() && !gridfile.eof()) {
    std::cerr << className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << ".\n";
    Cleanup();
    return false;
  }
      
  return true;
  
}

void 
ComponentTcad2d::Cleanup() {

  // Vertices
  vertices.clear();
  nVertices = 0;
  
  // Elements
  elements.clear();
  nElements = 0;
  
  // Regions
  regions.clear();
  nRegions = 0;
  
}

bool 
ComponentTcad2d::CheckRectangle(const double x, const double y, const int i) {
 
  if (y < vertices[elements[i].vertex[0]].y) return false;
  if (x > vertices[elements[i].vertex[3]].x) return false;
  if (y > vertices[elements[i].vertex[1]].y) return false;
  
  // Map (x, y) onto local variables (u, v) in [-1, 1]
  const double u = (x - 0.5 * (vertices[elements[i].vertex[0]].x + 
                               vertices[elements[i].vertex[3]].x)) / 
      (vertices[elements[i].vertex[3]].x - vertices[elements[i].vertex[0]].x);
  const double v = (y - 0.5 * (vertices[elements[i].vertex[0]].y + 
                               vertices[elements[i].vertex[1]].y)) / 
      (vertices[elements[i].vertex[1]].y - vertices[elements[i].vertex[0]].y);
  // Compute weighting factors for each corner
  a = (0.5 - u) * (0.5 - v);
  b = (0.5 - u) * (0.5 + v);
  c = (0.5 + u) * (0.5 + v);
  d = (0.5 + u) * (0.5 - v);            
        
  return true;

}

bool 
ComponentTcad2d::CheckTriangle(const double x, const double y, const int i) {

  if (x > vertices[elements[i].vertex[1]].x && 
      x > vertices[elements[i].vertex[2]].x) return false;
  if (y < vertices[elements[i].vertex[0]].y &&
      y < vertices[elements[i].vertex[1]].y &&
      y < vertices[elements[i].vertex[2]].y) return false;
  if (y > vertices[elements[i].vertex[0]].y &&
      y > vertices[elements[i].vertex[1]].y &&
      y > vertices[elements[i].vertex[2]].y) return false;
  
  // Map (x, y) onto local variables (a, b) such that
  // P = A + b * (B - A) + c * (C - A)
  // A point P is inside the triangle ABC if b, c > 0 and b + c < 1
  // b, c are also weighting factors for points B, C
  const double v1x = vertices[elements[i].vertex[1]].x - 
                     vertices[elements[i].vertex[0]].x;
  const double v1y = vertices[elements[i].vertex[1]].y - 
                     vertices[elements[i].vertex[0]].y;
  const double v2x = vertices[elements[i].vertex[2]].x - 
                     vertices[elements[i].vertex[0]].x;
  const double v2y = vertices[elements[i].vertex[2]].y - 
                     vertices[elements[i].vertex[0]].y;
  
  b = ((x - vertices[elements[i].vertex[0]].x) * v2y + 
       (vertices[elements[i].vertex[0]].y - y) * v2x) / (v1x * v2y - v1y * v2x);
  if (b < 0. || b > 1.) return false;
  
  c = ((vertices[elements[i].vertex[0]].x - x) * v1y + 
       (y - vertices[elements[i].vertex[0]].y) * v1x) / (v1x * v2y - v1y * v2x);
  if (c < 0. || b + c > 1.) return false;
  
  // Weighting factor for point A
  a = 1. - b - c;
  
  return true;

}

bool 
ComponentTcad2d::CheckLine(const double x, const double y, const int i) {

  if (x > vertices[elements[i].vertex[1]].x) return false;
  if (y < vertices[elements[i].vertex[0]].y && 
      y < vertices[elements[i].vertex[1]].y) return false;
  if (y > vertices[elements[i].vertex[0]].y &&
      y > vertices[elements[i].vertex[1]].y) return false;
  const double v1x = vertices[elements[i].vertex[1]].x - 
                     vertices[elements[i].vertex[0]].x;
  const double v1y = vertices[elements[i].vertex[1]].y - 
                     vertices[elements[i].vertex[0]].y;
  if (v1y * (y - vertices[elements[i].vertex[0]].y) == 
      v1x * (x - vertices[elements[i].vertex[0]].x)) {
    // Compute weighting factors for endpoints A, B
    b = ((x - vertices[elements[i].vertex[0]].x) * v1x + 
         (y - vertices[elements[i].vertex[0]].y) * v1y) / 
        (v1x * v1x + v1y * v1y);
    a = 1. - b;
    return true;
  }
  return false;

}

void 
ComponentTcad2d::UpdatePeriodicity() {

  if (debug) {
    std::cerr << className << "::UpdatePeriodicity\n:";
    std::cerr << "    Periodicities are not supported.\n";
  }
  
}

}
