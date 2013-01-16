#include <iostream>
#include <cmath>

#include <TMarker.h>
#include <TEllipse.h>
#include <TLine.h>
#include <TPolyLine.h>
#include <TBuffer3D.h>
#include <TBuffer3DTypes.h>
#include <TVirtualViewer3D.h>

#include "ComponentAnalyticField.hh"
#include "Plotting.hh"
#include "ViewCell.hh"

namespace Garfield {

ViewCellWire::ViewCellWire(const double x, const double y, const double z, 
                           const double diameter, const double length,
                           const int type) :
  TObject(),
  x0(x), y0(y), z0(z), r(10.e-4), l(50.), wireType(type) {

  className = "ViewCellWire";
  if (diameter > 0.) {
    r = diameter / 2.;
  } else {
    std::cerr << className << ":\n";
    std::cerr << "    Unphysical diameter (" << diameter << ").\n";
  }

  if (length > 0.) {
    l = length / 2.;
  } else {
    std::cerr << className << ":\n";
    std::cerr << "    Unphysical length (" << length << ").\n";
  }

}

TBuffer3D&
ViewCellWire::GetBuffer(bool& ok, const bool debug) {

  ok = false;

  static TBuffer3DTube wire;
  wire.ClearSectionsValid();
  wire.fID = this;
  if (wireType == 0) {
    wire.fColor = kBlue;
  } else if (wireType == 1) {
    wire.fColor = kRed + 2;
  } else if (wireType == 2) {
    wire.fColor = kPink + 3;
  } else if (wireType == 3) {
    wire.fColor = kCyan + 3;
  } else {
    wire.fColor = kBlue + wireType;
  }
  wire.fTransparency = 0;
  wire.fLocalFrame = kTRUE;
  wire.SetLocalMasterIdentity();
  wire.fReflection = kFALSE;
  // Set the center.
  wire.fLocalMaster[12] = x0;
  wire.fLocalMaster[13] = y0;
  wire.fLocalMaster[14] = z0;
  wire.SetSectionsValid(TBuffer3D::kCore);

  // Set the bounding box.
  const double bb = sqrt(r * r + l * l);
  double origin[3] = {x0, y0, z0};
  double halfLength[3] = {bb, bb, bb};
  wire.SetAABoundingBox(origin, halfLength);
  wire.SetSectionsValid(TBuffer3D::kBoundingBox);
  
  wire.fRadiusInner = 0.;
  wire.fRadiusOuter = r;
  wire.fHalfLength = l;
  wire.SetSectionsValid(TBuffer3D::kShapeSpecific);
  ok = true; 
  if (debug) {
    std::cout << className << "::GetBuffer:\n";
    std::cout << "    Center: (" << x0 << ", " << y0 << ", " << z0 << ")\n";
    std::cout << "    Radii:  " << wire.fRadiusInner << " - " 
                                << wire.fRadiusOuter << "\n";
  }
  return wire;

}

ViewCellPlane::ViewCellPlane(const double center, const bool vert,
                             const double size) :
  TObject(),
  planeCenter(center), isVertical(vert), planeSize(10.) {

  className = "ViewCellPlane";
  if (size > 0.) {
    planeSize = size;
  } else {
    std::cerr << className << ":\n";
    std::cerr << "    Unphysical size (" << size << ").\n";
  }

}

TBuffer3D&
ViewCellPlane::GetBuffer(bool& ok, const bool debug) {

  ok = false;
  const int col = kGreen + 2;
  static TBuffer3D plane(TBuffer3DTypes::kGeneric);
  plane.ClearSectionsValid();
  plane.fID = this;
  plane.fColor = col;
  plane.fTransparency = 50;
  plane.fLocalFrame = kTRUE;
  plane.SetLocalMasterIdentity();
  plane.fReflection = kFALSE;
  // Set the center.
  double x0 = 0., y0 = 0., z0 = 0.;
  if (isVertical) {
    x0 += planeCenter;
  } else {
    y0 += planeCenter;
  }
  plane.fLocalMaster[12] = x0;
  plane.fLocalMaster[13] = y0;
  plane.fLocalMaster[14] = z0;
  plane.SetSectionsValid(TBuffer3D::kCore);

  // Set the bounding box.
  const double bb = planeSize;
  double origin[3] = {x0, y0, z0};
  double halfLength[3] = {bb, bb, bb};
  plane.SetAABoundingBox(origin, halfLength);
  plane.SetSectionsValid(TBuffer3D::kBoundingBox);

  plane.SetRawSizes(8, 3 * 8, 12, 3 * 12, 6, 6 * 6);
  plane.SetSectionsValid(TBuffer3D::kRawSizes);
  // Points
  double dx = 0., dy = 0., dz = 0.;
  const double planeWidth = 0.01 * planeSize;
  if (isVertical) {
    dy = dz = planeSize;
    dx = planeWidth;
  } else {
    dx = dz = planeSize;
    dy = planeWidth;
  }
  // Points
  plane.fPnts[ 0] = -dx; plane.fPnts[ 1] = -dy; plane.fPnts[ 2] = -dz; 
  plane.fPnts[ 3] = +dx; plane.fPnts[ 4] = -dy; plane.fPnts[ 5] = -dz; 
  plane.fPnts[ 6] = +dx; plane.fPnts[ 7] = +dy; plane.fPnts[ 8] = -dz; 
  plane.fPnts[ 9] = -dx; plane.fPnts[10] = +dy; plane.fPnts[11] = -dz; 
  plane.fPnts[12] = -dx; plane.fPnts[13] = -dy; plane.fPnts[14] = +dz; 
  plane.fPnts[15] = +dx; plane.fPnts[16] = -dy; plane.fPnts[17] = +dz; 
  plane.fPnts[18] = +dx; plane.fPnts[19] = +dy; plane.fPnts[20] = +dz; 
  plane.fPnts[21] = -dx; plane.fPnts[22] = +dy; plane.fPnts[23] = +dz;
      
  // Segments
  plane.fSegs[ 0] = col; plane.fSegs[ 1] = 0; plane.fSegs[ 2] = 1; 
  plane.fSegs[ 3] = col; plane.fSegs[ 4] = 1; plane.fSegs[ 5] = 2; 
  plane.fSegs[ 6] = col; plane.fSegs[ 7] = 2; plane.fSegs[ 8] = 3; 
  plane.fSegs[ 9] = col; plane.fSegs[10] = 3; plane.fSegs[11] = 0; 
  plane.fSegs[12] = col; plane.fSegs[13] = 4; plane.fSegs[14] = 5; 
  plane.fSegs[15] = col; plane.fSegs[16] = 5; plane.fSegs[17] = 6; 
  plane.fSegs[18] = col; plane.fSegs[19] = 6; plane.fSegs[20] = 7; 
  plane.fSegs[21] = col; plane.fSegs[22] = 7; plane.fSegs[23] = 4; 
  plane.fSegs[24] = col; plane.fSegs[25] = 0; plane.fSegs[26] = 4; 
  plane.fSegs[27] = col; plane.fSegs[28] = 1; plane.fSegs[29] = 5; 
  plane.fSegs[30] = col; plane.fSegs[31] = 2; plane.fSegs[32] = 6; 
  plane.fSegs[33] = col; plane.fSegs[34] = 3; plane.fSegs[35] = 7; 

  // Polygons
  plane.fPols[ 0] = col; plane.fPols[ 1] = 4;  plane.fPols[ 2] = 8;
  plane.fPols[ 3] = 4  ; plane.fPols[ 4] = 9;  plane.fPols[ 5] = 0;
  plane.fPols[ 6] = col; plane.fPols[ 7] = 4;  plane.fPols[ 8] = 9;
  plane.fPols[ 9] = 5  ; plane.fPols[10] = 10; plane.fPols[11] = 1;
  plane.fPols[12] = col; plane.fPols[13] = 4;  plane.fPols[14] = 10;
  plane.fPols[15] = 6  ; plane.fPols[16] = 11; plane.fPols[17] = 2;
  plane.fPols[18] = col; plane.fPols[19] = 4;  plane.fPols[20] = 11;
  plane.fPols[21] = 7  ; plane.fPols[22] = 8;  plane.fPols[23] = 3;
  plane.fPols[24] = col; plane.fPols[25] = 4;  plane.fPols[26] = 1;
  plane.fPols[27] = 2  ; plane.fPols[28] = 3;  plane.fPols[29] = 0;
  plane.fPols[30] = col; plane.fPols[31] = 4;  plane.fPols[32] = 7;
  plane.fPols[33] = 6  ; plane.fPols[34] = 5;  plane.fPols[35] = 4;
  plane.SetSectionsValid(TBuffer3D::kRaw);
  if (debug) {
    std::cout << className << "::GetBuffer:\n";
    std::cout << "    Center: (" << x0 << ", " << y0 << ", " << z0 << ")\n";
  }
  ok = true; 
  return plane;

}

ViewCellTube::ViewCellTube(const double x, const double y, const double z, 
                           const double radius, const int nEdges) :
  TObject(),
  x0(x), y0(y), z0(z), r(1.), n(0) {

  className = "ViewCellTube";
  if (radius > 0.) {
    r = radius;
  } else {
    std::cerr << className << ":\n";
    std::cerr << "    Unphysical radius (" << radius << ").\n";
  }

  if (nEdges < 3 && nEdges != 0) {
    std::cerr << className << ":\n";
    std::cerr << "    Unphysical number of edges (" << nEdges << ").\n";
  } else {
    n = nEdges;
  }

}

TBuffer3D&
ViewCellTube::GetBuffer(bool& ok, const bool debug) {

  if (n == 0) return GetBufferCylinder(ok, debug);
  return GetBufferPolygon(ok, debug);

}

TBuffer3D&
ViewCellTube::GetBufferCylinder(bool& ok, const bool debug) {

  ok = false;
  static TBuffer3DTube tube;
  tube.ClearSectionsValid();
  tube.fID = this;
  tube.fColor = kGreen + 2;
  tube.fTransparency = 50;
  tube.fLocalFrame = kTRUE;
  tube.SetLocalMasterIdentity();
  tube.fReflection = kFALSE;
  // Set the center.
  tube.fLocalMaster[12] = x0;
  tube.fLocalMaster[13] = y0;
  tube.fLocalMaster[14] = z0;
  tube.SetSectionsValid(TBuffer3D::kCore);
  // Estimate the length.
  const double l = 3 * r;
  // Set the bounding box.
  const double bb = sqrt(r * r + l * l);
  double origin[3] = {x0, y0, z0};
  double halfLength[3] = {bb, bb, bb};
  tube.SetAABoundingBox(origin, halfLength);
  tube.SetSectionsValid(TBuffer3D::kBoundingBox);
  
  tube.fRadiusInner = 0.98 * r;
  tube.fRadiusOuter = 1.02 * r;
  tube.fHalfLength = l;
  tube.SetSectionsValid(TBuffer3D::kShapeSpecific);
  if (debug) {
    std::cout << className << "::GetBufferCylinder:\n";
    std::cout << "    Center: (" << x0 << ", " << y0 << ", " << z0 << ")\n";
    std::cout << "    Radii:  " << tube.fRadiusInner << " - " 
                                << tube.fRadiusOuter << "\n";
  }
  ok = true; 
  return tube;

}

TBuffer3D&
ViewCellTube::GetBufferPolygon(bool& ok, const bool debug) {

  const int col = kGreen + 2;
  static TBuffer3D tube(TBuffer3DTypes::kGeneric);
  tube.ClearSectionsValid();
  tube.fID = this;
  tube.fColor = col;
  tube.fTransparency = 50;
  tube.fLocalFrame = kTRUE;
  tube.SetLocalMasterIdentity();
  tube.fReflection = kFALSE;
  // Set the center.
  tube.fLocalMaster[12] = x0;
  tube.fLocalMaster[13] = y0;
  tube.fLocalMaster[14] = z0;
  tube.SetSectionsValid(TBuffer3D::kCore);
  // Estimate the length.
  const double l = 3 * r;
  // Set the bounding box.
  const double bb = sqrt(r * r + l * l);
  double origin[3] = {x0, y0, z0};
  double halfLength[3] = {bb, bb, bb};
  tube.SetAABoundingBox(origin, halfLength);
  tube.SetSectionsValid(TBuffer3D::kBoundingBox);
  
  // Raw sizes
  // 4 * n points (3 coordinates)
  // 8 * n segments (3 components: color, start point, end point
  // 4 * n polygons (6 components: color, segment count, 4 segment indices)
  tube.SetRawSizes(4 * n, 4 * 3 * n, 8 * n, 8 * 3 * n, 4 * n, 4 * 6 * n);
  tube.SetSectionsValid(TBuffer3D::kRawSizes);
  // Points
  const double w = 0.01;
  for (int i = 0; i < n; ++i) {
    double x1 = (1. - w) * r * cos(i * TwoPi / double(n));
    double y1 = (1. - w) * r * sin(i * TwoPi / double(n));
    double x2 = (1. + w) * r * cos(i * TwoPi / double(n));
    double y2 = (1. + w) * r * sin(i * TwoPi / double(n));
    // Inner points
    // Front, index i
    tube.fPnts[3 * i]     = x1;
    tube.fPnts[3 * i + 1] = y1;
    tube.fPnts[3 * i + 2] = -l;
    // Back, index n + i
    tube.fPnts[3 * (i + n)]     = x1;
    tube.fPnts[3 * (i + n) + 1] = y1;
    tube.fPnts[3 * (i + n) + 2] = +l;
    // Outer points
    // Front, index 2 * n + i
    tube.fPnts[3 * (i + 2 * n)]     = x2;
    tube.fPnts[3 * (i + 2 * n) + 1] = y2;
    tube.fPnts[3 * (i + 2 * n) + 2] = -l;
    // Back, index 3 * n + i
    tube.fPnts[3 * (i + 3 * n)]     = x2;
    tube.fPnts[3 * (i + 3 * n) + 1] = y2;
    tube.fPnts[3 * (i + 3 * n) + 2] = +l; 
  }
  // Segments
  for (int i = 0; i < n; ++i) {
    tube.fSegs[3 *  i         ] = col;
    tube.fSegs[3 * (i +     n)] = col;
    tube.fSegs[3 * (i + 2 * n)] = col;
    tube.fSegs[3 * (i + 3 * n)] = col;
    tube.fSegs[3 * (i + 4 * n)] = col;
    tube.fSegs[3 * (i + 5 * n)] = col;
    tube.fSegs[3 * (i + 6 * n)] = col;
    tube.fSegs[3 * (i + 7 * n)] = col;

    // Connect same-side points at constant radius.
    // Indices: i         -> inner, front
    //          i + n     -> inner, back
    //          i + 3 * n -> outer, front
    //          i + 4 * n -> outer, back
    tube.fSegs[3 *  i          + 1] = i;
    tube.fSegs[3 * (i +     n) + 1] = i + n;
    tube.fSegs[3 * (i + 3 * n) + 1] = i + 2 * n;
    tube.fSegs[3 * (i + 4 * n) + 1] = i + 3 * n;

    if (i == n - 1) {
      tube.fSegs[3 *  i          + 2] = 0;
      tube.fSegs[3 * (i +     n) + 2] = n;
      tube.fSegs[3 * (i + 3 * n) + 2] = 2 * n;
      tube.fSegs[3 * (i + 4 * n) + 2] = 3 * n;
    } else {
      tube.fSegs[3 *  i          + 2] = i         + 1;
      tube.fSegs[3 * (i +     n) + 2] = i +     n + 1;
      tube.fSegs[3 * (i + 3 * n) + 2] = i + 2 * n + 1;
      tube.fSegs[3 * (i + 4 * n) + 2] = i + 3 * n + 1;
    }
    // Connect opposite points at same radius.
    // Indices: 2 * n + i -> inner
    //          5 * n + i -> outer 
    tube.fSegs[3 * (i + 2 * n) + 1] = i;
    tube.fSegs[3 * (i + 2 * n) + 2] = i +     n;
    tube.fSegs[3 * (i + 5 * n) + 1] = i + 2 * n;
    tube.fSegs[3 * (i + 5 * n) + 2] = i + 3 * n;
    // Connect same-side points at constant phi.
    // Indices: 6 * n + i -> front
    //          7 * n + i -> back
    tube.fSegs[3 * (i + 6 * n) + 1] = i;
    tube.fSegs[3 * (i + 6 * n) + 2] = i + 2 * n;
    tube.fSegs[3 * (i + 7 * n) + 1] = i +     n;
    tube.fSegs[3 * (i + 7 * n) + 2] = i + 3 * n;
  }
  if (debug) {
    std::cout << className << "::GetBufferPolygon:\n";
    std::cout << "    Segment      Start      End\n";
    for (int i = 0; i < 8 * n; ++i) {
      std::cout << "      " << i 
                << "      " << tube.fSegs[3 * i + 1] 
                << "      " << tube.fSegs[3 * i + 2] << "\n";
    }
  }
  // Polygons
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < 4; ++j) {
      tube.fPols[6 * (j * n + i)] = col;
      tube.fPols[6 * (j * n + i) + 1] = 4;
    }

    tube.fPols[6 *  i      + 2] = i;
    tube.fPols[6 * (i + n) + 2] = i + 3 * n;
    if (i == n - 1) {
      tube.fPols[6 *  i      + 3] = 2 * n;
      tube.fPols[6 * (i + n) + 3] = 5 * n;
    } else {
      tube.fPols[6 *  i      + 3] = 2 * n + i + 1;
      tube.fPols[6 * (i + n) + 3] = 5 * n + i + 1;  
    }
    tube.fPols[6 *  i      + 4] = i + n;
    tube.fPols[6 * (i + n) + 4] = i + 4 * n;
    tube.fPols[6 *  i      + 5] = 2 * n + i;
    tube.fPols[6 * (i + n) + 5] = 5 * n + i;
  
    tube.fPols[6 * (i + 2 * n) + 2] = i;
    tube.fPols[6 * (i + 3 * n) + 2] = i + n;
    if (i == n - 1) {
      tube.fPols[6 * (i + 2 * n) + 3] = 6 * n;
      tube.fPols[6 * (i + 3 * n) + 3] = 7 * n;
    } else {
      tube.fPols[6 * (i + 2 * n) + 3] = 6 * n + i + 1;
      tube.fPols[6 * (i + 3 * n) + 3] = 7 * n + i + 1;
    }
    tube.fPols[6 * (i + 2 * n) + 4] = i + 3 * n;
    tube.fPols[6 * (i + 3 * n) + 4] = i + 4 * n; 
    tube.fPols[6 * (i + 2 * n) + 5] = i + 6 * n;
    tube.fPols[6 * (i + 3 * n) + 5] = i + 7 * n;
  }
  if (debug) {
    std::cout << className << "::GetBufferPolygon:\n";
    std::cout << "    Polygon      S1      S2      S3      S4\n";
    for (int i = 0; i < 4 * n; ++i) {
      std::cout << "      " << i 
                << "      " << tube.fPols[6 * i + 2] 
                << "      " << tube.fPols[6 * i + 3] 
                << "      " << tube.fPols[6 * i + 4]
                << "      " << tube.fPols[6 * i + 5] << "\n";
    }
  }
  tube.SetSectionsValid(TBuffer3D::kRaw);     
  ok = true; 
  return tube;

}

ViewCell::ViewCell() :
  className("ViewCell"), debug(false), useWireMarker(true),
  label("Cell Layout"),
  canvas(0), hasExternalCanvas(false),
  hasUserArea(false),
  xMin(-1.), yMin(-1.), zMin(-1.), 
  xMax( 1.), yMax( 1.), zMax( 1.),
  component(0) {

  plottingEngine.SetDefaultStyle();
  nWires3d = 0;
  nPlanes3d = 0;
  nTubes3d = 0;
  wires3d.clear();
  planes3d.clear();
  tubes3d.clear();
  
}

ViewCell::~ViewCell() {

  if (!hasExternalCanvas && canvas != 0) delete canvas;

}

void
ViewCell::SetComponent(ComponentAnalyticField* comp) {

  if (comp == 0) {
    std::cerr << className << "::SetComponent:\n";
    std::cerr << "    Component pointer is null.\n";
    return;
  }

  component = comp;

} 
  
void
ViewCell::SetCanvas(TCanvas* c) {

  if (c == 0) return;
  if (!hasExternalCanvas && canvas != 0) {
    delete canvas;
    canvas = 0;
  }
  canvas = c;
  hasExternalCanvas = true;

}

void 
ViewCell::SetArea(double xmin, double ymin, double zmin, 
                   double xmax, double ymax, double zmax) {

  // Check range, assign if non-null
  if (xmin == xmax || ymin == ymax || zmin == zmax) {
    std::cout << className << "::SetArea:\n";
    std::cout << "    Null area range not permitted.\n";
    return;
  }
  xMin = std::min(xmin, xmax);
  yMin = std::min(ymin, ymax);
  zMin = std::min(zmin, zmax);
  xMax = std::max(xmin, xmax);
  yMax = std::max(ymin, ymax);
  zMax = std::max(zmin, zmax);
  hasUserArea = true;
 
}

void
ViewCell::SetArea() {

  hasUserArea = false;

}

void
ViewCell::Plot2d() {

  if (!Plot(false)) {
    std::cerr << className << "::Plot2d:\n";
    std::cerr << "    Error creating 2d plot.\n";
  }

}

void
ViewCell::Plot3d() {

  if (!Plot(true)) {
    std::cerr << className << "::Plot3d:\n";
    std::cerr << "    Error creating 3d plot.\n";
  }

}

bool
ViewCell::Plot(const bool use3d) {

  if (component == 0) {
    std::cerr << className << "::Plot:\n";
    std::cerr << "    Component is not defined.\n";
    return false;
  }
  
  double pmin = 0., pmax = 0.;
  if (!component->GetVoltageRange(pmin, pmax)) {
    std::cerr << className << "::Plot:\n";
    std::cerr << "    Component is not ready.\n";
    return false;
  }

  // Get the bounding box
  double x0 = xMin, y0 = yMin, z0 = zMin;
  double x1 = xMax, y1 = yMax, z1 = zMax;
  if (!hasUserArea) {
    if (!component->GetBoundingBox(x0, y0, z0, x1, y1, z1)) {
      std::cerr << className << "::Plot:\n";
      std::cerr << "    Bounding box cannot be determined.\n";
      std::cerr << "    Call SetArea first.\n";
      return false;
    }
  }

  if (canvas == 0) {
    canvas = new TCanvas();
    if (!use3d) canvas->SetTitle(label.c_str());
    if (hasExternalCanvas) hasExternalCanvas = false;
  }
  if (!use3d) {
    canvas->Range(x0 - 0.1 * (x1 - x0), y0 - 0.1 * (y1 - y0), 
                  x1 + 0.1 * (x1 - x0), y1 + 0.1 * (y1 - y0));
  }
  canvas->cd();

  // Get the cell type.
  std::string cellType = component->GetCellType();

  // Get the periodicities.
  double sx = 0., sy = 0.;
  const bool perX = component->GetPeriodicityX(sx);
  const bool perY = component->GetPeriodicityY(sy);
  // Determine the number of periods present in the cell.
  int nMaxX = 0, nMinX = 0;
  int nMaxY = 0, nMinY = 0;
  if (perX) {
    nMinX = int(x0 / sx) - 1;
    nMaxX = int(x1 / sx) + 1;
  }
  if (perY) {
    nMinY = int(y0 / sy) - 1;
    nMaxY = int(y1 / sy) + 1;
  }

  wires3d.clear();
  nWires3d = 0;
  planes3d.clear();
  nPlanes3d = 0;
  tubes3d.clear();
  nTubes3d = 0;

  // Get the number of wires.
  int nWires = component->GetNumberOfWires();
  int nWireTypes = 0;
  std::vector<std::string> wireTypes;
  wireTypes.clear();

  // Loop over the wires.
  for (int i = nWires; i--;) {
    double xw = 0., yw = 0., dw = 0., vw = 0., lw = 0., qw = 0.;
    std::string lbl;
    int type = -1;
    int nTrap;
    component->GetWire(i, xw, yw, dw, vw, lbl, lw, qw, nTrap);
    // Check if other wires with the same label already exist.
    if (nWireTypes == 0) {
      wireTypes.push_back(lbl);
      type = 0;
      ++nWireTypes;
    } else {
      for (int j = nWireTypes; j--;) {
        if (lbl == wireTypes[j]) {
          type = j;
          break;
        }
      }
      if (type < 0) {
        wireTypes.push_back(lbl);
        type = nWireTypes;
        ++nWireTypes;
      }
    }
    for (int nx = nMinX; nx <= nMaxX; ++nx) {
      for (int ny = nMinY; ny <= nMaxY; ++ny) {
        double x = xw + nx * sx;
        double y = yw + ny * sy;
        if (x + 0.5 * dw <= x0 ||
            x - 0.5 * dw >= x1 ||
            y + 0.5 * dw <= y0 ||
            y - 0.5 * dw >= y1) {
          continue;
        }
        if (use3d) {
          ViewCellWire newWire(x, y, 0., dw, lw, type);
          wires3d.push_back(newWire);
          ++nWires3d;
        } else {
          PlotWire(x, y, dw, type);
        }
      }
    }
  }

  // Draw lines at the positions of the x planes.
  int nPlanesX = component->GetNumberOfPlanesX();
  for (int i = nPlanesX; i--;) {
    double xp = 0., vp = 0.;
    std::string lbl;
    component->GetPlaneX(i, xp, vp, lbl);
    for (int nx = nMinX; nx <= nMaxX; ++nx) {
      double x = xp + nx * sx;
      if (x < x0 || x > x1) continue;
      if (use3d) {
        ViewCellPlane newPlane(x, true, (y1 - y0) / 2.);
        planes3d.push_back(newPlane);
        ++nPlanes3d; 
      } else {
        PlotLine(x, y0, x, y1);
      }
    }
  }
  
  // Draw lines at the positions of the y planes.
  int nPlanesY = component->GetNumberOfPlanesY();
  for (int i = nPlanesY; i--;) {
    double yp = 0., vp = 0.;
    std::string lbl;
    component->GetPlaneY(i, yp, vp, lbl);
    for (int ny = nMinY; ny <= nMaxY; ++ny) {
      double y = yp + ny * sy;
      if (y < y0 || y > y1) continue;
      if (use3d) {
        ViewCellPlane newPlane(y, false, (x1 - x0) / 2.);
        planes3d.push_back(newPlane);
        ++nPlanes3d;
      } else {
        PlotLine(x0, y, x1, y);
      }
    }
  }

  double rt = 0., vt = 0.;
  int nt = 0;
  std::string lbl;
  if (component->GetTube(rt, vt, nt, lbl)) {
    if (use3d) {
      ViewCellTube newTube(0., 0., 0., rt, nt);
      tubes3d.push_back(newTube);
      ++nTubes3d;
    } else {
      PlotTube(0., 0., rt, nt);
    }
  }

  if (use3d) {
    Draw("ogl");
  } else {
    canvas->Update();
  }

  return true;

}

void
ViewCell::Paint(Option_t*) {

  if (nWires3d <= 0 && nTubes3d <= 0 && nPlanes3d <= 0) {
    std::cerr << className << "::Paint:\n";
    std::cerr << "    There is nothing to paint.\n";
    return;
  }

  TVirtualViewer3D* viewer = gPad->GetViewer3D();
  viewer->BeginScene();

  if (nWires3d > 0) {
    for (int i = nWires3d; i--;) {
      bool ok = false;
      TBuffer3D& buffer = wires3d[i].GetBuffer(ok, debug);
      int req = viewer->AddObject(buffer);
      if (req != TBuffer3D::kNone) {
        std::cerr << className << "::Paint:\n";
        std::cerr << "    Could not pass wire object to viewer.\n";
      }
    }
  }

  if (nPlanes3d > 0) {
    for (int i = nPlanes3d; i--;) {
      bool ok = false;
      TBuffer3D& buffer = planes3d[i].GetBuffer(ok, debug);
      int req = viewer->AddObject(buffer);
      if (req != TBuffer3D::kNone) {
        std::cerr << className << "::Paint:\n";
        std::cerr << "    Could not pass plane object to viewer.\n";
      }
    }
  }

  if (nTubes3d > 0) {
    for (int i = nTubes3d; i--;) {
      bool ok = false;
      TBuffer3D& buffer = tubes3d[i].GetBuffer(ok, debug);
      int req = viewer->AddObject(buffer);
      if (req != TBuffer3D::kNone) {
        std::cerr << className << "::Paint:\n";
        std::cerr << "    Could not pass tube object to viewer.\n";
      }
    }
  }

  viewer->EndScene();

}

void
ViewCell::Draw(Option_t* option) {

  TObject::Draw(option);
  gPad->GetViewer3D(option);

}

void
ViewCell::PlotWire(const double x, const double y, const double d,
                   const int type) {

  if (useWireMarker) {
    int markerStyle = 1;
    if (type == 0) {
      markerStyle = kFullCircle;
    } else if (type == 1) {
      markerStyle = kOpenCircle;
    } else if (type == 2) {
      markerStyle = kFullSquare;
    } else if (type == 3) {
      markerStyle = kOpenSquare;
    } else {
      markerStyle = 26 + type;
    }
    TMarker* marker = new TMarker(x, y, markerStyle);
    marker->Draw("P");
    return;
  }
  
  TEllipse* circle = new TEllipse(x, y, 0.5 * d);
  circle->Draw("");

}

void
ViewCell::PlotLine(const double x0, const double y0,
                   const double x1, const double y1) {

  TLine* line = new TLine(x0, y0, x1, y1);
  line->Draw("");

}

void
ViewCell::PlotTube(const double x0, const double y0,
                   const double r, const int n) {

  if (n <= 0) {
    TEllipse* circle = new TEllipse(x0, y0, r);
    circle->SetFillStyle(0);
    circle->Draw("");
    return;
  }

  TPolyLine* pline = new TPolyLine(n + 1);
  for (int i = 0; i <= n; ++i) {
    double x = x0 + r * cos(i * TwoPi / double(n));
    double y = y0 + r * sin(i * TwoPi / double(n));
    pline->SetPoint(i, x, y);
  }
  pline->Draw("");
  
}

}
