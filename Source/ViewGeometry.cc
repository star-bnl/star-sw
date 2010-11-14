#include <iostream>

#include <TBuffer3D.h>
#include <TBuffer3DTypes.h>
#include <TVirtualViewer3D.h>

#include "GeometrySimple.hh"
#include "Solid.hh"
#include "Plotting.hh"
#include "ViewGeometry.hh"

namespace Garfield {

ViewGeometryShape::ViewGeometryShape() :
  TObject(),
  solid(0), col(kBlue) {

}

void
ViewGeometryShape::SetSolid(Solid* s) {

  if (s == 0) {
    std::cerr << "ViewGeometryShape::SetSolid:\n";
    std::cerr << "    Solid pointer is null.\n";
    return;
  }

  solid = s;

}

void
ViewGeometryShape::SetColor(const int color) {

  col = color;

}

TBuffer3D& 
ViewGeometryShape::GetBuffer(bool& ok) {

  ok = false;

  static TBuffer3D buffer(TBuffer3DTypes::kGeneric);
  buffer.ClearSectionsValid();
  buffer.fID = this;
  buffer.fColor = col;
  buffer.fTransparency = 0;
  buffer.fLocalFrame = kFALSE;
  buffer.SetLocalMasterIdentity();
  buffer.fReflection = kFALSE;
  buffer.SetSectionsValid(TBuffer3D::kCore);

  // Make sure the shape has been set.
  if (solid == 0) {
    std::cerr << "ViewGeometryShape::GetBuffer:\n";
    std::cerr << "    Solid is not defined.\n";
    return buffer;
  }

  // Get the bounding box.
  double bbxmin, bbymin, bbzmin;
  double bbxmax, bbymax, bbzmax;
  if (!solid->GetBoundingBox(bbxmin, bbymin, bbzmin,
                             bbxmax, bbymax, bbzmax)) {
    std::cerr << "ViewGeometryShape::GetBuffer:\n";
    std::cerr << "    Could not determine bounding box.\n";
    return buffer;
  }
  double origin[3] = {0.5 * (bbxmin + bbxmax),
                      0.5 * (bbymin + bbymax),
                      0.5 * (bbzmin + bbzmax)};
  double halfLength[3] = {0.5 * (bbxmax - bbxmin),
                          0.5 * (bbymax - bbymin),
                          0.5 * (bbzmax - bbzmin)};
  buffer.SetAABoundingBox(origin, halfLength);
  buffer.SetSectionsValid(TBuffer3D::kBoundingBox);

  if (solid->IsTube()) {
    static TBuffer3DTube tube;
    tube.ClearSectionsValid();
    tube.fID = this;
    tube.fColor = col;
    tube.fTransparency = 0;
    tube.fLocalFrame = kFALSE;
    tube.SetLocalMasterIdentity();
    tube.fReflection = kFALSE;
    tube.SetSectionsValid(TBuffer3D::kCore);
  
    tube.SetAABoundingBox(origin, halfLength);
    tube.SetSectionsValid(TBuffer3D::kBoundingBox);

    double rmin, rmax, lz;
    if (!solid->GetDimensions(rmin, rmax, lz)) {
      std::cerr << "ViewGeometryShape::GetBuffer:\n";
      std::cerr << "    Could not determine tube dimensions.\n";
      return tube;
    }
    tube.fRadiusInner = rmin;
    tube.fRadiusOuter = rmax;
    tube.fHalfLength = lz;
    tube.SetSectionsValid(TBuffer3D::kShapeSpecific);
    ok = true;
    return tube;
  } else if (solid->IsBox()) {
    buffer.SetRawSizes(8, 3 * 8, 12, 3 * 12, 6, 6 * 6);
    buffer.SetSectionsValid(TBuffer3D::kRawSizes);
    double dx, dy, dz;
    double x0, y0, z0;
    if (!solid->GetDimensions(dx, dy, dz) || 
        !solid->GetCenter(x0, y0, z0)) {
      std::cerr << "ViewGeometryShape::GetBuffer:\n";
      std::cerr << "    Could not determine box dimensions.\n";
      return buffer;
    }
    // Points (8)
    // 3 components: x,y,z
    buffer.fPnts[ 0] = x0 - dx; 
    buffer.fPnts[ 1] = y0 - dy; buffer.fPnts[ 2] = z0 - dz; // 0
    buffer.fPnts[ 3] = x0 + dx; 
    buffer.fPnts[ 4] = y0 - dy; buffer.fPnts[ 5] = z0 - dz; // 1
    buffer.fPnts[ 6] = x0 + dx; 
    buffer.fPnts[ 7] = y0 + dy; buffer.fPnts[ 8] = z0 - dz; // 2
    buffer.fPnts[ 9] = x0 - dx; 
    buffer.fPnts[10] = y0 + dy; buffer.fPnts[11] = z0 - dz; // 3
    buffer.fPnts[12] = x0 - dx; 
    buffer.fPnts[13] = y0 - dy; buffer.fPnts[14] = z0 + dz; // 4
    buffer.fPnts[15] = x0 + dx; 
    buffer.fPnts[16] = y0 - dy; buffer.fPnts[17] = z0 + dz; // 5
    buffer.fPnts[18] = x0 + dx; 
    buffer.fPnts[19] = y0 + dy; buffer.fPnts[20] = z0 + dz; // 6
    buffer.fPnts[21] = x0 - dx; 
    buffer.fPnts[22] = y0 + dy; buffer.fPnts[23] = z0 + dz; // 7

    // Segments (12)
    // 3 components: segment color (ignored), 
    //               start point index, end point index
    // Indices reference the above points
    buffer.fSegs[ 0] = col; buffer.fSegs[ 1] = 0; buffer.fSegs[ 2] = 1; // 0
    buffer.fSegs[ 3] = col; buffer.fSegs[ 4] = 1; buffer.fSegs[ 5] = 2; // 1
    buffer.fSegs[ 6] = col; buffer.fSegs[ 7] = 2; buffer.fSegs[ 8] = 3; // 2
    buffer.fSegs[ 9] = col; buffer.fSegs[10] = 3; buffer.fSegs[11] = 0; // 3
    buffer.fSegs[12] = col; buffer.fSegs[13] = 4; buffer.fSegs[14] = 5; // 4
    buffer.fSegs[15] = col; buffer.fSegs[16] = 5; buffer.fSegs[17] = 6; // 5
    buffer.fSegs[18] = col; buffer.fSegs[19] = 6; buffer.fSegs[20] = 7; // 6
    buffer.fSegs[21] = col; buffer.fSegs[22] = 7; buffer.fSegs[23] = 4; // 7
    buffer.fSegs[24] = col; buffer.fSegs[25] = 0; buffer.fSegs[26] = 4; // 8
    buffer.fSegs[27] = col; buffer.fSegs[28] = 1; buffer.fSegs[29] = 5; // 9
    buffer.fSegs[30] = col; buffer.fSegs[31] = 2; buffer.fSegs[32] = 6; // 10
    buffer.fSegs[33] = col; buffer.fSegs[34] = 3; buffer.fSegs[35] = 7; // 11
 
    // Polygons (6)
    // 5 + (2 + n) components: polygon color (ignored), 
    //                         segment count (n = 3+),
    //                         seg1, seg2 .... segn index
    // Segment indices refer to the above 12 segments
    // Here n = 4 - each polygon defines a rectangle - 4 sides.
    buffer.fPols[ 0] = col; buffer.fPols[ 1] =  4; buffer.fPols[ 2] =  8; // 0
    buffer.fPols[ 3] = 4;   buffer.fPols[ 4] =  9; buffer.fPols[ 5] =  0;
    buffer.fPols[ 6] = col; buffer.fPols[ 7] =  4; buffer.fPols[ 8] =  9; // 1
    buffer.fPols[ 9] = 5;   buffer.fPols[10] = 10; buffer.fPols[11] =  1;
    buffer.fPols[12] = col; buffer.fPols[13] =  4; buffer.fPols[14] = 10; // 2
    buffer.fPols[15] = 6;   buffer.fPols[16] = 11; buffer.fPols[17] =  2;
    buffer.fPols[18] = col; buffer.fPols[19] =  4; buffer.fPols[20] = 11; // 3
    buffer.fPols[21] = 7;   buffer.fPols[22] =  8; buffer.fPols[23] =  3;
    buffer.fPols[24] = col; buffer.fPols[25] =  4; buffer.fPols[26] =  1; // 4
    buffer.fPols[27] = 2;   buffer.fPols[28] =  3; buffer.fPols[29] =  0;
    buffer.fPols[30] = col; buffer.fPols[31] =  4; buffer.fPols[32] =  7; // 5
    buffer.fPols[33] = 6;   buffer.fPols[34] =  5; buffer.fPols[35] =  4;
    buffer.SetSectionsValid(TBuffer3D::kRaw);
    ok = true;
    return buffer;
  }
 
  std::cerr << "ViewGeometryShape::GetBuffer:\n";
  std::cerr << "    Unknown type of solid.\n";

  return buffer;

}

ViewGeometry::ViewGeometry() :
  TObject(), 
  className("ViewGeometry"), debug(false), 
  canvas(0), hasExternalCanvas(false),
  geometry(0), nShapes(0) {

  plottingEngine.SetDefaultStyle();
  shapes.clear();

}

ViewGeometry::~ViewGeometry() {

  if (!hasExternalCanvas && canvas != 0) delete canvas;

}

void
ViewGeometry::SetGeometry(GeometrySimple* geo) {

  if (geo == 0) {
    std::cerr << className << "::SetGeometry:\n";
    std::cerr << "    Geometry pointer is null.\n";
    return;
  }

  geometry = geo;

} 
  
void
ViewGeometry::SetCanvas(TCanvas* c) {

  if (c == 0) return;
  if (!hasExternalCanvas && canvas != 0) {
    delete canvas;
    canvas = 0;
  }
  canvas = c;
  hasExternalCanvas = true;

}

void
ViewGeometry::Plot() {

  if (geometry == 0) {
    std::cerr << className << "::Plot:\n";
    std::cerr << "    Geometry is not defined.\n";
    return;
  }
  
  if (canvas == 0) {
    canvas = new TCanvas();
    canvas->SetTitle(label.c_str());
    if (hasExternalCanvas) hasExternalCanvas = false;
  }
  canvas->cd();

  shapes.clear();
  nShapes = 0;

  const int nSolids = geometry->GetNumberOfSolids();
  if (nSolids <= 0) {
    std::cerr << className << "::Plot:\n";
    std::cerr << "    Geometry is empty.\n";
    return;
  }
  
  Solid* solid = 0;
  for (int i = nSolids; i--;) {
    solid = 0;
    if (!geometry->GetSolid(i, solid)) {
      std::cerr << className << "::Plot:\n";
      std::cerr << "    Could not get solid " << i << " from geometry.\n";
      continue;
    }
    if (solid == 0) {
      std::cerr << className << "::Plot:\n";
      std::cerr << "    Got null pointer from geometry.\n";
      continue;
    }
    ViewGeometryShape newShape;
    newShape.SetSolid(solid);
    shapes.push_back(newShape);
    ++nShapes;
  }
  
  Draw("ogl");

}

void
ViewGeometry::Paint(Option_t*) {

  if (nShapes <= 0) {
    std::cerr << className << "::Paint:\n";
    std::cerr << "    There is nothing to paint.\n";
    return;
  }

  TVirtualViewer3D* viewer = gPad->GetViewer3D();
  viewer->BeginScene();

  for (int i = nShapes; i--;) {
    bool ok = false;
    TBuffer3D& buffer = shapes[i].GetBuffer(ok);
    int req = viewer->AddObject(buffer);

    if (req != TBuffer3D::kNone) {
      std::cerr << className << "::Paint:\n";
      std::cerr << "    Could not pass object to viewer.\n";
    }
  }

  viewer->EndScene();

}

void
ViewGeometry::Draw(Option_t* option) {

  TObject::Draw(option);
  gPad->GetViewer3D(option);

}

}
