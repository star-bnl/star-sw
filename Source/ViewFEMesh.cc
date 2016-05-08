// Some code was copied/modified from ViewField.cc
#include <iostream>
#include <cmath>

#include "ComponentFieldMap.hh"
#include "ComponentCST.hh"
#include "Plotting.hh"
#include "Random.hh"
#include "ViewFEMesh.hh"

namespace Garfield {

ViewFEMesh::ViewFEMesh()
    : className("ViewFEMesh"),
      label("Cell Layout"),
      debug(false),
      fillMesh(false),
      canvas(NULL),
      hasExternalCanvas(false),
      hasUserArea(false),
      xMin(-1.),
      yMin(-1.),
      zMin(-1.),
      xMax(1.),
      yMax(1.),
      zMax(1.),
      component(NULL),
      viewDrift(NULL),
      plotMeshBorders(false),
      xaxis(NULL),
      yaxis(NULL),
      axes(NULL),
      drawAxes(false) {

  plottingEngine.SetDefaultStyle();
  SetDefaultProjection();

  // Create a blank histogram for the axes.
  axes = new TH2D();
  axes->SetStats(false);
  axes->GetXaxis()->SetTitle("x");
  axes->GetYaxis()->SetTitle("y");
}

ViewFEMesh::~ViewFEMesh() {

  if (!hasExternalCanvas && canvas) delete canvas;
}

void ViewFEMesh::SetComponent(ComponentFieldMap* comp) {

  if (!comp) {
    std::cerr << className << "::SetComponent:\n";
    std::cerr << "    Component pointer is null.\n";
    return;
  }

  component = comp;
}

void ViewFEMesh::SetCanvas(TCanvas* c) {

  if (!c) return;
  if (!hasExternalCanvas && canvas) {
    delete canvas;
    canvas = NULL;
  }
  canvas = c;
  hasExternalCanvas = true;
}

TCanvas* ViewFEMesh::GetCanvas() { return canvas; }

void ViewFEMesh::SetArea(double xmin, double ymin, double zmin, double xmax,
                         double ymax, double zmax) {

  // Check range, assign if non-null
  if (xmin == xmax || ymin == ymax) {
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

void ViewFEMesh::SetArea() { hasUserArea = false; }

// The plotting functionality here is ported from Garfield
//  with some inclusion of code from ViewCell.cc
bool ViewFEMesh::Plot() {

  if (!component) {
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

  // Get the bounding box.
  if (!hasUserArea) {
    std::cerr << className << "::Plot:\n";
    std::cerr << "    Bounding box cannot be determined.\n";
    std::cerr << "    Call SetArea first.\n";
    return false;
  }

  // Set up a canvas if one does not already exist.
  if (!canvas) {
    canvas = new TCanvas();
    canvas->SetTitle(label.c_str());
    if (hasExternalCanvas) hasExternalCanvas = false;
  }
  canvas->Range(xMin, yMin, xMax, yMax);

  // Plot the elements
  ComponentCST* componentCST = dynamic_cast<ComponentCST*>(component);
  if (componentCST) {
    std::cout << className << "::Plot:\n";
    std::cout << "    The given component is a CST component.\n";
    std::cout << "    Method PlotCST is called now!.\n";
    DrawCST(componentCST);
  } else {
    DrawElements();
  }
  canvas->Update();

  return true;
}

// Set the projection plane: modified from ViewField.cc
//  to match functionality of Garfield
void ViewFEMesh::SetPlane(const double fx, const double fy, const double fz,
                          const double x0, const double y0, const double z0) {

  // Calculate 2 in-plane vectors for the normal vector
  double fnorm = sqrt(fx * fx + fy * fy + fz * fz);
  double dist = fx * x0 + fy * y0 + fz * z0;
  if (fnorm > 0 && fx * fx + fz * fz > 0) {
    project[0][0] = fz / sqrt(fx * fx + fz * fz);
    project[0][1] = 0;
    project[0][2] = -fx / sqrt(fx * fx + fz * fz);
    project[1][0] = -fx * fy / (sqrt(fx * fx + fz * fz) * fnorm);
    project[1][1] = (fx * fx + fz * fz) / (sqrt(fx * fx + fz * fz) * fnorm);
    project[1][2] = -fy * fz / (sqrt(fx * fx + fz * fz) * fnorm);
    project[2][0] = dist * fx / (fnorm * fnorm);
    project[2][1] = dist * fy / (fnorm * fnorm);
    project[2][2] = dist * fz / (fnorm * fnorm);
  } else if (fnorm > 0 && fy * fy + fz * fz > 0) {
    project[0][0] = (fy * fy + fz * fz) / (sqrt(fy * fy + fz * fz) * fnorm);
    project[0][1] = -fx * fz / (sqrt(fy * fy + fz * fz) * fnorm);
    project[0][2] = -fy * fz / (sqrt(fy * fy + fz * fz) * fnorm);
    project[1][0] = 0;
    project[1][1] = fz / sqrt(fy * fy + fz * fz);
    project[1][2] = -fy / sqrt(fy * fy + fz * fz);
    project[2][0] = dist * fx / (fnorm * fnorm);
    project[2][1] = dist * fy / (fnorm * fnorm);
    project[2][2] = dist * fz / (fnorm * fnorm);
  } else {
    std::cout << className << "::SetPlane:\n";
    std::cout << "    Normal vector has zero norm.\n";
    std::cout << "    No new projection set.\n";
  }

  // Store the plane description
  plane[0] = fx;
  plane[1] = fy;
  plane[2] = fz;
  plane[3] = dist;
}

// Set the default projection for the plane: copied from ViewField.cc
void ViewFEMesh::SetDefaultProjection() {

  // Default projection: x-y at z=0
  project[0][0] = 1;
  project[1][0] = 0;
  project[2][0] = 0;
  project[0][1] = 0;
  project[1][1] = 1;
  project[2][1] = 0;
  project[0][2] = 0;
  project[1][2] = 0;
  project[2][2] = 0;

  // Plane description
  plane[0] = 0;
  plane[1] = 0;
  plane[2] = 1;
  plane[3] = 0;
}

// Set the x-axis.
void ViewFEMesh::SetXaxis(TGaxis* ax) { xaxis = ax; }

// Set the y-axis.
void ViewFEMesh::SetYaxis(TGaxis* ay) { yaxis = ay; }

// Set the x-axis title.
void ViewFEMesh::SetXaxisTitle(const char* xtitle) {
  axes->GetXaxis()->SetTitle(xtitle);
}

// Set the y-axis title.
void ViewFEMesh::SetYaxisTitle(const char* ytitle) {
  axes->GetYaxis()->SetTitle(ytitle);
}

// Create default axes
void ViewFEMesh::CreateDefaultAxes() {

  // Create a new x and y axis
  xaxis = new TGaxis(
      xMin + std::abs(xMax - xMin) * 0.1, yMin + std::abs(yMax - yMin) * 0.1,
      xMax - std::abs(xMax - xMin) * 0.1, yMin + std::abs(yMax - yMin) * 0.1,
      xMin + std::abs(xMax - xMin) * 0.1, xMax - std::abs(xMax - xMin) * 0.1,
      2405, "x");
  yaxis = new TGaxis(
      xMin + std::abs(xMax - xMin) * 0.1, yMin + std::abs(yMax - yMin) * 0.1,
      xMin + std::abs(xMax - xMin) * 0.1, yMax - std::abs(yMax - yMin) * 0.1,
      yMin + std::abs(yMax - yMin) * 0.1, yMax - std::abs(yMax - yMin) * 0.1,
      2405, "y");

  // Label sizes
  xaxis->SetLabelSize(0.025);
  yaxis->SetLabelSize(0.025);

  // Titles
  xaxis->SetTitleSize(0.03);
  xaxis->SetTitle("x [cm]");
  yaxis->SetTitleSize(0.03);
  yaxis->SetTitle("y [cm]");
}

// Use ROOT plotting functions to draw the mesh elements on the canvas.
// General methodology ported from Garfield
void ViewFEMesh::DrawElements() {

  // Get the map boundaries from the component.
  double mapxmax = component->mapxmax;
  double mapxmin = component->mapxmin;
  double mapymax = component->mapymax;
  double mapymin = component->mapymin;
  double mapzmax = component->mapzmax;
  double mapzmin = component->mapzmin;

  // Get the periodicities.
  double sx = mapxmax - mapxmin;
  double sy = mapymax - mapymin;
  double sz = mapzmax - mapzmin;
  const bool perX = component->m_xPeriodic || component->m_xMirrorPeriodic;
  const bool perY = component->m_yPeriodic || component->m_yMirrorPeriodic;
  const bool perZ = component->m_zPeriodic || component->m_zMirrorPeriodic;

  // Clear the meshes and drift line lists.
  mesh.clear();
  driftLines.clear();

  // Prepare the final projection matrix (the transpose of the 2D array
  // "project").
  double fnorm =
      sqrt(plane[0] * plane[0] + plane[1] * plane[1] + plane[2] * plane[2]);
  TArrayD dataProj(9);
  dataProj[0] = project[0][0];
  dataProj[1] = project[1][0];
  dataProj[2] = plane[0] / fnorm;
  dataProj[3] = project[0][1];
  dataProj[4] = project[1][1];
  dataProj[5] = plane[1] / fnorm;
  dataProj[6] = project[0][2];
  dataProj[7] = project[1][2];
  dataProj[8] = plane[2] / fnorm;
  TMatrixD projMat(3, 3, dataProj.GetArray());

  // Calculate the determinant of the projection matrix.
  double projDet =
      projMat(0, 0) *
          (projMat(1, 1) * projMat(2, 2) - projMat(1, 2) * projMat(2, 1)) -
      projMat(0, 1) *
          (projMat(1, 0) * projMat(2, 2) - projMat(1, 2) * projMat(2, 0)) +
      projMat(0, 2) *
          (projMat(1, 0) * projMat(2, 1) - projMat(1, 1) * projMat(2, 0));

  // Calculate the inverse of the projection matrix for
  //  calculating coordinates in the viewing plane.
  if (projDet != 0) {
    projMat.Invert();
  } else {
    std::cerr << className << "::DrawElements:\n";
    std::cerr << "    Projection matrix is not invertible.\n";
    std::cerr << "    Finite element mesh will not be drawn.\n";
  }

  // Get the plane information.
  double fx = plane[0];
  double fy = plane[1];
  double fz = plane[2];
  double dist = plane[3];

  // Construct two empty single-column matrices for use as coordinate vectors.
  TMatrixD xMat(3, 1);

  // Determine the number of periods present in the cell.
  int nMaxX = 0, nMinX = 0;
  int nMaxY = 0, nMinY = 0;
  int nMaxZ = 0, nMinZ = 0;
  if (perX) {
    nMinX = int(xMin / sx) - 1;
    nMaxX = int(xMax / sx) + 1;
  }
  if (perY) {
    nMinY = int(yMin / sy) - 1;
    nMaxY = int(yMax / sy) + 1;
  }
  if (perZ) {
    nMinZ = int(zMin / sz) - 1;
    nMaxZ = int(zMax / sz) + 1;
  }

  // Loop over all elements.
  for (int elem = 0; elem < component->nElements; elem++) {

    // Do not plot the drift medium.
    if (component->materials[component->elements[elem].matmap].driftmedium &&
        !(plotMeshBorders))
      continue;
    // Do not create Polygons for disabled materials
    if (disabledMaterial[component->elements[elem].matmap]) continue;
    // -- Tetrahedral elements

    // Coordinates of vertices
    double vx1, vy1, vz1;
    double vx2, vy2, vz2;
    double vx3, vy3, vz3;
    double vx4, vy4, vz4;

    // Get the color for this element (default to 1).
    int colorID = colorMap.count(component->elements[elem].matmap);
    if (colorID != 0)
      colorID = colorMap[component->elements[elem].matmap];
    else
      colorID = 1;

    // Get the fill color for this element (default colorID).
    int colorID_fill = colorMap_fill.count(component->elements[elem].matmap);
    if (colorID_fill != 0)
      colorID_fill = colorMap_fill[component->elements[elem].matmap];
    else
      colorID_fill = colorID;

    // Loop over the periodicities in x.
    for (int nx = nMinX; nx <= nMaxX; nx++) {

      // Determine the x-coordinates of the tetrahedral vertices.
      if (component->m_xMirrorPeriodic && nx != 2 * (nx / 2)) {
        vx1 =
            mapxmin +
            (mapxmax - component->nodes[component->elements[elem].emap[0]].x) +
            sx * nx;
        vx2 =
            mapxmin +
            (mapxmax - component->nodes[component->elements[elem].emap[1]].x) +
            sx * nx;
        vx3 =
            mapxmin +
            (mapxmax - component->nodes[component->elements[elem].emap[2]].x) +
            sx * nx;
        vx4 =
            mapxmin +
            (mapxmax - component->nodes[component->elements[elem].emap[3]].x) +
            sx * nx;
      } else {
        vx1 = component->nodes[component->elements[elem].emap[0]].x + sx * nx;
        vx2 = component->nodes[component->elements[elem].emap[1]].x + sx * nx;
        vx3 = component->nodes[component->elements[elem].emap[2]].x + sx * nx;
        vx4 = component->nodes[component->elements[elem].emap[3]].x + sx * nx;
      }

      // Loop over the periodicities in y.
      for (int ny = nMinY; ny <= nMaxY; ny++) {

        // Determine the y-coordinates of the tetrahedral vertices.
        if (component->m_yMirrorPeriodic && ny != 2 * (ny / 2)) {
          vy1 = mapymin +
                (mapymax -
                 component->nodes[component->elements[elem].emap[0]].y) +
                sy * ny;
          vy2 = mapymin +
                (mapymax -
                 component->nodes[component->elements[elem].emap[1]].y) +
                sy * ny;
          vy3 = mapymin +
                (mapymax -
                 component->nodes[component->elements[elem].emap[2]].y) +
                sy * ny;
          vy4 = mapymin +
                (mapymax -
                 component->nodes[component->elements[elem].emap[3]].y) +
                sy * ny;
        } else {
          vy1 = component->nodes[component->elements[elem].emap[0]].y + sy * ny;
          vy2 = component->nodes[component->elements[elem].emap[1]].y + sy * ny;
          vy3 = component->nodes[component->elements[elem].emap[2]].y + sy * ny;
          vy4 = component->nodes[component->elements[elem].emap[3]].y + sy * ny;
        }

        // Loop over the periodicities in z.
        for (int nz = nMinZ; nz <= nMaxZ; nz++) {

          // Determine the z-coordinates of the tetrahedral vertices.
          if (component->m_zMirrorPeriodic && nz != 2 * (nz / 2)) {
            vz1 = mapzmin +
                  (mapzmax -
                   component->nodes[component->elements[elem].emap[0]].z) +
                  sz * nz;
            vz2 = mapzmin +
                  (mapzmax -
                   component->nodes[component->elements[elem].emap[1]].z) +
                  sz * nz;
            vz3 = mapzmin +
                  (mapzmax -
                   component->nodes[component->elements[elem].emap[2]].z) +
                  sz * nz;
            vz4 = mapzmin +
                  (mapzmax -
                   component->nodes[component->elements[elem].emap[3]].z) +
                  sz * nz;
          } else {
            vz1 =
                component->nodes[component->elements[elem].emap[0]].z + sz * nz;
            vz2 =
                component->nodes[component->elements[elem].emap[1]].z + sz * nz;
            vz3 =
                component->nodes[component->elements[elem].emap[2]].z + sz * nz;
            vz4 =
                component->nodes[component->elements[elem].emap[3]].z + sz * nz;
          }

          // Store the x and y coordinates of the relevant mesh vertices.
          std::vector<double> vX;
          std::vector<double> vY;

          // Boolean values for determining what coordinates are in the viewing
          // plane
          bool in1 = false, in2 = false, in3 = false, in4 = false;

          // Boolean value for determining whether a plane cut succeeded
          bool planeCut = false;

          // Value used to determine whether a vertex is in the plane.
          double pcf = std::max(
              std::max(std::max(std::max(std::max(std::max(std::abs(vx1),
                                                           std::abs(vy1)),
                                                  std::abs(vz1)),
                                         std::abs(fx)),
                                std::abs(fy)),
                       std::abs(fz)),
              std::abs(dist));

          // First isolate the vertices that are in the viewing plane.
          if (std::abs(fx * vx1 + fy * vy1 + fz * vz1 - dist) < 1.0e-4 * pcf) {
            in1 = true;
          }
          if (std::abs(fx * vx2 + fy * vy2 + fz * vz2 - dist) < 1.0e-4 * pcf) {
            in2 = true;
          }
          if (std::abs(fx * vx3 + fy * vy3 + fz * vz3 - dist) < 1.0e-4 * pcf) {
            in3 = true;
          }
          if (std::abs(fx * vx4 + fy * vy4 + fz * vz4 - dist) < 1.0e-4 * pcf) {
            in4 = true;
          }

          // Calculate the planar coordinates for those edges that are in the
          // plane.
          if (in1) {
            PlaneCoords(vx1, vy1, vz1, projMat, xMat);
            vX.push_back(xMat(0, 0));
            vY.push_back(xMat(1, 0));
          }
          if (in2) {
            PlaneCoords(vx2, vy2, vz2, projMat, xMat);
            vX.push_back(xMat(0, 0));
            vY.push_back(xMat(1, 0));
          }
          if (in3) {
            PlaneCoords(vx3, vy3, vz3, projMat, xMat);
            vX.push_back(xMat(0, 0));
            vY.push_back(xMat(1, 0));
          }
          if (in4) {
            PlaneCoords(vx4, vy4, vz4, projMat, xMat);
            vX.push_back(xMat(0, 0));
            vY.push_back(xMat(1, 0));
          }

          // Cut the sides that are not in the plane.
          if (!(in1 || in2)) {
            planeCut = PlaneCut(vx1, vy1, vz1, vx2, vy2, vz2, xMat);
            if (planeCut) {
              vX.push_back(xMat(0, 0));
              vY.push_back(xMat(1, 0));
            }
          }
          if (!(in1 || in3)) {
            planeCut = PlaneCut(vx1, vy1, vz1, vx3, vy3, vz3, xMat);
            if (planeCut) {
              vX.push_back(xMat(0, 0));
              vY.push_back(xMat(1, 0));
            }
          }
          if (!(in1 || in4)) {
            planeCut = PlaneCut(vx1, vy1, vz1, vx4, vy4, vz4, xMat);
            if (planeCut) {
              vX.push_back(xMat(0, 0));
              vY.push_back(xMat(1, 0));
            }
          }
          if (!(in2 || in3)) {
            planeCut = PlaneCut(vx2, vy2, vz2, vx3, vy3, vz3, xMat);
            if (planeCut) {
              vX.push_back(xMat(0, 0));
              vY.push_back(xMat(1, 0));
            }
          }
          if (!(in2 || in4)) {
            planeCut = PlaneCut(vx2, vy2, vz2, vx4, vy4, vz4, xMat);
            if (planeCut) {
              vX.push_back(xMat(0, 0));
              vY.push_back(xMat(1, 0));
            }
          }
          if (!(in3 || in4)) {
            planeCut = PlaneCut(vx3, vy3, vz3, vx4, vy4, vz4, xMat);
            if (planeCut) {
              vX.push_back(xMat(0, 0));
              vY.push_back(xMat(1, 0));
            }
          }

          // Create a convex TPolyLine object connecting the points.
          if (vX.size() >= 3) {

            // Eliminate crossings of the polygon lines
            //  (known as "butterflies" in Garfield).
            RemoveCrossings(vX, vY);

            // Create vectors to store the clipped polygon.
            std::vector<double> cX;
            std::vector<double> cY;

            // Clip the polygon to the view area.
            ClipToView(vX, vY, cX, cY);

            // If we have more than 2 vertices, add the polygon.
            if (cX.size() > 2) {

              // Again eliminate crossings of the polygon lines.
              RemoveCrossings(cX, cY);

              // Create the TPolyLine.
              int polyPts = 0;
              TPolyLine* poly = new TPolyLine();
              poly->SetLineColor(colorID);
              poly->SetFillColor(colorID_fill);
              poly->SetLineWidth(3);

              // Add all of the points.
              for (int pt = 0; pt < (int)cX.size(); pt++) {

                // Add this point.
                poly->SetPoint(polyPts, cX[pt], cY[pt]);
                polyPts++;
              }

              // Add the polygon to the mesh.
              mesh.push_back(poly);
            }
          }  // end TPolyLine construction if statement
        }    // end z-periodicity loop
      }      // end y-periodicity loop
    }        // end x-periodicity loop
  }          // end loop over elements

  // If we have an associated ViewDrift, plot projections of the drift lines.
  if (viewDrift) {

    for (int dline = 0; dline < (int)viewDrift->m_driftLines.size(); dline++) {

      // Get the number of points.
      const unsigned int npts = viewDrift->m_driftLines[dline].vect.size();
      // Create a TPolyLine that is a 2D projection of the original.
      TPolyLine* poly = new TPolyLine();
      poly->SetLineColor(viewDrift->m_driftLines[dline].n);
      int polyPts = 0;
      for (unsigned int pt = 0; pt < npts; pt++) {
        // Get the coordinates of this point.
        double ptx = viewDrift->m_driftLines[dline].vect[pt].x;
        double pty = viewDrift->m_driftLines[dline].vect[pt].y;
        double ptz = viewDrift->m_driftLines[dline].vect[pt].z;
        // Project this point onto the plane.
        PlaneCoords(ptx, pty, ptz, projMat, xMat);
        // Add this point if it is within the view.
        if (xMat(0, 0) >= xMin && xMat(0, 0) <= xMax && xMat(1, 0) >= yMin &&
            xMat(1, 0) <= yMax) {
          poly->SetPoint(polyPts, xMat(0, 0), xMat(1, 0));
          polyPts++;
        }
      }  // end loop over points

      // Add the drift line to the list.
      driftLines.push_back(poly);

    }  // end loop over drift lines
  }    // end if(viewDrift != 0)

  // Call the ROOT draw methods to plot the elements.
  canvas->cd();

  // Draw default axes by using a blank 2D histogram.
  if (!xaxis && !yaxis && drawAxes) {
    axes->GetXaxis()->SetLimits(xMin, xMax);
    axes->GetYaxis()->SetLimits(yMin, yMax);
    axes->Draw();
  }

  // Draw custom axes.
  if (xaxis && drawAxes) xaxis->Draw();
  if (yaxis && drawAxes) yaxis->Draw();

  // Draw the mesh on the canvas.
  for (int m = mesh.size(); m--;) {
    if (plotMeshBorders || !fillMesh) mesh[m]->Draw("same");
    if (fillMesh) mesh[m]->Draw("f:same");
  }

  // Draw the drift lines on the view.
  for (int m = driftLines.size(); m--;) {
    driftLines[m]->Draw("same");
  }
}

void ViewFEMesh::DrawCST(ComponentCST* componentCST) {
  /*The method is based on ViewFEMesh::Draw, thus the first part is copied from
   * there.
   * At the moment only x-y, x-z, and y-z are available due to the simple
   * implementation.
   * The advantage of this method is that there is no element loop and thus it
   * is much
   * faster.
   */
  // Get the map boundaries from the component
  double mapxmax = component->mapxmax;
  double mapxmin = component->mapxmin;
  double mapymax = component->mapymax;
  double mapymin = component->mapymin;
  double mapzmax = component->mapzmax;
  double mapzmin = component->mapzmin;

  // Get the periodicities.
  double sx = mapxmax - mapxmin;
  double sy = mapymax - mapymin;
  double sz = mapzmax - mapzmin;
  const bool perX = component->m_xPeriodic || component->m_xMirrorPeriodic;
  const bool perY = component->m_yPeriodic || component->m_yMirrorPeriodic;
  const bool perZ = component->m_zPeriodic || component->m_zMirrorPeriodic;

  // Clear the meshes and drift line lists
  mesh.clear();
  driftLines.clear();

  // Prepare the final projection matrix (the transpose of the 2D array
  // "project")
  double fnorm =
      sqrt(plane[0] * plane[0] + plane[1] * plane[1] + plane[2] * plane[2]);
  TArrayD dataProj(9);
  dataProj[0] = project[0][0];
  dataProj[1] = project[1][0];
  dataProj[2] = plane[0] / fnorm;
  dataProj[3] = project[0][1];
  dataProj[4] = project[1][1];
  dataProj[5] = plane[1] / fnorm;
  dataProj[6] = project[0][2];
  dataProj[7] = project[1][2];
  dataProj[8] = plane[2] / fnorm;
  TMatrixD projMat(3, 3, dataProj.GetArray());

  // Calculate the determinant of the projection matrix
  double projDet =
      projMat(0, 0) *
          (projMat(1, 1) * projMat(2, 2) - projMat(1, 2) * projMat(2, 1)) -
      projMat(0, 1) *
          (projMat(1, 0) * projMat(2, 2) - projMat(1, 2) * projMat(2, 0)) +
      projMat(0, 2) *
          (projMat(1, 0) * projMat(2, 1) - projMat(1, 1) * projMat(2, 0));

  // Calculate the inverse of the projection matrix for
  //  calculating coordinates in the viewing plane
  if (projDet != 0) {
    projMat.Invert();
  } else {
    std::cerr << className << "::DrawCST:\n";
    std::cerr << "    Projection matrix is not invertible.\n";
    std::cerr << "    Finite element mesh will not be drawn.\n";
  }

  // Construct two empty single-column matrices for use as coordinate vectors
  TMatrixD xMat(3, 1);

  // Determine the number of periods present in the cell.
  int nMaxX = 0, nMinX = 0;
  int nMaxY = 0, nMinY = 0;
  int nMaxZ = 0, nMinZ = 0;
  if (perX) {
    nMinX = int(xMin / sx) - 1;
    nMaxX = int(xMax / sx) + 1;
  }
  if (perY) {
    nMinY = int(yMin / sy) - 1;
    nMaxY = int(yMax / sy) + 1;
  }
  if (perZ) {
    nMinZ = int(zMin / sz) - 1;
    nMaxZ = int(zMax / sz) + 1;
  }

  int elem = 0;
  std::vector<PolygonInfo> elements;
  int nMinU = 0, nMaxU = 0, nMinV = 0, nMaxV = 0;
  double mapumin = 0., mapumax = 0., mapvmin = 0., mapvmax = 0.;
  double su = 0., sv = 0.;
  bool mirroru = false, mirrorv = false;
  double uMin, vMin, uMax, vMax;
  unsigned int n_x, n_y, n_z;
  componentCST->GetNumberOfMeshLines(n_x, n_y, n_z);
  double e_xmin, e_xmax, e_ymin, e_ymax, e_zmin, e_zmax;
  // xy view
  if (plane[0] == 0 && plane[1] == 0 && plane[2] == 1) {
    std::cout << className << "::DrawCST:\n";
    std::cout << "    Creating x-y mesh view.\n";
    ViewFEMesh::SetXaxisTitle("x [cm]");
    ViewFEMesh::SetYaxisTitle("y [cm]");
    // calculate the z position
    unsigned int i,j,z;
    if(!componentCST->Coordinate2Index(0,0,project[2][2],i,j,z)){
      std::cerr << "Could determine the position of the plane in z direction." << std::endl;
      return;
    }
    std::cout << "    The plane position in z direction is: " << project[2][2] << "\n";
    nMinU = nMinX;
    nMaxU = nMaxX;
    nMinV = nMinY;
    nMaxV = nMaxY;
    uMin = xMin;
    uMax = xMax;
    vMin = yMin;
    vMax = yMax;

    mapumin = mapxmin;
    mapumax = mapxmax;
    mapvmin = mapymin;
    mapvmax = mapymax;
    su = sx;
    sv = sy;
    mirroru = perX;
    mirrorv = perY;
    for (unsigned int y = 0; y < (n_y - 1); y++) {
      for (unsigned int x = 0; x < (n_x - 1); x++) {
        elem = componentCST->Index2Element(x,y,z);
        componentCST->GetElementBoundaries(elem, e_xmin, e_xmax, e_ymin, e_ymax, e_zmin, e_zmax);
        PolygonInfo tmp_info;
        tmp_info.element = elem;
        tmp_info.p1[0] = e_xmin;
        tmp_info.p2[0] = e_xmax;
        tmp_info.p3[0] = e_xmax;
        tmp_info.p4[0] = e_xmin;
        tmp_info.p1[1] = e_ymin;
        tmp_info.p2[1] = e_ymin;
        tmp_info.p3[1] = e_ymax;
        tmp_info.p4[1] = e_ymax;
        tmp_info.material = componentCST->GetElementMaterial(elem);
        elements.push_back(tmp_info);
      }
    }
    // xz-view
  } else if (plane[0] == 0 && plane[1] == -1 && plane[2] == 0) {
    std::cout << className << "::DrawCST:\n";
    std::cout << "    Creating x-z mesh view.\n";
    ViewFEMesh::SetXaxisTitle("x [cm]");
    ViewFEMesh::SetYaxisTitle("z [cm]");
    // calculate the y position
    unsigned int i = 0,j = 0,y = 0;
    if(!componentCST->Coordinate2Index(0,project[2][1],0,i,y,j)){
      std::cerr << "Could determine the position of the plane in y direction." << std::endl;
      return;
    }
    std::cout << "    The plane position in y direction is: " << project[2][1] << "\n";

    nMinU = nMinX;
    nMaxU = nMaxX;
    nMinV = nMinZ;
    nMaxV = nMaxZ;
    uMin = xMin;
    uMax = xMax;
    vMin = zMin;
    vMax = zMax;

    mapumin = mapxmin;
    mapumax = mapxmax;
    mapvmin = mapzmin;
    mapvmax = mapzmax;
    su = sx;
    sv = sz;
    mirroru = perX;
    mirrorv = perZ;
    for (unsigned int z = 0; z < (n_z - 1); z++) {
      for (unsigned int x = 0; x < (n_x - 1); x++) {
        elem = componentCST->Index2Element(x,y,z);
        componentCST->GetElementBoundaries(elem, e_xmin, e_xmax, e_ymin, e_ymax, e_zmin, e_zmax);
        PolygonInfo tmp_info;
        tmp_info.element = elem;
        tmp_info.p1[0] = e_xmin;
        tmp_info.p2[0] = e_xmax;
        tmp_info.p3[0] = e_xmax;
        tmp_info.p4[0] = e_xmin;
        tmp_info.p1[1] = e_zmin;
        tmp_info.p2[1] = e_zmin;
        tmp_info.p3[1] = e_zmax;
        tmp_info.p4[1] = e_zmax;
        tmp_info.material = componentCST->GetElementMaterial(elem);
        elements.push_back(tmp_info);
      }
    }

    // yz-view
  } else if (plane[0] == -1 && plane[1] == 0 && plane[2] == 0) {
    std::cout << className << "::DrawCST:\n";
    std::cout << "    Creating z-y mesh view.\n";
    ViewFEMesh::SetXaxisTitle("z [cm]");
    ViewFEMesh::SetYaxisTitle("y [cm]");
    // calculate the x position
    unsigned int i,j,x;
    if(!componentCST->Coordinate2Index(project[2][0],0,0,x,i,j)){
      std::cerr << "Could determine the position of the plane in x direction." << std::endl;
      return;
    }
    std::cout << "    The plane position in x direction is: " << project[2][0] << "\n";
    nMinU = nMinZ;
    nMaxU = nMaxZ;
    nMinV = nMinY;
    nMaxV = nMaxY;
    uMin = yMin;
    uMax = yMax;
    vMin = zMin;
    vMax = zMax;

    mapumin = mapzmin;
    mapumax = mapzmax;
    mapvmin = mapymin;
    mapvmax = mapymax;
    su = sz;
    sv = sy;
    mirroru = perZ;
    mirrorv = perY;
    for (unsigned int z = 0; z < (n_z-1); z++) {
      for (unsigned int y = 0; y < (n_y-1); y++) {
        elem = componentCST->Index2Element(x,y,z);
        componentCST->GetElementBoundaries(elem, e_xmin, e_xmax, e_ymin, e_ymax, e_zmin, e_zmax);
        PolygonInfo tmp_info;
        tmp_info.element = elem;
        tmp_info.p1[0] = e_zmin;
        tmp_info.p2[0] = e_zmax;
        tmp_info.p3[0] = e_zmax;
        tmp_info.p4[0] = e_zmin;
        tmp_info.p1[1] = e_ymin;
        tmp_info.p2[1] = e_ymin;
        tmp_info.p3[1] = e_ymax;
        tmp_info.p4[1] = e_ymax;
        tmp_info.material = componentCST->GetElementMaterial(elem);
        // Add the polygon to the mesh
        elements.push_back(tmp_info);
      }
    }
  } else {
    std::cerr << className << "::DrawCST:\n";
    std::cerr << "    The given plane name is not known.\n";
    std::cerr << "    Please choose one of the following: xy, xz, yz.\n";
    return;
  }
  std::cout << className << "::PlotCST:\n";
  std::cout << "    Number of elements in the projection of the unit cell:"
            << elements.size() << std::endl;
  std::vector<PolygonInfo>::iterator it;
  std::vector<PolygonInfo>::iterator itend = elements.end();

  for (int nu = nMinU; nu <= nMaxU; nu++) {
    for (int nv = nMinV; nv <= nMaxV; nv++) {
      it = elements.begin();
      while (it != itend) {
        if (disabledMaterial[(*it).material]) {
          // do not create Polygons for disabled materials
          it++;
          continue;
        }
        int colorID = colorMap.count((*it).material);
        if (colorID != 0)
          colorID = colorMap[(*it).material];
        else
          colorID = 1;

        // Get the fill color for this element (default colorID)
        int colorID_fill = colorMap_fill.count((*it).material);
        if (colorID_fill != 0)
          colorID_fill = colorMap_fill[(*it).material];
        else
          colorID_fill = colorID;

        TPolyLine* poly = new TPolyLine();
        poly->SetLineColor(colorID);
        poly->SetFillColor(colorID_fill);
        if (plotMeshBorders)
          poly->SetLineWidth(3);
        else
          poly->SetLineWidth(1);
        // Add 4 points of the square
        Double_t tmp_u[4], tmp_v[4];
        if (mirroru && nu != 2 * (nu / 2)) {
          // nu is odd
          tmp_u[0] = mapumin + (mapumax - (*it).p1[0]) + su * nu;
          tmp_u[1] = mapumin + (mapumax - (*it).p2[0]) + su * nu;
          tmp_u[2] = mapumin + (mapumax - (*it).p3[0]) + su * nu;
          tmp_u[3] = mapumin + (mapumax - (*it).p4[0]) + su * nu;
        } else {
          // nu is even
          tmp_u[0] = (*it).p1[0] + su * nu;
          tmp_u[1] = (*it).p2[0] + su * nu;
          tmp_u[2] = (*it).p3[0] + su * nu;
          tmp_u[3] = (*it).p4[0] + su * nu;
        }
        if (mirrorv && nv != 2 * (nv / 2)) {
          tmp_v[0] = mapvmin + (mapvmax - (*it).p1[1]) + sv * nv;
          tmp_v[1] = mapvmin + (mapvmax - (*it).p2[1]) + sv * nv;
          tmp_v[2] = mapvmin + (mapvmax - (*it).p3[1]) + sv * nv;
          tmp_v[3] = mapvmin + (mapvmax - (*it).p4[1]) + sv * nv;
        } else {
          tmp_v[0] = (*it).p1[1] + sv * nv;
          tmp_v[1] = (*it).p2[1] + sv * nv;
          tmp_v[2] = (*it).p3[1] + sv * nv;
          tmp_v[3] = (*it).p4[1] + sv * nv;
        }
        if(tmp_u[0] < uMin || tmp_u[1] > uMax || tmp_v[0] < vMin || tmp_v[2] > vMax){
          it++;
          continue;
        }
        poly->SetPoint(0, tmp_u[0], tmp_v[0]);
        poly->SetPoint(1, tmp_u[1], tmp_v[1]);
        poly->SetPoint(2, tmp_u[2], tmp_v[2]);
        poly->SetPoint(3, tmp_u[3], tmp_v[3]);
        // Add the polygon to the mesh
        mesh.push_back(poly);
        it++;
      }  // end element loop
    }  // end v-periodicity loop
  }  // end u-periodicity loop
  std::cout << className << "::PlotCST:\n";
  std::cout << "    Number of polygons to be drawn:"
            << mesh.size() << std::endl;
  // If we have an associated ViewDrift, plot projections of the drift lines
  if (viewDrift) {

    for (int dline = 0; dline < (int)viewDrift->m_driftLines.size(); dline++) {

      // Get the number of points.
      const unsigned int npts = viewDrift->m_driftLines[dline].vect.size();
      // Create a TPolyLine that is a 2D projection of the original
      TPolyLine* poly = new TPolyLine();
      poly->SetLineColor(viewDrift->m_driftLines[dline].n);
      int polyPts = 0;
      for (unsigned int pt = 0; pt < npts; pt++) {
        // Get the coordinates of this point in the TPolyLine3D
        double ptx = viewDrift->m_driftLines[dline].vect[pt].x;
        double pty = viewDrift->m_driftLines[dline].vect[pt].y;
        double ptz = viewDrift->m_driftLines[dline].vect[pt].z;
        // Project this point onto the plane
        PlaneCoords(ptx, pty, ptz, projMat, xMat);
        // Add this point if it is within the view
        if (xMat(0, 0) >= uMin && xMat(0, 0) <= uMax && xMat(1, 0) >= vMin &&
            xMat(1, 0) <= vMax) {
          poly->SetPoint(polyPts, xMat(0, 0), xMat(1, 0));
          polyPts++;
        }
      }  // end loop over points
      // Add the drift line to the list
      driftLines.push_back(poly);
    }  // end loop over drift lines
  }    // end if(viewDrift != 0)

  // Call the ROOT draw methods to plot the elements
  canvas->cd();
  // Draw default axes by using a blank 2D histogram.
  if (xaxis == 0 && yaxis == 0 && drawAxes) {
    axes->GetXaxis()->SetLimits(uMin, uMax);
    axes->GetYaxis()->SetLimits(vMin, vMax);
    axes->Draw();
  }
  // Draw custom axes.
  if (xaxis != 0 && drawAxes) xaxis->Draw("");
  if (yaxis != 0 && drawAxes) yaxis->Draw("");
  // Draw the mesh on the canvas
  for (int m = mesh.size(); m--;) {
    if (plotMeshBorders || !fillMesh) mesh[m]->Draw("same");
    if (fillMesh) mesh[m]->Draw("f:sames");
  }

  // Draw the drift lines on the view
  for (int m = driftLines.size(); m--;) {
    driftLines[m]->Draw("sames");
  }
  // TODO: Draw axes also at the end so that they are on top!
}

// Returns true if the specified point is in the view region
bool ViewFEMesh::InView(double x, double y) {
  return (x >= xMin && x <= xMax && y >= yMin && y <= yMax);
}

// Removes duplicate points and line crossings by correctly ordering
//  the points in the provided vectors.
//
//  NOTE: This is a 2D version of the BUTFLD method in Garfield.  It
//   follows the same general algorithm.
//
void ViewFEMesh::RemoveCrossings(std::vector<double>& x,
                                 std::vector<double>& y) {

  // Determine element dimensions
  double xmin = x[0], xmax = x[0];
  double ymin = y[0], ymax = y[0];
  for (int i = 1; i < (int)x.size(); i++) {
    if (x[i] < xmin) xmin = x[i];
    if (x[i] > xmax) xmax = x[i];
    if (y[i] < ymin) ymin = y[i];
    if (y[i] > ymax) ymax = y[i];
  }

  // First remove duplicate points
  double xtol = 1e-10 * std::abs(xmax - xmin);
  double ytol = 1e-10 * std::abs(ymax - ymin);
  for (int i = 0; i < (int)x.size(); i++) {
    for (int j = i + 1; j < (int)x.size(); j++) {
      if (std::abs(x[i] - x[j]) < xtol && std::abs(y[i] - y[j]) < ytol) {
        x.erase(x.begin() + j);
        y.erase(y.begin() + j);
        j--;
      }
    }
  }

  // No crossings with 3 points or less
  if (x.size() <= 3) return;

  // Save the polygon size so it is easily accessible
  int NN = x.size();

  // Keep track of the number of attempts
  int attempts = 0;

  // Exchange points until crossings are eliminated or we have attempted NN
  // times
  bool crossings = true;
  while (crossings && (attempts < NN)) {

    // Assume we are done after this attempt.
    crossings = false;

    for (int i = 1; i <= NN; i++) {
      for (int j = i + 2; j <= NN; j++) {

        // End the j-loop if we have surpassed N and wrapped around to i
        if ((j + 1) > NN && 1 + (j % NN) >= i) break;

        // Otherwise, detect crossings and attempt to eliminate them.
        else {

          // Determine if we have a crossing.
          double xc = 0., yc = 0.;
          if (LinesCrossed(x[(i - 1) % NN], y[(i - 1) % NN], x[i % NN],
                           y[i % NN], x[(j - 1) % NN], y[(j - 1) % NN],
                           x[j % NN], y[j % NN], xc, yc)) {

            // Swap each point from i towards j with each corresponding point
            //  from j towards i.
            for (int k = 1; k <= (j - i) / 2; k++) {

              double xs = x[(i + k - 1) % NN];
              double ys = y[(i + k - 1) % NN];
              x[(i + k - 1) % NN] = x[(j - k) % NN];
              y[(i + k - 1) % NN] = y[(j - k) % NN];
              x[(j - k) % NN] = xs;
              y[(j - k) % NN] = ys;

              // Force another attempt
              crossings = true;
            }
          }
        }
      }  // end loop over j
    }    // end loop over i

    // Increment the number of attempts
    attempts++;

  }  // end while(crossings)

  if (attempts > NN) {
    std::cerr << className << "::RemoveCrossings:\n";
    std::cerr
        << "    WARNING: Maximum attempts reached - crossings not removed.\n";
  }
}

//
// Determines whether the line connecting points (x1,y1) and (x2,y2)
//  and the line connecting points (u1,v1) and (u2,v2) cross somewhere
//  between the 4 points.  Sets the crossing point in (xc, yc).
//
// Ported from Garfield function CROSSD
//
bool ViewFEMesh::LinesCrossed(double x1, double y1, double x2, double y2,
                              double u1, double v1, double u2, double v2,
                              double& xc, double& yc) {

  // Set the tolerances.
  double xtol =
      1.0e-10 *
      std::max(std::abs(x1),
               std::max(std::abs(x2), std::max(std::abs(u1), std::abs(u2))));
  double ytol =
      1.0e-10 *
      std::max(std::abs(y1),
               std::max(std::abs(y2), std::max(std::abs(v1), std::abs(v2))));
  if (xtol <= 0) xtol = 1.0e-10;
  if (ytol <= 0) ytol = 1.0e-10;

  // Compute the distances and determinant (dx,dy) x (du,dv).
  double dy = y2 - y1;
  double dv = v2 - v1;
  double dx = x1 - x2;
  double du = u1 - u2;
  double det = dy * du - dx * dv;

  // Check for crossing because one of the endpoints is on both lines.
  if (OnLine(x1, y1, x2, y2, u1, v1)) {
    xc = u1;
    yc = v1;
    return true;
  } else if (OnLine(x1, y1, x2, y2, u2, v2)) {
    xc = u2;
    yc = v2;
    return true;
  } else if (OnLine(u1, v1, u2, v2, x1, y1)) {
    xc = x1;
    yc = y1;
    return true;
  } else if (OnLine(u1, v1, u2, v2, x2, y2)) {
    xc = x2;
    yc = y2;
    return true;
  }
  // Check if the lines are parallel (zero determinant).
  else if (std::abs(det) < xtol * ytol)
    return false;
  // No special case: compute point of intersection.
  else {

    // Solve crossing equations.
    xc = (du * (x1 * y2 - x2 * y1) - dx * (u1 * v2 - u2 * v1)) / det;
    yc = ((-1 * dv) * (x1 * y2 - x2 * y1) + dy * (u1 * v2 - u2 * v1)) / det;

    // Determine if this point is on both lines.
    if (OnLine(x1, y1, x2, y2, xc, yc) && OnLine(u1, v1, u2, v2, xc, yc))
      return true;
  }

  // The lines do not cross if we have reached this point.
  return false;
}

//
// Determines whether the point (u,v) lies on the line connecting
//  points (x1,y1) and (x2,y2).
//
// Ported from Garfield function ONLIND
//
bool ViewFEMesh::OnLine(double x1, double y1, double x2, double y2, double u,
                        double v) {

  // Set the tolerances
  double xtol =
      1.0e-10 * std::max(std::abs(x1), std::max(std::abs(x2), std::abs(u)));
  double ytol =
      1.0e-10 * std::max(std::abs(y1), std::max(std::abs(y2), std::abs(v)));
  if (xtol <= 0) xtol = 1.0e-10;
  if (ytol <= 0) ytol = 1.0e-10;

  // To store the coordinates of the comparison point
  double xc = 0, yc = 0;

  // Check if (u,v) is the point (x1,y1) or (x2,y2)
  if ((std::abs(x1 - u) <= xtol && std::abs(y1 - v) <= ytol) ||
      (std::abs(x2 - u) <= xtol && std::abs(y2 - v) <= ytol)) {
    return true;
  }
  // Check if the line is actually a point
  else if (std::abs(x1 - x2) <= xtol && std::abs(y1 - y2) <= ytol) {
    return false;
  }
  // Choose (x1,y1) as starting point if closer to (u,v)
  else if (std::abs(u - x1) + std::abs(v - y1) <
           std::abs(u - x2) + std::abs(v - y2)) {

    // Compute the component of the line from (x1,y1) to (u,v)
    //  along the line from (x1,y1) to (x2,y2)
    double dpar = ((u - x1) * (x2 - x1) + (v - y1) * (y2 - y1)) /
                  ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));

    // Determine the point on the line to which to compare (u,v)
    if (dpar < 0.0) {
      xc = x1;
      yc = y1;
    } else if (dpar > 1.0) {
      xc = x2;
      yc = y2;
    } else {
      xc = x1 + dpar * (x2 - x1);
      yc = y1 + dpar * (y2 - y1);
    }
  }
  // Choose (x2,y2) as starting point if closer to (u,v)
  else {

    // Compute the component of the line from (x2,y2) to (u,v)
    //  along the line from (x2,y2) to (x1,y1)
    double dpar = ((u - x2) * (x1 - x2) + (v - y2) * (y1 - y2)) /
                  ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));

    // Determine the point on the line to which to compare (u,v)
    if (dpar < 0.0) {
      xc = x2;
      yc = y2;
    } else if (dpar > 1.0) {
      xc = x1;
      yc = y1;
    } else {
      xc = x2 + dpar * (x1 - x2);
      yc = y2 + dpar * (y1 - y2);
    }
  }

  // Compare the calculated point to (u,v)
  if (std::abs(u - xc) < xtol && std::abs(v - yc) < ytol) return true;

  return false;
}

// Ported from Garfield: determines the point of intersection, in planar
//  coordinates, of a plane with the line connecting multiple points
// x1,y1,z1;x2,y2,z2: the world coordinates of the two points
// projMat;planeMat: the projection and plane matrices
// xMat: the resulting planar coordinates of the intersection point
bool ViewFEMesh::PlaneCut(double x1, double y1, double z1, double x2, double y2,
                          double z2, TMatrixD& xMat) {

  // Set up the matrix for cutting edges not in the plane
  TArrayD dataCut(9);
  TMatrixD cutMat(3, 3);
  dataCut[0] = project[0][0];
  dataCut[1] = project[1][0];
  dataCut[2] = x1 - x2;
  dataCut[3] = project[0][1];
  dataCut[4] = project[1][1];
  dataCut[5] = y1 - y2;
  dataCut[6] = project[0][2];
  dataCut[7] = project[1][2];
  dataCut[8] = z1 - z2;
  cutMat.SetMatrixArray(dataCut.GetArray());

  // Calculate the determinant of the cut matrix
  double cutDet =
      cutMat(0, 0) *
          (cutMat(1, 1) * cutMat(2, 2) - cutMat(1, 2) * cutMat(2, 1)) -
      cutMat(0, 1) *
          (cutMat(1, 0) * cutMat(2, 2) - cutMat(1, 2) * cutMat(2, 0)) +
      cutMat(0, 2) *
          (cutMat(1, 0) * cutMat(2, 1) - cutMat(1, 1) * cutMat(2, 0));

  // Do not proceed if the matrix is singular
  if (cutDet == 0) return false;

  // Set up a coordinate vector (RHS of equation)
  TArrayD dataCoords(3);
  TMatrixD coordMat(3, 1);
  dataCoords[0] = x1 - project[2][0];
  dataCoords[1] = y1 - project[2][1];
  dataCoords[2] = z1 - project[2][2];
  coordMat.SetMatrixArray(dataCoords.GetArray());

  // Invert the cut matrix and multiply to get the solution
  cutMat.Invert();
  xMat = cutMat * coordMat;

  // Return success if the plane point is between the two vertices
  if (xMat(2, 0) < 0 || xMat(2, 0) > 1) return false;
  return true;
}

// Ported from Garfield: calculates the planar coordinates
// x,y,z: original world coordinates
// projMat: the projection matrix
// xMat: the resulting planar coordinates in single-column (vector) form
bool ViewFEMesh::PlaneCoords(double x, double y, double z,
                             const TMatrixD& projMat, TMatrixD& xMat) {

  // Set up the coordinate vector
  TArrayD dataCoords(3);
  TMatrixD coordMat(3, 1);
  dataCoords[0] = x;
  dataCoords[1] = y;
  dataCoords[2] = z;
  coordMat.SetMatrixArray(dataCoords.GetArray());
  xMat = projMat * coordMat;

  return true;
}

// Ported from Garfield (function INTERD):
// Returns true if the point (x,y) is inside of the specified polygon.
// x: the x-coordinate
// y: the y-coordinate
// px: the x-vertices of the polygon
// py: the y-vertices of the polygon
// edge: a variable set to true if the point is located on the polygon edge
bool ViewFEMesh::IsInPolygon(double x, double y, std::vector<double>& px,
                             std::vector<double>& py, bool& edge) {

  // Get the number and coordinates of the polygon vertices.
  int pN = (int)px.size();

  // Handle the special case of less than 2 vertices.
  if (pN < 2) return false;
  // Handle the special case of exactly 2 vertices (a line).
  if (pN == 2) return OnLine(px[0], py[0], px[1], py[1], x, y);

  // Set the minimum and maximum coordinates of all polygon vertices.
  double px_min = px[0], py_min = py[0];
  double px_max = px[0], py_max = py[0];
  for (int i = 0; i < pN; i++) {
    px_min = std::min(px_min, px[i]);
    py_min = std::min(py_min, py[i]);
    px_max = std::max(px_max, px[i]);
    py_max = std::max(py_max, py[i]);
  }

  // Set the tolerances
  double xtol = 1.0e-10 * std::max(std::abs(px_min), std::abs(px_max));
  double ytol = 1.0e-10 * std::max(std::abs(py_min), std::abs(py_max));
  if (xtol <= 0) xtol = 1.0e-10;
  if (ytol <= 0) ytol = 1.0e-10;

  // If we have essentially one x value, check to see if y is in range.
  if (std::abs(px_max - px_min) < xtol) {
    edge = (y > (py_min - ytol) && y < (py_max + ytol) &&
            std::abs(px_max + px_min - 2 * x) < xtol);
    return false;
  }
  // If we have essentially one y value, check to see if x is in range.
  if (std::abs(py_max - py_min) < ytol) {
    edge = (x > (px_min - xtol) && x < (px_max + xtol) &&
            std::abs(py_max + py_min - 2 * y) < ytol);
    return false;
  }

  // Set "infinity" points.
  double xinf = px_min - std::abs(px_max - px_min);
  double yinf = py_min - std::abs(py_max - py_min);

  // Loop until successful or maximum iterations (100) reached.
  int niter = 0;
  bool done = false;
  int ncross = 0;

  while (!done && niter < 100) {

    // Assume we will finish on this loop.
    done = true;

    // Loop over all edges, counting the number of edges crossed by a line
    //  extending from (x,y) to (xinf,yinf).
    ncross = 0;
    for (int i = 0; (done && i < pN); i++) {

      // Determine whether the point lies on the edge.
      if (OnLine(px[i % pN], py[i % pN], px[(i + 1) % pN], py[(i + 1) % pN], x,
                 y)) {

        edge = true;
        return false;
      }

      // Determine whether this edge is crossed; if so increment the counter.
      double xc = 0., yc = 0.;
      if (LinesCrossed(x, y, xinf, yinf, px[i % pN], py[i % pN],
                       px[(i + 1) % pN], py[(i + 1) % pN], xc, yc))
        ncross++;

      // Ensure this vertex is not crossed by the line from (x,y)
      //  to (xinf,yinf); if so recompute (xinf,yinf) and start over.
      if (OnLine(x, y, xinf, yinf, px[i], py[i])) {

        // Recompute (xinf,yinf).
        xinf = px_min - RndmUniform() * std::abs(px_max - xinf);
        yinf = py_min - RndmUniform() * std::abs(py_max - yinf);

        // Start over.
        done = false;
        niter++;
      }
    }
  }

  // If we failed to finish iterating, return false.
  if (niter >= 100) {
    std::cerr << className << "::IsInPolygon: unable to determine whether ("
              << x << ", " << y << ") is inside a polygon.  Returning false.\n";
    return false;
  }

  // Point is inside for an odd, nonzero number of crossings.
  return (ncross != 2 * (ncross / 2));
}

// Ported from Garfield (method GRCONV):
// Clip the specified polygon to the view region; return the clipped polygon.
// px: the x-vertices of the polygon
// py: the y-vertices of the polygon
// cx: to contain the x-vertices of the clipped polygon
// cy: to contain the y-vertices of the clipped polygon
void ViewFEMesh::ClipToView(std::vector<double>& px, std::vector<double>& py,
                            std::vector<double>& cx, std::vector<double>& cy) {

  // Get the number and coordinates of the polygon vertices.
  int pN = (int)px.size();

  // Clear the vectors to contain the final polygon.
  cx.clear();
  cy.clear();

  // Set up the view vertices (counter-clockwise, starting at upper left).
  std::vector<double> vx;
  vx.push_back(xMin);
  vx.push_back(xMax);
  vx.push_back(xMax);
  vx.push_back(xMin);
  std::vector<double> vy;
  vy.push_back(yMax);
  vy.push_back(yMax);
  vy.push_back(yMin);
  vy.push_back(yMin);
  int vN = (int)vx.size();

  // Do nothing if we have less than 2 points.
  if (pN < 2) return;

  // Loop over the polygon vertices.
  for (int i = 0; i < pN; i++) {

    // Flag for skipping check for edge intersection.
    bool skip = false;

    // Loop over the view vertices.
    for (int j = 0; j < vN; j++) {

      // Determine whether this vertex lies on a view edge:
      //  if so add the vertex to the final polygon.
      if (OnLine(vx[j % vN], vy[j % vN], vx[(j + 1) % vN], vy[(j + 1) % vN],
                 px[i], py[i])) {

        // Add the vertex.
        cx.push_back(px[i]);
        cy.push_back(py[i]);

        // Skip edge intersection check in this case.
        skip = true;
      }

      // Determine whether a corner of the view area lies on this edge:
      //  if so add the corner to the final polygon.
      if (OnLine(px[i % pN], py[i % pN], px[(i + 1) % pN], py[(i + 1) % pN],
                 vx[j], vy[j])) {

        // Add the vertex.
        cx.push_back(vx[j]);
        cy.push_back(vy[j]);

        // Skip edge intersection check in this case.
        skip = true;
      }
    }

    // If we have not skipped the edge intersection check, look for an
    // intersection between this edge and the view edges.
    if (!skip) {

      // Loop over the view vertices.
      for (int j = 0; j < vN; j++) {

        // Check for a crossing with this edge;
        //  if one exists, add the crossing point.
        double xc = 0., yc = 0.;
        if (LinesCrossed(vx[j % vN], vy[j % vN], vx[(j + 1) % vN],
                         vy[(j + 1) % vN], px[i % pN], py[i % pN],
                         px[(i + 1) % pN], py[(i + 1) % pN], xc, yc)) {

          // Add a vertex.
          cx.push_back(xc);
          cy.push_back(yc);
        }
      }
    }
  }

  // Find all view field vertices inside the polygon.
  for (int j = 0; j < vN; j++) {

    // Test whether this vertex is inside the polygon.
    //  If so, add it to the final polygon.
    bool edge = false;
    if (IsInPolygon(vx[j], vy[j], px, py, edge)) {

      // Add the view vertex.
      cx.push_back(vx[j]);
      cy.push_back(vy[j]);
    }
  }

  // Find all polygon vertices inside the box.
  for (int i = 0; i < pN; i++) {

    // Test whether this vertex is inside the view.
    //  If so, add it to the final polygon.
    bool edge = false;
    if (IsInPolygon(px[i], py[i], vx, vy, edge)) {

      // Add the polygon vertex.
      cx.push_back(px[i]);
      cy.push_back(py[i]);
    }
  }
}
}
