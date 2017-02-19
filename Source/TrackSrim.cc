#include <iostream>
#include <cstdlib>
#include <fstream>

#include <TCanvas.h>
#include <TGraph.h>
#include <TH1.h>
#include <TLegend.h>

#include "TrackSrim.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Numerics.hh"
#include "Random.hh"
#include "Sensor.hh"

namespace {

void frame(TCanvas* c, double xmin, double ymin, double xmax, double ymax,
           const std::string& xtitle, const std::string& ytitle) {
  c->Range(340.8701, -0.1167765, 1233.468, 4.061288);
  c->SetFillColor(0);
  c->SetBorderMode(0);
  c->SetBorderSize(2);
  c->SetGridx();
  c->SetGridy();
  c->SetTickx();
  c->SetTicky();
  c->SetRightMargin(0.018);
  c->SetLeftMargin(0.098);
  c->SetTopMargin(0.015);
  c->SetBottomMargin(0.09);
  c->SetFrameLineWidth(2);
  c->SetFrameBorderMode(0);
  c->SetFrameBorderSize(12);
  c->SetFrameLineWidth(2);
  c->SetFrameBorderMode(0);
  c->SetFrameBorderSize(12);
  TH1* hframe = new TH1F("hframe", "", 10, xmin, xmax);
  hframe->SetMinimum(ymin);
  hframe->SetMaximum(ymax);
  hframe->SetDirectory(0);
  hframe->SetStats(0);
  hframe->GetXaxis()->SetTitle(xtitle.c_str());
  hframe->GetXaxis()->SetLabelFont(22);
  hframe->GetXaxis()->SetLabelOffset(0.007);
  hframe->GetXaxis()->SetLabelSize(0.038);
  hframe->GetXaxis()->SetTitleSize(0.044);
  hframe->GetXaxis()->SetTickLength(0.035);
  hframe->GetXaxis()->SetTitleOffset(0.9);
  hframe->GetXaxis()->SetTitleFont(22);
  hframe->GetYaxis()->SetTitle(ytitle.c_str());
  hframe->GetYaxis()->SetLabelFont(22);
  hframe->GetYaxis()->SetLabelOffset(0.01);
  hframe->GetYaxis()->SetLabelSize(0.038);
  hframe->GetYaxis()->SetTitleSize(0.044);
  hframe->GetYaxis()->SetTickLength(0.035);
  hframe->GetYaxis()->SetTitleOffset(0.9);
  hframe->GetYaxis()->SetTitleFont(22);
  hframe->Draw(" ");
}

double StepVavilov(const double rkappa) {

  double xlmin = -3.7;
  if (rkappa < 0.1) {
    xlmin = -2.7;
  } else if (rkappa < 1) {
    xlmin = -2.9;
  } else if (rkappa < 2) {
    xlmin = -3.0;
  } else if (rkappa < 3) {
    xlmin = -3.1;
  } else if (rkappa < 4) {
    xlmin = -3.2;
  } else if (rkappa < 5) {
    xlmin = -3.3;
  } else if (rkappa < 6) {
    xlmin = -3.4;
  } else if (rkappa < 7) {
    xlmin = -3.5;
  } else if (rkappa < 8) {
    xlmin = -3.6;
  } 
  return xlmin;
}

double Interpolate(const double x, const std::vector<double>& xtab,
                   const std::vector<double>& ytab) {

  if (x < xtab[0]) {
    return ytab[0];
  } else if (x > xtab.back()) {
    return ytab.back();
  }
  return Garfield::Numerics::Divdif(ytab, xtab, xtab.size(), x, 2);
}

void PrintSettings(const std::string& hdr,
                   const double de, const double step, const double ekin,
                   const double beta2, const double gamma, 
                   const double agas, const double zgas, const double density,
                   const double qpart, const double mpart, const double emax,
                   const double xi, const double kappa) {

  std::cout << hdr << "Settings:\n"
            << "    dE = " << de << " MeV,\n"
            << "    step = " << step << " cm.\n"
            << "    Ekin = " << ekin << " MeV,\n"
            << "    beta2 = " << beta2 << ",\n"
            << "    gamma = " << gamma << ".\n"
            << "    Agas = " << agas << ", Zgas = " << zgas << ",\n"
            << "    density = " << density << " g/cm3.\n"
            << "    Qpart = " << qpart << ", mpart = " << mpart << " MeV.\n"
            << "    Emax = " << emax << " MeV,\n"
            << "    xi = " << xi << " MeV,\n" 
            << "    kappa = " << kappa << ".\n";
}
}

namespace Garfield {

TrackSrim::TrackSrim()
    : m_precisevavilov(false),
      m_useTransStraggle(true),
      m_useLongStraggle(true),
      m_trackset(false),
      m_chargeset(false),
      m_debug(false),
      m_density(-1.0),
      m_work(-1.),
      m_fano(-1.),
      m_q(0.0),
      m_mass(-1.),
      m_a(-1.), 
      m_z(-1.),
      m_initialenergy(-1.0),
      m_maxclusters(-1),
      m_model(4),
      m_nsize(-1) {
  m_className = "TrackSrim";
}

void TrackSrim::SetTrack(const double x0, const double y0, const double z0,
                         const double xd, const double yd, const double zd,
                         const double l) {
  // Transfer the starting point
  m_xt0 = x0;
  m_yt0 = y0;
  m_zt0 = z0;

  // Normalise and store the direction
  const double normdir = sqrt(xd * xd + yd * yd + zd * zd);
  if (normdir <= 0) {
    std::cerr << m_className << "::SetTrack:\n"
              << "    Track length = 0; no SRIM cluster generation.\n";
    return;
  }
  m_xdir = xd / normdir;
  m_ydir = yd / normdir;
  m_zdir = zd / normdir;

  // Transfer the expected length
  m_tracklength = l;

  // Remember we have set a track
  m_trackset = true;
}

void TrackSrim::GetTrack(double& x0, double& y0, double& z0, double& xd,
                         double& yd, double& zd, double& l) {
  // Return the track parameters
  x0 = m_xt0;
  y0 = m_yt0;
  z0 = m_zt0;
  xd = m_xdir;
  yd = m_ydir;
  zd = m_zdir;
  l = m_tracklength;
}

bool TrackSrim::ReadFile(const std::string& file) {
  // SRMREA

  const std::string hdr = m_className + "::ReadFile:\n    ";
  // Open the material list.
  std::ifstream fsrim;
  fsrim.open(file.c_str(), std::ios::in);
  if (fsrim.fail()) {
    std::cerr << hdr << "Could not open SRIM  file " << file
              << " for reading.\n    The file perhaps does not exist.\n";
    return false;
  }
  unsigned int nread = 0;

  // Read the header
  if (m_debug) {
    std::cout << hdr << "SRIM header records from file " << file << "\n";
  }
  /*
  std::string line;
  while (std::getline(fsrim, line)) {
    ++nread;
    if (line.find("SRIM version") != std::string::npos) {
      if (m_debug) std::cout << "\t" << line << "\n";
    } else if (line.find("Calc. date") != std::string::npos) {
      if (m_debug) std::cout << "\t" << line << "\n";
    } else if (line.find("Ion =") != std::string::npos) {
      break;
    }
  }
  //*/
  ///*
  const int size = 100;
  char line[size];
  while (fsrim.getline(line, 100, '\n')) {
    nread++;
    if (strstr(line, "SRIM version") != NULL) {
      if (m_debug) std::cout << "\t" << line << "\n";
    } else if (strstr(line, "Calc. date") != NULL) {
      if (m_debug) std::cout << "\t" << line << "\n";
    } else if (strstr(line, "Ion =") != NULL) {
      break;
    }
  }
  //*/

  // Identify the ion
  char* token = NULL;
  token = strtok(line, " []=");
  token = strtok(NULL, " []=");
  token = strtok(NULL, " []=");
  SetCharge(std::atof(token));
  token = strtok(NULL, " []=");
  token = strtok(NULL, " []=");
  token = strtok(NULL, " []=");
  // Set the ion mass (convert amu to eV).
  SetMass(std::atof(token) * AtomicMassUnitElectronVolt);  

  // Find the target density
  if (!fsrim.getline(line, 100, '\n')) {
    std::cerr << hdr << "Premature EOF looking for target density (line "
              << nread << ").\n";
    return false;
  }
  nread++;
  if (!fsrim.getline(line, 100, '\n')) {
    std::cerr << hdr << "Premature EOF looking for target density (line "
              << nread << ").\n";
    return false;
  }
  nread++;
  token = strtok(line, " ");
  token = strtok(NULL, " ");
  token = strtok(NULL, " ");
  token = strtok(NULL, " ");
  SetDensity(std::atof(token));

  // Check the stopping units
  while (fsrim.getline(line, 100, '\n')) {
    nread++;
    if (strstr(line, "Stopping Units") == NULL) continue;
    if (strstr(line, "Stopping Units =  MeV / (mg/cm2)") != NULL) {
      if (m_debug) {
        std::cout << hdr << "Stopping units: MeV / (mg/cm2) as expected.\n";
      }
      break;
    }
    std::cerr << hdr << "Unknown stopping units. Aborting (line " << nread
              << ").\n";
    return false;
  }

  // Skip to the table
  while (fsrim.getline(line, 100, '\n')) {
    nread++;
    if (strstr(line, "-----------") != NULL) break;
  }

  // Read the table line by line
  m_energy.clear();
  m_emloss.clear();
  m_hdloss.clear();
  m_range.clear();
  m_transstraggle.clear();
  m_longstraggle.clear();
  unsigned int ntable = 0;
  while (fsrim.getline(line, 100, '\n')) {
    nread++;
    if (strstr(line, "-----------") != NULL) break;
    // Energy
    token = strtok(line, " ");
    m_energy.push_back(atof(token));
    token = strtok(NULL, " ");
    if (strcmp(token, "eV") == 0) {
      m_energy[ntable] *= 1.0e-6;
    } else if (strcmp(token, "keV") == 0) {
      m_energy[ntable] *= 1.0e-3;
    } else if (strcmp(token, "GeV") == 0) {
      m_energy[ntable] *= 1.0e3;
    } else if (strcmp(token, "MeV") != 0) {
      std::cerr << hdr << "Unknown energy unit " << token << "; aborting\n";
      return false;
    }
    // EM loss
    token = strtok(NULL, " ");
    m_emloss.push_back(atof(token));
    // HD loss
    token = strtok(NULL, " ");
    m_hdloss.push_back(atof(token));
    // Projected range
    token = strtok(NULL, " ");
    m_range.push_back(atof(token));
    token = strtok(NULL, " ");
    if (strcmp(token, "A") == 0) {
      m_range[ntable] *= 1.0e-8;
    } else if (strcmp(token, "um") == 0) {
      m_range[ntable] *= 1.0e-4;
    } else if (strcmp(token, "mm") == 0) {
      m_range[ntable] *= 1.0e-1;
    } else if (strcmp(token, "cm") != 0) {
      std::cerr << hdr << "Unknown distance unit " << token << "; aborting\n";
      return false;
    }
    // Longitudinal straggling
    token = strtok(NULL, " ");
    m_longstraggle.push_back(atof(token));
    token = strtok(NULL, " ");
    if (strcmp(token, "A") == 0) {
      m_longstraggle[ntable] *= 1.0e-8;
    } else if (strcmp(token, "um") == 0) {
      m_longstraggle[ntable] *= 1.0e-4;
    } else if (strcmp(token, "mm") == 0) {
      m_longstraggle[ntable] *= 1.0e-1;
    } else if (strcmp(token, "cm") != 0) {
      std::cerr << hdr << "Unknown distance unit " << token << "; aborting\n";
      return false;
    }
    // Transverse straggling
    token = strtok(NULL, " ");
    m_transstraggle.push_back(atof(token));
    token = strtok(NULL, " ");
    if (strcmp(token, "A") == 0) {
      m_transstraggle[ntable] *= 1.0e-8;
    } else if (strcmp(token, "um") == 0) {
      m_transstraggle[ntable] *= 1.0e-4;
    } else if (strcmp(token, "mm") == 0) {
      m_transstraggle[ntable] *= 1.0e-1;
    } else if (strcmp(token, "cm") != 0) {
      std::cerr << hdr << "Unknown distance unit " << token << "; aborting\n";
      return false;
    }

    // Increment table line counter
    ++ntable;
  }

  // Find the scaling factor and convert to MeV/cm
  double scale = -1.;
  while (fsrim.getline(line, 100, '\n')) {
    nread++;
    if (strstr(line, "=============") != NULL) {
      break;
    } else if (strstr(line, "MeV / (mg/cm2)") != NULL ||
               strstr(line, "MeV/(mg/cm2)") != NULL) {
      token = strtok(line, " ");
      scale = std::atof(token);
    }
  }
  if (scale < 0) {
    std::cerr << hdr << "Did not find stopping unit scaling; aborting.\n";
    return false;
  }
  scale *= 1.e3;
  for (unsigned int i = 0; i < ntable; ++i) {
    m_emloss[i] *= scale;
    m_hdloss[i] *= scale;
  }

  // Seems to have worked
  if (m_debug) {
    std::cout << hdr << "Successfully read " << file << "(" << nread
              << " lines).\n";
  }
  return true;
}

void TrackSrim::Print() {

  std::cout << "\nSRIM energy loss table\n\n"
            << "    Energy     EM Loss     HD loss       Range  "
            << "l straggle  t straggle\n"
            << "     [MeV]    [MeV/cm]    [MeV/cm]        [cm] "
            << "      [cm]        [cm]\n\n";
  const unsigned int nPoints = m_emloss.size();
  for (unsigned int i = 0; i < nPoints; ++i) {
    printf("%10g  %10g  %10g  %10g  %10g  %10g\n", m_energy[i],
           m_emloss[i] * m_density, m_hdloss[i] * m_density, m_range[i],
           m_longstraggle[i], m_transstraggle[i]);
  }
  printf("\nWork function:  %g eV\n", m_work);
  printf("Fano factor:    %g\n", m_fano);
  printf("Ion charge:     %g\n", m_q);
  printf("Mass:           %g MeV\n", 1.e-6 * m_mass);
  printf("Density:        %g g/cm3\n", m_density);
  printf("A, Z:           %g, %g\n", m_a, m_z);
}

void TrackSrim::PlotEnergyLoss() {

  // Make a graph for the 3 curves to plot
  double xmin = 0., xmax = 0.;
  double ymin = 0., ymax = 0.;
  const unsigned int nPoints = m_energy.size();
  TGraph* grem = new TGraph(nPoints);
  TGraph* grhd = new TGraph(nPoints);
  TGraph* grtot = new TGraph(nPoints);
  for (unsigned int i = 0; i < nPoints; ++i) {
    const double eplot = m_energy[i];
    if (eplot < xmin) xmin = eplot;
    if (eplot > xmax) xmax = eplot;
    const double emplot = m_emloss[i] * m_density;
    const double hdplot = m_hdloss[i] * m_density;
    const double totplot = emplot + hdplot;
    if (totplot < ymin) ymin = totplot;
    if (totplot > ymax) ymax = totplot;
    grem->SetPoint(i, eplot, emplot);
    grhd->SetPoint(i, eplot, hdplot);
    grtot->SetPoint(i, eplot, totplot);
  }

  // Prepare a plot frame
  TCanvas* celoss = new TCanvas();
  celoss->SetLogx();
  frame(celoss, xmin, 0.0, xmax, ymax * 1.05, "Ion energy [MeV]",
        "Energy loss [MeV/cm]");
  TLegend* legend = new TLegend(0.65, 0.75, 0.94, 0.95);
  legend->SetShadowColor(0);
  legend->SetTextFont(22);
  legend->SetTextSize(0.044);
  legend->SetFillColor(kWhite);
  legend->SetBorderSize(1);

  grem->SetLineColor(kBlue);
  grem->SetLineStyle(kSolid);
  grem->SetLineWidth(2.0);
  grem->SetMarkerStyle(21);
  grem->SetMarkerColor(kBlack);
  grem->Draw("SAME");
  legend->AddEntry(grem, "EM energy loss", "l");

  grhd->SetLineColor(kGreen + 2);
  grhd->SetLineStyle(kSolid);
  grhd->SetLineWidth(2.0);
  grhd->SetMarkerStyle(21);
  grhd->SetMarkerColor(kBlack);
  grhd->Draw("SAME");
  legend->AddEntry(grhd, "HD energy loss", "l");

  grtot->SetLineColor(kOrange);
  grtot->SetLineStyle(kSolid);
  grtot->SetLineWidth(2.0);
  grtot->SetMarkerStyle(21);
  grtot->SetMarkerColor(kBlack);
  grtot->Draw("SAME");
  legend->AddEntry(grtot, "Total energy loss", "l");

  legend->Draw();
}

void TrackSrim::PlotRange() {

  // Make a graph
  double xmin = 0., xmax = 0.;
  double ymin = 0., ymax = 0.;
  const unsigned int nPoints = m_energy.size();
  TGraph* grrange = new TGraph(nPoints);
  for (unsigned int i = 0; i < nPoints; ++i) {
    const double eplot = m_energy[i];
    if (eplot < xmin) xmin = eplot;
    if (eplot > xmax) xmax = eplot;
    const double rangeplot = m_range[i];
    if (rangeplot < ymin) ymin = rangeplot;
    if (rangeplot > ymax) ymax = rangeplot;
    grrange->SetPoint(i, eplot, rangeplot);
  }

  // Prepare a plot frame
  TCanvas* crange = new TCanvas();
  crange->SetLogx();
  frame(crange, xmin, 0.0, xmax, ymax * 1.05, "Ion energy [MeV]",
        "Projected range [cm]");

  grrange->SetLineColor(kOrange);
  grrange->SetLineStyle(kSolid);
  grrange->SetLineWidth(2.0);
  grrange->SetMarkerStyle(21);
  grrange->SetMarkerColor(kBlack);
  grrange->Draw("SAME");
}

void TrackSrim::PlotStraggling() {

  // Make a graph for the 2 curves to plot
  double xmin = 0., xmax = 0.;
  double ymin = 0., ymax = 0.;
  const unsigned int nPoints = m_energy.size();
  TGraph* grlong = new TGraph(nPoints);
  TGraph* grtrans = new TGraph(nPoints);
  for (unsigned int i = 0; i < nPoints; ++i) {
    const double eplot = m_energy[i];
    if (eplot < xmin) xmin = eplot;
    if (eplot > xmax) xmax = eplot;
    const double longplot = m_longstraggle[i];
    const double transplot = m_transstraggle[i];
    if (longplot < ymin) ymin = longplot;
    if (longplot > ymax) ymax = longplot;
    if (transplot < ymin) ymin = transplot;
    if (transplot > ymax) ymax = transplot;
    grlong->SetPoint(i, eplot, longplot);
    grtrans->SetPoint(i, eplot, transplot);
  }

  // Prepare a plot frame
  TCanvas* cstraggle = new TCanvas();
  cstraggle->SetLogx();
  frame(cstraggle, xmin, 0.0, xmax, ymax * 1.05, "Ion energy [MeV]",
        "Straggling [cm]");
  TLegend* legend = new TLegend(0.15, 0.75, 0.44, 0.95);
  legend->SetShadowColor(0);
  legend->SetTextFont(22);
  legend->SetTextSize(0.044);
  legend->SetFillColor(kWhite);
  legend->SetBorderSize(1);

  grlong->SetLineColor(kOrange);
  grlong->SetLineStyle(kSolid);
  grlong->SetLineWidth(2.0);
  grlong->SetMarkerStyle(21);
  grlong->SetMarkerColor(kBlack);
  grlong->Draw("SAME");
  legend->AddEntry(grlong, "Longitudinal", "l");

  grtrans->SetLineColor(kGreen + 2);
  grtrans->SetLineStyle(kSolid);
  grtrans->SetLineWidth(2.0);
  grtrans->SetMarkerStyle(21);
  grtrans->SetMarkerColor(kBlack);
  grtrans->Draw("SAME");
  legend->AddEntry(grtrans, "Transversal", "l");

  legend->Draw();
}

double TrackSrim::DedxEM(const double e) const {
  return Interpolate(e, m_energy, m_emloss);
}

double TrackSrim::DedxHD(const double e) const {
  return Interpolate(e, m_energy, m_hdloss);
}

bool TrackSrim::PreciseLoss(const double step, const double estart,
                            double& deem, double& dehd) const {
  // SRMRKS

  const std::string hdr = m_className + "::PreciseLoss: ";
  // Debugging
  if (m_debug) {
    std::cout << hdr << "\n"
              << "    Initial energy: " << estart << " MeV\n"
              << "    Step: " << step << " cm\n";
  }
  // Precision aimed for.
  const double eps = 1.0e-2;
  // Number of intervals.
  unsigned int ndiv = 1;
  // Loop until precision achieved
  const unsigned int nMaxIter = 10;
  bool converged = false;
  for (unsigned int iter = 0; iter < nMaxIter; ++iter) {
    double e4 = estart;
    double e2 = estart;
    deem = 0.;
    dehd = 0.;
    // Compute rk2 and rk4 over the number of sub-divisions
    const double s = m_density * step / ndiv;
    for (unsigned int i = 0; i < ndiv; i++) {
      // rk2: initial point
      const double em21 = s * DedxEM(e2);
      const double hd21 = s * DedxHD(e2);
      const double de21 = em21 + hd21;
      // Mid-way point
      const double em22 = s * DedxEM(e2 - 0.5 * de21);
      const double hd22 = s * DedxHD(e2 - 0.5 * de21);
      // Trace the rk2 energy
      e2 -= em22 + hd22;
      // rk4: initial point
      const double em41 = s * DedxEM(e4);
      const double hd41 = s * DedxHD(e4);
      const double de41 = em41 + hd41;
      // Mid-way point
      const double em42 = s * DedxEM(e4 - 0.5 * de41);
      const double hd42 = s * DedxHD(e4 - 0.5 * de41);
      const double de42 = em42 + hd42;
      // Second mid-point estimate
      const double em43 = s * DedxEM(e4 - 0.5 * de42);
      const double hd43 = s * DedxHD(e4 - 0.5 * de42);
      const double de43 = em43 + hd43;
      // End point estimate
      const double em44 = s * DedxEM(e4 - de43);
      const double hd44 = s * DedxHD(e4 - de43);
      const double de44 = em44 + hd44;
      // Store the energy loss terms (according to rk4)
      deem += em41 / 6.0 + em42 / 3.0 + em43 / 3.0 + em44 / 6.0;
      dehd += hd41 / 6.0 + hd42 / 3.0 + hd43 / 3.0 + hd44 / 6.0;
      // Store the new energy computed with rk4
      e4 -= de41 / 6.0 + de42 / 3.0 + de43 / 3.0 + de44 / 6.0;
    }
    if (m_debug) {
      std::cout << hdr << "\n    Iteration " << iter << " has " << ndiv 
                << " division(s). Losses:\n";
      printf("\tde4 = %12g, de2 = %12g MeV\n", estart - e2, estart - e4);
      printf("\tem4 = %12g, hd4 = %12g MeV\n", deem, dehd);
    }
    // Compare the two estimates
    if (fabs(e2 - e4) > eps * (fabs(e2) + fabs(e4) + fabs(estart))) {
      // Repeat with twice the number of steps.
      ndiv *= 2;
    } else {
      converged = true;
      break;
    }
  }

  if (!converged) {
    std::cerr << hdr << "No convergence achieved integrating energy loss.\n";
  } else if (m_debug) {
    std::cout << hdr << "Convergence at eps = " << eps << "\n";
  }
  return converged;
}

bool TrackSrim::EstimateRange(const double ekin, const double step,
                              double& stpmax) {
  // Find distance over which the ion just does not lose all its energy
  // ekin       : Kinetic energy [MeV]
  // step       : Step length as guessed [cm]
  // stpmax     : Maximum step
  // SRMDEZ

  const std::string hdr = m_className + "::EstimateRange: ";
  // Initial estimate
  stpmax = step;

  // Find the energy loss expected for the present step length.
  double st1 = step;
  double deem = 0., dehd = 0.;
  PreciseLoss(st1, ekin, deem, dehd);
  double de1 = deem + dehd;
  // Do nothing if this is ok
  if (de1 < ekin) {
    if (m_debug) std::cout << hdr << "Initial step OK.\n";
    return true;
  }
  // Find a smaller step for which the energy loss is less than EKIN.
  double st2 = step / 2.;
  double de2 = de1;
  const unsigned int nMaxIter = 20;
  for (unsigned int iter = 0; iter < nMaxIter; ++iter) {
    // See where we stand
    PreciseLoss(st2, ekin, deem, dehd);
    de2 = deem + dehd;
    // Below the kinetic energy: done
    if (de2 < ekin) break;
    // Not yet below the kinetic energy: new iteration.
    st1 = st2;
    de1 = de2;
    st2 /= 2.;
  }
  if (de2 >= ekin) {
    std::cerr << hdr << "\n    Did not find a smaller step in " << nMaxIter 
              << " iterations. Abandoned.\n";
    stpmax = 0.5 * (st1 + st2);
    return false;
  }
  if (m_debug)
    printf("\tstep 1 = %g cm, de 1 = %g MeV\n\tstep 2 = %g cm, de 2 = %g MeV\n",
           st1, de1 - ekin, st2, de2 - ekin);

  // Now perform a bisection
  for (unsigned int iter = 0; iter < nMaxIter; ++iter) {
    // Avoid division by zero.
    if (de2 == de1) {
      if (m_debug) {
        std::cerr << hdr << "Bisection failed due to equal energy loss for "
                  << "two step sizes. Abandoned.\n";
      }
      stpmax = 0.5 * (st1 + st2);
      return false;
    }
    // Estimate step to give total energy loss.
    double st3;
    if ((fabs(de1 - ekin) < 0.01 * fabs(de2 - de1)) ||
        (fabs(de1 - ekin) > 0.99 * fabs(de2 - de1))) {
      st3 = 0.5 * (st1 + st2);
    } else {
      st3 = st1 - (st2 - st1) * (de1 - ekin) / (de2 - de1);
    }
    // See how well we are doing.
    PreciseLoss(st3, ekin, deem, dehd);
    const double de3 = deem + dehd;
    if (m_debug) printf("\tStep 1 = %g cm, dE 1 = %g MeV\n", st1, de1 - ekin);
    if (m_debug) printf("\tStep 2 = %g cm, dE 2 = %g MeV\n", st2, de2 - ekin);
    if (m_debug) printf("\tStep 3 = %g cm, dE 3 = %g MeV\n", st3, de3 - ekin);
    //  Update the estimates above and below.
    if (de3 > ekin) {
      st1 = st3;
      de1 = de3;
    } else {
      st2 = st3;
      de2 = de3;
    }
    // See whether we've converged.
    if (fabs(de3 - ekin) < 1e-3 * (fabs(de3) + fabs(ekin)) ||
        fabs(st1 - st2) < 1e-3 * (fabs(st1) + fabs(st2))) {
      stpmax = st1 - (st2 - st1) * (de1 - ekin) / (de2 - de1);
      return true;
    }
  }
  if (m_debug) {
    std::cout << hdr << "Bisection did not converge in " << nMaxIter 
              << " steps. Abandoned.\n";
  }
  stpmax = st1 - (st2 - st1) * (de1 - ekin) / (de2 - de1);
  return false;
}

bool TrackSrim::Generate() {
  // Generates electrons for a SRIM track
  // SRMGEN
  const std::string hdr = m_className + "::Generate: ";

  // Verify that a sensor has been set
  if (!m_sensor) {
    std::cerr << m_className << "::Generate:\n"
              << "    Sensor is not defined.\n";
    return false;
  }

  // Get the bounding box.
  double xmin = 0., ymin = 0., zmin = 0.;
  double xmax = 0., ymax = 0., zmax = 0.;
  if (!m_sensor->GetArea(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << m_className << "::Generate:\n"
              << "    Drift area is not set.\n";
    return false;
  }

  // Header of debugging output.
  if (m_debug) {
    std::cout << hdr << "\n"
              << "    Track generation with the following parameters:\n";
    const unsigned int nTable = m_energy.size();
    printf("      Table size         %u\n", nTable);
    printf("      Track length       %g cm\n", m_tracklength);
    printf("      Particle energy    %g eV\n", m_initialenergy);
    printf("      Particle mass      %g eV\n", m_mass);
    printf("      Particle charge    %g\n", m_q);
    printf("      Work function      %g eV\n", m_work);
    printf("      Fano factor        %g\n", m_fano);
    printf("      Long. straggling:  %d\n", m_useLongStraggle);
    printf("      Trans. straggling: %d\n", m_useTransStraggle);
    //    printf("Vavilov generator: %d\n",     m_precisevavilov);
    printf("      Cluster size       %d\n", m_nsize);
  }

  // Verify that the parameters have been set
  if (!m_trackset) {
    std::cerr << hdr << "\n    Track location not set.\n";
    return false;
  }
  if (m_mass < 0.) {
    std::cerr << hdr << "\n    Particle mass not set.\n";
    return false;
  }
  if (!m_chargeset) {
    std::cerr << hdr << "\n    Particle charge not set.\n";
    return false;
  }
  if (m_initialenergy < 0.) {
    std::cerr << hdr << "\n    Initial particle energy not set.\n";
    return false;
  }
  if (m_work < 0.) {
    std::cerr << hdr << "\n    Work function not set.\n";
    return false;
  }
  if (m_fano < 0.) {
    std::cerr << hdr << "\n    Fano factor function not set.\n";
    return false;
  }
  if (m_a < 0. || m_z < 0.) {
    std::cerr << hdr << "\n    A and/or Z not set.\n";
    return false;
  }

  // Reset the cluster count
  m_currcluster = 0;
  m_clusters.clear();

  // Maximum number of clusters
  const int mxclus = 200;

  // Initial situation: starting position
  double x = m_xt0;
  double y = m_yt0;
  double z = m_zt0;

  // Check the initial energy.
  if (m_initialenergy < 1.e-20 * m_mass || m_initialenergy < 1.e-9 * m_work) {
    if (m_debug) {
      std::cout << hdr << "Initial particle energy E = " << m_initialenergy
                << " MeV such that beta2 = 0 or E << W; particle stopped.\n";
    }
    return true;
  }
  // Store the energy [MeV].
  double e = 1.e-6 * m_initialenergy;
  // Total distance covered
  double dsum = 0.0;
  // Pool of unused energy
  double epool = 0.0;

  // Loop generating clusters
  int iter = 0;
  while (iter < m_maxclusters || m_maxclusters < 0) {
    // Work out what the energy loss per cm, straggling and projected range are
    // at the start of the step.
    const double dedxem = DedxEM(e) * m_density;
    const double dedxhd = DedxHD(e) * m_density;
    const double prange = Interpolate(e, m_energy, m_range);
    double strlon = Interpolate(e, m_energy, m_longstraggle);
    double strlat = Interpolate(e, m_energy, m_transstraggle);

    if (!m_useLongStraggle) strlon = 0;
    if (!m_useTransStraggle) strlat = 0;

    if (m_debug) {
      std::cout << hdr << "\n    Energy = " << e << " MeV,\n    dEdx em, hd = "
                << dedxem << ", " << dedxhd << " MeV/cm,\n    e-/cm = " 
                << 1.e6 * dedxem / m_work << ".\n    Straggling long/lat: " 
                << strlon << ", " << strlat << " cm\n"; 
    }
    // Find the step size for which we get approximately the target # clusters.
    double step;
    if (m_nsize > 0) {
      step = m_nsize * 1.e-6 * m_work / dedxem;
    } else if (m_nsize < -1.5) {
      step = m_tracklength;
    } else {
      step = m_initialenergy / (0.5 * mxclus * 1.e6 * (dedxem + dedxhd));
    }
    // Truncate if this step exceeds the length.
    bool finish = false;
    if (dsum + step > m_tracklength) {
      step = m_tracklength - dsum;
      finish = true;
      if (m_debug) std::cout << hdr << "Finish raised. Track length reached.\n";
    }
    // Make an accurate integration of the energy loss over the step.
    double deem = 0., dehd = 0.;
    PreciseLoss(step, e, deem, dehd);
    // If the energy loss exceeds the particle energy, truncate step.
    double stpmax;
    if (deem + dehd > e) {
      EstimateRange(e, step, stpmax);
      step = stpmax;
      PreciseLoss(step, e, deem, dehd);
      deem = e * deem / (dehd + deem);
      dehd = e - deem;
      finish = true;
      if (m_debug) std::cout << hdr << "Finish raised. Track length reached.\n";
    } else {
      stpmax = m_tracklength - dsum;
    }

    // Ensure that this is larger than the minimum modelable step size.
    double stpmin;
    if (!SmallestStep(e, deem, step, stpmin)) {
      std::cerr << hdr << "\n    Failure computing the minimum step size."
                << "\n    Clustering abandoned.\n";
      return false;
    }

    double eloss;
    if (stpmin > stpmax) {
      // No way to find a suitable step size: use fixed energy loss
      if (m_debug) std::cout << hdr << "stpmin > stpmax. Deposit all energy.\n";
      eloss = deem;
      if (e - eloss - dehd < 0) eloss = e - dehd;
      finish = true;
      if (m_debug) std::cout << hdr << "Finish raised. Single deposit.\n";
    } else if (step < stpmin) {
      // If needed enlarge the step size
      if (m_debug) std::cout << hdr << "Enlarging step size.\n";
      step = stpmin;
      PreciseLoss(step, e, deem, dehd);
      if (deem + dehd > e) {
        if (m_debug) std::cout << hdr << "Excess loss. Recomputing stpmax.\n";
        EstimateRange(e, step, stpmax);
        step = stpmax;
        PreciseLoss(step, e, deem, dehd);
        deem = e * deem / (dehd + deem);
        dehd = e - deem;
        eloss = deem;
      } else {
        eloss = RndmEnergyLoss(e, deem, step);
      }
      // Draw an actual energy loss for such a step.
    } else {
      if (m_debug) std::cout << hdr << "Using existing step size.\n";
      eloss = RndmEnergyLoss(e, deem, step);
    }
    // Ensure we are neither below 0 nor above the total energy.
    if (eloss < 0) {
      if (m_debug) std::cout << hdr << "Truncating negative energy loss.\n";
      eloss = 0;
    } else if (eloss > e - dehd) {
      if (m_debug) std::cout << hdr << "Excess energy loss, using mean.\n";
      eloss = deem;
      if (e - eloss - dehd < 0) {
        eloss = e - dehd;
        finish = true;
        if (m_debug) std::cout << hdr << "Finish raised. Using mean energy.\n";
      }
    }
    if (m_debug) {
      std::cout << hdr << "\n    Step length = " << step << " cm.\n    "
                << "Mean loss =   " << deem << " MeV.\n    "
                << "Actual loss = " << eloss << " MeV.\n";
    }

    // Check that the cluster is in an ionisable medium and within bounding box
    Medium* medium = NULL;
    if (!m_sensor->GetMedium(x, y, z, medium)) {
      std::cerr << m_className << "::Generate:\n";
      std::cerr << "    No medium at position ("
		<< x << "," << y << "," << z << "), clustering incomplete.\n";
      return false;
    } else if (!medium->IsIonisable()) {
      std::cerr << "::Generate:\n";
      std::cerr << "    Medium at ("
		<< x << "," << y << "," << z << ") is not ionisable, clustering incomplete.\n";
      return false;
    } else if (!m_sensor->IsInArea(x, y, z)) {
      std::cerr << m_className << "::Generate:\n";
      std::cerr << "    Cluster at ("
		<< x << "," << y << "," << z << ") outside bounding box, clustering incomplete.\n";
      return false;
    }

    // Add a cluster.
    cluster newcluster;
    newcluster.x = x;
    newcluster.y = y;
    newcluster.z = z;
    if (m_fano < 0) {
      newcluster.electrons = int((eloss + epool) / (1.e-6 * m_work));
      newcluster.ec = m_work * newcluster.electrons;
    } else {
      double ecl = 1.e6 * (eloss + epool);
      newcluster.electrons = 0.0;
      newcluster.ec = 0.0;
      while (true) {
	//	if(newcluster.ec < 100) printf("ec = %g\n", newcluster.ec);
        const double ernd1 = RndmHeedWF(m_work, m_fano);
        if (ernd1 > ecl) break;
        newcluster.electrons++;
        newcluster.ec += ernd1;
        ecl -= ernd1;
      }
      //      printf("ec = %g DONE\n", newcluster.ec);
      if (m_debug)
        std::cout << hdr << "EM + pool: " << 1.e6 * (eloss + epool) 
                  << " eV, W: " << m_work << " eV, E/w: " 
                  << (eloss + epool) / (1.0e-6 * m_work) << ", n: " 
                  << newcluster.electrons << ".\n"; 
    }
    newcluster.kinetic = e;
    epool += eloss - 1.e-6 * newcluster.ec;
    if (m_debug) {
      std::cout << hdr << "Cluster " << m_clusters.size() << "\n    at (" 
                << newcluster.x << ", " << newcluster.y << ", " << newcluster.z
                << "),\n    e = " << newcluster.ec << ",\n    n = " 
                << newcluster.electrons << ",\n    pool = " 
                << epool << " MeV.\n";
    }
    m_clusters.push_back(newcluster);

    // Keep track of the length and energy
    dsum += step;
    e -= eloss + dehd;
    // Stop if the flag is raised
    if (finish) {
      if (m_debug) std::cout << hdr << "Finishing flag raised.\n";
      break;
      // Stop if the distance has been reached
    } else if (dsum > m_tracklength) {
      if (m_debug) std::cout << hdr << "Reached track length.\n";
      break;
      // Single cluster
    } else if (m_nsize < -1.5) {
      if (m_debug) std::cout << hdr << "Single cluster requested.\n";
      break;
      // No energy left
    } else if (e < m_initialenergy * 1.e-9) {
      if (m_debug) std::cout << hdr << "Energy exhausted.\n";
      break;
    }
    // Draw scattering distances
    const double scale = sqrt(step / prange);
    const double sigt1 = RndmGaussian(0., scale * strlat);
    const double sigt2 = RndmGaussian(0., scale * strlat);
    const double sigl = RndmGaussian(0., scale * strlon);
    if (m_debug) std::cout << hdr << "sigma l, t1, t2: " 
                         << sigl << ", " << sigt1 << ", " << sigt2 << "\n";
    // Rotation angles to bring z-axis in line
    double theta, phi;
    if (m_xdir * m_xdir + m_zdir * m_zdir <= 0) {
      if (m_ydir < 0) {
        theta = -HalfPi;
      } else if (m_ydir > 0) {
        theta = +HalfPi;
      } else {
        std::cerr << hdr << "\n    Zero step length; clustering abandoned.\n";
        return false;
      }
      phi = 0;
    } else {
      phi = atan2(m_xdir, m_zdir);
      theta = atan2(m_ydir, sqrt(m_xdir * m_xdir + m_zdir * m_zdir));
    }

    // Update position
    const double cp = cos(phi);
    const double ct = cos(theta);
    const double sp = sin(phi);
    const double st = sin(theta);
    x += step * m_xdir + cp * sigt1 - sp * st * sigt2 + sp * ct * sigl;
    y += step * m_ydir + ct * sigt2 + st * sigl;
    z += step * m_zdir - sp * sigt1 - cp * st * sigt2 + cp * ct * sigl;

    // (Do not) update direction
    if (false) {
      m_xdir = step * m_xdir + cp * sigt1 - sp * st * sigt2 + sp * ct * sigl;
      m_ydir = step * m_ydir + ct * sigt2 + st * sigl;
      m_zdir = step * m_zdir - sp * sigt1 - cp * st * sigt2 + cp * ct * sigl;
      double dnorm = sqrt(m_xdir * m_xdir + m_ydir * m_ydir + m_zdir * m_zdir);
      if (dnorm <= 0) {
        std::cerr << hdr << "\n    Zero step length; clustering abandoned.\n";
        return false;
      }
      m_xdir = m_xdir / dnorm;
      m_ydir = m_ydir / dnorm;
      m_zdir = m_zdir / dnorm;
    }
    // Next cluster
    iter++;
  }
  if (iter == m_maxclusters) {
    std::cerr << hdr << "Exceeded maximum number of clusters.\n";
  }
  return true;
  // finished generating
}

bool TrackSrim::SmallestStep(const double ekin, double de, double step,
                             double& stpmin) {
  // Determines the smallest step size for which there is little
  // or no risk of finding negative energy fluctuations.
  // SRMMST

  const std::string hdr = m_className + "::SmallestStep: ";
  const double expmax = 30;

  // By default, assume the step is right.
  stpmin = step;
  // Check correctness.
  if (ekin <= 0 || de <= 0 || step <= 0) {
    std::cerr << hdr << "\n    Input parameters not valid.\n    Ekin = "
              << ekin << " MeV, dE = " << de << " MeV, step length = " 
              << step << " cm.\n";
    return false;
  } else if (m_mass <= 0 || fabs(m_q) <= 0) {
    std::cerr << hdr << "\n    Track parameters not valid.\n    Mass = "
              << m_mass << " eV, charge = " << m_q << ".\n";
    return false;
  } else if (m_a <= 0 || m_z <= 0 || m_density <= 0) {
    std::cerr << hdr << "\n    Gas parameters not valid.\n    A = " << m_a
              << ", Z = " << m_z  << " density = " << m_density << " g/cm3.\n";
    return false;
  }

  // Basic kinematic parameters
  const double rkin = 1.e6 * ekin / m_mass;
  const double gamma = 1. + rkin;
  const double beta2 = rkin > 1.e-5 ? 1. - 1. / (gamma * gamma) : 2. * rkin;

  // Compute maximum energy transfer [MeV]
  const double rm = ElectronMass / m_mass;
  const double emax = 2 * ElectronMass * 1.e-6 * beta2 * gamma * gamma /
                      (1. + 2 * gamma * rm + rm * rm);
  // Compute the Rutherford term
  const double fconst = 0.1534;
  double xi = fconst * m_q * m_q * m_z * m_density * step / (m_a * beta2);
  // Compute the scaling parameter
  double rkappa = xi / emax;
  // Step size and energy loss
  double denow = de;
  double stpnow = step;
  const unsigned int nMaxIter = 10;
  for (unsigned int iter = 0; iter < nMaxIter; ++iter) {
    bool retry = false;
    // Debugging output.
    if (m_debug) {
      PrintSettings(hdr, denow, stpnow, ekin, beta2, gamma, m_a, m_z, m_density,
                    m_q, m_mass, emax, xi, rkappa);
    }
    double xinew = xi;
    double rknew = rkappa;
    if (m_model <= 0 || m_model > 4) {
      // No fluctuations: any step is permitted
      stpmin = stpnow;
    } else if (m_model == 1) {
      // Landau distribution
      const double xlmin = -3.;
      const double exponent = -xlmin - 1. + Gamma - beta2 - denow / xi;
      const double rklim = exponent < -expmax ? 0. : exp(exponent);
      stpmin = stpnow * (rklim / rkappa);
      if (m_debug) {
        std::cout << hdr << "Landau distribution is imposed.\n    kappa_min = "
                  << rklim << ", d_min = " << stpmin << " cm.\n";
      }
    } else if (m_model == 2) {
      // Vavilov distribution, ensure we're in range.
      const double xlmin = StepVavilov(rkappa);
      const double exponent = -xlmin - 1. + Gamma - beta2 - denow / xi;
      const double rklim = exponent < -expmax ? 0. : exp(exponent);
      stpmin = stpnow * (rklim / rkappa);
      xinew = fconst * m_q * m_q * m_z * m_density * stpmin / (m_a * beta2);
      rknew = xinew / emax;
      if (m_debug) {
        std::cout << hdr << "Vavilov distribution is imposed.\n    kappa_min = "
                  << rklim << ", d_min = " << stpmin << " cm\n    kappa_new = "
                  << rknew << ", xi_new = " << xinew << " MeV.\n";
      }
      if (stpmin > stpnow * 1.1) {
        if (m_debug) std::cout << hdr << "Step size increase. New pass.\n";
        retry = true;
      }
    } else if (m_model == 3) {
      // Gaussian model
      stpmin = stpnow * 16 * xi * emax * (1 - beta2 / 2) / (denow * denow);
      if (m_debug) {
        std::cout << hdr << "Gaussian distribution is imposed.\n";
        printf("\td_min = %g cm.\n\tsigma/mu_old = %g, sigma/mu_min = %g\n",
               stpmin, sqrt(xi * emax * (1 - beta2 / 2)) / de,
               sqrt((fconst * m_q * m_q * m_z * m_density * stpmin /
                     (m_a * beta2)) *
                    emax * (1 - beta2 / 2)) /
                   (stpmin * denow / stpnow));
      }
    } else if (rkappa < 0.05) {
      // Combined model: for low kappa, use the Landau distribution.
      const double xlmin = -3.;
      const double exponent = -xlmin - 1. + Gamma - beta2 - denow / xi;
      const double rklim = exponent < -expmax ? 0. : exp(exponent);
      stpmin = stpnow * (rklim / rkappa);
      xinew = fconst * m_q * m_q * m_z * m_density * stpmin / (m_a * beta2);
      rknew = xinew / emax;
      if (m_debug) {
        std::cout << hdr << "Landau distribution automatic.\n    kappa_min = "
                  << rklim << ", d_min = " << stpmin << " cm.\n";
      }
      if (rknew > 0.05 || stpmin > stpnow * 1.1) {
        retry = true;
        if (m_debug) {
          std::cout << hdr << "Model change or step increase. New pass.\n";
        }
      }
    } else if (rkappa < 5) {
      // For medium kappa, use the Vavilov distribution
      const double xlmin = StepVavilov(rkappa);
      const double exponent = -xlmin - 1. + Gamma - beta2 - denow / xi;
      const double rklim = exponent < -expmax ? 0. : exp(exponent);
      stpmin = stpnow * (rklim / rkappa);
      xinew = fconst * m_q * m_q * m_z * m_density * stpmin / (m_a * beta2);
      rknew = xinew / emax;
      if (m_debug) {
        std::cout << hdr << "Vavilov distribution automatic.\n    kappa_min = "
                  << rklim << ", d_min = " << stpmin << " cm\n    kappa_new = "
                  << rknew << ", xi_new = " << xinew << " MeV.\n";
      }
      if (rknew > 5 || stpmin > stpnow * 1.1) {
        retry = true;
        if (m_debug) {
          std::cout << hdr << "Model change or step increase. New pass.\n";
        }
      }
    } else {
      // And for large kappa, use the Gaussian values.
      stpmin = stpnow * 16 * xi * emax * (1 - beta2 / 2) / (denow * denow);
      if (m_debug) {
        std::cout << hdr << "Gaussian distribution automatic.\n";
        printf("\td_min = %g cm.\n\tsigma/mu_old = %g, sigma/mu_min = %g\n",
               stpmin, sqrt(xi * emax * (1 - beta2 / 2)) / de,
               sqrt((fconst * m_q * m_q * m_z * m_density * stpmin /
                     (m_a * beta2)) *
                    emax * (1 - beta2 / 2)) /
                   (stpmin * denow / stpnow));
      }
    }
    // See whether we should do another pass.
    if (stpnow > stpmin) {
      if (m_debug) {
        std::cout << hdr << "Step size ok, minimum: " << stpmin << " cm\n";
      }
      break;
    }
    if (!retry) {
      if (m_debug) {
        std::cerr << hdr << "\nStep size must be increased to " 
                  << stpmin << "cm.\n";
      }
      break;
    }
    // New iteration
    rkappa = rknew;
    xi = xinew;
    denow *= stpmin / stpnow;
    stpnow = stpmin;
    if (m_debug) std::cout << hdr << "Iteration " << iter << "\n";
    if (iter == nMaxIter - 1) {
      // Need interation, but ran out of tries
      std::cerr << hdr << "\n    No convergence reached on step size.\n";
    }
  }
  return true;
}

double TrackSrim::RndmEnergyLoss(const double ekin, const double de,
                                 const double step) {
  //   RNDDE  - Generates a random energy loss.
  //   VARIABLES : EKIN       : Kinetic energy [MeV]
  //            DE         : Mean energy loss over the step [MeV]
  //            STEP       : Step length [cm]
  //            BETA2      : Velocity-squared
  //            GAMMA      : Projectile gamma
  //            EMAX       : Maximum energy transfer per collision [MeV]
  //            XI         : Rutherford term [MeV]
  //            FCONST     : Proportionality constant
  //            EMASS      : Electron mass [MeV]
  // (Last changed on 26/10/07.)

  const std::string hdr = "TrackSrim::RndmEnergyLoss: ";
  // Check correctness.
  if (ekin <= 0 || de <= 0 || step <= 0) {
    std::cerr << hdr << "\n    Input parameters not valid.\n    Ekin = " 
              << ekin << " MeV, dE = " << de << " MeV, step length = " 
              << step << " cm.\n";
    return 0.;
  } else if (m_mass <= 0 || fabs(m_q) <= 0) {
    std::cerr << hdr << "\n    Track parameters not valid.\n    Mass = "
              << m_mass << " MeV, charge = " << m_q << ".\n";
    return 0.;
  } else if (m_a <= 0 || m_z <= 0 || m_density <= 0) {
    std::cerr << hdr << "\n    Material parameters not valid.\n A = " << m_a 
              << ", Z = " << m_z << ", density = " << m_density << " g/cm3.\n";
    return 0.;
  }
  // Basic kinematic parameters
  const double rkin = 1.e6 * ekin / m_mass;
  const double gamma = 1. + rkin;
  const double beta2 = rkin > 1.e-5 ? 1. - 1. / (gamma * gamma) : 2. * rkin;

  // Compute maximum energy transfer
  const double rm = ElectronMass / m_mass;
  const double emax = 2 * ElectronMass * 1.e-6 * beta2 * gamma * gamma /
                      (1. + 2 * gamma * rm + rm * rm);
  // Compute the Rutherford term
  const double fconst = 0.1534;
  const double xi = fconst * m_q * m_q * m_z * m_density * step / (m_a * beta2);
  // Compute the scaling parameter
  const double rkappa = xi / emax;
  // Debugging output.
  if (m_debug) {
    PrintSettings(hdr, de, step, ekin, beta2, gamma, m_a, m_z, m_density, m_q,
                  m_mass, emax, xi, rkappa);
  }
  double rndde = de;
  if (m_model <= 0 || m_model > 4) {
    // No fluctuations.
    if (m_debug) std::cout << hdr << "Fixed energy loss.\n";
  } else if (m_model == 1) {
    // Landau distribution
    if (m_debug) std::cout << hdr << "Landau imposed.\n";
    const double xlmean = -(log(rkappa) + beta2 + 1. - Gamma);
    rndde += xi * (RndmLandau() - xlmean);
  } else if (m_model == 2) {
    // Vavilov distribution, ensure we are in range.
    if (m_debug) std::cout << hdr << "Vavilov imposed.\n";
    if (rkappa > 0.01 && rkappa < 12) {
      const double xvav = RndmVavilov(rkappa, beta2);
      rndde += xi * (xvav + log(rkappa) + beta2 + (1 - Gamma));
    }
  } else if (m_model == 3) {
    // Gaussian model
    if (m_debug) std::cout << hdr << "Gaussian imposed.\n";
    rndde += RndmGaussian(0., sqrt(xi * emax * (1 - beta2 / 2)));
  } else if (rkappa < 0.05) {
    // Combined model: for low kappa, use the landau distribution.
    if (m_debug) std::cout << hdr << "Landau automatic.\n";
    const double xlmean = -(log(rkappa) + beta2 + (1 - Gamma));
    const double par[] = {0.50884,    1.26116, 0.0346688,  1.46314,
                          0.15088e-2, 1.00324, -0.13049e-3};
    const double xlmax = par[0] + par[1] * xlmean + par[2] * xlmean * xlmean +
                         par[6] * xlmean * xlmean * xlmean +
                         (par[3] + xlmean * par[4]) * exp(par[5] * xlmean);
    double xlan = RndmLandau();
    for (unsigned int iter = 0; iter < 100; ++iter) {
      if (xlan < xlmax) break;
      xlan = RndmLandau();
    }
    rndde += xi * (xlan - xlmean);
  } else if (rkappa < 5) {
    // For medium kappa, use the Vavilov distribution, precise
    // } else if (m_precisevavilov && rkappa < 5) {
    //   printf("Vavilov slow automatic\n");
    //   rndde = de+xi*(rndvvl(rkappa,beta2) + log(xi/emax)+beta2+(1-Gamma));
    //   // ... or fast.
    if (m_debug) std::cout << hdr << "Vavilov fast automatic.\n";
    const double xvav = RndmVavilov(rkappa, beta2);
    rndde += xi * (xvav + log(rkappa) + beta2 + (1 - Gamma));
  } else {
    // And for large kappa, use the Gaussian values.
    if (m_debug) std::cout << hdr << "Gaussian automatic.\n";
    rndde = RndmGaussian(de, sqrt(xi * emax * (1 - beta2 / 2)));
  }
  // Debugging output
  if (m_debug)
    std::cout << hdr << "Energy loss generated = " << rndde << " MeV.\n";
  return rndde;
}

bool TrackSrim::GetCluster(double& xcls, double& ycls, double& zcls,
                           double& tcls, int& n, double& e, double& extra) {
  if (m_debug) {
    printf("Current cluster: %d, array size: %ld", 
           m_currcluster, m_clusters.size());
  }
  // Stop if we have exhausted the list of clusters.
  if (m_currcluster >= m_clusters.size()) return false;

  xcls = m_clusters[m_currcluster].x;
  ycls = m_clusters[m_currcluster].y;
  zcls = m_clusters[m_currcluster].z;

  tcls = 0;
  n = m_clusters[m_currcluster].electrons;
  e = m_clusters[m_currcluster].ec;
  extra = m_clusters[m_currcluster].kinetic;
  // Move to next cluster
  ++m_currcluster;
  return true;
}

// Mandatory due to error in Track.hh
// TODO: why can't we use this interface?
bool TrackSrim::NewTrack(const double /*x0*/, const double /*y0*/,
                         const double /*z0*/, const double /*t0*/,
                         const double /*dx0*/, const double /*dy0*/,
                         const double /*dz0*/) {
  return false;
}
}
