#include <iostream>
#include <cstdlib>
#include <fstream>

#include <TCanvas.h>
#include <TGraph.h>
#include <TH1.h>
#include <TLegend.h>
#include <TMath.h>

#include "TrackSrim.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"
#include "Numerics.hh"

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
  } else {
    xlmin = -3.7;
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
}

namespace Garfield {

TrackSrim::TrackSrim()
    : m_precisevavilov(false),
      m_useTransStraggle(true),
      m_useLongStraggle(true),
      m_trackset(false),
      m_massset(false),
      m_chargeset(false),
      m_energyset(false),
      m_workset(false),
      m_fanoset(false),
      m_azset(false),
      m_work(0.0),
      m_fano(0.0),
      m_q(0.0),
      m_mass(-1.0),
      m_model(4),
      m_nsize(200) {
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
    printf(
        "TrackSrim::SetTrack Track length = 0; no SRIM cluster generation.\n");
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

  const std::string hdr = m_className + "::ReadFile:\n    ";
  // Open the material list.
  std::ifstream fsrim;
  fsrim.open(file.c_str(), std::ios::in);
  if (fsrim.fail()) {
    std::cerr << hdr << "Could not open SRIM  file " << file
              << " for reading.\n    The file perhaps does not exist.\n";
    return false;
  }
  int nread = 0;

  // Read the header
  if (m_debug) {
    std::cout << hdr << "SRIM header records from file " << file << "\n";
  }
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

  // Identify the ion
  char* token = NULL;
  token = strtok(line, " []=");
  token = strtok(NULL, " []=");
  token = strtok(NULL, " []=");
  SetCharge(std::atof(token));
  token = strtok(NULL, " []=");
  token = strtok(NULL, " []=");
  token = strtok(NULL, " []=");
  SetMass(std::atof(token) * 931.494061);  // Convert amu to eV

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
  for (unsigned int i = 0; i < ntable; ++i) {
    m_emloss[i] *= scale * 1.e3;
    m_hdloss[i] *= scale * 1.e3;
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
  printf("Mass:           %g\n", m_mass);
  printf("Density:        %g g/cm3\n", m_density);
  printf("A, Z:           %g, %g\n", m_a, m_z);
}

void TrackSrim::PlotEnergyLoss() {

  // Make a graph for the 3 curves to plot
  // TODO: find min/max using STL
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

void TrackSrim::PreciseLoss(const double step, const double estart,
                            double& deem, double& dehd) const {

  const std::string hdr = m_className + "::PreciseLoss: ";
  // Debugging
  if (m_debug)
    printf("TrackSrim::PreciseLoss\n\tinitial energy: %g MeV,\n\tstep: %g cm\n",
           estart, step);

  // Precision aimed for
  const double eps = 1.0e-2;

  // Initial settings
  int ndiv = 1;
  // Loop until precision achieved
  const unsigned int nMaxIter = 10;
  for (unsigned int iter = 0; iter < nMaxIter; ++iter) {
    double e4 = estart;
    double e2 = estart;
    deem = 0.;
    dehd = 0.;
    // Compute rk2 and rk4 over the number of sub-divisions
    const double s = step / ndiv;
    for (int i = 0; i < ndiv; i++) {
      // rk2: initial point
      const double em21 = s * DedxEM(e2) * m_density;
      const double hd21 = s * DedxHD(e2) * m_density;
      // mid-way point
      const double em22 = s * DedxEM(e2 - (em21 + hd21) / 2) * m_density;
      const double hd22 = s * DedxHD(e2 - (em21 + hd21) / 2) * m_density;
      // rk4: initial point
      const double em41 = s * DedxEM(e4) * m_density;
      const double hd41 = s * DedxHD(e4) * m_density;
      // mid-way point
      const double em42 = s * DedxEM(e4 - (em41 + hd41) / 2) * m_density;
      const double hd42 = s * DedxHD(e4 - (em41 + hd41) / 2) * m_density;
      // second mid-point estimate
      const double em43 = s * DedxEM(e4 - (em42 + hd42) / 2) * m_density;
      const double hd43 = s * DedxHD(e4 - (em42 + hd42) / 2) * m_density;
      // end point estimate
      const double em44 = s * DedxEM(e4 - (em43 + hd43)) * m_density;
      const double hd44 = s * DedxHD(e4 - (em43 + hd43)) * m_density;
      // store the energy loss terms (according to rk4)
      deem += em41 / 6.0 + em42 / 3.0 + em43 / 3.0 + em44 / 6.0;
      dehd += hd41 / 6.0 + hd42 / 3.0 + hd43 / 3.0 + hd44 / 6.0;
      // trace the rk2 energy
      e2 -= em22 + hd22;
      // store the new energy computed with rk4
      e4 -= (em41 + hd41) / 6.0 + (em42 + hd42) / 3.0 + (em43 + hd43) / 3.0 +
            (em44 + hd44) / 6.0;
    }
    if (m_debug) {
      printf("TrackSrim::PreciseLoss Iteration %d has %d divisions, losses:\n",
             iter, ndiv);
      printf("\tde4 = %12g, de2 = %12g MeV\n", estart - e2, estart - e4);
      printf("\tem4 = %12g, hd4 = %12g MeV\n", deem, dehd);
    }
    // compare the two estimates
    if (fabs(e2 - e4) > eps * (fabs(e2) + fabs(e4) + fabs(estart))) {
      if (iter == nMaxIter - 1) {
        std::cerr << hdr
                  << "No convergence achieved integrating energy loss.\n";
        break;
      }
      ndiv *= 2;
    } else {
      if (m_debug) std::cout << hdr << "Convergence at eps = " << eps << "\n";
      break;
    }
  }
}

bool TrackSrim::EstimateRange(const double ekin, const double step,
                              double& stpmax) {
  // Find distance over which the ion just does not lose all its energy
  // ekin       : Kinetic energy [MeV]
  // step       : Step length as guessed [cm]
  // stpmax     : Maximum step

  // Initial estimate
  stpmax = step;

  // Find the energy loss expected for the present step length.
  double st1 = step;
  double deem, dehd;
  PreciseLoss(st1, ekin, deem, dehd);
  double de1 = deem + dehd;
  // Do nothing if this is ok
  if (de1 < ekin) {
    if (m_debug) printf("TrackSrim::EstimateRange Initial step OK.\n");
    return true;
  }

  // Find a smaller step for which the energy loss is less than EKIN
  double st2 = step / 2;
  double de2 = de1;
  for (unsigned int iter = 0; iter < 20; ++iter) {
    // See where we stand
    PreciseLoss(st2, ekin, deem, dehd);
    de2 = deem + dehd;
    // Below the kinetic energy: done
    if (de2 < ekin) break;
    // Not yet below the kinetic energy: new iteration.
    st1 = st2;
    de1 = de2;
    st2 /= 2;
  }
  if (de2 >= ekin) {
    std::cerr << m_className << "::EstimateRange:\n"
              << "  Did not find a smaller step in 20 iterations. Abandoned.\n";
    stpmax = 0.5 * (st1 + st2);
    return false;
  }
  if (m_debug)
    printf("\tstep 1 = %g cm, de 1 = %g MeV\n\tstep 2 = %g cm, de 2 = %g MeV\n",
           st1, de1 - ekin, st2, de2 - ekin);

  // Now perform a bisection
  for (unsigned int iter = 0; iter < 20; ++iter) {
    // Avoid division by zero.
    if (de2 == de1) {
      if (m_debug)
        printf(
            "TrackSrim::EstimateRange Bisection failed due to equal energy "
            "loss for 2 step sizes; abandoned.\n");
      stpmax = 0.5 * (st1 + st2);
      return 1;
    }
    // Estimate step to give total energy loss.
    double st3;
    if (fabs(de1 - ekin) < 0.01 * fabs(de2 - de1) ||
        fabs(de1 - ekin) > 0.99 * fabs(de2 - de1)) {
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
  if (m_debug)
    printf(
        "TrackSrim::EstimateRange Bisection did not converge in 20 steps; "
        "abandoned.\n");
  stpmax = st1 - (st2 - st1) * (de1 - ekin) / (de2 - de1);
  return false;
}

bool TrackSrim::Generate() {
  // Generates electrons for a SRIM track
  const std::string hdr = m_className + "::Generate: ";

  // Header of debugging output.
  if (m_debug) {
    printf("TrackSrim::Generate\n");
    printf("\tTrack generation with the following parameters:\n");
    const unsigned int nTable = m_energy.size();
    printf("\tTable size         %u\n", nTable);
    printf("\tTrack length       %g cm\n", m_tracklength);
    printf("\tParticle energy    %g MeV\n", m_initialenergy);
    printf("\tParticle mass      %g MeV\n", m_mass);
    printf("\tParticle charge    %g\n", m_q);
    printf("\tWork function      %g eV\n", m_work);
    printf("\tFano factor        %g\n", m_fano);
    printf("\tLong. straggling:  %d\n", m_useLongStraggle);
    printf("\tTrans. straggling: %d\n", m_useTransStraggle);
    //    printf("Vavilov generator: %d\n",     m_precisevavilov);
    printf("\tCluster size       %d\n", m_nsize);
  }

  // Verify that the parameters have been set
  if (!m_trackset) {
    std::cerr << hdr << "\n    Track location not set.\n";
    return false;
  }
  if (!m_massset) {
    std::cerr << hdr << "\n    Particle mass not set.\n";
    return false;
  }
  if (!m_chargeset) {
    std::cerr << hdr << "\n    Particle charge not set.\n";
    return false;
  }
  if (!m_energyset) {
    std::cerr << hdr << "\n    Initial particle energy not set.\n";
    return false;
  }
  if (!m_workset) {
    std::cerr << hdr << "\n    Work function not set.\n";
    return false;
  }
  if (!m_fanoset) {
    std::cerr << hdr << "\n    Fano factor function not set.\n";
    return false;
  }
  if (!m_azset) {
    std::cerr << hdr << "\n    A and/or Z not set.\n";
    return false;
  }

  // Reset the cluster count
  m_currcluster = 0;
  clusters.clear();

  // Target maximum number of clusters
  const int mxclus = 200;

  // Initial situation: starting position
  double x = m_xt0;
  double y = m_yt0;
  double z = m_zt0;

  // Check and store initial energy
  if (fabs(m_initialenergy) < 1e-20 * m_mass ||
      fabs(m_initialenergy) < 1e-9 * m_work) {
    if (m_debug)
      printf(
          "TrackSrim::Generate\n Initial particle energy E = %g MeV such that "
          "beta2 = 0 or E << W; particle stopped.",
          m_initialenergy);
    return true;
  }
  double e = m_initialenergy;

  // Total distance covered
  double dsum = 0.0;

  // Pool of unused energy
  double epool = 0.0;

  // Loop generating clusters
  for (unsigned int iter = 0; iter < 100; ++iter) {
    // Work out what the energy loss per cm, straggling and projected range are
    // at the start of the step.
    const double dedxem = DedxEM(e) * m_density;
    const double dedxhd = DedxHD(e) * m_density;
    const double prange = Interpolate(e, m_energy, m_range);
    double strlon = Interpolate(e, m_energy, m_longstraggle);
    double strlat = Interpolate(e, m_energy, m_transstraggle);
    if (m_debug) printf("\te = %g long lat %g %g\n", e, strlon, strlat);

    if (!m_useLongStraggle) strlon = 0;
    if (!m_useTransStraggle) strlat = 0;

    if (m_debug)
      printf(
          "TrackSrim::Generate\n\tEnergy = %g MeV,\n\tdEdx em, hd = %g %g "
          "MeV/cm,\n\te-/cm = %g\n\tStraggling long/lat: %g %g cm\n",
          e, dedxem, dedxhd, 1e6 * dedxem / m_work, strlon, strlat);

    // Find the step size for which we get approximately the target # clusters.
    double step;
    if (m_nsize > 0) {
      step = (m_nsize * m_work) / (1e6 * dedxem);
    } else if (m_nsize < -1.5) {
      step = m_tracklength;
    } else {
      step = m_initialenergy / (0.5 * mxclus * (dedxem + dedxhd));
    }

    // Truncate if this step exceeds the length.
    bool finish = false;
    if (dsum + step > m_tracklength) {
      step = m_tracklength - dsum;
      finish = true;
      if (m_debug) std::cout << hdr << "Finish raised. Track length reached.\n";
    }

    // Make an accurate integration of the energy loss over the step.
    double deem, dehd;
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
      std::cerr << hdr << "\nFailure computing the minimum step size; "
                << "clustering abandoned.\n";
      return false;
    }

    double eloss;
    if (stpmin > stpmax) {
      // No way to find a suitable step size: use fixed energy loss
      if (m_debug) {
        std::cout << hdr << "stpmin > stpmax. Depositing all energy.\n";
      }
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
    if (m_debug)
      printf(
          "TrackSrim::Generate\n\tStep length = %g cm,\n\tmean loss =   %g "
          "MeV,\n\tactual loss = %g MeV.\n",
          step, deem, eloss);

    // Add a cluster.
    cluster newcluster;
    newcluster.x = x;
    newcluster.y = y;
    newcluster.z = z;
    if (m_fano < 0) {
      newcluster.electrons = int((eloss + epool) / (1.0e-6 * m_work));
      newcluster.ec = 1.0e-6 * m_work * newcluster.electrons;
    } else {
      double ecl = eloss + epool;
      newcluster.electrons = 0.0;
      newcluster.ec = 0.0;
      while (true) {
        const double ernd1 = RndmHeedWF(1.0e-6 * m_work, m_fano);
        if (ernd1 > ecl) break;
        (newcluster.electrons)++;
        (newcluster.ec) += ernd1;
        ecl -= ernd1;
      }
      if (m_debug)
        printf(
            "TrackSrim::Generate EM + pool: %g eV, W: %g eV, E/w: %g, n: %d\n",
            1.0e6 * (eloss + epool), m_work,
            (eloss + epool) / (1.0e-6 * m_work), newcluster.electrons);
    }
    newcluster.kinetic = e;
    epool = (eloss + epool) - newcluster.ec;
    if (m_debug) {
      const unsigned int nClusters = clusters.size();
      printf(
          "TrackSrim::Generate Cluster %u\n\tat (%g, %f, %f) cm,\n\te = %g "
          "MeV,\n\tn = %d,\n\tpool = %g MeV.\n",
          nClusters, newcluster.x, newcluster.y, newcluster.z, newcluster.ec,
          newcluster.electrons, epool);
    }
    clusters.push_back(newcluster);

    // Keep track of the length and energy
    dsum += step;
    e -= eloss + dehd;
    // Stop if the flag is raised
    if (finish) {
      if (m_debug) std::cout << hdr << "Finishing flag raised.\n";
      break;
      // Stop if the distance has been reached
    } else if (dsum > m_tracklength) {
      if (m_debug)
        printf("TrackSrim::Generate Reached track length d = %g cm.",
               m_tracklength);
      break;
      // Single cluster
    } else if (m_nsize < -1.5) {
      if (m_debug) std::cout << hdr << "Single cluster requested.\n";
      break;
      // No energy left
    } else if (e < m_initialenergy * 1E-9) {
      if (m_debug) std::cout << hdr << "Energy exhausted.\n";
      break;
    }
    // Draw scattering distances
    const double sigt1 = RndmGaussian(0., sqrt(step / prange) * strlat);
    const double sigt2 = RndmGaussian(0., sqrt(step / prange) * strlat);
    const double sigl = RndmGaussian(0., sqrt(step / prange) * strlon);
    if (m_debug) printf("\tsigma l, t1, t2: %g %g %g\n", sigt1, sigt2, sigl);

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
  }
  // Seems to have worked.
  return true;
}

bool TrackSrim::SmallestStep(double ekin, double de, double step,
                             double& stpmin) {
  // SRMMST - Determines the smallest step size for which there is little
  //          or no risk of finding negative energy fluctuations.

  const std::string hdr = m_className + "::SmallestStep: ";
  const double expmax = 30;

  // By default, assume the step is right.
  stpmin = step;
  // Check correctness.
  if (ekin <= 0 || de <= 0 || step <= 0) {
    printf(
        "TrackSrim::SmallestStep Input parameters not valid: Ekin = %g MeV, dE "
        "= %g MeV, step length = %g cm.\n",
        ekin, de, step);
    return false;
  } else if (m_mass <= 0 || fabs(m_q) <= 0) {
    printf(
        "TrackSrim::SmallestStep Track parameters not valid: mass = %g MeV, "
        "charge = %g.\n",
        m_mass, m_q);
    return false;
  } else if (m_a <= 0 || m_z <= 0 || m_density <= 0) {
    printf(
        "TrackSrim::SmallestStep Gas parameters not valid: A = %g, Z = %g, "
        "density = %g g/cm3.\n",
        m_a, m_z, m_density);
    return false;
  }

  // Basic kinematic parameters
  double beta2;
  if (ekin > 1e-5 * m_mass) {
    beta2 = 1 - 1 / ((1 + ekin / m_mass) * (1 + ekin / m_mass));
  } else {
    beta2 = 2 * ekin / m_mass;
  }
  const double gamma = 1 + ekin / m_mass;

  // Compute maximum energy transfer [MeV]
  const double rmass = 1.e6 * m_mass / ElectronMass;
  const double emax = 2 * ElectronMass * 1.e-6 * beta2 * gamma * gamma /
                      (1. + 2 * gamma * rmass + rmass * rmass);
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
    if (m_debug)
      printf(
          "TrackSrim::SmallestStep Settings\n\tdE = %g MeV,\n\tstep = %g "
          "cm\n\tEkin = %g MeV,\n\tbeta2 = %g,\n\tgamma = %g\n\tAgas = %g, "
          "Zgas = %g,\n\trho = %g g/cm3\n\tQpart = %g,\n\tmpart = %g "
          "MeV\n\tEmax = %g MeV,\n\txi = %g MeV\n\tkappa = %g.\n",
          denow, stpnow, ekin, beta2, gamma, m_a, m_z, m_density, m_q, m_mass,
          emax, xi, rkappa);
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
        std::cout << hdr << "Landau distribution is imposed.\n";
        printf("\tkappa_min = %g,\n\td_min = %g cm\n", rklim, stpmin);
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
        std::cout << hdr << "Vavilov distribution is imposed.\n";
        printf(
            "\tkappa_min = %g,\n\td_min = %g cm\n\tkappa_new = %g,\n\txi_new = "
            "%g MeV\n",
            rklim, stpmin, rknew, xinew);
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
        std::cout << hdr << "Landau distribution automatic.\n";
        printf("\tkappa_min = %g,\n\td_min = %g cm\n", rklim, stpmin);
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
        std::cout << hdr << "Vavilov distribution automatic.\n";
        printf(
            "\tkappa_min = %g,\n\td_min = %g cm\n\tkappa_new = "
            "%g,\n\txi_new = %g MeV\n",
            rklim, stpmin, rknew, xinew);
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
      if (m_debug) std::cout << "Step size ok, minimum: " << stpmin << " cm\n";
      break;
    }
    if (!retry) {
      std::cerr << hdr << "\nStep size must be increased to " << stpmin
                << " cm\n";
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
    std::cerr << hdr << "Input parameters not valid.\n";
    printf("    Ekin = %g, MeV, dE = %g MeV, step length = %g cm.\n", ekin, de,
           step);
    return 0.;
  } else if (m_mass <= 0 || fabs(m_q) <= 0) {
    std::cerr << hdr << "Track parameters not valid.\n";
    printf("    Mass = %g MeV, charge = %g\n", m_mass, m_q);
    return 0.;
  } else if (m_a <= 0 || m_z <= 0 || m_density <= 0) {
    std::cerr << hdr << "Material parameters not valid.\n";
    printf("    A = %g, Z = %g, density = %g g/cm3\n", m_a, m_z, m_density);
    return 0.;
  }
  // Basic kinematic parameters
  const double rkin = ekin / m_mass;
  const double gamma = 1. + rkin;
  const double beta2 = rkin > 1.e-5 ? 1. - 1. / (gamma * gamma) : 2. * rkin;

  // Compute maximum energy transfer
  const double rmass = 1.e6 * m_mass / ElectronMass;
  const double emax = 2 * ElectronMass * 1.e-6 * beta2 * gamma * gamma /
                      (1. + 2 * gamma * rmass + rmass * rmass);
  // Compute the Rutherford term
  const double fconst = 0.1534;
  const double xi = fconst * m_q * m_q * m_z * m_density * step / (m_a * beta2);
  // Compute the scaling parameter
  const double rkappa = xi / emax;
  // Debugging output.
  if (m_debug)
    printf(
        "TrackSrim::RndmEnergyLoss Settings\n\tdE = %g MeV,\n\tstep = %g "
        "cm,\n\tEkin = %g MeV,\n\tbeta2 = %g,\n\tgamma = %g,\n\tAgas = %g, "
        "Zgas = %g\n\tdensity = %g g/cm3,\n\tQpart = %g,\n\tmpart = %g "
        "MeV,\n\tEmax = %g MeV,\n\txi = %g MeV\n\tkappa = %g.\n",
        de, step, ekin, beta2, gamma, m_a, m_z, m_density, m_q, m_mass, emax,
        xi, rkappa);
  if (m_debug) std::cout << hdr << "\n";
  double rndde = de;
  if (m_model <= 0 || m_model > 4) {
    // No fluctuations.
    if (m_debug) std::cout << "    Fixed energy loss.\n";
  } else if (m_model == 1) {
    // Landau distribution
    if (m_debug) std::cout << "    Landau imposed.\n";
    const double xlmean = -(log(xi / emax) + beta2 + (1 - 0.577215));
    const double xlan = RndmLandau();
    rndde += xi * (xlan - xlmean);
  } else if (m_model == 2) {
    // Vavilov distribution, ensure we are in range.
    if (m_debug) std::cout << "    Vavilov imposed.\n";
    // if (m_precisevavilov && rkappa > 0.01 && rkappa <= 10) {
    // 	rndde += xi * (rndvvl(rkappa,beta2) + log(xi/emax)+beta2+(1-0.577215));
    // } else if ((!m_precisevavilov) && rkappa > 0.01 && rkappa <= 12) {
    if (rkappa > 0.01 && rkappa <= 12) {
      rndde += xi * (RndmVavilov(rkappa, beta2) + log(xi / emax) + beta2 +
                     (1 - 0.577215));
    }
  } else if (m_model == 3) {
    // Gaussian model
    if (m_debug) std::cout << "    Gaussian imposed.\n";
    rndde += RndmGaussian(0., sqrt(xi * emax * (1 - beta2 / 2)));
  } else if (rkappa < 0.05) {
    // Combined model: for low kappa, use the landau distribution.
    if (m_debug) std::cout << "    Landau automatic.\n";
    const double xlmean = -(log(xi / emax) + beta2 + (1 - 0.577215));
    const double par[] = {0.50884,    1.26116, 0.0346688,  1.46314,
                          0.15088e-2, 1.00324, -0.13049e-3};
    const double xlmax = (par[0] + par[1] * xlmean + par[2] * xlmean * xlmean +
                          par[6] * xlmean * xlmean * xlmean) +
                         (par[3] + xlmean * par[4]) * exp(par[5] * xlmean);
    int iter = 0;
    double xlan = RndmLandau();
    while (xlan > xlmax && iter < 100) {
      ++iter;
      xlan = RndmLandau();
    }
    rndde += xi * (xlan - xlmean);
  } else if (rkappa < 5) {
    // For medium kappa, use the Vavilov distribution, precise
    // } else if (m_precisevavilov && rkappa < 5) {
    //   printf("Vavilov slow automatic\n");
    //   rndde = de+xi*(rndvvl(rkappa,beta2) + log(xi/emax)+beta2+(1-0.577215));
    //   // ... or fast.
    if (m_debug) std::cout << "    Vavilov fast automatic.\n";
    rndde += xi * (RndmVavilov(rkappa, beta2) + log(xi / emax) + beta2 +
                   (1 - 0.577215));
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

  if (m_currcluster >= clusters.size()) return false;
  xcls = clusters[m_currcluster].x;
  ycls = clusters[m_currcluster].y;
  zcls = clusters[m_currcluster].z;
  tcls = 0;
  n = clusters[m_currcluster].electrons;
  e = clusters[m_currcluster].ec;
  extra = clusters[m_currcluster].kinetic;
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
