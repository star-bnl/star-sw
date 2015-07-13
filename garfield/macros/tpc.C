/* Heinrich Schindler 04/22/13
  root.exe lGarfield.C tpc.C+
 */
#define __USE_AvalancheMicroscopic
#include <iostream>
#include <fstream>

#include "TCanvas.h"
#include "TROOT.h"
#include "TApplication.h"
#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TSystem.h"
#include "TMath.h"
#include "ViewField.hh"
#include "ViewDrift.hh"
#include "ViewCell.hh"
#include "MediumMagboltz.hh"
#include "SolidBox.hh"
#include "GeometrySimple.hh"
#include "ComponentAnalyticField.hh"
#include "Sensor.hh"
#include "DriftLineRKF.hh"
#include "AvalancheMicroscopic.hh"
#include "AvalancheMC.hh"

#include "FundamentalConstants.hh"
#include "Random.hh"
#include "Plotting.hh"
//#define __DriftVelocity__
using namespace Garfield;

void tpc(TString IO = "Inner1170", Int_t nEvents = 10) {
  // STAR coordinate system (xS,yS,zS) => Garfield (yS,zG,xG,yG); Garfield(xG,yG,zG) = > Star(y,z,x)

  //  plottingEngine.SetDefaultStyle();
  const Bool_t Inner = IO.Contains("Inner");
  // Setup the gas.
  const Double_t BarPressure         = 1010.8; // [mbar], TPC-PTB, barometricPressure
  const Double_t inputTPCGasPressure = 1.93;   // [mbar], TPC-PT8, difference between barometer pressure and pressure in TPC
  //  const Double_t pressure = (1011. / 1000.) * 750.; // 1 bar = 750 torr 
  const Double_t pressure = ((BarPressure + inputTPCGasPressure)/ 1000.) * 750.; // [torr], 1 bar = 750 torr 
  const Double_t temperature = 297.839; //273.15 + 24.7; // inputGasTemperature (degree K)
  Double_t BField = 0.5; // Tesla
  Double_t Angle  = 0.0; // rad
  TString gasFile("P10.");
  Int_t b = 10*BField;
  Int_t t = temperature;
  Int_t p = pressure;
  gasFile += Form("B%ikGT%iP%i",b,t,p);
  TString fOutName(IO);
#ifdef __USE_AvalancheMicroscopic
  fOutName += "Microscopic";
#endif
  //Switch between Inner and Outer sector
  // Voltage settings
  const Double_t vAnodeI  =   1170.;
  const Double_t vAnodeO  =   1390.;
  Double_t vAnode = 1190;
  if (IO == "Inner1170") vAnode == 1170;
  if (IO == "Inner1135") vAnode == 1135;
  if (IO == "Inner1100") vAnode == 1100;
  if (IO == "Outer1345") vAnode == 1345;
  if (IO == "Outer1390") vAnode == 1390;
    //  const Double_t vAnode   = (Inner) ? vAnodeI : vAnodeO;
  fOutName += "_"; fOutName += (Int_t) vAnode; fOutName += "V_";
  fOutName += gasFile; fOutName += ".root";
#ifdef  __DriftVelocity__
  TFile *fOut = new TFile("DriftVelocity1V.root","recreate");
#else
  TFile *fOut = new TFile(fOutName,"recreate");
#endif
  // Create a gas medium.  
  MediumMagboltz* gas = new MediumMagboltz();
#if 0
  const Double_t density = LoschmidtNumber * 
    (pressure / AtmosphericPressure) *
    (273.15 / temperature);
  const Double_t pt = (temperature / 273.15) * (760. / pressure);
#endif
  // Set the temperature [K] and pressure [Torr].
  gas->SetTemperature(temperature);
  gas->SetPressure(pressure);
  gas->SetMaxElectronEnergy(300.);
  // Specify the gas mixture.
  Double_t Fracs[2] = {0.9, 0.1};
  gas->SetComposition("ar", 100*Fracs[0], "ch4", 100*Fracs[1]);
#if 1
//   gas->SetFieldGrid(100.,100e3, 20, true, BField, BField, 1, Angle, Angle, 1);
//   gasFile += ".gas";
  gas->SetFieldGrid(100.,100e3, 20, true, BField, BField, 1, Angle, Angle, 1);
  gasFile += ".gas";
#else
  gas->SetFieldGrid(100.,300., 21, false, BField, BField, 1, Angle, Angle, 1);
  gasFile += "1kVNew.gas";
#endif
  const Double_t rPenning = 0.57;
  const Double_t lambdaPenning = 0.e-4;
  gas->EnablePenningTransfer(rPenning, lambdaPenning, "ch4");
  if (gSystem->AccessPathName(gasFile)) {
    // Use Magboltz to generate the gas table and grid.
    // Specfify te number of collission [1e7] over which the electron is traced in Magboltz
    const Int_t ncoll = 10; //[1e7]
    const Bool_t verbose = true;
    gas->GenerateGasTable(ncoll, verbose);
    // Save the gas table for later use
    gas->WriteGasFile(gasFile.Data());
    return;
  } else {
    gas->LoadGasFile(gasFile.Data());
  }
  gas->PrintGas();
  const Char_t *path = gSystem->ExpandPathName("$GARFIELD_HOME/Data/IonMobility_Ar+_Ar.txt");
  gas->LoadIonMobility(path);
  delete path;
  gas->Initialise();
  const Double_t yGG      =      0.;
  const Double_t vGG      =   -115.; // Gating Grid
  const Double_t vCathode =  -27950;
  const Double_t yCathode =-208.707; // move into system where yGG = 0
  const Double_t eDrift   = (vCathode - vGG)/(yCathode - yGG); 
  const Double_t yC       = -10;     // pseudo cathone
  const Double_t vC       = vGG + eDrift*(yC - yGG);
  const Double_t gapI     = 0.2; // Anode pad plane gap Inner
  const Double_t gapO     = 0.4; // Anode pad plane gap Outer
  const Double_t gap      = (Inner) ? gapI : gapO;
  const Double_t aWpitch  = 0.4; // Anode wire spacing;
  const Double_t cWpitch  = 0.1; // Cathode wire spacing
  const Double_t ggWpitch = 0.1; // 
  // y coordinates of the wires
  const Double_t yGate = 0;
  const Double_t yCath = 0.6;
  const Double_t ySens = yCath + gap;
  const Double_t yPad  = ySens + gap;
  const Double_t vPad  = 0;

  // Periodicities
  const Int_t nRep = 10;
  const Double_t perSens = aWpitch;
  const Double_t perCath = cWpitch;
  const Double_t perGate = ggWpitch;
  
  // Wire diameters
  const Double_t dSens = 20.e-4;
  const Double_t dCath = 75.e-4;
  const Double_t dGate = 75.e-4;
  
  const Int_t nBinsAngular = 360;
  const Int_t nBinsRadial = 100;
  const Int_t nBinsGain = 100;
  TH1::StatOverflows();
#ifdef  __DriftVelocity__
  // Drift velocities and diffusion
  Int_t nEF = 1;
  Int_t EFmin = 100;
  Int_t EFmax = 200;
  TH1F *vElectron = new TH1F("vElectron","Electron drift velocity [cm/musec]",nEF,EFmin,EFmax);
  TH1F *dTElectron = new TH1F("dTElectron","Electron transferse diffusion [mkm/sqrt(cm)] ",nEF,EFmin,EFmax);
  TH1F *dLElectron = new TH1F("dLElectron","Electron longitudinal diffusion [mkm/sqrt(cm)] ",nEF,EFmin,EFmax);
  
  Double_t vx, vy, vz;
  Double_t dl, dt;
  for (Int_t i = 1; i <= nEF; i++) {
    Double_t ez = vElectron->GetBinCenter(i);
    gas->ElectronVelocity(0,0,-ez,0,0,0.5,vx,vy,vz);
    vElectron->SetBinContent(i,1e3*vz);
    gas->ElectronDiffusion(0,0,-ez,0,0,0.5,dl,dt);
    dTElectron->SetBinContent(i,1e4*dt);
    dLElectron->SetBinContent(i,1e4*dl);
  }
  fOut->Write();
  return;
#endif
  
  TH1F* hAngle = new TH1F("hAngle", "Angular distribution",
                          nBinsAngular, -180, 180);
  TH2F* hRadial = new TH2F("hRadial", "Ion starting points",
                           nBinsRadial, dSens / 2., 3. * dSens,
                           nBinsAngular, -180, 180); 
  TH1F* hElectrons = new TH1F("hElectrons", "Log_{10}(Number of electrons)",
                              nBinsGain, 0, 5.);
  TH1F* hIons = new TH1F("hIons", "Log_{10}(Number of ions)",
                         nBinsGain, 0, 5.);
  TH1F* tElectrons = new TH1F("tElectrons","time distribution of electors (ns)",100,0,1000);

  // Build the geometry, in this case just a box.
  GeometrySimple* geo = new GeometrySimple(); // geo->EnableDebugging();
  SolidBox* box = new SolidBox(0., 0., 0., 
                               nRep * perSens, -yC, 100.);
  // Add the solid to the geometry, together with the medium inside.
  geo->AddSolid(box, gas);

  // Setup the electric field.
  ComponentAnalyticField* comp = new ComponentAnalyticField(); 
  comp->SetMagneticField(0., BField, 0.);
  comp->SetPeriodicityX(nRep * perSens);
  // Add the anode wires.
  for (Int_t i = 0; i < nRep; ++i) {
    comp->AddWire((i - 2.) * perSens, ySens, dSens, vAnode, "s");
  }
  Int_t ng = TMath::Nint(aWpitch/cWpitch);
  // Add the cathode wires.
  for (Int_t i = 0; i < nRep*ng; ++i) {
    comp->AddWire(perCath * (i + 0.5), yCath, dCath, 0., "c");
  }
  // Add the gate wires.
  ng = TMath::Nint(aWpitch/ggWpitch);
  for (Int_t i = 0; i < nRep * ng; ++i) {
    comp->AddWire(perGate * (i + 0.5), yGate, dGate, vGG, "g");
  }
  comp->AddPlaneY(yPad, vPad, "p");
  comp->AddPlaneY(yC, vC, "q");
  
  comp->AddReadout("s");
  comp->AddReadout("p");
 
  comp->SetGeometry(geo);
  TCanvas *c_e = new TCanvas("Cell","Cell");
  ViewCell* cellView = new ViewCell();
  cellView->SetComponent(comp);
  cellView->SetArea(-nRep * perSens / 2., -yPad, -2.,           
		    nRep * perSens / 2.,  yPad,   2.); 
  cellView->SetCanvas(c_e);
  cellView->Plot2d();
  TCanvas *c_i = new TCanvas("Cell_i","Cell for ions");
  ViewCell* cellView_i = new ViewCell();
  cellView_i->SetCanvas(c_i);
  cellView_i->SetComponent(comp);
  cellView_i->SetArea(-nRep * perSens / 2., -yPad, -2.,           
		    nRep * perSens / 2.,  yPad,   2.); 
  cellView_i->Plot2d();
  // Make a sensor
  Sensor* sensor = new Sensor();
  //sensor->EnableDebugging();
  sensor->AddComponent(comp);
  sensor->AddElectrode(comp, "p");
  sensor->SetTimeWindow(0., 50., 1000);
  sensor->ClearSignal();

  ViewDrift* v_e = new ViewDrift();
  v_e->SetCanvas(c_e);
  v_e->SetArea(-nRep * perSens / 2., -yPad, -10.0,
	       nRep * perSens / 2.,  yPad, 10.0); 
  
  ViewDrift* v_i = new ViewDrift();
  v_i->SetCanvas(c_i);
  v_i->SetArea(-nRep * perSens / 2., -yPad, -10.0,
	       nRep * perSens / 2.,  yPad, 10.0); 
  
 // Plot the potential
  const bool plotField = false; // true;  
  if (plotField) {
    ViewField* fView = new ViewField;
    fView->SetSensor(sensor);
    fView->SetArea(-nRep * perSens / 2., -yPad,           
                    nRep * perSens / 2.,  yPad); 
    fView->SetVoltageRange(-200., 400.);
    //    fView->PlotSurface("e");
    fView->PlotContour();
  }
  //  return;
#ifdef __USE_AvalancheMicroscopic
  AvalancheMicroscopic* aval = new AvalancheMicroscopic(); 
#else
  AvalancheMC* aval = new AvalancheMC();
  //  aval->SetDistanceSteps(0.0005);
  aval->SetTimeSteps(0.05); // Do the drift line calculation in time steps of 50 ps
#endif
  //  aval->EnableDebugging();
  aval->EnableMagneticField();
  aval->SetSensor(sensor);
  aval->EnablePlotting(v_e);
 //aval->EnableNullCollisionSteps();
  DriftLineRKF* driftline_i = new DriftLineRKF();
  driftline_i->SetSensor(sensor);
  driftline_i->EnablePlotting(v_i);

  
  const Double_t xmin = -aWpitch/2;
  const Double_t xmax =  aWpitch/2;
  
  Double_t x0, y0, z0, t0, e0;
  Double_t x1, y1, z1, t1, e1;
  Int_t status;
  Double_t r, phi;
  Int_t ne, ni;
  Int_t nEndpoints;
  for (Int_t i = nEvents; i--;) {
    gas->ResetCollisionCounters();
    sensor->NewSignal();
    x0 = xmin + RndmUniform() * (xmax - xmin);
    y0 = yGate - 0.1;
    z0 = t0 = 0.;
    e0 = 0.1; // eV
    t0 = 0;
#ifdef __USE_AvalancheMicroscopic
    aval->AvalancheElectron(x0, y0, z0, t0, e0, 0., 0., 0.);
#else
    aval->AvalancheElectron(x0, y0, z0, t0);
#endif
    v_e->Plot(true,false);
    c_e->Update();
    aval->GetAvalancheSize(ne, ni);
    if (ne > 0) hElectrons->Fill(ne);    
    if (ni > 0) hIons->Fill(ni);
    nEndpoints = aval->GetNumberOfElectronEndpoints();
    if (i % 10 == 0) std::cout << i << "/" << nEvents << ": "
			      << ne << " electrons, "
			      << ni << " ions" << std::endl; 
    if (ne <= 0) continue;
    for (Int_t j = nEndpoints; j-- > 1;) {
      /*
	GetElectronsEndpoint
	GetElectronEndpoint(const int i,double& x0,double& y0,double& z0,double& t0,double& x1,double& y1,double& z1,double& t1,int& status) const
       */
#ifdef __USE_AvalancheMicroscopic
      aval->GetElectronEndpoint(j, x0, y0, z0, t0, e0,
                                   x1, y1, z1, t1, e1, status);
#else
      aval->GetElectronEndpoint(j, x0, y0, z0, t0,
				 x1, y1, z1, t1, status);
#endif
      
      tElectrons->Fill(t1);
      phi = atan((y1 - ySens) / x1) * 180. / Pi;
      if (x1 > 0.) {
        phi = -phi + 90.;
      } else {
        phi = -phi - 90.;
      }
      if (x1 > xmin && x1 < xmax) {
        hAngle->Fill(phi);
      }
      r = sqrt(pow(y0 - ySens, 2) + x0 * x0);
      phi = atan((y0 - ySens) / x0) * 180. / Pi;
      if (x0 > 0.) {
        phi = -phi + 90;
      } else {
        phi = -phi - 90.;
      }
      if (x0 > xmin && x0 < xmax) hRadial->Fill(r, phi);
      driftline_i->DriftIon(x0, y0, z0, t0);
      const std::vector<DriftLineRKF::step> &path_i = driftline_i->path();
    }    
    v_i->Plot(true,false);
    c_i->Update();
    // std::cout << "Next avalanche..." << std::endl;
  }
  fOut->Write();
}
#if 0
//________________________________________________________________________________
void DriftDraw() {
  
  struct DriftP_t {
    Double_t EField; // V/cm
    Double_t vZ;     // drift velosity [mkm/nsec]
    Double_t dvZ;    // estimated error (%)
    Double_t diffT;  // transverse diffusion mkm/sqrt(cm)
    Double_t ddiffT; // estimated error (%)
    Double_t diffL;  // longitudinal diffusion mkm/sqrt(cm)
    Double_t ddiffL; // estimated error (%)
  };
  Drift_t P[] = {
    { 100.0000, 0.5387E+02, 0.07, 191.643, 5.66, 414.491, 3.96},
    { 143.8450, 0.5527E+02, 0.05, 229.444, 4.46, 339.353, 4.19},
    { 206.9138, 0.5161E+02, 0.07, 284.862, 4.06, 302.789, 4.74},
    { 297.6351, 0.4501E+02, 0.07, 348.284, 5.69, 265.167, 3.73},
    { 428.1332, 0.3759E+02, 0.09, 420.312, 7.24, 233.833, 6.38}
  };
}
#endif
/* Roy Bossingham June 29, 2000
Global argon=90
Global ch4=10
Global bz=0.5
Global T= 297.04
Global P=760
&Gas
  Temperature {T}
  Pressure {P}
Global gas_file=`p10_Ar`/string(argon)/`CH4`/string(ch4)/`T`/string(T)/`P`/string(P)/`.dat`
say "gas_file {gas_file}"
Call inquire_file(gas_file, exist)
If exist Then
   get {gas_file}
Else
   magboltz argon {argon} ch4 {ch4}
   heed argon {argon} ch4 {ch4}
   add mobility 1.5e-6
   write {gas_file}
Endif

* PLANE direction coordinate ... ! X, Y, R, PHI
*      [V potential] ...         ! Volt
*      [LABEL label] ...         ! The label is a single alphabetic upper case character. A label by which the plane can be referenced in SELECT statements,
*      [{X-STRIP | Y-STRIP       | R-STRIP | PHI-STRIP | Z-STRIP} strip_min strip_max ... ! Anticipates read-out of a part of the plane.
*      [GAP gap] ...             ! Indicates the gap width to be used when computing the weighting field for the strip
*      [LABEL strip_label]]      ! A label by which the strip can be referenced in SELECT statements
*
* ROWS  [CARTESIAN | POLAR | TUBE]
* label n diameter x y [V [weight [length [density]]]]
&CELL
 CELL-IDENTIFIER TPC-INNER
 PLANE  Y 0.4  V -1.5
 PLANE  Y 3.4  V -397.0422
 ROWS
 S  31 0.0020 -6.0+I*0.4  0.6000 1150.0
 F 125 0.0075 -6.2+I*0.1  0.8000    0.0
 F 125 0.0075 -6.2+I*0.1  1.4000 -102.5

&MAGNETIC
  COMP 0.0 {bz} 0.0 T

*&GAS 
*  GET "p10_Ar90CH410T297.04P760.dat"
&FIELD
 AREA -1.0 0.0 1.0 2.0
 PLOT CONTOUR V RANGE -175. 225. N 16

&DRIFT
 LINES 104
 AREA  -0.200 0.35 0.200 1.65
 TRACK -0.195 1.60 0.195 1.60
 DRIFT TRACK FUNCTION-GRAPH TIME CONTOUR 0.010

get {gas_file}

&CELL
 CELL-IDENTIFIER TPC-OUTER
 PLANE  Y 0.4  V -1.5
 PLANE  Y 3.4  V -397.0422
 ROWS
 S  31 0.0020 -6.0+I*0.4  0.4000 1400.0
 F 125 0.0075 -6.2+I*0.1  0.8000    0.0
 F 125 0.0075 -6.2+I*0.1  1.4000 -102.5

&FIELD
 AREA -1.0 0.0 1.0 2.0
 PLOT CONTOUR V RANGE -175. 575. N 30

&DRIFT
 LINES 104
 AREA  -0.200 0.35 0.200 1.65
 TRACK -0.195 1.60 0.195 1.60
 DRIFT TRACK FUNCTION-GRAPH TIME CONTOUR 0.010
*&STOP
  
 */
