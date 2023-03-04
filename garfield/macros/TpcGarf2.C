/*
  root.exe lGarfield.C TpcGarf.C+
 */
//#define  __ROOT_GEOM__
#ifndef __CINT__
#include "MediumMagboltz.hh"
#include "SolidBox.hh"
#include "SolidTube.hh"
#include "GeometrySimple.hh"
#include "ComponentConstant.hh"
#include "Sensor.hh"
#include "ComponentAnalyticField.hh"
#include "AvalancheMC.hh"
#include "AvalancheMicroscopic.hh"
#include "ViewSignal.hh"
#include "TBuffer3D.h"
#include "ViewGeometry.hh"
#include "ViewCell.hh"
#include "ViewField.hh"
#include "ViewDrift.hh"
#include "TrackHeed.hh"
#include "wcpplib/matter/GasLib.h"
#include "wcpplib/matter/MatterDef.h"
#include "wcpplib/clhep_units/WPhysicalConstants.h"

#include "heed++/code/ElElasticScat.h"
#include "heed++/code/EnTransfCS.h"
#include "heed++/code/HeedCluster.h"
#include "heed++/code/HeedCondElectron.h"
#include "heed++/code/HeedDeltaElectron.h"
#include "heed++/code/HeedDeltaElectronCS.h"
#include "heed++/code/HeedMatterDef.h"
#include "heed++/code/HeedParticle.h"
#include "heed++/code/HeedPhoton.h"
#include "heed++/code/PhotoAbsCSLib.h"

#include "Plotting.hh"
#include "TApplication.h"
#include "TH1.h"
#include "TH2.h"
#include "TCanvas.h"
#include "TMath.h"
#include "TFile.h"
#include "TSystem.h"
#ifdef __ROOT_GEOM__
#include "TGeoManager.h"
#include "TGeoMedium.h"
#include "TGeoMaterial.h"
#include "TGeoVolume.h"
#include "GeometryRoot.hh"
#endif /* __ROOT_GEOM__ */
#endif /* __CINT__ */
void TpcGarf() {
  using namespace Garfield;
#if 0
  // Histograms
  TH1::StatOverflows(kTRUE); 
  TCanvas *c1 = new TCanvas();
  c1->SetSupportGL(kFALSE);
#endif
  // -----------------------------
  // Make a medium, Gas Table
  // -----------------------------
  Double_t BField = 0.5; // Tesla
  Double_t Angle  = 0.0; // rad
  TString gasFile("P10.");
  Int_t b = 10*BField;
  gasFile += Form("B%ikG",b);
  gasFile += ".gas";
  MediumMagboltz* gas = new MediumMagboltz();
  if (gSystem->AccessPathName(gasFile)) {
    Double_t Fracs[2] = {0.9, 0.1};
    gas->SetComposition("ar", 100*Fracs[0], "ch4", 100*Fracs[1]);
    // Set temperature [K]
    gas->SetTemperature(293.15);
    // Set pressure [Torr]
    gas->SetPressure(760.);
    // Specify no. of electric fields to be included in the table and the electric field range to be covered.
    // Use 20 field points between 100 V/cm and 100 kV/cm with logarithminc spacing
    //                emin emax   ne  logE  bmin    bmax   nb  amin   amax, na
    //    gas->SetFieldGrid(100.,100e3, 20, true, BField, BField, 1, Angle, Angle, 1);
    gas->SetFieldGrid(100.,100e3, 20, true); // , BField, BField, 1, Angle, Angle, 1);
    // Use Magboltz to generate the gas table and grid.
    // Specfify te number of collission [1e7] over which the electron is traced in Magboltz
    const Int_t ncoll = 10; //[1e7]
    const Bool_t verbose = true;
    gas->GenerateGasTable(ncoll, verbose);
    // Save the gas table for later use
    gas->WriteGasFile(gasFile.Data());
  } else {
    gas->LoadGasFile(gasFile.Data());
  }
  gas->PrintGas();
  // -----------------------------
  //  Electric Field
  // -----------------------------
  // Use ComponentAnalyticField which can handle 2D arrangements of wires, planes and tubes
  ComponentAnalyticField *cmp = new ComponentAnalyticField();
  // The Component requires a description of the geometry, that is a list of volumes and associsated media.
  const Double_t rWire = 10e-4; // Wire radius [cm]
  const Double_t rTube =  1.46; // Outer radius of the tube [cm]
  const Double_t lTube =   10.; // Half-length of the tube [cm]
#ifndef __ROOT_GEOM__
  GeometrySimple *geo = new GeometrySimple();
  // Make tube         (centered at the origin, inner and outer radii, its half length
  SolidTube *tube = new SolidTube(0., 0., 0., rWire, rTube, lTube);
  // Add the solid to the geometry, together with the medium inside
  geo->AddSolid(tube,gas);
#else /* __ROOT_GEOM__ */
  TGeoManager *geoman = new TGeoManager("TPC","geometry");
  // Mixture: TpceGeo3a_P10
  Int_t nel     = 3;
  Double_t density = 0.001541;
  TGeoMixture *pMat76 = new TGeoMixture("TpceGeo3a_P10", nel,density);
  Double_t a, z, w;
  a = 39.948000;   z = 18.000000;   w = 0.957286;  // AR
  pMat76->DefineElement(0,a,z,w);
  a = 12.010700;   z = 6.000000;   w = 0.031980;  // C
  pMat76->DefineElement(1,a,z,w);
  a = 1.007940;   z = 1.000000;   w = 0.010735;  // H
  pMat76->DefineElement(2,a,z,w);
  pMat76->SetIndex(75);
// Medium: TpceGeo3a_P10::TpceGeo3a_Standard
   Int_t numed   = 50;  // medium number
   Double_t par[8];
   par[0]  = 0.000000; // isvol
   par[1]  = 1.000000; // ifield
   par[2]  = 20.000000; // fieldm
   par[3]  = 20.000000; // tmaxfd
   par[4]  = 10.000000; // stemax
   par[5]  = 0.000000; // deemax
   par[6]  = 0.010000; // epsil
   par[7]  = 0.000000; // stmin
   TGeoMedium *pMed50 = new TGeoMedium("TpceGeo3a_P10::TpceGeo3a_Standard", numed,pMat76, par);
   Double_t rmin = 0;
   Double_t rMax = 200;
   TGeoVolume *tpc = geoman->MakeTube("TPC",pMed50,rmin,rMax,lTube);
   geoman->SetTopVolume(tpc);
   GeometryRoot *geo = new GeometryRoot();
   geo->SetMedium("P10",gas);
   geoman->CloseGeometry();
#endif /* ! __ROOT_GEOM__ */
  // Pass a pointer to the geometry class to the component
  cmp->SetGeometry(geo);
#if 0  
  // Detector geometry
  // Gap [cm]
  const double width = 1.;
  SolidBox* box = new SolidBox(width / 2., 0., 0., width / 2., 10., 10.);
  GeometrySimple* geo = new GeometrySimple();
  geo->AddSolid(box, gas);
#endif
#if 0
  // -----------------------------
  // Visualizing the Geometry
  // -----------------------------
  ViewGeometry *view = new ViewGeometry();
  //  view->SetCanvas(c1);
  view->SetGeometry(geo);
  view->Plot();
  //  c1->Update();
#endif
  // -----------------------------
  // Analytic Fields
  // -----------------------------
  // STAR (x,y,z) => Garfield (-z,y,x); zGG = 
  // -----------------------------
  //  Setup electric field
  // -----------------------------
  const Double_t vWire = 3270.; // Voltage
  const Double_t vTube =    0.;
  cmp->AddWire(0., 0., 2*rWire, vWire, "s"); // Add the wire "s" in the center
  cmp->AddTube(rTube, vTube, 0, "t");        // Add the tube
  cmp->AddReadout("s");                      // Calculate the signal induce on the wire "s"
  // -----------------------------
  ViewCell *viewcell = new ViewCell();
  //  viewcell->SetCanvas(c1);
  viewcell->SetComponent(cmp);
  viewcell->Plot2d();
  //  c1->Update();
  // -----------------------------
  // Sensor
  // -----------------------------
  Sensor *sensor = new Sensor();             // interface class to the transport ones
  sensor->AddComponent(cmp);                 // Calculate the electric field using the Component object cmp
  sensor->AddElectrode(cmp, "s");            // Request signal calculation for the electrod named "s" and cmp
  // -----------------------------
  // Visualizing the Field
  // -----------------------------
  ViewField *fieldview = new ViewField();
  fieldview->SetComponent(cmp);
  fieldview->SetSensor(sensor);
  fieldview->SetArea(-rTube,-rTube,rTube,rTube);
  fieldview->PlotContour("e");// "v" for potential
#if 1
  // -----------------------------
  // Calculate only the electron signal
  // -----------------------------
  const Double_t tMin =  0.;     // [ns]
  const Double_t tMax =  2.;     // [ns]
  const Double_t tStep = 0.01;   // 0.02 [ns]
  const Int_t nTimeBins = (Int_t) ((tMax - tMin)/tStep);
  sensor->SetTimeWindow(0., tStep, nTimeBins);
  // -----------------------------
  // Avalanche
  // -----------------------------
  AvalancheMC *aval = new AvalancheMC();
  aval->SetSensor(sensor); 
  aval->EnableSignalCalculation();  // Switch on signal calculation
  aval->SetTimeSteps(0.050); // Do the drift line calculation in time steps of 50 ps.
  // Starting position [cm] and time [ns] of the initial electron.
  const Double_t x0 = 0; // 
  const Double_t y0 = rWire + 100e-4; // The electron is started at 100 micron above the wire
  const Double_t z0 = 0.;
  const Double_t t0 = 0.;
  ViewDrift *driftview = new ViewDrift();
  driftview->SetArea(-y0,-y0,-y0,y0,y0,y0);
  aval->EnablePlotting(driftview);
  aval->EnableDebugging();
  aval->AvalancheElectron(x0,y0,z0,t0); // Simulate an avalanche.
  driftview->Plot();
#if 1
  // -----------------------------
  // Plot the current induced on the wire by the above avalanche
  // -----------------------------
  ViewSignal *signalView = new ViewSignal();
#if 0
  //  signalView->SetCanvas(c1);
  //  c1->Update();
#endif
  signalView->SetSensor(sensor);
  signalView->PlotSignal("s");
#endif
#endif
#if 0
  // Make a component
  ComponentConstant* comp = new ComponentConstant();
  comp->SetGeometry(geo);
  comp->SetElectricField(133.4, 0., 0.);

  // Make a sensor
  Sensor* sensor = new Sensor();
  sensor->AddComponent(comp);
  Medium *medium;
  sensor->GetMedium(0,0,0,medium);
  TFile *f = new TFile("dNdx_Heed.root","RECREATE");
  Int_t nBG = 72;
  Int_t BGminL10 = -2;
  Int_t BGmaxL10 =  5.1;
  Double_t dx = (BGmaxL10 - BGminL10)/(nBG - 1);
  TH1D *dNdx = new TH1D("dNdxL10","dNdx versus log_{10} ( #beta#gamma ) (Heed)",
			nBG,BGminL10-0.5*dx,BGmaxL10+0.5*dx);
  Double_t eminL10 = 9.41511421261162829e-01;
  Double_t emaxL10 = 5.40502303800167372e+00;
  Double_t emin = TMath::Power(10.,eminL10);
  Double_t emax = TMath::Power(10.,emaxL10);
  Int_t nEnergyIntervals = 951;
  Double_t dy = (emaxL10 - eminL10)/(nEnergyIntervals-1);
  TH1D *dNdE = new TH1D("dNdEL10","dNdE versus log_{10} (dE (eV)) (Heed) P10",
			nEnergyIntervals, eminL10 - 0.5*dy, emaxL10 + 0.5*dy);
  TH1D *dNdEAr = new TH1D("dNdEL10Ar","dNdE versus log_{10} (dE (eV)) (Heed) Argon",
			nEnergyIntervals, eminL10 - 0.5*dy, emaxL10 + 0.5*dy);
  TH1D *dNdECH4 = new TH1D("dNdEL10CH4","dNdE versus log_{10} (dE (eV)) (Heed) CH_{4}",
			nEnergyIntervals, eminL10 - 0.5*dy, emaxL10 + 0.5*dy);
  for (Int_t ig = 1; ig <= nBG; ig++) {
    Double_t BGL10 = dNdx->GetBinCenter(ig);
    Double_t BG    = TMath::Power(10.,BGL10);
    // Track class
    TrackHeed track;
    track.SetSensor(sensor);
    track.SetParticle("pi");
    Double_t pmomentum = 1e9*0.13956995*BG;
    track.SetMomentum(pmomentum);
    //    track.EnablePhotoAbsorptionCrossSectionOutput();
    track.EnableDebugging();
    track.SetEnergyMesh(emin,emax,nEnergyIntervals);
    /*
      DynLinArr< double > ACS;   // Photoabsirbtion cross section per one atom(Mb).
      DynLinArr< double > ICS;   // Photoionization cross section per one atom(Mb).
      heed_pacs.tx contains
      E (eV) < ACS and ICS> for each component
    */
    double x0 = 0., y0 = 0., z0 = 0., t0 = 0.;
    double dx0 = 1., dy0 = 0., dz0 = 0.; 
    track.NewTrack(x0, y0, z0, t0, dx0, dy0, dz0);
    // Cluster coordinates
    double xc = 0., yc = 0., zc = 0., tc = 0.;
    // Energy loss in a collision
    double ec = 0.;
    // Dummy variable (not used at present)
    double extra = 0.;
    // Total energy loss along the track
    double esum = 0.;
    // Number of electrons produced in a collision
    Int_t Nc;
    // Total number of electrons produced along the track
    track.GetCluster(xc, yc, zc, tc, Nc, ec, extra);
    const int nComponents = gas->GetNumberOfComponents();
    const EnergyMesh *energyMesh = track.Energymesh();
    const int nValues = energyMesh->get_q();
    const EnTransfCS* transferCs = track.Transfercs();
    const double nc = transferCs->quanC;
    const double dedx = transferCs->meanC * 1.e3;
    const double dedxLeft = transferCs->meanCleft * 1.e3;
    const double dedx1 = transferCs->meanC1 * 1.e3;
    const HeedMatterDef* matter = track.Matter();
    const double w = matter->W * 1.e6;
    const double f = matter->F;
    const double minI = matter->min_ioniz_pot * 1.e6;
    cout << "get " << "::Setup:" << endl;
    //    cout << "denisty " << track->GetDensity() << " (gram/cm3)" << endl;
    cout << "    Cluster density:             " << nc << " cm-1" << endl;
    cout << "    Stopping power (restricted): " << dedxLeft << " - " 
	      << dedx << " keV/cm" << endl;
    cout << "    Stopping power (incl. tail): " << dedx1 
	      << " keV/cm" << endl;
    cout << "    W value:                     " << w << " eV" << endl;
    cout << "    Fano factor:                 " << f << "" << endl;
    cout << "    Min. ionization potential:   " << minI << " eV" << endl;
    dNdx->SetBinContent(ig,nc);
    cout << "bin " << ig << " Content " << dNdx->GetBinContent(ig) << endl;
    if (ig == 1) {
      const MolecPhotoAbsCS** molPacs = track.Molpacs();
      if (nValues > 0) {
	for (int i = 0; i < nValues; ++i) {
	  Double_t E = dNdE->GetXaxis()->GetBinCenter(i+1);
	  Double_t e = energyMesh->get_ec(i);
	  Double_t T = 0;
	  cout << "E from histogram " << TMath::Power(10.,E) << "\te from Mesh " << 1.e6 * e << "  " ;
	  for (int j = 0; j < nComponents; ++j) {
	    cout << molPacs[j]->get_ACS(e) << "  "
		 << molPacs[j]->get_ICS(e) << "  ";
	    T += Fracs[j]*molPacs[j]->get_ICS(e);
	  }
	  cout << " T = " << T << endl;
	  dNdE->SetBinContent(i,T);
	  dNdEAr->SetBinContent(i,molPacs[0]->get_ICS(e));
	  dNdECH4->SetBinContent(i,molPacs[1]->get_ICS(e));

	}
      }
    }
  }
  f->Write();
#endif
}
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
