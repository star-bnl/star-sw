/*
  root.exe lGarfield.C eDep.C+
 */
#ifndef __CINT__
#include "Riostream.h"
#include "MediumMagboltz.hh"
#include "SolidBox.hh"
#include "GeometrySimple.hh"
#include "ComponentConstant.hh"
#include "Sensor.hh"
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
#endif
//________________________________________________________________________________
void eDep() {
using namespace Garfield;
using namespace Heed;
  // Histograms
  TH1::StatOverflows(kTRUE); 
 // Make a medium
  MediumMagboltz* gas = new MediumMagboltz();
  Double_t Fracs[2] = {0.9, 0.1};
  gas->SetComposition("ar", 100*Fracs[0], "ch4", 100*Fracs[1]);
  const Double_t BarPressure         = 1010.8; // [mbar], TPC-PTB, barometricPressure
  const Double_t inputTPCGasPressure = 1.93;   // [mbar], TPC-PT8, difference between barometer pressure and pressure in TPC
  //  const Double_t pressure = (1011. / 1000.) * 750.; // 1 bar = 750.06 torr 
  const Double_t pressure = ((BarPressure + inputTPCGasPressure)/ 1000.) * 750.06; // [torr], 1 bar = 750.06 torr 
  const Double_t temperature = 297.839; //273.15 + 24.7; // inputGasTemperature (degree K)
  Double_t BField = 0.5; // Tesla
  Double_t Angle  = 0.0; // rad

  gas->SetTemperature(temperature);
  gas->SetPressure(pressure);

  // Detector geometry
  // Gap [cm]
  const double width = 1.;
  SolidBox* box = new SolidBox(width / 2., 0., 0., width / 2., 10., 10.);
  GeometrySimple* geo = new GeometrySimple();
  geo->AddSolid(box, gas);

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
  Double_t m_e  = 0.51099906e-3;
  Double_t m_mu = 0.1056584;
  Double_t m_pi = 0.13956995;
  Double_t m_K  = 0.493677;
  Double_t m_p  = 938.27231e-3;
  Double_t m_d  = 1.875613;
  Double_t m_alpha  = 3.727417;
  Double_t m_t  = 2.80925;
  Double_t m_He3 = 2.80923;
  enum {Npart = 12};
  const Char_t *pnames[Npart] = {"e-","e+","mu-","mu+","pi-","pi+","K-","K+","p","pbar","d","alpha"};
  const Double_t mass[Npart] = {m_e,m_e,m_mu,m_mu,m_pi,m_pi,m_K,m_K,m_p,m_p,m_d,m_alpha};
  for (Int_t p = 0; p < Npart; p++) {
    TString dir("/");
    dir += pnames[p];
    gDirectory->cd("/");
    gDirectory->mkdir(pnames[p]);
    gDirectory->cd(dir);
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
      track.SetParticle(pnames[p]);
      //      const HeedParticle*   particle =   track.Particle();
      Double_t pmomentum = 1e9*mass[p]*BG;
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
#if 1
      const EnergyMesh *energyMesh = track.EnergyMesh();
      const int nValues = energyMesh->get_q();
#else
      const int nValues = nEnergyIntervals;
#endif
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
  }
  f->Write();
}
