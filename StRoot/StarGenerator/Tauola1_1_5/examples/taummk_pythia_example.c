/**
 * Example of use of tauola C++ interface. Pythia events are
 * generated with and without tau decays. Events with taus decayed
 * by pythia are compared against events with taus decayed via tauola.
 *
 * Fraction of energies carried by principal charged decay products to
 * tau energy is printed; respectively for pythia and tauola tau decays.
 *
 * @author Nadia Davidson  Mikhail Kirsanov Tomasz Przedzinski
 * @date 29 July 2010
 */

#include "Tauola/Log.h"
#include "Tauola/Plots.h"
#include "Tauola/Tauola.h"
#include "Tauola/TauolaHepMCEvent.h"

//pythia header files
#ifdef PYTHIA8180_OR_LATER
#include "Pythia8/Pythia.h"
#include "Pythia8/Pythia8ToHepMC.h"
#else
#include "Pythia.h"
#include "HepMCInterface.h"
#endif

#include "HepMC/IO_AsciiParticles.h"

#include "tauola_print_parameters.h"
using namespace std;
using namespace Pythia8;
using namespace Tauolapp;

bool ShowersOn=true;
int NumberOfEvents = 1000;

double hratio(HepMC::GenEvent* HepMCEvt, double* hratio2)
{
	double rat=0., rat2=0.;
	int nhadrmode=0;
	for(HepMC::GenEvent::particle_iterator p = HepMCEvt->particles_begin();
	    p!=HepMCEvt->particles_end();
	    ++p)
	{
		if(abs((*p)->pdg_id()) == 15 && (*p)->end_vertex() )
		{
			double ehadr=0.;
			int ihadrmode=0;
			for(HepMC::GenVertex::particle_iterator des = (*p)->end_vertex()->particles_begin(HepMC::children);
			    des!=(*p)->end_vertex()->particles_end(HepMC::children);
			    ++des)
			{
				if(abs((*des)->pdg_id()) == 20213 ||
				abs((*des)->pdg_id()) == 213 ||
				abs((*des)->pdg_id()) == 211 ||
				abs((*des)->pdg_id()) == 321 )
				{
					ehadr += (*des)->momentum().e();
					ihadrmode = 1;
				}
			}
			rat  += ehadr       /   (*p)->momentum().e();
			rat2 += ehadr*ehadr / ( (*p)->momentum().e()*(*p)->momentum().e() );
			nhadrmode += ihadrmode;
		}
	}
	if(nhadrmode)
	{
		rat  = rat  / (double)nhadrmode;
		rat2 = rat2 / (double)nhadrmode;
	}
	*hratio2 = rat2;
	return rat;
}

int main(int argc,char **argv)
{
	Log::SummaryAtExit();
	// Initialization of pythia
	Pythia pythia;
  
  // Pythia8 HepMC interface depends on Pythia8 version
#ifdef PYTHIA8180_OR_LATER
	HepMC::Pythia8ToHepMC ToHepMC;
#else
	HepMC::I_Pythia8 ToHepMC;
#endif

	//HepMC::IO_AsciiParticles ascii_io("example_PythiaParticle.dat",std::ios::out);
	HepMC::IO_AsciiParticles ascii_io1("cout",std::ios::out);

	if(!ShowersOn)
	{
		//pythia.readString("HadronLevel:all = off");
		pythia.readString("HadronLevel:Hadronize = off");
		pythia.readString("SpaceShower:QEDshower = off");
		pythia.readString("SpaceShower:QEDshowerByL = off");
		pythia.readString("SpaceShower:QEDshowerByQ = off");
		pythia.readString("PartonLevel:ISR = off");
		pythia.readString("PartonLevel:FSR = off");
	}

	//pythia.readString("WeakSingleBoson:ffbar2gmZ = on");

	pythia.readString("WeakDoubleBoson:ffbar2ZW = on");

	//pythia.readString("HiggsSM:gg2H = on");
	//pythia.readString("25:m0 = 200.");
	//pythia.readString("25:onMode = off");
	//pythia.readString("25:onIfAny = 23");

	pythia.readString("23:onMode = off");
	pythia.readString("23:onIfAny = 15");
	pythia.readString("24:onMode = off");
	pythia.readString("24:onIfAny = 15");
	//pythia.readString("23:onIfMatch = 15 -15");
	pythia.init( 2212, 2212, 14000.0); //proton proton collisions

	// Set up TAUOLA
	// Tauola::setSameParticleDecayMode(19);     //19 and 22 contains K0
	// Tauola::setOppositeParticleDecayMode(19); // 20 contains eta

	Tauola::initialize();

	tauola_print_parameters(); // Prints TAUOLA  parameters (residing inside its library): e.g. to test user interface

	// our default is GEV and MM, that will be outcome  units after TAUOLA
	// if HepMC unit variables  are correctly set.
	// with the following coice you can fix the units for final outcome:
	// Tauola::setUnits(Tauola::GEV,Tauola::MM);
	// Tauola::setUnits(Tauola::MEV,Tauola::CM);

	Tauola::setEtaK0sPi(0,0,0); // switches to decay eta K0_S and pi0 1/0 on/off.

	// Tauola::setTauLifetime(0.0); //new tau lifetime in mm
	// Tauola::spin_correlation.setAll(false);
	//  Log::LogDebug(true);

// --- Event loop with pythia only --------------------------------------------
	Log::Info()<<"FIRST LOOP: Pythia only" << endl;

	double rats=0., rats2=0.;

	for(int iEvent = 0; iEvent < NumberOfEvents; ++iEvent)
	{
		if(iEvent%100==0) Log::Info()<<"Event: "<<iEvent<<endl;
		if(!pythia.next()) continue;

		// Convert event record to HepMC
		HepMC::GenEvent * HepMCEvt = new HepMC::GenEvent();

		//Conversion needed if HepMC use different momentum units
		//than Pythia. However, requires HepMC 2.04 or higher.
		//    HepMCEvt->use_units(HepMC::Units::GEV,HepMC::Units::MM);

		ToHepMC.fill_next_event(pythia, HepMCEvt);

		double rat2;
		rats += hratio(HepMCEvt, &rat2);
		rats2 += rat2;

		//clean up HepMC event
		delete HepMCEvt;
	}

	rats  = rats  / (double)NumberOfEvents;
	rats2 = rats2 / (double)NumberOfEvents;

	double rpyt  = rats;
	double erpyt = sqrt( (rats2 - rats*rats)/ (double)NumberOfEvents );

// --- Event loop with pythia and tauola --------------------------------------
	Log::Info()<<"SECOND LOOP: Pythia + Tauola" << endl;

	pythia.particleData.readString("15:mayDecay = off");

	rats=0.; rats2=0.;

	for (int iEvent = 0; iEvent < NumberOfEvents; ++iEvent)
	{
		if(iEvent%100==0) Log::Info()<<"Event: "<<iEvent<<endl;
		if (!pythia.next()) continue;

		// Convert event record to HepMC
		HepMC::GenEvent * HepMCEvt = new HepMC::GenEvent();

		//Conversion needed if HepMC use different momentum units
		//than Pythia. However, requires HepMC 2.04 or higher.
		    HepMCEvt->use_units(HepMC::Units::GEV,HepMC::Units::MM);

		ToHepMC.fill_next_event(pythia, HepMCEvt);

		//ascii_io << HepMCEvt;
		if(iEvent<2)
		{
			cout << endl << "Event record before tauola:" << endl << endl;
			ascii_io1 << HepMCEvt;
		}

		//run TAUOLA on the event
		TauolaHepMCEvent * t_event = new TauolaHepMCEvent(HepMCEvt);
		//Since we let Pythia decay taus, we have to undecay them first.
		//t_event->undecayTaus();
		//ascii_io << HepMCEvt;

		t_event->decayTaus();

		//ascii_io << HepMCEvt;
		if(iEvent < 2)
		{
			cout << endl << "Event record after tauola:" << endl << endl;
			ascii_io1 << HepMCEvt;
		}

		delete t_event;
		Log::Debug(5) << "helicites =  " << Tauola::getHelPlus() << " "
		              << Tauola::getHelMinus()
		              << " electroweak wt= " << Tauola::getEWwt() << endl;

		double rat2;
		rats  += hratio(HepMCEvt, &rat2);
		rats2 += rat2;

		//clean up HepMC event
		delete HepMCEvt;
	}

	rats  = rats  / (double)NumberOfEvents;
	rats2 = rats2 / (double)NumberOfEvents;
	double ertau = sqrt( (rats2 - rats*rats)/ (double)NumberOfEvents );

	cout.precision(6);
	cout << "******************************************************" << endl;
	cout << "* f + fbar  -> Z0 + W+/-                             *" << endl;
	cout << "* with Z0 -> tau+ tau- and W+/- -> tau+/- nutau      *" << endl;
	cout << "* E(PI+- + K+- + A1+-) / E(TAU) ratio                *" << endl;
	cout << "*   pythia = " << rpyt <<  "   tauola = " << rats
	     << "        *" << endl;
	cout << "* erpythia = " << erpyt << " ertauola = " << ertau
	     << "        *" << endl;
	cout << "******************************************************" << endl;

	Log::RedirectOutput(Log::Info());
	pythia.statistics();
	Log::RevertOutput();

  // This is an access to old FORTRAN info on generated tau sample. 
  // That is why it refers to old version number (eg. 2.7) for TAUOLA.
  //Tauola::summary();
}
