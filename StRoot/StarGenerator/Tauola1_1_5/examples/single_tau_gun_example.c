#include <fstream>

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

#include "tauola_print_parameters.h"
using namespace std;
using namespace Pythia8; 
using namespace Tauolapp;

int N = 10000;
int EventsToCheck=20;

// elementary test of HepMC typically executed before
// detector simulation based on http://home.fnal.gov/~mrenna/HCPSS/HCPSShepmc.html
// similar test was performed in Fortran
// we perform it before and after Tauola (for the first several events)
void checkMomentumConservationInEvent(HepMC::GenEvent *evt)
{
	//cout<<"List of stable particles: "<<endl;

	double px=0.0,py=0.0,pz=0.0,e=0.0;
	
	for ( HepMC::GenEvent::particle_const_iterator p = evt->particles_begin();
	      p != evt->particles_end(); ++p )
	{
		if( (*p)->status() == 1 )
		{
			HepMC::FourVector m = (*p)->momentum();
			px+=m.px();
			py+=m.py();
			pz+=m.pz();
			e +=m.e();
			//(*p)->print();
		}
	}
	cout.precision(6);
	cout.setf(ios_base::floatfield);
	cout<<endl<<"Vector Sum: "<<px<<" "<<py<<" "<<pz<<" "<<e<<endl;
}

/*
   Simple boost routine example.
   Note that this routine will not respect direction of polarization vector along boost direction,
   thus (0,0,1) does not mean helicity state. This is simply z component of spin for tau
   with momentum px,py,pz. Can be thus helicitywise anything in range (-1,1).
*/
void simpleBoost(TauolaParticle *tau, TauolaParticle *target)
{
  double p1=tau->getPx();
  double p2=tau->getPy();
  double p3=tau->getPz();
  double E =tau->getE();
  double m =tau->getMass();

  double betx=p1/m;
  double bety=p2/m;
  double betz=p3/m;

  double gam=E/m;

  double pb=betx*target->getPx()+bety*target->getPy()+betz*target->getPz();

  target->setPx(target->getPx()+betx*(target->getE()+pb/(gam+1.0)));
  target->setPy(target->getPy()+bety*(target->getE()+pb/(gam+1.0)));
  target->setPz(target->getPz()+betz*(target->getE()+pb/(gam+1.0)));

  target->setE(target->getE()*gam+pb);
}

int main()
{
	// Initialization of pythia
	Pythia pythia;
	Event& event = pythia.event;

	// Pythia8 HepMC interface depends on Pythia8 version
#ifdef PYTHIA8180_OR_LATER
	HepMC::Pythia8ToHepMC ToHepMC;
#else
	HepMC::I_Pythia8 ToHepMC;
#endif

	pythia.readString("WeakSingleBoson:ffbar2gmZ = on");
	pythia.readString("23:onMode = off"); 
	pythia.readString("23:onIfAny = 15");

	// NOTE: this example is set up to pick undecayed tau
	//       changing this option requires changes in call to
	//       Tauola::decayOne and in tau searching 'for' loop
	//       See comments below
	pythia.readString("15:mayDecay = off");

	pythia.init( 11, -11, 92.);

	Tauola::setSameParticleDecayMode(3);
	Tauola::setOppositeParticleDecayMode(3);
	Tauola::initialize();

	tauola_print_parameters(); // Prints TAUOLA  parameters (residing inside its library): e.g. to test user interface

	ofstream s("out.txt");
	double y=0;
	int events_processed = 0;
	for(int i=0;i<N;i++)
	{
		// Convert event record to HepMC
		if(!pythia.next()) return -1;
		HepMC::GenEvent * HepMCEvt = new HepMC::GenEvent();
		//Conversion needed if HepMC uses different momentum units
		//than Pythia. However, requires HepMC 2.04 or higher.
		HepMCEvt->use_units(HepMC::Units::GEV,HepMC::Units::MM);

		ToHepMC.fill_next_event(event, HepMCEvt);

		if(i<EventsToCheck)
		{
			cout<<"                                          "<<endl;
			cout<<"Momentum conservation chceck BEFORE/AFTER Tauola"<<endl;
			checkMomentumConservationInEvent(HepMCEvt);
		}

		//TauolaHepMCEvent *tEvent = new TauolaHepMCEvent(HepMCEvt);
		// Search the event record for tau
		HepMC::GenParticle *tau=0;
		for(HepMC::GenEvent::particle_const_iterator p = HepMCEvt->particles_begin();p!=HepMCEvt->particles_end();p++)
		{
			if((*p)->pdg_id()==15)
			{
				if((*p)->status()!=1) continue;
				tau=*p;
				break;
			}
		}
	
		if(!tau) continue;
		/*
		Set boost routine if needed. Boost routine must look like:
		void boost(TauolaParticle *tau, TauolaParticle *target)
		See simpleBoost(...) function at the top of this file or documentation for more details
		*/
		//Tauola::setBoostRoutine(simpleBoost);
	
		//Create TauolaParticle from HepMC tau
		TauolaHepMCParticle *htau = new TauolaHepMCParticle(tau);
	
		// simplest use. For this demo it will do nothing because Pythia decayed taus already.
		Tauola::decayOne(htau); 
	
		//Decay single tau. If tau already has daughters - delete them.
		//Tauola::decayOne(htau,true);
		/*
		The third parameter, if provided, will be used as the polarization vector.
		*/
		//Tauola::decayOne(htau,true,0,0,1);

		if(i<EventsToCheck)
		{
			checkMomentumConservationInEvent(HepMCEvt);
		}

		if(htau->getDaughters().size()>=2)
		{
			double x = ((htau->getDaughters())[1]->getE())/htau->getE();
			//if(htau->getPz()<=0) x=1-x;   // this is necessary for checking interface for tau-gun running with simple Tauola::setBoostRoutine(simpleBoost);
			s<<x<<endl;
			y+=x;
			events_processed++;
		}

		delete HepMCEvt;
	}
	cout.setf(ios::fixed);
	cout<<y/events_processed<<endl;
	s.close();

  // This is an access to old FORTRAN info on generated tau sample. 
  // That is why it refers to old version number (eg. 2.7) for TAUOLA.
  //Tauola::summary();

	return 0;
}
