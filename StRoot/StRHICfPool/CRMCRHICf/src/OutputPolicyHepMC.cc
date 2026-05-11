#include <OutputPolicyHepMC.h>

#include <CRMCoptions.h>
#include <CRMCinterface.h>

// TODO check for has hepmc
#include <HepMC/GenEvent.h>
#include <HepMC/HeavyIon.h>
#include <HepMC/IO_GenEvent.h>
#include <HepMC/PdfInfo.h>

#ifdef HEPMC_HAS_CROSS_SECTION
#include <HepMC/GenCrossSection.h>
#endif

#ifdef HEPMC_HAS_UNITS
#include <HepMC/Units.h>
#endif

#include <iomanip>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <stdlib.h>
#include <string>

#include <boost/iostreams/device/file_descriptor.hpp>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <boost/iostreams/filter/zlib.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>

#include <CRMCconfig.h> //cmake generated

namespace io = boost::iostreams;

using namespace std;


OutputPolicyHepMC::OutputPolicyHepMC()
{
}


void
OutputPolicyHepMC::InitOutput(const CRMCoptions& cfg)
{
  boost::filesystem::path oldFile(cfg.GetOutputFileName());
  if(!boost::filesystem::is_other(cfg.GetOutputFileName())) //protect fifo file
    boost::filesystem::remove(oldFile); //before liboost v1.44 truncate does not seem to work properly in boost

  //io::filtering_ostream out; //top to bottom order
  fOut = new io::filtering_ostream();
  if (cfg.GetOutputMode()==CRMCoptions::eHepMCGZ)
    fOut->push(io::gzip_compressor(io::zlib::best_compression));
  fOut->push(io::file_descriptor_sink(cfg.GetOutputFileName()), ios_base::trunc);

  // Instantiate an IO strategy to write the data to file
  ascii_out = new HepMC::IO_GenEvent(*fOut);
}


void
OutputPolicyHepMC::FillEvent(const CRMCoptions& cfg, const int nEvent)
{
#ifdef HEPMC_HAS_UNITS
  HepMC::GenEvent* fEvtHepMC = new HepMC::GenEvent(HepMC::Units::GEV,
						   HepMC::Units::MM);
#else
  HepMC::GenEvent* fEvtHepMC = new HepMC::GenEvent();
#endif

 if (!hepevt.convert()) {
   // delete fEvtHepMC;
   throw std::runtime_error("!!!Could not read next event");
 }

 for (auto v : hepevt.vertices())  fEvtHepMC->add_vertex(v);
 HepMC::GenParticle* b1 = hepevt.beams().size() > 0 ? hepevt.beams()[0] : 0;
 HepMC::GenParticle* b2 = hepevt.beams().size() > 1 ? hepevt.beams()[1] : 0;
 fEvtHepMC->set_beam_particles(b1,b2);

 if (!cfg.IsTest()){
#ifdef HEPMC_HAS_CROSS_SECTION
   // set cross section information for this event
   HepMC::GenCrossSection crossSection;
   crossSection.set_cross_section(1e9*(cfg.GetProjectileId()>1 || cfg.GetTargetId()>1 ?
					  gCRMC_data.sigineaa : 
					  gCRMC_data.sigine)); //required in pB
   fEvtHepMC->set_cross_section(crossSection);
#endif

   // provide optional pdf set id numbers for CMSSW to work flavour of
   // partons and stuff. hope it's optional
   HepMC::PdfInfo pdf(0, 0, 0, 0, 0, 0, 0);
   fEvtHepMC->set_pdf_info(pdf);
   
   //Setting heavy ion infromation
   // 
   //  Ncoll_hard                    Number of hard scatterings
   //  Npart_proj                    Number of projectile participants
   //  Npart_targ                    Number of target participants
   //  Ncoll                         Number of NN (nucleon-nucleon) collisions
   //  spectator_neutrons            Number of spectator neutrons
   //  spectator_protons             Number of spectator protons
   //  N_Nwounded_collisions         Number of N-Nwounded collisions
   //                                (here Glauber number of participants
   //                                with at least 1 interaction) 
   //  Nwounded_N_collisions         Number of Nwounded-N collisons
   //                                (here Glauber number of participants
   //                                with at least 2 interaction2)
   //  Nwounded_Nwounded_collisions  Number of Nwounded-Nwounded collisions
   //                                (here GLauber number of collisions)
   //  impact_parameter              Impact Parameter(fm) of collision
   //  event_plane_angle             Azimuthal angle of event plane
   //  eccentricity                  eccentricity of participating nucleons
   //                                in the transverse plane
   //                                (as in phobos nucl-ex/0510031)
   //  sigma_inel_NN                 nucleon-nucleon inelastic
   //                                (including diffractive) cross-section
   
   HepMC::HeavyIon ion(gCRMC_data.kohevt,
		       gCRMC_data.npjevt,
		       gCRMC_data.ntgevt,
		       gCRMC_data.kolevt,
		       gCRMC_data.npnevt + gCRMC_data.ntnevt,
		       gCRMC_data.nppevt + gCRMC_data.ntpevt,
		       gCRMC_data.ng1evt,
		       gCRMC_data.ng2evt,
		       gCRMC_data.nglevt,
		       gCRMC_data.bimevt,
		       gCRMC_data.phievt,
		       gCRMC_data.fglevt,  //defined only if phimin=phimax=0.
		       gCRMC_data.sigine*1e9); //required in pB
   fEvtHepMC->set_heavy_ion(ion);

   // add some information to the event
   fEvtHepMC->set_event_number(nEvent);
   
   //an integer ID uniquely specifying the signal process (i.e. MSUB in Pythia)
   int sig_id = -1;
   switch (gCRMC_data.typevt) // if negative typevt mini plasma was created by event (except -4)
     {
     case   0: sig_id = 91; break; //elastic
     case   1: sig_id = 95; break; //ND
     case  -1: sig_id = 96; break; //ND with core
     case   2: sig_id = 94; break; //DD
     case  -2: sig_id = 94; break; //DD with core
     case   3: sig_id = 97; break; //CD
     case  -3: sig_id = 97; break; //CD with core
     case   4: sig_id = 92; break; //SD (proj excit.)
     case  -4: sig_id = 93; break; //SD (targ excit.)
     case  10: sig_id = 98; break; //pion exchange elastic
     case  11: sig_id = 98; break; //pion exchange ND
     case -11: sig_id = 98; break; //pion exchange ND with core
     case  12: sig_id = 98; break; //pion exchange DD
     case -12: sig_id = 98; break; //pion exchange DD with core
     case  13: sig_id = 98; break; //pion exchange CD
     case -13: sig_id = 98; break; //pion exchange CD with core
     case  14: sig_id = 98; break; //pion exchange SD (proj excit.)
     case -14: sig_id = 98; break; //pion exchange SD (targ excit.)
     default: cerr << "Signal ID not recognised for setting HEPEVT" << endl;
     }
   fEvtHepMC->set_signal_process_id(sig_id);

   if(fEvtHepMC->vertices_begin()!=fEvtHepMC->vertices_end())
     fEvtHepMC->set_signal_process_vertex(*(fEvtHepMC->vertices_begin()));


   // write the event out to the ascii file
   (*ascii_out) << fEvtHepMC;
 }
 else {
  // Test mode : compute directly some observables
  int    multiplicity = 0;
  int    plateau      = 0;
  double energy       = 0;
  double pz           = 0;
  for (HepMC::GenEvent::particle_const_iterator i = fEvtHepMC->particles_begin();
       i != fEvtHepMC->particles_end(); ++i) {
    auto p = *i;
    if(p->status() != 1) continue;
    
    multiplicity++;
    double eta = p->momentum().pseudoRapidity();
    double pt  = p->momentum().perp();

    if (fabs(eta) < 10000) _eta.fill(eta);
    _pt.fill(pt);
    
    if (fabs(eta) < 0.5) plateau++;
    
    energy += p->momentum().e();
    pz     += p->momentum().pz();
  }
  _e  .fill(energy);
  _pz .fill(pz);
  _m  .fill(multiplicity);
  _mid.fill(plateau);
 }
 // we also need to delete the created event from memory
 delete fEvtHepMC;
}


void
OutputPolicyHepMC::CloseOutput(const CRMCoptions& cfg)
{
  //fOut->close();
  delete ascii_out;
  delete fOut;
}

//--------------------------------------------------------------------
void
OutputPolicyHepMC::PrintTestEvent(const CRMCoptions& cfg)
{
  PrintCrossSections(cfg);
  
  std::cout.setf(std::ios::showpoint);
  std::cout.setf(std::ios::fixed);
  std::cout.precision(3);
  std::ostream& o = std::cout;

  if (_m._cnt <= 0) {
    o << "Error during test : no particles !" << std::endl;
    return;
  }

  o << "\n"
    << "  Energy (GeV):                  " << _e    << "\n"
    << "  Long. Momentum (GeV/c):        " << _pz   << "\n"
    << "  Multiplicity:                  " << _m    << "\n"
    << "  PlateauHeight:                 " << _mid  << "\n"
    << "  MeanPseudorapidity:            " << _eta  << "\n"
    << "  MeanPt (GeV/c):                " << _pt   << "\n"
    << std::endl; 
}
