//////////////////////////////////////////////////////////////////////////
// testPolarization.cc
// 
// garren@fnal.gov, Oct. 2010
//////////////////////////////////////////////////////////////////////////

#include <iostream>
#include <fstream>
#include <vector>

#include "HepMC/GenEvent.h"
#include "HepMC/IO_GenEvent.h"

int main() {
    //
    // In this example we will place the following event into HepMC "by hand"
    //
    //     name status pdg_id  parent Px       Py    Pz       Energy      Mass
    //  1  !p+!    3   2212    0,0    0.000    0.000 7000.000 7000.000    0.938
    //  2  !p+!    3   2212    0,0    0.000    0.000-7000.000 7000.000    0.938
    //=========================================================================
    //  3  !d!     3      1    1,1    0.750   -1.569   32.191   32.238    0.000
    //  4  !u~!    3     -2    2,2   -3.047  -19.000  -54.629   57.920    0.000
    //  5  !W-!    3    -24    1,2    1.517   -20.68  -20.605   85.925   80.799
    //  6  !gamma! 1     22    1,2   -3.813    0.113   -1.833    4.233    0.000
    //  7  !d!     1      1    5,5   -2.445   28.816    6.082   29.552    0.010
    //  8  !u~!    1     -2    5,5    3.962  -49.498  -26.687   56.373    0.006

    // open an output file
    const char outfile[] = "testPolarization.cout";
    std::ofstream os( outfile );
    if( !os ) {
      std::cerr << "cannot open " << outfile << std::endl;
      exit(-1);
    }
    // declare several IO_GenEvent instances for comparison
    HepMC::IO_GenEvent xout1("testPolarization1.dat",std::ios::out);
    HepMC::IO_GenEvent xout2("testPolarization2.dat",std::ios::out);
    // output streams for copy test
    std::ofstream xout4( "testPolarization4.out" );
    std::ofstream xout5( "testPolarization5.out" );
 
    // build the graph, which will look like
    //                       p7                   #
    // p1                   /                     #
    //   \v1__p3      p5---v4                     #
    //         \_v3_/       \                     #
    //         /    \        p8                   #
    //    v2__p4     \                            #
    //   /            p6                          #
    // p2                                         #
    //
    // define a flow pattern as  p1 -> p3 -> p6
    //                       and p2 -> p4 -> p5
    //

    // First create the event container, with Signal Process 20, event number 1
    //
    HepMC::GenEvent* evt = new HepMC::GenEvent( 20, 1 );
    //
    // create vertex 1 and vertex 2, together with their inparticles
    HepMC::GenVertex* v1 = new HepMC::GenVertex();
    evt->add_vertex( v1 );
    HepMC::GenParticle* p1 = new HepMC::GenParticle( HepMC::FourVector(0,0,7000,7000),
				       2212, 3 );
    p1->set_flow(1,231);
    p1->set_polarization(HepMC::Polarization(rand(),rand()));
    v1->add_particle_in( p1 );
    HepMC::GenVertex* v2 = new HepMC::GenVertex();
    evt->add_vertex( v2 );
    HepMC::GenParticle* p2 = new HepMC::GenParticle( HepMC::FourVector(0,0,-7000,7000),
				       2212, 3 );
    p2->set_flow(1,243);
    p2->set_polarization(HepMC::Polarization(rand(),rand()));
    v2->add_particle_in( p2 );
    //
    // create the outgoing particles of v1 and v2
    HepMC::GenParticle* p3 = 
	new HepMC::GenParticle( HepMC::FourVector(.750,-1.569,32.191,32.238),
	                        1, 3 );
    p3->set_flow(1,231);
    p3->set_polarization(HepMC::Polarization(rand(),rand()));
    v1->add_particle_out( p3 );
    HepMC::GenParticle* p4 = 
	new HepMC::GenParticle( HepMC::FourVector(-3.047,-19.,-54.629,57.920),
	                        -2, 3 );
    p4->set_flow(1,243);
    p4->set_polarization(HepMC::Polarization(rand(),rand()));
    v2->add_particle_out( p4 );
    //
    // create v3
    HepMC::GenVertex* v3 = new HepMC::GenVertex();
    evt->add_vertex( v3 );
    v3->add_particle_in( p3 );
    v3->add_particle_in( p4 );
    HepMC::GenParticle* p6 = 
          new HepMC::GenParticle( HepMC::FourVector(-3.813,0.113,-1.833,4.233 ),
	                          22, 1 );
    p6->set_flow(1,231);
    p6->set_polarization(HepMC::Polarization(rand(),rand()));
    v3->add_particle_out( p6 );
    HepMC::GenParticle* p5 = 
	new HepMC::GenParticle( HepMC::FourVector(1.517,-20.68,-20.605,85.925),
	                        -24, 3 );
    p5->set_flow(1,243);
    p5->set_polarization(HepMC::Polarization(rand(),rand()));
    v3->add_particle_out( p5 );
    //
    // create v4
    HepMC::GenVertex* v4 = new HepMC::GenVertex(HepMC::FourVector(0.12,-0.3,0.05,0.004));
    evt->add_vertex( v4 );
    v4->add_particle_in( p5 );
    HepMC::GenParticle* p7 = new HepMC::GenParticle( HepMC::FourVector(-2.445,28.816,6.082,29.552), 1,1 );
    v4->add_particle_out( p7 );
    HepMC::GenParticle* p8 = new HepMC::GenParticle( HepMC::FourVector(3.962,-49.498,-26.687,56.373), -2,1 );
    v4->add_particle_out( p8 );
    //    
    // tell the event which vertex is the signal process vertex
    evt->set_signal_process_vertex( v3 );
    // the event is complete, we now print it out
    evt->print( os );
    // print each particle so we can see the polarization
    for ( HepMC::GenEvent::particle_iterator ip = evt->particles_begin();
          ip != evt->particles_end(); ++ip ) {
        (*ip)->print(os);
    }

    // write event
    xout1 << evt;
    // call the write method directly
    evt->write(xout4);
    // make a copy and write it
    HepMC::GenEvent(*evt).write(xout5);

    // try changing polarization
    p2->set_polarization(HepMC::Polarization(rand(),rand()));
    xout2 << evt;

    // now clean-up by deleteing all objects from memory
    //
    // deleting the event deletes all contained vertices, and all particles
    // contained in those vertices
    delete evt;

    return 0;
}
