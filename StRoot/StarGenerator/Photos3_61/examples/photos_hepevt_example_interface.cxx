/**
 * Interface for FORTRAN example of use of Photos++
 *
 * @author Tomasz Przedzinski
 * @date 21 August 2013
 */
#include <cstdlib>
#include "Photos/Photos.h"
#include "Photos/PhotosHEPEVTEvent.h"
using namespace Photospp;

extern "C" {

  void photos_init_()
  {
    Photos::initialize();
    Photos::forceMassFromEventRecord(11);
    
    // Turn on pair emission and turn off photon emission
    // Note that this example loops over the same event until at least one
    // particle is added, so uncommenting these two flags can be used
    // to test if a pair is correctly emitted
    //Photos::setPairEmission(true);
    //Photos::setPhotonEmission(false);
    
    // Turn on NLO corrections
    //Photos::setMeCorrectionWtForW(true);
    //Photos::setMeCorrectionWtForZ(true);
  }

  void photos_process_()
  {
    PhotosHEPEVTEvent *event = new PhotosHEPEVTEvent();

    PhotosHEPEVTEvent::read_event_from_HEPEVT(event);
    //event->print();

    int part = event->getParticleCount();
    int loop = 0;
    while( event->getParticleCount() <= part && loop < 10000 ) {
        event->process();
        ++loop;
    }
    
    if( loop >= 10000 ) {
        printf("photos_hepevt_example: there is some technical problem - no particles generated after 10000 loops!\n");
        exit(-1);
    }
    //event->print();

    PhotosHEPEVTEvent::write_event_to_HEPEVT(event);

    delete event;
  }

  void photos_process_particle_(int *i)
  {
    PhotosHEPEVTEvent *event = new PhotosHEPEVTEvent();

    PhotosHEPEVTEvent::read_event_from_HEPEVT(event);
    //event->print();

    PhotosParticle *p = event->getParticle(*i - 1);

    Photos::processParticle(p);
    //event->print();

    PhotosHEPEVTEvent::write_event_to_HEPEVT(event);

    delete event;
  }

  void photos_process_branch_(int *i)
  {
    PhotosHEPEVTEvent *event = new PhotosHEPEVTEvent();

    PhotosHEPEVTEvent::read_event_from_HEPEVT(event);
    //event->print();

    PhotosParticle *p = event->getParticle(*i - 1);

    Photos::processBranch(p);
    //event->print();

    PhotosHEPEVTEvent::write_event_to_HEPEVT(event);

    delete event;
  }
}
