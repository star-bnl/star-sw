//StiHitFiller.h
//M.L. Miller (Yale Software)
//04/01

//class to define interface between StEvent, StiHitContainer, and StiObjectFactoryInterface<StiHit> (adaptor)

#ifndef StiHitFiller_HH
#define StiHitFiller_HH

#include "StTimer.hh"

#include <iostream.h>
#include <vector>
#include <map>
#include "../pams/global/inc/StDetectorId.h" //for detector enumerations

#include "StiHit.h"
#include "StiObjectFactoryInterface.h"

using std::vector;
using std::map;

class StiHitContainer;
class StiGeometryTransform;
class StTpcCoordinateTransform;
class StEvent;
class StPrimaryVertex;

class StiHitFiller
{
public:

    typedef vector<StDetectorId> det_id_vector;
    
    StiHitFiller();
    virtual ~StiHitFiller();

    void addDetector(StDetectorId det);
    void setEvent(StEvent* val) {mevent=val;}
    void fillHits(StiHitContainer*, StiObjectFactoryInterface<StiHit>*);

    void setTranslator();
    friend ostream& operator<<(ostream&, const StiHitFiller&);

private:
    void fillTpcHits(StiHitContainer*, StiObjectFactoryInterface<StiHit>*);
    void fillSvtHits(StiHitContainer*, StiObjectFactoryInterface<StiHit>*);
    void fillPrimaryVertices(StiHitContainer*, StiObjectFactoryInterface<StiHit>*);
    
private:
    StiGeometryTransform* mtranslator;
    //StTpcCoordinateTransform* mtpctransformer;
    
    StEvent* mevent;
    det_id_vector mvec;
    StTimer mtimer;
};

ostream& operator<<(ostream&, const StiHitFiller&);

#endif
