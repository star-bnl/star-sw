//StiHitFiller.h
//M.L. Miller (Yale Software)
//04/01

//class to define interface between StEvent, StiHitContainer, and StiHitFactory (adaptor)

#ifndef StiHitFiller_HH
#define StiHitFiller_HH

#include <vector>
#include <map>
#include "../pams/global/inc/StDetectorId.h" //for detector enumerations
#include "StiFactoryTypedefs.h"

class ostream;
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
    void fillHits(StiHitContainer*, StiHitFactory*);

    void setTranslator();
    friend ostream& operator<<(ostream&, const StiHitFiller&);

private:
    void fillTpcHits(StiHitContainer*, StiHitFactory*);
    void fillSvtHits(StiHitContainer*, StiHitFactory*);
    void fillPrimaryVertices(StiHitContainer*, StiHitFactory*);
    
private:
    StiGeometryTransform* mtranslator;
    //StTpcCoordinateTransform* mtpctransformer;
    
    StEvent* mevent;
    det_id_vector mvec;
};

ostream& operator<<(ostream&, const StiHitFiller&);

#endif
