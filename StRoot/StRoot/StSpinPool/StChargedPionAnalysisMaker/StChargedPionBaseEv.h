#ifndef STAR_StChargedPionBaseEv
#define STAR_StChargedPionBaseEv

// $Id: StChargedPionBaseEv.h,v 1.1 2008/07/17 17:06:30 kocolosk Exp $

/*****************************************************************************
 * @class StChargedPionBaseEv
 * @author Adam Kocoloski
 *
 * ABC defining common interface for data and Monte Carlo events
 *****************************************************************************/

#include <map>
using std::map;

#include <string>
using std::string;

#include "TObject.h"
class TClonesArray;

class StChargedPionVertex;
class StChargedPionTrack;
class StChargedPionJet;

class StChargedPionBaseEv : public TObject
{
public:
    virtual unsigned int runId() const                                  = 0;
    virtual unsigned int eventId() const                                = 0;
    virtual unsigned int bbcTimeBin() const                             = 0;
    
    virtual const string& muDstName() const                             = 0;
    
    virtual bool isSimuTrigger(unsigned int trigId) const               = 0;
    
    virtual int highTowerAdc(short towerId) const                       = 0;
    virtual int triggerPatchAdc(short patchId) const                    = 0;
    virtual int jetPatchAdc(short patchId) const                        = 0;
    
    virtual unsigned int                    nVertices() const           = 0;
    virtual TClonesArray*                   vertices()                  = 0;
    virtual const TClonesArray*             vertices() const            = 0;
                                    
    virtual unsigned int                    nTracks() const             = 0;
    virtual TClonesArray*                   tracks()                    = 0;
    virtual const TClonesArray*             tracks() const              = 0;
                                    
    virtual unsigned int                    nJets() const               = 0;
    virtual TClonesArray*                   jets()                      = 0;
    virtual const TClonesArray*             jets() const                = 0;
                                    
    virtual StChargedPionVertex*            vertex(int i)               = 0;
    virtual const StChargedPionVertex*      vertex(int i) const         = 0;
                                    
    virtual StChargedPionTrack*             track(int i)                = 0;
    virtual const StChargedPionTrack*       track(int i) const          = 0;
                                    
    virtual StChargedPionJet*               jet(int i)                  = 0;
    virtual const StChargedPionJet*         jet(int i) const            = 0;
    
    // setters
    virtual void setRunId(unsigned int)                                 = 0;
    virtual void setEventId(unsigned int)                               = 0;
    virtual void setBbcTimeBin(unsigned short)                          = 0;
    virtual void setMuDstName(const char*)                              = 0;
    virtual void addSimuTrigger(unsigned int)                           = 0;
    virtual void addHighTower(short towerId, int ADC)                   = 0;
    virtual void addTriggerPatch(short patchId, int ADC)                = 0;
    virtual void addJetPatch(short patchId, int ADC)                    = 0;
    virtual void addVertex(const StChargedPionVertex*)                  = 0;
    virtual void addTrack(const StChargedPionTrack*)                    = 0;
    virtual void addJet(const StChargedPionJet*)                        = 0;
    
    virtual void setL2Result(const void *address, bool emulated=false)  = 0;
    
    // and one static function
    static unsigned int triggerBit(unsigned int trigId);
    
protected:
    static map<unsigned int, unsigned int> mTriggerLookup; //!
    
private:
    ClassDef(StChargedPionBaseEv, 1)
};

#endif

/*****************************************************************************
 * $Log: StChargedPionBaseEv.h,v $
 * Revision 1.1  2008/07/17 17:06:30  kocolosk
 * big-bang integration StChargedPionMcEvent framework
 *
 *****************************************************************************/
