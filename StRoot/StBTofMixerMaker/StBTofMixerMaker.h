//
//  StBTofMixerMaker.h
//  
//
//  Created by Nickolas Luttrell on 6/3/16.
//
//

#ifndef StBTofMixerMaker_HH
#define StBTofMixerMaker_HH

#include "StMaker.h"
#include "St_DataSet.h"

class StEvent;
class StBTofCollection;
class StBTofHit;

#include <vector>

class StBTofMixerMaker : public StMaker{
    
public:
    StBTofMixerMaker(const char *name="Simulation");
    
    virtual ~StBTofMixerMaker();
    
    virtual int  Init();
    int          InitRun(int);
    int          FinishRun(int);
    virtual int  Make();
    virtual int  Finish();

    virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StBTofMixerMaker.h,v 1.3 2018/06/21 03:38:46 jdb Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StBTofMixerMaker,2)
    
protected:
    
    // All the internal definitions should be placed here.
    
    StEvent            *mEvent;                     //!< The StEvent info from previous Makers
    StBTofCollection   *mEventCollection = nullptr; //!< BtofCollection from StEvent
    StBTofCollection   *mBTofSimCollection = nullptr;   //!< BTofCollection produced by BTofSimMaker
    StBTofCollection    *mNewCollection = nullptr;      //!< BTofCollection to be assembled by embedding
    bool mIsEmbedding = kFALSE;                     //!< Embedding flag
    
    //! Find duplicate hits between the BTofCollections
    void findDuplicates(std::vector<StBTofHit*> &eventHits, StBTofCollection *simHits);
    
};

#endif /* StBTofMixerMaker_h */



// end of StBTofMixerMaker.h