#ifndef ST_TRIGGER_FILTER_MAKER_HH
#define ST_TRIGGER_FILTER_MAKER_HH

// $Id: StTriggerFilterMaker.h,v 1.4 2015/09/09 20:29:39 akio Exp $

/*****************************************************************************
 * @class StTriggerFilterMaker
 * @author A.Kocoloski
 *
 * Privileged Maker which skips events unless they match supplied trigger IDs.
 *****************************************************************************/

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include <vector>
using std::vector;

class StTriggerFilterMaker : public StMaker {
public:
    StTriggerFilterMaker(const char *name = "triggerFilter");
    virtual ~StTriggerFilterMaker();
    
    //virtual void Clear(const char *option="");
    virtual Int_t Init();
    //virtual Int_t InitRun(int runnumber);
    virtual Int_t Make();
    //virtual Int_t Finish();
    
    void addTrigger(unsigned int trigId) { mGoodTriggers.push_back(trigId); }
    void addVetoTrigger(unsigned int trigId) { mVetoTriggers.push_back(trigId); }
    void printTriggerId(int v=1) {mPrint=v;}

    const vector<unsigned int> getTriggers() const {return mGoodTriggers;}
    const vector<unsigned int> getVetoTriggers() const {return mGoodTriggers;}

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StTriggerFilterMaker.h,v 1.4 2015/09/09 20:29:39 akio Exp $ built " __DATE__ " " __TIME__; return cvs;}
    
private:
    vector<unsigned int> mGoodTriggers; //!
    vector<unsigned int> mVetoTriggers; //!
    int mPrint; //!

    ClassDef(StTriggerFilterMaker,1)
};

#endif

/*****************************************************************************
 * $Log: StTriggerFilterMaker.h,v $
 * Revision 1.4  2015/09/09 20:29:39  akio
 * Adding Vetoing TriggerId
 * Also adding printing if option is set
 *
 * Revision 1.3  2014/08/06 11:43:40  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.2  2008/07/31 18:02:14  mattheww
 * Added method to get trigger list
 *
 * Revision 1.1  2008/01/23 04:45:08  kocolosk
 * Privileged Maker which skips events unless they fired any one of a set of supplied trigIDs
 *
 *****************************************************************************/
