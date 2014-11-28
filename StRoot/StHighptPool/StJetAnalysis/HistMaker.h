//HistMaker.h

#ifndef HistMaker_HH
#define HistMaker_HH

#include "TObject.h"
#include "AnaCuts.h"

class StJetMuEvent;
class StMuTrack;
class TH1;
class TH2;

class HistMaker
{
public:
    HistMaker();
    virtual ~HistMaker();

    //Gets/sets
    void setCuts(const AnaCuts& c) {mCuts=c;}

    void setdEtaVsdPhi(TH2* h) {mdEtaVsdPhi=h;}
    TH2* dEtaVsdPhi() const {return mdEtaVsdPhi;}
    
    //action
    void fill(StJetMuEvent*);
    
private:
    bool isTrigger(StMuTrack* track);
    bool acceptTrack(StMuTrack* track);

    TH2* mdEtaVsdPhi; //!
    
    AnaCuts mCuts;
    
    ClassDef(HistMaker,1)
};
#endif
