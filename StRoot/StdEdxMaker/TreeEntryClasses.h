// TreeEntryClasses.h
// M.L. MIller, Yale Software, 7/00

#ifndef TreeEntryClasses_h
#define TreeEntryClasses_h

#include "TObject.h"
#include "TStyle.h"
#include "TNtuple.h"
#include "TH1.h"
#include "TF1.h"
#include "TCanvas.h"
#include "TFile.h"

class StEvent;
class StTrack;
class StPrimaryVertex;
class StHit;
class StTpcHit;
class StDedxPidTraits;

class EventEntry : public TObject {
public:
    EventEntry();
    virtual ~EventEntry();

    void fill(const StEvent*);
    void clear();
    
private:
    int m_run;
    int m_eventno;
    int m_date;
    int m_time;
    int m_nprimarytracks;
    //    int m_multiplicity;
    
    ClassDef(EventEntry,1)    
};

class VertexEntry : public TObject {
public:
    VertexEntry();
    virtual ~VertexEntry();
    void print() const;
    void clear();
    void fill(StPrimaryVertex*);
    
private:
    double m_xvertex;
    double m_yvertex;
    double m_zvertex;
    
    ClassDef(VertexEntry,1)
};

class HitEntry : public TObject {
public:
    HitEntry();
    HitEntry(StTpcHit*);
    void init();
    void print() const;
    virtual ~HitEntry();

    //Access---
    void setDx(double);
    void setCrossingAngle(double);
    
    double m_xhit;
    double m_yhit;
    double m_zhit;
    int m_pad;
    int m_padrow;
    int m_sector;
    int m_pin;
    int m_fee;
    int m_rdo;
    double m_de;
    double m_dx;
    double m_crossingangle;
    ClassDef(HitEntry,1)
};

class TrackEntry : public TObject {
public:
    TrackEntry();
    TrackEntry(const TrackEntry&);
    TrackEntry(const StTrack*);
    virtual ~TrackEntry();
    
    //Methods
    void fill(StTrack*);
    void fill(const TrackEntry&);
    void clear();
    void print() const;
    void copyTrack(const TrackEntry& t);
    void copyHits(const TrackEntry& t);
    
    void setAvgZ(double );
    void setAvgZSquared(double );
    void setAvgZInner(double );
    void setAvgZInnerSquared(double );
    void setAvgZOuter(double );
    void setAvgZOuterSquared(double );
    void setMean55(double);
    void setErrorOnMean55(double);
    void setDeDxPoints55(int);
    void setFlag0Points(int);
    void setFlag0PointsInner(int);
    void setFlag0PointsOuter(int);
    void setPidTraits(const StDedxPidTraits*);
    void setze(Double_t m) {m_ze=m;}
    void setzPi(Double_t m) {m_zPi=m;}
    void setzK(Double_t m) {m_zK=m;}
    void setzp(Double_t m) {m_zp=m;}
    void setzd(Double_t m) {m_zd=m;}
    void addHit(HitEntry);
    void setZFitted(double m) {m_zFitted = m;}
    void setdZFitted(double m) {m_dzFitted = m;}
    void setSFitted(double m) {m_sFitted = m;}
    void setdSFitted(double m) {m_dsFitted = m;}
    void setValFitted(double m) {m_ValFitted = m;}
    void setTpcLength(double m) {m_TpcLength = m;}
    void setTrackZ(double m) {m_TrackZ = m;}
    void addVertex(const VertexEntry&);
    void addEvent(const EventEntry&);

    
    //Access
    double pt() const;
    double pz() const;
    double pmag() const;
    double eta() const;
    int tpcpoints() const;
    int fitpoints() const;
    int flag0Points() const;
    int flag0PointsInner() const;
    int flag0PointsOuter() const;
    double dipangle() const;
    double mean70() const;
    double erroronmean70() const;
    int dedxpoints70() const;
    double mean55() const;
    double erroronmean55() const;
    int dedxpoints55() const;
    double avgz() const;
    double avgzsquared() const;
    double avgzinner() const;
    double avgzinnersquared() const;
    double avgzouter() const;
    double avgzoutersquared() const;
    int charge() const;
    int hitCounter() const;
    double zFitted() const {return m_zFitted;}
    double dzFitted() const {return m_dzFitted;}
    double sFitted() const {return m_sFitted;}
    double dsFitted() const {return m_dsFitted;}
    double ValFitted() const {return m_ValFitted;}
    double TpcLength() const {return m_TpcLength;}
    double TrackZ() const {return m_TrackZ;}
    double ze() const {return m_ze;}
    double zPi() const {return m_zPi;}
    double zK() const {return m_zK;}
    double zp() const {return m_zp;}
    double zd() const {return m_zd;}
    const TClonesArray* hitArray() const;
    const VertexEntry& vertex() const;
    const EventEntry& event() const;
    
private:
    TClonesArray* m_hitArray;
    double m_ze;
    double m_zPi;
    double m_zK;
    double m_zp;
    double m_zd;
    double m_pt;
    double m_pz;
    double m_pmag;
    double m_eta;
    int m_tpcpoints;
    int m_fitpoints;
    int m_flag0points;
    int m_flag0pointsinner;
    int m_flag0pointsouter;
    double m_dipangle;
    double m_mean70;
    double m_erroronmean70;
    int m_dedxpoints70;
    double m_mean55;
    double m_erroronmean55;
    int m_dedxpoints55;
    double m_avgz;
    double m_avgzsquared;
    double m_avgzinner;
    double m_avgzinnersquared;
    double m_avgzouter;
    double m_avgzoutersquared;
    int m_charge;
    int m_HitCounter;
    double m_zFitted;
    double m_dzFitted;
    double m_sFitted;
    double m_dsFitted;
    double m_ValFitted;
    double m_TpcLength;
    double m_TrackZ;
    VertexEntry m_Vertex;     //Vertex info
    EventEntry m_Event;       //Event info
    
    ClassDef(TrackEntry,1)
};

#endif
