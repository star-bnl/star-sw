// TreeEntryClasses.cxx
// M.L. Miller, Yale Software, 7/00
#include "StMaker.h"
#include "TreeEntryClasses.h"
#include "StLorentzVectorF.hh"
// StEvent
#include "StEventTypes.h"
//#include "StEvent/StTpcDedxPidAlgorithm.h"
long countPrimaryTracks(StEvent&);

//--------------------------------------------
ClassImp(HitEntry)
HitEntry::HitEntry() {};

HitEntry::HitEntry(StTpcHit* hit)
{
    init();
    const StThreeVectorF position = hit->position();
    m_xhit = position.x();
    m_yhit = position.y();
    m_zhit = position.z();
    m_de = hit->charge();
    m_pad = 0;
    m_padrow = hit->padrow();
    m_sector = hit->sector();
}

HitEntry::~HitEntry() {};

void HitEntry::init()
{
    m_xhit = m_yhit = m_zhit = m_de = m_dx = m_crossingangle = 0.;
    m_pad = m_padrow = m_sector = m_fee = m_rdo = 0 ;
    return;
}

//Access
void HitEntry::setDx(double dx){m_dx = dx;}

void HitEntry::setCrossingAngle(double cangle){m_crossingangle = cangle;}

void HitEntry::print() const{
cout<<m_xhit<<"\t"<<m_yhit<<"\t"<<m_zhit<<"\t"
    <<m_sector<<"\t"<<m_padrow<<"\t"<<m_de<<"\t"<<m_dx<<"\t"<<m_crossingangle<<endl;
}

//--------------------------------------------- 
ClassImp(TrackEntry)
    TrackEntry::TrackEntry(const StTrack* track)
{
    m_hitArray = new TClonesArray("HitEntry",50);
    
    StThreeVectorF p = track->geometry()->momentum();    
    m_pt = p.perp();
    m_pz = p.z();
    m_pmag = p.mag();
    m_eta = p.pseudoRapidity();
    m_tpcpoints = track->detectorInfo()->numberOfPoints(kTpcId);
    m_fitpoints = track->fitTraits().numberOfFitPoints(kTpcId);
    m_dipangle = track->geometry()->dipAngle();
    m_charge = track->geometry()->charge();
    m_HitCounter=0;
    m_Vertex.clear();
    m_Event.clear();
}

TrackEntry::TrackEntry()
{
    m_hitArray = new TClonesArray("HitEntry",50);
    m_HitCounter=0;
    m_Vertex.clear();
    m_Event.clear();
    clear();
}

//Copy constructor
TrackEntry::TrackEntry(const TrackEntry& t) 
{
    m_hitArray = new TClonesArray("HitEntry",50);
    copyTrack(t);
}


TrackEntry::~TrackEntry()
{
    m_hitArray->Delete();
    delete m_hitArray;
}

void TrackEntry::fill(const TrackEntry& t) 
{
    copyTrack(t);
    return;
}

void TrackEntry::clear()
{
  m_hitArray->Clear();
  m_ze            = 0;
  m_zPi           = 0;
  m_zK            = 0;
  m_zp            = 0;
  m_zd            = 0;
    m_pt = 0.;
    m_pz = 0.;
    m_pmag = 0.;
    m_eta = 0.;
    m_tpcpoints = 0;
    m_fitpoints = 0;
    m_flag0points = 0;
    m_flag0pointsinner = 0;
    m_flag0pointsouter = 0;
    m_charge = 0;
    m_mean70 = 0.;
    m_erroronmean70 = 0.;
    m_dedxpoints70 = 0;
    m_mean55 = 0.;
    m_erroronmean55 = 0.;
    m_dedxpoints55 = 0;
    m_avgz=0.;
    m_avgzsquared=0.;
    m_avgzinner=0.;
    m_avgzinnersquared=0.;
    m_avgzouter=0.;
    m_avgzoutersquared=0.;

    m_zFitted       = 0;
    m_dzFitted      = 0;
    m_sFitted       = 0;
    m_dsFitted      = 0;
    m_ValFitted     = 0;
    
    m_HitCounter = 0;
    m_Vertex.clear();
    m_Event.clear();

    return;
}

void TrackEntry::copyTrack(const TrackEntry& t)
{
    m_ze = t.ze();
    m_zPi = t.zPi();
    m_zK = t.zK();
    m_zp = t.zp();
    m_zd = t.zd();
    m_pt = t.pt();
    m_pz = t.pz();
    m_pmag = t.pmag();
    m_eta = t.eta();
    m_tpcpoints = t.tpcpoints();
    m_fitpoints = t.fitpoints();
    m_flag0points = t.flag0Points();
    m_flag0pointsinner = t.flag0PointsInner();
    m_flag0pointsouter = t.flag0PointsOuter();
    m_dipangle = t.dipangle();
    m_mean70 = t.mean70();
    m_erroronmean70 = t.erroronmean70();
    m_dedxpoints70 = t.dedxpoints70();
    m_mean55 = t.mean55();
    m_erroronmean55 = t.erroronmean55();
    m_dedxpoints55 = t.dedxpoints55();
    m_avgz = t.avgz();
    m_avgzsquared = t.avgzsquared();
    m_avgzinner = t.avgzinner();
    m_avgzinnersquared = t.avgzinnersquared();
    m_avgzouter = t.avgzouter();
    m_avgzoutersquared = t.avgzoutersquared();
    m_charge = t.charge();
    m_HitCounter = t.hitCounter();
    m_zFitted = t.zFitted();
    m_dzFitted = t.dzFitted();
    m_sFitted = t.sFitted();
    m_dsFitted = t.dsFitted();
    m_ValFitted = t.ValFitted();
    m_TpcLength = t.TpcLength();
    m_TrackZ = t.TrackZ();
  //-------
    m_Vertex = t.vertex();
    m_Event = t.event();
    copyHits(t);
    
    return;
}
void TrackEntry::copyHits(const TrackEntry& t)
{
    TClonesArray* pArray = const_cast<TClonesArray*>(t.hitArray());
    TClonesArray& oldArray = *pArray;
    TClonesArray& rHitArray = *m_hitArray;
    for (int i=0; i<m_HitCounter; i++) {
	HitEntry* temp = static_cast<HitEntry*>(oldArray[i]);
	HitEntry& oldEntry = *temp;
	new(rHitArray[i]) HitEntry(oldEntry);
    }
}

void TrackEntry::setFlag0Points(int val){m_flag0points = val;}
void TrackEntry::setFlag0PointsInner(int val)
{
    m_flag0pointsinner = val;
    return;
}

void TrackEntry::setFlag0PointsOuter(int val)
{
    m_flag0pointsouter = val;
    return;
}

void TrackEntry::setAvgZ(double val)
{
    m_avgz = val;
    return;
}

void TrackEntry::setAvgZSquared(double val)
{
    m_avgzsquared = val;
    return;
}

void TrackEntry::setAvgZInner(double val)
{
    m_avgzinner = val;
    return;
}

void TrackEntry::setAvgZInnerSquared(double val)
{
    m_avgzinnersquared = val;
    return;
}

void TrackEntry::setAvgZOuter(double val)
{
    m_avgzouter = val;
    return;
}

void TrackEntry::setAvgZOuterSquared(double val)
{
    m_avgzoutersquared = val;
    return;
}

void TrackEntry::setMean55(double val)
{
    m_mean55 = val;
    return;
}

void TrackEntry::setErrorOnMean55(double val)
{
    m_erroronmean55 = val;
    return;
}

void TrackEntry::setDeDxPoints55(int val)
{
    m_dedxpoints55 = val;
    return;
}

void TrackEntry::setPidTraits(const StDedxPidTraits* traits)
{
    m_mean70 = traits->mean();
    m_erroronmean70 = traits->errorOnMean();
    m_dedxpoints70 = traits->numberOfPoints();
    return;
}

void TrackEntry::addEvent(const EventEntry& entry)
{
    m_Event = entry;
    return;
}

void TrackEntry::addVertex(const VertexEntry& entry)
{
    m_Vertex = entry;
    return;
}

void TrackEntry::addHit(HitEntry entry)
{
    TClonesArray& cArr = *m_hitArray;
    new(cArr[m_HitCounter]) HitEntry(entry);
    m_HitCounter++;
    return;
}

void TrackEntry::print() const
{
    cout <<m_pt<<"\t"<<m_pz<<"\t"<<m_eta<<"\t"<<m_tpcpoints<<"\t"<<m_fitpoints<<"\t"<<m_HitCounter<<endl;

    cout <<"\t\t\t----\t HitEntry Info\t----"<<endl;
    cout <<"x \t y \t z \t secotr \t padrow \t de \t dx \t cangle"<<endl;
    TClonesArray& rHitArray = *m_hitArray;
    for (int i=0; i<m_HitCounter; i++) {
	const HitEntry* temp = static_cast<HitEntry*>(rHitArray[i]);
	const HitEntry& hit = *temp;
	hit.print();
    }
    return;
}

double TrackEntry::pt() const {return m_pt;}
double TrackEntry::pz() const {return m_pz;}
double TrackEntry::pmag() const {return m_pmag;}
double TrackEntry::eta() const {return m_eta;}
int TrackEntry::tpcpoints() const {return m_tpcpoints;}
int TrackEntry::fitpoints() const {return m_fitpoints;}
int TrackEntry::flag0Points() const {return m_flag0points;}
int TrackEntry::flag0PointsInner() const {return m_flag0pointsinner;}
int TrackEntry::flag0PointsOuter() const {return m_flag0pointsouter;}
double TrackEntry::dipangle() const {return m_dipangle;}
int TrackEntry::charge() const {return m_charge;}
double TrackEntry::mean70() const {return m_mean70;}
double TrackEntry::erroronmean70() const {return m_erroronmean70;}
int TrackEntry::dedxpoints70() const {return m_dedxpoints70;}
double TrackEntry::mean55() const {return m_mean55;}
double TrackEntry::erroronmean55() const {return m_erroronmean55;}
int TrackEntry::dedxpoints55() const {return m_dedxpoints55;}
double TrackEntry::avgz() const {return m_avgz;}
double TrackEntry::avgzsquared() const {return m_avgzsquared;}
double TrackEntry::avgzinner() const {return m_avgzinner;}
double TrackEntry::avgzinnersquared() const {return m_avgzinnersquared;}
double TrackEntry::avgzouter() const {return m_avgzouter;}
double TrackEntry::avgzoutersquared() const {return m_avgzoutersquared;}
int TrackEntry::hitCounter() const {return m_HitCounter;}
const TClonesArray* TrackEntry::hitArray() const {return m_hitArray;}
const VertexEntry& TrackEntry::vertex() const {return m_Vertex;}
const EventEntry& TrackEntry::event() const {return m_Event;}

//--------------------------------------------- 
ClassImp(VertexEntry)
    VertexEntry::VertexEntry()
{
}

VertexEntry::~VertexEntry() 
{
}

void VertexEntry::fill(StPrimaryVertex* vtx)
{
    StThreeVectorF position = vtx->position();
    m_xvertex = position.x();
    m_yvertex = position.y();
    m_zvertex = position.z();
    return;
}

void VertexEntry::clear()
{
    m_xvertex=0;
    m_yvertex=0;
    m_zvertex=0;
    return;
}

//---------------------------------------------------------------------
ClassImp(EventEntry)
    EventEntry::EventEntry() {};

EventEntry::~EventEntry() {};

void EventEntry::clear()
{
    m_time = 0;
    m_nprimarytracks = 0;
    //    m_multiplicity = 0;
  m_run = 0;
  m_eventno = 0;
  m_date =  0;
  m_time = 0;
    
    return;
}

void EventEntry::fill(const StEvent* pEvent)
{
  m_run = StMaker::GetChain()->GetRunNumber();
  m_eventno = StMaker::GetChain()->GetEventNumber();
  m_date =  StMaker::GetChain()->GetDate();
  m_time = StMaker::GetChain()->GetTime();
  //    m_time = pEvent->time();
  //    m_multiplicity = pEvent->summary()->numberOfTracks();
  //    m_nprimarytracks = pEvent->summary()->numberOfGoodPrimaryTracks();
  m_nprimarytracks = countPrimaryTracks(*pEvent);
}

