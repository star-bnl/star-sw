// DeDxPreparation.h
// M.L. Miller
// 6/00

#ifndef DeDxPreparation_HH
#define DeDxPreparation_HH

#include <map>
#include <vector>
using std::map;
using std::vector;

#include "TpcHitVecUtilities.h"
#include "TpcMapUtilities.h"

class HitEntry;

typedef map<HitMapKey, PadrowLocation, MapKeyLessThan>::value_type padrowMapValType;

class DeDxPreparation : public TpcHitVecUtilities {
public:
    DeDxPreparation();
    DeDxPreparation(StTrack*, int, int, int);
    virtual ~DeDxPreparation();

    //Access------------------------------------------
    void clear(); //Clear class, keep base class info
    void clearAll(); //Clear base class, too
    void setFillTuple(bool);
    void setMinPadrow(int);
    void setMaxPadrow(int);
    void setBField(double b) {m_BField = b;}
    double avgZ() {return m_AvgZ;}
    double avgZSquared(){return m_AvgZSquared;}
    void DoFitZ();
    double fitZ() const {return m_fitZ;}
    double fitdZ() const {return m_fitdZ;}
    double fitS() const {return m_fitS;}
    double fitdS() const {return m_fitdS;}
    double ValFitted() const {return m_ValFitted;}
    double TpcLength() const {return m_TpcLength;}
    double TrackZ() const {return m_TrackZ;}
    void setfitZ(Double_t m) {m_fitZ = m;}
    void setfitdZ(Double_t m) {m_fitdZ = m;}
    void setfitS(Double_t m) {m_fitS = m;}
    void setfitdS(Double_t m) {m_fitdS = m;}
    void setValFitted(Double_t m) {m_ValFitted = m;}
    void setTpcLength(Double_t m) {m_TpcLength = m;}
    void setTrackZ(Double_t m) {m_TrackZ = m;}
    int numberOfGoodHits() const;
    const vector<double>& normChargeVec() const; //!
    const vector<HitEntry>& hitEntryVec() const; //!
    
    //Methods---------------------------------------
    void buildMaps();
    bool keepHit(StTpcHit*);     //Hit Selection Filter
    void dxNorm();     //Normalize by the pathlength
    double findPathLength(StTpcHit* );     //Find the pathlength across a pad
    double crossingAngle(StTpcHit* );     //Calculate the crossing Angle for a pad (middle of the pad)
    StThreeVectorD sectorNormal(int );     //Return the normal vector for a sector
    Int_t fillPerHitTuple(StTpcHit*, double);
    void findAvgZ();
    
    //STL Utilities-----------------------------------
    void printHitEntryVec();
    void printNormChargeVec();
    // Fit
    void plotResults();
    void doFit();
protected:
    vector<double> m_normChargeVec; //!A vector of charge normalized by pathlength.
    vector<HitEntry> m_HitEntryVec; //!    A vector of class HitEntry
    map<int, StThreeVectorD> m_SectorNormalMap; //! Map of normal vectors to a sector
    map<HitMapKey, PadrowLocation, MapKeyLessThan> m_PadrowMap; //! Map of 3 points in each padrow 

    double m_AvgZ;
    double m_AvgZSquared;
    double m_fitZ;
    double m_fitdZ;
    double m_fitS;
    double m_fitdS;
    double m_ValFitted;
    double m_TpcLength;
    double m_TrackZ;
    bool m_FillTuple;
    double m_BField;
    int m_minPadRow;
    int m_maxPadRow;
    bool m_debug;
};

#endif
