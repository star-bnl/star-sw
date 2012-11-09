#ifndef ST_CHARGED_PION_TRACK_HH
#define ST_CHARGED_PION_TRACK_HH

// $Id: StChargedPionTrack.h,v 1.6 2012/11/09 03:31:34 perev Exp $

#include "StPhysicalHelixD.hh"
#include "StThreeVectorF.hh"

#include "TObject.h"
#include "TLorentzVector.h"

class StChargedPionTrack : public TLorentzVector
{
public:
    StChargedPionTrack();
    virtual ~StChargedPionTrack();
    StChargedPionTrack(const StChargedPionTrack& t);
    
    short id() const; ///< Returns the track id(or key), is unique for a track node, i.e. global and primary tracks have the same id.
    short flag() const; ///< Returns flag, (see StEvent manual for type information) 
    
    short vertexIndex() const; ///< Returns index of associated primary vertex.
    StThreeVectorF vertex() const; ///<Position of vertex for this track.
    
    unsigned short nHits() const;     ///< Return total number of hits on track.
    unsigned short nHitsPoss() const; ///< Return number of possible hits on track.
    unsigned short nHitsDedx() const; ///< Return number of hits used for dEdx. 
    unsigned short nHitsFit() const;  ///< Return total number of hits used in fit. 
    
    float nSigmaElectron() const;  ///< Returns Craig's distance to the calculated dE/dx band for electrons in units of sigma.
    float nSigmaPion() const;      ///< Returns Craig's distance to the calculated dE/dx band for pions in units of sigma.
    float nSigmaKaon() const;      ///< Returns Craig's distance to the calculated dE/dx band for kaons in units of sigma.
    float nSigmaProton() const;    ///< Returns Craig's distance to the calculated dE/dx band for protons in units of sigma.
    float dEdx() const; ///< Returns measured dE/dx value.
    
    float chi2() const;     ///< Returns chi2 of fit.       
    float chi2prob() const; ///< Returns probability of fit.
    
    float pt() const;   ///< Returns pT at point of dca to primary vertex.
    float phi() const;  ///< Returns phi at point of dca to primary vertex.
    float eta() const;  ///< Returns pseudo rapidity at point of dca to primary vertex.
    
    short charge() const;  ///< Returns charge. 
    double B() const; ///<Returns signed magnetic field.
    
    //some random stuff
    double length() const; ///< Returns length of track (cm) from primary vertex to last measured point.
    double lengthMeasured() const;  ///< Returns length of track (cm) from first to last measured point.
    
    //global track properties
    StThreeVectorF          globalLastPoint() const; ///< Returns positions of last measured point on global track.
    StPhysicalHelixD        globalHelix() const; ///< Returns inner helix (first measured point on global track).
    StThreeVector<double>   globalFirstPoint() const; ///< Returns positions of first measured point on global track.
    
    //these are calculated using first measured point
    double                  globalPt() const;
    double                  globalPhi() const;
    double                  globalEta() const;
    TLorentzVector          globalP() const;
    
    //can choose a different point if you like ... specify vertex to recover StMuTrack global kinematics
    double                  globalPt(StThreeVectorF position) const;
    double                  globalPhi(StThreeVectorF position) const;
    double                  globalEta(StThreeVectorF position) const;
    TLorentzVector          globalP(StThreeVectorF position) const;
    
    StThreeVectorF          globalDca() const; ///< Returns 3D distance of closest approach to primary vertex.
    StThreeVectorF          globalDca(StThreeVectorF position) const; ///< Returns 3D DCA to a specified point.
    
    //setters
    void setId(short aId) {mId = aId;}
    void setFlag(short aFlag) {mFlag = aFlag;}
    
    void setVertexIndex(int aVertexIndex) {mVertexIndex = aVertexIndex;}
    void setVertex(StThreeVectorF aPosition) {mVertex = aPosition;}
    
    void setNHits(unsigned short aNHits) {mNHits = aNHits;}
    void setNHitsPoss(unsigned short aNHitsPoss) {mNHitsPoss = aNHitsPoss;}
    void setNHitsDedx(unsigned short aNHitsDedx) {mNHitsDedx = aNHitsDedx;}
    void setNHitsFit(unsigned short aNHitsFit) {mNHitsFit = aNHitsFit;}
    
    void setNSigmaElectron(float aNSigmaElectron) {mNSigmaElectron = aNSigmaElectron;}
    void setNSigmaPion(float aNSigmaPion) {mNSigmaPion = aNSigmaPion;}
    void setNSigmaKaon(float aNSigmaKaon) {mNSigmaKaon = aNSigmaKaon;}
    void setNSigmaProton(float aNSigmaProton) {mNSigmaProton = aNSigmaProton;}
    void setDedx(float aDedx) {mdEdx = aDedx;}
    
    void setChi2(float aChi2) {mChi2 = aChi2;}
    void setChi2prob(float aChi2Prob) {mChi2Prob = aChi2Prob;}
    
    void setPtEtaPhi(float aPt, float aEta, float aPhi);
    void setPt(float aPt);
    void setPhi(float aPhi);
    void setEta(float aEta);
    
    void setCharge(short aCharge) {mCharge = aCharge;}
    void setB(double aMagneticField) {mB = aMagneticField;}
    
    void setGlobalLastPoint(StThreeVectorF aLastPoint) {mGlobalLastPoint = aLastPoint;}
    void setGlobalHelix(StPhysicalHelixD aHelix) {mGlobalHelix = aHelix;}
    
protected:    
    //basic QA -- could also use fBits to store online sanity checks
    Short_t mId;
    Short_t mFlag;
    
    Char_t mVertexIndex;
    StThreeVectorF mVertex; //stored for convenience of DCA calculation

    UChar_t mNHits;           
    UChar_t mNHitsPoss;      
    UChar_t mNHitsDedx;       
    UChar_t mNHitsFit;
    
    Float_t mNSigmaElectron;
    Float_t mNSigmaPion;
    Float_t mNSigmaKaon;
    Float_t mNSigmaProton;
    Float_t mdEdx;
    
    Float_t mChi2;
    Float_t mChi2Prob;
    
    Char_t mCharge;
    Double_t mB;
        
    //global track properties -- most can be recalculated from helix
    StThreeVectorF      mGlobalLastPoint;
    StPhysicalHelixD    mGlobalHelix;
    
    ClassDef(StChargedPionTrack,2)
};

inline short StChargedPionTrack::id() const {return mId;}
inline short StChargedPionTrack::flag() const {return mFlag;}

inline short StChargedPionTrack::vertexIndex() const {return mVertexIndex;}
inline StThreeVectorF StChargedPionTrack::vertex() const {return mVertex;}

inline unsigned short StChargedPionTrack::nHits() const {return mNHits;}
inline unsigned short StChargedPionTrack::nHitsPoss() const {return mNHitsPoss;}
inline unsigned short StChargedPionTrack::nHitsDedx() const {return mNHitsDedx;}
inline unsigned short StChargedPionTrack::nHitsFit() const {return mNHitsFit;}

inline float StChargedPionTrack::nSigmaElectron() const {return mNSigmaElectron;}
inline float StChargedPionTrack::nSigmaPion() const {return mNSigmaPion;}
inline float StChargedPionTrack::nSigmaKaon() const {return mNSigmaKaon;}
inline float StChargedPionTrack::nSigmaProton() const {return mNSigmaProton;}
inline float StChargedPionTrack::dEdx() const {return mdEdx;}

inline float StChargedPionTrack::chi2() const {return mChi2;}
inline float StChargedPionTrack::chi2prob() const {return mChi2Prob;}

inline float StChargedPionTrack::pt() const {return this->Pt();}
inline float StChargedPionTrack::phi() const {return this->Phi();}
inline float StChargedPionTrack::eta() const {return this->Eta();}

inline short StChargedPionTrack::charge() const {return mCharge;}
inline double StChargedPionTrack::B() const {return mB;}

inline StThreeVectorF StChargedPionTrack::globalLastPoint() const {return mGlobalLastPoint;}
inline StPhysicalHelixD StChargedPionTrack::globalHelix() const {return mGlobalHelix;}
inline StThreeVector<double> StChargedPionTrack::globalFirstPoint() const {return mGlobalHelix.origin();}

#endif

/*****************************************************************************
 * $Log: StChargedPionTrack.h,v $
 * Revision 1.6  2012/11/09 03:31:34  perev
 * Cleanup
 *
 * Revision 1.5  2008/12/29 15:58:31  kocolosk
 * removed commented code and added $Id: StChargedPionTrack.h,v 1.6 2012/11/09 03:31:34 perev Exp $/$Log: StChargedPionTrack.h,v $
 * removed commented code and added $Id$/Revision 1.6  2012/11/09 03:31:34  perev
 * removed commented code and added $Id$/Cleanup
 * removed commented code and added $Id$/ as needed
 *
 *****************************************************************************/
