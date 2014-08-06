////////////////////////////////////////////////////////////
//                                                        //
//    StGammaEvent                                        //
//                                                        //
//    Original concept and implementation by Jason        //
//    Webb (Valpo)                                        //
//                                                        //
////////////////////////////////////////////////////////////

#ifndef STAR_StGammaEvent
#define STAR_StGammaEvent

#include <set>

#include "TObject.h"
#include "TVector3.h"

#include "StGammaCandidate.h"
#include "StGammaCandidateMaker.h"

#include "StGammaTrack.h"
#include "StGammaTower.h"
#include "StGammaStrip.h"

#include "TClonesArray.h"
#include "TObjString.h"

class StMuTrack;
class StGammaPythiaEvent;

#define TPC_VERTEX 0x0001

class StGammaEvent: public TObject 
{

    public:
    
        enum Spin4State { kBlueUpYellUp=5, kBlueUpYellDn=6, kBlueDnYellUp=9, kBlueDnYellDn=10, kUnknownSpin4=42 };
        enum PolarizationType { kUnpolarized=0, kLongLong, kTransTrans, kRadRad, kUnknownPol=42 };
        // see http://www.star.bnl.gov/protected/spin/balewski/2005-spinDB/definitions/
    
        StGammaEvent();
        ~StGammaEvent();
        
        virtual const char* GetCVS() const
        {static const char cvs[] = "Tag $Name:  $ $Id: StGammaEvent.h,v 1.16 2014/08/06 11:43:18 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs; }
        
        void Clear(Option_t *opts= "");
        
        UShort_t mFlags;  /// Event flags (see above)
        UShort_t flags() const { return mFlags; }
        
        //////////////////////////////////////////////////
        //                   Accessors                  //
        //////////////////////////////////////////////////
        
        Int_t numberOfTracks() const { return mTracks->GetEntriesFast(); }         /// Return number of tracks
        Int_t numberOfTowers() const { return mTowers->GetEntriesFast(); }         /// Return number of towers
        Int_t numberOfPreshower1() const { return mPreshower1->GetEntriesFast(); } /// Return number of pre1
        Int_t numberOfPreshower2() const { return mPreshower2->GetEntriesFast(); } /// Return number of pre2
        Int_t numberOfPostshower() const { return mPostshower->GetEntriesFast(); } /// Return number of post
        Int_t numberOfStrips() const { return mStrips->GetEntriesFast(); }         /// Return number of strips
        Int_t numberOfCandidates() const { return mCandidates->GetEntriesFast(); } /// Return number of candidates
        
        StGammaTrack *track( Int_t i ) const { return (StGammaTrack*)mTracks->At(i); }                 /// Return ith track
        StGammaTower *tower( Int_t i ) const { return (StGammaTower*)mTowers->At(i); }                 /// Return ith tower
        StGammaTower *preshower1( Int_t i ) const { return (StGammaTower*)mPreshower1->At(i); }        /// Return ith pre1
        StGammaTower *preshower2( Int_t i ) const { return (StGammaTower*)mPreshower2->At(i); }        /// Return ith pre2
        StGammaTower *postshower( Int_t i ) const { return (StGammaTower*)mPostshower->At(i); }        /// Return ith post
        StGammaStrip *strip( Int_t i ) const { return (StGammaStrip*)mStrips->At(i); }                 /// Return ith strip
        StGammaCandidate *candidate( Int_t i ) const { return (StGammaCandidate*)mCandidates->At(i); } /// Return ith candidate
        
        Int_t runNumber() const { return mRunNumber; }              /// Returns run number
        Int_t eventNumber() const { return mEventNumber; }          /// Returns event number
        set<int>& triggerIds() { return mTriggerIds; }
        bool isTrigger(int id) const { return mTriggerIds.find(id) != mTriggerIds.end(); }
        bool isSimuTrigger(int id) const { return mSimuTriggerIds.find(id) != mSimuTriggerIds.end(); }
        TObjString muDstFileName() const { return mMudstFileName; } /// Returns muDst file from which event originated
        TVector3& vertex(){ return mVertex; }                       /// Returns the primary vertex
        Float_t vertexRank() { return mVertexRank; }                /// Returns rank of primary vertex
        Float_t magneticField() const { return mMagneticField; }    /// Magnetic field (kG)
        StGammaPythiaEvent* pythia() { return mPythia; }            /// Pythia event

        Bool_t validSpinDb() { return mSpinDbValid; }
        UShort_t spin4() { return mSpin4; }
        UShort_t bunchCrossing7bit() { return mBunchCrossing7bit; }
        UShort_t bunchCrossing48bit() { return mBunchCrossing48bit; }
        UShort_t bunchCrossingStar() { return mBunchCrossingStar; }
        UShort_t polarizationType() { return mPolarizationType; }        
        
        Float_t sumPt( Float_t eta_min = -2.5, Float_t eta_max = +2.5 ) const;     /// Returns track+tower pT in eta range
        Float_t sumTrackPt(Float_t eta_min = -2.5, Float_t eta_max = +2.5 ) const; /// Returns track pT in eta range
        Float_t sumTowerPt(Float_t eta_min = -2.5, Float_t eta_max = +2.5 ) const; /// Returns tower pT in eta range
        
        //////////////////////////////////////////////////
        //                   Mutators                   //
        //////////////////////////////////////////////////
        
        StGammaTrack *newTrack( StMuTrack *mutr=0 ); /// Add a new track
        StGammaTower *newTower();                    /// Add a new tower
        StGammaTower *newPreshower1();               /// Add a new preshower1 (bprs) element
        StGammaTower *newPreshower2();               /// Add a new preshower2 element
        StGammaTower *newPostshower();               /// Add a new postshower element
        StGammaStrip *newStrip();                    /// Add a new SMD strip
        StGammaCandidate *newCandidate();            /// Add a new gamma candidate
        
        void SetRunNumber( Int_t run ){ mRunNumber=run; }
        void SetEventNumber( Int_t event ){ mEventNumber=event; }
        void SetTriggerIds(const vector<unsigned int>& triggerIds) { copy(triggerIds.begin(), triggerIds.end(), inserter(mTriggerIds, mTriggerIds.begin())); }
        void SetSimuTriggerIds(const vector<unsigned int>& triggerIds) { copy(triggerIds.begin(), triggerIds.end(), inserter(mSimuTriggerIds, mSimuTriggerIds.begin())); }
        void SetMudstFileName(const TObjString &i) { mMudstFileName = i; }
        void SetVertex(const TVector3& vertex ){ mVertex=vertex; }
        void SetVertexRank(Float_t rank) { mVertexRank = rank; }
        void SetMagneticField( Float_t magneticField) { mMagneticField = magneticField; }
        void SetPythia(StGammaPythiaEvent* pythia) { mPythia = pythia; }
        
        void SetValidDb( Bool_t v ){ mSpinDbValid=v; }
        void SetSpin4( UShort_t s ){ mSpin4=s; }
        void SetBunchCrossing7bit( UShort_t b ){ mBunchCrossing7bit=b; }
        void SetBunchCrossing48bit( UShort_t b ){ mBunchCrossing48bit=b; }
        void SetBunchCrossingStar( UShort_t b ){ mBunchCrossingStar=b; }
        void SetPolarizationType( UShort_t t ){ mPolarizationType=t; }
        void SetDsmVertex( UShort_t v ){ mDsmVertex=v; }

    protected:
    
        Int_t mRunNumber;            /// Run number
        Int_t mEventNumber;          /// Event number  
        set<int> mTriggerIds;        /// Trigger ID's
        set<int> mSimuTriggerIds;    /// Simulated Trigger ID's        
        TObjString mMudstFileName;   /// File from which StGammaEvent originatedt
        TVector3 mVertex;            /// Event primary vertex (TPC)
        Float_t mVertexRank;         /// Primary vertex rank
        Float_t mMagneticField;      /// Magnetic field (kG)
        StGammaPythiaEvent* mPythia;

        Bool_t    mSpinDbValid;          
        UShort_t  mSpin4;
        UShort_t  mBunchCrossing7bit;
        UShort_t  mBunchCrossing48bit;
        UShort_t  mBunchCrossingStar;
        UShort_t  mPolarizationType;
        
        UShort_t  mDsmVertex;

    private:
    
        Int_t InitArrays();
        
        TClonesArray *mTracks;     //-> array of all tracks
        TClonesArray *mTowers;     //-> array of all towers
        TClonesArray *mPreshower1; //-> array of all preshower1
        TClonesArray *mPreshower2; //-> array of all preshower2
        TClonesArray *mPostshower; //-> array of all postshower
        TClonesArray *mStrips;     //-> array of all strips
        TClonesArray *mCandidates; //-> array of all candidates
        
        friend Int_t StGammaCandidateMaker::Compress();
        
        ClassDef(StGammaEvent, 1);

};

#endif
