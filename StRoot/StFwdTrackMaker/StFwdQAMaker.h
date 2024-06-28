#ifndef ST_FWD_TREE_MAKER_H
#define ST_FWD_TREE_MAKER_H

#include "TClonesArray.h"
#ifndef __CINT__
#include "GenFit/Track.h"
#include "StFwdTrackMaker/include/Tracker/FwdHit.h"
#include "StMuDSTMaker/COMMON/StMuFwdTrack.h"
#endif

#include "StChain/StMaker.h"
#include "TTree.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include "StEvent/StEnumerations.h"

class StMuFwdTrack;
class StMuFwdTrackProjection;
class ForwardTracker;
class StFwdTrack;

/** @brief TClonesArray writer
 * Helper class for writing TClonesArrays to TTree of custom class type
 */
template<class BranchType>
class TClonesArrayWriter {
    public:
	TClonesArrayWriter() {}
	~TClonesArrayWriter() {}

	void createBranch( TTree *tree, const char* name, int buffSize = 256000, int splitLevel = 99){
		_tca = new TClonesArray( BranchType().classname() );
		tree->Branch( name, &this->_tca, buffSize, splitLevel );
	}

	void add( BranchType &branch ){
		if ( nullptr == this->_tca ) return;
		BranchType *new_branch = new ((*this->_tca)[this->_n]) BranchType( );
		new_branch->copy( &branch );
		this->_n++;
	}

	void add( BranchType *branch ){
		if ( nullptr == this->_tca || nullptr == branch) return;
		BranchType *new_branch = new ((*this->_tca)[this->_n]) BranchType( );
		new_branch->copy( branch );
		this->_n++;
	}

	void reset(){
		this->_n = 0;
		if( nullptr != this->_tca )
			this->_tca->Clear();
	}

	UInt_t N() const { return _n; }
	BranchType *at( UInt_t i ){
		if ( nullptr == _tca )
			return nullptr;
		return (BranchType*)_tca->At( i );
	}

    protected:
	TClonesArray        * _tca = nullptr;
	UInt_t                _n   = 0;
};

class FwdTreeHeader : public TObject {
    public:
    FwdTreeHeader() : TObject() {
        run = 0;
        event = 0;
        tofmult = 0;
        vpdVz = -999;
        pv.SetXYZ(0, 0, 0);
    }

    void set( int r, int e, int t, TVector3 &p ){
        run = r;
        event = e;
        tofmult = t;
        pv = p;
    }

    void clear() {
        run = 0;
        event = 0;
        tofmult = 0;
        TVector3 pv(-999, -999, -999);
        vpdVz = -999;
    }

    TVector3 pv;
    int run, event, tofmult;
    float vpdVz;

    ClassDef(FwdTreeHeader, 1)
};

class FwdTreeFttCluster : public TObject {
    public:
    TString classname() { return "FwdTreeFttCluster"; }
    void clear(){
        TObject::Clear();
        pos.SetXYZ(-1, -1, -1);
        mPlane = 0;
        mQuadrant = 0;
        mRow = 0;
        mOrientation = kFttUnknownOrientation;
        mNStrips = 0;
        mSumAdc = 0.0;
        mX = 0.0;
        mSigma = 0.0;
    }

    void copy( FwdTreeFttCluster *cluster ){
        pos = cluster->pos;
        mPlane = cluster->mPlane;
        mQuadrant = cluster->mQuadrant;
        mRow = cluster->mRow;
        mOrientation = cluster->mOrientation;
        mNStrips = cluster->mNStrips;
        mSumAdc = cluster->mSumAdc;
        mX = cluster->mX;
        mSigma = cluster->mSigma;
    }

    TVector3 pos;
    UChar_t mId = 0;
    UChar_t mPlane = 0;
    UChar_t mQuadrant = 0;
    UChar_t mRow = 0;
    UChar_t mOrientation = kFttUnknownOrientation;        // Orientation of cluster
    Int_t mNStrips=0;         // Number of strips
    Float_t mSumAdc=0.0;      // Total ADC (0th moment)
    Float_t mX=0.0;             // Mean x ("center of gravity") in local grid coordinate (1st moment)
    Float_t mSigma=0.0;

    ClassDef(FwdTreeFttCluster, 1);
};

class FwdTreeHit : public TObject {
    public:
    TString classname() { return "FwdTreeHit"; }
    FwdTreeHit() : TObject() {
        pos.SetXYZ(-1, -1, -1);
        id = 0;
        vol = 0;
        det = 0;
        trackId = -1;
    }
    FwdTreeHit(float x, float y, float z, int v, int d, int trkId = -1) : TObject() {
        pos.SetXYZ(x, y, z);
        id = 0;
        vol = v;
        det = d;
        trackId = trkId;
    }

    void set(float x, float y, float z, int v, int d, int trkId = -1) {
        pos.SetXYZ(x, y, z);
        id = 0;
        vol = v;
        det = d;
        trackId = trkId;
    }

    int id, vol, det, trackId;
    TVector3 pos;

    void copy( FwdTreeHit *hit ){
        id = hit->id;
        vol = hit->vol;
        det = hit->det;
        pos = hit->pos;
        trackId = hit->trackId;
    }

    ClassDef(FwdTreeHit, 2)
};

class FwdTreeTrackProjection : public TObject {
    public:
    FwdTreeTrackProjection() {}
    FwdTreeTrackProjection(   unsigned short detId, 
                            TVector3 xyz, 
                            TVector3 mom, 
                            float c[9] ) {
        set( detId, xyz, mom, c );
    }

    void set(   unsigned short detId, 
                TVector3 xyz, 
                TVector3 mom, 
                float c[9]) {
        mDetId = detId;
        mXYZ = xyz;
        mMom = mom;
        memcpy( mCov, c, sizeof(mCov) ); 
    }
    void copy(   FwdTreeTrackProjection *other ){
        mDetId = other->mDetId;
        mXYZ   = other->mXYZ;
        mMom   = other->mMom;
        memcpy( mCov, other->mCov, sizeof(mCov) ); 
    }
    TVector3 mXYZ;
	TVector3 mMom;
    unsigned char mDetId;
    float mCov[9];

    float dx(){
        return sqrt( mCov[0] );
    }
    float dy(){
        return sqrt( mCov[4] );
    }
    float dz(){
        return sqrt( mCov[8] );
    }

    ClassDef(FwdTreeTrackProjection, 1)
};

class FwdTreeRecoTrack : public TObject {
    public:
    TString classname() { return "FwdTreeRecoTrack"; }
    FwdTreeRecoTrack() : TObject() {
        id = 0;
        q = 0;
        status = 0;
        mom.SetXYZ(0, 0, 0);
        projs.clear();
    }

    virtual void Clear(){
        TObject::Clear();
        seeds.clear();
        projs.clear();
    }

    void set( StMuFwdTrack *muFwdTrack );
    void set( StFwdTrack *fwdTrack );

    /**
    * Copies the values of the given FwdTreeRecoTrack object to the current object.
    *
    * @param hit The FwdTreeRecoTrack object to copy from.
    */
    void copy( FwdTreeRecoTrack *hit ){
        id = hit->id;
        q = hit->q;
        status = hit->status;
        mom = hit->mom;
        seeds = hit->seeds;
        mChi2 = hit->mChi2;
        projs = hit->projs;
        nFailedPoints = hit->nFailedPoints;
    }

    int id, q, status;
    int nFailedPoints;
    float mChi2;
    TVector3 mom;
    vector<FwdTreeTrackProjection> projs;
    vector<FwdTreeHit> seeds;
    
    ClassDef(FwdTreeRecoTrack, 4);
};

class StFcsDb;
class StFcsCluster;
class StMuFcsCluster;
class FwdTreeFcsCluster : public TObject {
    public:
    TString classname() { return "FwdTreeFcsCluster"; }
    void copy( FwdTreeFcsCluster * clu ){
        mId = clu->mId;
        mDetectorId = clu->mDetectorId;
        mCategory = clu->mCategory;
        mNTowers = clu->mNTowers;
        mEnergy = clu->mEnergy;
        mX = clu->mX;
        mY = clu->mY;
        mSigmaMin = clu->mSigmaMin;
        mSigmaMax = clu->mSigmaMax;
        mTheta = clu->mTheta;
        mChi2Ndf1Photon = clu->mChi2Ndf1Photon;
        mChi2Ndf2Photon = clu->mChi2Ndf2Photon;
        mFourMomentum = clu->mFourMomentum;
        pos = clu->pos;
    }

    void set( StFcsCluster *clu, StFcsDb* fcsDb );
    void set( StMuFcsCluster *clu, StFcsDb* fcsDb );

    Int_t mId=-1;             // Eventwise cluster ID
    UShort_t mDetectorId=0;   // Detector starts from 1
    Int_t mCategory=0;        // Category of cluster (see StFcsClusterCategory)
    Int_t mNTowers=0;         // Number of non-zero-energy tower hits in the cluster
    Float_t mEnergy=0.0;      // Total energy contained in this cluster (0th moment)
    Float_t mX=0.0;  // Mean x ("center of gravity") in local grid coordinate (1st moment)
    Float_t mY=0.0;  // Mean y ("center of gravity") in local grid coordinate (1st moment)
    Float_t mSigmaMin=0.0;        // Minimum 2nd moment
    Float_t mSigmaMax=0.0;        // Maximum 2nd moment (along major axis)
    Float_t mTheta=0.0;           //Angle in x-y plane that defines the direction of least-2nd-sigma
    Float_t mChi2Ndf1Photon=0.0;  // &chi;<sup>2</sup> / ndf for 1-photon fit
    Float_t mChi2Ndf2Photon=0.0;  // &chi;<sup>2</sup> / ndf for 2-photon fit
    TLorentzVector mFourMomentum;  // Cluster four momentum
    TVector3 pos;                   // STAR XYZ position

    ClassDef(FwdTreeFcsCluster, 1);
};

class FwdTreeMonteCarloTrack : public TObject {
    public:

    FwdTreeMonteCarloTrack() : TObject() {
        id = 0;
        q = 0;
        status = 0;
        mom.SetXYZ(0, 0, 0);
    }

    int id, q, status;
    TVector3 mom;

    ClassDef(FwdTreeMonteCarloTrack, 1);
};

/** @brief
* This class is a container for the data that will be written to the output tree.
*/
struct FwdTreeData {

    /** @brief Primary event vertex*/
    FwdTreeHeader header;
    TClonesArrayWriter<FwdTreeHit> ftt;
    // TClonesArrayWriter<FwdTreeHit> fttClusters;
    TClonesArrayWriter<FwdTreeFttCluster> fttClusters;
    TClonesArrayWriter<FwdTreeHit> fst;

    TClonesArrayWriter<FwdTreeFcsCluster> wcal;
    TClonesArrayWriter<FwdTreeFcsCluster> hcal;

    TClonesArrayWriter<FwdTreeRecoTrack> reco;

    int nSeedTracks;
    TClonesArrayWriter<FwdTreeHit> seeds;


    void clear();
};


class StMuDstMaker;
class StMuDst;
class StMuFwdTrackCollection;
class StMuFcsCollection;
class StFwdTrackMaker;
class StEvent;

class StFwdQAMaker : public StMaker {

    ClassDef(StFwdQAMaker, 0);

  public:
    StFwdQAMaker();
    ~StFwdQAMaker(){/* nada */};

    int Init();
    int Finish();
    int Make();
    void Clear(const Option_t *opts = "");

    void FillFttClusters();
    void FillFcsStEvent();
    void FillFcsStMuDst();

  protected:
    TFile *mTreeFile = nullptr;
    TTree *mTree     = nullptr;
    FwdTreeData mTreeData;

    StEvent *mStEvent = nullptr;
    StMuDstMaker *mMuDstMaker = nullptr;
    StMuDst *mMuDst = nullptr;
    StMuFwdTrackCollection * mMuForwardTrackCollection = nullptr;
    StMuFcsCollection *mMuFcsCollection = nullptr;
    StFwdTrackMaker *mFwdTrackMaker = nullptr;
    StFcsDb *mFcsDb = nullptr;

};


#endif
