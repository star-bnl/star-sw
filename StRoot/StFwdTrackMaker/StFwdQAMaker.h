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
#include "StThreeVectorD.hh"

#include <map>

class StMuFwdTrack;
class StMuFwdTrackProjection;
class ForwardTracker;
class StFwdTrack;
class StMuMcTrack;

/** @brief TClonesArray writer
 * Helper class for writing TClonesArrays to TTree of custom class type
 */
template<class BranchType>
class TClonesArrayWriter {
    public:
	TClonesArrayWriter() {}
	~TClonesArrayWriter() {}

	void createBranch( TTree *tree, const char* name, int buffSize = 256000, int splitLevel = 99){
        _tca = new TClonesArray( BranchType::Class_Name() );
		tree->Branch( name, &this->_tca, buffSize, splitLevel );
	}

	void add( BranchType &branch ){
		if ( nullptr == this->_tca ) return;
		BranchType *new_branch = new ((*this->_tca)[this->_n]) BranchType( );
        *new_branch = branch;
		this->_n++;
	}

	void add( BranchType *branch ){
		if ( nullptr == this->_tca || nullptr == branch) return;
		BranchType *new_branch = new ((*this->_tca)[this->_n]) BranchType( );
        *new_branch = *branch;
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

class StFcsDb;
class StFcsCluster;
class StFcsHit;
class StMuFcsCluster;
class StMuFcsHit;
class StMuFttCluster;
class StMuFttPoint;
class StMuFstHit;
class StMuFwdTrackSeedPoint;


/**
 * @brief Store Cluster with STAR XYZ position
 *
 */
class FcsClusterWithStarXYZ: public TObject {
    public:
    FcsClusterWithStarXYZ() {
        mXYZ.SetXYZ(0, 0, 0);
        mClu = nullptr;
    }
    FcsClusterWithStarXYZ( StMuFcsCluster *clu, StFcsDb *fcsDb );
    TVector3 mXYZ;
    StMuFcsCluster *mClu;
    ClassDef(FcsClusterWithStarXYZ, 1);
};

/**
 * @brief Store Hit with STAR XYZ position
 *
 */
class FcsHitWithStarXYZ: public TObject {
    public:
    FcsHitWithStarXYZ() {
        mXYZ.SetXYZ(0, 0, 0);
        mHit = nullptr;
    }
    FcsHitWithStarXYZ( StMuFcsHit *hit, StFcsDb *fcsDb );
    TVector3 mXYZ;
    StMuFcsHit *mHit;
    ClassDef(FcsHitWithStarXYZ, 1);
};


/** @brief
* This class is a container for the data that will be written to the output tree.
*/
struct FwdQATreeData {

    /** @brief Primary event vertex*/
    FwdTreeHeader header;
    /** @brief MC tracks */
    TClonesArrayWriter<StMuMcTrack> mcTracks;
    TClonesArrayWriter<StMuFttPoint> fttPoints;
    TClonesArrayWriter<StMuFttCluster> fttClusters;
    TClonesArrayWriter<StMuFstHit> fstPoints;

    TClonesArrayWriter<FcsClusterWithStarXYZ> wcal;
    TClonesArrayWriter<FcsHitWithStarXYZ> wcalHits;
    TClonesArrayWriter<FcsClusterWithStarXYZ> hcal;
    TClonesArrayWriter<FcsHitWithStarXYZ> hcalHits;
    TClonesArrayWriter<FcsHitWithStarXYZ> epdHits;
    TClonesArrayWriter<StMuFwdTrack> reco;

    int nSeedTracks;
    TClonesArrayWriter<StMuFwdTrackSeedPoint> seeds;


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
    void FillFstPoints();
    void FillFcsStEvent();
    void FillFcsStMuDst();
    void FillTracks();
    void FillMcTracks();

    void ProcessFwdTracks();
    void ProcessFwdMuTracks();

    void setMuDstInput() { mAnalyzeMuDst = true; }
    void setLocalOutputFile( TString f ) { mLocalOutputFile = f; }
    void setTreeFilename( TString f ) {mTreeFilename = f;}

  protected:
    TFile *mTreeFile = nullptr;
    TTree *mTree     = nullptr;
    FwdQATreeData mTreeData;

    StEvent *mStEvent = nullptr;
    StMuDstMaker *mMuDstMaker = nullptr;
    StMuDst *mMuDst = nullptr;
    StMuFwdTrackCollection * mMuForwardTrackCollection = nullptr;
    StMuFcsCollection *mMuFcsCollection = nullptr;
    StFwdTrackMaker *mFwdTrackMaker = nullptr;
    StFcsDb *mFcsDb = nullptr;


//========================================================= new stuff
    std::map<TString, TH1*> mHists;

    /**
     * @brief Get the Hist object from the map
     *  - Additional check and safety for missing histograms
     * @param n Histogram name
     * @return TH1* histogram if found, otherwise a 'nil' histogram with one bin
     */
    TH1* getHist( TString n ){
      if (mHists.count(n))
        return mHists[n];
      LOG_ERROR << "Attempting to access non-existing histogram" << endm;
      return new TH1F( "NULL", "NULL", 1, 0, 1 ); // returning nullptr can lead to seg fault, this fails softly
    }

    /**
     * @brief Control whether the analysis uses StEvent (default) or MuDst as input
     * 
     */
    bool mAnalyzeMuDst = false;
    TString mLocalOutputFile;
    TString mTreeFilename;
//====================================================== end new stuff

};


#endif
