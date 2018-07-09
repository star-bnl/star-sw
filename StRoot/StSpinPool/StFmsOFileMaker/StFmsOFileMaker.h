#ifndef STAR_StFmsOFileMaker_HH
#define STAR_StFmsOFileMaker_HH


/* StFmsOFileMaker.h
 *
 * This maker produces "OFiles" from MuDSTs, which contain trees compatible with the
 * Penn State University "root12fms" FMS calibration and analysis software
 *
 * Author: Christopher Dilks 
 *
 */ 

#include "StMaker.h"
#include "StLorentzVectorF.hh"
#include "StThreeVectorF.hh"
#include "TMatrix.h"

class StMuDst;
class StMuEvent;
class StEvent;
class StMuDstMaker;
class StFmsDbMaker;
class StSpinDbMaker;

class StFmsCollection;
class StMuFmsCollection;
//class StMuRpsCollection; // deprecated (no afterburner)
class StMuRpsCollection2; // uses afterburner instead
class StMuRpsUtil;

class StMuFmsCluster;
class StMuFmsPoint;
class StMuFmsHit;
class StFmsCluster;
class StFmsPoint;
class StFmsHit;
class StFmsPointPair;

class StMuRpsTrack;
class StMuRpsTrackPoint;

//class StDAQReader;

class TClonesArray;


#ifndef CONST_DEFINED
#define CONST_DEFINED

// FMS constants
const Int_t NCELLS = 1264; // total number of FMS cells
const Int_t NROWS[4] = { 34, 34, 24, 24 }; // number of rows (for each nstb [0-3])
const Int_t NCOLS[4] = { 17, 17, 12, 12 }; // number of columns
const Float_t CELL_WIDTH[4] = { 5.812, 5.812, 3.822, 3.822 }; // cell width in cm

// root12fms/FpdRoot::Geom constants
// (note to self: see heppellab16f:~/15tran/root12fms/PRINT_GEOM_TABLE.C)
const Float_t zPos[4] = { 734.099976,
                          734.099976,
                          729.700012,
                          729.700012 }; // approximate z-position of each nstb's shower max
const Float_t xOffset[4] = { -0.30,
                              0.30,
                             -0.93,
                              0.93 }; // x offset
const Float_t yOffset[4] = { 98.8,
                             98.8,
                             46.5,
                             46.5 }; // y offset


// RP constants
const Int_t N_TRACKS_MAX = 1000; // max number of tracks
const Double_t thetaRpLimits[2][2] = { {-1.5e-3, 5.0e-3},
                                       { 1.0e-3, 5.5e-3} }; // [rad]; theta limits from rafal

#endif

// =============================================================================================

class StFmsOFileMaker : public StMaker{

  public: 
    StFmsOFileMaker(StMuDstMaker * maker, const Char_t* name="FmsOFile");
    ~StFmsOFileMaker();

    // chain methods
    Int_t Init();
    Int_t Make();
    Int_t Finish();

    // modifiers
    void setFileName(char* file) { OFilename=file; };
    void setPrint(int v) { mPrint=v; };

    // accessors
    static Int_t getNTracksMax() { return N_TRACKS_MAX; };

    // resetters
    void ResetRpTrackVars(Int_t i_);
    void ResetRpTrackPointVars(Int_t i_);
    void ResetFmsBranchVars();

    // control switches
    Bool_t build_evtr;
    Bool_t verbose,verbose_clu,verbose_pho,verbose_rp;
    Bool_t check_spinbyte;
    Bool_t dump_spinbits;


  // -------------------------------------------------------------------------------------------------
  private:

    // event and collection pointers =================================================================

    // DBs
    StSpinDbMaker * mSpinDb;
    StFmsDbMaker * mFmsDb;

    // collections
    //StMuFmsCollection * muFmsColl; // deprecated -- using StFmsCollection instead
    //StMuRpsCollection * mMuRpsColl; // deprecated -- associated with pre-afterburner data
    StMuRpsUtil * mAfterburner; // afterburner (RP post-processing)
    StMuRpsCollection2 * mMuRpsColl; // "2" means it's associated with afterburner result
    StFmsCollection * dsFmsColl;

    // events
    StMuDst * muDst;
    StMuEvent * muEvent;
    StEvent * dsEvent;

    // StMuEvent event object and object arrays
    TClonesArray * muHits;
    TClonesArray * muClusters;
    TObjArray * clusterArr[4];
    TClonesArray * muPoints;
    StMuFmsCluster * muclu;
    StMuFmsHit * muhit;
    StMuRpsTrack * trk;
    const StMuRpsTrackPoint * trkpnt;

    // StEvent event objects
    StFmsCluster * clu;
    StFmsPoint * pho;
    StFmsHit * hit;
    StFmsPointPair * pair;


    // general variables for all trees ===============================================================
    UInt_t bc[2]; // bxing counter [0=lowBits,1=highBits]
    Int_t evid; // event id
    Long64_t BunchL; // bxing counter (same as bc, but it's a 'long int' instead)


    // evtr tree =====================================================================================
    // easy-to-understand, but very big tree, useful for things like an event display
    // use option "build_evtr" to enable/disable
    TTree * evtr;

    Int_t nhits,nclusters,npoints,npairs; // number of hits, clusters, points, pairs (according to collections)
    Int_t hc,cc,pc,ppc; // hit/cluster/point/pair counter (which may be less than nhits/nclusters/npoints)

    // evtr branches for hits
    unsigned short hit_nstb[NCELLS];
    unsigned short hit_chan[NCELLS];
    Int_t hit_row[NCELLS];
    Int_t hit_col[NCELLS];
    unsigned short hit_adc[NCELLS];
    Float_t hit_en[NCELLS];
    Float_t gain[NCELLS];
    Float_t gainCorr[NCELLS];
    Short_t bitshift[NCELLS];
    unsigned short adcCorrected[NCELLS];
    unsigned short hit_tdc[NCELLS];
    //float timeDepCorr[NCELLS];

    // evtr branches for clusters
    Int_t clu_id[NCELLS];
    Float_t clu_en[NCELLS];
    Float_t clu_x[NCELLS];
    Float_t clu_y[NCELLS];
    Float_t clu_sigmin[NCELLS];
    Float_t clu_sigmax[NCELLS];
    Float_t clu_csqn1[NCELLS];
    Float_t clu_csqn2[NCELLS];
    Int_t clu_category[NCELLS];
    Int_t clu_ntowers[NCELLS];
    Int_t clu_nphotons[NCELLS];
    unsigned short clu_nstb[NCELLS];
    StThreeVectorF clu_pos;
    //Float_t clu_enFromHits[NCELLS];
    //Float_t clu_enFromPoints[NCELLS];

    // evtr branches for points
    Int_t pho_id[NCELLS];
    Float_t pho_en[NCELLS];
    Float_t pho_x[NCELLS];
    Float_t pho_y[NCELLS];
    Float_t pho_px[NCELLS];
    Float_t pho_py[NCELLS];
    Float_t pho_pz[NCELLS];
    unsigned short pho_nstb[NCELLS];
    StThreeVectorF pho_pos;
    StLorentzVectorF pho_mom;
    Int_t pho_fpsPid[NCELLS];

    // evtr branchs for point pairs
    Float_t pair_en[NCELLS];
    Float_t pair_pt[NCELLS];
    Float_t pair_eta[NCELLS];
    Float_t pair_phi[NCELLS];
    Float_t pair_mass[NCELLS];
    Float_t pair_dgg[NCELLS];
    Float_t pair_zgg[NCELLS];
    Float_t pair_x[NCELLS];
    Float_t pair_y[NCELLS];
    Float_t pair_coneRad[3][NCELLS]; // [0=100mrad, 1=70mrad, 2=30mrad]
    Float_t pair_coneEn[3][NCELLS]; // [0=100mrad, 1=70mrad, 2=30mrad]
    Float_t pair_coneEnFrac[3][NCELLS]; // [0=100mrad, 1=70mrad, 2=30mrad]
    UInt_t pair_fpsPid[NCELLS];
   


    // OFile p_out tree =======================================================================
    // somewhat smaller tree, compatible with FMS root12fms code
    // this is the primary tree stored in an OFile

    TTree * p_out;

    // p_out event-level branches
    Int_t spin2bit; 
    Int_t nwrds; 
    Int_t Rnum,Rnum_tmp;
    Int_t Bunchid7bit; 
    Int_t EventN; 
    Int_t ievt; 
    UInt_t L2sum[2]; 
    UInt_t lastdsm[8]; 
    Int_t TrigBits; 

    // p_out FMS branches
    Int_t nphotons; 
    Int_t tpes[NCELLS]; 
    Float_t pxyzt[NCELLS]; 
    Int_t nSavedHits; 
    UInt_t SavedHits[NCELLS]; 
    UChar_t SavedTDC[NCELLS];
    Float_t SavedCluTDC[NCELLS];
    Int_t nCluster; 
    Int_t nPhotonClu; 
    Int_t SCIndex[NCELLS]; 
    Int_t SPCIndex[NCELLS]; 
    Float_t SPCEnergy[NCELLS]; 

    // p_out other (probably unused) branches
    UInt_t Fpde[8]; 
    UChar_t BBcSums[5]; 
    Float_t BBcVertex[7]; 

    // p_out FPS branches
    Int_t fpsPid[NCELLS];

    // p_out RP branches
    Int_t n_tracks,n_trackpoints;
    Int_t tc; // track counter
    Int_t tpc[N_TRACKS_MAX]; // trackpoint counter
    // -- track variables (prefixed with t_)
    Int_t t_index[N_TRACKS_MAX];
    Int_t t_branch[N_TRACKS_MAX];
    Int_t t_type[N_TRACKS_MAX];
    UInt_t t_planesUsed[N_TRACKS_MAX];
    Double_t t_p[N_TRACKS_MAX];
    Double_t t_pt[N_TRACKS_MAX];
    Double_t t_eta[N_TRACKS_MAX];
    Double_t t_time[N_TRACKS_MAX];
    Double_t t_theta[N_TRACKS_MAX][3]; // [track] [angle (X,Y,full)]
    Double_t t_thetaRP[N_TRACKS_MAX][3]; // [track] [angle (X,Y,full)]
    Double_t t_phi[N_TRACKS_MAX];
    Double_t t_phiRP[N_TRACKS_MAX];
    Double_t t_t[N_TRACKS_MAX];
    Double_t t_xi[N_TRACKS_MAX];
    Bool_t t_isBad[N_TRACKS_MAX];
    Double_t t_qualHash[N_TRACKS_MAX];
    // -- trackpoint variables (prefixed with p_) 
    Bool_t p_tpExists[N_TRACKS_MAX][2]; // [0 for first trackpoint, 1 for second trackpoint]
    Int_t p_RPid[N_TRACKS_MAX][2];
    Int_t p_clustid_s1[N_TRACKS_MAX][2]; 
    Int_t p_clustid_s2[N_TRACKS_MAX][2]; 
    Int_t p_clustid_s3[N_TRACKS_MAX][2]; 
    Int_t p_clustid_s4[N_TRACKS_MAX][2]; 
    Int_t p_quality[N_TRACKS_MAX][2];
    UInt_t p_planesUsed[N_TRACKS_MAX][2];
    Double_t p_x[N_TRACKS_MAX][2];
    Double_t p_y[N_TRACKS_MAX][2];
    Double_t p_z[N_TRACKS_MAX][2];
    Double_t p_time_pmt1[N_TRACKS_MAX][2];
    Double_t p_time_pmt2[N_TRACKS_MAX][2];

    // p_out misc branches for activity in other detectors
    unsigned short bbcADCSum[2];
    unsigned short bbcADCSumLarge[2];
    unsigned short bbcEarliestTDC[2];
    unsigned short bbcEarliestTDCLarge[2];
    unsigned short zdcADCSum[2];
    unsigned short vpdADCSum[2];
    unsigned short tofMultiplicity;
    //unsigned short mtdADC[2];


    
    // other variables ========================================================

    char * OFilename;
    TFile * OFile;
    int mPrint;
    unsigned long long L2sum_full;
    //StDAQReader * daqReader;

    // spin QA vars
    Int_t spin4bit;
    Int_t bNib,yNib;
    Int_t spinbyte,spinbyte_db;
    Int_t spinbyte_curr,spinbyte_db_curr;
    Bool_t spinbyte_same;
    char spc_b,spc_y;
    Int_t sp_b,sp_y;
    Int_t spinFromSpinbyte;

    // FMS vars
    unsigned short detId;
    unsigned short nstb;
    unsigned short hitADC;
    unsigned short hitTDC;
    Float_t cluSumTDC,cluAveTDC;
    Int_t cluHitCount;
    int hit_r,hit_c,hit_n;
    Float_t cluEn;
    Int_t ent[4];
    Float_t en_sum_all;
    Float_t en_sum[4];
    Float_t clu_en_sortval[NCELLS];
    Float_t EnergySum,Energy;
    Int_t clu_en_sortind[NCELLS];
    StLorentzVectorF vecs[NCELLS];
    Int_t num_null;
    TMatrix * eMat[4]; // [nstb]
    unsigned int p1,p2,p3,p4,pa;

    // RP vars
    //Int_t trkSortIdx[N_TRACKS_MAX];
    //Double_t trkSortQual[N_TRACKS_MAX];
    Double_t trkMom;
    Int_t trkPlanes;
    Int_t trkGlobal;
    Int_t trkInGeom;
    Double_t trkThetaX;
    Double_t trkThetaY;


    virtual const char * GetCVS() const
    {
      static const char cvs[] = "Tag  " __DATE__ " " __TIME__ ;
      return cvs;
    };

    ClassDef(StFmsOFileMaker,1);
};

#endif
