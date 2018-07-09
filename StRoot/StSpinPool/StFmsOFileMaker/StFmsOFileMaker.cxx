/* StFmsOFileMaker.cxx
 *
 * This maker produces "OFiles" from MuDSTs, which contain trees compatible with the
 * Penn State University "root12fms" FMS calibration and analysis software
 *
 * Author: Christopher Dilks 
 *
 */ 
 

#include "StFmsOFileMaker.h"

// DB headers
#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"
#include "StFmsDbMaker/StFmsDbMaker.h"

// StMuEvent headers
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuFmsCollection.h"
#include "StMuDSTMaker/COMMON/StMuFmsCluster.h"
#include "StMuDSTMaker/COMMON/StMuFmsHit.h"
#include "StMuDSTMaker/COMMON/StMuFmsPoint.h"
#include "StMuDSTMaker/COMMON/StMuRpsCollection.h"
#include "StMuDSTMaker/COMMON/StMuRpsTrackPoint.h"
#include "StMuDSTMaker/COMMON/StMuRpsTrack.h"

// StEvent headers
#include "StEvent/StEvent.h"
#include "StEvent/StFmsCollection.h"
#include "StEvent/StFmsHit.h"
#include "StEvent/StFmsCluster.h"
#include "StEvent/StFmsPoint.h"
#include "StEvent/StFmsPointPair.h"
#include "StEvent/StTriggerData.h"
#include "StEvent/StTriggerDetectorCollection.h"
#include "StEvent/StTriggerId.h"
#include "StEvent/StBbcTriggerDetector.h"

// RP afterburner
#include "StMuRpsUtil/StMuRpsUtil.h"
#include "StMuRpsUtil/StMuRpsCollection2.h"

// ROOT headers
#include "TFile.h"
#include "TTree.h"
#include "TObjArray.h"
#include "TMath.h"
#include "TVector3.h"
#include "TLorentzVector.h"

// other
#include "StMessMgr.h"
#include "Stypes.h"
#include "StEventTypes.h"
#include "StEnumerations.h"
#include <bitset>
//#include "StDAQMaker/StDAQReader.h"


ClassImp(StFmsOFileMaker);


//########################################################################


// CONSTRUCTOR
StFmsOFileMaker::StFmsOFileMaker(StMuDstMaker * maker, const Char_t* name) :
  StMaker(name),OFilename((char *)"FmsOFile.root"),mPrint(1) {
    LOG_INFO << "[++++++] begin StFmsOFileMaker construction..." << endm;


    // CONTROL SWITCHES <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    // default values set here, but you can override them in your macro which builds/executes your chain
    
    // logging switches
    dump_spinbits = true; // if true, dumps spin pattern during first event
    verbose = false; // if true, prints out event-by-event, cluster-by-cluster, hit-by-hit info
    verbose_clu = true; // if true, prints out kinematics for each cluster 
                        // (if !verbose, then verbose_clu forced to be false)
    verbose_pho = true; // if true, prints out kinematics for each photon along with 
                        // types[nwrds] and pxyzt[nwrds] (if !verbose, forced to be false)
    verbose_rp = false; // RP diagnostics

    // check spinbyte (if true, does quasi-spin-QA by checking spin info from v124 bits)
    check_spinbyte = false;

    // build extra trees
    build_evtr = true; // if true, builds evtr

    // <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    // force verbose_clu and verbose_pho to be false if !verbose
    if(!verbose) {
      verbose_clu = false;
      verbose_pho = false;
    };


    mAfterburner = new StMuRpsUtil(maker); // RP afterburner
    for(int nn=0; nn<4; nn++) eMat[nn] = new TMatrix(NROWS[nn],NCOLS[nn]); // FMS energy matrices
    Rnum_tmp = 0; // (for checking if runnumber changed (highly unlikely))

    LOG_INFO << "[++++++] ....constructed." << endm;
};



// DESTRUCTOR
StFmsOFileMaker::~StFmsOFileMaker() { /* no-op */ };


//########################################################################


Int_t StFmsOFileMaker::Init() {  

  // load databases
  mFmsDb = static_cast<StFmsDbMaker*>(GetMaker("fmsDb"));  
  mSpinDb = static_cast<StSpinDbMaker*>(GetMaker("spinDb"));  
  if(!mFmsDb){
    LOG_ERROR  << "StFmsOFileMaker::InitRun Failed to get mFmsDb" << endm;
    return kStFatal;
  }    
  if(!mSpinDb){
    LOG_ERROR  << "StFmsOFileMaker::InitRun Failed to get mSpinDb" << endm;
    return kStFatal;
  }    


  // BEGIN TEST CODE
  /*
  St_DataSet * daqReaderDS = GetDataSet("StDAQReader");
  printf("TEST ---- daqReaderDS @ %p\n",(void*)daqReaderDS);
  daqReader = (StDAQReader*)(daqReaderDS->GetObject());
  printf("TEST ---- daqReader @ %p\n",(void*)daqReader);
  */
  // END TEST CODE


  // instantiate "OFile" (basically a form of "picoDST" or "femptoDST" produced from muDST)
  OFile = new TFile(OFilename,"RECREATE");


  // BEGIN DEFINING TREES ///////////////////////////////////////////////////////////////////////////


  // OFile p_out tree ================================================================================
  // this is the primary tree stored in an OFile
  p_out = new TTree("p_out","p_out");

 
  // event-level branches = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
  p_out->Branch("spin",&spin2bit,"spin/I"); // spin of protons in bXing
                                        // (0=--, 1=-+, 2=+-, 3=++ (notation (B)(Y)), 40=otherwise)
  p_out->Branch("br_Rnum",&Rnum,"Rnum/I"); // run number 
  p_out->Branch("br_Bunchid7bit",&Bunchid7bit,"Bunchid7bit/I"); // bXing number
  p_out->Branch("br_BunchL",&BunchL,"BunchL/L"); // long int version of bc, which supposedly serves as
                                                 // a unique event ID
  p_out->Branch("br_ievt",&ievt,"ievt/I"); // event number (assuming each segment has 10,000 events)


  // FMS branches = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
  p_out->Branch("nphotons",&nphotons,"nphotons/I"); //  number of photons found by yiqun
  p_out->Branch("br_nwrds",&nwrds,"nwrds/I"); // number of "words"
                                                // incremented by 1 for each NSTB which passes
                                                // "esum-cut" incremented also by 1 for each photon
                                                // found finally, multiplied by 4 after all
                                                // increments above have occurred for all 4 NSTBs
  p_out->Branch("br_types",tpes,"tpes[nwrds]/I");   // "types array" -- the type of int[4]-length "word" 
                                                    // (either new NSTB or new photon... see below)
      /* 
       * the way the code loops below after the "event-cut" is that it loops through the 4 NSTBs, and then
       * through the photons found after the reconstruction
       * -> tpes is filled with "5+NSTB" at entry nwrds*4, then nwrds is incremented by 1
       * -> tpes is then filled with "305+NSTB" at entry nwrds+4 for each new photon, 
       *    then nwrds is incremented by 1
       * -> all other entries are 0
       *
       * example -- say 2 photons in nstb0 and 1 in nstb1, zero in the others:
       *   --> nwrds = [ (4 nstbs) + (3 total photons) ] * 4 = 7*4 = 28
       *
       * index   entry
       * 0       5 + 0 = 5
       * 1       0
       * 2       0
       * 3       0
       * 4       305 + 0 = 305
       * 5       0
       * 6       0
       * 7       0
       * 8       305 + 0 = 305 
       * 9       0
       * 10      0
       * 11      0
       * 12      5 + 1 = 6
       * 13      0
       * 14      0
       * 15      0
       * 16      305 + 1 = 306
       * 17      0
       * 18      0
       * 19      0
       * 20      5 + 2 = 7
       * 21      0
       * 22      0
       * 23      0
       * 24      5 + 3 = 8
       * 25      0
       * 26      0
       * 27      0
       */
  p_out->Branch("br_pxyzt",pxyzt,"pxyzt[nwrds]/F"); // momentum 4-vectors for each word
      /*
       * if type<300, then the 4-vector is total 4-momentum of all of the hits in the NSTB  
       * (see AnalTools::FourMom)
       * if type>300, then the 4-vector is the 4-momentum  of the photon
       *
       * the types are filled every 4 entries above because we store 4-vector info in this array: 
       * example -- say 2 photons in nstb0 and 1 in nstb1, zero in the others 
       *   --> nwrds = [ (4 nstbs) + (3 total photons) ] * 4 = 7*4 = 28
       *
       * index   entry
       * 0       p_x of NSTB 0
       * 1       p_y of NSTB 0
       * 2       p_z of NSTB 0
       * 3       Esum of NSTB 0
       * 4       p_x of 1st photon in NSTB 0
       * 5       p_y of 1st photon in NSTB 0
       * 6       p_z of 1st photon in NSTB 0
       * 7       E of 1st photon in NSTB 0
       * 8       p_x of 2nd photon in NSTB 0
       * 9       p_y of 2nd photon in NSTB 0
       * 10      p_z of 2nd photon in NSTB 0
       * 11      E of 2nd photon in NSTB 0
       * 12      p_x of NSTB 1
       * 13      p_y of NSTB 1
       * 14      p_z of NSTB 1
       * 15      E of NSTB 1
       * 16      p_x of 1st photon in NSTB 1
       * 17      p_y of 1st photon in NSTB 1
       * 18      p_z of 1st photon in NSTB 1
       * 19      E of 1st photon in NSTB 1
       * 20      p_x of NSTB 2
       * 21      p_y of NSTB 2
       * 22      p_z of NSTB 2
       * 23      E of NSTB 2
       * 24      p_x of NSTB 3
       * 25      p_y of NSTB 3
       * 26      p_z of NSTB 3
       * 27      E of NSTB 3
       */
  p_out->Branch("fpsPid",fpsPid,"fpsPid[nwrds]/I"); // FPS particle ID (see StFmsPoint.h)
                                                    // note: this branch is filled in same manner as br_types;
                                                    // if(300<br_types[i]<309) then fpsPid[i] is set to
                                                    // the corresponding point's FPS PID; otherwise
                                                    // it is filled with the number -1 by default
                                                    // (not the most efficient...)
       /*
        * kFpsPidNoFps=0,       // hit no slat
        * kFpsPidBad=1,         // hit status bad slat
        * kFpsPidGamma1=10,     // L1==0 L2==0 L3==0    gamma which did not convert
        * kFpsPidGamma2=11,     // L1==0 L2==0 L3>=1    golden gamma
        * kFpsPidGamma3=12,     // L1>=1 L2==0 L3==0    gamma with extra hit in layer1
        * kFpsPidGamma4=13,     // L1==0 L2>=1 L3==0    gamma with extra hit in layer2
        * kFpsPidGamma5=14,     // L1>=1 L2==0 L3>=1    gamma with extra hit in layer1
        * kFpsPidGamma6=15,     // L1==0 L2>=1 L3>=1    gamma with extra hit in layer2
        * kFpsPidGamma7=16,     // L1>=2 L2>=2 L3>=5    gamma converted to e+e-
        * kFpsPidMip=20,        // L1==1 L2==1 L3==1    MIP (charged hadron or muon)
        * kFpsPidElectron1=30,  // L1==1 L2==1 L3>=5    golden electron/positron
        * kFpsPidElectron2=31,  // L1==1 L2>=2 L3>=5    electron
        * kFpsPidElectron3=32,  // L1>=2 L2==1 L3>=5    electron
        * kFpsPidUnknown=40     // L1>=1 L2>=1 L3==0    not sure what to do
        */
  p_out->Branch("br_nSavedHits",&nSavedHits,"nSavedHits/I"); // number of saved hits, 
                                                   // see AnalTools::storeCluster, which calls 
                                                   // AnalTools::SaveClHit to increments this number
                                                   // 
                                                   // for call to AnalTools::storeCluser, 
                                                   // search for "STORE-CLUSTER" --> minimum ADC = 1; 
                                                   // min cluster energy = 0.5 
  p_out->Branch("br_SavedHits",SavedHits,"SavedHits[nSavedHits]/i"); // encoding of a single hit: 
         /*
          * encodes east/west, nstb, row, column, and ADC
          * see AnalTools::storeCluster which calls AnalTools::SaveClHit, which
          * does this encoding for each hit tower 
          *
          * the 4-byte encoding is layed out in the following way:
          *  - E = EW
          *  - N = NSTB
          *  - R = Row
          *  - C = Col
          *  - A = ADC
          *
          *   |  E N N N  |  R R R R  |  R R C C  |  C C C C  |
          *   |  A A A A  |  A A A A  |  A A A A  |  A A A A  | 
          *  
          * decode it by using the following (decimal places put in hex numbers for readability!)
          * ( based on AnalTools::PrClHit )
          * --> EW =   ( SavedHits & 0x.8000.0000 ) / 0x.8000.0000
          * --> NSTB = ( SavedHits & 0x.7000.0000 ) / 0x.1000.0000
          * --> Row  = ( SavedHits & 0x.0F90.0000 ) / 0x.0040.0000
          * --> Col =  ( SavedHits & 0x.003F.0000 ) / 0x.0001.0000
          * --> ADC =    SavedHits & 0x.0000.FFFF 
          *
          * the hits are ordered by highest-energy cluster
          */
  p_out->Branch("br_SavedTDC",SavedTDC,"SavedTDC[nSavedHits]/b"); // TDC value of the hit
  p_out->Branch("br_TrigBits",&TrigBits,"TrigBits/I"); // FMS layer 2 (DSMs) output bits 
         /*
          * (i.e., input bits to TCU, known as lastdsm[5] in the h111 tree)
          *
          * in fact this is exactly lastdsm[5] in this tree
          * to filter, use (TrigBits >> (L2_output_bit)) & 0x1
          *
          * 2015 pp output bits:
          * 0 - smbs3
          * 1 - smbs2
          * 2 - smbs1
          * 3 - lgbs3
          * 4 - lgbs2
          * 5 - lgbs1
          * 6 - dibs
          * 7 - jp2
          * 8 - jp1
          * 9 - jp0
          * 10 - dijet
          * 11 - unused
          * 12 - unused
          * 13 - unused
          * 14 - unused
          * 15 - unused
          */

  p_out->Branch("br_nCluster",&nCluster,"nCluster/I"); // number of clusters; 
                                                       // incremented every time 
                                                       // AnalTools::storeCluster is called

  p_out->Branch("br_nPhotonClu",&nPhotonClu,"nPhotonClu/I"); // total number of photons within clusters 
         /*
          * (equivalent to StMuFmsPoints?)
          * n.b. nPhotonClu-nCluster >= 0 for all events
          * this number is only incremented as we loop through the clusters and look at
          * their photon contents
          * BUT... in a sample OFile, I found nPhotonClu==nphotons for all events
          */
  p_out->Branch("br_SCIndex",SCIndex,"SCIndex[nCluster]/I"); // number of saved hits (nSavedHits) 
         /*                                                  // for current cluster, but... 
          * nSavedHits is incremented as we loop through each cluster, so basically:
          * to find out how many hits are in cluster number 3, we do: SCIndex[3]-SCIndex[2]
          * to find out how many hits are in the first cluster, we do: SCIndex[1]-SCindex[0] 
          * (n.b. SCindex[0]==0 for all events)
          * to find out how many hits are in the last cluster, we do: nSavedHits-SCindex[nCluster-1]
          */
  p_out->Branch("br_SavedCluTDC",SavedCluTDC,"SavedCluTDC[nCluster]/F"); // average of clusters' hits' TCDs
          /* NOTE: Steve Heppelmann may be doing a slew correction here; I am not yet, since
           * the idea of a slew correction is still under active development at PSU
           * (see root12fms/FpdRoot/HitCluster.cxx::AveTDC() )
           */
  p_out->Branch("br_SPCIndex",SPCIndex,"SPCIndex[nPhotonClu]/I"); // cluster index 
         // (starting at 0) of nth photon (ordered by highest-energy clusters)
         
  p_out->Branch("br_SPCEnergy",SPCEnergy,"SPCEnergy[nPhotonClu]/F"); // photon energy

  p_out->Branch("br_L2sum",L2sum,"L2sum[2]/i"); // L2 trigger ID
  p_out->Branch("br_lastdsm",lastdsm,"lastdsm[8]/i"); // full TCU input bitstream



  // RP branches = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

  p_out->Branch("RP_n_tracks",&tc,"RP_n_tracks/I"); // total number of tracks


  // tracks -------------------

  p_out->Branch("RP_t_index",t_index,"RP_t_index[RP_n_tracks]/I"); // track index number, according to 
                                                                   // how RP tracks are sorted 
                                                                   // (note: I have a deprecated sorting
                                                                   //  algorithm, which sorted tracks by
                                                                   //  energy and 'quality', emphasizing good
                                                                   //  tracks)
  p_out->Branch("RP_t_branch",t_branch,"RP_t_branch[RP_n_tracks]/I"); // RP branch 
    /*
     * 0 = EU
     * 1 = ED
     * 2 = WU
     * 3 = WD
     */
  p_out->Branch("RP_t_type",t_type,"RP_t_type[RP_n_tracks]/I"); // track type
    /*
     * 0 = rpsLocal -- 1 track point
     * 1 = rpsGlobal -- 2 track points
     * 2 = rpsUndefined -- track not defined
     */
  p_out->Branch("RP_t_planesUsed",t_planesUsed,"RP_t_planesUsed[RP_n_tracks]/i"); // number of SSD planes hit
                                                                                  // by track points in track
  p_out->Branch("RP_t_p",t_p,"RP_t_p[RP_n_tracks]/D"); // momentum
  p_out->Branch("RP_t_pt",t_pt,"RP_t_pt[RP_n_tracks]/D"); // transverse momentum
  p_out->Branch("RP_t_eta",t_eta,"RP_t_eta[RP_n_tracks]/D"); // pseudorapidity
  p_out->Branch("RP_t_time",t_time,"RP_t_time[RP_n_tracks]/D"); // time of track detection

  TString t_theta_str = Form("RP_t_theta[RP_n_tracks][%d]/D",StMuRpsTrack::mNumberOfAngleTypes);
  TString t_thetaRP_str = Form("RP_t_thetaRP[RP_n_tracks][%d]/D",StMuRpsTrack::mNumberOfAngleTypes);
  p_out->Branch("RP_t_theta",t_theta,t_theta_str.Data()); // polar angle at RP according to STAR coord sys
  p_out->Branch("RP_t_thetaRP",t_thetaRP,t_thetaRP_str.Data()); // polar angle at RP according to STAR survey

  p_out->Branch("RP_t_phi",t_phi,"RP_t_phi[RP_n_tracks]/D"); // azimuth at RP according to STAR coord sys
  p_out->Branch("RP_t_phiRP",t_phiRP,"RP_t_phiRP[RP_n_tracks]/D"); // azimuth at RP according to STAR survey
  p_out->Branch("RP_t_t",t_t,"RP_t_t[RP_n_tracks]/D"); // squared 4-momentum transfer
  p_out->Branch("RP_t_xi",t_xi,"RP_t_xi[RP_n_tracks]/D"); // fractional momentum loss (pbeam-p)/pbeam
  //p_out->Branch("RP_t_isBad",t_isBad,"RP_t_isBad[RP_n_tracks]/O"); // true if any kinematics have nonsense 
                                                                     // values; DEPRECATED, since track counter
                                                                     // tc is not incremented if isBad==true,
                                                                     // causing nonsense tracks to not
                                                                     // get added to p_out
  //p_out->Branch("RP_t_qualHash",t_qualHash,"RP_t_qualHash[RP_n_tracks]/D"); // DEPRECATED hash track info into 
                                                                            // a "quality" number, for 
                                                                            // sorting tracks

  // track points -------------------
  p_out->Branch("RP_n_trackpoints",tpc,"RP_n_trackpoints[RP_n_tracks]/I"); 
                                              // number of trackpoints for this track
                                              // NOTE: RP_n_trackpoints =t_type+1, if 
                                              // t_type != rpsUndefined (=2) 
  p_out->Branch("RP_p_tpExists",p_tpExists,"RP_p_tpExists[RP_n_tracks][2]/O"); 
                                              // true if track pointexists
  p_out->Branch("RP_p_RPid",p_RPid,"RP_p_RPid[RP_n_tracks][2]/I");
                                              // RP id:
                                              /*
                                               * 0 = E1U   1 = E1D   2 = E2U   3 = E2D
                                               * 4 = W1U   5 = W1D   6 = W2U   7 = W2D
                                               */
  p_out->Branch("RP_p_clustid_s1",p_clustid_s1,"RP_p_clustid_s1[RP_n_tracks][2]/I"); 
                                              // track point cluster IDs, indexed by SSD plane (1-4)
                                              // =-1 if plane not used in track point
  p_out->Branch("RP_p_clustid_s2",p_clustid_s2,"RP_p_clustid_s2[RP_n_tracks][2]/I");
  p_out->Branch("RP_p_clustid_s3",p_clustid_s3,"RP_p_clustid_s3[RP_n_tracks][2]/I");
  p_out->Branch("RP_p_clustid_s4",p_clustid_s4,"RP_p_clustid_s4[RP_n_tracks][2]/I");
  p_out->Branch("RP_p_quality",p_quality,"RP_p_quality[RP_n_tracks][2]/I"); 
                                              // track point quality:
                                              /* 
                                               * 0 = rpsNormal -- not golden and not undefined
                                               * 1 = rpsGolden -- single cluster in all 4 SSD planes
                                               * 2 = rpsNotSet -- undefined track point
                                               */
  p_out->Branch("RP_p_planesUsed",p_planesUsed,"RP_p_planesUsed[RP_n_tracks][2]/i"); 
                                              // number of SSD planes with valid cluster for track point
  p_out->Branch("RP_p_x",p_x,"RP_p_x[RP_n_tracks][2]/D"); // STAR survey coords x-position
  p_out->Branch("RP_p_y",p_y,"RP_p_y[RP_n_tracks][2]/D"); // STAR survey coords y-position
  p_out->Branch("RP_p_z",p_z,"RP_p_z[RP_n_tracks][2]/D"); // STAR survey coords z-position
  p_out->Branch("RP_p_time_pmt1",p_time_pmt1,"RP_p_time_pmt1[RP_n_tracks][2]/D"); 
                                              // TAC (in ns) of PMT 1 hit (2 PMT's per vessel)
  p_out->Branch("RP_p_time_pmt2",p_time_pmt2,"RP_p_time_pmt2[RP_n_tracks][2]/D"); 
                                              // TAC (in ns) of PMT 1 hit (2 PMT's per vessel)
  

  // miscellaneous branches = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
  // for things like BBC activity, TOF activity, etc.
  p_out->Branch("bbcADCSum",bbcADCSum,"bbcADCSum[2]/s"); // sum of BBC ADC for all tiles [0=east,1=west]
  p_out->Branch("bbcADCSumLarge",bbcADCSumLarge,"bbcADCSumLarge[2]/s"); 
                                                         // sum of BBC ADC for large tiles [0=east,1=west]
  p_out->Branch("bbcEarliestTDC",bbcEarliestTDC,"bbcEarliestTDC[2]/s"); // earlist BBC TDC (all tiles)
  p_out->Branch("bbcEarliestTDCLarge",bbcEarliestTDCLarge,"bbcEarliestTDCLarge[2]/s"); 
                                                                        // earlist BBC TDC (large tiles)
  p_out->Branch("zdcADCSum",zdcADCSum,"zdcADCSum[2]/s"); // sum of ZDC ADC [0=east,1=west]
  p_out->Branch("vpdADCSum",vpdADCSum,"vpdADCSum[2]/s"); // sum of VPD ADC [0=east,1=west]
  p_out->Branch("tofMultiplicity",&tofMultiplicity,"tofMultiplicity/s"); // TOF multiplicity
  //p_out->Branch("mtdADC",mtdADC,"mtdADC[2]/s"); // MTD adc [0=east,1=west]


  // deprecated branches = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
  // to be eventually removed from root15fms, but left here for compatibility...
  p_out->Branch("br_BBcSums",BBcSums,"BBcSums[5]/b"); // not used
  p_out->Branch("br_BBcVertex",BBcVertex,"BBcVertex[7]/F"); // not used
  p_out->Branch("br_EventN",&EventN,"EventN/I"); // probably not needed ?
                                                 // comes from dataSet::event, which is from 
                                                 // branch "event" from much older version h111 trees
  //p_out->Branch("br_Fpde",Fpde,"Fpde[8]/i"); // unecessary for run 15++ ???






  // event tree 'evtr' =================================================================================
  // "easy-to-read" but much bigger event tree (useful for things like event display)
  if(build_evtr) {
    evtr = new TTree("evtr","evtr");
    evtr->Branch("runnum",&Rnum,"Rnum/I");
    evtr->Branch("ievt",&ievt,"ievt/I");
    evtr->Branch("Bunchid7bit",&Bunchid7bit,"Bunchid7bit/I");
    evtr->Branch("BunchL",&BunchL,"BunchL/L");

    // hit branches  = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    evtr->Branch("nhits",&hc,"nhits/I"); // # hits in the event
    evtr->Branch("hit_nstb",hit_nstb,"hit_nstb[nhits]/s");
    evtr->Branch("hit_chan",hit_chan,"hit_chan[nhits]/s");
    evtr->Branch("hit_row",hit_row,"hit_row[nhits]/I");
    evtr->Branch("hit_col",hit_col,"hit_col[nhits]/I");
    evtr->Branch("hit_adc",hit_adc,"hit_adc[nhits]/s");
    evtr->Branch("hit_energy",hit_en,"hit_energy[nhits]/F");
    evtr->Branch("hit_gain",gain,"hit_gain[nhits]/F");
    evtr->Branch("hit_gainCorr",gainCorr,"hit_gainCorr[nhits]/F");
    evtr->Branch("hit_bitshift",bitshift,"hit_bitshift[nhits]/S"); // the bitshift (a.k.a. 'bitshift gain')
    evtr->Branch("hit_adcCorrected",adcCorrected,"hit_adcCorrected[nhits]/s"); // corrected for off-by-1 issue
    evtr->Branch("hit_tdc",hit_tdc,"hit_tdc[nhits]/s"); // timing
    //evtr->Branch("hit_timeDepCorr",timeDepCorr,"hit_timeDepCorr[nhits]/F"); // unused?

    // cluster branches  = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    evtr->Branch("nclusters",&cc,"nclusters/I"); // # clusters in the event
    evtr->Branch("clu_nstb",clu_nstb,"clu_nstb[nclusters]/s");
    evtr->Branch("clu_category",clu_category,"clu_category[nclusters]/I"); // cluster category 
      //(see StEvent/StEnumerations.h)
      /* 
       * 0 - ambiguous: could be 1- or 2-photon cluster, needs to be fitted
       * 1 - one photon in the cluster
       * 2 - two photons in the cluster
       * 3 - invalid (default/unknown/error)
       */
    evtr->Branch("clu_ntowers",clu_ntowers,"clu_ntowers[nclusters]/I"); // # FMS towers in the cluster
    evtr->Branch("clu_nphotons",clu_nphotons,"clu_nphotons[nclusters]/I"); // # photons in the cluster
    evtr->Branch("clu_energy",clu_en,"clu_energy[nclusters]/F"); // cluster energy
    /*
    evtr->Branch("clu_energyFromHits",clu_enFromHits,"clu_energyFromHits[nclusters]/F"); 
                                                 // cluster energy (from summing over its hits' energies)
    evtr->Branch("clu_energyFromPoints",clu_enFromPoints,"clu_energyFromPoints[nclusters]/F"); 
                                                 // cluster energy (from summing over its points' energies)
    */
    evtr->Branch("clu_x",clu_x,"clu_x[nclusters]/F"); // x centroid (1st moment)
    evtr->Branch("clu_y",clu_y,"clu_y[nclusters]/F"); // y centroid (1st moment)
    evtr->Branch("clu_sigmaMin",clu_sigmin,"clu_sigmaMin[nclusters]/F"); // min 2nd moment
    evtr->Branch("clu_sigmaMax",clu_sigmax,"clu_sigmaMax[nclusters]/F"); // max 2nd moment
    evtr->Branch("clu_chi2ndf1phot",clu_csqn1,"clu_chi2ndf1phot[nclusters]/F"); 
                                                   // chi^2/NDF for 1-photon fit to cluster
    evtr->Branch("clu_chi2ndf2phot",clu_csqn2,"clu_chi2ndf2phot[nclusters]/F"); 
                                                   // chi^2/NDF for 2-photon fit to cluster
    evtr->Branch("clu_id",clu_id,"clu_id[nclusters]/I"); // cluster ID

    // point branches  = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    evtr->Branch("npoints",&pc,"npoints/I"); // number of points
    evtr->Branch("pho_nstb",pho_nstb,"pho_nstb[npoints]/s");
    evtr->Branch("pho_energy",pho_en,"pho_energy[npoints]/F");
    evtr->Branch("pho_x",pho_x,"pho_x[npoints]/F");
    evtr->Branch("pho_y",pho_y,"pho_y[npoints]/F");
    evtr->Branch("pho_id",pho_id,"pho_id[npoints]/I"); // point ID
    evtr->Branch("pho_px",pho_px,"pho_px[npoints]/F"); // point momentum x
    evtr->Branch("pho_py",pho_py,"pho_py[npoints]/F"); // point momentum y
    evtr->Branch("pho_pz",pho_pz,"pho_pz[npoints]/F"); // point momentum z
    evtr->Branch("pho_fpsPid",pho_fpsPid,"pho_fpsPid[npoints]/I"); // FPS PID

    // pair branches  = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    evtr->Branch("npairs",&ppc,"npairs/I");
    evtr->Branch("pair_energy",pair_en,"pair_energy[npairs]/F");
    evtr->Branch("pair_pt",pair_pt,"pair_pt[npairs]/F"); // transverse momentum
    evtr->Branch("pair_eta",pair_eta,"pair_eta[npairs]/F"); // pseudorapidity
    evtr->Branch("pair_phi",pair_phi,"pair_phi[npairs]/F"); // azimuth
    evtr->Branch("pair_mass",pair_mass,"pair_mass[npairs]/F"); // inviariant mass
    evtr->Branch("pair_dgg",pair_dgg,"pair_dgg[npairs]/F"); // transverse distance between points
    evtr->Branch("pair_zgg",pair_zgg,"pair_zgg[npairs]/F"); // energy imbalance
    evtr->Branch("pair_x",pair_x,"pair_x[npairs]/F"); // x centroid
    evtr->Branch("pair_y",pair_y,"pair_y[npairs]/F"); // y centroid
    evtr->Branch("pair_coneRad",pair_coneRad,"pair_coneRad[3][npairs]/F"); // isolation cone radius
                       /* first index is which cone size: [0=100mrad, 1=70mrad, 2=30mrad] */
    evtr->Branch("pair_coneEn",pair_coneEn,"pair_coneEn[3][npairs]/F"); // isolation cone energy
    evtr->Branch("pair_coneEnFrac",pair_coneEnFrac,"pair_coneEnFrac[3][npairs]/F"); // pair energy / cone energy
    evtr->Branch("pair_fpsPid",pair_fpsPid,"pair_fpsPid[npairs]/i"); // FPS particle ID for the pair:
              /* according to StFmsPointPair:
               * each digit {0=bad,1=gamma,2=hadron,3=electron}
               * for pair(npoint=2), 11=gg, 22=hh, 33=ee, 13=ge,etc
               * for nPoint>2, LSD is first point and MSD is last 
               */
  }; // end if build_evtr


  // END DEFINING TREES ///////////////////////////////////////////////////////////////////////////

  return kStOK;
};


//########################################################################


Int_t StFmsOFileMaker::Finish() {
  LOG_INFO << Form("Writing and closing %s",OFilename) << endm;
  OFile->Write();
  OFile->Close();
  return kStOK;
}


//########################################################################

 
Int_t StFmsOFileMaker::Make() {

  // obtain event instances from muDST and DST data streams
  muDst = (StMuDst*) GetInputDS("MuDst");
  muEvent = muDst->event(); // from muDST
  dsEvent = (StEvent*) GetInputDS("StEvent"); // from DST


  if(!muEvent) { LOG_ERROR << "StFmsOFileMaker::Make -- no StMuEvent" << endm;  return kStErr; };
  if(!dsEvent) { LOG_ERROR << "StFmsOFileMaker::Make -- no StEvent" << endm;  return kStErr; };

  StRunInfo runInfo = muEvent->runInfo();


  // obtain FMS collection
  //muFmsColl = muDst->muFmsCollection(); // from StMuEvent -- DEPRECATED; prefer data from StEvent
  dsFmsColl = (StFmsCollection*) dsEvent->fmsCollection(); // from StEvent
  
  // obtain RP collection
  //mMuRpsColl = muDst->RpsCollection(); // deprecated, since afterburner has been enabled
  mAfterburner->updateVertex(0.000415, 0.000455, 0.0); // specific to run 15 pp200 trans !!!
  mMuRpsColl = mAfterburner->process(); // executes afterburner 

  //if(!muFmsColl) {LOG_ERROR << "StFmsOFileMaker::Make -- no MuFmsCollection"<<endm; return kStErr;}
  if(!dsFmsColl) {LOG_ERROR << "StFmsOFileMaker::Make -- no FmsCollection"<<endm; return kStErr;}
  if(!mMuRpsColl) {LOG_ERROR << "StFmsOFileMaker::Make -- no MuRpsCollection"<<endm; return kStErr;}



  // get FMS object arrays from StMuEvent collections; deprecated, since we prefer methods like
  // StMuFmsCollection::getHit(), getCluster(), getPoint(), etc.
  /*
  muHits = muFmsColl->getHitArray();
  muClusters = muFmsColl->getClusterArray();
  muPoints = muFmsColl->getPointArray();
  */

  // get FMS object arrays from StEvent
  StSPtrVecFmsHit & dsHits = dsFmsColl->hits();
  StSPtrVecFmsCluster & dsClusters = dsFmsColl->clusters();
  StSPtrVecFmsPoint & dsPoints = dsFmsColl->points();
  vector<StFmsPointPair*> & dsPairs = dsFmsColl->pointPairs();


  // get number of each type of object (which is the max number of *actual* objects to read out;
  // note that I keep track of how many *actual* objects there are, which is needed for when filling
  // trees)
  nhits = dsFmsColl->numberOfHits();
  nclusters = dsFmsColl->numberOfClusters();
  npoints = dsFmsColl->numberOfPoints();
  npairs = dsFmsColl->numberOfPointPairs();


  if(verbose) {
    LOG_INFO << "[++++++] nhits=" << nhits 
             << "  nclusters=" << nclusters 
             << "  npoints=" << npoints 
             << endm;
  };



  // event-level info ============================================================================

  // event id and run number
  ievt = muEvent->eventId(); // -->p_out,evtr
  Rnum = muEvent->runNumber(); // -->p_out,evtr


  // lastdsm (TCU inputs) and L2sum (trigger IDs satisfied)
  for(int dd=0; dd<8; dd++) 
    lastdsm[dd] = muEvent->triggerData()->lastDSM(dd); // -->p_out
  TrigBits = lastdsm[5]; // -->p_out

  L2sum_full = muEvent->triggerData()->l2sum();
  for(int ld=0; ld<2; ld++)
    L2sum[ld] = (L2sum_full >> ld*32) & 0xFFFFFFFF; // -->p_out


  // bXing number
  Bunchid7bit = muEvent->triggerData()->bunchId7Bit(); // -->p_out,evtr
  for(int b=0; b<2; b++) bc[b]=(muEvent->eventInfo()).bunchCrossingNumber(b); // -->p_out
  BunchL = (Long64_t) bc[0] + bc[1]*((Long64_t)pow(2,32)); // -->p_out,evtr


  // compare muEvent and dsEvent ============================================================
  // DEPRECATED (enable it if you want to test it.. not everything is available in both StEvent and StMuEvent)
  /*
  if(muEvent->eventId() != dsEvent->id()) {
    LOG_ERROR << "MISMATCH BETWEEN StEvent and StMuEvent: " <<
                 "muEvent id=" << muEvent->eventId() << " but " <<
                 "dsEvent id=" << dsEvent->id() << endm;
  };
  if(muEvent->runId() != dsEvent->runId()) {
    LOG_ERROR << "MISMATCH BETWEEN StEvent and StMuEvent: " <<
                 "muEvent runId=" << muEvent->runId() << " but " <<
                 "dsEvent runId=" << dsEvent->runId() << endm;
  };
  if((muEvent->eventInfo()).time() != dsEvent->time()) {
    LOG_ERROR << "MISMATCH BETWEEN StEvent and StMuEvent: " <<
                 "muEvent time=" << (muEvent->eventInfo()).time() << " but " <<
                 "dsEvent time=" << dsEvent->time() << endm;
  };
  // bunchCrossingNumber not filled in dsEvent ??
  if((muEvent->eventInfo()).bunchCrossingNumber(0) != dsEvent->info()->bunchCrossingNumber(0) ||
     (muEvent->eventInfo()).bunchCrossingNumber(1) != dsEvent->info()->bunchCrossingNumber(1) ) {
    LOG_ERROR << "MISMATCH BETWEEN StEvent and StMuEvent: " <<
                 "muEvent and dsEvent have different bc:" << endm;
    LOG_ERROR << "        muEvent: " << (muEvent->eventInfo()).bunchCrossingNumber(0) <<
                                "  " << (muEvent->eventInfo()).bunchCrossingNumber(1) << endm;
    LOG_ERROR << "        dsEvent: " << dsEvent->bunchCrossingNumber(0) <<
                                "  " << dsEvent->bunchCrossingNumber(1) << endm;
  };
  printf("============= muEvent::eventInfo ======================\n");
  (muEvent->eventInfo()).Print();
  printf("============= dsEvent::eventInfo ======================\n");
  dsEvent->info()->Print();
  printf("=======================================================\n");
  */



  // spin info ============================================================================

  // if runnum changed, load new spindb (highly unlikely, but done anyway to be safe)
  if(Rnum!=Rnum_tmp) {
    Rnum_tmp = Rnum;

    // initialize spin db for this run number
    mSpinDb->InitRun(Rnum);
    //assert(mSpinDb->isValid() && "mSpinDb->isValid = false");
    //mSpinDb->print(1);
  };

  if(verbose) LOG_INFO << "[+] Bunchid7bit=" << Bunchid7bit << endm;

  // get spin4bit (this is what people usually call "the spinbit" in the STAR framework language)
  /* spin4bit has 4 bits: [blueUp][blueDown][yellUp][yellDown]
   *         b-y-  spin4bit =  5 = 0101
   *         b-y+  spin4bit =  6 = 0110
   *         b+y-  spin4bit =  9 = 1001
   *         b+y+  spin4bit = 10 = 1010
   */
  spin4bit = mSpinDb->spin4usingBX7(Bunchid7bit);

  // determine p_out spin ("spin2bit")
  /* spin2bit has 2 bits: [0=blueDown 1=blueUp] [0=yellDown 1=yellUp]
   * b-y-  spin2bit = 0 = 00
   * b-y+  spin2bit = 1 = 01
   * b+y-  spin2bit = 2 = 10
   * b+y+  spin2bit = 3 = 11
   * undefined/bad: spin2bit set to 40
   */
  // get spin2bit from spin4bit:
  switch(spin4bit) {
    case 5: spin2bit=0; break;
    case 6: spin2bit=1; break;
    case 9: spin2bit=2; break;
    case 10: spin2bit=3; break;
    default: spin2bit=40;
  }; // -->p_out
  

  // spin QA stuff (based on v124bits); Branden Summa did this for run15, but
  // when I was originally writing this maker before the spin QA was done, I
  // had to use the v124bits to access the spin. Now that spin QA is done, we
  // have access to spin via the spin4bit above, therefore I've just left the
  // old v124bits code here as an optional cross-check of the spin QA
  if(check_spinbyte) {
    // get spinbyte from mSpinDb
    /* spinbyte has 8 bits: 
     *     [4-bit blueNibble] [4-bit yellowNibble]
     * each nibble has 4 bits: 
     *     [0=polarized 1=unpolarized] [0=notUp 1=up] [0=notDown 1=down] [0=notFilled 1=filled]
     *         b-y-  spinbyte = 0011 0011
     *         b-y+  spinbyte = 0011 0101
     *         b+y-  spinbyte = 0101 0011
     *         b+y+  spinbyte = 0101 0101
     */
    spinbyte_db = (mSpinDb->getSpin8bits())[Bunchid7bit];
    // and get spinbyte from v124bits (for spin QA)
    yNib = (mSpinDb->getRawV124bits())[ (Bunchid7bit*3 + 240) % 360 ] & 0x0f;
    bNib = (mSpinDb->getRawV124bits())[ Bunchid7bit*3 ] >> 4;
    spinbyte = (bNib<<4) + yNib;

    // get p_out's spin2bit from spinbyte: 
    // yellow spin:
    if( (spinbyte >> 0 ) & 0x1) {
      if( (spinbyte >> 2) & 0x1) sp_y=1;
      else if( (spinbyte >> 1) & 0x1) sp_y=0;
    }
    else sp_y=-1;
    // blue spin:
    if( (spinbyte >> 4 ) & 0x1) {
      if( (spinbyte >> 6) & 0x1) sp_b=1;
      else if( (spinbyte >> 5) & 0x1) sp_b=0;
    }
    else sp_b=-1;
    // set p_out spin2bit variable:
    spinFromSpinbyte = (sp_y>=0 && sp_b>=0) ? (sp_b<<1)+sp_y : 40;

    if(verbose) {
      LOG_INFO << "[+] spinbyte=" << std::bitset<8>(spinbyte)
               << "  spinbyte_db=" << std::bitset<8>(spinbyte_db)
               << "  spin4bit=" << std::bitset<4>(spin4bit)
               << "  spin2bit=" << std::bitset<2>(spin2bit)
               << "  spinFromSpinbyte=" << std::bitset<2>(spinFromSpinbyte) <<  endm;
    };

    // check spinbyte and spin2bit consistency
    if(spinbyte!=spinbyte_db) 
      LOG_ERROR << "SPINBYTE PROBLEM: spinbyte from v124 != spinbyte from DB" << endm;
    if(spin2bit!=spinFromSpinbyte) 
      LOG_ERROR << "SPINBYTE PROBLEM: spin2bit from spinbyte != spin2bit from spin4bit from DB" << endm;

  }; // end if(check_spinbyte)


  // dump spinbytes (only for first event) (still uses v124bits...)
  if(dump_spinbits) {
    LOG_INFO << "SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS" << endm;
    LOG_INFO << "--- BEGIN SPINBYTE DUMP ---" << endm;
    for(int bx=0; bx<120; bx++) {
      yNib = (mSpinDb->getRawV124bits())[ (bx*3 + 240) % 360 ] & 0x0f;
      bNib = (mSpinDb->getRawV124bits())[ bx*3 ] >> 4;
      spinbyte_curr = (bNib<<4) + yNib;
      spinbyte_db_curr = (mSpinDb->getSpin8bits())[bx];
      spinbyte_same = spinbyte_curr==spinbyte_db_curr;

      if( (spinbyte_curr >> 0 ) & 0x1) {
        if( (spinbyte_curr >> 2) & 0x1) spc_y='+';
        else if( (spinbyte_curr >> 1) & 0x1) spc_y='-';
      }
      else spc_y='0';

      if( (spinbyte_curr >> 4 ) & 0x1) {
        if( (spinbyte_curr >> 6) & 0x1) spc_b='+';
        else if( (spinbyte_curr >> 5) & 0x1) spc_b='-';
      }
      else spc_b='0';

      LOG_INFO << std::bitset<4>(spinbyte_curr>>4) << 
                  "." << 
                  std::bitset<4>(spinbyte_curr) << 
                  "  " << 
                  std::bitset<4>(spinbyte_db_curr>>4) << 
                  "." << 
                  std::bitset<4>(spinbyte_db_curr) << 
                  "  " << 
                  "same=" << spinbyte_same << " " <<  
                  spc_b << " " << spc_y << " bx=" << bx << endm;
    };
    LOG_INFO << "--- END SPINBYTE DUMP ---" << endm;
    LOG_INFO << "SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS" << endm;
    dump_spinbits = false; // if we did it once, don't do it again
  }; // end if(dump_spinbits)


  
  // debugging diagnostics for trigger info ===================================================
  if(verbose) {
    LOG_INFO << Form("TRIGGER -- bx=%d  |  spinbyte=0x%X=",
      Bunchid7bit,spinbyte) << std::bitset<8>(spinbyte) << endm;
    //LOG_INFO << Form("TRIGGER -- Bunchid7bit=%d  spin4bit=%d  bc[1]=%d bc[0]=%d  (b7bittest=%d)",
      //Bunchid7bit,spin4bit,bc[1],bc[0],((bc[0]-7)%120)==Bunchid7bit) << endm;
    //for(int dd=0; dd<8; dd++) LOG_INFO << Form("           lastdsm[%d]=0x%x",dd,lastdsm[dd]) << endm;
    LOG_INFO << Form("           TrigBits=0x%x",TrigBits) << endm;
    for(int ld=0; ld<2; ld++) LOG_INFO << Form("           l2sum[%d]=0x%x",ld,L2sum[ld]) << endm;
    LOG_INFO << Form("           l2sum_full=0x%llx",L2sum_full) << endm;
  };



  // <-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><->
  //
  //  BEGIN TREE FILLING LOOPS
  //
  // <-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><->



  //////////////////////////////////////////////////////////////////////////////////
  //
  //    FMS Sector 
  //
  //////////////////////////////////////////////////////////////////////////////////


  // reset FMS branches
  ResetFmsBranchVars();
  

  // CLUSTER LOOP for evtr ======================================================
  // - also reorganizes clusters into arrays of clusters for each NSTB (clusterArr[4]),
  //   which is needed later for sorted cluster loops which are for filling p_out
  // - also computes cluster sums for each NSTB (en_sum[4]), also needed later for p_out
  // - 'ci' is an iterator through the list of clusters; 'cc' counts the number of actual
  //   clusters which are considered (it is initially zero when going into this loop)
  for(int nn=0; nn<4; nn++) clusterArr[nn] = new TObjArray();
  for(int ci=0; ci<nclusters; ci++) {
    clu = dsClusters[ci];
    if(clu) { 

      detId = clu->detectorId();
      clu_nstb[cc] = detId-7; // -->evtr

      clu_id[cc] = clu->id(); // -->evtr
      clu_ntowers[cc] = clu->nTowers(); // -->evtr
      clu_nphotons[cc] = clu->nPhotons(); // -->evtr
      clu_category[cc] = clu->category(); // -->evtr

      clu_en[cc] = clu->energy(); // -->evtr

      clu_pos = mFmsDb->getStarXYZ(detId,clu->x(),clu->y()); // get STAR (x,y,x)
      clu_x[cc] = clu_pos.x(); // this is in modified STAR coords  // -->evtr
      clu_y[cc] = clu_pos.y(); // -->evtr

      clu_sigmin[cc] = clu->sigmaMin(); // -->evtr
      clu_sigmax[cc] = clu->sigmaMax(); // -->evtr
      clu_csqn1[cc] = clu->chi2Ndf1Photon(); // -->evtr
      clu_csqn2[cc] = clu->chi2Ndf2Photon(); // -->evtr

      // check if cluster's energy matches sum of cluster's hits' energies
      // and sum of cluster's points' energies (short answer: yes, it does)
      /*
      if(build_evtr) {
        clu_enFromHits[cc] = 0;
        clu_enFromPoints[cc] = 0;
        StPtrVecFmsHit & cluhitvec = clu->hits();
        for(unsigned int hi=0; hi<cluhitvec.size(); hi++) {
          hit = (StFmsHit*)(cluhitvec[hi]);
          if(hit) clu_enFromHits[cc] += hit->energy(); // -->evtr
        };
        StPtrVecFmsPoint & clupointvec = clu->points();
        for(unsigned int pi=0; pi<clupointvec.size(); pi++) {
          pho = (StFmsPoint*)(clupointvec[pi]);
          if(pho) clu_enFromPoints[cc] += pho->energy(); // -->evtr
        };
      };
      */

      // add this cluster to clusterArr and add its energy to en_sum
      nstb = clu_nstb[cc];
      cluEn = clu_en[cc];
      if(nstb>=1 && nstb<=4) {
        clusterArr[nstb-1]->AddLast(clu);
        en_sum[nstb-1] += cluEn;
        en_sum_all += cluEn;
        cc++; // -->evtr
      }
      else LOG_ERROR << "NSTB of cluster out of range; IGNORING THIS CLUSTER" << endm;
    };
  }; // end cluster loop


  // get number of clusters in each nstb
  for(int nn=0; nn<4; nn++) ent[nn] = clusterArr[nn]->GetEntries();

  if(verbose) {
    LOG_INFO << "CLUSTER LOOP --- " << "nclusters=" << nclusters
             << " n(1)=" << ent[0] << " n(2)=" << ent[1] << " n(3)=" << ent[2] << " n(4)=" << ent[3] << endm;
  };



  // HIT LOOP for evtr ======================================
  // - also fills energy matrices 'eMat[4]' used for filling p_out later
  // - 'hi' is an iterator through the list of hits; 'hc' counts the number of actual
  //   hits which are considered (it is initially zero when going into this loop)
  for(int hi=0; hi<nhits; hi++) {
    hit = dsHits[hi];
    if(hit) {
      detId = hit->detectorId();
      nstb = detId - 7;
      hit_nstb[hc] = nstb; // -->evtr
      hit_chan[hc] = hit->channel(); // -->evtr
      hit_en[hc] = hit->energy(); // -->evtr
      hit_adc[hc] = hit->adc(); // -->evtr
      hit_tdc[hc] = hit->tdc(); // -->evtr

      if(verbose) LOG_INFO << "[++++++++++++] nstb=" << nstb << endm;

      if(hit_nstb[hc]>=1 && hit_nstb[hc]<=4 && hit_en[hc]>0) {

        hit_row[hc] = (hit_chan[hc]-1)/NCOLS[nstb-1]; // -->evtr
        hit_col[hc] = (hit_chan[hc]-1)%NCOLS[nstb-1]; // -->evtr

        gain[hc] = mFmsDb->getGain((Int_t)detId,(Int_t)hit_chan[hc]); // -->evtr
        gainCorr[hc] = mFmsDb->getGainCorrection((Int_t)detId,(Int_t)hit_chan[hc]); // -->evtr
        bitshift[hc] = mFmsDb->getBitShiftGain((Int_t)detId,(Int_t)hit_chan[hc]); // -->evtr
        adcCorrected[hc] = mFmsDb->getCorrectedAdc(detId,hit_chan[hc],hit_adc[hc]); // -->evtr
        //timeDepCorr[hc] = mFmsDb->getTimeDepCorr(ievt,detId-8,hit_chan[hc]);
        

        if(verbose) {
          LOG_INFO << " ENERGY MATRIX LOOP :: hc=" << hc
                   << " nstb=" << nstb
                   << " chan=" << hit_chan[hc]
                   << " (r" << hit_row[hc] << ",c" << hit_col[hc] << ")"
                   << " en=" << hit_en[hc]
                   << " adc=" << hit_adc[hc]
                   << endm;
        };

        if(hit_row[hc]>=0 && hit_row[hc]<NROWS[nstb-1] && hit_col[hc]>=0 && hit_col[hc]<NCOLS[nstb-1]) {
          (*(eMat[nstb-1]))(hit_row[hc],hit_col[hc]) = hit_en[hc];
        };
        hc++; // -->evtr;
      };
    };
  }; // end hit loop and energy matrix filling
  


  // POINT LOOP for evtr ==============================================
  // - 'pi' is an iterator through the list of points; 'pc' counts the number of actual
  //   points which are considered (it is initially zero when going into this loop)
  if(build_evtr) {
    for(int pi=0; pi<npoints; pi++) {
      pho = dsPoints[pi];
      if(pho) {
        pho_nstb[pc] = pho->detectorId() - 7; // -->evtr
        pho_id[pc] = pho->id(); // -->evtr
        pho_en[pc] = pho->energy(); // -->evtr
        pho_pos = pho->XYZ();
        pho_x[pc] = pho_pos.x(); // -->evtr // this is STAR coordinates (using survey equations)
        pho_y[pc] = pho_pos.y(); // -->evtr
        pho_mom = pho->fourMomentum();
        pho_px[pc] = pho_mom.px(); // -->evtr
        pho_py[pc] = pho_mom.py(); // -->evtr
        pho_pz[pc] = pho_mom.pz(); // -->evtr
        pho_fpsPid[pc] = pho->fpsPid(); // -->evtr;
        pc++; // -->evtr
      };
    }; // end point loop
  }; // end if build_evtr


  // PAIR LOOP for evtr ==============================================
  // - 'ppi' is an iterator through the list of pairs; 'ppc' counts the number of actual
  //   pairs which are considered (it is initially zero when going into this loop)
  if(build_evtr) {
    for(int ppi=0; ppi<npairs; ppi++) {
      pair = dsPairs[ppi];
      if(pair) {
        pair_en[ppc] = pair->energy(); // -->evtr
        pair_pt[ppc] = pair->pT(); // -->evtr
        pair_eta[ppc] = pair->eta(); // -->evtr
        pair_phi[ppc] = pair->phi(); // -->evtr
        pair_mass[ppc] = pair->mass(); // -->evtr
        pair_dgg[ppc] = pair->dgg(); // -->evtr
        pair_zgg[ppc] = pair->zgg(); // -->evtr
        pair_x[ppc] = pair->x(); // -->evtr
        pair_y[ppc] = pair->y(); // -->evtr
        pair_fpsPid[ppc] = pair->fpsPid(); // -->evtr
        for(int con=0; con<3; con++) {
          pair_coneRad[con][ppc] = pair->coneRadius(con); // -->evtr
          pair_coneEn[con][ppc] = pair->coneEnergy(con); // -->evtr
          pair_coneEnFrac[con][ppc] = pair->coneEnergyFraction(con); // -->evtr
        };
        ppc++; // -->evtr
      };
    }; // end pair loop
  }; // end if build_evtr


  // END FILLING evtr BRANCHES
  /////////
  

  /////////
  // BEGIN FILLING MAIN p_out BRANCHES


  // BEGIN MAIN CLUSTER LOOP =====================================================================
  // - several p_out branches are filled here
  // - within each nstb, it sorts clusters by energy
  // - for each cluster, it loops through its hits and points
  // --------------------------------------------------------------------

  // loop through each nstb, in turn, looping through their clusters
  for(int nn=0; nn<4; nn++) {
    nstb=nn+1;
    if(verbose_clu) LOG_INFO << "NSTB " << nn+1 << "-----------------------------------" << endm;

    // reset sort value array
    for(int ii=0; ii<NCELLS; ii++) clu_en_sortval[ii]=-1;

    // "esum-cut" for each nstb; demands there be some energy in this nstb, but not too much
    if(en_sum[nn]>1.0 && en_sum[nn]<700.0) {
      
      // compute "average" momentum 4-vector for this nstb
      // (following root12fms/FpdRoot/AnalTools->FourMom->mom(TMatrix,...) )
      TVector3 off(0,0,0);
      TVector3 momsum(0.,0.,0.);
      TVector3 xyz(0.,0.,0.);
      Float_t width = CELL_WIDTH[nn];
      off[2] = zPos[nn];
      off[1] = yOffset[nn];
      off[0] = xOffset[nn];
      Int_t signx = (nn==0||nn==2) ? 1:-1;
      EnergySum = 0;
      for(int ir=0; ir<NROWS[nn]; ir++) {
        for(int ic=0; ic<NCOLS[nn]; ic++) {
          xyz[0] = off[0] - signx*ic*width;
          xyz[1] = off[1] - ir*width;
          xyz[2] = off[2];
          xyz = xyz * (1/xyz.Mag());
          Energy = TMath::Max((*eMat[nn])(ir,ic),(Float_t)0.0);
          xyz = xyz * Energy;
          EnergySum += Energy; // 4-momentum energy
          momsum = momsum + xyz; // 4-momentum momenta
        };
      };

      // add nstb blocks to types and 4-momenta arrays
      tpes[nwrds*4] = nn+5; // -->p_out
      vecs[nwrds] = StLorentzVectorF(momsum[0],momsum[1],momsum[2],EnergySum);
      nwrds++;


      // sort clusters by energy
      if(verbose_clu) LOG_INFO << "     unsorted clusters:" << endm;
      for(int ci=0; ci<ent[nn]; ci++)
      {
        clu_en_sortval[ci] = ((StFmsCluster*)(clusterArr[nn]->At(ci)))->energy();
        if(verbose_clu)  LOG_INFO << "          en=" << clu_en_sortval[ci] << endm;
      }; 
      TMath::Sort(NCELLS,clu_en_sortval,clu_en_sortind);
      

      // sorted cluster loop ------------------------------------------------------
      if(verbose_clu) LOG_INFO << "     sorted clusters:" << endm;
      for(int ci=0; ci<ent[nn]; ci++) {
        clu = (StFmsCluster*)(clusterArr[nn]->At(clu_en_sortind[ci]));

        // loop through this cluster's photons ---------------------------------------------
        StPtrVecFmsPoint & phoOfClu = clu->points();
        for(unsigned int pi=0; pi<phoOfClu.size(); pi++) {
          pho = (StFmsPoint*) phoOfClu[pi];
          if(pho) {

            vecs[nwrds] = pho->fourMomentum();
            tpes[nwrds*4] = 305 + nn; // -->p_out
            fpsPid[nwrds*4] = pho->fpsPid(); // -->p_out

            SPCIndex[nPhotonClu] = nCluster; // -->p_out
            SPCEnergy[nPhotonClu] = pho->energy(); // -->p_out
            nPhotonClu++;

            if(verbose_pho) {
              LOG_INFO << "            photon " << pi+1
                       << " id=" << pho->id()
                       << " en=" << pho->energy()
                       << " 4-mom=(" 
                          << vecs[nwrds].px() << ","
                          << vecs[nwrds].py() << ","
                          << vecs[nwrds].pz() << ","
                          << vecs[nwrds].e() << ")"
                       << "  parent_clu_id=" << pho->cluster()->id()
                       << " xyz=("
                          << (pho->XYZ()).x() << ","
                          << (pho->XYZ()).y() << ","
                          << (pho->XYZ()).z() << ")"
                       << endm;
              LOG_INFO << " ---- SPCIndex[" << nPhotonClu-1
                       << "] = " << SPCIndex[nPhotonClu-1] << endm;
              LOG_INFO << " ---- SPCEnergy[" << nPhotonClu-1
                       << "] = " << SPCEnergy[nPhotonClu-1] << endm;
            };

            nwrds++;
            nphotons++; // -->p_out

          }
          else {
            num_null++;
            if(verbose_pho) 
              LOG_INFO << 
              "            photon " << pi+1 << " -- NULL POINTER... potential issue??" << endm;
          };

        }; // end cluster's photon loop


        // append to SCIndex
        SCIndex[nCluster] = nSavedHits; // -->p_out
        if(verbose_pho) LOG_INFO << "      SCIndex[" << nCluster << "]=" << SCIndex[nCluster] << endm;


        // loop through this cluster's hits ------------------------------------------------------
        StPtrVecFmsHit & hitOfClu = clu->hits();
        cluSumTDC = 0;
        cluHitCount = 0;
        for(unsigned int hi=0; hi<hitOfClu.size(); hi++) {
          hit = (StFmsHit*)(hitOfClu[hi]);
          if(hit) {
            // get hit data
            hit_n = hit->detectorId()-7;
            hit_r = (hit->channel()-1) / NCOLS[hit_n-1];
            hit_c = (hit->channel()-1) % NCOLS[hit_n-1];
            hitADC = hit->adc();
            hitTDC = hit->tdc();

            // encode SavedHits
            p1 =                    0x80000000;
            p2 = ((hit_n-1)&7)    * 0x10000000;
            p3 = ((hit_r-1)&0x3F) * 0x00400000;
            p4 = ((hit_c-1)&0x3F) * 0x00010000;
            SavedHits[nSavedHits] = (hitADC&0xFFFF) + p1 + p2 + p3 + p4; // -->p_out

            // encode timing (TDC values)
            SavedTDC[nSavedHits] = (UChar_t) hitTDC; // -->p_out
            cluSumTDC += (Float_t) hitTDC;
            cluHitCount++;

            // increment nSavedHits
            nSavedHits++; // -->p_out

            if(verbose_pho) {
              LOG_INFO << " SavedHits loop: (n" << hit_n << ",r" << hit_r << ",c" << hit_c << ")  adc="
                       << hitADC << " SavedHits[" << nSavedHits-1 << "]="
                       << Form("0x%8x",SavedHits[nSavedHits-1])
                       << endm;
            };
          }
          else {
            if(verbose_pho) LOG_INFO << "            hit " << hi+1 
                                     << " -- NULL POINTER... potential issue??" << endm;
          };
        }; // end cluster's hit loop


        // compute cluster's average TDC
        if(cluHitCount>0) cluAveTDC = cluSumTDC / cluHitCount;
        else cluAveTDC = 0;
        SavedCluTDC[nCluster] = cluAveTDC; // -->p_out

        nCluster++; // -->p_out

      }; // end sorted cluster loop
    }; // end if ent[nn]>0
  }; // end NSTB loop
  // END MAIN CLUSTER LOOP ================================
 
  
  // fill 4-momentum array "pxyzt" for p_out ============================================================
  int wcnt = 0;
  for(int kk=0; kk<nwrds; kk++) {
    pxyzt[wcnt++] = vecs[kk].px(); // -->p_out
    pxyzt[wcnt++] = vecs[kk].py(); // -->p_out
    pxyzt[wcnt++] = vecs[kk].pz(); // -->p_out
    pxyzt[wcnt++] = vecs[kk].e(); // -->p_out
  };

  // nwrds = (3+1 dimensions) * (number of valid photons + number of NSTBs which pass esum-cut)
  nwrds *= 4; // -->p_out

  


  //////////////////////////////////////////////////////////////////////////////////
  //
  //    RP Sector 
  //
  //////////////////////////////////////////////////////////////////////////////////


  // get number of tracks/trackpoints
  n_tracks = mMuRpsColl->numberOfTracks();
  n_trackpoints = mMuRpsColl->numberOfTrackPoints();


  // reset track and track point variables for this event
  for(int i=0; i<N_TRACKS_MAX; i++) ResetRpTrackVars(i);
  

  // check if n_tracks and n_trackpoints are sane
  if(n_tracks>N_TRACKS_MAX) {
    LOG_INFO << "WARNING WARNING WARNING -- n_tracks > N_TRACKS_MAX; only analysing first " 
            << N_TRACKS_MAX << " tracks (also will set n_trackpoints to -1)" << endm;
    n_tracks = N_TRACKS_MAX;
    n_trackpoints = -1;
  }
  if(n_trackpoints>2*N_TRACKS_MAX) {
    LOG_ERROR << "WARNING WARNING WARNING -- n_trackpoints > 2 * N_TRACKS_MAX; setting to -1" << endm;
    n_trackpoints = -1;
  };


  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  // pre-track loop, for sorting tracks  -- DEPRECATED
  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  /*
  if(n_tracks>0) {
    for(int i = 0; i<n_tracks; i++) {
      trk = mMuRpsColl->track(i);
      if(trk) {
        trkPlanes = trk->planesUsed();
        trkMom = trk->p();
        trkThetaX = trk->thetaRp(StMuRpsTrack::rpsAngleThetaX);
        trkThetaY = trk->thetaRp(StMuRpsTrack::rpsAngleThetaY);

        trkGlobal = (trk->type()==StMuRpsTrack::rpsGlobal) ? 1:0;
        if(trkThetaX > thetaRpLimits[0][0] && trkThetaX < thetaRpLimits[0][1] &&
           TMath::Abs(trkThetaY) > thetaRpLimits[1][0] && TMath::Abs(trkThetaY) < thetaRpLimits[1][1])
          trkInGeom = 1;
        else trkInGeom=0;

        // bound momentum in order to get reasonable track hash for sorting
        if(trkMom<0) {
          LOG_ERROR << "WARNING WARNING WARNING -- trkMom<0" << endm;
          trkMom=0;
        }

        // suppress tracks with nonsensical momenta (c.f. stronger cut later, where t_isBad is set)
        if(trkMom>300 || trkMom<0) {
          trkMom=0;
          trkPlanes=0;
          trkGlobal=0;
          trkInGeom=0;
        };

        // hash track kinematics etc. into a number for sorting
        // hash = Min(trkMom,300) + 1,000*trkPlanes + 10,000*trkGlobal + 20,000*trkInGeom
        // -- trkGlobal=0 && trkInGeom=0  --> hash in range  2,000 -  8,300
        // -- trkGlobal=0 && trkInGeom=1  --> hash in range 12,000 - 18,300
        // -- trkGlobal=1 && trkInGeom=0  --> hash in range 22,000 - 28,300
        // -- trkGlobal=1 && trkInGeom=1  --> hash in range 32,000 - 38,300
        //  -- trkMom bound between 0 and 300 (to prevent bad hash of nonsensical momenta)
        //  -- nplanes in a single track point is >=2; nplanes in a single track is <=8
        trkSortQual[i] = trkMom + 
                        1000*trkPlanes + 
                        10000*trkInGeom +
                        20000*trkGlobal;
      }; // end if(trk)
    }; // end track loop
    
    // sort tracks
    TMath::Sort(N_TRACKS_MAX,trkSortQual,trkSortIdx);

    // dump sort table (FOR DEBUGGING)
    if(true) {
      for(int i = 0; i<n_tracks; i++) {
        LOG_INFO << std::setprecision(20) << "trkSortIdx[" << i << "]=" << trkSortIdx[i] <<
                    "  trkSortQual[" << trkSortIdx[i] << "]=" << trkSortQual[trkSortIdx[i]] <<
                    endm;
      };
    };

  }; // end if(n_tracks>0)
  */

  

  // ----------------------------------------------
  // main track loop
  // ----------------------------------------------
  tc = 0; // reset valid track counter
  if(n_tracks>0) {
    for(int i = 0; i<n_tracks; i++) {
      if(verbose_rp) LOG_INFO << "for i="<<i<<endm;

      // reset trackpoint variables for this track
      ResetRpTrackPointVars(tc);

      // select next track from sorted list -- DEPRECATED
      //if(trkSortIdx[i]<0) LOG_ERROR << "ERROR: negative trkSortIdx" << endm;
      //if(trkSortIdx[i]>N_TRACKS_MAX) LOG_ERROR << "ERROR: trkSortIdx > N_TRACKS_MAX" << endm;
      //trk = mMuRpsColl->track(trkSortIdx[i]);
      //t_qualHash[i] = trkSortQual[trkSortIdx[i]]; // deprecated

      // select next track (from not-sorted list)
      trk = mMuRpsColl->track(i);

      
      if(trk) {

        // fill track leaves
        t_index[tc] = i; // -->p_out
        t_branch[tc] = trk->branch(); // -->p_out
        t_type[tc] = (Int_t)(trk->type()); // -->p_out
        t_planesUsed[tc] = trk->planesUsed(); // -->p_out
        t_p[tc] = trk->p(); // -->p_out
        t_pt[tc] = trk->pt(); // -->p_out
        t_eta[tc] = trk->eta(); // -->p_out
        t_time[tc] = trk->time(); // -->p_out
        

        for(int a=0; a<3; a++) {
          t_theta[tc][a] = trk->theta(a); // -->p_out
          t_thetaRP[tc][a] = trk->thetaRp(a); // -->p_out
        };


        t_phi[tc] = trk->phi(); // -->p_out
        t_phiRP[tc] = trk->phiRp(); // -->p_out
        t_t[tc] = trk->t( runInfo.beamEnergy(trk->branch()<2 ? StBeamDirection::east : StBeamDirection::west) ); // -->p_out
        t_xi[tc] = trk->xi( runInfo.beamEnergy(trk->branch()<2 ? StBeamDirection::east : StBeamDirection::west) ); // -->p_out


        // loop through this track's trackpoints=======================================
        // - tc is the track counter, tpc[tc] is the trackpoint counter for track tc
        for(int j=0; j<2; j++) {
          trkpnt = trk->trackPoint(j);
          if(trkpnt) {
            p_tpExists[tc][tpc[tc]] = true; // -->p_out
            p_RPid[tc][tpc[tc]] = trkpnt->rpId(); // -->p_out
            p_clustid_s1[tc][tpc[tc]] = trkpnt->clusterId(0); // -->p_out
            p_clustid_s2[tc][tpc[tc]] = trkpnt->clusterId(1); // -->p_out
            p_clustid_s3[tc][tpc[tc]] = trkpnt->clusterId(2); // -->p_out
            p_clustid_s4[tc][tpc[tc]] = trkpnt->clusterId(3); // -->p_out
            p_quality[tc][tpc[tc]] = (Int_t)(trkpnt->quality()); // -->p_out
            p_planesUsed[tc][tpc[tc]] = trkpnt->planesUsed(); // -->p_out
            p_x[tc][tpc[tc]] = trkpnt->x(); // -->p_out
            p_y[tc][tpc[tc]] = trkpnt->y(); // -->p_out
            p_z[tc][tpc[tc]] = trkpnt->z(); // -->p_out
            p_time_pmt1[tc][tpc[tc]] = trkpnt->time(0); // -->p_out
            p_time_pmt2[tc][tpc[tc]] = trkpnt->time(1); // -->p_out
            tpc[tc]++; // -->p_out
          }; // end if(trkpnt)
        }; // end loop through this track's track points


        // suppress possible nonsense values, which marks track as t_isBad=true and resets all RP branches
        t_isBad[tc]=false;
        if(t_branch[tc]<0 || t_branch[tc]>3) t_isBad[tc]=true;
        if(t_type[tc]<0 || t_type[tc]>2) t_isBad[tc]=true;
        if(t_planesUsed[tc]<0 || t_planesUsed[tc]>8) t_isBad[tc]=true;
        if(t_p[tc]<0 || t_p[tc]>300) t_isBad[tc]=true;
        if(t_pt[tc]<0 || t_pt[tc]>300) t_isBad[tc]=true;
        if(t_eta[tc]<-30 || t_eta[tc]>30) t_isBad[tc]=true;
        if(t_xi[tc]<-3 || t_xi[tc]>3) t_isBad[tc]=true;
        if(t_t[tc]<-1000 || t_t[tc]>0) t_isBad[tc]=true;

        if(t_isBad[tc]) ResetRpTrackVars(tc);
        else tc++; // increment tc iff !isBad

      }; // end if(track)
    }; // end track loop
  }; // end if n_tracks>0


  //////////////////////////////////////////////////////////////////////////////////
  //
  //    MISC Sector 
  //
  //////////////////////////////////////////////////////////////////////////////////
  //printf("dsEvent::triggerData @ %p\n",(void*)dsEvent->triggerData());
  //printf("dsEvent::triggerDetectorCollection @ %p\n",(void*)dsEvent->triggerDetectorCollection());

  // BBC, ZDC, VPD branches
  for(int ew=0; ew<2; ew++) {
    // BBC
    bbcADCSum[ew] = muEvent->triggerData()->bbcADCSum((StBeamDirection)ew);
    bbcADCSumLarge[ew] = muEvent->triggerData()->bbcADCSumLargeTile((StBeamDirection)ew);
    bbcEarliestTDC[ew] = muEvent->triggerData()->bbcEarliestTDC((StBeamDirection)ew);
    bbcEarliestTDCLarge[ew] = muEvent->triggerData()->bbcEarliestTDCLarge((StBeamDirection)ew);
    // ZDC
    zdcADCSum[ew] = 0;
    for(int pmt=1; pmt<=3; pmt++) {
      zdcADCSum[ew] += muEvent->triggerData()->zdcADC((StBeamDirection)ew,pmt); // is this correct?
    };
    // VPD
    vpdADCSum[ew] = 0;
    for(int pmt=1; pmt<=16; pmt++) {
      vpdADCSum[ew] += muEvent->triggerData()->vpdADC((StBeamDirection)ew,pmt); // is this correct?
    };
  }; // end for(ew)

  // TOF
  tofMultiplicity = muEvent->triggerData()->tofMultiplicity();

  // MTD (doesn't work?)
  /*
  for(int ew=0; ew<2; ew++) {
    mtdADC[ew] = muEvent->triggerData()->mtdAdc((StBeamDirection)ew,0); // (only pmt=0 used)
  };
  */

  // BEGIN TEST
  /*
  printf("\n\n TEST\n\n");
  StTriggerData2013 * td = (StTriggerData2013*) muEvent->triggerData();
  printf("%p\n",(void*)td->EvtDesc);
  printf("year=%d\n",muEvent->triggerData()->year());
  printf("\n\n END TEST\n\n");
  */

  // fill the deprecated p_out branches ===========================================

  EventN = 0; // -->p_out  // unused in OFile->Output chain, just leave at 0
  for(int bb=0; bb<5; bb++) BBcSums[bb]=0; // -->p_out
  for(int bb=0; bb<7; bb++) BBcVertex[bb]=0; // -->p_out
  for(int bb=0; bb<8; bb++) 
    Fpde[bb] = muEvent->triggerData()->fpdLayer1DSMRaw(StBeamDirection::west,bb); // -->p_out




  // finally fill the trees and print some arrays ===============================================
  if(nwrds>0) {
    if(build_evtr) evtr->Fill(); // -->evtr
    p_out->Fill(); // -->p_out
    if(verbose_pho) {
      LOG_INFO << "nSavedHits=" << nSavedHits << endm;
      LOG_INFO << "nPhotonClu=" << nPhotonClu << endm;
      LOG_INFO << "TYPES AND PXYZT ARRAYS -----------------------" << endm;
      for(int kk=0; kk<nwrds; kk++) {
        LOG_INFO << "     ent = " << kk
                 << "  types = " << tpes[kk]
                 << "  pxyzt = " << pxyzt[kk]
                 << endm;
      };
    };
  };

  // <-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><->
  //
  //  DONE FILLING TREES 
  //
  // <-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><-><->



  // print some useful statistics for this event
  if(verbose) {
    // note: diff is npoints-nphotons, where npoints is the total number of points
    // and nphotons is the number of photons added to p_out; this number can differ
    // because of the above esum-cut
    LOG_INFO << ">>>>>   "
             << " nhits=" << nhits
             << " nclusters=" << nclusters
             << " npoints=" << npoints
             << " nphotons=" << nphotons
             << " diff=" << npoints-nphotons
             << " num_null=" << num_null
             << endm;
  };

  // print list of photons (for comparing to photons in clusters)
  // -- sometimes you'll see photons in this list which aren't in the list of photons
  //    within each cluster, collected above; these are just the photons in clusters which
  //    did not pass esum-cut
  if(verbose_pho) {
    LOG_INFO << "POINT LOOP" << endm;
    for(int pi=0; pi<npoints; pi++) {
      pho = dsPoints[pi];
      if(pho) {
        LOG_INFO << " *** found photon: id=" << pho->id() 
                 << " en=" << pho->energy() 
                 << " nstb=" << pho->detectorId()-7
                 << " clu_id=" << pho->cluster()->id()
                 << endm;
      }
      else LOG_ERROR << "*** WARNING: null pointer to photon in dsFmsColl, entry " << pi << endm;
    };
  };


  // clean up ========================================================================
  // delete pointers to memory which was allocated specifically for this event
  for(int nn=0; nn<4; nn++) {
    if(clusterArr[nn]) delete clusterArr[nn];
  };

  // reset afterburner
  mAfterburner->clear(); // CRITICAL! this must be done at the end of the event!

  return kStOK;
};


// reset track (and its trackpoint) branches for track i_
void StFmsOFileMaker::ResetRpTrackVars(Int_t i_) {
  t_index[i_]=-1;
  t_branch[i_]=-1;
  t_type[i_]=-1;
  t_planesUsed[i_]=0;
  t_p[i_]=-1;
  t_pt[i_]=-1;
  t_eta[i_]=-100;
  t_time[i_]=-1;

  for(int a=0; a<3; a++) {
    t_theta[i_][a] = -1;
    t_thetaRP[i_][a] = -1;
  };

  t_phi[i_]=-100;
  t_phiRP[i_]=-100;
  t_t[i_]=-1000;
  t_xi[i_]=-1000;
  //t_isBad[i_]=true; // DO NOT RESET isBad HERE

  /* (deprecated)
  //t_qualHash[i_]=-1000;
  //trkSortQual[i_]=-1000;
  //trkSortIdx[i_]=-1;
  */

  ResetRpTrackPointVars(i_);
};


// reset trackpoint branches for track i_
void StFmsOFileMaker::ResetRpTrackPointVars(Int_t i_) {
  tpc[i_] = 0; // reset valid trackpoint counter
  for(int j=0; j<2; j++) {
    p_tpExists[i_][j]=false;
    p_RPid[i_][j]=-1;
    p_clustid_s1[i_][j]=-1;
    p_clustid_s2[i_][j]=-1;
    p_clustid_s3[i_][j]=-1;
    p_clustid_s4[i_][j]=-1;
    p_quality[i_][j]=-1;
    p_planesUsed[i_][j]=0; 
    p_x[i_][j]=-1000;
    p_y[i_][j]=-1000;
    p_z[i_][j]=-1000;
    p_time_pmt1[i_][j]=-1;
    p_time_pmt2[i_][j]=-1;
  };
};


void StFmsOFileMaker::ResetFmsBranchVars() {

  // reset cluster & hit counters & arrays for p_out
  nwrds = 0; 
  nSavedHits = 0;
  nCluster = 0;
  nPhotonClu = 0;
  nphotons = 0;
  num_null = 0;
  for(int ii=0; ii<NCELLS; ii++) {
    tpes[ii] = 0;
    pxyzt[ii] = 0.0;
    SavedHits[ii] = 0;
    SavedTDC[ii] = 0;
    SavedCluTDC[ii] = 0;
    SCIndex[ii] = 0;
    SPCIndex[ii] = 0;
    SPCEnergy[ii] = 0.0;
    fpsPid[ii] = -1;
  };

  // variables upon which some p_out branches depend
  en_sum_all=0;
  for(int nn=0; nn<4; nn++) {
    en_sum[nn]=0;
    eMat[nn]->Zero(); // reset energy matrix
  };

  // counters
  hc = 0;  // hits
  cc = 0; // clusters
  pc = 0; // points
  ppc = 0; // pairs

  // evtr 
  for(int c=0; c<NCELLS; c++) {
    if(build_evtr) {
      // evtr hits
      hit_nstb[c] = -1;
      hit_chan[c] = -1;
      hit_row[c] = -1;
      hit_col[c] = -1;
      hit_adc[c] = -1;
      hit_en[c] = -1;
      gain[c] = -1;
      gainCorr[c] = -1;
      bitshift[c] = -10;
      adcCorrected[c] = -1;
      hit_tdc[c] = -1;
      //timeDepCorr[c] = -1;
      
      // evtr clusters
      clu_id[c] = -1;
      clu_en[c] = -1;
      clu_x[c] = -1000;
      clu_y[c] = -1000;
      clu_sigmin[c] = -1;
      clu_sigmax[c] = -1;
      clu_csqn1[c] = -1;
      clu_csqn2[c] = -1;
      clu_category[c] = -1;
      clu_ntowers[c] = -1;
      clu_nphotons[c] = -1;
      clu_nstb[c] = -1;

      // evtr points
      pho_id[c] = -1;
      pho_en[c] = -1;
      pho_x[c] = -1000;
      pho_y[c] = -1000;
      pho_px[c] = -1000;
      pho_py[c] = -1000;
      pho_pz[c] = -1000;
      pho_nstb[c] = -1;
      pho_fpsPid[c] = -1;

      // evtr pairs
      pair_en[c] = -1;
      pair_pt[c] = -1;
      pair_eta[c] = -1;
      pair_phi[c] = -1000;
      pair_mass[c] = -1000;
      pair_dgg[c] = -1;
      pair_zgg[c] = -1;
      pair_x[c] = -1000;
      pair_y[c] = -1000;
      pair_fpsPid[c] = 0;
      for(int con=0; con<3; con++) {
        pair_coneRad[con][c] = -1;
        pair_coneEn[con][c] = -1;
        pair_coneEnFrac[con][c] = -1;
      };
    };
  }; // end loop over c to NCELLS
};
