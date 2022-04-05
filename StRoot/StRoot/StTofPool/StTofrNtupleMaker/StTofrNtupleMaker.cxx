/*******************************************************************
 *
 * $Id: StTofrNtupleMaker.cxx,v 1.13 2018/02/26 23:26:51 smirnovd Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: example maker to get the matched TOFr cells and fill
 *              into TOFr tree.
 *
 *******************************************************************/
#include <iostream>
#include <math.h>
#include <vector>
#include <stdlib.h>
#include <stdio.h>
#include <algorithm>
#include <iterator>

#include "StEventTypes.h"
#include "Stypes.h"
#include "StThreeVectorF.hh"
#include "StMeasuredPoint.h"
#include "StDedxPidTraits.h"
#include "StTrackPidTraits.h"
#include "StarClassLibrary/StParticleTypes.hh"
#include "StarClassLibrary/StParticleDefinition.hh"
#include "StMuDSTMaker/COMMON/StMuUtilities.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StHelix.hh"
#include "StTrackGeometry.h"
#include "StParticleTypes.hh"
#include "StTpcDedxPidAlgorithm.h"
#include "StHit.h"
#include "StAssociationMaker/StTrackPairInfo.hh"
#include "StEventUtilities/StuRefMult.hh"
#include "PhysicalConstants.h"
#include "StPhysicalHelixD.hh"
#include "TTree.h"
#include "TFile.h"
#include "StMemoryInfo.hh"
#include "StMessMgr.h"
#include "StTimer.hh"
#include "tables/St_pvpdStrobeDef_Table.h"
#include "tables/St_g2t_vertex_Table.h" // tmp for Dz(vertex)
#include "tables/St_vertexSeed_Table.h" //

#include "StTofUtil/tofPathLength.hh"
#include "StTofUtil/StTofrGeometry.h"
#include "StTofUtil/StTofrDaqMap.h"
#include "StTofUtil/StTofCellCollection.h"
#include "StTofUtil/StTofHitCollection.h"
#include "StTofrNtupleMaker.h"



//---------------------------------------------------------------------------
/// constructor sets default parameters
StTofrNtupleMaker::StTofrNtupleMaker(const Char_t *name="tofrNtuple", const Char_t *outname="tofntuple.root") : StMaker(name) {
  mTupleFileName=outname;
  
  doPrintMemoryInfo = kFALSE;
  doPrintCpuInfo    = kFALSE;
}

/// default empty destructor
StTofrNtupleMaker::~StTofrNtupleMaker(){ /* nope */}

//---------------------------------------------------------------------------
/// initialize ntuple and daqmap, and reset counters
Int_t StTofrNtupleMaker::Init(){

  if (mTupleFileName!="") bookNtuples();

  mAcceptedEvents = 0;
  mPvpdEntries = 0;
  mTofrEvents  = 0;
  mTofrEntries = 0;

  return kStOK;
}


Int_t StTofrNtupleMaker::InitRun(int runnumber) {

  if(mInitGeomFromOther) {
    TDataSet *geom = GetDataSet("tofrGeometry");
    mTofrGeom = (StTofrGeometry *)geom->GetObject();
  } else {
    mTofrGeom = new StTofrGeometry("tofrGeoNtuple","tofGeo in NtupleMaker");
    if(!mTofrGeom->IsInitDone()) {
      gMessMgr->Info("TofrGemetry initialization..." ,"OS");
      TVolume *starHall = (TVolume *)GetDataSet("HALL");
      mTofrGeom->Init(starHall);
    } 
  }

  gMessMgr->Info("    -- retrieving run parameters from Calibrations_tof","OS");
  TDataSet *mDbDataSet = GetDataBase("Calibrations/tof/pvpdStrobeDef");
  if (!mDbDataSet){
    gMessMgr->Error("unable to get TOF run parameters","OS");
    assert(mDbDataSet);
  }
  St_pvpdStrobeDef* pvpdStrobeDef = static_cast<St_pvpdStrobeDef*>(mDbDataSet->Find("pvpdStrobeDef"));
  if (!pvpdStrobeDef){
    gMessMgr->Error("unable to find TOF run param table","OS");
    assert(pvpdStrobeDef);
  }
  pvpdStrobeDef_st *strobeDef = static_cast<pvpdStrobeDef_st*>(pvpdStrobeDef->GetArray());
  int numRows = pvpdStrobeDef->GetNRows();
  if (mNPVPD != numRows) gMessMgr->Warning("#tubes inconsistency in dbase");
  for (int i=0;i<mNPVPD;i++){
    int ii = strobeDef[i].id - 1;
    mStrobeTdcMin[ii] = strobeDef[i].strobeTdcMin;
    mStrobeTdcMax[ii] = strobeDef[i].strobeTdcMax;
    if (Debug())
      LOG_INFO << "tube " << strobeDef[i].id << "  min:"<< strobeDef[i].strobeTdcMin
	   <<" max:"<< strobeDef[i].strobeTdcMax<< endm;
  }

  //========== Set Beam Line ===================== 
  double x0 = 0.;
  double y0 = 0.;
  double dxdz = 0.;
  double dydz = 0.;

  // Get Current Beam Line Constraint from database
  /*TDataSet* dbDataSet = this->GetDataBase("Calibrations/rhic");

  if (dbDataSet) {
    vertexSeed_st* vSeed = ((St_vertexSeed*) (dbDataSet->FindObject("vertexSeed")))->GetTable();

  x0 = vSeed->x0;
  y0 = vSeed->y0;
  dxdz = vSeed->dxdz;
  dydz = vSeed->dydz;
  }
  else {
    LOG_INFO << "StGenericVertexMaker -- No Database for beamline" << endm;
  }
  */
  LOG_INFO<< " beamline is set by hand "<<endl;

  LOG_INFO << "BeamLine Constraint: " << endm;
  LOG_INFO << "x(z) = " << x0 << " + " << dxdz << " * z" << endm;
  LOG_INFO << "y(z) = " << y0 << " + " << dydz << " * z" << endm;
  
  //beam line not be calibrated yet
  //x0 shift by 0.5
  //x0 = 0.5;
 
  StThreeVectorD origin(x0,y0,0.0);
  double pt = 88889999;
  double nxy=::sqrt(dxdz*dxdz +  dydz*dydz);
  if(nxy<1.e-5){ // beam line _MUST_ be tilted
    LOG_WARN << "StppLMVVertexFinder:: Beam line must be tilted!" << endm;
    nxy=dxdz=1.e-5;
  }
  double p0=pt/nxy;
  double px   = p0*dxdz;
  double py   = p0*dydz;
  double pz   = p0; // approximation: nx,ny<<0
  StThreeVectorD MomFstPt(px*GeV, py*GeV, pz*GeV);
  delete mBeamHelix;
  mBeamHelix = new StPhysicalHelixD(MomFstPt,origin,0.5*tesla,1.);

  mBeamX = x0;
  mBeamY = y0;

  return kStOK;
}

Int_t StTofrNtupleMaker::FinishRun(int runnumber)
{
  if(!mInitGeomFromOther) {
    if(mTofrGeom) delete mTofrGeom;
  }
  mTofrGeom = 0;
  delete mBeamHelix;
  
  return kStOK;
}


/// write and close the ntuple file
Int_t StTofrNtupleMaker::Finish() {

  if (!(mTupleFileName=="")){
    mTupleFile->Write();
    mTupleFile->Close();
    LOG_INFO << "StTofrNtupleMaker::Finish() ntuple file " 
	 << mTupleFileName  << " closed." << endm;
  }
 
  //delete mPvpdTuple;
  //delete mCellTuple;
  //delete mTupleFile;
 
  LOG_INFO << "StTofrNtupleMaker -- statistics" << endm;
  LOG_INFO << " accepted events     : " << mAcceptedEvents << endm;
  LOG_INFO << " pVPD entries        : " << mPvpdEntries << endm;
  LOG_INFO << " Tofr entries/events : " << mTofrEntries << "/" << mTofrEvents << endm;
  return kStOK;
}


//---------------------------------------------------------------------------
/// get tofr slat, pvpd rawdata and global data from StEvent and store in flat TTrees (ntuples)
Int_t StTofrNtupleMaker::Make(){
  LOG_INFO << "StTofrNtupleMaker -- welcome" << endm;

  StEvent *event = (StEvent *) GetInputDS("StEvent");

  //.........................................................................
  // event selection ...
  if (!event || 
      //!event->primaryVertex() ||
      !event->tofCollection() ||
      //!event->tofCollection()->dataPresent()
      !event->tofCollection()->rawdataPresent()){
    LOG_INFO << "StTofrNtupleMaker -- nothing to do ... bye-bye"<< endm;
    return kStOK;
  }

  mAcceptedEvents++;
  StTimer timer;
  if (doPrintCpuInfo) timer.start();
  if (doPrintMemoryInfo) StMemoryInfo::instance()->snapshot();

  // determine TOF configuration from run#
  mYear2= (event->runId()<4000000);
  mYear3= (event->runId()>4000000&&event->runId()<5000000);
  mYear4= (event->runId()>5000000);


  //.........................................................................
  // Collect global data for both ntuples

  //-- Primary vertex & trigger information
cout<<"runId: "<<event->runId()<<"  runnumber"<<event->id()<<endl;
  float xvtx = -999.;
  //xvtx = event->primaryVertex()->position().x();
  float yvtx = -999.;
  //yvtx = event->primaryVertex()->position().y();
  float zvtx = -999.;
  //zvtx = event->primaryVertex()->position().z();

  StL0Trigger* pTrigger = event->l0Trigger();
  unsigned int triggerWord = 0;
  if (pTrigger) triggerWord = pTrigger->triggerWord();
  
  //-------- fill event summary info -----------
  mCellData.run = event->runId();    // the run number
  mCellData.evt = event->id();       // the event number
  mCellData.trgword = triggerWord;
  mCellData.vertexX = xvtx;        
  mCellData.vertexY = yvtx;              
  mCellData.vertexZ = zvtx;              
 
  if(Debug()){
    LOG_INFO << " vertexZ: "<<mCellData.vertexZ<<endm;
  } 

  //-- read in TOFr info
  //-- make sure tofSlats are available
  StTofCollection *theTof = event->tofCollection();
  if (event->tofCollection()->cellsPresent()){

    mTofrEvents++;
    int entriesThisEvent(0);
    
    //initialize vpd content
    for(int i=0;i<19;i++){
      mCellData.pvpdLeadingEdgeTimeEast[i] = 0;
      mCellData.pvpdTotEast[i] = 0;
      mCellData.pvpdLeadingEdgeTimeWest[i] = 0;
      mCellData.pvpdTotWest[i] = 0;
    } 
    
    //-- Loop over the cell container and retrieve the relevant parameters
    StSPtrVecTofCell& cellTofVec = theTof->tofCells();
    int ntofhits = 0;
    float tdcsumeast = 0., tdcsumwest = 0.;
    unsigned int vpdEast=0, vpdWest=0, nVpdEast=0, nVpdWest=0;
    for (size_t i = 0; i < cellTofVec.size(); i++) {
      StTofCell *thisCell = cellTofVec[i];
      int trayId = thisCell->trayIndex();
      if(Debug()) LOG_INFO << " tray ID: "<< trayId<<endm;
      if(trayId==122){ //pvpd east)
        int tubeId = thisCell->cellIndex()-1;
        vpdEast += 1<<tubeId;
        nVpdEast++;

        mCellData.pvpdLeadingEdgeTimeEast[tubeId] = thisCell->leadingEdgeTime();
        mCellData.pvpdTotEast[tubeId] = thisCell->tot();
        cout<<" tray/tube "<< trayId<<"/"<<tubeId<<" letime/tot "<< thisCell->leadingEdgeTime()<<"/"<< thisCell->tot()<<endl;
        tdcsumeast += thisCell->leadingEdgeTime();
        mPvpdEntries++;
      }else if(trayId==121){ //pvpd west
        int tubeId = thisCell->cellIndex()-1;
        vpdWest += 1<<tubeId;
        nVpdWest++;
        mCellData.pvpdLeadingEdgeTimeWest[tubeId] = thisCell->leadingEdgeTime();
        mCellData.pvpdTotWest[tubeId] = thisCell->tot();

        tdcsumwest += thisCell->leadingEdgeTime();
        mPvpdEntries++;
      }else if(trayId<=120&&trayId>=0){ //tofr
//        if(!(thisCell->matchFlag())) continue;
        mCellData.tray[ntofhits] = trayId;
        mCellData.module[ntofhits] = thisCell->moduleIndex();
        mCellData.cell[ntofhits] = thisCell->cellIndex();
        mCellData.daq[ntofhits] = thisCell->daqIndex();
        mCellData.leadingEdgeTime[ntofhits] = thisCell->leadingEdgeTime();
//        mCellData.leadingEdgeTime[ntofhits] = 0.025*thisCell->tdc();//for run5
        mCellData.tot[ntofhits] = thisCell->tot();
//        mCellData.tot[ntofhits] = 0.1*thisCell->adc() - 0.025*thisCell->tdc();//for run5

        //mCellData.matchFlag[ntofhits] = thisCell->matchFlag();  
 
        //- hit local position
        if(Debug()) LOG_INFO << " trayID/moduleID/cellID/leadingEdgeTime/tot"<< trayId <<"/"<< mCellData.module[ntofhits]<<"/"<< mCellData.cell[ntofhits] << "/" << mCellData.leadingEdgeTime[ntofhits] << "/"<< mCellData.tot[ntofhits]<<"/"<<endm;

        StThreeVectorD globalHit = thisCell->position();
        StTofrGeomSensor* sensor = mTofrGeom->GetGeomSensor(thisCell->moduleIndex(), thisCell->trayIndex());
        double local[3], globalp[3];
        globalp[0] = globalHit.x();
        globalp[1] = globalHit.y();
        globalp[2] = globalHit.z();
        sensor->Master2Local(&globalp[0], &local[0]);
        StThreeVectorD localHit(local[0], local[1], local[2]);
        float ycenter = (thisCell->cellIndex()-1)*3.45-8.625;
        delete sensor;
       
       cout<<"zHit local = "<<local[2]<<"  "<< thisCell->zHit()<<endl; 
        mCellData.xlocal[ntofhits] = (Float_t) localHit.x();
        mCellData.ylocal[ntofhits] = (Float_t) localHit.y();
        mCellData.zlocal[ntofhits] = (Float_t) localHit.z();
        mCellData.deltay[ntofhits] = local[1] - ycenter;

        if(Debug()){
           LOG_INFO <<" global:("<<globalp[0]<<", "<<globalp[1]<<", "<<globalp[2]<<")"
                <<"\n \t local:("<<localHit.x()<<", "<<localHit.y()<<", "<<localHit.z()<<")"<<endm;
        }

        //- track information
        StTrack *thisTrack = thisCell->associatedTrack();
        //StTrack *globalTrack = thisTrack->node()->track(global);

        StTrackGeometry *theTrackGeometry = thisTrack->geometry();
        const StThreeVectorF momentum = theTrackGeometry->momentum();
        //- dig out from the dedx and rich pid traits
        float dedx(0.), cherang(0), dedxerror(0);
        int dedx_np(0), cherang_nph(0);
        StSPtrVecTrackPidTraits& traits = thisTrack->pidTraits();
        for (unsigned int it=0;it<traits.size();it++){
	  if (traits[it]->detector() == kTpcId){
	    StDedxPidTraits *pid = dynamic_cast<StDedxPidTraits*>(traits[it]);
	    if (pid && pid->method() ==kTruncatedMeanId){
	      dedx    = pid->mean()*1e6;
	      dedx_np =  pid->numberOfPoints();
	      dedxerror = pid->errorOnMean()*1e6;
	    }
	  } else if  (traits[it]->detector() == kRichId){
	    StRichPidTraits *pid = dynamic_cast<StRichPidTraits*>(traits[it]);
	    if (pid){ 
	      StRichSpectra* pidinfo = pid->getRichSpectra();
	      if (pidinfo && pidinfo->getCherenkovPhotons()>2){
	        cherang     = pidinfo->getCherenkovAngle();
	        cherang_nph = pidinfo->getCherenkovPhotons();
	      }
	    }
	  }
        }
   
        StThreeVector<double> tofPos =  theTrackGeometry->helix().at(theTrackGeometry->helix().pathLengths(*mBeamHelix).first);
        StThreeVector<double> dcatof = tofPos - mBeamHelix->at(theTrackGeometry->helix().pathLengths(*mBeamHelix).second);//real dca

        float mNSigmaElectron(0.);
        float mNSigmaPion(0.);
        float mNSigmaKaon(0.);
        float mNSigmaProton(0.); 
        
        static StTpcDedxPidAlgorithm PidAlgorithm;
        static StElectron* Electron = StElectron::instance();
        static StPionPlus* Pion = StPionPlus::instance();
        static StKaonPlus* Kaon = StKaonPlus::instance();
        static StProton* Proton = StProton::instance();
        const StParticleDefinition* pd = thisTrack->pidTraits(PidAlgorithm);
        
        if (pd) {
          mNSigmaElectron = fabsMin(PidAlgorithm.numberOfSigma(Electron), __SIGMA_SCALE__);
          mNSigmaPion =     pack2Int( fabsMin(PidAlgorithm.numberOfSigma(Pion),__SIGMA_SCALE__),    __SIGMA_SCALE__ );
          mNSigmaKaon =     pack2Int( fabsMin(PidAlgorithm.numberOfSigma(Kaon),__SIGMA_SCALE__),     __SIGMA_SCALE__ );
          mNSigmaProton =   pack2Int( fabsMin(PidAlgorithm.numberOfSigma(Proton),__SIGMA_SCALE__),   __SIGMA_SCALE__ );
        }
        
        double pathLength = -999.;
        //tofPathLength(&event->primaryVertex()->position(),
        //  				&thisCell->position(),
        //  				theTrackGeometry->helix().curvature());
        pathLength = tofPathLength(&tofPos, &thisCell->position(), theTrackGeometry->helix().curvature());
        if(Debug()) LOG_INFO << "dca(x,y,z) = (" << dcatof.x() <<", " << dcatof.y() 
                             <<", " << dcatof.z() << "), tof pathLength = " << pathLength<<endm;
        mCellData.trackId[ntofhits]     = (Int_t)thisTrack->key();
        mCellData.charge[ntofhits]      = theTrackGeometry->charge();
        if(thisTrack->detectorInfo()) {
          mCellData.nHits[ntofhits] = thisTrack->detectorInfo()->numberOfPoints(kTpcId);
        } else {
          mCellData.nHits[ntofhits] = 0;
        }
        mCellData.nHitsFit[ntofhits]    = thisTrack->fitTraits().numberOfFitPoints(kTpcId);
        mCellData.dcaX[ntofhits]         = dcatof.x();
        mCellData.dcaY[ntofhits]         = dcatof.y();
        mCellData.dcaZ[ntofhits]         = tofPos.z();
        //globalTrack->geometry()->helix().distance(event->primaryVertex()->position());
        mCellData.length[ntofhits]      = (float)fabs(pathLength);
        mCellData.p[ntofhits]           = momentum.mag();
        mCellData.pt[ntofhits]	        = momentum.perp();
        mCellData.px[ntofhits]          = momentum.x();
        mCellData.py[ntofhits]          = momentum.y();
        mCellData.pz[ntofhits]          = momentum.z();
        mCellData.eta[ntofhits]         = momentum.pseudoRapidity();
        mCellData.dedx[ntofhits]        = dedx;
        mCellData.dedxError[ntofhits]   = dedxerror;
        mCellData.nHitsDedx[ntofhits]   = dedx_np;
        mCellData.cherenkovAngle[ntofhits]    = cherang;
        mCellData.cherenkovPhotons[ntofhits]  = cherang_nph;
        mCellData.nSigE[ntofhits]       = mNSigmaElectron;
        mCellData.nSigPi[ntofhits]      = mNSigmaPion;
        mCellData.nSigK[ntofhits]       = mNSigmaKaon;
        mCellData.nSigP[ntofhits]       = mNSigmaProton;


        mCellData.tofcorr[ntofhits]     = -999.;
        mCellData.beta[ntofhits]     = -999.;
        StSPtrVecTofHit& hitTofVec = theTof->tofHits();
        for(size_t ih=0;ih<hitTofVec.size();ih++) {
           StTofHit *aHit = (StTofHit *)hitTofVec[ih];
           if(!aHit) continue;
           if(aHit->trayIndex() == thisCell->trayIndex() &&
              aHit->moduleIndex() == thisCell->moduleIndex() &&
              aHit->cellIndex() == thisCell->cellIndex()) {
              mCellData.tofcorr[ntofhits] = aHit->timeOfFlight();
              mCellData.beta[ntofhits] = aHit->beta();
           }
            
        }
        
        ntofhits++;
      }//end of tofr loop
    }
    mCellData.vpdEast = vpdEast;
    mCellData.vpdWest = vpdWest;
    mCellData.numberOfVpdEast = nVpdEast;
    mCellData.numberOfVpdWest = nVpdWest;
    mCellData.tDiff = theTof->tdiff();
    mCellData.nTofHits = ntofhits;
    mPvpdTuple->Fill();
    mCellTuple->Fill();
    mTofrEntries = ntofhits;
    entriesThisEvent = ntofhits;

    if(Debug()){
      LOG_INFO << " #vpd East:  "<<mCellData.numberOfVpdEast
               << "\n \t #vpd West:  "<<mCellData.numberOfVpdWest<<endm;
    }
    LOG_INFO << " Tofr update: " << entriesThisEvent << " entries" <<endm;
  }


  //- debug info
  if (doPrintMemoryInfo) {
        StMemoryInfo::instance()->snapshot();
        StMemoryInfo::instance()->print();
  }
  if (doPrintCpuInfo) {
    timer.stop();
    LOG_INFO << "CPU time for StEventMaker::Make(): "
	 << timer.elapsedTime() << " sec\n" << endm;
  }

  LOG_INFO << "StTofrNtupleMaker -- bye-bye" << endm;
  return kStOK;
}


//---------------------------------------------------------------------------
/// create and initialize ntuple and TTrees
void StTofrNtupleMaker::bookNtuples(){
  mTupleFile = new TFile(mTupleFileName.c_str(), "RECREATE");
  LOG_INFO << "StTofrNtupleMaker::bookNtuples()  file "
       << mTupleFileName << " opened" << endm;

  // pVPD timing
  mPvpdTuple = new TTree("pvpd","tofr timing");
  mPvpdTuple->SetAutoSave(1000);
  mPvpdTuple->Branch("run",&mCellData.run,"run/I");
  mPvpdTuple->Branch("evt",&mCellData.evt,"evt/I");
  mPvpdTuple->Branch("trgword",&mCellData.trgword,"trgword/I");
  mPvpdTuple->Branch("vertexX",&mCellData.vertexX,"vertexX/F");
  mPvpdTuple->Branch("vertexY",&mCellData.vertexY,"vertexY/F");
  mPvpdTuple->Branch("vertexZ",&mCellData.vertexZ,"vertexZ/F");
  mPvpdTuple->Branch("vpdEast",&mCellData.vpdEast,"vpdEast/I");
  mPvpdTuple->Branch("vpdWest",&mCellData.vpdWest,"vpdWest/I");
  mPvpdTuple->Branch("numberOfVpdEast",&mCellData.numberOfVpdEast,"numberOfVpdEast/I");
  mPvpdTuple->Branch("numberOfVpdWest",&mCellData.numberOfVpdWest,"numberOfVpdWest/I");
  mPvpdTuple->Branch("tDiff",&mCellData.tDiff,"tDiff/F");
  mPvpdTuple->Branch("pvpdLeadingEdgeTimeEast",&mCellData.pvpdLeadingEdgeTimeEast,"pvpdLeadingEdgeTimeEast[19]/D");
  mPvpdTuple->Branch("pvpdLeadingEdgeTimeWest",&mCellData.pvpdLeadingEdgeTimeWest,"pvpdLeadingEdgeTimeWest[19]/D");
  mPvpdTuple->Branch("pvpdTotEast",&mCellData.pvpdTotEast,"pvpdTotEast[19]/D");
  mPvpdTuple->Branch("pvpdTotWest",&mCellData.pvpdTotWest,"pvpdTotWest[19]/D");

  // Tofr calibration ntuple
  mCellTuple = new TTree("tofr","Tofr cell data");
  mCellTuple->SetAutoSave(1000);
  mCellTuple->Branch("run",&mCellData.run,"run/I");
  mCellTuple->Branch("evt",&mCellData.evt,"evt/I");
  mCellTuple->Branch("trgword",&mCellData.trgword,"trgword/I");
  mCellTuple->Branch("vertexX",&mCellData.vertexX,"vertexX/F");
  mCellTuple->Branch("vertexY",&mCellData.vertexY,"vertexY/F");
  mCellTuple->Branch("vertexZ",&mCellData.vertexZ,"vertexZ/F");
  mCellTuple->Branch("vpdEast",&mCellData.vpdEast,"vpdEast/I");
  mCellTuple->Branch("vpdWest",&mCellData.vpdWest,"vpdWest/I");
  mCellTuple->Branch("numberOfVpdEast",&mCellData.numberOfVpdEast,"numberOfVpdEast/I");
  mCellTuple->Branch("numberOfVpdWest",&mCellData.numberOfVpdWest,"numberOfVpdWest/I");
  mCellTuple->Branch("tDiff",&mCellData.tDiff,"tDiff/F");
  mCellTuple->Branch("pvpdLeadingEdgeTimeEast",&mCellData.pvpdLeadingEdgeTimeEast,"pvpdLeadingEdgeTimeEast[19]/D");
  mCellTuple->Branch("pvpdLeadingEdgeTimeWest",&mCellData.pvpdLeadingEdgeTimeWest,"pvpdLeadingEdgeTimeWest[19]/D");
  mCellTuple->Branch("pvpdTotEast",&mCellData.pvpdTotEast,"pvpdTotEast[19]/D");
  mCellTuple->Branch("pvpdTotWest",&mCellData.pvpdTotWest,"pvpdTotWest[19]/D");
  mCellTuple->Branch("nTofHits",&mCellData.nTofHits,"nTofHits/I");
  mCellTuple->Branch("tray",&mCellData.tray,"tray[nTofHits]/I");
  mCellTuple->Branch("module",&mCellData.module,"module[nTofHits]/I");
  mCellTuple->Branch("cell",&mCellData.cell,"cell[nTofHits]/I");
  mCellTuple->Branch("daq",&mCellData.daq,"daq[nTofHits]/I");
  mCellTuple->Branch("leadingEdgeTime",&mCellData.leadingEdgeTime,"leadingEdgeTime[nTofHits]/D");
  mCellTuple->Branch("tot",&mCellData.tot,"tot[nTofHits]/D");
//  mCellTuple->Branch("matchFlag",&mCellData.matchFlag,"matchFlag/I");
  mCellTuple->Branch("xlocal",&mCellData.xlocal,"xlocal[nTofHits]/F");
  mCellTuple->Branch("ylocal",&mCellData.ylocal,"ylocal[nTofHits]/F");
  mCellTuple->Branch("zlocal",&mCellData.zlocal,"zlocal[nTofHits]/F");
  mCellTuple->Branch("deltay",&mCellData.deltay,"deltay[nTofHits]/F");
  mCellTuple->Branch("trackId",&mCellData.trackId,"trackId[nTofHits]/I");
  mCellTuple->Branch("charge",&mCellData.charge,"charge[nTofHits]/I");
  mCellTuple->Branch("p",&mCellData.p,"p[nTofHits]/F");
  mCellTuple->Branch("pt",&mCellData.pt,"pt[nTofHits]/F");
  mCellTuple->Branch("px",&mCellData.px,"px[nTofHits]/F");
  mCellTuple->Branch("py",&mCellData.py,"py[nTofHits]/F");
  mCellTuple->Branch("pz",&mCellData.pz,"pz[nTofHits]/F");
  mCellTuple->Branch("eta",&mCellData.eta,"eta[nTofHits]/F");
  mCellTuple->Branch("dcaX",&mCellData.dcaX,"dcaX[nTofHits]/F");
  mCellTuple->Branch("dcaY",&mCellData.dcaY,"dcaY[nTofHits]/F");
  mCellTuple->Branch("dcaZ",&mCellData.dcaZ,"dcaZ[nTofHits]/F");
  mCellTuple->Branch("length",&mCellData.length,"length[nTofHits]/F");
  mCellTuple->Branch("nHits",&mCellData.nHits,"nHits[nTofHits]/I");
  mCellTuple->Branch("nHitsFit",&mCellData.nHitsFit,"nHitsFit[nTofHits]/I");
  mCellTuple->Branch("nHitsDedx",&mCellData.nHitsDedx,"nHitsDedx[nTofHits]/I"); 
  mCellTuple->Branch("dedx",&mCellData.dedx,"dedx[nTofHits]/F"); 
  mCellTuple->Branch("dedxError",&mCellData.dedxError,"dedxError[nTofHits]/F"); 
  mCellTuple->Branch("cherenkovAngle",&mCellData.cherenkovAngle,"cherenkovAngle[nTofHits]/F");
  mCellTuple->Branch("cherenkovPhotons",&mCellData.cherenkovPhotons,"cherenkovPhotons[nTofHits]/I");
  mCellTuple->Branch("nSigE",&mCellData.nSigE,"nSigE[nTofHits]/F");
  mCellTuple->Branch("nSigPi",&mCellData.nSigPi,"nSigPi[nTofHits]/F");
  mCellTuple->Branch("nSigK",&mCellData.nSigK,"nSigK[nTofHits]/F");
  mCellTuple->Branch("nSigP",&mCellData.nSigP,"nSigP[nTofHits]/F");
  mCellTuple->Branch("tofcorr",&mCellData.tofcorr,"tofcorr[nTofHits]/F");
  mCellTuple->Branch("beta",&mCellData.beta,"beta[nTofHits]/F");
  
  return;
}

/*****************************************************************
 *
 * $Log: StTofrNtupleMaker.cxx,v $
 * Revision 1.13  2018/02/26 23:26:51  smirnovd
 * StTof: Remove outdated ClassImp macro
 *
 * Revision 1.12  2018/02/26 23:13:20  smirnovd
 * Move embedded CVS log messages to the end of file
 *
 * Revision 1.11  2016/05/05 16:18:07  geurts
 * addressed Cppcheck report: fixed uninitialized variables and remove one unused variable.
 *
 * Revision 1.10  2012/12/14 06:35:52  geurts
 * Changed global database calls to direct table access and/or removed deprecated database access code.
 *
 * Revision 1.9  2008/06/05 18:33:45  dongx
 * -added members in tree: tDiff, tofcorr and beta for check
 * -beamLine read from database
 *
 * Revision 1.8  2008/05/08 21:09:37  dongx
 * Changed precision of time info to double type
 *
 * Revision 1.7  2008/05/06 18:42:09  dongx
 * Updated for Run8 analysis
 *
 * Revision 1.5  2007/04/17 23:11:12  dongx
 * replaced with standard STAR Loggers
 *
 * Revision 1.4  2004/04/12 16:17:03  dongx
 * add AdcLoRes in the ntuple
 *
 * Revision 1.3  2004/04/09 16:13:23  dongx
 * fix a potential bug causing crash
 *
 * Revision 1.2  2004/03/29 19:10:56  dongx
 * correct the pVPD ADC read-out for year2 and year3
 *
 * Revision 1.1  2004/03/11 22:39:54  dongx
 * first release
 *
 */
