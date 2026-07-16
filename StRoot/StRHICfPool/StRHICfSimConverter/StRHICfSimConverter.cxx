
#include "StRHICfSimConverter.h"

// STAR headers
#include "StChain/StChain.h"
#include "StChain/StChainOpt.h"
#include "StEvent/StBbcTriggerDetector.h"

// StMuEvents headers
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "StMuDSTMaker/COMMON/StMuBTofHit.h"

// StarGenEvent
#include "StarGenerator/BASE/StarGenerator.h"
#include "StarGenerator/BASE/StarPrimaryMaker.h"
#include "StarGenerator/EVENT/StarGenPPEvent.h"
#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"

// StRHICfSimDst 
#include "StRHICfPool/StRHICfSimDst/StRHICfSimPar.h"
#include "StRHICfPool/StRHICfSimDst/StRHICfSimDst.h"
#include "StRHICfPool/StRHICfSimDst/StRHICfSimEvent.h"
#include "StRHICfPool/StRHICfSimDst/StRHICfSimTrack.h"
#include "StRHICfPool/StRHICfSimDst/StRHICfSimBBC.h"
#include "StRHICfPool/StRHICfSimDst/StRHICfSimBTof.h"
#include "StRHICfPool/StRHICfSimDst/StRHICfSimZDC.h"

#include "StEvent/StEnumerations.h"
#include "StEvent/StRHICfCollection.h"
#include "StEvent/StRHICfHit.h"
#include "StEvent/StRHICfPoint.h"

StRHICfSimConverter::StRHICfSimConverter(int convertFlag, const char* fileName, const Char_t* name) 
: StMaker(name), mConvertFlag(convertFlag), mInputFile(fileName), mRHICfRunType(-1)
{
}

StRHICfSimConverter::~StRHICfSimConverter()
{
}

Int_t StRHICfSimConverter::Init()
{
    if(mConvertFlag == kMuDst2SimDst){InitMuDst2SimDst();}
    if(mConvertFlag == kSimRecoMode){InitSimRecoMode();}

    return kStOk;
}

Int_t StRHICfSimConverter::Make()
{
    LOG_INFO << "StRHICfSimConverter::Make()" << endm;
    if(mConvertFlag == kMuDst2SimDst){ConvertMuDst2SimDst();}
    if(mConvertFlag == kSimRecoMode){
        if(mSimDstTree->GetEntries() <= mEvent){return kStEOF;}
        RecoSimulation();
    }

    return kStOk;
}

Int_t StRHICfSimConverter::Finish()
{
    if(mConvertFlag == kMuDst2SimDst){
        mSimDstFile -> cd();
        mSimDstTree -> Write();
        mSimDstFile -> Close();
    }

    if(mConvertFlag == kSimRecoMode){
        mOutSimDstFile -> cd();
        mOutSimDstTree -> Write();
        mOutSimDstFile -> Close();
    }

    return kStOk;
}

Int_t StRHICfSimConverter::clear()
{
    return kStOk;
}

Int_t StRHICfSimConverter::InitMuDst2SimDst()
{
    mChain = new TChain("genevents");

    TObjArray *tokens;

    TString generatorFile = "";
    if(mInputFile.Length() == 0){
        LOG_ERROR << "Input file is not a existing ... " << endm;
        return kStFatal;
    }
    else{
        int fileNum = 0;
        TString muDstFile = "";
        if( mInputFile.Index(".list") != -1 || mInputFile.Index(".lis") != -1 ){
            std::ifstream inputStream( mInputFile.Data() );
            if(!inputStream) {LOG_ERROR << "ERROR: Cannot open list file " << mInputFile << endm;}

            std::string file;
            size_t pos;
            while(getline(inputStream, file)){
                pos = file.find_first_of(" ");
                if (pos != std::string::npos ) file.erase(pos,file.length()-pos);
                if(file.find("MuDst") != std::string::npos){
                    muDstFile = file;
                    fileNum++;
                }
            }
            if(fileNum != 1){
                LOG_ERROR << "StRHICfSimConverter::InitMuDst2SimDst() -- Input file must be one MuDst file" << endm;
                return kStFatal;
            }
        }
        if(muDstFile == ""){muDstFile = mInputFile;}
        if(muDstFile.Index("MuDst") != -1) {
            mOutputFile = muDstFile;
            tokens = mOutputFile.Tokenize("/");
            mOutputFile = ((TObjString *) tokens -> At(tokens->GetEntries()-1)) -> GetString();
            mOutputFile.ReplaceAll(".MuDst.root",".RHICfSimDst.root");

            generatorFile = muDstFile;
            generatorFile.ReplaceAll(".MuDst.root", ".genevents.root");
            int valid = mChain -> Add(generatorFile);
            if(valid == 0){
                LOG_ERROR << "StRHICfSimConverter::InitMuDst2SimDst() -- There is no Generator ROOT file !!! " << endm;
                return kStFatal;
            }
            LOG_INFO << Form("StRHICfSimConverter::InitMuDst2SimDst() -- Find a Event generator file: %s",generatorFile.Data()) << endm;

            for(int i=0; i<=rGeneratorNum; i++){
                TString generatorName = StRHICfSimPar::GetGeneratorName(i);
                if(mOutputFile.Index(generatorName) != -1){
                    LOG_INFO << Form("StRHICfSimConverter::InitMuDst2SimDst() -- Find a Event generator name: %s",generatorName.Data()) << endm;
                    mGeneratorIdx = i;
                    break;
                }
            }
            if(mGeneratorIdx >= rGeneratorNum){
                LOG_ERROR << "StRHICfSimConverter::InitMuDst2SimDst() -- Can not find a event generator type !!!" << endm;
                return kStFatal;
            }

            if(muDstFile.Index("TL") != -1){mRHICfRunType = rTLtype;}
            else if(muDstFile.Index("TS") != -1){mRHICfRunType = rTStype;}
            else if(muDstFile.Index("TOP") != -1){mRHICfRunType = rTOPtype;}
            else{
                LOG_ERROR << "StRHICfSimConverter::InitMuDst2SimDst() -- Can not find a RHICf run type !!!" << endm;
                return kStFatal;
            }
        }
    }

    LOG_INFO << Form("StRHICfSimConverter::InitMuDst2SimDst() -- Output file name: %s",mOutputFile.Data()) << endm;
    
    mSimDstFile = new TFile(mOutputFile.Data(), "RECREATE");
    mSimDstTree = new TTree("StRHICfSimDst","StRHICfSimDst");

    mSimDst = new StRHICfSimDst();
    mSimDst -> CreateDstArray(mSimDstTree);

    TString generatorType = "StRHICfSimGenerator";
    if(mOutputFile.Index("Pythia8") != -1){generatorType = "Pythia8";}

    mGenPPEvent = new StarGenPPEvent(generatorType);
    mGenEvent = new StarGenEvent("primaryEvent");

    mChain -> SetBranchAddress(generatorType, &mGenPPEvent);
    mChain -> SetBranchAddress("primaryEvent", &mGenEvent);
    mTmpChainEvent = 0;

    InitRHICfGeometry();

    mDatabasePDG = new TDatabasePDG();

    return kStOk;
}

Int_t StRHICfSimConverter::InitSimRecoMode()
{
    if(mInputFile.Length() == 0){
        LOG_ERROR << "Input file is not a existing ... " << endm;
        return kStFatal;
    }
    else{
        if(mInputFile.Index("rhicfsim.RHICfSimDst.root") != -1 && mInputFile.Index("reco") == -1) {
            LOG_INFO << Form("StRHICfSimConverter::InitSimRecoMode() -- Find a SimDst file: %s",mInputFile.Data()) << endm;

            mSimDstFile = new TFile(mInputFile.Data(), "READ");
            mSimDstTree = (TTree*)mSimDstFile -> Get("StRHICfSimDst");

            mSimDst = new StRHICfSimDst();
            mSimDst -> ReadDstArray(mSimDstTree);

            mInputFile.ReplaceAll("rhicfsim.RHICfSimDst.root", "reco.rhicfsim.RHICfSimDst.root");
            TString outputFileName = mInputFile;
            mOutSimDstFile = new TFile(outputFileName.Data(), "RECREATE");
            mOutSimDstTree = mSimDstTree -> CloneTree(0);

            LOG_INFO << Form("StRHICfSimConverter::InitSimRecoMode() -- Output File: %s",outputFileName.Data()) << endm;
        }
        else{
            LOG_ERROR << Form("StRHICfSimConverter::InitSimRecoMode() -- Input file has a different format !!! %s",mInputFile.Data()) << endm;
            return kStFatal;
        }
    }
    if(!mSimDst){
        LOG_ERROR << Form("StRHICfSimConverter::InitSimRecoMode() -- Could not read file: %s",mInputFile.Data()) << endm;
        return kStFatal;
    }

    mRHICfPointMaker = new StRHICfPointMaker();
    mRHICfPointMaker -> setMCReco();

    mEvent = 0;

    return kStOk;
}

Int_t StRHICfSimConverter::ConvertMuDst2SimDst()
{
    mSimDst -> Clear();
    mParticleIdx.clear();

    double trkMom[3];
    double trkVtxStart[3];
    double trkVtxEnd[3];

    // =================== DST Set up ======================
    mMuDst = (StMuDst*) GetInputDS("MuDst"); // from DST
    if(!mMuDst) {LOG_ERROR << "no StMuDst" << endm;  return kStFatal;};

    mMuEvent = mMuDst->event(); // from muDST
    if(!mMuEvent) {LOG_ERROR << "no StMuEvent" << endm;  return kStFatal;};

    // Get the event info
    mSimEvent = mSimDst -> GetSimEvent();
    int eventNumber = mMuEvent -> eventNumber();

    mSimEvent -> SetEventNumber(eventNumber);
    mSimEvent -> SetRHICfRunType(mRHICfRunType);
    if(mGeneratorIdx < rGeneratorNum){mSimEvent -> SetGeneratorIdx(mGeneratorIdx);}

    // ======================= Get MuMcTrack and Save RHICfSimTrack ============================
    mMcVtxArray = mMuDst -> mcArray(0);
    mMcTrkArray = mMuDst -> mcArray(1); 

    int RHICfParticleNum = 0;
    int primaryTrkNum = 0;
    int propagatedTrkNum = 0;
    int mcTrkNum = mMcTrkArray -> GetEntriesFast();
    for(int i=0; i<mcTrkNum; i++){
        memset(trkMom, 0., sizeof(trkMom));
        memset(trkVtxStart, 0., sizeof(trkVtxStart));
        fill_n(&trkVtxEnd[0], 3, -9999.);

        // Set the first mcTrack loop
        mMcTrk = (StMuMcTrack*)mMcTrkArray -> UncheckedAt(i);

        int gePid = mMcTrk -> GePid();
        int pid = GetGePid2PDG(gePid);
        if(pid == 0){continue;} // for invaild PDG Encoding
        
        int trkId = mMcTrk -> Id() -1; // starting of track Id from 0 index
        int VtxIdStart = mMcTrk -> IdVx();
        int VtxIdEnd = mMcTrk -> IdVxEnd();

        double energy = mMcTrk -> E();
        trkMom[0] = mMcTrk -> Pxyz().x();
        trkMom[1] = mMcTrk -> Pxyz().y();
        trkMom[2] = mMcTrk -> Pxyz().z();

        mMcVtx = (StMuMcVertex*)mMcVtxArray -> UncheckedAt(VtxIdStart-1);
        trkVtxStart[0] = mMcVtx->XyzV().x();
        trkVtxStart[1] = mMcVtx->XyzV().y();
        trkVtxStart[2] = mMcVtx->XyzV().z();

        int parentTrkId = mMcVtx -> IdParTrk() - 1; // starting of parentTrk Id from 0 index
        int daughterNum = mMcVtx -> NoDaughters();

        if(VtxIdEnd != 0){
            mMcVtx = (StMuMcVertex*)mMcVtxArray -> UncheckedAt(VtxIdEnd-1);
            trkVtxEnd[0] = mMcVtx->XyzV().x();
            trkVtxEnd[1] = mMcVtx->XyzV().y();
            trkVtxEnd[2] = mMcVtx->XyzV().z();
        }

        // Check the saving condition
        bool isPrimary = (VtxIdStart == 1)? true : false;
        bool isPropagate = IsSimPropagate(trkVtxStart, trkVtxEnd, trkMom);
        int rhicfHit = GetRHICfGeoHit(trkVtxStart, trkVtxEnd, trkMom, energy, gePid);

        // Save track data
        if(isPrimary || isPropagate || rhicfHit != -1){
            int simTrkId = mSimDst->GetSimTrackNum();
            mSimTrk = mSimDst -> GetSimTrack(simTrkId);
            mSimTrk -> SetId(simTrkId);
            mSimTrk -> SetPid(pid);
            mSimTrk -> SetParentId(-1);
            mSimTrk -> SetDaughterNum(daughterNum);
            mSimTrk -> SetEnergy(energy);
            mSimTrk -> SetMomentum(trkMom[0], trkMom[1], trkMom[2]);   
            mSimTrk -> SetVertexStart(trkVtxStart[0], trkVtxStart[1], trkVtxStart[2]);
            mSimTrk -> SetVertexEnd(trkVtxEnd[0], trkVtxEnd[1], trkVtxEnd[2]);
            mSimTrk -> SetIsFinal();
            if(isPrimary){
                mSimTrk -> SetIsPrimary();
                primaryTrkNum++;
            }
            if(isPropagate){
                mSimTrk->SetIsSimPropagate();
                propagatedTrkNum++;
            }
            if(rhicfHit != -1 || isPrimary){
                mParticleIdx.push_back(simTrkId);
            }
            if(rhicfHit != -1){
                mSimTrk->SetIsRHICfHit();
                RHICfParticleNum++;
            }
        }
    }

    // ======================= Make Generate event data =======================
    GetGenEventData();

    for(int i=0; i<mSimDst->GetSimTrackNum(); i++){
        mSimTrk = mSimDst -> GetSimTrack(i);
    }

    // ======================= Get BBC ============================
    mSimBBC = mSimDst -> GetSimBBC();
    StBbcTriggerDetector bbc = mMuEvent -> bbcTriggerDetector();
    bbc.setYear(2017);
    int bbcPMTNum = bbc.numberOfPMTs();
    for(int i=0; i<bbcPMTNum; i++){
        int adc = bbc.adc(i);
        int dir = (i < bbcPMTNum/2)? rEast : rWest;

        // small tile
        if((i < rBBCSmallPmtNum) || (bbcPMTNum/2 <= i && i < bbcPMTNum/2 + rBBCSmallPmtNum)){
            int pmtIdx = (dir == rEast)? i : i - bbcPMTNum/2;
            mSimBBC -> SetSmallADC(dir, pmtIdx, adc);
        }
        else{ // large tile
            int pmtIdx = (dir == rEast)? i - rBBCSmallPmtNum : i - (bbcPMTNum/2 + rBBCSmallPmtNum);
            mSimBBC -> SetLargeADC(dir, pmtIdx, adc);
        }
    }

    // ======================= Get B-TOF ============================
    Int_t bTofTrayMult = mMuEvent -> btofTrayMultiplicity();
    Int_t bTofMult = mMuDst -> GetNBTofHit();
    for (int i=0; i<bTofMult; ++i) {
        mMuBTofHit = (StMuBTofHit*)mMuDst -> btofHit(i);
        if (!mMuBTofHit) continue;
        if (mMuBTofHit->tray() > 120) continue; // BTof has only 120 trays, over the 120 index will be VPD hit info
        int trayId = mMuBTofHit -> tray();
        int moduleId = mMuBTofHit -> module();
        int cellId = mMuBTofHit -> cell();
        int idTruth = mMuBTofHit -> idTruth();

        mSimBTof = mSimDst -> GetSimBTof(mSimDst -> GetSimBTofNum());
        mSimBTof -> SetTray(trayId);
        mSimBTof -> SetModule(moduleId);
        mSimBTof -> SetCell(cellId);
        mSimBTof -> SetSimTrkId(idTruth);
    }

    mSimEvent -> SetPrimaryTrkNum(primaryTrkNum);

    // Saved data
    if(propagatedTrkNum > 1 || RHICfParticleNum != 0){
        mSimDstTree -> Fill();

        // Event Summary print
        LOG_INFO << "StRHICfSimConverter -- Event Summary --" << endm;
        LOG_INFO << " ----- Process Id: " << mSimEvent -> GetProcessId() << endm;
        LOG_INFO << " ----- Primary Trk Num: " << mSimEvent->GetPrimaryTrkNum() << ", Generate Trk Num: " << mSimEvent->GetGenFinalParNum() << ", Propagated Trk Num: " << propagatedTrkNum << endm;
        LOG_INFO << " ----- RHICf Truth Trk Num: " << RHICfParticleNum << endm;
    }


    return kStOk;
}

Int_t StRHICfSimConverter::GetGenEventData()
{
    int simDstEvent = mSimEvent -> GetEventNumber();
    for(int event=mTmpChainEvent; event<mChain->GetEntriesFast(); event++){
        mChain -> GetEntry(event);

        if(mGenEvent -> GetFilterResult() != 1){continue;}
        if(simDstEvent == mGenEvent->GetEventNumber()){
            mTmpChainEvent = event;

            // Set the Event gernerator information
            int processId = mGenPPEvent -> process;
            double xParton1 = mGenPPEvent -> xParton1;
            double xParton2 = mGenPPEvent -> xParton2;
            double xPdf1 =  mGenPPEvent -> xPdf1;
            double xPdf2 =  mGenPPEvent -> xPdf2;
            double Q2Fac =  mGenPPEvent -> Q2fac;
            double Q2Ren =  mGenPPEvent -> Q2ren;
            double sHat =  mGenPPEvent -> sHat;
            double tHat =  mGenPPEvent -> tHat;
            double uHat =  mGenPPEvent -> uHat;
            double ptHat =  mGenPPEvent -> ptHat;

            int genEventNum = -999;
            if(mGeneratorIdx == rPythia8){genEventNum = event+1;}
            else{genEventNum = mGenPPEvent -> weight;}

            int finalParNum = 0;
            int finalChargedParNum = 0;

            mSimEvent -> SetGenEventNumber(genEventNum);
            mSimEvent -> SetProcessId(processId);
            mSimEvent -> SetXParton1(xParton1);
            mSimEvent -> SetXParton2(xParton2);
            mSimEvent -> SetXPdf1(xPdf1);
            mSimEvent -> SetXPdf2(xPdf2);
            mSimEvent -> SetQ2Fac(Q2Fac);
            mSimEvent -> SetQ2Renorm(Q2Ren);
            mSimEvent -> SetsHat(sHat);
            mSimEvent -> SettHat(tHat);
            mSimEvent -> SetuHat(uHat);
            mSimEvent -> SetPtHat(ptHat);

            // Find a particle mothers
            vector<pair<int, int>> pairParticles;

            int parNum = mGenEvent -> GetNumberOfParticles();
            for(int par=0; par<parNum; par++){
                mGenParticle = (*mGenEvent)[par];
                int status = mGenParticle -> GetStatus();
                int pid = mGenParticle -> GetId();

                // Diffraction informations
                if(status == 15){
                    double diffMass = mGenParticle -> GetMass();
                    double px = mGenParticle -> GetPx();
                    double py = mGenParticle -> GetPy();
                    double pz = mGenParticle -> GetPz();
                    double p = sqrt(px*px + py*py + pz*pz);
                    double cosTheta = (p == 0.)? 1. : pz/p;
                    double diffEta = -0.5*TMath::Log((1. - cosTheta)/(1. + cosTheta));
                    if(par == 3){
                        mSimEvent -> SetDiffractionAMass(diffMass);
                        mSimEvent -> SetDiffractionAEta(diffEta);
                    }
                    if(par == 4){
                        mSimEvent -> SetDiffractionBMass(diffMass);
                        mSimEvent -> SetDiffractionBEta(diffEta);
                    }
                    if(par == 5){
                        mSimEvent -> SetDiffractionCMass(diffMass);
                        mSimEvent -> SetDiffractionCEta(diffEta);
                    }
                }
                // Get the number of final state particle 
                if(status == 1){
                    finalParNum++;
                    if(abs(pid) == 11 || abs(pid) == 13 || pid == 22 || abs(pid) == 211 || abs(pid) == 321 || abs(pid) == 2212){
                        finalChargedParNum++;
                    }
                }

                // GenTrack match to SimTrack 
                int genEnergyInt = int(mGenParticle -> GetEnergy()*100);
                int genPtInt = int(mGenParticle -> pt()*10000);

                int matchingParNum = mParticleIdx.size();
                if(int(pairParticles.size()) == matchingParNum){continue;}

                for(int r=0; r<matchingParNum; r++){
                    mSimTrk = mSimDst -> GetSimTrack(mParticleIdx[r]);
                    int simTrkPid = mSimTrk->GetPid();
                    double px = mSimTrk -> GetPx();
                    double py = mSimTrk -> GetPy();
                    double e = mSimTrk -> GetE();
                    
                    int simTrkEnergyInt = int(e*100);
                    int simTrkPtInt = int(sqrt(px*px + py*py)*10000);

                    // Getting the same RHICf particle 
                    if(pid == simTrkPid && genEnergyInt == simTrkEnergyInt && genPtInt == simTrkPtInt ){
                        pairParticles.push_back(make_pair(mParticleIdx[r], par));
                    }
                }
            }

            // Set the generator level number of particles
            mSimEvent -> SetGenFinalParNum(finalParNum);
            mSimEvent -> SetGenFinalChargedParNum(finalChargedParNum);

            // Find a parent particles for RHICf particles
            vector<pair<int, int>> tmpSavedTrackId; // [gen track id, sim track id]
            int pairParticleNum = pairParticles.size();

            for(int i=0; i<pairParticleNum; i++){
                int simTrkIdx = pairParticles[i].first;
                int genParIdx = pairParticles[i].second;

                while(true){
                    mSimTrk = mSimDst -> GetSimTrack(simTrkIdx);
                    int simTrkParentId = mSimTrk -> GetParentId();

                    mGenParticle = (*mGenEvent)[genParIdx];
                    int status = mGenParticle -> GetStatus();
                    int genParentId1 = mGenParticle -> GetFirstMother();
                    int genParentId2 = mGenParticle -> GetLastMother();

                    if(genParentId2 != 0){ // primary particle case
                        if(!mSimTrk->IsPrimary()){
                            mSimTrk -> SetIsPrimary();
                            mSimEvent -> SetPrimaryTrkNum(mSimEvent->GetPrimaryTrkNum()+1);
                        }
                        break; 
                    }
                    else{ // decayed particle 
                        if(simTrkParentId < 0){ // case: but MuDst not include mother particle, save the parent tracks
                            // Find a duplicated saving tracks
                            int existSavedTrkId = -1;
                            for(int i=0; i<tmpSavedTrackId.size(); i++){
                                if(genParentId1 == tmpSavedTrackId[i].first){
                                    existSavedTrkId = tmpSavedTrackId[i].second;
                                    break;
                                }
                            }
                            // Set the this track's parent id and exit if track id already saved
                            if(existSavedTrkId != -1){
                                mSimTrk -> SetParentId(existSavedTrkId); // for duaghter particle 
                                break;
                            }

                            // Set the final state flag
                            if(status == 1){mSimTrk -> SetIsFinal();}

                            // Assigned event generator parent track
                            mGenParticle = (*mGenEvent)[genParentId1];

                            // cut the intemediate tracks
                            int ParentStatus = mGenParticle -> GetStatus();
                            if(ParentStatus > 2){break;} 

                            // Set the track's parent
                            int simTrkNum = mSimDst->GetSimTrackNum();
                            mSimTrk -> SetParentId(simTrkNum);

                            double StartVx = mSimTrk -> GetVxStart();
                            double StartVy = mSimTrk -> GetVyStart();
                            double StartVz = mSimTrk -> GetVzStart();

                            // Make new parent SimTrack
                            mSimTrk = mSimDst -> GetSimTrack(simTrkNum); // set mother particle
                            mSimTrk -> SetId(simTrkNum);
                            mSimTrk -> SetParentId(-1);
                            if(ParentStatus == 1){mSimTrk -> SetIsFinal();}
                            mSimTrk -> SetPid(mGenParticle->GetId()); 
                            mSimTrk -> SetDaughterNum(2);
                            mSimTrk -> SetEnergy(mGenParticle->GetEnergy());
                            mSimTrk -> SetMomentum(mGenParticle->GetPx(), mGenParticle->GetPy(), mGenParticle->GetPz());

                            double motherStartVx = mGenParticle->GetVx();
                            double motherStartVy = mGenParticle->GetVy();
                            double motherStartVz = mGenParticle->GetVz();
                            mSimTrk -> SetVertexStart(motherStartVx, motherStartVy, motherStartVz);
                            mSimTrk -> SetVertexEnd(StartVx, StartVy, StartVz);
                            mSimTrk -> SetIsPrimary();
                            mSimEvent -> SetPrimaryTrkNum(mSimEvent->GetPrimaryTrkNum()+1);

                            // Next to the grandmother particle
                            simTrkIdx = simTrkNum;
                            genParIdx = genParentId1;

                            // Push the saved track id 
                            tmpSavedTrackId.push_back(make_pair(genParentId1, simTrkNum));
                        }
                        else{ // case: MuDst have a mother particle
                            // Next to the grandmother particle
                            simTrkIdx = simTrkParentId;
                            genParIdx = genParentId1;
                        }
                    }
                }
            }
            break;
        }
    }
    return kStOk;
}

bool StRHICfSimConverter::IsSimPropagate(double vtxStart[], double vtxEnd[], double mom[])
{
    double p = sqrt(mom[0]*mom[0] + mom[1]*mom[1] + mom[2]*mom[2]);
    double unitX = mom[0]/p;
    double unitY = mom[1]/p;
    double unitZ = mom[2]/p;

    double cosTheta = (p == 0.)? 1. : unitZ;
    double eta = -0.5*TMath::Log((1. - cosTheta)/(1. + cosTheta));

    // 1st cut: if track is not alive 
    if(vtxEnd[2] > -9990.){return false;} 

    // 2nd cut: x-y starting point larger then world size in RHICf geant4 simulation (tightly 0.5 m)
    if(fabs(vtxStart[0]) > 50. || fabs(vtxStart[1]) > 50.){return false;}

    // 3rd cut: if track eta < 5
    if(eta < 5){return false;}

    // 4th cut: startVz larger than 10m (DX magnet)
    if(fabs(vtxStart[2]) > 1000.){return false;}

    // 5th cut: if Z diraction is negative
    if(unitZ < 0.){return false;}

    double startR = sqrt(vtxStart[0]*vtxStart[0] + vtxStart[1]*vtxStart[1]);
    
    // 6th cut: if startR > DX magnet diameter and partice going to outer direction
    if(startR > 30.){
        if(vtxStart[0] > 0 && unitX > 0){return false;}
        if(vtxStart[0] < 0 && unitX < 0){return false;}
        if(vtxStart[1] > 0 && unitY > 0){return false;}
        if(vtxStart[1] < 0 && unitY < 0){return false;}
    }
    return true;
}

Int_t StRHICfSimConverter::GetGePid2PDG(int gepid)
{
    if(gepid == 4){return 0;} // neutrino case
    if(gepid == 48){return 0;} // geantino case 
    if(gepid == 45){return 1000010020;} // Deuteron case
    if(gepid == 46){return 1000010030;} // Triron case
    if(gepid == 47){return 1000020040;} // Alpha case

    return mDatabasePDG -> ConvertGeant3ToPdg(gepid);
}

Bool_t StRHICfSimConverter::IsInterestedParticle(int gepid)
{
    switch(gepid)
    {
        case 13 : return true; // n
        case 10 : return true; // K0_L
        case 1  : return true; // gamma

        default  : return false; // other charged and uninterested particles
    }
    return false;
}

Int_t StRHICfSimConverter::RecoSimulation()
{
    FillMCData();

    int testRunNumber = 0;
    if(mRHICfRunType == rTStype){testRunNumber = 18177043;}
    if(mRHICfRunType == rTLtype){testRunNumber = 18176012;}
    if(mRHICfRunType == rTOPtype){testRunNumber = 18177018;}

    mRHICfPointMaker -> InitRun(testRunNumber); // this run number is for initialization of RHICfDbMaker, it doesn't dependent to reco
    mRHICfPointMaker -> Make();
    SaveRecoData();

    return kStOk;
}

Int_t StRHICfSimConverter::FillMCData()
{
    LOG_INFO << "StRHICfSimConverter::FillMCData() -- Start the RHICf simulation reconstruction, event: " << mEvent << " / " << mSimDstTree->GetEntries()-1 << endl;
    mSimDstTree -> GetEntry(mEvent); 

    mRHICfColl = new StRHICfCollection();
    mRHICfColl -> isAllSave();

    mRHICfHit = mRHICfColl -> hitCollection();
    mRHICfHit -> initDataArray();
    mSimRHICfHit = mSimDst -> GetSimRHICfHit();

    mSimEvent = mSimDst -> GetSimEvent();
    mRHICfRunType = mSimEvent -> GetRHICfRunType();
    mRHICfColl -> setRunType(mSimEvent -> GetRHICfRunType());

    for(int it=0; it<kRHICfNtower; it++){
        for(int ip=0; ip<kRHICfNplate; ip++){
            Float_t plateEnergy = mSimRHICfHit -> GetPlatedE(it, ip);
            mRHICfHit -> setPlateEnergy(it, ip, plateEnergy);
        }
        for(int il=0; il<kRHICfNlayer; il++){
            for(int ixy=0; ixy<kRHICfNxy; ixy++){
                int chSize = (it == 0)? 20 : 40;
                for(int ich=0; ich<chSize; ich++){
                    Float_t gsobarEnergy = mSimRHICfHit -> GetGSOBardE(it, il, ixy, ich);
                    mRHICfHit -> setGSOBarEnergy(it, il, ixy, ich, gsobarEnergy);
                }
            }
        }
    }
    mRHICfPointMaker -> setMCCollection(mRHICfColl);

    mEvent++;
    return kStOk;
}

Int_t StRHICfSimConverter::SaveRecoData()
{
    mRHICfHit = mRHICfColl -> hitCollection();
    mSimRHICfHit = mSimDst -> GetSimRHICfHit();

    // RHICf Hit
    for(int it=0; it<kRHICfNtower; it++){
        for(int io=0; io<2; io++){
            Int_t gsoMaxLayer = mRHICfHit -> getGSOMaxLayer(it, io);
            mSimRHICfHit -> SetGSOMaxLayer(it, io, gsoMaxLayer);
        }

        Float_t l20 = mRHICfHit -> getL20(it);
        Float_t l90 = mRHICfHit -> getL90(it);
        mSimRHICfHit -> SetL20(it, l20);
        mSimRHICfHit -> SetL90(it, l90);
        
        Int_t multiHitNum = mRHICfHit -> getMultiHitNum(it);
        mSimRHICfHit -> SetMultiHitNum(it, multiHitNum);
        
        for(int il=0; il<kRHICfNlayer; il++){
            for(int ixy=0; ixy<kRHICfNxy; ixy++){

                Int_t singleHitNum = mRHICfHit -> getSingleHitNum(it, il, ixy);
                Int_t maxPeakBin = mRHICfHit -> getMaxPeakBin(it, il, ixy);
                Float_t singleHitPos = mRHICfHit -> getSingleHitPos(it, il, ixy);
                Float_t singlePeakHeight = mRHICfHit -> getSinglePeakHeight(it, il, ixy);
                Float_t singleChi2 = mRHICfHit -> getSingleFitChi2(it, il, ixy);
                Float_t multiChi2 = mRHICfHit -> getMultiFitChi2(it, il, ixy);

                mSimRHICfHit -> SetSingleHitNum(it, il, ixy, singleHitNum);
                mSimRHICfHit -> SetMaxPeakBin(it, il, ixy, maxPeakBin);
                mSimRHICfHit -> SetSingleHitPos(it, il, ixy, singleHitPos);
                mSimRHICfHit -> SetSinglePeakHeight(it, il, ixy, singlePeakHeight);
                mSimRHICfHit -> SetSingleFitChi2(it, il, ixy, singleChi2);
                mSimRHICfHit -> SetMultiFitChi2(it, il, ixy, multiChi2);

                for(int io=0; io<2; io++){
                Float_t multiHitPos = mRHICfHit -> getMultiHitPos(it, il, ixy, io);
                Float_t multiPeakRaw = mRHICfHit -> getMultiPeakRaw(it, il, ixy, io);
                Float_t multiPeakHeight = mRHICfHit -> getMultiPeakHeight(it, il, ixy, io);
                Float_t multiEnergySum = mRHICfHit -> getMultiEnergySum(it, il, ixy, io);
                    
                mSimRHICfHit -> SetMultiHitPos(it, il, ixy, io, multiHitPos);
                mSimRHICfHit -> SetMultiPeakRaw(it, il, ixy, io, multiPeakRaw);
                mSimRHICfHit -> SetMultiPeakHeight(it, il, ixy, io, multiPeakHeight);
                mSimRHICfHit -> SetMultiEnergySum(it, il, ixy, io, multiEnergySum);
                }
            }
        }
    }

    // RHICf Point
    for(unsigned int i=0; i<mRHICfColl->numberOfPoints(); i++){
        Int_t towerIdx = mRHICfColl -> pointCollection()[i] -> getTowerIdx();
        Int_t pid = mRHICfColl -> pointCollection()[i] -> getPID();
        Float_t posX = mRHICfColl -> pointCollection()[i] -> getPointPos(0);
        Float_t posY = mRHICfColl -> pointCollection()[i] -> getPointPos(1);
        Float_t photonE = mRHICfColl -> pointCollection()[i] -> getPointEnergy(0);
        Float_t hadronE = mRHICfColl -> pointCollection()[i] -> getPointEnergy(1);

        StRHICfSimRHICfPoint* simRHICfPoint = mSimDst -> GetSimRHICfPoint(i);
        simRHICfPoint -> SetTowerIdx(towerIdx);
        simRHICfPoint -> SetPID(pid);
        simRHICfPoint -> SetPointPos(posX, posY);
        simRHICfPoint -> SetPointEnergy(photonE, hadronE);
    }

    LOG_INFO << " StRHICfSimConverter::SaveRecoData() -- RHICfPoint number: " << mRHICfColl->numberOfPoints() << endm;

    mOutSimDstTree -> Fill();
  
    return kStOk;
}

void StRHICfSimConverter::InitRHICfGeometry()
{
    if(mRHICfRunType < rTLtype || mRHICfRunType > rTOPtype){
        LOG_ERROR << "StRHICfSimConverter::InitRHICfGeometry() warning!!! RHICf run type is not setted!!!" << endm;
    }

    double distTowersCenter = 4.74; // [cm], Distance between the center of small and large tower
    double towerPosYByRun = 0.; // [cm], RHICf detector y-axis shift position by run types with respect to global coordinate
    if(mRHICfRunType == rTLtype){towerPosYByRun = -4.74;}
    if(mRHICfRunType == rTStype){towerPosYByRun = 0.;}
    if(mRHICfRunType == rTOPtype){towerPosYByRun = 2.16;}

    mRHICfPoly = new TH2Poly();
    mRHICfPoly -> SetName("RHICfPoly");
    mRHICfPoly -> SetStats(0);

    double x[4], y[4];
    for(int tower=0; tower<2; tower++){
        double towerGlobalPosY = towerPosYByRun;
        if(tower == RHICfTower::LargeTower){towerGlobalPosY += distTowersCenter;}

        for(int boundary=0; boundary<4; boundary++){
            x[boundary] = GetRHICfDetectorBoundary(tower, 0, boundary);
            y[boundary] = towerGlobalPosY + GetRHICfDetectorBoundary(tower, 1, boundary);
        }
        mRHICfPoly -> AddBin(4, x, y);
    }
}

Int_t StRHICfSimConverter::GetRHICfGeoHit(double vtxStart[], double vtxEnd[], double mom[], double e, int gepid)
{
    if(!IsInterestedParticle(gepid)){return -1;}
    if(e < 1.){return -1;} // energy cut 1 GeV

    double p = sqrt(mom[0]*mom[0] + mom[1]*mom[1] + mom[2]*mom[2]);
    double unitVecX = mom[0]/p;
    double unitVecY = mom[1]/p;
    double unitVecZ = mom[2]/p;

    if(vtxEnd[2] > -9990.){return -1;} // stopped track cut
    if(unitVecZ < 0){return -1;} // opposite side cut

    double mRHICfDetZ = 1780.; // [cm]
    double z = mRHICfDetZ - vtxStart[2];
    if(z < 0.){return -1;} // create z-position cut

    double x = z * (unitVecX/unitVecZ) + vtxStart[0];
    double y = z * (unitVecY/unitVecZ) + vtxStart[1];

    int type = mRHICfPoly -> FindBin(x, y);
    if(type < 1 || type > 2){return -1;} // RHICf geometrical hit cut

    return type;
}

double StRHICfSimConverter::GetRHICfDetectorBoundary(int towerIdx, int xyIdx, int boundaryIdx)
{
  const double SQRT2 = sqrt(2);
  double detectorSize = 0.;
  if(towerIdx == RHICfTower::SmallTower){detectorSize = 2.;} // [cm]
  else if(towerIdx == RHICfTower::LargeTower){detectorSize = 4.;} // [cm]
  else{return -999.;}

  if(boundaryIdx < 0 || boundaryIdx > 3){return -999.;}
  bool isEvenNumber = (boundaryIdx%2 == 0)? true : false;

  double valueSign = (boundaryIdx < 2)? 1. : -1.;
  double detectorBoundaryPoint = valueSign * SQRT2 * detectorSize/2.;

  if(xyIdx == 0){ // x-axis
    if(isEvenNumber){ return detectorBoundaryPoint; }// even number
    else{ return 0.; }// odd number
  }
  else if(xyIdx == 1){ // y-axis
    if(isEvenNumber){ return 0.; } // even number
    else{ return detectorBoundaryPoint; }// odd number
  }

  return -999.;
}

ClassImp(StRHICfSimConverter)