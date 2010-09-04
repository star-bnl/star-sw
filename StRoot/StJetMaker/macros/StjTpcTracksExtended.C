#include "StMaker.h"


#include <StMuDSTMaker/COMMON/StMuTrack.h>
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <StTriggerUtilities/StTriggerSimuMaker.h>
#include <StTriggerUtilities/StTriggerSimuResult.h>
#include <StTriggerUtilities/Bemc/StBemcTriggerSimu.h>

#include <TChain.h>
#include <TDirectory.h>
#include <TVector3.h>

#include <libgen.h>

#include <string>
#include <cstring>

using namespace std;

class StjTpcTracksExtended : public StMaker {

public:

  StjTpcTracksExtended(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
    : StMaker(name), _file(file), _uDstMaker(uDstMaker)
  { }

  virtual ~StjTpcTracksExtended() { }

  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjTpcTracksExtended.C,v 1.1.2.2 2010/09/04 04:35:44 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  TDirectory* _file;
  TTree* _tree;

  StMuDstMaker* _uDstMaker;

  Int_t _runNumber;
  Int_t _eventId;
  Double_t _vertexZ;

  UInt_t   _nTracks;
  Double_t _pt[4096];
  Double_t _eta[4096];
  Double_t _phi[4096];

  Short_t _type[4096];
  Short_t _flag[4096];
  Int_t _index2Global[4096];
  Int_t _vertexIndex[4096];
  UShort_t _nHits[4096];
  UShort_t _nHitsPoss[4096];
  UShort_t _nHitsDedx[4096];
  UShort_t _nHitsFit[4096];
  Double_t _pidProbElectron[4096];
  Double_t _pidProbPion[4096];
  Double_t _pidProbKaon[4096];
  Double_t _pidProbProton[4096];
  Double_t _nSigmaElectron[4096];
  Double_t _nSigmaPion[4096];
  Double_t _nSigmaKaon[4096];
  Double_t _nSigmaProton[4096];
  Double_t _dEdx[4096];
  Double_t _chi2xy[4096];
  Double_t _chi2z[4096];
  Double_t _dcax[4096];
  Double_t _dcay[4096];
  Double_t _dcaz[4096];
  Double_t _dcaGlobalx[4096];
  Double_t _dcaGlobaly[4096];
  Double_t _dcaGlobalz[4096];
  Double_t _firstPointx[4096];
  Double_t _firstPointy[4096];
  Double_t _firstPointz[4096];
  Double_t _lastPointx[4096];
  Double_t _lastPointy[4096];
  Double_t _lastPointz[4096];
  Double_t _helixdipAngle[4096];
  Double_t _helixcurvature[4096];
  Double_t _helixphase[4096];
  Double_t _helixxcenter[4096];
  Double_t _helixycenter[4096];
  Int_t _helixh[4096];
  Double_t _outerHelixdipAngle[4096];
  Double_t _outerHelixcurvature[4096];
  Double_t _outerHelixphase[4096];
  Double_t _outerHelixxcenter[4096];
  Double_t _outerHelixycenter[4096];
  Int_t _outerHelixh[4096];

  Short_t  _trackId[4096];
  Int_t    _trackIndex[4096];

  Double_t _magneticField;


public:

  Int_t Init()
  {
    _file->cd();
    _tree = new TTree("tpcTracksExtended", "tpcTracksExtended");
    _tree->SetAutoSave(kMaxLong64);
    _tree->SetMaxTreeSize(kMaxLong64);


    _tree->Branch("runNumber"  , &_runNumber  , "runNumber/I"  );
    _tree->Branch("eventId"    , &_eventId    , "eventId/I"    );

    _tree->Branch("nTracks"    , &_nTracks         , "nTracks/i"                );
    _tree->Branch("pt"         ,  _pt              , "pt[nTracks]/D"            );
    _tree->Branch("eta"        ,  _eta             , "eta[nTracks]/D"           );
    _tree->Branch("phi"        ,  _phi             , "phi[nTracks]/D"           );

    _tree->Branch("trackId"       ,  _trackId         , "trackId[nTracks]/S"       );
    _tree->Branch("trackIndex"    ,  _trackIndex      , "trackIndex[nTracks]/I"    );

    _tree->Branch("type"                , _type               ,"type[nTracks]/S" );
    _tree->Branch("flag"                , _flag               ,"flag[nTracks]/S" );
    _tree->Branch("index2Global"        , _index2Global       ,"index2Global[nTracks]/I" );
    _tree->Branch("vertexIndex"         , _vertexIndex        ,"vertexIndex[nTracks]/I" );
    _tree->Branch("nHits"               , _nHits              ,"nHits[nTracks]/s" );
    _tree->Branch("nHitsPoss"           , _nHitsPoss          ,"nHitsPoss[nTracks]/s" );
    _tree->Branch("nHitsDedx"           , _nHitsDedx          ,"nHitsDedx[nTracks]/s" );
    _tree->Branch("nHitsFit"            , _nHitsFit           ,"nHitsFit[nTracks]/s" );
    _tree->Branch("pidProbElectron"     , _pidProbElectron    ,"pidProbElectron[nTracks]/D" );
    _tree->Branch("pidProbPion"         , _pidProbPion        ,"pidProbPion[nTracks]/D" );
    _tree->Branch("pidProbKaon"         , _pidProbKaon        ,"pidProbKaon[nTracks]/D" );
    _tree->Branch("pidProbProton"       , _pidProbProton      ,"pidProbProton[nTracks]/D" );
    _tree->Branch("nSigmaElectron"      , _nSigmaElectron     ,"nSigmaElectron[nTracks]/D" );
    _tree->Branch("nSigmaPion"          , _nSigmaPion         ,"nSigmaPion[nTracks]/D" );
    _tree->Branch("nSigmaKaon"          , _nSigmaKaon         ,"nSigmaKaon[nTracks]/D" );
    _tree->Branch("nSigmaProton"        , _nSigmaProton       ,"nSigmaProton[nTracks]/D" );
    _tree->Branch("dEdx"                , _dEdx               ,"dEdx[nTracks]/D" );
    _tree->Branch("chi2xy"              , _chi2xy             ,"chi2xy[nTracks]/D" );
    _tree->Branch("chi2z"               , _chi2z              ,"chi2z[nTracks]/D" );
    _tree->Branch("dcax"                , _dcax               ,"dcax[nTracks]/D" );
    _tree->Branch("dcay"                , _dcay               ,"dcay[nTracks]/D" );
    _tree->Branch("dcaz"                , _dcaz               ,"dcaz[nTracks]/D" );
    _tree->Branch("dcaGlobalx"          , _dcaGlobalx         ,"dcaGlobalx[nTracks]/D" );
    _tree->Branch("dcaGlobaly"          , _dcaGlobaly         ,"dcaGlobaly[nTracks]/D" );
    _tree->Branch("dcaGlobalz"          , _dcaGlobalz         ,"dcaGlobalz[nTracks]/D" );
    _tree->Branch("firstPointx"         , _firstPointx        ,"firstPointx[nTracks]/D" );
    _tree->Branch("firstPointy"         , _firstPointy        ,"firstPointy[nTracks]/D" );
    _tree->Branch("firstPointz"         , _firstPointz        ,"firstPointz[nTracks]/D" );
    _tree->Branch("lastPointx"          , _lastPointx         ,"lastPointx[nTracks]/D" );
    _tree->Branch("lastPointy"          , _lastPointy         ,"lastPointy[nTracks]/D" );
    _tree->Branch("lastPointz"          , _lastPointz         ,"lastPointz[nTracks]/D" );
    _tree->Branch("helixdipAngle"       , _helixdipAngle      ,"helixdipAngle[nTracks]/D" );
    _tree->Branch("helixcurvature"      , _helixcurvature     ,"helixcurvature[nTracks]/D" );
    _tree->Branch("helixphase"          , _helixphase         ,"helixphase[nTracks]/D" );
    _tree->Branch("helixxcenter"        , _helixxcenter       ,"helixxcenter[nTracks]/D" );
    _tree->Branch("helixycenter"        , _helixycenter       ,"helixycenter[nTracks]/D" );
    _tree->Branch("helixh"              , _helixh             ,"helixh[nTracks]/I" );
    _tree->Branch("outerHelixdipAngle"  , _outerHelixdipAngle ,"outerHelixdipAngle[nTracks]/D" );
    _tree->Branch("outerHelixcurvature" , _outerHelixcurvature,"outerHelixcurvature[nTracks]/D" );
    _tree->Branch("outerHelixphase"     , _outerHelixphase    ,"outerHelixphase[nTracks]/D" );
    _tree->Branch("outerHelixxcenter"   , _outerHelixxcenter  ,"outerHelixxcenter[nTracks]/D" );
    _tree->Branch("outerHelixycenter"   , _outerHelixycenter  ,"outerHelixycenter[nTracks]/D" );
    _tree->Branch("outerHelixh"         , _outerHelixh        ,"outerHelixh[nTracks]/I" );

    _tree->Branch("magneticField"       , &_magneticField      ,"magneticField/D" );

    _tree->Branch("vertexZ"    , &_vertexZ    , "vertexZ/D"    );    

    return kStOk;
  }

  Int_t Make()
  {
    _runNumber = _uDstMaker->muDst()->event()->runId();
    _eventId = _uDstMaker->muDst()->event()->eventId();
    _vertexZ = _uDstMaker->muDst()->event()->primaryVertexPosition().z();

    StMuDst* uDst = _uDstMaker->muDst();
    UInt_t nTracks = uDst->numberOfPrimaryTracks();


    for(int i = 0; i < nTracks; ++i) {
      const StMuTrack* mutrack = uDst->primaryTracks(i);

      if(mutrack->flag() < 0) continue;

      if(mutrack->topologyMap().trackFtpcEast() || mutrack->topologyMap().trackFtpcWest()) continue;

      fillTrack(mutrack, i);
      _nTracks = i + 1;
    }


    double magneticField = uDst->event()->magneticField()/10.0; // Tesla
    _magneticField = magneticField;

    _tree->Fill();
    return kStOk;
  }

  void fillTrack(const StMuTrack* mutrack, int i)
  {
    TVector3 p(mutrack->momentum().x(), mutrack->momentum().y(), mutrack->momentum().z());
    _pt[i] = p.Pt();
    _eta[i] = p.Eta();
    _phi[i] = p.Phi();

    _trackId[i] = mutrack->id();
    _trackIndex[i] = i;

    _type[i]                = mutrack->type();
    _flag[i]                = mutrack->flag();
    _index2Global[i]        = mutrack->index2Global();
    _vertexIndex[i]         = mutrack->vertexIndex();
    _nHits[i]               = mutrack->nHits();
    _nHitsPoss[i]           = mutrack->nHitsPoss();
    _nHitsDedx[i]           = mutrack->nHitsDedx();       
    _nHitsFit[i]            = mutrack->nHitsFit();
    _pidProbElectron[i]     = mutrack->pidProbElectron();
    _pidProbPion[i]         = mutrack->pidProbPion();
    _pidProbKaon[i]         = mutrack->pidProbKaon();
    _pidProbProton[i]       = mutrack->pidProbProton();
    _nSigmaElectron[i]      = mutrack->nSigmaElectron();
    _nSigmaPion[i]          = mutrack->nSigmaPion();
    _nSigmaKaon[i]          = mutrack->nSigmaKaon();
    _nSigmaProton[i]        = mutrack->nSigmaProton();
    _dEdx[i]                = mutrack->dEdx();
    _chi2xy[i]              = mutrack->chi2xy();
    _chi2z[i]               = mutrack->chi2z();
    _dcax[i]                = mutrack->dca().x();
    _dcay[i]                = mutrack->dca().y();
    _dcaz[i]                = mutrack->dca().z();
    _dcaGlobalx[i]          = mutrack->dcaGlobal().x();
    _dcaGlobaly[i]          = mutrack->dcaGlobal().y();
    _dcaGlobalz[i]          = mutrack->dcaGlobal().z();
    _firstPointx[i]         = mutrack->firstPoint().x();
    _firstPointy[i]         = mutrack->firstPoint().y();
    _firstPointz[i]         = mutrack->firstPoint().z();
    _lastPointx[i]          = mutrack->lastPoint().x();
    _lastPointy[i]          = mutrack->lastPoint().y();
    _lastPointz[i]          = mutrack->lastPoint().z();
    _helixdipAngle[i]       = mutrack->helix().dipAngle();
    _helixcurvature[i]      = mutrack->helix().curvature();
    _helixphase[i]          = mutrack->helix().phase();
    _helixxcenter[i]        = mutrack->helix().xcenter();
    _helixycenter[i]        = mutrack->helix().ycenter();
    _helixh[i]              = mutrack->helix().h();
    _outerHelixdipAngle[i]  = mutrack->outerHelix().dipAngle();
    _outerHelixcurvature[i] = mutrack->outerHelix().curvature();
    _outerHelixphase[i]     = mutrack->outerHelix().phase();
    _outerHelixxcenter[i]   = mutrack->outerHelix().xcenter();
    _outerHelixycenter[i]   = mutrack->outerHelix().ycenter();
    _outerHelixh[i]         = mutrack->outerHelix().h();
  }

  Int_t Finish()
  {
    _tree->BuildIndex("runNumber", "eventId");
    return kStOk;
  }
  ClassDef(StjTpcTracksExtended, 0)
};
