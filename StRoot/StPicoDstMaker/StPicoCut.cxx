#include "StEnumerations.h"
#include "StPicoCut.h"
#include "StPicoV0.h"
#include "StPicoTrack.h"
#include "StPicoConstants.h"
#include "StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

ClassImp(StPicoCut)

//----------------------------------------------------------------------------------
StPicoCut::StPicoCut() {}

//----------------------------------------------------------------------------------
StPicoCut::~StPicoCut() {}

//----------------------------------------------------------------------------------
bool StPicoCut::passEvent( StMuEvent *ev )
{
  if(!ev){
//    LOG_INFO << "StPicoCut::passEvent  No StMuEvent" << endm;
    return kFALSE;
  }
  StThreeVectorF pVertex = ev->eventSummary().primaryVertexPosition();
  if(fabs(pVertex.x())<1.e-5 && fabs(pVertex.y())<1.e-5 && fabs(pVertex.z())<1.e-5){
//   LOG_INFO << "StPicoCut::passEvent  bad vertices (x,y,z) = ("
//            << pVertex.x() << ","
//            << pVertex.y() << ","
//            << pVertex.z() << ")"
//            << endm;
    return kFALSE;
  }
  if(fabs(pVertex.z())>Pico::mVzMax){
//    LOG_INFO << "StPicoCut::passEvent  z-vertex out of range, vz = " << pVertex.z() << endm;
    return kFALSE;
  }

  const Float_t vx = pVertex.x() ;
  const Float_t vy = pVertex.y() ;
  if(sqrt(vx*vx+vy*vy)>Pico::mVrMax){
//    LOG_INFO << "StPicoCut::passEvent  vr-vertex out of range, vr = " << sqrt(vx*vx+vy*vy)
//      << ",  vx = " << vx
//      << ",  vy = " << vy
//      << endm;
    return kFALSE ;
  }

  bool isTrg = kFALSE;
  for(int i=0;i<nTrigger;i++) {
    if(ev->triggerIdCollection().nominal().isTrigger(Pico::mTriggerId[i])){
      isTrg = kTRUE;
      break;
    }
  }

  if(!isTrg){
    for(int i=0;i<nTriggerMtd;i++) {
      if(ev->triggerIdCollection().nominal().isTrigger(Pico::mTriggerIdMtd[i])){
	isTrg = kTRUE;
	break;
      }
    }
  }

  if(!isTrg) return kFALSE;

  if(ev->refMult()<Pico::mRefMultMin) return kFALSE;

  return kTRUE;
}

//----------------------------------------------------------------------------------
bool StPicoCut::passTrack( StMuTrack *t )
{
  if(!t) return kFALSE;
  if(t->type()!=global) return kFALSE;
  if(t->flag()<0||t->flag()>1000) {
    return kFALSE;
  }
  if(t->p().perp()<Pico::mPtMin) return kFALSE;
  if(t->dcaGlobal().mag()>Pico::mGDcaMax) return kFALSE;
  if(t->flag()/100<7) {  // TPC tracks
    if( t->nHitsFit(kTpcId) < Pico::mNHitsFitMin ) return kFALSE;
    if( (1.0*t->nHitsFit(kTpcId))/(1.0*t->nHitsPoss(kTpcId)) < Pico::mRatioMin ) return kFALSE;
  } else { // FTPC tracks
    float eta = t->p().pseudoRapidity();
    int nHitsFitFtpc = 0;
    int nHitsMaxFtpc = 0;
    if(eta>0) {
      nHitsFitFtpc = t->nHitsFit(kFtpcWestId);
      nHitsMaxFtpc = t->nHitsPoss(kFtpcWestId);
    } else {
      nHitsFitFtpc = t->nHitsFit(kFtpcEastId);
      nHitsMaxFtpc = t->nHitsPoss(kFtpcEastId);
    }
    float ratioFtpc  = (1.*nHitsFitFtpc)/(1.*nHitsMaxFtpc);
    if( nHitsFitFtpc < Pico::mNHitsFtpcFlowMin ) return kFALSE;
    if( ratioFtpc < Pico::mRatioMin ) return kFALSE;
  }

  return kTRUE;
}
/*
//----------------------------------------------------------------------------------
bool StPicoCut::passV0Daughter( StPicoTrack *t )
{
  if(!t) return kFALSE;
  if(t->nHitsFit()<Pico::mV0DaughterNHitsFitMin) return kFALSE;

  bool pionCand = fabs(t->nSigmaPion())<=Pico::mV0DaughterNSigmaPionMax && 
                  ( ( t->gMom().perp()<Pico::mV0DaughterDca2VertexPtMax && t->dca()>=Pico::mV0KsPionDca2VertexMin ) ||
                    t->gMom().perp()>=Pico::mV0DaughterDca2VertexPtMax );
  bool protonCand = fabs(t->nSigmaProton())<=Pico::mV0DaughterNSigmaProtonMax &&
                  ( ( t->gMom().perp()<Pico::mV0DaughterDca2VertexPtMax && t->dca()>=Pico::mV0LambdaProtonDca2VertexMin ) ||
                    t->gMom().perp()>=Pico::mV0DaughterDca2VertexPtMax );
  if( !pionCand && !protonCand ) return kFALSE;

  return kTRUE;
}
//----------------------------------------------------------------------------------
bool StPicoCut::passV0( StPicoV0 *v0, StMuEvent *ev )
{
  if(!v0) return kFALSE;
  if(v0->dcaDaughters()>Pico::mV0DcaDaughtersMax) return kFALSE;
  StThreeVectorF v0Mom = v0->momentum(pos) + v0->momentum(neg);
  StThreeVectorF pVtx = ev->eventSummary().primaryVertexPosition();
  if(v0Mom.dot(v0->v0Pos()-pVtx)<=0) return kFALSE;  // V0 going away from primary vertex

  return kTRUE;
}
//----------------------------------------------------------------------------------
bool StPicoCut::passKs( StPicoV0 *v0 )
{
  if(!v0) return kFALSE;

  v0->setParticleHypothesis(pion, pion);
  StPicoTrack *t_pos = v0->track(pos);
  StPicoTrack *t_neg = v0->track(neg);

  // dEdx selection
  if(fabs(t_pos->nSigmaPion())>Pico::mV0KsNSigmaPionMax) return kFALSE;
  if(fabs(t_neg->nSigmaPion())>Pico::mV0KsNSigmaPionMax) return kFALSE;

  // daughter-pVertex dca cut for low pT tracks
  if(t_pos->gMom().perp()<Pico::mV0DaughterDca2VertexPtMax &&
     t_pos->dca()<Pico::mV0KsPionDca2VertexMin) return kFALSE;
  if(t_neg->gMom().perp()<Pico::mV0DaughterDca2VertexPtMax &&
     t_neg->dca()<Pico::mV0KsPionDca2VertexMin) return kFALSE;

  // typology cut
  if(v0->dca2Vertex()>Pico::mV0KsDca2VertexMax) return kFALSE;
  if(v0->decayLength()<Pico::mV0KsDecayLengthMin || v0->decayLength()>Pico::mV0KsDecayLengthMax) return kFALSE;

  // mass cut
  if(fabs(v0->m()-Pico::mMassV0[ks])>Pico::mV0KsMassWindowMax) return kFALSE;

  return kTRUE;
}

//----------------------------------------------------------------------------------
bool StPicoCut::passLambda( StPicoV0 *v0 )
{
  if(!v0) return kFALSE;

  v0->setParticleHypothesis(proton, pion);
  StPicoTrack *t_pos = v0->track(pos);
  StPicoTrack *t_neg = v0->track(neg);

  // dEdx selection
  if(fabs(t_pos->nSigmaProton())>Pico::mV0LambdaNSigmaProtonMax) return kFALSE;
  if(fabs(t_neg->nSigmaPion())>Pico::mV0LambdaNSigmaPionMax) return kFALSE;

  // daughter-pVertex dca cut for low pT tracks
  if(t_pos->gMom().perp()<Pico::mV0DaughterDca2VertexPtMax && 
     t_pos->dca()<Pico::mV0LambdaProtonDca2VertexMin) return kFALSE;
  if(t_neg->gMom().perp()<Pico::mV0DaughterDca2VertexPtMax && 
     t_neg->dca()<Pico::mV0LambdaPionDca2VertexMin) return kFALSE;  

  // typology cut
  if(v0->dca2Vertex()>Pico::mV0LambdaDca2VertexMax) return kFALSE; 
  if(v0->decayLength()<Pico::mV0LambdaDecayLengthMin || v0->decayLength()>Pico::mV0LambdaDecayLengthMax) return kFALSE;

  // mass cut
  if(fabs(v0->m()-Pico::mMassV0[lambda])>Pico::mV0LambdaMassWindowMax) return kFALSE;

  return kTRUE;
}

//----------------------------------------------------------------------------------
bool StPicoCut::passLbar( StPicoV0 *v0 )
{
  if(!v0) return kFALSE;

  v0->setParticleHypothesis(pion, proton);
  StPicoTrack *t_pos = v0->track(pos);
  StPicoTrack *t_neg = v0->track(neg);

  // dEdx selection
  if(fabs(t_pos->nSigmaPion())>Pico::mV0LambdaNSigmaPionMax) return kFALSE;
  if(fabs(t_neg->nSigmaProton())>Pico::mV0LambdaNSigmaProtonMax) return kFALSE;

  // daughter-pVertex dca cut for low pT tracks   
  if(t_pos->gMom().perp()<Pico::mV0DaughterDca2VertexPtMax && 
     t_pos->dca()<Pico::mV0LambdaPionDca2VertexMin) return kFALSE;
  if(t_neg->gMom().perp()<Pico::mV0DaughterDca2VertexPtMax && 
     t_neg->dca()<Pico::mV0LambdaProtonDca2VertexMin) return kFALSE;

  // typology cut
  if(v0->dca2Vertex()>Pico::mV0LambdaDca2VertexMax) return kFALSE;
  if(v0->decayLength()<Pico::mV0LambdaDecayLengthMin || v0->decayLength()>Pico::mV0LambdaDecayLengthMax) return kFALSE;

  // mass cut
  if(fabs(v0->m()-Pico::mMassV0[lambda])>Pico::mV0LambdaMassWindowMax) return kFALSE; 

  return kTRUE;
}

//----------------------------------------------------------------------------------
int StPicoCut::flowFlag( StMuTrack *p )
{
  if(!p) return others;
  if(p->type()!=primary) return others;
  if(p->vertexIndex()!=0) return others;

  StThreeVectorF mom = p->p();
  float pt = mom.perp();
  float eta = mom.pseudoRapidity();
  float gDca = p->dcaGlobal().mag();
  int nHitsFitTpc = p->nHitsFit(kTpcId);
  int nHitsMaxTpc = p->nHitsPoss(kTpcId);
  float ratioTpc  = (1.*nHitsFitTpc)/(1.*nHitsMaxTpc);
  int nHitsFitFtpc = 0;
  int nHitsMaxFtpc = 0;
  if(eta>0) {
    nHitsFitFtpc = p->nHitsFit(kFtpcWestId);
    nHitsMaxFtpc = p->nHitsPoss(kFtpcWestId);
  } else {
    nHitsFitFtpc = p->nHitsFit(kFtpcEastId);
    nHitsMaxFtpc = p->nHitsPoss(kFtpcEastId);
  }
  float ratioFtpc  = (1.*nHitsFitFtpc)/(1.*nHitsMaxFtpc);

  if( pt > Pico::mPtTpcFlowMin && pt < Pico::mPtTpcFlowMax &&
      gDca < Pico::mDcaTpcFlowMax &&
      fabs(eta) < Pico::mEtaTpcFlowMax &&
      nHitsFitTpc >= Pico::mNHitsTpcFlowMin && ratioTpc > Pico::mRatioMin )
    return tpcFlow;

  if( pt > Pico::mPtFtpcFlowMin && pt < Pico::mPtFtpcFlowMax &&
      gDca < Pico::mDcaFtpcFlowMax &&
      fabs(eta) < Pico::mEtaFtpcFlowMax && fabs(eta) > Pico::mEtaFtpcFlowMin &&
      nHitsFitFtpc >= Pico::mNHitsFtpcFlowMin && ratioFtpc > Pico::mRatioMin )     
    return ftpcFlow;

  return others;
}
*/