#include "StPicoTrack.h"
#include "StPicoCut.h"
#include "StPicoConstants.h"
#include "StPicoDstMaker.h"
#include "TVector2.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

ClassImp(StPicoTrack)

//----------------------------------------------------------------------------------
StPicoTrack::StPicoTrack() : mId(0), mChi2(Pico::USHORTMAX), mPMomentum(0., 0., 0.), mDedx(0),
   mNHitsFit(0), mNHitsMax(0), mNHitsDedx(0), mNSigmaPion(Pico::SHORTMAX), mNSigmaKaon(Pico::SHORTMAX),
   mNSigmaProton(Pico::SHORTMAX), mNSigmaElectron(Pico::SHORTMAX),
   mMap0(0), mMap1(0), mPar(), mErrMatrix(), mEmcPidTraitsIndex(-1),
   mBTofPidTraitsIndex(-1), mMtdPidTraitsIndex(-1)
{
}

/////////////////////////////////////////////////////////////////////////////////////////
// t - the global track.  p - the associated primary track from the first primary vertex
/////////////////////////////////////////////////////////////////////////////////////////
//----------------------------------------------------------------------------------
StPicoTrack::StPicoTrack(StMuTrack* t, StMuTrack* p, float phi_weight, int flowFlag, double B, StDcaGeometry* dcaG)
: mId(0), mChi2(Pico::USHORTMAX), mPMomentum(0., 0., 0.), mDedx(0),
   mNHitsFit(0), mNHitsMax(0), mNHitsDedx(0), mNSigmaPion(Pico::SHORTMAX), mNSigmaKaon(Pico::SHORTMAX),
   mNSigmaProton(Pico::SHORTMAX), mNSigmaElectron(Pico::SHORTMAX),                 
   mMap0(0), mMap1(0), mPar(), mErrMatrix(), mEmcPidTraitsIndex(-1),                   
   mBTofPidTraitsIndex(-1), mMtdPidTraitsIndex(-1)

{
   if (!t || t->type() != global || (p && (p->type() != primary || p->id() != t->id())))
   {
      LOG_WARN << " Bad StPicoTrack constructor ...... Check!" << endm;
   }
   else
   {
      mId        = (UShort_t)t->id();
      mChi2      = (t->chi2() * 1000. > Pico::USHORTMAX) ? Pico::USHORTMAX : (UShort_t)(TMath::Nint(t->chi2() * 1000.));
      if (p)
      {
         mPMomentum = p->p();
      }
      int q      = t->charge();
      mDedx      = (t->dEdx() * 1e6 * 1000. > Pico::USHORTMAX) ? Pico::USHORTMAX : (UShort_t)(TMath::Nint(t->dEdx() * 1e6 * 1000.));
      int flag = t->flag();
      if (flag / 100 < 7) // TPC tracks
      {
         mNHitsFit  = (Char_t)(t->nHitsFit(kTpcId) * q);
         mNHitsMax  = (UChar_t)(t->nHitsPoss(kTpcId));
      }
      else     // FTPC tracks
      {
         if (t->helix().momentum(B * kilogauss).pseudoRapidity() > 0.)
         {
            mNHitsFit  = (Char_t)(t->nHitsFit(kFtpcWestId) * q);
            mNHitsMax  = (UChar_t)(t->nHitsPoss(kFtpcWestId));
         }
         else
         {
            mNHitsFit  = (Char_t)(t->nHitsFit(kFtpcEastId) * q);
            mNHitsMax  = (UChar_t)(t->nHitsPoss(kFtpcEastId));
         }
      }
      mNHitsDedx = (Char_t)(t->nHitsDedx());
      mNSigmaPion     = (fabs(t->nSigmaPion() * 100.) > Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(t->nSigmaPion() * 100.));
      mNSigmaKaon     = (fabs(t->nSigmaKaon() * 100.) > Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(t->nSigmaKaon() * 100.));
      mNSigmaProton   = (fabs(t->nSigmaProton() * 100.) > Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(t->nSigmaProton() * 100.));
      mNSigmaElectron = (fabs(t->nSigmaElectron() * 100.) > Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(t->nSigmaElectron() * 100.));

      mMap0 = (UInt_t)(t->topologyMap().data(0));
      mMap1 = (UInt_t)(t->topologyMap().data(1));

/*
      unsigned int map0 = t->topologyMap().data(0);
      unsigned int map1 = t->topologyMap().data(1);
      mNHitsMapHFT = map0 >> 1 & 0x7F; // see hitMap definition in StTrackTopologyMap
      mFirstTpcHitRow = 0;
      mLastTpcHitRow = 0;
      for (int row = 1; row <= 24; row++)
      {
         if (map0 >> (row + 7) & 0x1)
         {
            mFirstTpcHitRow = row;
            break;
         }
      }
      if (mFirstTpcHitRow == 0)
      {
         for (int row = 25; row <= 45; row++)
         {
            if (map1 >> (row - 25) & 0x1)
            {
               mFirstTpcHitRow = row;
               break;
            }
         }
      }
      for (int row = 45; row >= 25; row--)
      {
         if (map1 >> (row - 25) & 0x1)
         {
            mLastTpcHitRow = row;
            break;
         }
      }
      if (mLastTpcHitRow == 0)
      {
         for (int row = 24; row >= 1; row--)
         {
            if (map0 >> (row + 7) & 0x1)
            {
               mLastTpcHitRow = row;
               break;
            }
         }
      }
*/

      if (dcaG)
      {
         const float* params = dcaG->params();
         const float* errMatrix = dcaG->errMatrix();
         for (int i = 0; i < 6; i++) mPar[i] = params[i];
         for (int i = 0; i < 15; i++) mErrMatrix[i] = errMatrix[i];

      }
      else
      {
         cout << " This track doesn't have a dcaGeometry!!!!" << endl;
      }

   }// end if
}

//----------------------------------------------------------------------------------
StPicoTrack::~StPicoTrack()
{
   /* noop */
}
//----------------------------------------------------------------------------------
void StPicoTrack::Print(const Char_t *option) const
{
   if (strcmp(option, "tpc") == 0 || strcmp(option, "") == 0)
   {
      LOG_INFO << "id=" << id()
               << " chi2=" << chi2()
               << endm;
      LOG_INFO << "pMom=" << pMom() << endm;
      LOG_INFO << " nHitsFit = " << nHitsFit() << " nHitsdEdx = " << nHitsDedx() << endm;
      LOG_INFO << " nSigma Pi/K/P/E = " << nSigmaPion() << "/" << nSigmaKaon() << "/" << nSigmaProton() << "/" << nSigmaElectron() << endm;
   }
}
