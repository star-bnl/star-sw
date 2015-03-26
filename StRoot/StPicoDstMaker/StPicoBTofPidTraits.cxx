#include "TMath.h"

#include "StMuDSTMaker/COMMON/StMuBTofHit.h"
#include "StMuDSTMaker/COMMON/StMuBTofPidTraits.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMessMgr.h"

#include "StPicoConstants.h"
#include "StPicoBTofPidTraits.h"

ClassImp(StPicoBTofPidTraits)

//----------------------------------------------------------------------------------
StPicoBTofPidTraits::StPicoBTofPidTraits() :
  mTrackIndex(-1),
  mBTofCellId(-1), mBTofMatchFlag(-1), 
  mBTof(0), mBTofBeta(0), 
  mBTofYLocal(-999), mBTofZLocal(-999),
  mBTofHitPosX(-999), mBTofHitPosY(-999), mBTofHitPosZ(-999)
{
  // constructor
}

//----------------------------------------------------------------------------------
StPicoBTofPidTraits::StPicoBTofPidTraits(const StMuTrack *gTrack, 
					 const StMuTrack *pTrack, 
					 const Int_t index)
{
  mTrackIndex = (index > Pico::SHORTMAX) ? -1 : (Short_t)index;

  StMuBTofHit *btofHit = (StMuBTofHit*)gTrack->tofHit();
  Int_t tray         = btofHit->tray(); 
  Int_t module       = btofHit->module(); 
  Int_t cell         = btofHit->cell();
  Float_t tof        = gTrack->btofPidTraits().timeOfFlight();
  Float_t beta       = (pTrack) ? pTrack->btofPidTraits().beta() : -999.;
  StThreeVectorF pos = gTrack->btofPidTraits().position();

  mBTofCellId  = (Short_t)((tray-1)*192+(module-1)*6+(cell-1));
  mBTofMatchFlag = (UChar_t)(gTrack->btofPidTraits().matchFlag());
  if(tof<0) { mBTof = 0; }
  else { mBTof = (tof*1000.>Pico::USHORTMAX) ? Pico::USHORTMAX : (UShort_t)(TMath::Nint(tof*1000.)); }
  if(beta<0) { mBTofBeta = 0; }
  else { mBTofBeta = (beta*20000.>Pico::USHORTMAX) ? Pico::USHORTMAX : (UShort_t)(TMath::Nint(beta*20000.)); }
  mBTofHitPosX = (fabs(pos.x()*100.)>Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(pos.x()*100.));
  mBTofHitPosY = (fabs(pos.y()*100.)>Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(pos.y()*100.)); 
  mBTofHitPosZ = (fabs(pos.z()*100.)>Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(pos.z()*100.)); 
  mBTofYLocal  = (fabs(gTrack->btofPidTraits().yLocal())*1000.>Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(gTrack->btofPidTraits().yLocal()*1000.));
  mBTofZLocal  = (fabs(gTrack->btofPidTraits().zLocal())*1000.>Pico::SHORTMAX) ? Pico::SHORTMAX : (Short_t)(TMath::Nint(gTrack->btofPidTraits().zLocal()*1000.));
}

//----------------------------------------------------------------------------------
StPicoBTofPidTraits::~StPicoBTofPidTraits()
{
  // destructor
}

//----------------------------------------------------------------------------------
void StPicoBTofPidTraits::Print(const Char_t *option) const
{
  LOG_INFO << " Matched track index = " << mTrackIndex << endm;
  LOG_INFO << " BTOF cellId = " << btofCellId() << " tof = " << btof() << " beta = " << btofBeta() << endm;
  LOG_INFO << " BTOF match = " << btofMatchFlag() << " yLocal/zLocal " << btofYLocal() << " " << btofZLocal() << endm;
}

