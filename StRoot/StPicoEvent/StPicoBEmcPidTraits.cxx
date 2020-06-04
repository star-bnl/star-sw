#include <limits>
#include "TMath.h"
#include "St_base/StMessMgr.h"

#include "StPicoEvent/StPicoBEmcPidTraits.h"


//----------------------------------------------------------------------------------
StPicoBEmcPidTraits::StPicoBEmcPidTraits() :
  mTrackIndex(-1),
  mBemcId(-9999), mBemcAdc0(-9999), mBemcE0(-9999), mBemcE(-9999),
  mBemcZDist(-9999), mBemcPhiDist(-9999),
  mBemcSmdNEta(std::numeric_limits<unsigned char>::max()), mBemcSmdNPhi(std::numeric_limits<unsigned char>::max()),
  mBtowId(-9999), mBtowId23(std::numeric_limits<unsigned char>::max()),
  mBtowE(-9999), mBtowE2(-9999), mBtowE3(-9999),
  mBtowEtaDist(-9999), mBtowPhiDist(-9999)
{
  // constructor
}

//----------------------------------------------------------------------------------
StPicoBEmcPidTraits::StPicoBEmcPidTraits(Int_t index, Int_t id, Int_t adc0, Float_t const* e, Float_t const* dist, Int_t const* nhit, Int_t const* ntow): StPicoBEmcPidTraits()
{
  mTrackIndex = (index > std::numeric_limits<short>::max()) ? -1 : (Short_t)index;

  auto getConstrainedShort = [](float x)
  {
    return fabs(x) >= std::numeric_limits<short>::max() ? std::numeric_limits<short>::max() : (short)(TMath::Nint(x));
  };

  mBemcId       = (id > std::numeric_limits<short>::max()) ? -1 : (Short_t)id;
  mBemcAdc0     = (adc0 > std::numeric_limits<unsigned short>::max()) ? std::numeric_limits<unsigned short>::max() : (UShort_t)adc0;
  mBemcE0       = getConstrainedShort(e[0] * 1000.);
  mBemcE        = getConstrainedShort(e[1] * 1000.);
  mBemcZDist    = getConstrainedShort(dist[0] * 100.);
  mBemcPhiDist  = getConstrainedShort(dist[1] * 10000.);
  mBemcSmdNEta     = (nhit[0] > std::numeric_limits<unsigned char>::max()) ? std::numeric_limits<unsigned char>::max() : (UChar_t)(nhit[0]);
  mBemcSmdNPhi     = (nhit[1] > std::numeric_limits<unsigned char>::max()) ? std::numeric_limits<unsigned char>::max() : (UChar_t)(nhit[1]);

  mBtowId       = (ntow[0] <= 0 || ntow[0] > 4800) ? -1 : (Short_t)ntow[0];
  mBtowId23     = (ntow[1] < 0 || ntow[1] >= 9 || ntow[2] < 0 || ntow[2] >= 9) ? -1 : (Char_t)(ntow[1] * 10 + ntow[2]);
  mBtowE        = getConstrainedShort(e[2] * 1000.);
  mBtowE2       = getConstrainedShort(e[3] * 1000.);
  mBtowE3       = getConstrainedShort(e[4] * 1000.);
  mBtowEtaDist  = getConstrainedShort(dist[2] * 10000.);
  mBtowPhiDist  = getConstrainedShort(dist[3] * 10000.);
}

//----------------------------------------------------------------------------------
StPicoBEmcPidTraits::~StPicoBEmcPidTraits()
{
  // destructor
}

//----------------------------------------------------------------------------------
void StPicoBEmcPidTraits::Print(const Char_t* option) const
{
  LOG_INFO << "Matched track index = " << mTrackIndex << endm;
  LOG_INFO << " BEMC Id = " << bemcId() << " BTOW Adc0 = " << bemcAdc0() << " bemc E0 = " << bemcE0() << " e = " << bemcE() << endm;
  LOG_INFO << " BEMC distz = " << bemcZDist() << " distphi = " << bemcPhiDist() << endm;
  LOG_INFO << " BSMD nEta/nPhi = " << bemcSmdNEta() << "/" << bemcSmdNPhi() << endm;
  LOG_INFO << " BTOW Id = " << btowId() << " tower Id 2/3 = " << btowId2() << " " << btowId3() << endm;
  LOG_INFO << " BTOW energy = " << btowE() << " " << btowE2() << " " << btowE3() << endm;
  LOG_INFO << " BTOW position to center = " << btowEtaDist() << " " << btowPhiDist() << endm;
}
