//
// StPicoBEmcPidTraits holds information about BEMC-matched tracks
//

// C++ headers
#include <limits>

// ROOT headers
#include "TMath.h"

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoBEmcPidTraits.h"

ClassImp(StPicoBEmcPidTraits)

//_________________
StPicoBEmcPidTraits::StPicoBEmcPidTraits() : TObject(),
  mTrackIndex(-1),
  mBemcId(-9999), mBemcAdc0(-9999), mBemcE0(-9999), mBemcE(-9999),
  mBemcZDist(-9999), mBemcPhiDist(-9999),
  mBemcSmdNEta(std::numeric_limits<unsigned char>::max()), mBemcSmdNPhi(std::numeric_limits<unsigned char>::max()),
  mBtowId(-9999), mBtowId23(std::numeric_limits<unsigned char>::max()),
  mBtowE(-9999), mBtowE2(-9999), mBtowE3(-9999),
  mBtowEtaDist(-9999), mBtowPhiDist(-9999) {
  /* emtpy */
}

//_________________
StPicoBEmcPidTraits::StPicoBEmcPidTraits(Int_t index, Int_t id, Int_t adc0, const Float_t* e, const Float_t* dist, 
					 const Int_t* nhit, const Int_t* ntow) : TObject() {

  mTrackIndex = (index > std::numeric_limits<short>::max()) ? -1 : (Short_t)index;

  auto getConstrainedShort = [](float x) {
    return ( ( TMath::Abs(x) >= std::numeric_limits<short>::max() ) ?
	     std::numeric_limits<short>::max() : (short)(TMath::Nint(x)) );
  };

  mBemcId       = (id > std::numeric_limits<short>::max()) ? -1 : (Short_t)id;
  mBemcAdc0     = ( (adc0 > std::numeric_limits<unsigned short>::max()) ?
		    std::numeric_limits<unsigned short>::max() : (UShort_t)adc0 );
  mBemcE0       = getConstrainedShort(e[0] * 1000.);
  mBemcE        = getConstrainedShort(e[1] * 1000.);
  mBemcZDist    = getConstrainedShort(dist[0] * 100.);
  mBemcPhiDist  = getConstrainedShort(dist[1] * 10000.);
  mBemcSmdNEta  = ( (nhit[0] > std::numeric_limits<unsigned char>::max()) ?
		    std::numeric_limits<unsigned char>::max() : (UChar_t)(nhit[0]) );
  mBemcSmdNPhi  = ( (nhit[1] > std::numeric_limits<unsigned char>::max()) ?
		    std::numeric_limits<unsigned char>::max() : (UChar_t)(nhit[1]) );

  mBtowId       = (ntow[0] <= 0 || ntow[0] > 4800) ? -1 : (Short_t)ntow[0];

  // Logic: If at least one closest to the mactched tower was
  // found than we check the second one. The 1st and the 2nd
  // digits of mBtowId23 represent Ids of the 1st and the 2nd highest
  // towers that are the closest to the track-matched one, respectively.
  if (ntow[1] < 0 || ntow[1] >= 9) {
    if (!(ntow[2] < 0 || ntow[2] >= 9))  { // If 2 towers were found
      mBtowId23 = (Char_t)(ntow[1] * 10 + ntow[2]);
    }
    else {                                 // If only 1 tower was found
      mBtowId23 = (Char_t)(ntow[1] * 10 + 9); 
    }
  }
  else { // If none of the towers with energy>0.2 GeV were found near the matched tower
    mBtowId23 = 99;
  }

  mBtowE        = getConstrainedShort(e[2] * 1000.);
  mBtowE2       = getConstrainedShort(e[3] * 1000.);
  mBtowE3       = getConstrainedShort(e[4] * 1000.);
  mBtowEtaDist  = getConstrainedShort(dist[2] * 10000.);
  mBtowPhiDist  = getConstrainedShort(dist[3] * 10000.);
}

//_________________
StPicoBEmcPidTraits::StPicoBEmcPidTraits(const StPicoBEmcPidTraits &traits) {
  mTrackIndex = traits.mTrackIndex;
  mBemcId = traits.mBemcId;
  mBemcAdc0 = traits.mBemcAdc0;
  mBemcE0 = traits.mBemcE0;
  mBemcE = traits.mBemcE;
  mBemcZDist = traits.mBemcZDist;
  mBemcPhiDist = traits.mBemcPhiDist;
  mBemcSmdNEta = traits.mBemcSmdNEta;
  mBemcSmdNPhi = traits.mBemcSmdNPhi;

  mBtowId = traits.mBtowId;
  mBtowId23 = traits.mBtowId23;
  mBtowE = traits.mBtowE;
  mBtowE2 = traits.mBtowE2;
  mBtowE3 = traits.mBtowE3;
  mBtowEtaDist = traits.mBtowEtaDist;
  mBtowPhiDist = traits.mBtowPhiDist;
}

//_________________
StPicoBEmcPidTraits::~StPicoBEmcPidTraits() {
  /* empty */
}

//_________________
void StPicoBEmcPidTraits::Print(const Char_t* option) const {
  LOG_INFO << "Matched track index = " << mTrackIndex << endm;
  LOG_INFO << " BEMC Id = " << bemcId() << " BTOW Adc0 = " << bemcAdc0()
	   << " bemc E0 = " << bemcE0() << " e = " << bemcE() << endm;
  LOG_INFO << " BEMC distz = " << bemcZDist() << " distphi = " << bemcPhiDist() << endm;
  LOG_INFO << " BSMD nEta/nPhi = " << bemcSmdNEta() << "/" << bemcSmdNPhi() << endm;
  LOG_INFO << " BTOW Id = " << btowId() << " tower Id 2/3 = " << btowId2() << " " << btowId3() << endm;
  LOG_INFO << " BTOW energy = " << btowE() << " " << btowE2() << " " << btowE3() << endm;
  LOG_INFO << " BTOW position to center = " << btowEtaDist() << " " << btowPhiDist() << endm;
}

//_________________
void StPicoBEmcPidTraits::setEnergy(Float_t energy[5]) {
  auto getConstrainedShort = [](float x) {
    return fabs(x) >= std::numeric_limits<short>::max() ?
    std::numeric_limits<short>::max() : (short)(TMath::Nint(x));
  };
  mBemcE0 = getConstrainedShort(energy[0] * 1000.);
  mBemcE = getConstrainedShort(energy[1] * 1000.);
  mBtowE = getConstrainedShort(energy[2] * 1000.);
  mBtowE2 = getConstrainedShort(energy[3] * 1000.);
  mBtowE3 = getConstrainedShort(energy[4] * 1000.);  
}

//_________________
void StPicoBEmcPidTraits::setDistances(Float_t dist[4]) {
  auto getConstrainedShort = [](float x) {
    return fabs(x) >= std::numeric_limits<short>::max() ?
    std::numeric_limits<short>::max() : (short)(TMath::Nint(x));
  };
  mBemcZDist = getConstrainedShort(dist[0] * 100.);
  mBemcPhiDist = getConstrainedShort(dist[1] * 10000.);
  mBtowEtaDist = getConstrainedShort(dist[2] * 10000.);
  mBtowPhiDist = getConstrainedShort(dist[3] * 10000.);
}

//_________________
void StPicoBEmcPidTraits::setNHits(Int_t nhit[2]) {
  mBemcSmdNEta = ( (nhit[0] > std::numeric_limits<unsigned char>::max()) ?
		   std::numeric_limits<unsigned char>::max() : (UChar_t)(nhit[0]) );
  mBemcSmdNPhi = ( (nhit[1] > std::numeric_limits<unsigned char>::max()) ?
		   std::numeric_limits<unsigned char>::max() : (UChar_t)(nhit[1]) );
}

//_________________
void StPicoBEmcPidTraits::setNTOW(Int_t ntow[3]) {
  mBtowId = (ntow[0] <= 0 || ntow[0] > 4800) ? -1 : (Short_t)ntow[0];
  // Logic: If at least one closest to the mactched tower was
  // found than we check the second one. The 1st and the 2nd
  // digits of mBtowId23 represent Ids of the 1st and the 2nd highest
  // towers that are the closest to the track-matched one, respectively.
  if (ntow[1] < 0 || ntow[1] >= 9) {
    if (!(ntow[2] < 0 || ntow[2] >= 9))  { // If 2 towers were found
      mBtowId23 = (Char_t)(ntow[1] * 10 + ntow[2]);
    }
    else {                                 // If only 1 tower was found
      mBtowId23 = (Char_t)(ntow[1] * 10 + 9); 
    }
  }
  else { // If none of the towers with energy>0.2 GeV were found near the matched tower
    mBtowId23 = 99;
  }  
}
