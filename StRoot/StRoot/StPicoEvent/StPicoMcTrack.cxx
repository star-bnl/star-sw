//
// StPicoMcTrack holds information about tracks from Monte Carlo (generator + GEANT)
//

// C++ headers
#include <limits>

// ROOT headers
#include "TString.h"

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoMcTrack.h"

ClassImp(StPicoMcTrack)

//________________
StPicoMcTrack::StPicoMcTrack() : TObject(),
  mId(0), mGePid(0), mCharge(0), mPx(0), mPy(0), mPz(0), mE(0),
  mIsFromShower(kFALSE), mIdVtxStart(-1), mIdVtxStop(-1), mIdVtxItrmd(-1) {
  // Default constructor
  for (Int_t i=ktpc; i<ktot; i++) mHits[i] = 0;
}

//________________
StPicoMcTrack::StPicoMcTrack(const StPicoMcTrack& t) : TObject() {
  // Copy constructor
  mId = t.mId;
  mGePid = t.mGePid;
  mCharge = t.mCharge;
  for (Int_t i=ktpc; i<ktot; i++) mHits[i] = t.mHits[i];
  mPx = t.mPx;
  mPy = t.mPy;
  mPz = t.mPz;
  mE = t.mE;
  mIsFromShower = t.mIsFromShower;
  mIdVtxStart = t.mIdVtxStart;
  mIdVtxStop = t.mIdVtxStop;
  mIdVtxItrmd = t.mIdVtxItrmd;
}

//________________
StPicoMcTrack::~StPicoMcTrack() {
  // Destructor
  /* empty */
}

//________________
Int_t StPicoMcTrack::pdgId() const {
  // Return PDG ID
  Int_t id = 0;
  // Not implemented yet
  return id;
}

//________________
void StPicoMcTrack::setId(Int_t id) {
  mId = ( ( id<0 || id>std::numeric_limits<UShort_t>::max() ) ?
          std::numeric_limits<UShort_t>::max() : (UShort_t)id );
}

//________________
Int_t StPicoMcTrack::correctGePid(Int_t gePid) {
  // By pass embedding particle redefinition
  if (gePid ==           99) gePid =         11151;
  if (gePid ==          207) gePid =            41;
  if (gePid ==        40001) gePid =            24;
  if (gePid ==           98) gePid =            18;
  if (gePid ==        40002) gePid =            32;
  if (gePid ==           97) gePid =            26;
  if (gePid ==        40003) gePid =            23;
  if (gePid ==        40004) gePid =            31;
  if (gePid ==        40005) gePid =            22;
  if (gePid ==        40006) gePid =            30;
  if (gePid ==        10150) gePid =           150;
  if (gePid ==        10151) gePid =           151;
  if (gePid ==        11151) gePid =         10151;
  if (gePid ==        10018) gePid =            98;
  if (gePid ==        10026) gePid =            97;
  if (gePid ==        10017) gePid =            17;
  if (gePid ==        10039) gePid =            39;
  if (gePid ==        10040) gePid =            40;
  if (gePid ==           98) gePid =            18;
  if (gePid ==           97) gePid =            26;
  if (gePid < 0 || gePid > 50) {
    //    cout << "Illegal gePid " << gePid << endl;
    gePid = 51;
  }
  return gePid;
}

//________________
const Char_t *StPicoMcTrack::geName() {
  static const Char_t *geNames[52] = {
    //   1       2       3      4         5           6       7        8         9          10
    "",
    "gamma"   ,"e+"   ,"e-"  ,"nu"   ,"mu+"   ,"mu-"   ,"pi0"  ,"pi+"   ,"pi-"      ,"K0L",
    "K+"      ,"K-"   ,"N"   ,"P"    ,"Pbar"  ,"K0S"   ,"eta"  ,"Lambda","Sigma+"   ,"Sigma0",
    "S-"      ,"Xi0"  ,"Xi-" ,"Omega","Nbar"  ,"LamBar","SBar-","SBar0" ,"SBar+"    ,"XiBar0",
    "XiBar+"  ,"OmBar","tau+","tau-" ,"D+"    ,"D-"    ,"D0"   ,"Dbar0" ,"Ds+"      ,"Ds-"   ,
    "LambC+"  ,"W+"   ,"W-"  ,"Z0"   ,"H2"    ,"H3"    ,"alpha","geanti","He3"      ,"Cerenk",
    "??????"};

  static TString name;
  Int_t iGe = correctGePid( geantId() );
  name = geNames[iGe];
  return name.Data();
}

//________________
void StPicoMcTrack::Print(const Char_t* option __attribute__((unused))) const {
  LOG_INFO << "id: " << id() << " GePid: " << geantId() << " charge: " << charge()
           << Form(" vtx start/stop/itrm: %3d/%3d/%3d \n", idVtxStart(), idVtxStop(), idVtxItrmd() )
           << Form(" px/py/pz/E: %4.3f/%4.3f/%4.3f/%4.3f", p().X(), p().Y(), p().Z(), energy() )
           << " isFromShower: " << isFromShower() << "\n" << endm;
}