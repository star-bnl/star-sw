
#include <math.h>

#include "Globals.h"

#include "TVector3.h"

using namespace std;


StEmcGeom *gBTowGeom;
StEmcGeom *mBSmdGeom[mxBSmd];
TVector3   gBCalTowerCoords[mxBtow];               // vs. tower ID
TVector3   gBSmdStripCoords[mxBSmd][mxBStrips];    // vs. strip ID
TVector3   gETowCoords[mxEtowSec *mxEtowSub][mxEtowEta];
int        gMapBTowEtaPhiBin2Id[mxBTetaBin * mxBTphiBin];  // vs. (iEta, iPhi)

EBeamId    aBeams[] = {kBLUE_BEAM, kYELLOW_BEAM};
BeamIdSet  gBeams(aBeams, aBeams+2); 

ESingleSpinState    aSingleSpinStates[] = {kBU_Y0, kBD_Y0, kB0_YU, kB0_YD};
SingleSpinStateSet  gSingleSpinStateSet(aSingleSpinStates, aSingleSpinStates+4); 

EDoubleSpinState    aDoubleSpinStates[] = {kBU_YU, kBU_YD, kBD_YU, kBD_YD};
DoubleSpinStateSet  gDoubleSpinStateSet(aDoubleSpinStates, aDoubleSpinStates+4); 


/**
 * Converts eta-phi real pair to (XXX:ds: preshower???) eta-phi bins.
 * Input eta range is from -0.99 to 0.99 only.
 * Returns true on success, false otherwise.
 */
bool ConvertEtaPhi2Bins(float eta, float phi, int &iEta, int &iPhi)
{
   if (phi < 0) phi += 2*M_PI; // I want phi in [0,2Pi]

   if (fabs(eta) >= 0.99) return false;

   // convention:  iPhi=[0,119], kEta=[1,40]
   int kEta = 1 + (int)((eta + 1.) / 0.05);
   iEta = kEta - 1;

   iPhi  = 24 - (int)( phi/M_PI/2 * 120.);
   iPhi += (iPhi < 0) ? 120 : 0;

   //printf("IJ=%d %d\n",iEta,iPhi);
   if (iEta < 0 || iEta >= mxBTetaBin) return false;
   if (iPhi < 0 || iPhi >= mxBTphiBin) return false;

   return true;
}


/** Converts patch index to eta-phi bins? */
void PatchToEtaPhi(int patch, int *eta, int *phi)
{
   if (patch < 0 || patch > 299) {
      Error("PatchToEtaPhi", "PatchToEtaPhi p=%d, out of range. Eta phi not defined.\n", patch);
      return;
   }

   if (patch < 150) {
      int m = 14 - patch / 10;
      int n = patch % 10;
      *eta = n / 2 + 5;
      *phi = n % 2 + m * 2;
   }
   else {
      int m = 29 - patch / 10;
      int n = patch % 10;
      *eta = 4 - n / 2;
      *phi = 1 - n % 2 + m * 2;
   }
}


string AsString(EBeamId beamId)
{
   switch (beamId) {
   case kBLUE_BEAM:
	   return "BLU";
   case kYELLOW_BEAM:
	   return "YEL";
   default:
      return "UNK";
   }
}


string AsString(EBeamId beamId, ESpinState spinState)
{
   if (beamId == kBLUE_BEAM && spinState == kSPIN_UP)
      return "u0";
   if (beamId == kBLUE_BEAM && spinState == kSPIN_DOWN)
      return "d0";
   if (beamId == kYELLOW_BEAM && spinState == kSPIN_UP)
      return "0u";
   if (beamId == kYELLOW_BEAM && spinState == kSPIN_DOWN)
      return "0d";

   return "UNK";
}


string AsString(ESingleSpinState dss)
{
   switch (dss) {
   case kBU_Y0:
	   return "u0";
   case kBD_Y0:
	   return "d0";
   case kB0_YU:
	   return "0u";
   case kB0_YD:
	   return "0d";
   default:
      return "UNK";
   }
}


string AsString(EDoubleSpinState dss)
{
   switch (dss) {
   case kBU_YU:
	   return "uu";
   case kBU_YD:
	   return "ud";
   case kBD_YU:
	   return "du";
   case kBD_YD:
	   return "dd";
   default:
      return "UNK";
   }
}


ESingleSpinState AsSingleSpinState(EBeamId beamId, ESpinState spinState)
{
   if (beamId == kBLUE_BEAM && spinState == kSPIN_UP)
      return kBU_Y0;
   if (beamId == kBLUE_BEAM && spinState == kSPIN_DOWN)
      return kBD_Y0;
   if (beamId == kYELLOW_BEAM && spinState == kSPIN_UP)
      return kB0_YU;
   if (beamId == kYELLOW_BEAM && spinState == kSPIN_DOWN)
      return kB0_YD;
}
