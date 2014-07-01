#include "St_spaceChargeCorC.h"
#include "St_trigDetSumsC.h"
#include "StMemStat.h"

Double_t St_spaceChargeCorC::getSpaceChargeCoulombs(Double_t scaleFactor)
  {
    St_trigDetSumsC* scalers = St_trigDetSumsC::instance();
    if (! scalers ) return 0;
    Double_t zf = zeroField(0); // potential validity margin for scalers
    if (zf>0 && zf<1) scalers->setValidityMargin(zf);
    Double_t coulombs = 0;
//    StMemStat::PrintMem("Space charge before GetDate");
    int idate  = StMaker::GetChain()->GetDate();
//    StMemStat::PrintMem("Space charge AFTER int GetDate() conversion");  

    bool use_powers = idate > 20090101;
//    bool use_powers = (StMaker::GetChain()->GetDate() > 20090101);
//    StMemStat::PrintMem("Space charge AFTER comparision");
    for (int row=0;row< (int) getNumRows();row++) {
      Double_t mult = 0;
      switch ((int) getSpaceChargeDetector(row)) {
        case (0) : mult = scalers->getMult(); break; // vpdx as of 2007-12-19
        case (1) : mult = scalers->getBBCX(); break;
        case (2) : mult = scalers->getZDCX(); break;
        case (3) : mult = scalers->getZDCEast()+scalers->getZDCWest(); break;
        case (4) : mult = scalers->getBBCEast()+scalers->getBBCWest(); break;
        case (5) : mult = scalers->getZDCEast(); break;
        case (6) : mult = scalers->getZDCWest(); break;
        case (7) : mult = scalers->getBBCEast(); break;
        case (8) : mult = scalers->getBBCWest(); break;
        case (9) : mult = scalers->getBBCYellowBkg(); break;
        case (10): mult = scalers->getBBCBlueBkg(); break;
        case (11): mult = scalers->getPVPDEast(); break;
        case (12): mult = scalers->getPVPDWest(); break;
        case (13) : mult = scalers->getCTBOrTOFp(); break; // zdcx-no-killer as of 2011
        case (14) : mult = scalers->getCTBEast(); break; // zdce-no-killer as of 2011
        case (15) : mult = scalers->getCTBWest(); break; // zdcw-no-killer as of 2011

        default  : mult = 0.;
      }
      if (mult < 0) {
        Mark();
        return 0; // Unphysical scaler rates will be uncorrected
      } else UnMark();
      Double_t saturation = getSpaceChargeSatRate(row);
      Double_t correction = getSpaceChargeCorrection(scaleFactor,row);
      Double_t factor     = getSpaceChargeFactor(row);
      Double_t offset     = getSpaceChargeOffset(row);
      Double_t intens = (mult < saturation) ? mult : saturation;
      if (use_powers) coulombs += ::pow(intens-offset,factor) * correction ;
      else coulombs += factor * (intens-offset) * correction ;
    }
    return coulombs;
  }
TString St_spaceChargeCorC::getSpaceChargeString(Double_t scaleFactor)
  {
    TString str,mult;
    int idate  = StMaker::GetChain()->GetDate();
    bool use_powers = idate > 20090101;
    for (int row=0;row< (int) getNumRows();row++) {
      switch ((int) getSpaceChargeDetector(row)) {
        case (0) : mult = "vpdx"; break; // vpdx as of 2007-12-19
        case (1) : mult = "bbcx"; break;
        case (2) : mult = "zdcx"; break;
        case (3) : mult = "(zdce+zdcw)"; break;
        case (4) : mult = "(bbce+bbcw)"; break;
        case (5) : mult = "zdce"; break;
        case (6) : mult = "zdcw"; break;
        case (7) : mult = "bbce"; break;
        case (8) : mult = "bbcw"; break;
        case (9) : mult = "bbcyb"; break;
        case (10): mult = "bbcbb"; break;
        case (11): mult = "vpde"; break;
        case (12): mult = "vpdw"; break;
        case (13): mult = "zdcxnk"; break; // zdcx-no-killer as of 2011
        case (14): mult = "zdcenk"; break; // zdce-no-killer as of 2011
        case (15): mult = "zdcwnk"; break; // zdcw-no-killer as of 2011
        default  : mult = "";
      }
      Double_t correction = getSpaceChargeCorrection(scaleFactor,row);
      Double_t factor     = getSpaceChargeFactor(row);
      Double_t offset     = getSpaceChargeOffset(row);
      if (factor==0 || correction==0 || mult.Length()==0) continue;
      if (str.Length()) str += "+";
      str += Form("(%g)*(",correction);
      Bool_t factorIsInt = (TMath::Abs(factor - ((double) TMath::Nint(factor))) < 1e-6);
      if (factorIsInt && factor>0.5 && factor<1.5) str += mult;
      else {
        if (use_powers) {
          if (factorIsInt && factor>1.5) {
            // use scaler*scaler*... form
            while (factor>1.5) { (str += mult) += "*"; factor--; }
            str += mult;
          } else str += Form("pow(%s,%f)",mult.Data(),factor);
        } else (str += Form("%g)*(",factor)) += mult;
      }
      if (offset!=0) str += Form("-(%g)",offset);
      str += ")";
    }
    return str;
  }
