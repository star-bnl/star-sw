#ifndef __AgMLBsmdVolumeId_h__
#define __AgMLBsmdVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <TVirtualMC.h>
#include <TMath.h>
#include <TLorentzVector.h>
#include <StMessMgr.h>
#include <TGeoManager.h>
#include <TGeoMatrix.h>

/**
 * @class AgMLBsmdVolumeId
 * @brief A volume identifier for the Barrel Shower Maximum Detector (BSMD).
 *
 * This class provides a unique integer identifier for each sensitive volume
 * in the BSMD. The ID is calculated based on the barrel side (East/West),
 * phi bin, eta bin, layer, and strip number, mapping these to a standard STAR
 * numbering scheme.
 */

class AgMLBsmdVolumeId : public AgMLVolumeId {

  const double width_eta1 {1.53668};
  const double width_eta2 {1.96088};
  const double width_phi {1.49348};
  const int max_strip {75};
  const int n_strip_phi {15};

public:
  
  AgMLBsmdVolumeId() : AgMLVolumeId() { };

  virtual int id( int* numbv ) const { 
    

    int rileft    = numbv[0];
    int phi_mod   = numbv[1];
    // Note: in the g2t_volume_id.g layer is numbv[2]
    // but numbv[2] is always 2. I think geant4 treats the mother 
    // volume CSDA as a separate volume, so the layer info is shifted by 1.
    int layer     = numbv[3];
    int phi_encoded {0};
    if (rileft == 1) {
      phi_encoded = 60 - phi_mod + 1;
    } else {
      phi_encoded = phi_mod;
    }

    TLorentzVector _direction(0,0,0,0);
    TVirtualMC::GetMC()->TrackPosition( _direction );

    double xg[4], xl[4];
    _direction.GetXYZT( xg );
    TVirtualMC::GetMC()->Gmtod( xg, xl, 1 );


    int strip     {  0  };
    int eta_bin   {  0  };
    int forw_back {layer};

    if (forw_back == 4) forw_back = 3;

    // the SMD-Eta is treated as one big gas volume (by the geometry definition)
    // so we have to segment it ourself from the local z position of the hit.
    if (forw_back <= 2) { //smde

      double start_z_pos {0.0};
      double width {0.0};
      
      if (forw_back == 1) {
        width = width_eta1;
      } else {
        width = width_eta2;
      }

      // Center alignment
      const double total_width {max_strip * width};
      start_z_pos = -total_width / 2.0;

      // Strip = (LocalZ - Start) / Width
      strip = static_cast<int>(floor( (xl[2] - start_z_pos) / width )) + 1;

      // In East (rileft=2), the module is rotated 180 degrees.
      // Strip 1 is still at Eta=0 (which is now Max Local Z).
      // So we must invert the calculated strip index.
      if (rileft == 2) {
        strip = max_strip - strip + 1;
      }
      //for smde, calculate eta bin from strip
      int global_strip {strip};
      if (forw_back == 2) global_strip += 75; 
      eta_bin = (global_strip - 1) / 15 + 1;
    }
    else { //smdp

      TGeoNavigator* nav = gGeoManager->GetCurrentNavigator();

      if( !nav ) {
        LOG_FATAL << "No Pointer to Navigator" << endm;
        return -1;
      }

      // SAVE CURRENT NAVIGATOR STATE
      nav->PushPath();

      nav->CdUp();//go to csme
      nav->CdUp();//go to csda

      
      double masterPos[3] = {xg[0], xg[1], xg[2]}; //global hit position
      double localPos[3];
      //smdp is rotated 180 in phi 90 theta wrt tower
      //we have to un-rotate to get local coordinates
      nav->MasterToLocal(masterPos, localPos);

      // RESTORE NAVIGATOR STATE
      nav->PopPath();

      double total_width {n_strip_phi * width_phi}; // ~22.4022
      double start_y     {  -total_width / 2.0   };

      strip = static_cast<int>(floor( (localPos[1] - start_y) / width_phi ) ) + 1;      
      eta_bin = static_cast<int>( TMath::Abs(_direction.Eta()) * 10.0) + 1;
    }

    int volume_id = 100000000 * rileft
                  + 1000000   * eta_bin
                  + 1000      * phi_encoded
                  + 100       * forw_back
                  + strip;
    return volume_id;
  };
};

#endif