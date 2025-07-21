#ifndef __AgMLEmcVolumeId_h__
#define __AgMLEmcVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <TGeoNavigator.h>
#include <StMessMgr.h>
#include <TLorentzVector.h>
#include <TVirtualMC.h>
#include <TMath.h>

#include <TString.h>
#include <StMessMgr.h>

/**
 * @class AgMLEmcVolumeId
 * @brief A volume identifier for the Barrel Electromagnetic Calorimeter (BEMC).
 *
 * This class provides a unique integer identifier for each sensitive volume
 * in the BEMC, based on its position in the geometry hierarchy. The ID
 * is calculated from the barrel side (east/west), module number, eta ring,
 * phi sub-module, and superlayer.
 */
class AgMLEmcVolumeId : public AgMLVolumeId {

  const int    numberOfEtaRings = 20;

public:
  
  AgMLEmcVolumeId() : AgMLVolumeId() { /* nada */ };

  virtual int id( int* numbv ) const { 


    int rileft = numbv[0]; // east barrel vs west barrel
    int phi    = numbv[1]; // module
    int superl = numbv[2]; // before / after SMD layer

    TLorentzVector _direction(0,0,0,0);
    TVirtualMC::GetMC()->TrackPosition( _direction );

    double xg[4], xl[4];
    _direction.GetXYZT( xg );
    double pseudoRapidity = TMath::Abs(_direction.Eta());        

    // Obtain local coordinates from global coordinates
    TVirtualMC::GetMC()->Gmtod( xg, xl, 1 );
    
    // Calculate the eta ring and submodule
    int eta_tow = ( pseudoRapidity * double(numberOfEtaRings) ) + 1.0;
    int phi_sub = ( xl[1]>= 0 )? 1 : 0; // (-13,0)=0, (0,13)=1

    //    LOG_INFO << Form("eta=%f xlocal=%f %f %f | phi_sub=%i eta_tow=%i", pseudoRapidity, xl[0], xl[1], xl[2],phi_sub,eta_tow) << endm;
    
    int volumeid = -999;

    if ( rileft==1 ) {
      phi = 60 - phi + 1;
      if ( phi_sub==0 ) {
	phi_sub=2;
      }
    }
    else {
      phi = 60+phi;
      phi_sub = phi_sub + 1;      
    }

    volumeid=10000000*rileft+100000*eta_tow+100*phi+10*phi_sub+superl;

    return volumeid;

  };
};


#endif
