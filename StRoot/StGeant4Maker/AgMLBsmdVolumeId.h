#ifndef __AgMLBsmdVolumeId_h__
#define __AgMLBsmdVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <TVirtualMC.h>
#include <TVector3.h>
#include <TLorentzVector.h>
#include <TMath.h>
#include <iostream>
#include <iomanip>

class AgMLBsmdVolumeId : public AgMLVolumeId {
public:
    AgMLBsmdVolumeId() : AgMLVolumeId() { }

    virtual int id( int* numbv ) const {
      

      int rl      = numbv[0]; // Side
      int phiMod  = numbv[1]; // Module
      int section = numbv[3]; // Section/Layer
      int strip   = numbv[4]; // Strip

      int type = 1; // 1=BSMDE, 3=BSMDP

      // Deep hierarchy fallback? important?
      if (strip == 0 && numbv[5] > 0) strip = numbv[5];

      int offset = 0;

      TLorentzVector _direction(0,0,0,0);
      TVirtualMC::GetMC()->TrackPosition( _direction );

      double xg[4], xl[4];
      _direction.GetXYZT( xg );
      double pseudoRapidity = TMath::Abs(_direction.Eta());        

      // Obtain local coordinates from global coordinates
      TVirtualMC::GetMC()->Gmtod( xg, xl, 1 );

      if ( rl==1 ) {
        phiMod = 60 - phiMod + 1;
      }
      else {
        phiMod = 60+phiMod;
      }

      // very important 
      // Section 1 = Low Eta, Section 3 = High Eta (+75), Others = Phi
      // check ClabGeo2.xml
      if (section == 1) {type=1; offset = 0;}
      else if (section == 3) {type=1; offset = 75;}
      else {type=3; offset = 0;} // BSMDP


      LOG_INFO  << " rl=" << rl
                << " phiMod=" << phiMod
                << " type=" << type
                << " section=" << section
                << " strip=" << strip
                << " offset=" << offset
                << " xlocal=" << xl[0] << "," << xl[1] << "," << xl[2]
                << " eta=" << pseudoRapidity
                << endm;


      int finalStrip = strip + offset;

      int etaBin = 1;

      if (type == 1) { 
        // BSMDE (Eta)
        etaBin = static_cast<int>((finalStrip - 1) / 7.5) + 1;
      } 
      else { 
        // BSMDP (Phi)
        // have to get it from the position
        // used similar logic in AgMLEmcVolumeId
        // have to check if it is correct but it looks fine
        double x, y, z;
        TVirtualMC::GetMC()->TrackPosition(x, y, z);
        TVector3 pos(x, y, z);
        
        if (pos.Mag() > 0) {
          double etaVal = TMath::Abs(pos.Eta());
          //this is the most important line in phi calculation
          // phi is segmented into 10 bins per 1 unit of eta
          // this meas that each bin is 0.1 in eta
          etaBin = static_cast<int>(etaVal / 0.1) + 1;
        }
      }

      int volumeid =  (rl     * 100000000) + //not the same ocd (check AgMLEsmdVolumeId)
                      (etaBin * 1000000  ) + 
                      (phiMod * 1000     ) + 
                      (type   * 100      ) + 
                      (finalStrip        ) ;
      return volumeid;
    }
};
#endif