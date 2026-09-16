#ifndef __AgMLFmsVolumeId_h__
#define __AgMLFmsVolumeId_h__

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <TGeoNavigator.h>
#include <StMessMgr.h>
#include <TLorentzVector.h>
#include <TVirtualMC.h>
#include <TMath.h>

#include <TString.h>
#include <StMessMgr.h>

// Implemented for FpdmGeo4
/**
 * @class AgMLFmsVolumeId
 * @brief A volume identifier for the Forward Meson Spectrometer (FMS).
 *
 * This class calculates a unique integer identifier for each sensitive cell
 * in the FMS, including its preshower and postshower detectors. The ID is
 * constructed based on the detector side (east/west), north/south-top/bottom
 * configuration, and cell number, with complex logic to map geometry numbers
 * to the final channel ID.
 */
class AgMLFmsVolumeId : public AgMLVolumeId {

public:
  
  AgMLFmsVolumeId( const std::string cd="FLGR") : AgMLVolumeId(),cd_(cd) { /* nada */ };

  const std::string cd_;

  virtual int id( int* numbv ) const { 

    int volumeid=0;

    int n1=numbv[0];
    int n2=numbv[1];
    int sl=-999;  
    if (cd_=="FLGR") sl=1;
    if (cd_=="FLXF") sl=2;
    if (cd_=="FPSC") sl=3;
    if (cd_=="FOSC") sl=4;
    assert(sl>0);

    int eastwest=-999;
    int nstb=-999;
    int ch=-999;
    if ( sl<=2 ) {
      eastwest=(n1-1)/2 + 1;
      if(n1==1)          nstb=1; 
      if(n1==2)          nstb=2;
      if(n1==3 && sl==2) nstb=1;
      if(n1==4 && sl==2) nstb=2;
      if(n1==3 && sl==1) nstb=3;
      if(n1==4 && sl==1) nstb=4; 
      assert(nstb>0); //! Wrong nstb in FPD/FMS      
      ch=n2;
      if      ( eastwest==1 ) {
	if(nstb<=2) {
	  if(n2>=11  && n2<=21 )  ch=n2 +  7;
	  if(n2>=22  && n2<=33 )  ch=n2 + 13;
	  if(n2>=34  && n2<=46 )  ch=n2 + 18;
	  if(n2>=47  && n2<=60 )  ch=n2 + 22;
	  if(n2>=61  && n2<=75 )  ch=n2 + 25;
	  if(n2>=76  && n2<=91 )  ch=n2 + 27;
	  if(n2>=92  && n2<=125)  ch=n2 + 28;
	  if(n2>=126 && n2<=269)  ch=n2 + 36 + 8*((n2-126)/9);
	  if(n2>=270 && n2<=319)  ch=n2 +156;
	  if(n2>=320 && n2<=334)  ch=n2 +157;
	  if(n2>=335 && n2<=348)  ch=n2 +159;
	  if(n2>=349 && n2<=361)  ch=n2 +162;
	  if(n2>=362 && n2<=373)  ch=n2 +166;
	  if(n2>=374 && n2<=384)  ch=n2 +171;
	  if(n2>=385 && n2<=394)  ch=n2 +177;
	}
	else {
	  if(n2>= 85 && n2<=154)  ch=n2 +  5 + 5*((n2-85)/7);
	  if(n2>=155 && n2<=238)  ch=n2 + 50 ;
	}
      }
      else if ( eastwest==2 ) {
	if(nstb<=2) {
	  if(n2>=11  && n2<=21 )  ch=n2 +  7;
	  if(n2>=22  && n2<=33 )  ch=n2 + 13;
	  if(n2>=34  && n2<=46 )  ch=n2 + 18;
	  if(n2>=47  && n2<=60 )  ch=n2 + 22;
	  if(n2>=61  && n2<=75 )  ch=n2 + 25;
	  if(n2>=76  && n2<=91 )  ch=n2 + 27;
	  if(n2>=92  && n2<=125)  ch=n2 + 28;
	  if(n2>=126 && n2<=269)  ch=n2 + 36 + 8*((n2-126)/9);
	  if(n2>=270 && n2<=319)  ch=n2 +156;
	  if(n2>=320 && n2<=334)  ch=n2 +157;
	  if(n2>=335 && n2<=348)  ch=n2 +159;
	  if(n2>=349 && n2<=361)  ch=n2 +162;
	  if(n2>=362 && n2<=373)  ch=n2 +166;
	  if(n2>=374 && n2<=384)  ch=n2 +171;
	  if(n2>=385 && n2<=394)  ch=n2 +177;
	}
	else {
	  if(n2>= 85 && n2<=154)  ch=n2 +  5 + 5*((n2-85)/7);
	  if(n2>=155 && n2<=238)  ch=n2 + 50;
	}
      }
      
    }// sl==2
    else if (sl==3){ // FPS (FMS-Preshower)
      // TODO
    }// sl==3
    else if (sl==4){ // FPOST (FMS-Postshower)
      // TODO
    }// sl==4
    
    volumeid = 10000 * eastwest + 1000 * nstb  +  ch;

    return volumeid;

  };



};


#endif
