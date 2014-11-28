#include <map>
#include "TString.h"
#include "TObject.h"

#include "TGeoManager.h"
#include "TGeoNode.h"
#include "TGeoVolume.h"

void applyColorScheme( const Char_t *TOP="CAVE" )
{

  std::map< TString, Int_t > colors;
  std::map< TString, Int_t > trans;   // transparency level

  //
  // Inner detector support module
  //
  colors["SUCA"]=kGray+3;
  colors["SUCB"]=kGray+3;
  colors["SUCC"]=kGray+3;
  colors["SUCD"]=kGray+3;
  colors["SUCE"]=kGray+3;  
  colors["SUCF"]=kGray+3;
  colors["SUCG"]=kOrange-1;
  //
  // PIXL Detector
  //
  colors["PLAC"]=kBlue-4;
  colors["PLPS"]=kMagenta-9;
  colors["PLA1"]=kBlue-4;
  colors["PLP1"]=kMagenta-9;
  //
  // FGT
  //
  colors["FGHV"]=kRed-3;
  colors["FGWB"]=kGreen+3;
  colors["FGWE"]=kGreen-7;
  colors["FGWC"]=kBlue+2;
  colors["FGWD"]=kYellow;
  //
  // TPC
  //
  colors["TPAD"]=kBlue -10;
  colors["TPA1"]=kBlue -10;
  colors["TOFC"]=kGray;
  colors["TIFC"]=kGray;
  colors["TWRB"]=kGray+3;  colors["TWR1"]=kGray+3;  colors["TWR2"]=kGray+3;  colors["TWR3"]=kGray+3;
  colors["TBRW"]=kGray+3;
  colors["TRIB"]=kGray+3;  colors["TRI1"]=kGray+3;  colors["TRI2"]=kGray+3;  colors["TRI3"]=kGray+3;  colors["TRI4"]=kGray+3;  colors["TRI5"]=kGray+3;  colors["TRI6"]=kGray+3;  colors["TRI7"]=kGray+3;  colors["TRI8"]=kGray+3;  colors["TRI9"]=kGray+3;
  colors["TWIR"]=kGray+3;  colors["TWI1"]=kGray+3;
  colors["TWBT"]=kGray+3;
  colors["TWMR"]=kGray+3;
  colors["TWRC"]=kGray+3;
  colors["TWRG"]=kGray+3;

  colors["TRDC"]=kGreen+3;
  colors["FEEI"]=kGreen-3;
  colors["FEER"]=kOrange-3;
  colors["FEEP"]=kBlue-10;

  colors["TCAB"]=kMagenta;
  colors["TCA1"]=kMagenta;
  //
  // Magnet
  //
  colors["MBAR"]=kBlue+2;
  colors["MRGV"]=kBlue+2;
  colors["MCSE"]=kBlue+2;
  colors["MCS1"]=kBlue+2;
  colors["MPTV"]=kBlue+2;
  //
  // FMS
  //
  colors["FLGR"]=kBlue-6;  colors["FLG1"]=kBlue-6;
  colors["FLXF"]=kBlue-9;  colors["FLX1"]=kBlue-9;
  colors["FALU"]=kGray+3;
  //
  // BEMC
  //
  colors["CSLG"]=kBlue;
  //
  // BBC
  //
  colors["CLAD"]=kBlue;
  colors["BPOL"]=kGray+2;
  colors["CLA1"]=kBlue;
  colors["BPO1"]=kGray+2;
  //
  // FSCE
  //
  colors["FSCT"]=kRed+2;
  

  //
  // Reset colors for all volumes
  //
  {
    TGeoIterator next( gGeoManager->FindVolumeFast(TOP) );
    TGeoNode *node = 0;
    
    while ( (node=(TGeoNode*)next()) )
      {
	TGeoVolume *volume = node->GetVolume();
	Int_t color = colors[ volume->GetName() ];
	if ( color != 0 )
	  {
	    
	    volume->SetLineColor( TMath::Abs(color) );

	  }
	
      }
  }
}

void applyTransparency(const Char_t *TOP, Int_t value)
{


  TGeoIterator next( gGeoManager->FindVolumeFast(TOP) );
  TGeoNode *node = 0;
  
  while ( (node=(TGeoNode*)next()) )
    {
      TGeoVolume *volume = node->GetVolume();
      if ( volume )
	volume->SetTransparency(value);
      
    }
}

