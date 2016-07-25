// @(#)root/eve:$Id: geom_star.C,v 1.1 2013/08/07 13:08:24 fisyak Exp $
// Author: Matevz Tadel
/*

.L $STAR/StarVMC/Geometry/macros/applyColorScheme.C
*/
#include "Riostream.h"
#include <map>
#include "TString.h"
#include "TObject.h"
#include "TROOT.h"
#include "TInterpreter.h"
#include "TFile.h"
#include "TGeoManager.h"
#include "TGeoNode.h"
#include "TGeoVolume.h"
#include "TEveManager.h"
#include "TEveGeoNode.h"
#include "TEveElement.h"
#include "TEveViewer.h"
#include "TGLViewer.h"
#include "TGLClip.h"
TGLViewer* viewer;

Bool_t nocache = false;
Bool_t viewall = kTRUE;

//________________________________________________________________________________
void applyColorScheme( const Char_t *TOP="HALL",  Int_t debug = 0 )
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
  // HALL
  colors["HALL"]=kGreen;  
  colors["WALL"]=kGreen;
  colors["HELC"]=kRed;
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
	if ( volume && color != 0 )
	  {
	    if (debug)
	    cout << "Volume " << volume->GetName() << "\tcolor = " << color << endl;
	    volume->SetVisibility();
	    volume->SetLineColor( TMath::Abs(color) );

	  }
	
      }
  }
}
//________________________________________________________________________________
void applyTransparency(const Char_t *TOP, Int_t value, Int_t debug = 0)
{


  TGeoIterator next( gGeoManager->FindVolumeFast(TOP) );
  TGeoNode *node = 0;
  
  while ( (node=(TGeoNode*)next()) )
    {
      TGeoVolume *volume = node->GetVolume();
      if ( volume ) {
	if (debug) cout << "Volume " << volume->GetName() << "\ttransperancy " << value << endl;
	volume->SetVisibility();
	volume->SetTransparency(value);
      }
    }
}
#if 1
//________________________________________________________________________________
void addDetectorTab( const Char_t *name, 
		     const Char_t *title, 
		     const Char_t *_top="CAVE",
		     const Int_t   vis = 5 )
{

  TGeoVolume *top = gGeoManager->FindVolumeFast(_top);
  if (!top)
    {
      cout << Form("Top volume %s not found",_top) << endl;
      return;
    }

  TGeoNode       *node = top -> FindNode(name);
  if ( !node )
    {
      cout << Form("Node %s not found",name) << endl;
      return;
    }

  TEveViewer *viewer = gEve -> SpawnNewViewer( title );
  TEveScene  *scene  = gEve -> SpawnNewScene ( title );
  viewer -> AddScene( scene );

  TGLViewer *v3 = viewer->GetGLViewer();
  v3->SetClearColor(33);
  
  TEveGeoTopNode *edon = new TEveGeoTopNode( gGeoManager, node );
  {
    edon -> SetVisLevel( vis );
  }
  
  gEve -> AddGlobalElement( (TEveElement *) edon, (TEveElement *) scene );

}
#endif
//________________________________________________________________________________
void geom_star(const Char_t *vers = "y2013_1x")
{
   TEveManager::Create();
   viewer = gEve->GetDefaultGLViewer(); // Default

   TFile::SetCacheFileDir(".");
   if (! gGeoManager) {
     TFile *f = TFile::Open(Form("%s.root",vers));
     if (!f || !gGeoManager) {
       // Get the default viewe
       //   gGeoManager = gEve->GetGeometry("http://root.cern.ch/files/cms.root");
       TString path("/afs/rhic.bnl.gov/star/packages/.DEV2/.sl53_gcc451/obj/StarDb/AgiGeometry/");
       path += vers; path += ".h";
       gROOT->LoadMacro(path);
       gInterpreter->ProcessLine(TString(vers) + "()");
       if (! gGeoManager) return;
     }
   }
   gGeoManager->DefaultColors();
   
   TEveGeoTopNode* top = new TEveGeoTopNode(gGeoManager,gGeoManager->GetTopNode());
   top->SetVisLevel(3);
   gEve->AddGlobalElement(top);
#if 0

   TEveGeoTopNode* trk = new TEveGeoTopNode(gGeoManager, top->FindNode("TPCE_1"));
   trk->SetVisLevel(6);
   gEve->AddGlobalElement(trk);
   TEveGeoTopNode* calo = new TEveGeoTopNode(gGeoManager, top->FindNode("CALO_1"));
   calo->SetVisLevel(3);
   gEve->AddGlobalElement(calo);

   TEveGeoTopNode* muon = new TEveGeoTopNode(gGeoManager, top->FindNode("MUON_1"));
   muon->SetVisLevel(4);
   gEve->AddGlobalElement(muon);
#else
#if 0
  addDetectorTab( "TPCE_1", "TPC"  );

  addDetectorTab( "CALB_1", "BEMC" );
  {
    addDetectorTab( "CPHI_1", "BEMC module", "CHLV" );
  }
  addDetectorTab( "ECAL_1", "EEMC" );
  addDetectorTab( "FBOX_1", "FPD" );
  addDetectorTab( "FBO1_3", "FMS N" );
  addDetectorTab( "FBO2_4", "FMS S" );
  addDetectorTab( "IDSM_1", "IDSM" );
  {
    addDetectorTab( "FGTM_1", "FGT", "IDSM" );
  }

  addDetectorTab( "BBCM_1", "BBC" );
  addDetectorTab( "MUTD_1", "MTD" );
  addDetectorTab( "BTOF_1", "TOF", "CAVE", 10  );
  {
    addDetectorTab("BTRA_1","TOF tray", "BSEC", 10);
  }

  //  addDetectorTab( "FTPC_1", "FTPC" );
  //  addDetectorTab( "FTPC_1", "FTPC", "SVTT" );
  //  addDetectorTab( "FTRO_1", "FTPC readout" );
  //  addDetectorTab( "SVTT_1", "SVTT" );
  //  addDetectorTab( "SCON_1", "SCON" );
  //  addDetectorTab( "SCON_1", "SCON", "SVTT" );

  //  addDetectorTab( "FSCE_1", "FSCE" );
  //  addDetectorTab( "ETTV_1", "EIDD" );

  addDetectorTab( "PIPE_1", "pipe" );
  addDetectorTab( "MAGP_1", "magnet" );
#endif
#endif
  //  applyTransparency( "HALL", 90 ); // cave and descendants 90% transparent
  applyTransparency( "CAVE", 90 ); // cave and descendants 90% transparent
  applyTransparency( "WALL", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WAL1", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WAL2", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WAL3", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WAL4", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WAL5", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WAL6", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WAL7", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WAL8", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WAL9", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WAL0", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WALa", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WALb", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WALc", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WALd", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WALe", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WALf", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WALg", 40 ); // cave and descendants 40% transparent
  applyTransparency( "WALh", 40 ); // cave and descendants 40% transparent
  applyTransparency( "SHLD", 40 ); // cave and descendants 40% transparent
  applyTransparency( "SHL1", 40 ); // cave and descendants 40% transparent
  applyTransparency( "SHL2", 40 ); // cave and descendants 40% transparent
  applyTransparency( "SHL3", 40 ); // cave and descendants 40% transparent
  applyTransparency( "SHL4", 40 ); // cave and descendants 40% transparent
  applyTransparency( "SHL5", 40 ); // cave and descendants 40% transparent
  applyTransparency( "CRAT",  1 ); // cave and descendants 40% transparent
  applyTransparency( "HELC",  1 ); // cave and descendants 40% transparent
  applyTransparency( "MGWP", 40 ); // cave and descendants 40% transparent
  applyTransparency( "IDSM", 70 ); // cone and descendants 70% transparent
  applyTransparency( "PXMO", 20 ); // pixel mother and descendants ...
  applyColorScheme("HALL");
  gEve->FullRedraw3D(kTRUE);
#if 1
  // EClipType not exported to CINT (see TGLUtil.h):
  // 0 - no clip, 1 - clip plane, 2 - clip box
  //   TGLViewer *v = gEve->GetDefaultGLViewer();
  //  viewer->GetClipSet()->SetClipType(TGLClip::kClipNone);
  //  viewer->GetClipSet()->SetClipType(TGLClip::kClipBox);
  // viewer->GetClipSet()->SetClipType(TGLClip::kClipPlane);
  viewer->ColorSet().Background().SetColor(kMagenta+4);
  //  viewer->ColorSet().Background().SetColor(kYellow);
  viewer->SetGuideState(TGLUtil::kAxesEdge, kTRUE, kFALSE, 0);
  viewer->RefreshPadEditor(viewer);
  
  viewer->CurrentCamera().RotateRad(-1.2, 0.5);
  // viewer->CurrentCamera().RotateRad(-1.2, 0.7);
  //  viewer->CurrentCamera().RotateRad(-1.2, 0.2);
  viewer->DoDraw();
  //  gEve->GetDefaultGLViewer()->SavePicture("geom_lhcb.png")
  viewer->SavePicture("geom_star.png");
#endif
}

