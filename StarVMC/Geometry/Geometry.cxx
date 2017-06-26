#include "Geometry.h"
#include "TBrowser.h"
#include "TList.h"
#include "TClass.h"
#include "TSystem.h"

#include "TGeoManager.h"
#include "TGeoVolume.h"

#include "geometryStats.hh"

#include <iostream>
//#include <fstream>

//#include "St_geant_Maker/St_geant_Maker.h"
//#include "TGiant3.h"

// ----------------------------------------------------------------------

// ----------------------------------------------------------------------
Geom_t geom;
BbcmGeom_t bbcmGeom;
CalbGeom_t calbGeom;
CaveGeom_t caveGeom;
EcalGeom_t ecalGeom;
FpdmGeom_t fpdmGeom;
FtpcGeom_t ftpcGeom;
MagpGeom_t magpGeom;
MfldGeom_t mfldGeom;
MutdGeom_t mutdGeom;
PhmdGeom_t phmdGeom;
PipeGeom_t pipeGeom;
PixlGeom_t pixlGeom;
SconGeom_t sconGeom;
SisdGeom_t sisdGeom;
SvttGeom_t svttGeom;
BtofGeom_t btofGeom;
TpceGeom_t tpceGeom;
VpddGeom_t vpddGeom;
UpstGeom_t upstGeom;
ZcalGeom_t zcalGeom;
FtroGeom_t ftroGeom;
RichGeom_t richGeom;
FgtdGeom_t fgtdGeom;
IdsmGeom_t idsmGeom;
IstdGeom_t istdGeom;
FsceGeom_t fsceGeom;
EiddGeom_t eiddGeom;
TpcxGeom_t tpcxGeom;

PxstGeom_t pxstGeom;
PsupGeom_t psupGeom;
DtubGeom_t dtubGeom;
HcalGeom_t hcalGeom;

#include "ZcalGeo.h"
#include "VpddGeo.h"
#include "VpddGeo1.h"
#include "VpddGeo2.h"
#include "UpstGeo.h"
#include "ShapGeo.h"
#include "TutrGeo1.h"
#include "TutrGeo2.h"
#include "TutrGeo3.h"
#include "TutrGeo4.h"
#include "TpcxGeo1.h"
#include "TpceGeo1.h"
#include "TpceGeo2.h"
#include "TpceGeo3a.h"
#include "TestGeo1.h"
#include "SvttGeo.h"
#include "SvttGeo1.h"
#include "SvttGeo10.h"
#include "SvttGeo11.h"
#include "SvttGeo2.h"
#include "SvttGeo3.h"
#include "SvttGeo4.h"
#include "SvttGeo5.h"
#include "SvttGeo6.h"
#include "SvttGeo7.h"
#include "SvttGeo9.h"
#include "SupoGeo.h"
#include "SupoGeo1.h"
#include "SisdGeo.h"
#include "SisdGeo1.h"
#include "SisdGeo2.h"
#include "SisdGeo3.h"
#include "SisdGeo4.h"
#include "SisdGeo5.h"
#include "SisdGeo6.h"
#include "SisdGeo7.h"
#include "ShldGeo.h"
#include "SconGeo.h"
#include "QuadGeo.h"
#include "DtubGeo1.h"
#include "HcalGeo1.h"
#include "PixlGeo3.h"
#include "PixlGeo4.h"
#include "PixlGeo5.h"
#include "PxstGeo1.h"
#include "PipeGeo.h"
#include "PipeGeo00.h"
#include "PipeGeo1.h"
#include "PipeGeo2.h"
#include "PhmdGeo.h"
#include "MutdGeo.h"
#include "MutdGeo2.h"
#include "MutdGeo3.h"
#include "MutdGeo4.h"
#include "MagpGeo.h"
#include "IstdGeo0.h"
#include "IdsmGeo1.h"
#include "FtroGeo.h"
#include "FtpcGeo.h"
#include "FtpcGeo1.h"
#include "FsceGeo.h"
#include "FpdmGeo1.h"
#include "FpdmGeo2.h"
#include "FpdmGeo3.h"
#include "FgtdGeo3.h"
#include "FgtdGeoV.h"
#include "EiddGeo.h"
#include "EcalGeo.h"
#include "EcalGeo6.h"
#include "CaveGeo.h"
#include "CaveGeo2.h"
#include "CalbGeo.h"
#include "CalbGeo1.h"
#include "CalbGeo2.h"
#include "BtofGeo1.h"
#include "BtofGeo2.h"
#include "BtofGeo3.h"
#include "BtofGeo4.h"
#include "BtofGeo5.h"
#include "BtofGeo6.h"
#include "BtofGeo7.h"
#include "BtofGeo8.h"
#include "BbcmGeo.h"

FtsdGeom_t ftsdGeom;

#include "StMessMgr.h"

// ----------------------------------------------------------------------
Geometry::Geometry() : AgModule("Geometry","STAR Master Geometry Module")
{

  BbcmInit(); MagpInit();

  CalbInit(); CaveInit(); EcalInit(); FpdmInit(); FtpcInit(); MutdInit(); PipeInit();
  PixlInit(); SconInit(); SisdInit(); SvttInit(); BtofInit(); TpceInit(); VpddInit();
  UpstInit(); ZcalInit(); FtroInit(); RichInit(); PhmdInit(); FgtdInit(); IdsmInit();
  FsceInit(); EiddInit(); TpcxInit(); IstdInit(); PxstInit(); PsupInit(); HcalInit();
  FtsdInit();

  const Char_t *path = ".:StarVMC/Geometry/macros/:$STAR/StarVMC/Geometry/macros/";
  const Char_t *file = gSystem->Which( path, "StarGeometryDb.C", kReadPermission );

  gROOT -> ProcessLine( Form(".x %s",file) );

}

// ----------------------------------------------------------------------
void Geometry::ConstructGeometry( const Char_t *tag )
{

  LOG_INFO << endm;
  Info("Geometry",Form("AgML constructing geometry tag %s",tag));

  // // Check to see if tag looks like a macro name.  If so, execut it.
  // if ( TString(tag).Contains(".") )
  //   {
  //     gROOT -> ProcessLine(Form(".x %s",tag));
  //     return;
  //   }

  // Select y2009a configuration
  geom.Use("select", tag );

  // And apply the flags to each subsystem
#define Initialize(sys){						\
    out += #sys;  out += ": "; out += geom.sys##Flag.Data();  out += "\t"; \
    if (! sys##Geom.Use("select", geom. sys##Flag) ) {	Int_t _save = gErrorIgnoreLevel; gErrorIgnoreLevel=0; \
      Fatal(GetName(),Form("Could not initialize %s with flag %s",#sys, geom.sys##Flag.Data())); \
      gErrorIgnoreLevel=_save;						\
    }									\
  }
  LOG_INFO << endm;
  TString out = "";
  Initialize(cave);  Initialize(magp);  Initialize(pipe);  Initialize(bbcm); Info("Geometry",out); out="";
  Initialize(vpdd);  Initialize(zcal);  Initialize(btof);  Initialize(mutd); Info("Geometry",out); out="";
  Initialize(phmd);  Initialize(svtt);  Initialize(sisd);  Initialize(scon); Info("Geometry",out); out="";
  Initialize(idsm);  Initialize(ftpc);  Initialize(ftro);  Initialize(fgtd); Info("Geometry",out); out="";
  Initialize(calb);  Initialize(ecal);  Initialize(fpdm);  Initialize(upst); Info("Geometry",out); out="";
  Initialize(fsce);  Initialize(eidd);  Initialize(istd);  Initialize(pxst); Info("Geometry",out); out="";
  Initialize(hcal);                                                          Info("Geometry",out); out="";
  LOG_INFO << endm;
#undef Initialize


  // Cave goes first as it must be present to place things into
  geom.success_cave = ConstructCave( geom.caveFlag, geom.caveStat );  
  {
    TGeoVolume *HALL = gGeoManager->FindVolumeFast("HALL");
    gGeoManager->SetTopVolume(HALL);
  }
  
  // The magnet
  geom.success_magp = ConstructMagp( geom.magpFlag, geom.magpStat );

  // Minbias trigger and vertex positioning detectors
  geom.success_bbcm = ConstructBbcm("BBCMon",       geom.bbcmStat );
  geom.success_vpdd = ConstructVpdd( geom.vpddFlag, geom.vpddStat );
  geom.success_zcal = ConstructZcal( geom.zcalFlag, geom.zcalStat );


  // PID detectors
  geom.success_btof = ConstructBtof( geom.btofFlag, geom.btofStat );
  geom.success_mutd = ConstructMutd( geom.mutdFlag, geom.mutdStat );
  geom.success_phmd = ConstructPhmd( geom.phmdFlag, geom.phmdStat );

  // Tracking detectors
  geom.success_tpce = ConstructTpce( geom.tpceFlag, geom.tpceStat );
  geom.success_tpcx = ConstructTpcx( geom.tpcxFlag, geom.tpcxStat );                     assert( geom.tpceFlag != geom.tpcxFlag);

  geom.success_svtt = ConstructSvtt( geom.svttFlag, geom.svttStat ); // SVT must preceede FTPC and SISD

  // Add in the support cone
  geom.success_scon = ConstructScon( geom.sconFlag, geom.sconStat ); // support cone before SVT 
  geom.success_idsm = ConstructIdsm( geom.idsmFlag, geom.idsmStat );

  // Beam pipe is always there, but configuration may change.  And
  // it should follow the support cone.
  geom.success_pipe = ConstructPipe( geom.pipeFlag, geom.pipeStat );

  // SISD must come after the IDSM, as we may place it inside 
  geom.success_sisd = ConstructSisd( geom.sisdFlag, geom.sisdStat ); // 

  geom.success_pixl = ConstructPixl( geom.pixlFlag, geom.pixlStat ); // OLD development pixel detector (must follow idsm, scon)
  geom.success_dtub = ConstructDtub( geom.dtubFlag, geom.dtubStat );
  geom.success_istd = ConstructIstd( geom.istdFlag, geom.istdStat ); // JB : ist 
  geom.success_pxst = ConstructPxst( geom.pxstFlag, geom.pxstStat ); // JB : ist 
  geom.success_psup = ConstructPsup( geom.psupFlag, geom.psupStat );

  geom.success_ftpc = ConstructFtpc( geom.ftpcFlag, geom.ftpcStat );
  geom.success_ftro = ConstructFtro( geom.ftroFlag, geom.ftroStat );
  geom.success_fgtd = ConstructFgtd( geom.fgtdFlag, geom.fgtdStat );
		
  // Calorimetry
  geom.success_calb = ConstructCalb( geom.calbFlag, geom.calbStat );
  geom.success_ecal = ConstructEcal( geom.ecalFlag, geom.ecalStat );
  geom.success_fpdm = ConstructFpdm( geom.fpdmFlag, geom.fpdmStat );

  // eSTAR upgrades
  geom.success_eidd = ConstructEidd( geom.eiddFlag, geom.eiddStat );
  geom.success_fsce = ConstructFsce( geom.fsceFlag, geom.fsceStat );
  geom.success_hcal = ConstructHcal( geom.hcalFlag, geom.hcalStat );
  geom.success_ftsd = ConstructFtsd( geom.ftsdFlag, geom.ftsdStat );

  // Place the upstream areas
  geom.success_upst = ConstructUpst( geom.upstFlag, geom.upstStat );

  if ( geom.closeGeometry )
    {
      gGeoManager->CloseGeometry();
    }



}

// ----------------------------------------------------------------------
void Geometry::StarsimGeometry( const Char_t *tag )
{
#if 1
  // Select specified geometry (for information gathering only)
  geom.Use("select", tag );

  gROOT->ProcessLine( "gROOT->Macro(\"Load.C\");" );
  gROOT->ProcessLine( "Load(\"libSt_g2t, libStarMagField.so, St_geant_Maker\");" );

  TString cmds="";
#endif

#if 0
  // Load in shared libraries needed for geant / starsim
  St_geant_Maker *geant = new St_geant_Maker();
  geant->Init();

  // Configure and build the geometry in starsim
  TGiant3::Geant3()->Kuexec( Form("DETP GEOM %s",tag) );
  TGiant3::Geant3()->Kuexec( Form("GEXEC $STAR_LIB/geometry.so") );
  
  // Export the geometry to rz format
  TGiant3::Geant3()->Kuexec( Form("GRFILE %s.rz",tag) );
  
  // Convert rz file to ROOT/TGeo using g2root
  TGiant3::Geant3()->Kuexec( Form("SHELL g2root %s.rz",tag) );

  // And load the geometry using ROOT
  gROOT->ProcessLine(Form(".x %s.C",tag));
#endif 
}



// ----------------------------------------------------------------------


AgModule *Geometry::CreateModule( const Char_t *module, const Char_t *top )
{

  Info( GetName(), Form("AgML/Geometry creating module %s",module) );
  if ( top )
  Info( GetName(), Form("AgML/Geometry setting top module %s / volume %s", module, top) );

  // Set the current module to this
  _module = this;

  TString name = module;
  TString NAME = name; NAME.ToUpper();

  // Import the module's name space into ROOT/CInt
  TString cmd = "using namespace "+NAME+";";
  gROOT->ProcessLine(cmd);

  // Now create the requested module and return a pointer to it
  TClass *_class = TClass::GetClass(module);

  if (!_class )
    {
      Error(GetName(),Form("-- %s is not available in library --",module));
      return NULL;
    }

 AgModule *_new = (AgModule *)_class -> New();

  _new -> ConstructGeometry();

  if ( top )
    {
      TGeoVolume *volume = gGeoManager -> FindVolumeFast( top );
      gGeoManager->SetTopVolume(volume);
    }

  // Reset the current module to this
  _module = this;

  return _new;

}

void Geometry::GetStatistics( const Char_t *module )
{

  TString modu = module; modu=modu(0,4);
  modu.ToUpper();
  geometryStats( modu );

}

Int_t Geometry::numberOfNodes( const Char_t *vol )
{
  GetStatistics(vol);
  return getNumberOfNodes();
}

Int_t Geometry::numberOfVolumes( const Char_t *vol )
{
  GetStatistics(vol);
  return getNumberOfVolumes();
}

// ----------------------------------------------------------------------
Bool_t Geometry::ConstructEcal( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  // Select the endcap data structure indicated by the flag
  if ( !ecalGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;
    }

  // Prepare the interface to the module
  AgStructure::AgDetpNew( ecalGeom.module , Form("Endcap Calorimeter with config %s",flag) );
  AgStructure::AgDetpAdd( "Emcg_t", "onoff",    (Int_t)ecalGeom.config );
  AgStructure::AgDetpAdd( "Emcg_t", "fillmode", (Int_t)ecalGeom.efill );
  if ( ecalGeom.geometry > 5 )
    {
      AgStructure::AgDetpAdd( "Ecut_t", "absorber",  (Int_t)ecalGeom.emcuts );
      AgStructure::AgDetpAdd( "Ecut_t", "sensitive", (Int_t)ecalGeom.emcuts );
    }

  if ( go )
  if ( !CreateModule( ecalGeom.module ) )
    {
      Warning(GetName(),"Could not create module "+ecalGeom.module);
      return false;
    }

  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructBbcm( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  if ( !bbcmGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;
    }

  if ( go )
    {
      if ( !CreateModule( bbcmGeom.module ) )
	{
	  Warning(GetName(),"Could not create module "+bbcmGeom.module);
	  return false;
	}
    }
  else
    {
      std::cout << "BBCM not built" << std::endl;
    }
  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructPipe( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  if ( !pipeGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;
    }

  AgStructure::AgDetpNew( pipeGeom.module, Form("Beam pipe with config %s",flag) );
  AgStructure::AgDetpAdd( "Pipv_t", "pipeconfig", (Float_t)pipeGeom.config );
  AgStructure::AgDetpAdd( "Pipv_t", "pipeflag",     (Int_t)pipeGeom.flag );

  if ( go )
  if ( !CreateModule( pipeGeom.module ) )
    {
      Warning(GetName(),"Could not create module "+pipeGeom.module);
      return false;
    }
  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructFpdm( const Char_t *flag, Bool_t go )
{ if (!go) return false;
  if ( !fpdmGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;
    }

  AgStructure::AgDetpNew( fpdmGeom.module, Form("FPD/FPD++/FMS with config %s",flag) );
  if ( fpdmGeom.position )
    {
      std::cout << "Configure FMS N/S separation" << std::endl;
      AgStructure::AgDetpAdd( "Fmcg_t", "fmsnorthx", (Float_t)-50.3 );  // need to extend agstructure
      AgStructure::AgDetpAdd( "Fmcg_t", "fmssouthx", (Float_t)+50.3 );  // to permit this
    }


  if ( !CreateModule( fpdmGeom.module ) )
    {
      Warning(GetName(),"Could not create module "+fpdmGeom.module);
      return false;
    }
  return true;
}
// ----------------------------------------------------------------------
//
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructTpce( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  if ( !tpceGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;
    }

  AgStructure::AgDetpNew( tpceGeom.module, Form("Time Projection Chamber with configuration %s",flag));
  if ( tpceGeom.dens > 0 )
    {
      AgStructure::AgDetpAdd( "Tpcg_t", "gascorr", (Float_t)2 );
    }
  if ( tpceGeom.rmax > 0 )
    {
      AgStructure::AgDetpAdd( "Tpcg_t", "rmax", (Float_t)207.77 );
    }
  if ( tpceGeom.subversion > 0 )
    {
      AgStructure::AgDetpAdd( "Tpcc_t", "version", tpceGeom.subversion );
    }

  if ( go )
  if ( !CreateModule( tpceGeom.module ) )
    {
      Warning(GetName(),"Could not create module "+tpceGeom.module);
      return false;
    }
  return true;
}
// ----------------------------------------------------------------------
//
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructTpcx( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  if ( !tpcxGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;
    }

  AgStructure::AgDetpNew( tpcxGeom.module, Form("Time Projection Chamber with configuration %s",flag));
  if ( tpcxGeom.dens > 0 )
    {
      AgStructure::AgDetpAdd( "Tpcg_t", "gascorr", (Float_t)2 );
    }
  if ( tpcxGeom.rmax > 0 )
    {
      AgStructure::AgDetpAdd( "Tpcg_t", "rmax", (Float_t)207.77 );
    }

  if ( go )
  if ( !CreateModule( tpcxGeom.module ) )
    {
      Warning(GetName(),"Could not create module "+tpcxGeom.module);
      return false;
    }
  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructCalb( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  if ( !calbGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;
    }

  AgStructure::AgDetpNew( calbGeom.module, Form("Barrel Calorimeter with configuration %s",flag));
  AgStructure::AgDetpAdd( "Calg_t", "nmodule", calbGeom.nmod );
  AgStructure::AgDetpAdd( "Calg_t", "shift",   calbGeom.shift );
  if ( calbGeom.config >= 2 )
    {
      AgStructure::AgDetpAdd("Ccut_t","absorber",  (Float_t)calbGeom.emcuts);
      AgStructure::AgDetpAdd("Ccut_t","sensitive", (Float_t)calbGeom.emcuts);
    }

  if ( go )
  if ( !CreateModule( calbGeom.module ) )
    {
      Warning(GetName(),"Could not create module "+calbGeom.module);
      return false;
    }
  return true;
}
// -------------------------------------------------------------------
Bool_t Geometry::ConstructBtof( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  if ( !btofGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;
    }

  AgStructure::AgDetpNew( btofGeom.module, Form("Barrel Time of Flight with configuration %s",flag));
  AgStructure::AgDetpAdd( "Btog_t", "choice", (Float_t)btofGeom.config );
  if ( btofGeom.itof > 5 )
    {
      AgStructure::AgDetpAdd("Btog_t","x0",(Float_t)btofGeom.tofX0);
      AgStructure::AgDetpAdd("Btog_t","z0",(Float_t)btofGeom.tofZ0);      
    }

  if ( go )
  if ( !CreateModule( btofGeom.module ) )
    {
      Warning(GetName(),"Could not create module "+btofGeom.module);
      return false;
    }
  return true;
}
// -------------------------------------------------------------------
Bool_t Geometry::ConstructVpdd( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  if ( !vpddGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;
    }

  AgStructure::AgDetpNew( vpddGeom.module, Form("Vertex Position Detector with configuration %s",flag));
  AgStructure::AgDetpAdd( "Vpdv_t", "vpdconfig", (Float_t)vpddGeom.config );

  if ( go )
  if ( !CreateModule( vpddGeom.module ) )
    {
      Warning(GetName(),"Could not create module "+vpddGeom.module);
      return false;
    }
  return true;
}
// -------------------------------------------------------------------
Bool_t Geometry::ConstructPhmd( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  if ( !phmdGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;
    }

  AgStructure::AgDetpNew( phmdGeom.module );
  AgStructure::AgDetpAdd( "Pmvr_t", "config", (Int_t)phmdGeom.config );

  if ( go )
  if ( !CreateModule( phmdGeom.module ) )
    {
      Warning(GetName(),"Could not create module "+phmdGeom.module);
      return false;
    }
  return true;
}
// -------------------------------------------------------------------
Bool_t Geometry::ConstructFtpc( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  if ( !ftpcGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;
    }

  AgStructure::AgDetpNew( ftpcGeom.module, Form("Forward Time Projection Chamber with configuration %s",flag));

  if (go)
    {
      if ( !CreateModule( ftpcGeom.module ) )
	{
	  Warning(GetName(),"Could not create module "+ftpcGeom.module);
	  return false;
	}

      if ( !CreateModule( ftpcGeom.supportModule ) )
	{
	  Warning(GetName(),"Could not create module "+ftpcGeom.supportModule);
	  return false;
	}
    }

  return true;

}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructMutd( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  if ( !mutdGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;
    }
  if (!go) return true;

  AgStructure::AgDetpNew( mutdGeom.module, Form("Muon Tagging Detector with configuration %s",flag));
  if ( mutdGeom.config >= 4 ) 
    AgStructure::AgDetpAdd( "Mtdg_t", "config", (Float_t)mutdGeom.config );

  if ( mutdGeom.config >= 16 ) // corrections to backlegs
    AgStructure::AgDetpAdd( "Mtdg_t", "version", (Float_t)1.1 );

  if ( !CreateModule( mutdGeom.module ) )
    {
      Warning(GetName(),"Could not create module "+mutdGeom.module);
      return false;
    }
  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructEidd( const Char_t *flag, Bool_t go )
{ if (!go) return false;
  //
  if (! eiddGeom.Use("select",flag) )
    {
      Error(GetName(), Form("Cannot locate configuration %s",flag));
      return false;
    }
  if (!CreateModule( eiddGeom.module ) )
    {
      Warning(GetName(),"Could not create module "+eiddGeom.module );
      return false;
    }
  return true;
}
Bool_t Geometry::ConstructFsce( const Char_t *flag, Bool_t go )
{ if (!go) return false;
  //
  if (! fsceGeom.Use("select",flag) )
    {
      Error(GetName(), Form("Cannot locate configuration %s",flag));
      return false;
    }
  if (!CreateModule( fsceGeom.module ) )
    {
      Warning(GetName(),"Could not create module "+fsceGeom.module );
      return false;
    }
  return true;
}

Bool_t Geometry::ConstructHcal( const Char_t *flag, Bool_t go )
{ if (!go) return false;
  //
  if (! hcalGeom.Use("select",flag) )
    {
      Error(GetName(), Form("Cannot locate configuration %s",flag));
      return false;
    }
  if (!CreateModule( hcalGeom.module ) )
    {
      Warning(GetName(),"Could not create module "+hcalGeom.module );
      return false;
    }
  return true;
}

Bool_t Geometry::ConstructFtsd( const Char_t *flag, Bool_t go )
{ if (!go) return false;
  //

  LOG_INFO << "Creating FTSD" << endm;

  if (! ftsdGeom.Use("select",flag) )
    {
      Error(GetName(), Form("Cannot locate configuration %s",flag));
      return false;
    }
  if (!CreateModule( ftsdGeom.module ) )
    {
      Warning(GetName(),"Could not create module "+ftsdGeom.module );
      return false;
    }
  return true;
}

// ----------------------------------------------------------------------
Bool_t Geometry::ConstructFtro( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  if ( !ftroGeom.Use("select",flag) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;
    }

  AgStructure::AgDetpNew( "FtroGeo", Form("Forward TPC Readout configuration"));

  if ( go )
  if ( !CreateModule( "FtroGeo" ) )
    {
      Warning(GetName(),"Could not create module FtroGeo");
      return false;
    }
  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructSvtt( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  if ( !svttGeom.Use("select",flag) ) 
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;
    }

  AgStructure::AgDetpNew( svttGeom.module, Form("Silicon Vertex Detector Configuration"));
  AgStructure::AgDetpAdd("Svtg_t","config",       (float)svttGeom.config );       // SVTT configuration
  AgStructure::AgDetpAdd("Svtg_t","conever",      (float)sconGeom.config );       // Support cone configuration
  std::cout << "=== sconGeom.config = " << sconGeom.config << " ===" << std::endl;
  std::cout << "=== sconGeom.select = " << sconGeom.select.Data() << " ===" << std::endl;
  /** 
   *  Things are tangled up a bit here... sconGeom.config hasn't been resolved
   *  yet.  I have been doing this at the start of each constructor.  Instead,
   *  perhaps I should be applying the geometry flags to the geometry descriptors
   *  before calling the constructors...
   */


  if ( svttGeom.svshconfig>0 ) {
    AgStructure::AgDetpAdd("Svtg_t","supportver", (float)svttGeom.svshconfig );
  }

  /**
   * The SVT has a rather complicated configuration system, given
   * the 11 different versions of the geometry and its tight coupling
   * to the inner recesses of STAR.
   */
  if ( svttGeom.nlayer    < 7 )    AgStructure::AgDetpAdd("Svtg_t", "nlayer",    (int)  svttGeom.nlayer );
  if ( svttGeom.n1stlayer > 1 )    AgStructure::AgDetpAdd("Svtg_t", "nmin",      (float)svttGeom.n1stlayer );
  if ( pipeGeom.config   >= 4 )    AgStructure::AgDetpAdd("Svtg_t", "ifmany",    (float)1 );

  /* !!! SELECTS VERSION 3 -- Need to apply versioning scheme to AgStructure manipulation !!! */
  if ( svttGeom.nwafer    > 0 )    AgStructure::AgDetpAdd("Svtl_t(3)", "nwafer", (float)1 ); 
  assert(svttGeom.nwafer==0 ); 
  /* !!! ================================================================================ !!! */

  if ( svttGeom.waferdim  > 0 )    AgStructure::AgDetpAdd("Swca_t", "waferwid",  (float)svttGeom.waferdim );
  if ( svttGeom.waferdim  > 0 )    AgStructure::AgDetpAdd("Swca_t", "waferlen",  (float)svttGeom.waferdim );
  if ( svttGeom.water    == 0 )    AgStructure::AgDetpAdd("Swam_t", "len",       (float)0. );
  


  if ( go )
  if ( !CreateModule( svttGeom.module ) )
    {
      Warning(GetName(),Form("Could not create module %s",svttGeom.module.Data()) );
      return false;
    }
  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructSisd( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  /* Add SISD select */

  AgStructure::AgDetpNew( sisdGeom.module, Form("Silicon Strip Detector Configuration") );

  /// If the SVT was successfullly built, placement of sisd is w/in SVT.
  /// == (n.b. Svtt ctor must be called before sisd) ==
  if ( geom.success_svtt )
    {
      AgStructure::AgDetpAdd( "Ssdp_t", "placement", 1 );
    }

  Int_t config = sisdGeom.config;
  Int_t level  = 0;
  if ( config > 10 )
    {

      level = config / 10;  
      if ( level <= 5 ) config = config % 10;
      
    }

  AgStructure::AgDetpAdd("Ssdp_t","config",config);
    
  if (go)
  if ( !CreateModule( sisdGeom.module ) )
    {
      Warning(GetName(),Form("Could not create module %s",sisdGeom.module.Data()) );
      return false;
    }
  return true;

}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructMagp( const Char_t *flag, Bool_t go )
{ if (!go) return false;
 
  if ( !magpGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;      
    }

  AgStructure::AgDetpNew( "MagpGeo", Form("Magnet configuration %s",flag));
  AgStructure::AgDetpAdd( "Magg_t", "version", magpGeom.version );

  //  cout << "Magg_t version = " << magpGeom.version << endl;

  if ( go )
  if ( !CreateModule( magpGeom.module  ) )
    {
      Warning(GetName(),"Could not create module MagpGeo");
      return false;
    }
  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructCave( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  if ( !caveGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;      
    }

  AgStructure::AgDetpNew( caveGeom.module, Form("Wide Angle Hall configuration %s",flag));
  AgStructure::AgDetpAdd( "Cvcf_t", "config", int(caveGeom.config) );
  if ( geom.tpcRefSys )
    {
      AgStructure::AgDetpAdd( "Cvcf_t", "tpcrefsys", int(1) );    
    }

  if ( go )
  if ( !CreateModule( caveGeom.module, "HALL"  ) )
    {
      Warning(GetName(),"Could not create module CaveGeo");
      return false;
    }
  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructScon( const Char_t *flag, Bool_t go )
{ if (!go) return false;
  if ( !sconGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;      
    }

  if ( sconGeom.module == "NONE" )
    {
      return true;
    }

  /*
   * Note: This likely fails... Svtg_t is defined in multiple geometries,...
   * and AgDetpAdd _should_ pick up all of them...
   */


  AgStructure::AgDetpNew( "SconGeo", Form("Support Cone Configuration configuration %s",flag));
  AgStructure::AgDetpAdd( "Svtg_t", "conever", (Float_t)sconGeom.config );

  if ( go )
  if ( !CreateModule( sconGeom.module  ) )
    {
      Warning(GetName(),"Could not create module SconGeo");
      return false;
    }
  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructUpst( const Char_t *flag, Bool_t go )
{ if (!go) return false;
  if ( !upstGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;      
    }

  if ( go )
  if ( !CreateModule( upstGeom.module  ) )
    {
      Warning(GetName(),"Could not create module UpstGeo");
      return false;
    }
  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructZcal( const Char_t *flag, Bool_t go )
{ if (!go) return false;
  if ( !zcalGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;      
    }

  if ( go )
  if ( !CreateModule( zcalGeom.module  ) )
    {
      Warning(GetName(),"Could not create module ZcalGeo");
      return false;
    }
  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructRich( const Char_t *flag, Bool_t go )
{ if (!go) return false;
  if ( !richGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;      
    }

  if ( go )
  if ( !CreateModule( richGeom.module  ) )
    {
      Warning(GetName(),"Could not create module RichGeo");
      return false;
    }
  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructFgtd( const Char_t *flag, Bool_t go )
{ if (!go) return false;

  

  if ( !fgtdGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;      
    }

  if ( fgtdGeom.module == "None" ) 
    {
      return false;
    }


  AgStructure::AgDetpNew( fgtdGeom.module, Form("Forward GEM Tracker with configuration %s", flag));

  if ( fgtdGeom.config<40 )
    AgStructure::AgDetpAdd( "Fggg_t", "fgstconfig", (Float_t) (fgtdGeom.config%30) );
  else
    AgStructure::AgDetpAdd( "Fggg_t", "fgstconfig", (Float_t) (fgtdGeom.config)    );

  if ( go )
  if ( !CreateModule( fgtdGeom.module  ) )
    {
      Warning(GetName(),"Could not create module FgtdGeo");
      return false;
    }
  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructIdsm( const Char_t *flag, Bool_t go )
{ if (!go) return false;
  if ( !idsmGeom.Use( "select", flag ) )
    {
      Error(GetName(),Form("Cannot locate configuration %s",flag));
      return false;      
    }

  AgStructure::AgDetpNew( idsmGeom.module, Form("Inner Detector Support Module with configuration %s",flag));
  //  if ( geom.fgtdStat ) AgStructure::AgDetpAdd( "Idsc_t", "version", (Float_t)2.0 );
  //  else                 AgStructure::AgDetpAdd( "Idsc_t", "version", (Float_t)1.0 );
  AgStructure::AgDetpAdd( "Idsc_t", "version", float( idsmGeom.config ) );

  if ( go )
  if ( !CreateModule( idsmGeom.module  ) )
    {
      Warning(GetName(),"Could not create module IdsmGeo");
      return false;
    }
  return true;
}
// ---------------------------------------------------------------------- This is the old development pixel detector
Bool_t Geometry::ConstructPixl( const Char_t *flag, Bool_t go )
{                                                  if (!go) return false;

  if ( !pixlGeom.Use("select",flag) )
    {
      Error( GetName(), Form("Cannot locate configuration %s",flag) );
      return false;
    }

  AgStructure::AgDetpNew(pixlGeom.module, Form("PIXL Configuration %s",flag));
  if(pixlGeom.config>=50 ){
    AgStructure::AgDetpAdd("Pxlw_t","secversion", (Float_t) pixlGeom.secversion);
    AgStructure::AgDetpAdd("Pxlw_t","ladrconfig", pixlGeom.ladrconfig );
  }

  if (go) if ( !CreateModule(pixlGeom.module) ) {
    Warning(GetName(),Form("Could not create module %s",pixlGeom.module.Data()));
    return false;
  }

  return true;

}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructDtub( const Char_t *flag, Bool_t go )
{                                                     if (!go) return false;

  if ( !dtubGeom.Use("select",flag) )
    {
      Error(GetName(), Form("Cannot locate configuration %s",flag) );
      return false;
    }

  AgStructure::AgDetpNew( dtubGeom.module, Form("DTUB Configuration %s",flag) );

  if (go ) if ( !CreateModule(dtubGeom.module) ) {
    Warning(GetName(),Form("Could not create module %s",dtubGeom.module.Data()));
    return false;
  }
  return true;

}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructIstd( const Char_t *flag, Bool_t go )
{                                                  if (!go) return false;

  if ( !istdGeom.Use("select",flag) )
    {
      Error( GetName(), Form("Cannot locate configuration %s",flag) );
      return false;
    }

  AgStructure::AgDetpNew(istdGeom.module, Form("IST Configuration %s",flag));
  /* Not required for PixlGeo4 --> pixlgeo00
     AgStructure::AgDetpAdd("Pxlv_t","ladver",   2.0f );
     AgStructure::AgDetpAdd("Pxlv_t","location", pixlGeom.location );
  */


  if (go) if ( !CreateModule(istdGeom.module) ) {
    Warning(GetName(),Form("Could not create module %s",istdGeom.module.Data()));
    return false;
  }

  return true;

}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructPxst( const Char_t *flag, Bool_t go )
{                                                  if (!go) return false;

  if ( !pxstGeom.Use("select",flag) )
    {
      Error( GetName(), Form("Cannot locate configuration %s",flag) );
      return false;
    }

  AgStructure::AgDetpNew(pxstGeom.module, Form("PXST Configuration %s",flag));
  /* Not required for PixlGeo4 --> pixlgeo00
     AgStructure::AgDetpAdd("Pxlv_t","ladver",   2.0f );
     AgStructure::AgDetpAdd("Pxlv_t","location", pixlGeom.location );
  */


  if (go) if ( !CreateModule(pxstGeom.module) ) {
    Warning(GetName(),Form("Could not create module %s",pxstGeom.module.Data()));
    return false;
  }

  return true;

}
// ----------------------------------------------------------------------
Bool_t Geometry::ConstructPsup( const Char_t *flag, Bool_t go )
{                                                  if (!go) return false;

  if ( !psupGeom.Use("select",flag) )
    {
      Error( GetName(), Form("Cannot locate configuration %s",flag) );
      return false;
    }

  AgStructure::AgDetpNew(psupGeom.module, Form("PSUP Configuration %s",flag));
  /* Not required for PixlGeo4 --> pixlgeo00
     AgStructure::AgDetpAdd("Pxlv_t","ladver",   2.0f );
     AgStructure::AgDetpAdd("Pxlv_t","location", pixlGeom.location );
  */


  if (go) if ( !CreateModule(psupGeom.module) ) {
    Warning(GetName(),Form("Could not create module %s",psupGeom.module.Data()));
    return false;
  }

  return true;

}
// ----------------------------------------------------------------------

// ----------------------------------------------------------------------
Bool_t Geometry::CalbInit()
{
  
  // Barrel is off
  calbGeom.select="CALBof";
  calbGeom.config=-1;
  calbGeom.fill();

  // Full barrel in y2007
  calbGeom.select="CALB00";
  calbGeom.config=0;
  calbGeom.emsedit=1;
  calbGeom.nmod.at(0)=12;  calbGeom.nmod.at(1)=0;
  calbGeom.shift.at(0)=87; calbGeom.shift.at(1)=0;
  calbGeom.fill();

  //
  calbGeom.select="CALBa0";
  calbGeom.nmod.at(0)=24;
  calbGeom.shift.at(0)=21;
  calbGeom.fill();

  //
  calbGeom.select="CALBb0";
  calbGeom.nmod.at(0)=60; calbGeom.shift.at(0)=0;
  calbGeom.nmod.at(1)= 0; calbGeom.shift.at(1)=0;
  calbGeom.fill();

  //
  calbGeom.select="CALBc0";
  calbGeom.nmod.at(0)=60; calbGeom.shift.at(0)=75;
  calbGeom.nmod.at(1)=60; calbGeom.shift.at(1)=0;
  calbGeom.fill();
  
  //
  calbGeom.select="CALBe0";
  calbGeom.nmod.at(0)=60; calbGeom.shift.at(0)=75;
  calbGeom.nmod.at(1)=60; calbGeom.shift.at(1)=105;
  calbGeom.fill();

  //
  calbGeom.select="CALB01";
  calbGeom.config=1;
  calbGeom.module="CalbGeo1";
  calbGeom.fill();

  //
  calbGeom.select="CALB02";
  calbGeom.config=2;
  calbGeom.module="CalbGeo2";
  calbGeom.emcuts=1;
  calbGeom.fill();

  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::CaveInit()
{
  caveGeom.select="CAVEon"; {
    caveGeom.config = 1;caveGeom.module="CaveGeo";
    caveGeom.SetTitle("STAR Wide Angle Hall Mater Configuration: Default Cave");
    caveGeom.fill();
  }
  //replace [exe CAVE03;] with [ "We need an even bigger Cave";   CaveConfig = 3;]
  caveGeom.select="CAVE03"; caveGeom.config=3; caveGeom.module="CaveGeo"; caveGeom.fill();
  //replace [exe CAVE04;] with [ "We need an even bigger Cave";   CaveConfig = 4;]
  caveGeom.select="CAVE04"; caveGeom.config=4; caveGeom.module="CaveGeo"; caveGeom.fill();
  //replace [exe CAVE05;] with [ "How about we just get the dimensions right and be done with it"; CaveConfig=5;]
  caveGeom.select="CAVE05"; caveGeom.config=5; caveGeom.module="CaveGeo2";  caveGeom.fill();
  caveGeom.select="CAVE06"; caveGeom.config=6; caveGeom.module="CaveGeo3";  caveGeom.fill();
  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::EcalInit()
{
  //replace [exe ECALof;] with [;ECAL=off;]
  ecalGeom.select="ECALof"; ecalGeom.config=-1; ecalGeom.fill();

  //replace [exe ECAL31;] with [;"ECAL31"; ECAL=on;
  //                             ecalFill=3; "all sectors filled "
  //                             EcalConfig=1; " one ECAL patch, west ";
  //                             EcalGeometry=5; "old version of the geometry file";
  //                            ]
  ecalGeom.select="ECAL31"; ecalGeom.efill=3; ecalGeom.config=1; ecalGeom.geometry=5; ecalGeom.fill();

  //replace [exe ECAL11;] with [;"ECAL11"; ECAL=on;
  //                             ecalFill=1;
  //                             EcalConfig=1;
  //                             EcalGeometry=5; "old version of the geometry file";
  ecalGeom.select="ECAL11"; ecalGeom.efill=1; ecalGeom.fill();

  //replace [exe ECAL31;] with [;"ECAL31";
  //                             ECAL=on;
  //                             ecalFill=3;
  //                             EcalConfig=1;   " one ECAL patch, west ";
  //                             EcalGeometry=5; "old version of the geometry file";
  //                            ]
  ecalGeom.select="ECAL31"; ecalGeom.efill=3; ecalGeom.config=1; ecalGeom.fill();


  //replace [exe ECAL33;] with [;"ECAL33"; ECAL=on;
  //                             ecalFill=3 "all sectors filled "; EcalConfig=3; "both wheels"
  //                             EcalGeometry=5; "old version of the geometry file";
  //                            ;]
  ecalGeom.select="ECAL33"; ecalGeom.efill=3; ecalGeom.config=3; ecalGeom.fill();

  //replace [exe ECALv6;] with[;"ECAL version 6.1 (or higher)"
  //                           ;ECAL=on;
  //                           ;EcalFill=3;     "all sectors filled";
  //                           ;EcalConfig=1;   "EEMC on west poletip only";
  //                           ;EcalGeometry=6; "Version 6.1 and higher";
  //                           ]
  ecalGeom.select="ECALv6"; 
  {
    ecalGeom.efill=3; 
    ecalGeom.config=1; 
    ecalGeom.geometry=6; 
    ecalGeom.module="EcalGeo6"; 
    ecalGeom.fill();
  }
  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::FpdmInit()
{

  fpdmGeom.position = 0; // closed
  fpdmGeom.select="FPDMof"; fpdmGeom.config=0; fpdmGeom.module="None";     fpdmGeom.fill();
  fpdmGeom.select="FPDM00"; fpdmGeom.config=0; fpdmGeom.module="FpdmGeo";  fpdmGeom.fill();
  fpdmGeom.select="FPDM01"; fpdmGeom.config=1; fpdmGeom.module="FpdmGeo1"; fpdmGeom.fill();
  fpdmGeom.select="FPDM02"; fpdmGeom.config=2; fpdmGeom.module="FpdmGeo2"; fpdmGeom.fill();
  fpdmGeom.select="FPDM03"; fpdmGeom.config=3; fpdmGeom.module="FpdmGeo3"; fpdmGeom.fill();
  fpdmGeom.select="FPDM04"; fpdmGeom.config=4; fpdmGeom.module="FpdmGeo4"; fpdmGeom.fill();

  fpdmGeom.position = 1; // open
  fpdmGeom.select="FPDM13"; fpdmGeom.config=3; fpdmGeom.module="FpdmGeo3"; fpdmGeom.fill();  



  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::FsceInit()
{
  fsceGeom.SetTitle("Forward Something Calorimeter");
  fsceGeom.select="FSCEof"; fsceGeom.config=0; fsceGeom.module="NONE";    fsceGeom.fill();
  fsceGeom.select="FSCEv0"; fsceGeom.config=1; fsceGeom.module="FsceGeo"; fsceGeom.fill();
  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::HcalInit()
{
  hcalGeom.SetTitle("HCAL Prototype Calorimeter");
  hcalGeom.select="HCALof"; hcalGeom.config=0; hcalGeom.module="NONE";     hcalGeom.fill();
  hcalGeom.select="HCALv0"; hcalGeom.config=0; hcalGeom.module="HcalGeo";  hcalGeom.fill();
  hcalGeom.select="HCALv1"; hcalGeom.config=1; hcalGeom.module="HcalGeo1"; hcalGeom.fill();
  hcalGeom.select="HCALvF"; hcalGeom.config=1; hcalGeom.module="HcalGeoF"; hcalGeom.fill(); // FermiTBF
  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::FtsdInit()
{
  ftsdGeom.SetTitle("FTSD Prototype Tracker");
  ftsdGeom.select="FTSDof"; ftsdGeom.module="NONE";      ftsdGeom.fill();
  ftsdGeom.select="FTSDv0"; ftsdGeom.module="FtsdGeo";   ftsdGeom.fill();
  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::EiddInit()
{
  eiddGeom.select="EIDDof"; eiddGeom.config=0; eiddGeom.module="NONE"   ; eiddGeom.fill();
  eiddGeom.select="EIDDv0"; eiddGeom.config=1; eiddGeom.module="EiddGeo"; eiddGeom.fill();
  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::FtpcInit()
{
  //replace [exe FTPCof;] with ["ftpc configuration"; FTPC=off;]
  ftpcGeom.select="FTPCof";
  {
    ftpcGeom.config=-1; 
    ftpcGeom.fill();
  }
  //replace [exe FTPCG00;] with ["ftpc configuration"; FTPC=on;
  //                            ;FtpcConfig = 0;"FTPC Support";SupoConfig = 1;]
  ftpcGeom.select="FTPC00"; 
  { 
    ftpcGeom.config=0; 
    ftpcGeom.module="FtpcGeo"; 
    ftpcGeom.supportModule="SupoGeo"; 
    ftpcGeom.supo = 1;
    ftpcGeom.fill();
  }
  //replace [exe FTPC01;] with ["ftpc configuration"; FTPC=on;
  //                            ;FtpcConfig = 1;"FTPC Support";SupoConfig = 1;]
  ftpcGeom.select="FTPC01"; 
  {
    ftpcGeom.config=1; 
    ftpcGeom.module="FtpcGeo1"; 
    ftpcGeom.supportModule="SupoGeo1"; 
    ftpcGeom.supo = 1; 
    ftpcGeom.fill();
  }
  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::MfldInit()
{
  //replace [exe MFLDof;] with [ MFLD=off;]
  mfldGeom.select="MFLDof"; mfldGeom.config=-1; mfldGeom.fill();
  //replace [exe MFLD23;] with [ MFLD=on; magField = 2.5; MfldConfig=3;]
  mfldGeom.select="MFLD23"; mfldGeom.config=3; mfldGeom.field=2.5; mfldGeom.fill();
  //replace [exe MFLD53;] with [ MFLD=on; magField = 5.0; MfldConfig=3;]
  mfldGeom.select="MFLD53"; mfldGeom.config=3; mfldGeom.field=5.0; mfldGeom.fill();
  //replace [exe MFLD54;] with [ MFLD=on; magField = 5.0; MfldConfig=4;]
  mfldGeom.select="MFLD54"; mfldGeom.config=4; mfldGeom.field=5.0; mfldGeom.fill();
  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::MutdInit()
{

  mutdGeom.select="MUTDof";    mutdGeom.module = "None";    mutdGeom.config=0;      mutdGeom.fill();
  mutdGeom.select="MUTD01";    mutdGeom.module="MutdGeo";   mutdGeom.config=1;      mutdGeom.fill();
  mutdGeom.select="MUTD02";    mutdGeom.module="MutdGeo2";  mutdGeom.config=2;      mutdGeom.fill();
  mutdGeom.select="MUTD03";    mutdGeom.module="MutdGeo3";  mutdGeom.config=3;      mutdGeom.fill();
  mutdGeom.select="MUTD04";    mutdGeom.module="MutdGeo4";  mutdGeom.config=4;      mutdGeom.fill();
  mutdGeom.select="MUTD05";    mutdGeom.module="MutdGeo4";  mutdGeom.config=5;      mutdGeom.fill();   
  mutdGeom.select="MUTD12";    mutdGeom.module="MutdGeo4";  mutdGeom.config=12;     mutdGeom.fill();
  mutdGeom.select="MUTD13";    mutdGeom.module="MutdGeo4";  mutdGeom.config=13;     mutdGeom.fill();
  mutdGeom.select="MUTD14";    mutdGeom.module="MutdGeo5";  mutdGeom.config=14;     mutdGeom.fill();
  mutdGeom.select="MUTD15";    mutdGeom.module="MutdGeo7";  mutdGeom.config=15;     mutdGeom.fill();
  mutdGeom.select="MUTD16";    mutdGeom.module="MutdGeo7";  mutdGeom.config=16;     mutdGeom.fill();

  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::PhmdInit()
{
  //replace [exe PHMDof;] with ["Photon Multiplicity Detector Version ";PHMD=off; PhmdConfig = 0;]
  phmdGeom.select="PHMDof"; {
    phmdGeom.config=-1;
    phmdGeom.module="PhmdGeo"; 
    phmdGeom.SetTitle("Photon Multiplicity Detector -disabled-");
    phmdGeom.fill();
  }

  //replace [exe PHMD01;] with ["Photon Multiplicity Detector Version ";PHMD=on;  PhmdConfig = 1;]
  phmdGeom.select="PHMD01";{
    phmdGeom.config=1;
    phmdGeom.module="PhmdGeo"; 
    phmdGeom.SetTitle("Photon Multiplicity Detector Version 1");
    phmdGeom.fill();
  }

  //replace [exe PHMD02;] with ["Photon Multiplicity Detector Version ";PHMD=on;  PhmdConfig = 2;]
  phmdGeom.select="PHMD02";{
    phmdGeom.config=2;
    phmdGeom.module="PhmdGeo"; 
    phmdGeom.SetTitle("Photon Multiplicity Detector Version 2");
    phmdGeom.fill();
  }

  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::PipeInit() // Does this break the config=-1 scheme?
{

  pipeGeom.select="PIPEon"; {
    pipeGeom.config=2; 
    pipeGeom.flag=1; 
    pipeGeom.module="PipeGeo"; 
    pipeGeom.SetTitle("Pipe Geometry Default Configuration (PIPE12)");
    pipeGeom.fill();
  }

  //replace [exe PIPE00;] with [ "Simplest.Gerrit"; PipeConfig = -1;PipeFlag   = -1;]
  pipeGeom.select="PIPE00"; {
    pipeGeom.config=-1; 
    pipeGeom.flag=-1;
    pipeGeom.module="PipeGeo00"; 
    pipeGeom.SetTitle("Simplest");
    pipeGeom.fill();
  }

  //replace [exe PIPE12;] with [ "Default pipe"; PipeConfig = 2 ; PipeFlag     = 1;]
  pipeGeom.select="PIPE12"; {
    pipeGeom.config=2; 
    pipeGeom.flag=1; 
    pipeGeom.module="PipeGeo"; 
    pipeGeom.SetTitle("Default Pipe");
    pipeGeom.fill();
  }

  pipeGeom.select="PIPE04"; {
    pipeGeom.config=4; 
    pipeGeom.flag=0; 
    pipeGeom.module="PipeGeo";
    pipeGeom.fill();
  }

  pipeGeom.select="PIPE14"; {
    pipeGeom.config=4; 
    pipeGeom.flag=1;
    pipeGeom.module="PipeGeo";
    pipeGeom.fill();
  }

  // Pipe ala UPGR16 geometry
  pipeGeom.select="PIPE06"; {
    pipeGeom.config=6; 
    pipeGeom.flag=0; 
    pipeGeom.module="PipeGeo";
    pipeGeom.fill();
  }

  // New beam pipe
  pipeGeom.select="PIPEv1"; {
    pipeGeom.config=0;
    pipeGeom.flag=0;
    pipeGeom.module="PipeGeo1";
    pipeGeom.fill();
  }

  // Run 13 beam pipe
  pipeGeom.select="PIPEv2"; {
    pipeGeom.config=0;
    pipeGeom.flag  =0;
    pipeGeom.module="PipeGeo2";
    pipeGeom.fill();
  }

  pipeGeom.select="PIPEv3"; {
    pipeGeom.config=30;
    pipeGeom.flag  =1;
    pipeGeom.module="PipeGeo3";
    pipeGeom.fill();
  }
    
    

  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::PixlInit() // Probably breaks config=-1 scheme
{
  pixlGeom.module="PixlGeo3";
  pixlGeom.select="PIXL00"; pixlGeom.config=-1; pixlGeom.fill(); 
  pixlGeom.select="PIXL01"; pixlGeom.config=1; pixlGeom.fill();

  pixlGeom.module="PixlGeo4";
  pixlGeom.select="PIXL02"; pixlGeom.config=1; pixlGeom.location=2.0; pixlGeom.fill();

  pixlGeom.module="PixlGeo5";
  pixlGeom.select="PIXL05"; pixlGeom.config=50; pixlGeom.ladrconfig=1; pixlGeom.secversion=7; pixlGeom.fill();  

  pixlGeom.module="PixlGeo6";
  pixlGeom.select="PIXL06"; pixlGeom.config=60; pixlGeom.ladrconfig=1; pixlGeom.secversion=1; pixlGeom.fill();

  pixlGeom.module="PixlGeo6";
  pixlGeom.select="PIXL62"; pixlGeom.config=62; pixlGeom.ladrconfig=2; pixlGeom.secversion=1; pixlGeom.fill();


  dtubGeom.module="DtubGeo1";    
  dtubGeom.select="DTUB01"; dtubGeom.config=1; dtubGeom.location=2.0; dtubGeom.fill();
  
  
  return true;
}
// ----------------------------------------------------------------------------
Bool_t Geometry::IstdInit() // Probably breaks config=-1 scheme
{
  istdGeom.select="ISTDof";  istdGeom.config=0;  istdGeom.module="NONE"    ;  istdGeom.fill(); 
  istdGeom.select="ISTD01";  istdGeom.config=1;  istdGeom.module="IstdGeo0";  istdGeom.fill(); 
  istdGeom.select="ISTD02";  istdGeom.config=2;  istdGeom.module="IstdGeo1";  istdGeom.fill(); 
  istdGeom.select="ISTD03";  istdGeom.config=3;  istdGeom.module="IstdGeo2";  istdGeom.fill(); 
  return true;
}
// ----------------------------------------------------------------------------
Bool_t Geometry::PxstInit() // Probably breaks config=-1 scheme
{
  pxstGeom.module="NONE"    ;  pxstGeom.select="PXSTof"; pxstGeom.config=-0; pxstGeom.fill(); 
  pxstGeom.module="PxstGeo1";  pxstGeom.select="PXST01"; pxstGeom.config=-1; pxstGeom.fill(); 
  return true;
}
Bool_t Geometry::PsupInit() // Probably breaks config=-1 scheme
{
  psupGeom.module="NONE"    ;  psupGeom.select="PSUPof"; psupGeom.config=-0; psupGeom.fill(); 
  psupGeom.module="PsupGeo";  psupGeom.select="PSUP01"; psupGeom.config=-1; psupGeom.fill(); 
  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::SconInit()
{
  sconGeom.SetTitle("Support Cone Master Geometry");
  sconGeom.select="SCONof"; sconGeom.module="NONE";    sconGeom.config=0; sconGeom.fill();
  sconGeom.select="SCON02"; sconGeom.module="NONE";    sconGeom.config=2; sconGeom.fill();
  sconGeom.select="SCON12"; sconGeom.module="SconGeo"; sconGeom.config=2; sconGeom.fill();
  sconGeom.select="SCON13"; sconGeom.module="SconGeo"; sconGeom.config=3; sconGeom.fill();
  sconGeom.select="SCON14"; sconGeom.module="SconGeo"; sconGeom.config=4; sconGeom.fill();
  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::SisdInit()
{
  //replace [exe SISDof;] with ["Silicon Strip Detector off "; SISD=off;]
  sisdGeom.select="SISDof"; {
    sisdGeom.module="Meh";
    sisdGeom.fill();
  }

  sisdGeom.select="SISD02"; sisdGeom.config= 2; sisdGeom.module="SisdGeo";  sisdGeom.fill();
  sisdGeom.select="SISD12"; sisdGeom.config=12; sisdGeom.module="SisdGeo2"; sisdGeom.fill();
  sisdGeom.select="SISD22"; sisdGeom.config=22; sisdGeom.module="SisdGeo2"; sisdGeom.fill();
  sisdGeom.select="SISD23"; sisdGeom.config=23; sisdGeom.module="SisdGeo3"; sisdGeom.fill();
  sisdGeom.select="SISD24"; sisdGeom.config=24; sisdGeom.module="SisdGeo4"; sisdGeom.fill();
  sisdGeom.select="SISD35"; sisdGeom.config=35; sisdGeom.module="SisdGeo6"; sisdGeom.fill();
  sisdGeom.select="SISD55"; sisdGeom.config=55; sisdGeom.module="SisdGeo6"; sisdGeom.fill();
  sisdGeom.select="SISD65"; sisdGeom.config=65; sisdGeom.module="SisdGeo6"; sisdGeom.fill();
  sisdGeom.select="SISD75"; sisdGeom.config=75; sisdGeom.module="SisdGeo6"; sisdGeom.fill();
  sisdGeom.select="SISD85"; sisdGeom.config=85; sisdGeom.module="SisdGeo7"; sisdGeom.fill();

  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::SvttInit()
{

  svttGeom.select="SVTTof"; {
    svttGeom.config=-1; 
    svttGeom.fill();
  }

  /////////////////////////////////////////////////////////////////////////////////////////
  //
  // Uses SvttGeo.xml
  //
  svttGeom.module="SvttGeo";    

  svttGeom.select="SVTT00"; {
    svttGeom.config=0; 
    svttGeom.water=0; 
    svttGeom.nwafer=7; 
    svttGeom.nlayer=6; 
    svttGeom.fill();
  }

  svttGeom.select="SVTT100"; {
    svttGeom.config=0; 
    svttGeom.water=1; 
    svttGeom.nwafer=0; 
    svttGeom.nlayer=3; 
    svttGeom.waferdim=0; 
    svttGeom.fill();
  }

  /////////////////////////////////////////////////////////////////////////////////////////
  //
  // Uses SvttGeo1.xml
  //
  svttGeom.module = "SvttGeo1";

  svttGeom.select="SVTT101"; {
    svttGeom.config=1; 
    svttGeom.water=1; 
    svttGeom.nlayer=6; 
    svttGeom.fill();  
  }


  /////////////////////////////////////////////////////////////////////////////////////////
  //
  // Uses SvttGeo2.xml
  //
  svttGeom.module = "SvttGeo2";

  svttGeom.select="SVTT102"; 
  {
    svttGeom.config=2; 
    svttGeom.water=1; 
    svttGeom.nwafer=0; 
    svttGeom.nlayer=6; 
    svttGeom.waferdim=0; 
    svttGeom.svshconfig=0;
    svttGeom.fill();  
  }

  /////////////////////////////////////////////////////////////////////////////////////////
  //
  // Uses SvttGeo3.xml
  //
  svttGeom.module = "SvttGeo3";

  svttGeom.select="SVT103"; 
  {
    svttGeom.config=3; 
    svttGeom.water=1; 
    svttGeom.nwafer=0; 
    svttGeom.nlayer=6; 
    svttGeom.waferdim=0; 
    svttGeom.svshconfig=0;
    svttGeom.fill();      
  }

  svttGeom.select="SVT203"; 
  {
    svttGeom.config=3;
    svttGeom.water=1; 
    svttGeom.nwafer=0; 
    svttGeom.nlayer=6; 
    svttGeom.waferdim=0; 
    svttGeom.svshconfig=2;
    svttGeom.fill();          
  }


  /////////////////////////////////////////////////////////////////////////////////////////
  //
  // Uses SvttGeo6.xml
  //
  svttGeom.module = "SvttGeo6";

  svttGeom.select="SVT106"; 
  {
    svttGeom.config=6;
    svttGeom.water=1; 
    svttGeom.nwafer=0; 
    svttGeom.nlayer=6; 
    svttGeom.waferdim=0; 
    svttGeom.svshconfig=0;
    svttGeom.fill();          
  }

  /////////////////////////////////////////////////////////////////////////////////////////
  //
  // Uses SvttGeo4.xml
  //
  svttGeom.module="SvttGeo4";

  svttGeom.select="SVT204"; 
  {
    svttGeom.config=4;
    svttGeom.water=1; 
    svttGeom.nwafer=0; 
    svttGeom.nlayer=6; 
    svttGeom.waferdim=0; 
    svttGeom.svshconfig=2;
    svttGeom.fill();          
  }

  svttGeom.select="SVT304"; 
  {
    svttGeom.config=4;
    svttGeom.water=1; 
    svttGeom.nwafer=0; 
    svttGeom.nlayer=6; 
    svttGeom.waferdim=0; 
    svttGeom.svshconfig=3;
    svttGeom.fill();          
  }


  /////////////////////////////////////////////////////////////////////////////////////////
  //
  // Uses SvttGeo6.xml
  //
  svttGeom.module="SvttGeo6";

  svttGeom.select="SVT206"; 
  {
    svttGeom.config=6;
    svttGeom.water=1; 
    svttGeom.nwafer=0; 
    svttGeom.nlayer=6; 
    svttGeom.waferdim=0; 
    svttGeom.svshconfig=2;
    svttGeom.fill();          
  }

  svttGeom.select="SVT306"; 
  {
    svttGeom.config=6;
    svttGeom.water=1; 
    svttGeom.nwafer=0; 
    svttGeom.nlayer=6; 
    svttGeom.waferdim=0; 
    svttGeom.svshconfig=3;
    svttGeom.fill();          
  }


  svttGeom.select="SVT306x";
  {
    svttGeom.config=6;
    svttGeom.water=1; 
    svttGeom.nwafer=0; 
    svttGeom.nlayer=7; 
    svttGeom.waferdim=0; 
    svttGeom.svshconfig=3;
    svttGeom.fill();          
  }

  /////////////////////////////////////////////////////////////////////////////////////////
  //
  // Uses SvttGeo10.xml
  //
  svttGeom.module="SvttGeo10";

  svttGeom.select="SVT310x";
  {
    svttGeom.config=10;
    svttGeom.water=1; 
    svttGeom.nwafer=0; 
    svttGeom.nlayer=7; 
    svttGeom.waferdim=0; 
    svttGeom.svshconfig=3;
    svttGeom.fill();          
  }


  /////////////////////////////////////////////////////////////////////////////////////////
  //
  // Uses SvttGeo11.xml
  //
  svttGeom.module="SvttGeo11";

  svttGeom.select="SVT211";
  {
    svttGeom.config=11;
    svttGeom.water=1; 
    svttGeom.nwafer=0; 
    svttGeom.nlayer=6; 
    svttGeom.waferdim=0; 
    svttGeom.svshconfig=2;
    svttGeom.fill();          
  }

  svttGeom.select="SVT311";
  {
    svttGeom.config=11;
    svttGeom.water=1; 
    svttGeom.nwafer=0; 
    svttGeom.nlayer=6; 
    svttGeom.waferdim=0; 
    svttGeom.svshconfig=3;
    svttGeom.fill();          
  }

  svttGeom.select="SVT312";
  {
    svttGeom.config=12;
    svttGeom.water=1; 
    svttGeom.nwafer=0; 
    svttGeom.nlayer=6; 
    svttGeom.waferdim=0; 
    svttGeom.svshconfig=3;
    svttGeom.fill();          
  }

  svttGeom.select="SVT312x";
  {
    svttGeom.config     = 12;
    svttGeom.water      = 1; 
    svttGeom.nwafer     = 0; 
    svttGeom.nlayer     = 7; 
    svttGeom.waferdim   = 0; 
    svttGeom.svshconfig = 3;
    svttGeom.fill();          
  }

  return true;

}

Bool_t Geometry::BtofInit()
{
  //replace [exe BTOF42;] with [;BTOF=on; BtofConfig= 4;Itof=2 " call btofgeo2 ";]
  btofGeom.select="BTOF42"; btofGeom.config=4; btofGeom.itof=2; btofGeom.module="BtofGeo2"; btofGeom.fill();
  //replace [exe BTOF52;] with [;BTOF=on; BtofConfig= 5;Itof=2 " call btofgeo2 ";]
  btofGeom.select="BTOF52"; btofGeom.config=5; btofGeom.itof=2; btofGeom.module="BtofGeo2"; btofGeom.fill();
  //replace [exe BTOF72;] with [;BTOF=on; BtofConfig= 7;Itof=2 " call btofgeo2 ";]
  btofGeom.select="BTOF72"; btofGeom.config=7; btofGeom.itof=2; btofGeom.module="BtofGeo2"; btofGeom.fill();

  //replace [exe BTOF84;] with [;BTOF=on; BtofConfig= 8;Itof=4 " call btofgeo4 ";]
  btofGeom.select="BTOF84"; btofGeom.config=8; btofGeom.itof=4; btofGeom.module="BtofGeo4"; btofGeom.fill();

  //replace [exe BTOFa5;] with [;BTOF=on; BtofConfig=10;Itof=5 " call btofgeo5 ";]
  btofGeom.select="BTOFa5"; btofGeom.config=10; btofGeom.itof=5; btofGeom.module="BtofGeo5"; btofGeom.fill();
  //replace [exe BTOF16;] with [;" X.Dong";BTOF=on;
  //                            BtofConfig=1; Itof=6 " call btofgeo6 ";
  //                            tofX0= 0.00; tofZ0=-0.50;]
  btofGeom.select="BTOF16";
  {
    btofGeom.config=1; 
    btofGeom.itof=6; 
    btofGeom.module="BtofGeo6"; 
    btofGeom.tofX0=0.0;
    btofGeom.tofZ0=-0.50;
    btofGeom.fill();
  }
  //replace [exe BTOF66;] with [;" X.Dong";BTOF=on;
  //                            BtofConfig=6; Itof=6 " call btofgeo6 ";
  //                            tofX0= 0.00; tofZ0=0;]
  //
  btofGeom.select="BTOF66";
  {
    btofGeom.config=6; 
    btofGeom.itof=6; 
    btofGeom.module="BtofGeo6"; 
    btofGeom.tofX0=0.0;
    btofGeom.tofZ0=0.0;
    btofGeom.fill();
  }
  //replace [exe BTOF67;] with [;"F.Geurts fixes to sensitive volumes";
  //                            BtofConfig=6; Itof=7 "call btofgeo7";
  //                            tofX0=0.00; tofZ0=0.00;]
  //
  btofGeom.select="BTOF67";
  {
    btofGeom.config=6; 
    btofGeom.itof=7; 
    btofGeom.module="BtofGeo7"; 
    btofGeom.tofX0=0.0;
    btofGeom.tofZ0=0.0;
    btofGeom.fill();
  }

  //replace [exe BTOFb6;] with [;" X.Dong";BTOF=on;
  //                            BtofConfig=11; Itof=6 " call btofgeo6 ";
  //                            tofX0= 0.00; tofZ0=-0.50;]
  //
  btofGeom.select="BTOFb6";
  {
    btofGeom.config=11; 
    btofGeom.itof=6; 
    btofGeom.module="BtofGeo6"; 
    btofGeom.tofX0=0.0;
    btofGeom.tofZ0=-0.50;
    btofGeom.fill();
  }
  //replace [exe BTOFb7;] with [;" X.Dong";BTOF=on;
  //                            BtofConfig=11; Itof=7 " call btofgeo7 ";
  //                            tofX0= 0.00; tofZ0=-0.50;]
  //
  btofGeom.select="BTOFb7";
  {
    btofGeom.config=11; 
    btofGeom.itof=7; 
    btofGeom.module="BtofGeo7"; 
    btofGeom.tofX0=0.0;
    btofGeom.tofZ0=-0.50;
    btofGeom.fill();
  }
  //replace [exe BTOFc6;] with [;" F.Geurts";BTOF=on; BtofConfig=12; Itof=6 " call btofgeo6 ";]
  btofGeom.select="BTOFc6";
  {
    btofGeom.config=12; 
    btofGeom.itof=6; 
    btofGeom.module="BtofGeo6"; 
    btofGeom.fill();
  }
  //replace [exe BTOFc7;] with [;" F.Geurts";BTOF=on; BtofConfig=12; Itof=7 " call btofgeo7 ";]
  btofGeom.select="BTOFc7";
  {
    btofGeom.config=12; 
    btofGeom.itof=7; 
    btofGeom.module="BtofGeo7"; 
    btofGeom.fill();
  }


  // -------------------------------------------------------------------------
  btofGeom.select="BTOFv8";
  {
    btofGeom.config = 13;        // 
    btofGeom.itof   = 8;         // BtofGeo8
    btofGeom.module = "BtofGeo8"; 
    btofGeom.tofX0=0.0;
    btofGeom.tofZ0=0.0;
    btofGeom.fill();
  }



  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::TpcxInit()
{

  tpcxGeom.select = "TPCXof"; {
    tpcxGeom.config = -1;
    tpcxGeom.padconfig = -1;
    tpcxGeom.module="none";
    tpcxGeom.fill();
  }
  
  tpcxGeom.select = "TPCX10"; {
    tpcxGeom.config    = 1;
    tpcxGeom.padconfig = 0;
    tpcxGeom.module = "TpcxGeo1";
    tpcxGeom.fill();
  };

  tpcxGeom.select="TPCX16"; {
    tpcxGeom.config = 1;
    tpcxGeom.padconfig=6;
    tpcxGeom.module="TpcxGeo2";
    tpcxGeom.fill();
  };


  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::TpceInit()
{
  //replace [exe TPCE00;] with [;"New version of the TPC backplane "; TpceConfig = 1;]
  tpceGeom.select="TPCE00";
  {
    tpceGeom.config = 1;
    tpceGeom.module="TpceGeo";
    tpceGeom.fill();
  }
  //replace [exe TPCE01;] with [;"New version of the TPC backplane "; TpceConfig = 1;
  //                             "gas density correction";            DensConfig = 1;]
  tpceGeom.select="TPCE01";
  {
    tpceGeom.config = 1;
    tpceGeom.dens  = 1;
    tpceGeom.module="TpceGeo";
    tpceGeom.fill();
  }
  //replace [exe TPCE02;] with [;"New version of the TPC backplane "; TpceConfig = 2;
  //                             "gas density correction";            DensConfig = 1;]
  tpceGeom.select="TPCE02";
  {
    tpceGeom.config = 2;
    tpceGeom.dens  = 1;
    tpceGeom.module="TpceGeo1";
    tpceGeom.fill();
  }
  //replace [exe TPCE03;] with [;"New version of the TPC backplane "; TpceConfig = 3;
  //                             "gas density correction";            DensConfig = 1;]
  tpceGeom.select="TPCE03";
  {
    tpceGeom.config = 3;
    tpceGeom.dens  = 1;
    tpceGeom.module="TpceGeo2";
    tpceGeom.fill();
  }
  //replace [exe TPCE04;] with [;"New version of the TPC backplane "; TpceConfig = 4;
  //                             "gas density correction";            DensConfig = 1;]
  tpceGeom.select="TPCE04";
  {
    tpceGeom.config = 4;
    tpceGeom.dens  = 1;
    tpceGeom.module="TpceGeo3a";
    tpceGeom.fill();
  }
  //replace [exe TPCE04r;] with [;"New version of the TPC backplane "; TpceConfig = 4;
  //                              "gas density correction";            DensConfig = 1;
  //                              "radius correction";                 RmaxConfig = 1;
  tpceGeom.select="TPCE04r";
  {
    tpceGeom.config = 4;
    tpceGeom.dens  = 1;
    tpceGeom.rmax   = 1;
    tpceGeom.subversion = 3.0;
    tpceGeom.module="TpceGeo3a";
    tpceGeom.fill();
  }


  tpceGeom.select="TPCE31";
  {
    tpceGeom.config = 4;
    tpceGeom.dens   = 1;
    tpceGeom.rmax   = 1;
    tpceGeom.subversion = 3.1;
    tpceGeom.module="TpceGeo3a";
    tpceGeom.fill();
  }
  tpceGeom.select="TPCE41";
  {
    tpceGeom.config = 4;
    tpceGeom.dens   = 1;
    tpceGeom.rmax   = 1;
    tpceGeom.subversion = 4.1;
    tpceGeom.module="TpceGeo4a";
    tpceGeom.fill();
  }
  tpceGeom.select="TPCE51";
  {
    tpceGeom.config = 4;
    tpceGeom.dens   = 1;
    tpceGeom.rmax   = 1;
    tpceGeom.subversion = 5.1;
    tpceGeom.module="TpceGeo5a";
    tpceGeom.fill();
  }
  tpceGeom.select="TPCE40"; { tpceGeom.subversion=3.1; tpceGeom.module="TpceGeo4"; tpceGeom.fill(); }

  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::VpddInit()
{
  //replace [exe VPDDof;] with [;VPDD=off;]
  vpddGeom.select="VPDDof";
  {
    vpddGeom.module="none";
    vpddGeom.fill();
  }
  //replace [exe VPDD02;] with  [;"pseudo Vertex Position Detector";VPDD=on;VpddConfig=2;]
  vpddGeom.select="VPDD02";
  {
    vpddGeom.module="VpddGeo";
    vpddGeom.config=2;
    vpddGeom.fill();
  }
  //replace [exe VPDD03;] with  [;"pseudo Vertex Position Detector";VPDD=on;VpddConfig=3;]
  vpddGeom.select="VPDD03";
  {
    vpddGeom.module="VpddGeo";
    vpddGeom.config=3;
    vpddGeom.fill();
  }  
  //replace [exe VPDD04;] with  [;"pseudo Vertex Position Detector";VPDD=on;VpddConfig=4;]
  vpddGeom.select="VPDD04";
  {
    vpddGeom.module="VpddGeo";
    vpddGeom.config=4;
    vpddGeom.fill();
  }
  //replace [exe VPDD07;] with  [;"pseudo Vertex Position Detector";VPDD=on;VpddConfig=7;]
  vpddGeom.select="VPDD07";
  {
    vpddGeom.module="VpddGeo2";
    vpddGeom.config=7;
    vpddGeom.fill();
  }

  vpddGeom.select="VPDD08"; 
  vpddGeom.module="VpddGeo3";
  vpddGeom.config=7;
  vpddGeom.fill();

  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::BbcmInit()
{

  bbcmGeom.select="BBCMon"; bbcmGeom.module="BbcmGeo"; bbcmGeom.fill();
  bbcmGeom.select="BBCMof"; bbcmGeom.module="none"; bbcmGeom.fill();
  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::MagpInit()
{
  magpGeom.select="MAGPon"; magpGeom.module="MagpGeo"; magpGeom.fill();
  magpGeom.select="MAGPof"; magpGeom.module="None";    magpGeom.fill();
  magpGeom.select="MAGPv1"; magpGeom.module="MagpGeo"; magpGeom.version=2.0; magpGeom.fill();
  return true;
};
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::UpstInit()
{
  upstGeom.select="UPSTon"; upstGeom.module="UpstGeo"; upstGeom.fill();
  upstGeom.select="UPSTof"; upstGeom.module="None"; upstGeom.fill();
  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::ZcalInit()
{
  zcalGeom.select="ZCALon"; zcalGeom.module="ZcalGeo"; zcalGeom.fill();
  zcalGeom.select="ZCALof"; zcalGeom.module="None"; zcalGeom.fill();
  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::RichInit()
{
  richGeom.select="RICHon"; richGeom.module="RichGeo"; richGeom.fill();
  richGeom.select="RICHof"; richGeom.module="None"; richGeom.fill();
  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::FgtdInit()
{

  // No FGT
  fgtdGeom.select="FGTDof";   fgtdGeom.module="None";     fgtdGeom.config=0;    fgtdGeom.fill();
  // Turn on the legacy FGT
  fgtdGeom.select="FGTDon";   fgtdGeom.module="FgtdGeo2"; fgtdGeom.config=1;    fgtdGeom.fill();
  // Production FGT
  // 31 -- y2012 config w/ one full disk and 5 dual quadrant disks
  fgtdGeom.select="FGTD31";   fgtdGeom.module="FgtdGeo3"; fgtdGeom.config=31;   fgtdGeom.fill();
  // 32 -- anticipated full config y2013 and beyond
  fgtdGeom.select="FGTD32";   fgtdGeom.module="FgtdGeo3"; fgtdGeom.config=32;   fgtdGeom.fill();


  // vf -- very forward FGT
  fgtdGeom.select="FGTDvf";    fgtdGeom.module="FgtdGeoV"; fgtdGeom.config=55;   fgtdGeom.fill();
  fgtdGeom.select="FGTDv55";   fgtdGeom.module="FgtdGeoV"; fgtdGeom.config=55;   fgtdGeom.fill();
  fgtdGeom.select="FGTDv56";   fgtdGeom.module="FgtdGeoV"; fgtdGeom.config=56;   fgtdGeom.fill();


  /*
    fgtdGeom.select="FGTD32";   fgtdGeom.module="FgtdGeo3"; fgtdGeom.config=32;   fgtdGeom.fill();
    fgtdGeom.select="FGTD33";   fgtdGeom.module="FgtdGeo3"; fgtdGeom.config=33;   fgtdGeom.fill();
    fgtdGeom.select="FGTD34";   fgtdGeom.module="FgtdGeo3"; fgtdGeom.config=34;   fgtdGeom.fill();
  */
  return true;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Bool_t Geometry::IdsmInit()
{
  idsmGeom.select="IDSMof"; idsmGeom.module="None"    ; idsmGeom.config=0; idsmGeom.fill();
  idsmGeom.select="IDSMon"; idsmGeom.module="IdsmGeo1"; idsmGeom.config=1; idsmGeom.fill();
  idsmGeom.select="IDSM01"; idsmGeom.module="IdsmGeo1"; idsmGeom.config=1; idsmGeom.fill();
  idsmGeom.select="IDSM02"; idsmGeom.module="IdsmGeo2"; idsmGeom.config=2; idsmGeom.fill();
  idsmGeom.select="IDSM14"; idsmGeom.module="IdsmGeo2"; idsmGeom.config=14;idsmGeom.fill();
  return true;
}
// ----------------------------------------------------------------------
Bool_t Geometry::FtroInit()
{
  ftroGeom.select="FTROon"; ftroGeom.module="FtroGeo"; ftroGeom.config=1; ftroGeom.fill();
  ftroGeom.select="FTROof"; ftroGeom.module="None";    ftroGeom.config=0; ftroGeom.fill();
  ftroGeom.select="FTRO01"; ftroGeom.module="FtroGeo"; ftroGeom.config=1; ftroGeom.fill();
  return true;
}
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------
Bool_t Geometry::GeomInit()
{
  assert(0); // deprecated.  will remove when nobody complains about this...

  // Common to most geometry definitions
  geom.magpFlag = "MAGPon"; geom.magpStat=1;
  geom.zcalFlag = "ZCALon"; geom.zcalStat=1;
  geom.bbcmFlag = "BBCMon"; geom.bbcmStat=1;

  //                                                           y2008  tag
  geom.select="y2008"; geom.SetTitle("STAR y2008 Geometry");
  {

    geom.sconFlag = "SCON02"; geom.sconStat=1; // 1
    geom.tpceFlag = "TPCE03"; geom.tpceStat=1; // 2
    geom.btofFlag = "BTOFb6"; geom.btofStat=1; // 3
    geom.calbFlag = "CALB02"; geom.calbStat=1; // 4
    geom.ecalFlag = "ECAL31"; geom.ecalStat=1; // 5
    geom.fpdmFlag = "FPDM03"; geom.fpdmStat=1; // 6
    geom.vpddFlag = "VPDD07"; geom.vpddStat=1; // 7
    geom.ftpcFlag = "FTPC01"; geom.ftpcStat=1; // 8
    geom.ftroFlag = "FTRO01"; geom.ftroStat=1; // 9
    geom.mutdFlag = "MUTD03"; geom.mutdStat=1; // 10    
    geom.pipeFlag = "PIPE12"; geom.pipeStat=1; // 11
    geom.phmdFlag = "PHMDof"; geom.phmdStat=0; // 12
    geom.caveFlag = "CAVE04"; geom.caveStat=1; // 

    geom.fill();
  } 

  //                                                           y2009a tag
  geom.select="y2009a"; geom.SetTitle("STAR y2009a Geometry");
  {

    geom.ecalFlag = "ECALv6"; geom.ecalStat=1; // 1
    geom.pipeFlag = "PIPE12"; geom.pipeStat=1; // 2
    geom.fpdmFlag = "FPDM03"; geom.fpdmStat=1; // 3
    geom.tpceFlag = "TPCE04"; geom.tpceStat=1; // 4
    geom.calbFlag = "CALB02"; geom.calbStat=1; // 5
    geom.btofFlag = "BTOFc6"; geom.btofStat=1; // 6
    geom.vpddFlag = "VPDD07"; geom.vpddStat=1; // 7
    geom.ftpcFlag = "FTPC01"; geom.ftpcStat=1; // 8
    geom.ftroFlag = "FTRO01"; geom.ftroStat=1; // 9
    geom.mutdFlag = "MUTD03"; geom.mutdStat=1; // 10
    geom.sconFlag = "SCON13"; geom.sconStat=1; // 11
    geom.caveFlag = "CAVE04"; geom.caveStat=1; // 12
    geom.upstFlag = "UPSTon"; geom.upstStat=1;

    geom.phmdFlag = "PHMDof"; geom.phmdStat=0;

    geom.fill(); 
  }

  //                                                             test tag
  geom.select="test"; geom.SetTitle("STAR Test Geometry");
  {
    geom.ecalFlag = "ECAL31"; geom.ecalStat=0;
    geom.pipeFlag = "PIPE12"; geom.pipeStat=0;
    geom.fpdmFlag = "FPDM03"; geom.fpdmStat=0;
    geom.tpceFlag = "TPCE03"; geom.tpceStat=1;
    geom.calbFlag = "CALB02"; geom.calbStat=0;
    geom.btofFlag = "BTOFc6"; geom.btofStat=0;
    geom.vpddFlag = "VPDD07"; geom.vpddStat=0;
    geom.ftpcFlag = "FTPC01"; geom.ftpcStat=0;
    geom.ftroFlag = "FTRO01"; geom.ftroStat=0;
    geom.mutdFlag = "MUTD03"; geom.mutdStat=0;
    geom.caveFlag = "CAVE04"; geom.caveStat=1;
    geom.fill(); 
  }

  return true;
}
// ----------------------------------------------------------------------

void printModule( const Char_t *system, const Char_t *flag )
{

  assert(0); // deprecated

}



#define FORM(x)  Form("#color[%i]{", geom.success_##x + 2) << Form("+ %4s: %10s", #x, x##Flag.Data())
#define ENDL     "}" << std::endl;

void Geom_t::Print( const Option_t *opts ) const
{

  const Char_t *onoff[]={"disabled"," enabled"};
  const Char_t *emcuts[]={"old EM","low EM","mid EM","high EM"};

  std::cout << "============================================================================== " << std::endl;
  std::cout << GetName() << " " << select << " : " << GetTitle() << std::endl;
  std::cout << std::endl;

  std::cout << "Tracking Detectors:" << std::endl;
  std::cout << "-------------------" << std::endl;
  std::cout << FORM(tpce); printModule("TpceGeom_t",tpceFlag); std::cout << " " << onoff[tpceStat] << ENDL;
  std::cout << FORM(ftpc); printModule("FtpcGeom_t",ftpcFlag); std::cout << " " << onoff[ftpcStat] << ENDL;
  std::cout << FORM(ftro); printModule("FtroGeom_t",ftroFlag); std::cout << " " << onoff[ftroStat] << ENDL;
  std::cout << FORM(svtt); printModule("SvttGeom_t",svttFlag); std::cout << " " << onoff[svttStat] << ENDL;
  std::cout << FORM(sisd); printModule("SisdGeom_t",sisdFlag); std::cout << " " << onoff[sisdStat] << ENDL;
  std::cout << std::endl;

  std::cout << "Particle ID Detectors:" << std::endl;
  std::cout << "----------------------" << std::endl;
  std::cout << FORM(btof); printModule("BtofGeom_t",btofFlag); std::cout << " " << onoff[btofStat] << ENDL;
  std::cout << FORM(phmd); printModule("PhmdGeom_t",phmdFlag); std::cout << " " << onoff[phmdStat] << ENDL;
  std::cout << FORM(mutd); printModule("MutdGeom_t",mutdFlag); std::cout << " " << onoff[mutdStat] << ENDL;
  std::cout << FORM(rich); printModule("RichGeom_t",richFlag); std::cout << " " << onoff[richStat] << ENDL;
  std::cout << std::endl;

  std::cout << "Calorimetry:" << std::endl;
  std::cout << "------------" << std::endl;
  std::cout << FORM(calb); printModule("CalbGeom_t",calbFlag); std::cout << " " << onoff[calbStat] << Form(" %s",emcuts[calbCuts]) << ENDL; 
  std::cout << FORM(ecal); printModule("EcalGeom_t",ecalFlag); std::cout << " " << onoff[ecalStat] << Form(" %s",emcuts[ecalCuts]) << ENDL; 
  std::cout << FORM(fpdm); printModule("FpdmGeom_t",fpdmFlag); std::cout << " " << onoff[fpdmStat] << ENDL;
  std::cout << std::endl;
  
  std::cout << "Vertexing, Triggering, Polarimetry:" << std::endl;
  std::cout << "----------------------------------" << std::endl;
  std::cout << FORM(bbcm); printModule("BbcmGeom_t",bbcmFlag); std::cout << " " << onoff[bbcmStat] << ENDL; 
  std::cout << FORM(vpdd); printModule("VpddGeom_t",vpddFlag); std::cout << " " << onoff[vpddStat] << ENDL; 
  std::cout << FORM(zcal); printModule("ZcalGeom_t",zcalFlag); std::cout << " " << onoff[zcalStat] << ENDL; 
  std::cout << std::endl;

  std::cout << "Building, Supports, Magnets, etc...:" << std::endl;
  std::cout << "------------------------------------" << std::endl;
  std::cout << FORM(cave); printModule("CaveGeom_t",caveFlag); std::cout << " " << onoff[caveStat] << ENDL; 
  std::cout << FORM(pipe); printModule("PipeGeom_t",pipeFlag); std::cout << " " << onoff[pipeStat] << ENDL; 
  std::cout << FORM(magp); printModule("MagpGeom_t",magpFlag); std::cout << " " << onoff[magpStat] << ENDL; 
  std::cout << FORM(mfld); printModule("MfldGeom_t",mfldFlag); std::cout << " " << onoff[mfldStat] << ENDL; 
  std::cout << FORM(scon); printModule("SconGeom_t",sconFlag); std::cout << " " << onoff[sconStat] << ENDL; 
//std::cout << FORM("+ SUPO: %10s ",supoFlag.Data()); printModule("SupoGeom_t",supoFlag); std::cout << " " << onoff[supoStat] << ENDL; 
  std::cout << FORM(upst); printModule("UpstGeom_t",upstFlag); std::cout << " " << onoff[upstStat] << ENDL; 
  std::cout << std::endl;
  
}
#undef FORM
#undef ENDL


#if 0

void Geom_t::Print( const Option_t *opts ) const
{

  const Char_t *onoff[]={"disabled"," enabled"};
  const Char_t *emcuts[]={"old EM","low EM","mid EM","high EM"};

  std::cout << "============================================================================== " << std::endl;
  std::cout << GetName() << " " << select << " : " << GetTitle() << std::endl;
  std::cout << std::endl;

  std::cout << "Tracking Detectors:" << std::endl;
  std::cout << "-------------------" << std::endl;
  std::cout << Form("+ TPC:  %10s ",tpceFlag.Data()); printModule("TpceGeom_t",tpceFlag); std::cout << " " << onoff[tpceStat] << std::endl;;
  std::cout << Form("+ FTPC: %10s ",ftpcFlag.Data()); printModule("FtpcGeom_t",ftpcFlag); std::cout << " " << onoff[ftpcStat] << std::endl;;
  std::cout << Form("+ FTRO: %10s ",ftroFlag.Data()); printModule("FtroGeom_t",ftroFlag); std::cout << " " << onoff[ftroStat] << std::endl;;
  std::cout << Form("+ SVTT: %10s ",svttFlag.Data()); printModule("SvttGeom_t",svttFlag); std::cout << " " << onoff[svttStat] << std::endl;;
  std::cout << Form("+ SISD: %10s ",sisdFlag.Data()); printModule("SisdGeom_t",sisdFlag); std::cout << " " << onoff[sisdStat] << std::endl;;
  std::cout << std::endl;

  std::cout << "Particle ID Detectors:" << std::endl;
  std::cout << "----------------------" << std::endl;
  std::cout << Form("+ BTOF: %10s ",btofFlag.Data()); printModule("BtofGeom_t",btofFlag); std::cout << " " << onoff[btofStat] << std::endl;;
  std::cout << Form("+ PHMD: %10s ",phmdFlag.Data()); printModule("PhmdGeom_t",phmdFlag); std::cout << " " << onoff[phmdStat] << std::endl;;
  std::cout << Form("+ MUTD: %10s ",mutdFlag.Data()); printModule("MutdGeom_t",mutdFlag); std::cout << " " << onoff[mutdStat] << std::endl;;
  std::cout << Form("+ RICH: %10s ",richFlag.Data()); printModule("RichGeom_t",richFlag); std::cout << " " << onoff[richStat] << std::endl;;
  std::cout << std::endl;

  std::cout << "Calorimetry:" << std::endl;
  std::cout << "------------" << std::endl;
  std::cout << Form("+ BEMC: %10s ",calbFlag.Data()); printModule("CalbGeom_t",calbFlag); std::cout << " " << onoff[calbStat] << Form(" %s",emcuts[calbCuts]) << std::endl;; 
  std::cout << Form("+ EEMC: %10s ",ecalFlag.Data()); printModule("EcalGeom_t",ecalFlag); std::cout << " " << onoff[ecalStat] << Form(" %s",emcuts[ecalCuts]) << std::endl;; 
  std::cout << Form("+ FPDM: %10s ",fpdmFlag.Data()); printModule("FpdmGeom_t",fpdmFlag); std::cout << " " << onoff[fpdmStat] << std::endl;;
  std::cout << std::endl;
  
  std::cout << "Vertexing, Triggering, Polarimetry:" << std::endl;
  std::cout << "----------------------------------" << std::endl;
  std::cout << Form("+ BBCM: %10s ",bbcmFlag.Data()); printModule("BbcmGeom_t",bbcmFlag); std::cout << " " << onoff[bbcmStat] << std::endl;; 
  std::cout << Form("+ VPDD: %10s ",vpddFlag.Data()); printModule("VpddGeom_t",vpddFlag); std::cout << " " << onoff[vpddStat] << std::endl;; 
  std::cout << Form("+ ZCAL: %10s ",zcalFlag.Data()); printModule("ZcalGeom_t",zcalFlag); std::cout << " " << onoff[zcalStat] << std::endl;; 
  std::cout << std::endl;

  std::cout << "Building, Supports, Magnets, etc...:" << std::endl;
  std::cout << "------------------------------------" << std::endl;
  std::cout << Form("+ CAVE: %10s ",caveFlag.Data()); printModule("CaveGeom_t",caveFlag); std::cout << " " << onoff[caveStat] << std::endl;; 
  std::cout << Form("+ PIPE: %10s ",pipeFlag.Data()); printModule("PipeGeom_t",pipeFlag); std::cout << " " << onoff[pipeStat] << std::endl;; 
  std::cout << Form("+ MAGP: %10s ",magpFlag.Data()); printModule("MagpGeom_t",magpFlag); std::cout << " " << onoff[magpStat] << std::endl;; 
  std::cout << Form("+ MFLD: %10s ",mfldFlag.Data()); printModule("MfldGeom_t",mfldFlag); std::cout << " " << onoff[mfldStat] << std::endl;; 
  std::cout << Form("+ SCON: %10s ",sconFlag.Data()); printModule("SconGeom_t",sconFlag); std::cout << " " << onoff[sconStat] << std::endl;; 
//std::cout << Form("+ SUPO: %10s ",supoFlag.Data()); printModule("SupoGeom_t",supoFlag); std::cout << " " << onoff[supoStat] << std::endl;; 
  std::cout << Form("+ UPST: %10s ",upstFlag.Data()); printModule("UpstGeom_t",upstFlag); std::cout << " " << onoff[upstStat] << std::endl;; 
  std::cout << std::endl;
  
}

#endif
