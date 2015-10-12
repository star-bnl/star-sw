#ifndef __Geometry_h__
#define __Geometry_h__

#include "StarVMC/StarAgmlLib/AgModule.h"
#include "StarVMC/StarAgmlLib/AgStructure.h"

#include "TString.h"
#include "TROOT.h"

#define AddDetector(x) TString x##Flag; Int_t x##Stat; Int_t nvolumes_##x; Int_t nnodes_##x; Int_t success_##x;
#define SetDetector(x) x##Flag="-none-"; x##Stat=0; nvolumes_##x=0; nnodes_##x=0; success_##x=0; detectors.push_back( #x  );
// ----------------------------------------------------------------------
class Geom_t : public AgStructure
{
 public:
  ClassDef(Geom_t,1);
  Geom_t(): AgStructure("Geom_t","STAR Master Geometry Table")
    {

  SetDetector(ecal);  SetDetector(pipe);  SetDetector(fpdm);  SetDetector(btof);
  SetDetector(vpdd);  SetDetector(ftpc);  SetDetector(ftro);  SetDetector(mutd);
  SetDetector(tpce);  SetDetector(calb);  SetDetector(scon);  SetDetector(svtt);
  SetDetector(phmd);  SetDetector(upst);  SetDetector(zcal);  SetDetector(cave);
  SetDetector(magp);  SetDetector(bbcm);  SetDetector(sisd);  SetDetector(rich);
  SetDetector(mfld);  SetDetector(fgtd);  SetDetector(idsm);  SetDetector(pixl);

  SetDetector(istd); // placeholder for IST
  SetDetector(pxld); // placeholder for PXL
  SetDetector(fsce); // Forward calorimeter upgrade
  SetDetector(eidd); // East poletip detectors
  SetDetector(tpcx); // eXtended TPC

  SetDetector(pxst);
  SetDetector(psup);
  SetDetector(dtub);
  SetDetector(hcal);

  SetDetector(ftsd); // Forward tracking system

      ecalCuts=1;
      calbCuts=1;
      
      // Few default flags for very stabilized geometries
      magpFlag="MAGPon";
      bbcmFlag="BBCMon";
      upstFlag="UPSTon";
      zcalFlag="ZCALon";

      // By default, do not create TpcRefSys
      tpcRefSys = false;

      // By default close the geometry
      closeGeometry = 1;

    };
  ~Geom_t(){ /* nada */ };

  TString select; // Geometry tag

  AddDetector(ecal);  AddDetector(pipe);  AddDetector(fpdm);  AddDetector(btof);
  AddDetector(vpdd);  AddDetector(ftpc);  AddDetector(ftro);  AddDetector(mutd);
  AddDetector(tpce);  AddDetector(calb);  AddDetector(scon);  AddDetector(svtt);
  AddDetector(phmd);  AddDetector(upst);  AddDetector(zcal);  AddDetector(cave);
  AddDetector(magp);  AddDetector(bbcm);  AddDetector(sisd);  AddDetector(rich);
  AddDetector(mfld);  AddDetector(fgtd);  AddDetector(idsm);  AddDetector(pixl);

  AddDetector(istd);
  AddDetector(pxld);
  AddDetector(fsce);
  AddDetector(eidd);
  AddDetector(tpcx); // eXtended TPC

  AddDetector(pxst);
  AddDetector(psup);
  AddDetector(dtub);

  AddDetector(hcal);

  AddDetector(ftsd); // Forward calo system

  Int_t ecalCuts;
  Int_t calbCuts;

  Int_t tpcRefSys;

  Int_t closeGeometry; // Flag which closes the geometry at end of construct.  If set false, the user is responsible for closing the geometry before drawing, simulation, etc...

  void Print ( const Option_t *opts="" ) const; // *MENU*

  typedef Int_t Meh_t;

  // Accumulate list of all detectors
  std::vector<TString> detectors;

};
#undef SetDetector
#undef AddDetector






// ----------------------------------------------------------------------
class BbcmGeom_t : public AgStructure
{ public:
  ClassDef(BbcmGeom_t,1);
  BbcmGeom_t(): AgStructure("BbcmGeom_t","STAR Beam Beam Counter Master Geometry Table")
    {
      select="default";
      module="none";
      config=-1;
    };
  ~BbcmGeom_t(){ /* nada */ };
  TString select;
  TString module;
  Int_t config;
};

class CalbGeom_t : public AgStructure
{ public:
  ClassDef(CalbGeom_t,1);
  CalbGeom_t(): AgStructure("CalbGeom_t","STAR Barrel Master Geometry Table")
    {
      select="default"; module="CalbGeo"; config=-1; emsedit=0;
      nmod = Array_t<Int_t>(2);
      shift= Array_t<Int_t>(2);
      emcuts = 0; // Default to zero
    };
  ~CalbGeom_t(){ };
  TString select;       // Selects BEMC options
  TString module;       // Which module to load (which AgSTAR subroutine to call)
  Int_t   config;       // Master configuration switch
  Int_t   emsedit;      //
  Array_t<Int_t> nmod;  //
  Array_t<Int_t> shift; //
  Int_t   emcuts;       // EM cut level
};

// ----------------------------------------------------------------------
class CaveGeom_t : public AgStructure
{ public:
  ClassDef(CaveGeom_t,1);
  CaveGeom_t(): AgStructure("CaveGeom_t","STAR Cave Master Geometry Table")
    {
      select="default"; module="CaveGeo"; config=-1;
    }
    ~CaveGeom_t(){};
  TString select;
  TString module;
  Int_t   config;
};

// ----------------------------------------------------------------------
class EcalGeom_t : public AgStructure
{ public:
  ClassDef(EcalGeom_t,1);
  EcalGeom_t(): AgStructure("EcalGeom_t","STAR Endcap Master Geometry Table")
    {
      select="default"; module="EcalGeo";     config=-1;     efill=3;     geometry=5;
      emcuts=0;
    };
  ~EcalGeom_t(){ };
  TString select;
  TString module;
  Int_t   config, efill, geometry, emcuts;
};

// ----------------------------------------------------------------------
class FpdmGeom_t : public AgStructure
{ public:
  ClassDef(FpdmGeom_t,1);
  FpdmGeom_t(): AgStructure("FpdmGeom_t","STAR Forward Pion Detector / Forward Meson Spectrometer Master Geometry Table")
    {
      select="default"; module="FpdmGeo";     config=-1; 
    }
  ~FpdmGeom_t(){ };
  TString select;
  TString module;
  Int_t   config;
  Int_t   position;
};

class FsceGeom_t : public AgStructure
{ public:
  ClassDef(FsceGeom_t,1);
  FsceGeom_t(): AgStructure("FsceGeom_t","STAR Forward ??? Calorimeter Master Geometry Table")
    { select="default"; module="-none-"; config=0; }
  ~FsceGeom_t(){ };

  TString select;
  TString module;
  Int_t   config;
};

class HcalGeom_t : public AgStructure
{ public:
  ClassDef(HcalGeom_t,1);
  HcalGeom_t(): AgStructure("HcalGeom_t","STAR Forward ??? Calorimeter Master Geometry Table")
    { select="default"; module="-none-"; config=0; }
  ~HcalGeom_t(){ };

  TString select;
  TString module;
  Int_t   config;
};

class EiddGeom_t : public AgStructure
{ public:
  ClassDef(EiddGeom_t,1);
  EiddGeom_t(): AgStructure("EiddGeom_t","STAR EAST poletip TRD, TOF and ECAL")
    { select="default"; module="-none-"; config=0; }
  ~EiddGeom_t(){ };
  TString select;
  TString module;
  Int_t   config;
};
  

// ----------------------------------------------------------------------
class FtpcGeom_t : public AgStructure
{ public:
  ClassDef(FtpcGeom_t,1);
  FtpcGeom_t(): AgStructure("FtpcGeom_t","STAR Forward Time Projection Chamber Master Geometry Table")
    {
      select="default"; module="FtpcGeo";     config=-1;supo=1;
    };
    ~FtpcGeom_t(){ };
  TString select;
  TString module;    
  TString supportModule;
  Int_t     config, supo;
};

// ----------------------------------------------------------------------
class FtsdGeom_t : public AgStructure
{ public:
  ClassDef(FtsdGeom_t,1);
  FtsdGeom_t(): AgStructure("FtsdGeom_t","STAR Forward Tracking System Master Geometry Table")
    {
      select="default"; module="FtsdGeo";  
    };
    ~FtsdGeom_t(){ };
  TString select;
  TString module;    
};

// ----------------------------------------------------------------------
class MagpGeom_t: public AgStructure
{ public:
  ClassDef(MagpGeom_t,1);
  MagpGeom_t(): AgStructure("MagpGeom_t","STAR Magnet Master Geometry Table")
    {
      select="default";
      module="MagpGeo";      
      version=1.0;
    };
  ~MagpGeom_t(){ /* nada */ };
  TString select;
  TString module;
  Float_t version;
};
  
// ----------------------------------------------------------------------
class MfldGeom_t : public AgStructure
{ public:
  ClassDef(MfldGeom_t,1);
  MfldGeom_t(): AgStructure("MfldGeom_t","STAR Magnetic Field Master Geometry Table")
    { // Magnetic field is provided somehow through StRoot.  Will need to investigate
      // the details before we implement starmc
      select="default"; module="NONE";     config=-1; field=0;
    };
  ~MfldGeom_t(){ };
  TString select;
  TString module;
  Int_t     config;
  Float_t   field;
};

// ----------------------------------------------------------------------
class MutdGeom_t : public AgStructure
{ public:
  ClassDef(MutdGeom_t,1);
  MutdGeom_t(): AgStructure("MutdGeom_t","STAR Muon Tagging Detector Master Geometry Table")
    {
      select="default"; module="MutdGeo"; config=-1;
    };
  ~MutdGeom_t(){ };
  TString select;
  TString module;
  Int_t config;
};

// ----------------------------------------------------------------------
class PhmdGeom_t : public AgStructure
{ public:
  ClassDef(PhmdGeom_t,1);
  PhmdGeom_t(): AgStructure("PhmdGeom_t","STAR Photon Multiplicity Detector Master Geometry Table")
    {
      select="default"; module="PhmdGeo"; config=-1;
    }
  ~PhmdGeom_t(){ };
  TString select;
  TString module;
  Int_t config;
};

// ----------------------------------------------------------------------
class PipeGeom_t : public AgStructure
{ public:
  ClassDef(PipeGeom_t,1);
  PipeGeom_t(): AgStructure("PipeGeom_t","STAR Beam Pipe Master Geometry Table")
    {
      select="PIPEon"; module="PipeGeo"; config=2; flag=3;
    };
  ~PipeGeom_t(){ };
  TString select;
  TString module;
  Int_t config;
  Int_t flag;
};

// ----------------------------------------------------------------------
class PixlGeom_t : public AgStructure
{ public:
  ClassDef(PixlGeom_t,1);
  PixlGeom_t(): AgStructure("PixlGeom_t","STAR Pixel Detector Master Geometry Table")
    {
      select="default"; module="PixlGeo3"; config=-1; location=1.0; ladrconfig=-1.0; secversion=-1.0;
    }
  ~PixlGeom_t(){ };
  TString select;
  TString module;  // Geometry module
  Int_t   config;  // Geometry module configuration
  Float_t location; // used in earlier geometries
  Float_t ladrconfig;  // Cable type
  Float_t secversion;  // sectors populated

};
// ----------------------------------------------------------------------
class DtubGeom_t : public AgStructure 
{ public:
  ClassDef(DtubGeom_t,1);
  DtubGeom_t(): AgStructure("DtubGeom_t","STAR Pixel Detector DtubGeo Geometry Table")
    {
      select="default"; module="DtubGeo1"; config=-1; location=1.0;      
    }
    ~DtubGeom_t(){ /* nada */ }
    TString select;
    TString module;
    Int_t   config;
    Float_t location;
};
// ----------------------------------------------------------------------
class SconGeom_t : public AgStructure
{ public:
  ClassDef(SconGeom_t,1);
  SconGeom_t(): AgStructure("SconGeom_t","STAR Support Cone Master Geometry Table")
    {
      select="default"; module="SconGeo"; config=-1;
    }
  ~SconGeom_t(){ };
  TString select;
  TString module;
  Float_t config;
};

// ----------------------------------------------------------------------
class SisdGeom_t : public AgStructure
{ public:
  ClassDef(SisdGeom_t,1);
  SisdGeom_t(): AgStructure("SisdGeom_t","STAR Silcon Strip Detector Master Geometry Table")
    {
      select="default"; module="SisdGeo"; config=-1;
    }
  ~SisdGeom_t(){ };
  TString select;
  TString module;
  Int_t config;
};

// ----------------------------------------------------------------------
class SvttGeom_t : public AgStructure
{ public:
  ClassDef(SvttGeom_t,1);
  SvttGeom_t(): AgStructure("SvttGeom_t","STAR Silcon Vertex Detector Master Geometry Table")
    {
      select="default"; module="SvttGeo11"; config=-1;
    }
  ~SvttGeom_t(){ };
  TString select;
  TString module;
  Int_t config;
  Int_t coneconfig;
  Int_t water;
  Int_t nwafer;
  Int_t nlayer;
  Int_t n1stlayer;
  Float_t waferdim; 
  Int_t svshconfig;
};

// ----------------------------------------------------------------------
class BtofGeom_t : public AgStructure
{ public:
  ClassDef(BtofGeom_t,1);
  BtofGeom_t(): AgStructure("BtofGeom_t","STAR Barrel Time Of Flight Master Geometry Table")
    {
      select="default"; module="BtofGeo"; config=-1; itof=0; tofX0 = 0; tofZ0=0;
    }
  ~BtofGeom_t(){ };
  TString select;
  TString module;
  Int_t config;
  Int_t itof;
  Float_t tofX0;
  Float_t tofZ0;
};

// ----------------------------------------------------------------------
class TpceGeom_t : public AgStructure
{ public:
  ClassDef(TpceGeom_t,1);
  TpceGeom_t(): AgStructure("TpceGeom_t","STAR Time Projection Chamber Master Geometry Table")
    {
      select="default"; module="TpceGeom"; config=-1; dens=-1; rmax=-1; subversion=-1;
    };
 ~TpceGeom_t(){ };
  TString select;
  TString module;
  Int_t config;
  Int_t dens;
  Int_t rmax;
  Float_t subversion;
};

class TpcxGeom_t : public AgStructure
{ public:
  ClassDef(TpcxGeom_t,1);
  TpcxGeom_t(): AgStructure("TpcxGeom_t","STAR Time Projection Chamber Master Geometry Table")
    {
      select="default"; module="TpcxGeom"; config=-1; dens=-1; rmax=-1;
    };
 ~TpcxGeom_t(){ };
  TString select;
  TString module;
  Int_t   config;
  Int_t   padconfig;
  Int_t   dens;
  Int_t   rmax;
};

// ----------------------------------------------------------------------
class VpddGeom_t : public AgStructure
{ public:
  ClassDef(VpddGeom_t,1);
  VpddGeom_t(): AgStructure("VpddGeom_t","STAR Vertex Position Detector Master Geometry Table")
    {
      select="default"; module="VpddGeo"; config=-1;
    };
  ~VpddGeom_t(){ };
  TString select;
  TString module;
  Int_t config;
};

// ----------------------------------------------------------------------
class UpstGeom_t : public AgStructure
{ public:
  ClassDef(UpstGeom_t,1);
  UpstGeom_t(): AgStructure("UpstGeom_t","STAR Upstream Master Geometry Table")
    {
      select="UPSTon"; module="UpstGeo"; config=-1;
    };
  ~UpstGeom_t(){ };
  TString select;
  TString module;
  Int_t config;
};

// ----------------------------------------------------------------------
class ZcalGeom_t : public AgStructure
{ public:
  ClassDef(ZcalGeom_t,1);
  ZcalGeom_t(): AgStructure("ZcalGeom_t","STAR Zero Degree Calorimeter Master Geometry Table")
    {
      select="ZCALon"; module="ZcalGeo"; config=-1;
    };
  ~ZcalGeom_t(){ };
  TString select;
  TString module;
  Int_t config;
};

// ----------------------------------------------------------------------
class FtroGeom_t : public AgStructure
{ public:
  ClassDef(FtroGeom_t,1);
  FtroGeom_t(): AgStructure("FtroGeom_t","STAR FTPC Readout Master Geometry Table")
    {
      select="FTROon"; module="FtroGeo"; config=-1;
    };
    ~FtroGeom_t(){ };
  TString select;
  TString module;
  Int_t config;
};

// ----------------------------------------------------------------------
class RichGeom_t : public AgStructure
{ public:
  ClassDef(RichGeom_t,1);
  RichGeom_t(): AgStructure("RichGeom_t","STAR FTPC Readout Master Geometry Table")
    {
      select="RICHon"; module="RichGeo"; config=-1;
    };
    ~RichGeom_t(){ };
  TString select;
  TString module;
  Int_t config;
};
// ----------------------------------------------------------------------
class FgtdGeom_t : public AgStructure
{ public:
  ClassDef(FgtdGeom_t,1);
  FgtdGeom_t(): AgStructure("FgtdGeom_t","STAR FGTD Master Geometry Table")
    {
      select="FGTDon"; module="FgtdGeo2"; config=1;
    }
    TString select;
    TString module;
    Int_t config;
};
// ----------------------------------------------------------------------
class IdsmGeom_t : public AgStructure
{ public:
  ClassDef(IdsmGeom_t,1);
  IdsmGeom_t(): AgStructure("IdsmGeom_t","STAR IDSM Master Geometry Table")
    {
      select="IDSMon"; module="IdsmGeo1"; config=1;
    }
    TString select;
    TString module;
    Int_t config;
};
// ----------------------------------------------------------------------
class IstdGeom_t : public AgStructure
{ public:
  ClassDef(IstdGeom_t,1);
  IstdGeom_t(): AgStructure("IstdGeom_t","STAR ISTD Master Geometry Table")
    {
      select="ISTDon"; module="IstdGeo"; config=1; 
    }
    TString select;
    TString module;
    Int_t config;
};
// ----------------------------------------------------------------------
class PxstGeom_t : public AgStructure
{ public:
  ClassDef(PxstGeom_t,1);
  PxstGeom_t(): AgStructure("PxstGeom_t","STAR PXST Master Geometry Table")
    {
      //      select="PXSTon"; module="PxstGeo"; config=1; 
      select="PXSTon"; module="PixlGeo"; config=1; 
    }
    TString select;
    TString module;
    Int_t config;
};
class PsupGeom_t : public AgStructure
{ public:
  ClassDef(PsupGeom_t,1);
  PsupGeom_t(): AgStructure("PsupGeom_t","STAR PSUP Master Geometry Table")
    {
      //      select="PSUPon"; module="PsupGeo"; config=1; 
      select="PSUPon"; module="PixlGeo"; config=1; 
    }
    TString select;
    TString module;
    Int_t config;
};
// ----------------------------------------------------------------------
class PxldGeom_t : public AgStructure
{ public:
  ClassDef(PxldGeom_t,1);
  PxldGeom_t(): AgStructure("PxldGeom_t","STAR PXLD Master Geometry Table")
    {
      select="PXLDon"; module="PxldGeo"; config=1;
    }
    TString select;
    TString module;
    Int_t config;
};

//////////////////////////////////////////////////////////
//
class Geometry : public AgModule
{
 public:
  
  Geometry();
  ~Geometry(){ /* nada */ };

  void ConstructGeometry(const Char_t *tag);
  void StarsimGeometry(const Char_t *tag);

  Int_t numberOfNodes( const Char_t *volume="CAVE" );
  Int_t numberOfVolumes( const Char_t *volume="CAVE" );

 private:
 protected:

  void GetStatistics( const Char_t *volume );

  Bool_t BbcmInit();
  Bool_t CalbInit();  Bool_t EcalInit();  Bool_t CaveInit();
  Bool_t FpdmInit();  Bool_t FtpcInit();  Bool_t MfldInit();
  Bool_t MutdInit();  Bool_t PhmdInit();  Bool_t PipeInit();
  Bool_t PixlInit();  Bool_t SconInit();  Bool_t SisdInit();
  Bool_t SvttInit();  Bool_t BtofInit();  Bool_t TpceInit();
  Bool_t VpddInit();  Bool_t MagpInit();  Bool_t UpstInit();
  Bool_t ZcalInit();  Bool_t FtroInit();  Bool_t RichInit();
  Bool_t FgtdInit();  Bool_t IdsmInit();  Bool_t FsceInit();
  Bool_t EiddInit();  Bool_t TpcxInit();  Bool_t HcalInit();

  Bool_t IstdInit();
  Bool_t PxldInit(){ /* placeholder */ return false; }

  Bool_t PxstInit();
  Bool_t PsupInit();
  
  Bool_t FtsdInit();

  Bool_t GeomInit();

  Bool_t ConstructBbcm( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructEcal( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructPipe( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructFpdm( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructTpce( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructCalb( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructBtof( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructVpdd( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructFtpc( const Char_t *flag, Bool_t go=true );
  // FTRO constructed with FTPC
  Bool_t ConstructMutd( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructFtro( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructSvtt( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructSisd( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructMagp( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructScon( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructCave( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructPhmd( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructUpst( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructZcal( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructRich( const Char_t *flag, Bool_t go=false ); // RICH is deprecated

  Bool_t ConstructPixl( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructDtub( const Char_t *flag, Bool_t go=true );

  // FGT and new inner support cone
  Bool_t ConstructFgtd( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructIdsm( const Char_t *flag, Bool_t go=true );

  // New Inner Detectors
  Bool_t ConstructPxst( const Char_t *flag, Bool_t go=false );
  Bool_t ConstructIstd( const Char_t *flag, Bool_t go=false );
  Bool_t ConstructPxld( const Char_t *flag, Bool_t go=false ) { /* placeholder */ return false; }
  Bool_t ConstructPsup( const Char_t *flag, Bool_t go=false );

  // eSTAR Upgrades
  Bool_t ConstructFsce( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructEidd( const Char_t *flag, Bool_t go=true );
  Bool_t ConstructHcal( const Char_t *flag, Bool_t go=true );

  Bool_t ConstructFtsd( const Char_t *flag, Bool_t go=true );

  // TPC Upgrade
  Bool_t ConstructTpcx( const Char_t *flag, Bool_t go=true );

 public:
  AgModule *CreateModule( const Char_t *name, const Char_t *top = NULL );
    
  ClassDef(Geometry,1);



};

#endif
