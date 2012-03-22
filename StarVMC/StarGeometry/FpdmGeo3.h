#ifndef __FpdmGeo3__ 
#define __FpdmGeo3__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace FPDMGEO3 // $NMSPC 
{ 
   class Fmcg_t : public AgStructure 
   { 
      ClassDef(Fmcg_t,1); 
      public: 
      Float_t version; 
      Float_t chkvsim; 
      Float_t pbplate; 
      Float_t fmsnorthx; 
      Float_t fmssouthx; 
      Fmcg_t() : AgStructure("Fmcg_t","User-defined AgML structure") 
      { 
         version=0; 
         chkvsim=0; 
         pbplate=0; 
         fmsnorthx=0; 
         fmssouthx=0; 
         _index=0; 
      } 
      ~ Fmcg_t(){ /* nada */ }; 
   }; 
   class Fpos_t : public AgStructure 
   { 
      ClassDef(Fpos_t,1); 
      public: 
      Float_t imod; 
      Float_t itype; 
      Float_t x; 
      Float_t y; 
      Float_t z; 
      Float_t ay; 
      Float_t az; 
      Fpos_t() : AgStructure("Fpos_t","User-defined AgML structure") 
      { 
         imod=0; 
         itype=0; 
         x=0; 
         y=0; 
         z=0; 
         ay=0; 
         az=0; 
         _index=0; 
      } 
      ~ Fpos_t(){ /* nada */ }; 
   }; 
   class Fbxd_t : public AgStructure 
   { 
      ClassDef(Fbxd_t,1); 
      public: 
      Float_t type; 
      Float_t height; 
      Float_t depth; 
      Float_t width; 
      Float_t nx; 
      Float_t ny; 
      Float_t nxl; 
      Float_t nyl; 
      Float_t xoffset; 
      Float_t zoffset; 
      Float_t psoffset; 
      Float_t smdoff; 
      Fbxd_t() : AgStructure("Fbxd_t","User-defined AgML structure") 
      { 
         type=0; 
         height=0; 
         depth=0; 
         width=0; 
         nx=0; 
         ny=0; 
         nxl=0; 
         nyl=0; 
         xoffset=0; 
         zoffset=0; 
         psoffset=0; 
         smdoff=0; 
         _index=0; 
      } 
      ~ Fbxd_t(){ /* nada */ }; 
   }; 
   class Flgg_t : public AgStructure 
   { 
      ClassDef(Flgg_t,1); 
      public: 
      Float_t type; 
      Float_t width; 
      Float_t depth; 
      Float_t dgap; 
      Float_t althick; 
      Float_t phcathdz; 
      Float_t phcathr; 
      Float_t mumetdz; 
      Float_t mumetr; 
      Flgg_t() : AgStructure("Flgg_t","User-defined AgML structure") 
      { 
         type=0; 
         width=0; 
         depth=0; 
         dgap=0; 
         althick=0; 
         phcathdz=0; 
         phcathr=0; 
         mumetdz=0; 
         mumetr=0; 
         _index=0; 
      } 
      ~ Flgg_t(){ /* nada */ }; 
   }; 
   class Flgm_t : public AgStructure 
   { 
      ClassDef(Flgm_t,1); 
      public: 
      Float_t type; 
      Float_t density; 
      Float_t radlen; 
      Float_t pbcont; 
      Float_t critene; 
      Float_t molierer; 
      Flgm_t() : AgStructure("Flgm_t","User-defined AgML structure") 
      { 
         type=0; 
         density=0; 
         radlen=0; 
         pbcont=0; 
         critene=0; 
         molierer=0; 
         _index=0; 
      } 
      ~ Flgm_t(){ /* nada */ }; 
   }; 
   class Pbpd_t : public AgStructure 
   { 
      ClassDef(Pbpd_t,1); 
      public: 
      Float_t z; 
      Float_t width; 
      Float_t height; 
      Float_t thick; 
      Pbpd_t() : AgStructure("Pbpd_t","User-defined AgML structure") 
      { 
         z=0; 
         width=0; 
         height=0; 
         thick=0; 
         _index=0; 
      } 
      ~ Pbpd_t(){ /* nada */ }; 
   }; 
   class Fmxg_t : public AgStructure 
   { 
      ClassDef(Fmxg_t,1); 
      public: 
      Float_t version; 
      Float_t sapex; 
      Float_t sbase; 
      Float_t sgap; 
      Float_t nstrip; 
      Float_t g10width; 
      Float_t g10hgt; 
      Float_t g10thick; 
      Fmxg_t() : AgStructure("Fmxg_t","User-defined AgML structure") 
      { 
         version=0; 
         sapex=0; 
         sbase=0; 
         sgap=0; 
         nstrip=0; 
         g10width=0; 
         g10hgt=0; 
         g10thick=0; 
         _index=0; 
      } 
      ~ Fmxg_t(){ /* nada */ }; 
   }; 
   class Inse_t : public AgStructure 
   { 
      ClassDef(Inse_t,1); 
      public: 
      Float_t width; 
      Float_t depth; 
      Float_t height; 
      Float_t sheetdpt; 
      Float_t holegap; 
      Float_t holedepth; 
      Float_t holeheight; 
      Float_t gapdepth; 
      Float_t gapheight; 
      Float_t gatedepth; 
      Float_t ra; 
      Float_t rb; 
      Float_t diam; 
      Float_t rmax; 
      Float_t gategap; 
      Inse_t() : AgStructure("Inse_t","User-defined AgML structure") 
      { 
         width=0; 
         depth=0; 
         height=0; 
         sheetdpt=0; 
         holegap=0; 
         holedepth=0; 
         holeheight=0; 
         gapdepth=0; 
         gapheight=0; 
         gatedepth=0; 
         ra=0; 
         rb=0; 
         diam=0; 
         rmax=0; 
         gategap=0; 
         _index=0; 
      } 
      ~ Inse_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- FBOX -- 
   ///@defgroup FBOX_doc 
   ///@class FBOX 
   ///@brief is one Pb-Glass fpd detector 
   class FBOX : public AgBlock 
   {  public: 
      FBOX() : AgBlock("FBOX","is one Pb-Glass fpd detector"){ }; 
      ~FBOX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FBOX,1); 
   }; 
   // ---------------------------------------------------------------------- FTOW -- 
   ///@defgroup FTOW_doc 
   ///@class FTOW 
   ///@brief is one PbG Tower 
   class FTOW : public AgBlock 
   {  public: 
      FTOW() : AgBlock("FTOW","is one PbG Tower"){ }; 
      ~FTOW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FTOW,1); 
   }; 
   // ---------------------------------------------------------------------- FWAL -- 
   ///@defgroup FWAL_doc 
   ///@class FWAL 
   ///@brief is almunum wrapper 
   class FWAL : public AgBlock 
   {  public: 
      FWAL() : AgBlock("FWAL","is almunum wrapper"){ }; 
      ~FWAL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FWAL,1); 
   }; 
   // ---------------------------------------------------------------------- FLGR -- 
   ///@defgroup FLGR_doc 
   ///@class FLGR 
   ///@brief is Lead Glass detector 
   class FLGR : public AgBlock 
   {  public: 
      FLGR() : AgBlock("FLGR","is Lead Glass detector"){ }; 
      ~FLGR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FLGR,1); 
   }; 
   // ---------------------------------------------------------------------- FLXF -- 
   ///@defgroup FLXF_doc 
   ///@class FLXF 
   ///@brief is Lead Glass detector 
   class FLXF : public AgBlock 
   {  public: 
      FLXF() : AgBlock("FLXF","is Lead Glass detector"){ }; 
      ~FLXF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FLXF,1); 
   }; 
   // ---------------------------------------------------------------------- FALU -- 
   ///@defgroup FALU_doc 
   ///@class FALU 
   ///@brief is Aluminium Base Cell 
   class FALU : public AgBlock 
   {  public: 
      FALU() : AgBlock("FALU","is Aluminium Base Cell"){ }; 
      ~FALU(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FALU,1); 
   }; 
   // ---------------------------------------------------------------------- FBAS -- 
   ///@defgroup FBAS_doc 
   ///@class FBAS 
   ///@brief is Steel Base Plate 
   class FBAS : public AgBlock 
   {  public: 
      FBAS() : AgBlock("FBAS","is Steel Base Plate"){ }; 
      ~FBAS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FBAS,1); 
   }; 
   // ---------------------------------------------------------------------- FENC -- 
   ///@defgroup FENC_doc 
   ///@class FENC 
   ///@brief is Steel Enclosure 
   class FENC : public AgBlock 
   {  public: 
      FENC() : AgBlock("FENC","is Steel Enclosure"){ }; 
      ~FENC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FENC,1); 
   }; 
   // ---------------------------------------------------------------------- FEAC -- 
   ///@defgroup FEAC_doc 
   ///@class FEAC 
   ///@brief is Steel Enclosure 
   class FEAC : public AgBlock 
   {  public: 
      FEAC() : AgBlock("FEAC","is Steel Enclosure"){ }; 
      ~FEAC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FEAC,1); 
   }; 
   // ---------------------------------------------------------------------- FEBC -- 
   ///@defgroup FEBC_doc 
   ///@class FEBC 
   ///@brief is Air square hole 
   class FEBC : public AgBlock 
   {  public: 
      FEBC() : AgBlock("FEBC","is Air square hole"){ }; 
      ~FEBC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FEBC,1); 
   }; 
   // ---------------------------------------------------------------------- FECC -- 
   ///@defgroup FECC_doc 
   ///@class FECC 
   ///@brief is Steel distancer 
   class FECC : public AgBlock 
   {  public: 
      FECC() : AgBlock("FECC","is Steel distancer"){ }; 
      ~FECC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FECC,1); 
   }; 
   // ---------------------------------------------------------------------- FEDC -- 
   ///@defgroup FEDC_doc 
   ///@class FEDC 
   ///@brief is Steel Enclosure part on south 
   class FEDC : public AgBlock 
   {  public: 
      FEDC() : AgBlock("FEDC","is Steel Enclosure part on south"){ }; 
      ~FEDC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FEDC,1); 
   }; 
   // ---------------------------------------------------------------------- FEEC -- 
   ///@defgroup FEEC_doc 
   ///@class FEEC 
   ///@brief is Steel Enclosure part on north 
   class FEEC : public AgBlock 
   {  public: 
      FEEC() : AgBlock("FEEC","is Steel Enclosure part on north"){ }; 
      ~FEEC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FEEC,1); 
   }; 
   // ---------------------------------------------------------------------- FETC -- 
   ///@defgroup FETC_doc 
   ///@class FETC 
   ///@brief is Air Enclosure part 
   class FETC : public AgBlock 
   {  public: 
      FETC() : AgBlock("FETC","is Air Enclosure part"){ }; 
      ~FETC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FETC,1); 
   }; 
   // ---------------------------------------------------------------------- FERC -- 
   ///@defgroup FERC_doc 
   ///@class FERC 
   ///@brief is Air Enclosure part 
   class FERC : public AgBlock 
   {  public: 
      FERC() : AgBlock("FERC","is Air Enclosure part"){ }; 
      ~FERC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FERC,1); 
   }; 
   // ---------------------------------------------------------------------- FESC -- 
   ///@defgroup FESC_doc 
   ///@class FESC 
   ///@brief is Air Enclosure part 
   class FESC : public AgBlock 
   {  public: 
      FESC() : AgBlock("FESC","is Air Enclosure part"){ }; 
      ~FESC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FESC,1); 
   }; 
   // ---------------------------------------------------------------------- FPCT -- 
   ///@defgroup FPCT_doc 
   ///@class FPCT 
   ///@brief is Photo Cathode 
   class FPCT : public AgBlock 
   {  public: 
      FPCT() : AgBlock("FPCT","is Photo Cathode"){ }; 
      ~FPCT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FPCT,1); 
   }; 
   // ---------------------------------------------------------------------- FUMT -- 
   ///@defgroup FUMT_doc 
   ///@class FUMT 
   ///@brief is mu metal 
   class FUMT : public AgBlock 
   {  public: 
      FUMT() : AgBlock("FUMT","is mu metal"){ }; 
      ~FUMT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FUMT,1); 
   }; 
   // ---------------------------------------------------------------------- PBPT -- 
   ///@defgroup PBPT_doc 
   ///@class PBPT 
   ///@brief is pb plate 
   class PBPT : public AgBlock 
   {  public: 
      PBPT() : AgBlock("PBPT","is pb plate"){ }; 
      ~PBPT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PBPT,1); 
   }; 
   // ---------------------------------------------------------------------- FSHM -- 
   ///@defgroup FSHM_doc 
   ///@class FSHM 
   ///@brief is the SHower Max section 
   class FSHM : public AgBlock 
   {  public: 
      FSHM() : AgBlock("FSHM","is the SHower Max section"){ }; 
      ~FSHM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSHM,1); 
   }; 
   // ---------------------------------------------------------------------- FXGT -- 
   ///@defgroup FXGT_doc 
   ///@class FXGT 
   ///@brief is the G10 layer in the SMax 
   class FXGT : public AgBlock 
   {  public: 
      FXGT() : AgBlock("FXGT","is the G10 layer in the SMax"){ }; 
      ~FXGT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FXGT,1); 
   }; 
   // ---------------------------------------------------------------------- FHMS -- 
   ///@defgroup FHMS_doc 
   ///@class FHMS 
   ///@brief is sHower Max Strip 
   class FHMS : public AgBlock 
   {  public: 
      FHMS() : AgBlock("FHMS","is sHower Max Strip"){ }; 
      ~FHMS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FHMS,1); 
   }; 
   /// \class FpdmGeo3 
   /// \brief  is the Forward Pion Detector Modules GEOmetry  
   class FpdmGeo3 : public AgModule 
   { 
      public: 
      FpdmGeo3(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~FpdmGeo3(){ }; 
      ClassDef(FpdmGeo3,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace FpdmGeo3 
#endif // __FpdmGeo3__ 
