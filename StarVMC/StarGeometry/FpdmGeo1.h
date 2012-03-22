#ifndef __FpdmGeo1__ 
#define __FpdmGeo1__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace FPDMGEO1 // $NMSPC 
{ 
   class Fmcg_t : public AgStructure 
   { 
      ClassDef(Fmcg_t,1); 
      public: 
      Float_t version; 
      Float_t chkvsim; 
      Fmcg_t() : AgStructure("Fmcg_t","User-defined AgML structure") 
      { 
         version=0; 
         chkvsim=0; 
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
      Fpos_t() : AgStructure("Fpos_t","User-defined AgML structure") 
      { 
         imod=0; 
         itype=0; 
         x=0; 
         y=0; 
         z=0; 
         ay=0; 
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
      Float_t nx; 
      Float_t ny; 
      Float_t xoffset; 
      Float_t zoffset; 
      Float_t psoffset; 
      Float_t smdoffset; 
      Fbxd_t() : AgStructure("Fbxd_t","User-defined AgML structure") 
      { 
         type=0; 
         height=0; 
         depth=0; 
         nx=0; 
         ny=0; 
         xoffset=0; 
         zoffset=0; 
         psoffset=0; 
         smdoffset=0; 
         _index=0; 
      } 
      ~ Fbxd_t(){ /* nada */ }; 
   }; 
   class Flgg_t : public AgStructure 
   { 
      ClassDef(Flgg_t,1); 
      public: 
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
      Float_t density; 
      Float_t radlen; 
      Float_t pbcont; 
      Float_t critene; 
      Float_t molierer; 
      Flgm_t() : AgStructure("Flgm_t","User-defined AgML structure") 
      { 
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
      Float_t g10height; 
      Float_t g10thick; 
      Fmxg_t() : AgStructure("Fmxg_t","User-defined AgML structure") 
      { 
         version=0; 
         sapex=0; 
         sbase=0; 
         sgap=0; 
         nstrip=0; 
         g10width=0; 
         g10height=0; 
         g10thick=0; 
         _index=0; 
      } 
      ~ Fmxg_t(){ /* nada */ }; 
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
   // ---------------------------------------------------------------------- FLGT -- 
   ///@defgroup FLGT_doc 
   ///@class FLGT 
   ///@brief is one PbG Tower 
   class FLGT : public AgBlock 
   {  public: 
      FLGT() : AgBlock("FLGT","is one PbG Tower"){ }; 
      ~FLGT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FLGT,1); 
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
   /// \class FpdmGeo1 
   /// \brief  is the Forward Pion Detector Modules GEOmetry  
   class FpdmGeo1 : public AgModule 
   { 
      public: 
      FpdmGeo1(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~FpdmGeo1(){ }; 
      ClassDef(FpdmGeo1,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace FpdmGeo1 
#endif // __FpdmGeo1__ 
