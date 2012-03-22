#ifndef __EiddGeo__ 
#define __EiddGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace EIDDGEO // $NMSPC 
{ 
   class Eidv_t : public AgStructure 
   { 
      ClassDef(Eidv_t,1); 
      public: 
      Float_t version; 
      Float_t eidconfig; 
      Eidv_t() : AgStructure("Eidv_t","User-defined AgML structure") 
      { 
         version=0; 
         eidconfig=0; 
         _index=0; 
      } 
      ~ Eidv_t(){ /* nada */ }; 
   }; 
   class Eidg_t : public AgStructure 
   { 
      ClassDef(Eidg_t,1); 
      public: 
      Float_t version; 
      Float_t ettz; 
      Float_t etrvz; 
      Float_t etfvz; 
      Float_t eclvz; 
      Float_t rmin; 
      Float_t rmax; 
      Float_t dphi; 
      Float_t ettthick; 
      Float_t tfvthick; 
      Float_t tofthick; 
      Float_t tbxthick; 
      Float_t tpbthick; 
      Float_t tglthick; 
      Float_t tgpthick; 
      Float_t elvthick; 
      Float_t eclthick; 
      Float_t eaathick; 
      Float_t eabthick; 
      Float_t scnthick; 
      Float_t trvthick; 
      Float_t trdthick; 
      Float_t trathick; 
      Float_t tabthick; 
      Float_t tbdthick; 
      Eidg_t() : AgStructure("Eidg_t","User-defined AgML structure") 
      { 
         version=0; 
         ettz=0; 
         etrvz=0; 
         etfvz=0; 
         eclvz=0; 
         rmin=0; 
         rmax=0; 
         dphi=0; 
         ettthick=0; 
         tfvthick=0; 
         tofthick=0; 
         tbxthick=0; 
         tpbthick=0; 
         tglthick=0; 
         tgpthick=0; 
         elvthick=0; 
         eclthick=0; 
         eaathick=0; 
         eabthick=0; 
         scnthick=0; 
         trvthick=0; 
         trdthick=0; 
         trathick=0; 
         tabthick=0; 
         tbdthick=0; 
         _index=0; 
      } 
      ~ Eidg_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- ETTV -- 
   ///@defgroup ETTV_doc 
   ///@class ETTV 
   ///@brief is the whole ETTIE detector volume 
   class ETTV : public AgBlock 
   {  public: 
      ETTV() : AgBlock("ETTV","is the whole ETTIE detector volume"){ }; 
      ~ETTV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ETTV,1); 
   }; 
   // ---------------------------------------------------------------------- ETRV -- 
   ///@defgroup ETRV_doc 
   ///@class ETRV 
   ///@brief is the endcap TRD detector volume (3 layers) 
   class ETRV : public AgBlock 
   {  public: 
      ETRV() : AgBlock("ETRV","is the endcap TRD detector volume (3 layers)"){ }; 
      ~ETRV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ETRV,1); 
   }; 
   // ---------------------------------------------------------------------- ECLV -- 
   ///@defgroup ECLV_doc 
   ///@class ECLV 
   ///@brief is the endcap convertor-scintillator volume  
   class ECLV : public AgBlock 
   {  public: 
      ECLV() : AgBlock("ECLV","is the endcap convertor-scintillator volume "){ }; 
      ~ECLV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ECLV,1); 
   }; 
   // ---------------------------------------------------------------------- ETFV -- 
   ///@defgroup ETFV_doc 
   ///@class ETFV 
   ///@brief is the endcap TOF volume  
   class ETFV : public AgBlock 
   {  public: 
      ETFV() : AgBlock("ETFV","is the endcap TOF volume "){ }; 
      ~ETFV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ETFV,1); 
   }; 
   // ---------------------------------------------------------------------- ETOF -- 
   ///@defgroup ETOF_doc 
   ///@class ETOF 
   ///@brief is the ETOF supermodule 
   class ETOF : public AgBlock 
   {  public: 
      ETOF() : AgBlock("ETOF","is the ETOF supermodule"){ }; 
      ~ETOF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ETOF,1); 
   }; 
   // ---------------------------------------------------------------------- TBOX -- 
   ///@defgroup TBOX_doc 
   ///@class TBOX 
   ///@brief is the ETOF module gas box 
   class TBOX : public AgBlock 
   {  public: 
      TBOX() : AgBlock("TBOX","is the ETOF module gas box"){ }; 
      ~TBOX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TBOX,1); 
   }; 
   // ---------------------------------------------------------------------- TGAS -- 
   ///@defgroup TGAS_doc 
   ///@class TGAS 
   ///@brief is the ETOF module gas volume 
   class TGAS : public AgBlock 
   {  public: 
      TGAS() : AgBlock("TGAS","is the ETOF module gas volume"){ }; 
      ~TGAS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TGAS,1); 
   }; 
   // ---------------------------------------------------------------------- TPCB -- 
   ///@defgroup TPCB_doc 
   ///@class TPCB 
   ///@brief is the MRPC readout Board 
   class TPCB : public AgBlock 
   {  public: 
      TPCB() : AgBlock("TPCB","is the MRPC readout Board"){ }; 
      ~TPCB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TPCB,1); 
   }; 
   // ---------------------------------------------------------------------- TGLA -- 
   ///@defgroup TGLA_doc 
   ///@class TGLA 
   ///@brief is the MRPC glass plate 
   class TGLA : public AgBlock 
   {  public: 
      TGLA() : AgBlock("TGLA","is the MRPC glass plate"){ }; 
      ~TGLA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TGLA,1); 
   }; 
   // ---------------------------------------------------------------------- TGAP -- 
   ///@defgroup TGAP_doc 
   ///@class TGAP 
   ///@brief is the MRPC gas gap (sensitive) 
   class TGAP : public AgBlock 
   {  public: 
      TGAP() : AgBlock("TGAP","is the MRPC gas gap (sensitive)"){ }; 
      ~TGAP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TGAP,1); 
   }; 
   // ---------------------------------------------------------------------- TECL -- 
   ///@defgroup TECL_doc 
   ///@class TECL 
   ///@brief is the TECL supermodule 
   class TECL : public AgBlock 
   {  public: 
      TECL() : AgBlock("TECL","is the TECL supermodule"){ }; 
      ~TECL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TECL,1); 
   }; 
   // ---------------------------------------------------------------------- TEAA -- 
   ///@defgroup TEAA_doc 
   ///@class TEAA 
   ///@brief is the 1st lead convertor 
   class TEAA : public AgBlock 
   {  public: 
      TEAA() : AgBlock("TEAA","is the 1st lead convertor"){ }; 
      ~TEAA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TEAA,1); 
   }; 
   // ---------------------------------------------------------------------- TEAB -- 
   ///@defgroup TEAB_doc 
   ///@class TEAB 
   ///@brief is the 2nd lead convertor 
   class TEAB : public AgBlock 
   {  public: 
      TEAB() : AgBlock("TEAB","is the 2nd lead convertor"){ }; 
      ~TEAB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TEAB,1); 
   }; 
   // ---------------------------------------------------------------------- SCIN -- 
   ///@defgroup SCIN_doc 
   ///@class SCIN 
   ///@brief is the plastic scintillator (sensitive) 
   class SCIN : public AgBlock 
   {  public: 
      SCIN() : AgBlock("SCIN","is the plastic scintillator (sensitive)"){ }; 
      ~SCIN(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SCIN,1); 
   }; 
   // ---------------------------------------------------------------------- ETRD -- 
   ///@defgroup ETRD_doc 
   ///@class ETRD 
   ///@brief is the ETRD supermodule 
   class ETRD : public AgBlock 
   {  public: 
      ETRD() : AgBlock("ETRD","is the ETRD supermodule"){ }; 
      ~ETRD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ETRD,1); 
   }; 
   // ---------------------------------------------------------------------- TRAD -- 
   ///@defgroup TRAD_doc 
   ///@class TRAD 
   ///@brief is the TR radiator 
   class TRAD : public AgBlock 
   {  public: 
      TRAD() : AgBlock("TRAD","is the TR radiator"){ }; 
      ~TRAD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TRAD,1); 
   }; 
   // ---------------------------------------------------------------------- TABS -- 
   ///@defgroup TABS_doc 
   ///@class TABS 
   ///@brief is the TRD Xe absorber (sensitive) 
   class TABS : public AgBlock 
   {  public: 
      TABS() : AgBlock("TABS","is the TRD Xe absorber (sensitive)"){ }; 
      ~TABS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TABS,1); 
   }; 
   // ---------------------------------------------------------------------- TBOD -- 
   ///@defgroup TBOD_doc 
   ///@class TBOD 
   ///@brief is the TRD readout mother board 
   class TBOD : public AgBlock 
   {  public: 
      TBOD() : AgBlock("TBOD","is the TRD readout mother board"){ }; 
      ~TBOD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TBOD,1); 
   }; 
   /// \class EiddGeo 
   /// \brief  is the ETTIE (Endcap Trd-Tof for Iding Electron) Detector of STAR  
   class EiddGeo : public AgModule 
   { 
      public: 
      EiddGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~EiddGeo(){ }; 
      ClassDef(EiddGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace EiddGeo 
#endif // __EiddGeo__ 
