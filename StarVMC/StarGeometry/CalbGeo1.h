#ifndef __CalbGeo1__ 
#define __CalbGeo1__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace CALBGEO1 // $NMSPC 
{ 
   class Calg_t : public AgStructure 
   { 
      ClassDef(Calg_t,1); 
      public: 
      Float_t version; 
      Float_t rmin; 
      Float_t etacut; 
      Float_t crackwd; 
      Float_t frontthk; 
      Float_t compthk; 
      Float_t airthk; 
      Float_t backthk; 
      Float_t spacethk; 
      Array_t<Float_t> scintthk; 
      Float_t absorthk; 
      Float_t abpapthk; 
      Float_t g10sbthk; 
      Float_t smalfwdh; 
      Float_t smalfthk; 
      Float_t smgasthk; 
      Float_t smgaswdh; 
      Float_t smgasrad; 
      Float_t smaffwdh; 
      Float_t smafbwdh; 
      Float_t smetawdh; 
      Float_t seta1wdh; 
      Float_t netfirst; 
      Float_t seta2wdh; 
      Float_t netsecon; 
      Float_t set12wdh; 
      Float_t sphiwdh; 
      Float_t sphidwdh; 
      Float_t nphistr; 
      Float_t nsmdalw; 
      Float_t nsuper; 
      Float_t nsmd; 
      Array_t<Float_t> nsublay; 
      Array_t<Float_t> nmodule; 
      Array_t<Float_t> shift; 
      Float_t maxmodule; 
      Float_t netat; 
      Float_t nsub; 
      Float_t netasmdp; 
      Array_t<Float_t> modmap; 
      Calg_t() : AgStructure("Calg_t","User-defined AgML structure") 
      { 
         version=0; 
         rmin=0; 
         etacut=0; 
         crackwd=0; 
         frontthk=0; 
         compthk=0; 
         airthk=0; 
         backthk=0; 
         spacethk=0; 
         scintthk = Array_t<Float_t>(2); 
         absorthk=0; 
         abpapthk=0; 
         g10sbthk=0; 
         smalfwdh=0; 
         smalfthk=0; 
         smgasthk=0; 
         smgaswdh=0; 
         smgasrad=0; 
         smaffwdh=0; 
         smafbwdh=0; 
         smetawdh=0; 
         seta1wdh=0; 
         netfirst=0; 
         seta2wdh=0; 
         netsecon=0; 
         set12wdh=0; 
         sphiwdh=0; 
         sphidwdh=0; 
         nphistr=0; 
         nsmdalw=0; 
         nsuper=0; 
         nsmd=0; 
         nsublay = Array_t<Float_t>(2); 
         nmodule = Array_t<Float_t>(2); 
         shift = Array_t<Float_t>(2); 
         maxmodule=0; 
         netat=0; 
         nsub=0; 
         netasmdp=0; 
         modmap = Array_t<Float_t>(60); 
         _index=0; 
      } 
      ~ Calg_t(){ /* nada */ }; 
   }; 
   class Calr_t : public AgStructure 
   { 
      ClassDef(Calr_t,1); 
      public: 
      Float_t rmin; 
      Float_t rprs; 
      Float_t rsmd1; 
      Float_t rsmd2; 
      Float_t rmax; 
      Calr_t() : AgStructure("Calr_t","User-defined AgML structure") 
      { 
         rmin=0; 
         rprs=0; 
         rsmd1=0; 
         rsmd2=0; 
         rmax=0; 
         _index=0; 
      } 
      ~ Calr_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- CALB -- 
   ///@defgroup CALB_doc 
   ///@class CALB 
   ///@brief is EMC Barrel envelope 
   class CALB : public AgBlock 
   {  public: 
      CALB() : AgBlock("CALB","is EMC Barrel envelope"){ }; 
      ~CALB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CALB,1); 
   }; 
   // ---------------------------------------------------------------------- CHLV -- 
   ///@defgroup CHLV_doc 
   ///@class CHLV 
   ///@brief corresponds to double modules... 
   class CHLV : public AgBlock 
   {  public: 
      CHLV() : AgBlock("CHLV","corresponds to double modules..."){ }; 
      ~CHLV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CHLV,1); 
   }; 
   // ---------------------------------------------------------------------- CPHI -- 
   ///@defgroup CPHI_doc 
   ///@class CPHI 
   ///@brief corresponds to a single module 
   class CPHI : public AgBlock 
   {  public: 
      CPHI() : AgBlock("CPHI","corresponds to a single module"){ }; 
      ~CPHI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CPHI,1); 
   }; 
   // ---------------------------------------------------------------------- CSUP -- 
   ///@defgroup CSUP_doc 
   ///@class CSUP 
   ///@brief is a super layer with few layers inside 
   class CSUP : public AgBlock 
   {  public: 
      CSUP() : AgBlock("CSUP","is a super layer with few layers inside"){ }; 
      ~CSUP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CSUP,1); 
   }; 
   // ---------------------------------------------------------------------- CPBP -- 
   ///@defgroup CPBP_doc 
   ///@class CPBP 
   ///@brief is a block that an author failed to adequately document 
   class CPBP : public AgBlock 
   {  public: 
      CPBP() : AgBlock("CPBP","is a block that an author failed to adequately document"){ }; 
      ~CPBP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CPBP,1); 
   }; 
   // ---------------------------------------------------------------------- CSCI -- 
   ///@defgroup CSCI_doc 
   ///@class CSCI 
   ///@brief a scintillator layer. 
   class CSCI : public AgBlock 
   {  public: 
      CSCI() : AgBlock("CSCI","a scintillator layer."){ }; 
      ~CSCI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CSCI,1); 
   }; 
   // ---------------------------------------------------------------------- CBTW -- 
   ///@defgroup CBTW_doc 
   ///@class CBTW 
   ///@brief is the Module Front Back Plate 
   class CBTW : public AgBlock 
   {  public: 
      CBTW() : AgBlock("CBTW","is the Module Front Back Plate"){ }; 
      ~CBTW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CBTW,1); 
   }; 
   // ---------------------------------------------------------------------- CSMD -- 
   ///@defgroup CSMD_doc 
   ///@class CSMD 
   ///@brief is the shower maximum detector envelope 
   class CSMD : public AgBlock 
   {  public: 
      CSMD() : AgBlock("CSMD","is the shower maximum detector envelope"){ }; 
      ~CSMD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CSMD,1); 
   }; 
   // ---------------------------------------------------------------------- CSMG -- 
   ///@defgroup CSMG_doc 
   ///@class CSMG 
   ///@brief is G10 front back plate 
   class CSMG : public AgBlock 
   {  public: 
      CSMG() : AgBlock("CSMG","is G10 front back plate"){ }; 
      ~CSMG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CSMG,1); 
   }; 
   // ---------------------------------------------------------------------- CSDA -- 
   ///@defgroup CSDA_doc 
   ///@class CSDA 
   ///@brief is Al block with sensitive gas volume 
   class CSDA : public AgBlock 
   {  public: 
      CSDA() : AgBlock("CSDA","is Al block with sensitive gas volume"){ }; 
      ~CSDA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CSDA,1); 
   }; 
   // ---------------------------------------------------------------------- CSMC -- 
   ///@defgroup CSMC_doc 
   ///@class CSMC 
   ///@brief is the front first (last) Al rib 
   class CSMC : public AgBlock 
   {  public: 
      CSMC() : AgBlock("CSMC","is the front first (last) Al rib"){ }; 
      ~CSMC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CSMC,1); 
   }; 
   // ---------------------------------------------------------------------- CSMB -- 
   ///@defgroup CSMB_doc 
   ///@class CSMB 
   ///@brief is the back first (last) Al rib 
   class CSMB : public AgBlock 
   {  public: 
      CSMB() : AgBlock("CSMB","is the back first (last) Al rib"){ }; 
      ~CSMB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CSMB,1); 
   }; 
   // ---------------------------------------------------------------------- CSME -- 
   ///@defgroup CSME_doc 
   ///@class CSME 
   ///@brief is the part of CSDA Al box with Ar/CO2 sensiteve gas 
   class CSME : public AgBlock 
   {  public: 
      CSME() : AgBlock("CSME","is the part of CSDA Al box with Ar/CO2 sensiteve gas"){ }; 
      ~CSME(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CSME,1); 
   }; 
   // ---------------------------------------------------------------------- CSHI -- 
   ///@defgroup CSHI_doc 
   ///@class CSHI 
   ///@brief is a sensiteve Ar/CO2 box 
   class CSHI : public AgBlock 
   {  public: 
      CSHI() : AgBlock("CSHI","is a sensiteve Ar/CO2 box"){ }; 
      ~CSHI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CSHI,1); 
   }; 
   /// \class CalbGeo1 
   /// \brief  is the geometry of the Barrel EM Calorimeter  
   class CalbGeo1 : public AgModule 
   { 
      public: 
      CalbGeo1(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~CalbGeo1(){ }; 
      ClassDef(CalbGeo1,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace CalbGeo1 
#endif // __CalbGeo1__ 
