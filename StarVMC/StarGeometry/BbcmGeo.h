#ifndef __BbcmGeo__ 
#define __BbcmGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace BBCMGEO // $NMSPC 
{ 
   class Bbcg_t : public AgStructure 
   { 
      ClassDef(Bbcg_t,1); 
      public: 
      Float_t version; 
      Array_t<Float_t> onoff; 
      Array_t<Float_t> zdis; 
      Bbcg_t() : AgStructure("Bbcg_t","User-defined AgML structure") 
      { 
         version=0; 
         onoff = Array_t<Float_t>(3); 
         zdis = Array_t<Float_t>(2); 
         _index=0; 
      } 
      ~ Bbcg_t(){ /* nada */ }; 
   }; 
   class Hexg_t : public AgStructure 
   { 
      ClassDef(Hexg_t,1); 
      public: 
      Float_t type; 
      Float_t irad; 
      Float_t clad; 
      Float_t thick; 
      Float_t zoffset; 
      Float_t xoffset; 
      Float_t yoffset; 
      Hexg_t() : AgStructure("Hexg_t","User-defined AgML structure") 
      { 
         type=0; 
         irad=0; 
         clad=0; 
         thick=0; 
         zoffset=0; 
         xoffset=0; 
         yoffset=0; 
         _index=0; 
      } 
      ~ Hexg_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- BBCM -- 
   ///@defgroup BBCM_doc 
   ///@class BBCM 
   ///@brief is one BBC East or West module 
   class BBCM : public AgBlock 
   {  public: 
      BBCM() : AgBlock("BBCM","is one BBC East or West module"){ }; 
      ~BBCM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BBCM,1); 
   }; 
   // ---------------------------------------------------------------------- BBCA -- 
   ///@defgroup BBCA_doc 
   ///@class BBCA 
   ///@brief is one BBC Annulus module 
   class BBCA : public AgBlock 
   {  public: 
      BBCA() : AgBlock("BBCA","is one BBC Annulus module"){ }; 
      ~BBCA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BBCA,1); 
   }; 
   // ---------------------------------------------------------------------- THXM -- 
   ///@defgroup THXM_doc 
   ///@class THXM 
   ///@brief is on Triple HeXagonal Module 
   class THXM : public AgBlock 
   {  public: 
      THXM() : AgBlock("THXM","is on Triple HeXagonal Module"){ }; 
      ~THXM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(THXM,1); 
   }; 
   // ---------------------------------------------------------------------- SHXT -- 
   ///@defgroup SHXT_doc 
   ///@class SHXT 
   ///@brief is one Single HeXagonal Tile 
   class SHXT : public AgBlock 
   {  public: 
      SHXT() : AgBlock("SHXT","is one Single HeXagonal Tile"){ }; 
      ~SHXT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHXT,1); 
   }; 
   // ---------------------------------------------------------------------- CLAD -- 
   ///@defgroup CLAD_doc 
   ///@class CLAD 
   ///@brief is one CLADding of BPOL active region 
   class CLAD : public AgBlock 
   {  public: 
      CLAD() : AgBlock("CLAD","is one CLADding of BPOL active region"){ }; 
      ~CLAD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CLAD,1); 
   }; 
   // ---------------------------------------------------------------------- BPOL -- 
   ///@defgroup BPOL_doc 
   ///@class BPOL 
   ///@brief is one Bbc POLystyren active scintillator layer 
   class BPOL : public AgBlock 
   {  public: 
      BPOL() : AgBlock("BPOL","is one Bbc POLystyren active scintillator layer"){ }; 
      ~BPOL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BPOL,1); 
   }; 
   /// \class BbcmGeo 
   /// \brief  is the Beam Beam Counter Modules GEOmetry  
   class BbcmGeo : public AgModule 
   { 
      public: 
      BbcmGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~BbcmGeo(){ }; 
      ClassDef(BbcmGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace BbcmGeo 
#endif // __BbcmGeo__ 
