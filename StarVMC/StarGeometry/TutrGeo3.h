#ifndef __TutrGeo3__ 
#define __TutrGeo3__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace TUTRGEO3 // $NMSPC 
{ 
   class Tubg_t : public AgStructure 
   { 
      ClassDef(Tubg_t,1); 
      public: 
      Int_t version; 
      Float_t rmin; 
      Float_t rmax; 
      Float_t dz; 
      Tubg_t() : AgStructure("Tubg_t","User-defined AgML structure") 
      { 
         version=0; 
         rmin=0; 
         rmax=0; 
         dz=0; 
         _index=0; 
      } 
      ~ Tubg_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- TUTR -- 
   ///@defgroup TUTR_doc 
   ///@class TUTR 
   ///@brief Main volume in the AGML tutorial geometry 
   class TUTR : public AgBlock 
   {  public: 
      TUTR() : AgBlock("TUTR","Main volume in the AGML tutorial geometry"){ }; 
      ~TUTR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TUTR,1); 
   }; 
   // ---------------------------------------------------------------------- ABOX -- 
   ///@defgroup ABOX_doc 
   ///@class ABOX 
   ///@brief A volume which is a box in the tutorial 
   class ABOX : public AgBlock 
   {  public: 
      ABOX() : AgBlock("ABOX","A volume which is a box in the tutorial"){ }; 
      ~ABOX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ABOX,1); 
   }; 
   // ---------------------------------------------------------------------- ATUB -- 
   ///@defgroup ATUB_doc 
   ///@class ATUB 
   ///@brief A volume which is a tube in the tutorial 
   class ATUB : public AgBlock 
   {  public: 
      ATUB() : AgBlock("ATUB","A volume which is a tube in the tutorial"){ }; 
      ~ATUB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ATUB,1); 
   }; 
   /// \class TutrGeo3 
   /// \brief Tutorial Geometry 1 
   class TutrGeo3 : public AgModule 
   { 
      public: 
      TutrGeo3(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~TutrGeo3(){ }; 
      ClassDef(TutrGeo3,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace TutrGeo3 
#endif // __TutrGeo3__ 
