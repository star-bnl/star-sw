#ifndef __TutrGeo1__ 
#define __TutrGeo1__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace TUTRGEO1 // $NMSPC 
{ 
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
   /// \class TutrGeo1 
   /// \brief Tutorial Geometry 1 
   class TutrGeo1 : public AgModule 
   { 
      public: 
      TutrGeo1(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~TutrGeo1(){ }; 
      ClassDef(TutrGeo1,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace TutrGeo1 
#endif // __TutrGeo1__ 
