#ifndef __TutrGeo2__ 
#define __TutrGeo2__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace TUTRGEO2 // $NMSPC 
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
   // ---------------------------------------------------------------------- ATUB -- 
   ///@defgroup ATUB_doc 
   ///@class ATUB 
   ///@brief A TUBE with innner and outer radius 
   class ATUB : public AgBlock 
   {  public: 
      ATUB() : AgBlock("ATUB","A TUBE with innner and outer radius"){ }; 
      ~ATUB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ATUB,1); 
   }; 
   // ---------------------------------------------------------------------- ATBS -- 
   ///@defgroup ATBS_doc 
   ///@class ATBS 
   ///@brief A TUBE segment with innner and outer radius... and limits in phi 
   class ATBS : public AgBlock 
   {  public: 
      ATBS() : AgBlock("ATBS","A TUBE segment with innner and outer radius... and limits in phi"){ }; 
      ~ATBS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ATBS,1); 
   }; 
   // ---------------------------------------------------------------------- ACON -- 
   ///@defgroup ACON_doc 
   ///@class ACON 
   ///@brief A CONE with innner and outer radius 
   class ACON : public AgBlock 
   {  public: 
      ACON() : AgBlock("ACON","A CONE with innner and outer radius"){ }; 
      ~ACON(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ACON,1); 
   }; 
   // ---------------------------------------------------------------------- ACNS -- 
   ///@defgroup ACNS_doc 
   ///@class ACNS 
   ///@brief A CONE segment with innner and outer radius... and limits in phi 
   class ACNS : public AgBlock 
   {  public: 
      ACNS() : AgBlock("ACNS","A CONE segment with innner and outer radius... and limits in phi"){ }; 
      ~ACNS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ACNS,1); 
   }; 
   /// \class TutrGeo2 
   /// \brief Tutorial Geometry 1 
   class TutrGeo2 : public AgModule 
   { 
      public: 
      TutrGeo2(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~TutrGeo2(){ }; 
      ClassDef(TutrGeo2,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace TutrGeo2 
#endif // __TutrGeo2__ 
