#ifndef __TutrGeo4__ 
#define __TutrGeo4__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace TUTRGEO4 // $NMSPC 
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
   // ---------------------------------------------------------------------- APCN -- 
   ///@defgroup APCN_doc 
   ///@class APCN 
   ///@brief A polycone 
   class APCN : public AgBlock 
   {  public: 
      APCN() : AgBlock("APCN","A polycone"){ }; 
      ~APCN(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(APCN,1); 
   }; 
   // ---------------------------------------------------------------------- APGN -- 
   ///@defgroup APGN_doc 
   ///@class APGN 
   ///@brief A polygon 
   class APGN : public AgBlock 
   {  public: 
      APGN() : AgBlock("APGN","A polygon"){ }; 
      ~APGN(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(APGN,1); 
   }; 
   // ---------------------------------------------------------------------- APAR -- 
   ///@defgroup APAR_doc 
   ///@class APAR 
   ///@brief A parallelpiped 
   class APAR : public AgBlock 
   {  public: 
      APAR() : AgBlock("APAR","A parallelpiped"){ }; 
      ~APAR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(APAR,1); 
   }; 
   // ---------------------------------------------------------------------- AELT -- 
   ///@defgroup AELT_doc 
   ///@class AELT 
   ///@brief An elliptical tube 
   class AELT : public AgBlock 
   {  public: 
      AELT() : AgBlock("AELT","An elliptical tube"){ }; 
      ~AELT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(AELT,1); 
   }; 
   /// \class TutrGeo4 
   /// \brief Tutorial Geometry 1 
   class TutrGeo4 : public AgModule 
   { 
      public: 
      TutrGeo4(); 
      virtual void ConstructGeometry(); 
      ~TutrGeo4(){ }; 
      ClassDef(TutrGeo4,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace TutrGeo4 
#endif // __TutrGeo4__ 
