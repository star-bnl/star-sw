#ifndef __CaveGeo__ 
#define __CaveGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace CAVEGEO // $NMSPC 
{ 
   class Cvcf_t : public AgStructure 
   { 
      ClassDef(Cvcf_t,1); 
      public: 
      Float_t version; 
      Int_t config; 
      Cvcf_t() : AgStructure("Cvcf_t","User-defined AgML structure") 
      { 
         version=0; 
         config=0; 
         _index=0; 
      } 
      ~ Cvcf_t(){ /* nada */ }; 
   }; 
   class Cave_t : public AgStructure 
   { 
      ClassDef(Cave_t,1); 
      public: 
      Float_t version; 
      Float_t rmin; 
      Array_t<Float_t> rmax; 
      Array_t<Float_t> dz; 
      Float_t dconc; 
      Cave_t() : AgStructure("Cave_t","User-defined AgML structure") 
      { 
         version=0; 
         rmin=0; 
         rmax = Array_t<Float_t>(2); 
         dz = Array_t<Float_t>(2); 
         dconc=0; 
         _index=0; 
      } 
      ~ Cave_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- HALL -- 
   ///@defgroup HALL_doc 
   ///@class HALL 
   ///@brief is GSTAR building 
   class HALL : public AgBlock 
   {  public: 
      HALL() : AgBlock("HALL","is GSTAR building"){ }; 
      ~HALL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(HALL,1); 
   }; 
   // ---------------------------------------------------------------------- CAVE -- 
   ///@defgroup CAVE_doc 
   ///@class CAVE 
   ///@brief is GSTAR cave with subsystem envelopes 
   class CAVE : public AgBlock 
   {  public: 
      CAVE() : AgBlock("CAVE","is GSTAR cave with subsystem envelopes"){ }; 
      ~CAVE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(CAVE,1); 
   }; 
   /// \class CaveGeo 
   /// \brief   builds CAVE for GSTAR  
   class CaveGeo : public AgModule 
   { 
      public: 
      CaveGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~CaveGeo(){ }; 
      ClassDef(CaveGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace CaveGeo 
#endif // __CaveGeo__ 
