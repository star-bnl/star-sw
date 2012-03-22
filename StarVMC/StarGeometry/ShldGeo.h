#ifndef __ShldGeo__ 
#define __ShldGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace SHLDGEO // $NMSPC 
{ 
   class Shlg_t : public AgStructure 
   { 
      ClassDef(Shlg_t,1); 
      public: 
      Float_t version; 
      Float_t z; 
      Float_t dx; 
      Float_t dy; 
      Float_t dz; 
      Float_t baselevel; 
      Float_t basez; 
      Float_t basedx; 
      Float_t basedy; 
      Float_t slabx; 
      Float_t slabz; 
      Float_t slabdy; 
      Float_t slabdz; 
      Float_t fidz; 
      Float_t fidy; 
      Float_t holex; 
      Float_t holey; 
      Float_t floorthk; 
      Float_t floorlen; 
      Float_t floorwidth; 
      Float_t floorpos; 
      Shlg_t() : AgStructure("Shlg_t","User-defined AgML structure") 
      { 
         version=0; 
         z=0; 
         dx=0; 
         dy=0; 
         dz=0; 
         baselevel=0; 
         basez=0; 
         basedx=0; 
         basedy=0; 
         slabx=0; 
         slabz=0; 
         slabdy=0; 
         slabdz=0; 
         fidz=0; 
         fidy=0; 
         holex=0; 
         holey=0; 
         floorthk=0; 
         floorlen=0; 
         floorwidth=0; 
         floorpos=0; 
         _index=0; 
      } 
      ~ Shlg_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- SHLD -- 
   ///@defgroup SHLD_doc 
   ///@class SHLD 
   ///@brief is the shield mother volume in the STAR cave 
   class SHLD : public AgBlock 
   {  public: 
      SHLD() : AgBlock("SHLD","is the shield mother volume in the STAR cave"){ }; 
      ~SHLD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHLD,1); 
   }; 
   // ---------------------------------------------------------------------- SFLR -- 
   ///@defgroup SFLR_doc 
   ///@class SFLR 
   ///@brief is the floor 
   class SFLR : public AgBlock 
   {  public: 
      SFLR() : AgBlock("SFLR","is the floor"){ }; 
      ~SFLR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFLR,1); 
   }; 
   // ---------------------------------------------------------------------- SHBS -- 
   ///@defgroup SHBS_doc 
   ///@class SHBS 
   ///@brief is the shield base 
   class SHBS : public AgBlock 
   {  public: 
      SHBS() : AgBlock("SHBS","is the shield base"){ }; 
      ~SHBS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHBS,1); 
   }; 
   // ---------------------------------------------------------------------- SHLS -- 
   ///@defgroup SHLS_doc 
   ///@class SHLS 
   ///@brief is the lateral slab 
   class SHLS : public AgBlock 
   {  public: 
      SHLS() : AgBlock("SHLS","is the lateral slab"){ }; 
      ~SHLS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHLS,1); 
   }; 
   // ---------------------------------------------------------------------- SHBI -- 
   ///@defgroup SHBI_doc 
   ///@class SHBI 
   ///@brief is the back iron slab 
   class SHBI : public AgBlock 
   {  public: 
      SHBI() : AgBlock("SHBI","is the back iron slab"){ }; 
      ~SHBI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHBI,1); 
   }; 
   // ---------------------------------------------------------------------- SHFI -- 
   ///@defgroup SHFI_doc 
   ///@class SHFI 
   ///@brief is the forward iron slab 
   class SHFI : public AgBlock 
   {  public: 
      SHFI() : AgBlock("SHFI","is the forward iron slab"){ }; 
      ~SHFI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHFI,1); 
   }; 
   // ---------------------------------------------------------------------- SHOL -- 
   ///@defgroup SHOL_doc 
   ///@class SHOL 
   ///@brief is the hole in the forward iron slab 
   class SHOL : public AgBlock 
   {  public: 
      SHOL() : AgBlock("SHOL","is the hole in the forward iron slab"){ }; 
      ~SHOL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SHOL,1); 
   }; 
   /// \class ShldGeo 
   /// \brief  is the shielding  
   class ShldGeo : public AgModule 
   { 
      public: 
      ShldGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~ShldGeo(){ }; 
      ClassDef(ShldGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace ShldGeo 
#endif // __ShldGeo__ 
