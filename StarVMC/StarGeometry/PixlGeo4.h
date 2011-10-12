#ifndef __PixlGeo4__ 
#define __PixlGeo4__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace PIXLGEO4 // $NMSPC 
{ 
   class Pxld_t : public AgStructure 
   { 
      ClassDef(Pxld_t,1); 
      public: 
      Float_t version; 
      Float_t subversion; 
      Float_t totallength; 
      Float_t passivethk; 
      Float_t activethk; 
      Float_t layerthk; 
      Float_t rin; 
      Float_t rout; 
      Pxld_t() : AgStructure("Pxld_t","User-defined AgML structure") 
      { 
         version=0; 
         subversion=0; 
         totallength=0; 
         passivethk=0; 
         activethk=0; 
         layerthk=0; 
         rin=0; 
         rout=0; 
         _index=0; 
      } 
      ~ Pxld_t(){ /* nada */ }; 
   }; 
   class Pixg_t : public AgStructure 
   { 
      ClassDef(Pixg_t,1); 
      public: 
      Float_t layer; 
      Float_t noladders; 
      Float_t r; 
      Float_t a; 
      Float_t poffset; 
      Float_t aoffset; 
      Pixg_t() : AgStructure("Pixg_t","User-defined AgML structure") 
      { 
         layer=0; 
         noladders=0; 
         r=0; 
         a=0; 
         poffset=0; 
         aoffset=0; 
         _index=0; 
      } 
      ~ Pixg_t(){ /* nada */ }; 
   }; 
   class Pxbg_t : public AgStructure 
   { 
      ClassDef(Pxbg_t,1); 
      public: 
      Float_t version; 
      Float_t length; 
      Float_t rin; 
      Float_t thk; 
      Pxbg_t() : AgStructure("Pxbg_t","User-defined AgML structure") 
      { 
         version=0; 
         length=0; 
         rin=0; 
         thk=0; 
         _index=0; 
      } 
      ~ Pxbg_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- PXMO -- 
   ///@defgroup PXMO_doc 
   ///@class PXMO 
   ///@brief is the mother of the pixel detector volumes 
   class PXMO : public AgBlock 
   {  public: 
      PXMO() : AgBlock("PXMO","is the mother of the pixel detector volumes"){ }; 
      ~PXMO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXMO,1); 
   }; 
   // ---------------------------------------------------------------------- PXLA -- 
   ///@defgroup PXLA_doc 
   ///@class PXLA 
   ///@brief is the mother of a layer 
   class PXLA : public AgBlock 
   {  public: 
      PXLA() : AgBlock("PXLA","is the mother of a layer"){ }; 
      ~PXLA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXLA,1); 
   }; 
   // ---------------------------------------------------------------------- PLMI -- 
   ///@defgroup PLMI_doc 
   ///@class PLMI 
   ///@brief is the mother of a silicon ladder 
   class PLMI : public AgBlock 
   {  public: 
      PLMI() : AgBlock("PLMI","is the mother of a silicon ladder"){ }; 
      ~PLMI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PLMI,1); 
   }; 
   // ---------------------------------------------------------------------- PLPS -- 
   ///@defgroup PLPS_doc 
   ///@class PLPS 
   ///@brief is the passive layer of the ladder 
   class PLPS : public AgBlock 
   {  public: 
      PLPS() : AgBlock("PLPS","is the passive layer of the ladder"){ }; 
      ~PLPS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PLPS,1); 
   }; 
   // ---------------------------------------------------------------------- PLAC -- 
   ///@defgroup PLAC_doc 
   ///@class PLAC 
   ///@brief is the active layer of the ladder 
   class PLAC : public AgBlock 
   {  public: 
      PLAC() : AgBlock("PLAC","is the active layer of the ladder"){ }; 
      ~PLAC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PLAC,1); 
   }; 
   /// \class PixlGeo4 
   /// \brief is the SIMPLIFIED pixel detector  
   class PixlGeo4 : public AgModule 
   { 
      public: 
      PixlGeo4(); 
      virtual void ConstructGeometry(); 
      ~PixlGeo4(){ }; 
      ClassDef(PixlGeo4,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace PixlGeo4 
#endif // __PixlGeo4__ 
