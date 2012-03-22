#ifndef __PipeGeo1__ 
#define __PipeGeo1__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace PIPEGEO1 // $NMSPC 
{ 
   class Pipv_t : public AgStructure 
   { 
      ClassDef(Pipv_t,1); 
      public: 
      Float_t version; 
      Int_t config; 
      Pipv_t() : AgStructure("Pipv_t","User-defined AgML structure") 
      { 
         version=0; 
         config=0; 
         _index=0; 
      } 
      ~ Pipv_t(){ /* nada */ }; 
   }; 
   class Pipg_t : public AgStructure 
   { 
      ClassDef(Pipg_t,1); 
      public: 
      Float_t version; 
      Float_t zoffset; 
      Float_t yoffset; 
      Float_t xoffset; 
      Float_t zoffber; 
      Float_t vacdens; 
      Pipg_t() : AgStructure("Pipg_t","User-defined AgML structure") 
      { 
         version=0; 
         zoffset=0; 
         yoffset=0; 
         xoffset=0; 
         zoffber=0; 
         vacdens=0; 
         _index=0; 
      } 
      ~ Pipg_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- PIPE -- 
   ///@defgroup PIPE_doc 
   ///@class PIPE 
   ///@brief Pipe mother volume 
   class PIPE : public AgBlock 
   {  public: 
      PIPE() : AgBlock("PIPE","Pipe mother volume"){ }; 
      ~PIPE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PIPE,1); 
   }; 
   // ---------------------------------------------------------------------- PALS -- 
   ///@defgroup PALS_doc 
   ///@class PALS 
   ///@brief The aluminium section of the beam pipe 
   class PALS : public AgBlock 
   {  public: 
      PALS() : AgBlock("PALS","The aluminium section of the beam pipe"){ }; 
      ~PALS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PALS,1); 
   }; 
   // ---------------------------------------------------------------------- PALH -- 
   ///@defgroup PALH_doc 
   ///@class PALH 
   ///@brief The hole in the AL beam pipe 
   class PALH : public AgBlock 
   {  public: 
      PALH() : AgBlock("PALH","The hole in the AL beam pipe"){ }; 
      ~PALH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PALH,1); 
   }; 
   // ---------------------------------------------------------------------- PBES -- 
   ///@defgroup PBES_doc 
   ///@class PBES 
   ///@brief The berillium section of the beam pipe 
   class PBES : public AgBlock 
   {  public: 
      PBES() : AgBlock("PBES","The berillium section of the beam pipe"){ }; 
      ~PBES(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PBES,1); 
   }; 
   /// \class PipeGeo1 
   /// \brief Beam pipe in y2013 and beyond 
   class PipeGeo1 : public AgModule 
   { 
      public: 
      PipeGeo1(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~PipeGeo1(){ }; 
      ClassDef(PipeGeo1,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace PipeGeo1 
#endif // __PipeGeo1__ 
