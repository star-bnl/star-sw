#ifndef __PipeGeo00__ 
#define __PipeGeo00__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace PIPEGEO00 // $NMSPC 
{ 
   class Pipv_t : public AgStructure 
   { 
      ClassDef(Pipv_t,1); 
      public: 
      Float_t version; 
      Float_t pipeconfig; 
      Pipv_t() : AgStructure("Pipv_t","User-defined AgML structure") 
      { 
         version=0; 
         pipeconfig=0; 
         _index=0; 
      } 
      ~ Pipv_t(){ /* nada */ }; 
   }; 
   class Pipg_t : public AgStructure 
   { 
      ClassDef(Pipg_t,1); 
      public: 
      Float_t config; 
      Float_t beinnr; 
      Float_t beoutr; 
      Float_t beleng; 
      Pipg_t() : AgStructure("Pipg_t","User-defined AgML structure") 
      { 
         config=0; 
         beinnr=0; 
         beoutr=0; 
         beleng=0; 
         _index=0; 
      } 
      ~ Pipg_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- PIPE -- 
   ///@defgroup PIPE_doc 
   ///@class PIPE 
   ///@brief is the STAR beam pipe mother volume 
   class PIPE : public AgBlock 
   {  public: 
      PIPE() : AgBlock("PIPE","is the STAR beam pipe mother volume"){ }; 
      ~PIPE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PIPE,1); 
   }; 
   // ---------------------------------------------------------------------- PIPC -- 
   ///@defgroup PIPC_doc 
   ///@class PIPC 
   ///@brief is the Central Beam PIPe Volume 
   class PIPC : public AgBlock 
   {  public: 
      PIPC() : AgBlock("PIPC","is the Central Beam PIPe Volume"){ }; 
      ~PIPC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PIPC,1); 
   }; 
   // ---------------------------------------------------------------------- PVAC -- 
   ///@defgroup PVAC_doc 
   ///@class PVAC 
   ///@brief is the Vacuum Volume of Be section of pipe 
   class PVAC : public AgBlock 
   {  public: 
      PVAC() : AgBlock("PVAC","is the Vacuum Volume of Be section of pipe"){ }; 
      ~PVAC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PVAC,1); 
   }; 
   /// \class PipeGeo00 
   /// \brief  is the SIMPLIFIED geometry  of the STAR beam pipe.  
   class PipeGeo00 : public AgModule 
   { 
      public: 
      PipeGeo00(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~PipeGeo00(){ }; 
      ClassDef(PipeGeo00,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace PipeGeo00 
#endif // __PipeGeo00__ 
