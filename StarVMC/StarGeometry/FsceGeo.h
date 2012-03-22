#ifndef __FsceGeo__ 
#define __FsceGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace FSCEGEO // $NMSPC 
{ 
   class Fscp_t : public AgStructure 
   { 
      ClassDef(Fscp_t,1); 
      public: 
      Float_t version; 
      Float_t towerwidth; 
      Float_t towerlength; 
      Float_t ntowersx; 
      Float_t ntowersy; 
      Float_t nempty; 
      Float_t distfromvtx; 
      Fscp_t() : AgStructure("Fscp_t","User-defined AgML structure") 
      { 
         version=0; 
         towerwidth=0; 
         towerlength=0; 
         ntowersx=0; 
         ntowersy=0; 
         nempty=0; 
         distfromvtx=0; 
         _index=0; 
      } 
      ~ Fscp_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- FSCE -- 
   ///@defgroup FSCE_doc 
   ///@class FSCE 
   ///@brief is the container volume for all towers 
   class FSCE : public AgBlock 
   {  public: 
      FSCE() : AgBlock("FSCE","is the container volume for all towers"){ }; 
      ~FSCE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSCE,1); 
   }; 
   // ---------------------------------------------------------------------- FSCT -- 
   ///@defgroup FSCT_doc 
   ///@class FSCT 
   ///@brief is a sensitive Tungsten+Sci+Epoxy tower 
   class FSCT : public AgBlock 
   {  public: 
      FSCT() : AgBlock("FSCT","is a sensitive Tungsten+Sci+Epoxy tower"){ }; 
      ~FSCT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSCT,1); 
   }; 
   /// \class FsceGeo 
   /// \brief  is the geometry of the Fiber Sampling Calorimeter  
   class FsceGeo : public AgModule 
   { 
      public: 
      FsceGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~FsceGeo(){ }; 
      ClassDef(FsceGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace FsceGeo 
#endif // __FsceGeo__ 
