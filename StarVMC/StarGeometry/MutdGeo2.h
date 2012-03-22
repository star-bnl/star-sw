#ifndef __MutdGeo2__ 
#define __MutdGeo2__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace MUTDGEO2 // $NMSPC 
{ 
   class Mtdg_t : public AgStructure 
   { 
      ClassDef(Mtdg_t,1); 
      public: 
      Float_t version; 
      Float_t rpmtin; 
      Float_t rpmtout; 
      Float_t rmrpcin; 
      Float_t rmrpcout; 
      Float_t rmin; 
      Float_t rmax; 
      Float_t dz; 
      Float_t length; 
      Array_t<Float_t> radii; 
      Mtdg_t() : AgStructure("Mtdg_t","User-defined AgML structure") 
      { 
         version=0; 
         rpmtin=0; 
         rpmtout=0; 
         rmrpcin=0; 
         rmrpcout=0; 
         rmin=0; 
         rmax=0; 
         dz=0; 
         length=0; 
         radii = Array_t<Float_t>(2); 
         _index=0; 
      } 
      ~ Mtdg_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- MUTD -- 
   ///@defgroup MUTD_doc 
   ///@class MUTD 
   ///@brief is the muon detector mother 
   class MUTD : public AgBlock 
   {  public: 
      MUTD() : AgBlock("MUTD","is the muon detector mother"){ }; 
      ~MUTD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MUTD,1); 
   }; 
   // ---------------------------------------------------------------------- MUSC -- 
   ///@defgroup MUSC_doc 
   ///@class MUSC 
   ///@brief is a sector of MUON Trigger Barrel Scintillators 
   class MUSC : public AgBlock 
   {  public: 
      MUSC() : AgBlock("MUSC","is a sector of MUON Trigger Barrel Scintillators"){ }; 
      ~MUSC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MUSC,1); 
   }; 
   // ---------------------------------------------------------------------- MPMT -- 
   ///@defgroup MPMT_doc 
   ///@class MPMT 
   ///@brief is a Main TRay covering box for PMT 
   class MPMT : public AgBlock 
   {  public: 
      MPMT() : AgBlock("MPMT","is a Main TRay covering box for PMT"){ }; 
      ~MPMT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MPMT,1); 
   }; 
   // ---------------------------------------------------------------------- MMRP -- 
   ///@defgroup MMRP_doc 
   ///@class MMRP 
   ///@brief is a Main TRay covering box for MRPC 
   class MMRP : public AgBlock 
   {  public: 
      MMRP() : AgBlock("MMRP","is a Main TRay covering box for MRPC"){ }; 
      ~MMRP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MMRP,1); 
   }; 
   /// \class MutdGeo2 
   /// \brief  is the geometry of the STAR muon trigger system  
   class MutdGeo2 : public AgModule 
   { 
      public: 
      MutdGeo2(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~MutdGeo2(){ }; 
      ClassDef(MutdGeo2,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace MutdGeo2 
#endif // __MutdGeo2__ 
