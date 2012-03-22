#ifndef __UpstGeo__ 
#define __UpstGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace UPSTGEO // $NMSPC 
{ 
   class Pipu_t : public AgStructure 
   { 
      ClassDef(Pipu_t,1); 
      public: 
      Float_t version; 
      Float_t zposit; 
      Float_t dz_upst; 
      Float_t p1innr; 
      Float_t p1outr; 
      Float_t p1leng; 
      Float_t p2innr; 
      Float_t p2outr; 
      Float_t p2leng; 
      Float_t p3innr; 
      Float_t p3outr; 
      Float_t p3leng; 
      Float_t dxinnr; 
      Float_t dxoutr; 
      Float_t dxleng; 
      Float_t csinnr; 
      Float_t csoutr; 
      Float_t ceinnr; 
      Float_t ceoutr; 
      Float_t cleng; 
      Float_t pginnr; 
      Float_t pgoutr; 
      Float_t pgleng; 
      Pipu_t() : AgStructure("Pipu_t","User-defined AgML structure") 
      { 
         version=0; 
         zposit=0; 
         dz_upst=0; 
         p1innr=0; 
         p1outr=0; 
         p1leng=0; 
         p2innr=0; 
         p2outr=0; 
         p2leng=0; 
         p3innr=0; 
         p3outr=0; 
         p3leng=0; 
         dxinnr=0; 
         dxoutr=0; 
         dxleng=0; 
         csinnr=0; 
         csoutr=0; 
         ceinnr=0; 
         ceoutr=0; 
         cleng=0; 
         pginnr=0; 
         pgoutr=0; 
         pgleng=0; 
         _index=0; 
      } 
      ~ Pipu_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- UPST -- 
   ///@defgroup UPST_doc 
   ///@class UPST 
   ///@brief is the upstream mother volume in the STAR cave 
   class UPST : public AgBlock 
   {  public: 
      UPST() : AgBlock("UPST","is the upstream mother volume in the STAR cave"){ }; 
      ~UPST(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(UPST,1); 
   }; 
   // ---------------------------------------------------------------------- PUPD -- 
   ///@defgroup PUPD_doc 
   ///@class PUPD 
   ///@brief is the Beam PIPe before the DX magnet 
   class PUPD : public AgBlock 
   {  public: 
      PUPD() : AgBlock("PUPD","is the Beam PIPe before the DX magnet"){ }; 
      ~PUPD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PUPD,1); 
   }; 
   // ---------------------------------------------------------------------- PVAD -- 
   ///@defgroup PVAD_doc 
   ///@class PVAD 
   ///@brief is the Vacuum Volume of the PIPe before the DX magnet 
   class PVAD : public AgBlock 
   {  public: 
      PVAD() : AgBlock("PVAD","is the Vacuum Volume of the PIPe before the DX magnet"){ }; 
      ~PVAD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PVAD,1); 
   }; 
   // ---------------------------------------------------------------------- PUPE -- 
   ///@defgroup PUPE_doc 
   ///@class PUPE 
   ///@brief is the Beam PIPe through the DX mAgnet Volume 
   class PUPE : public AgBlock 
   {  public: 
      PUPE() : AgBlock("PUPE","is the Beam PIPe through the DX mAgnet Volume"){ }; 
      ~PUPE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PUPE,1); 
   }; 
   // ---------------------------------------------------------------------- PVAE -- 
   ///@defgroup PVAE_doc 
   ///@class PVAE 
   ///@brief is the Vacuum Volume of DX mAgnet pipe 
   class PVAE : public AgBlock 
   {  public: 
      PVAE() : AgBlock("PVAE","is the Vacuum Volume of DX mAgnet pipe"){ }; 
      ~PVAE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PVAE,1); 
   }; 
   // ---------------------------------------------------------------------- PUPF -- 
   ///@defgroup PUPF_doc 
   ///@class PUPF 
   ///@brief is the Outer PIPe through the DX mAgnet Volume 
   class PUPF : public AgBlock 
   {  public: 
      PUPF() : AgBlock("PUPF","is the Outer PIPe through the DX mAgnet Volume"){ }; 
      ~PUPF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PUPF,1); 
   }; 
   // ---------------------------------------------------------------------- DXMG -- 
   ///@defgroup DXMG_doc 
   ///@class DXMG 
   ///@brief is the return yoke for the DX mAgnet 
   class DXMG : public AgBlock 
   {  public: 
      DXMG() : AgBlock("DXMG","is the return yoke for the DX mAgnet"){ }; 
      ~DXMG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(DXMG,1); 
   }; 
   // ---------------------------------------------------------------------- DCON -- 
   ///@defgroup DCON_doc 
   ///@class DCON 
   ///@brief is the beam pipe Bell section at the end of DX 
   class DCON : public AgBlock 
   {  public: 
      DCON() : AgBlock("DCON","is the beam pipe Bell section at the end of DX"){ }; 
      ~DCON(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(DCON,1); 
   }; 
   // ---------------------------------------------------------------------- DVAC -- 
   ///@defgroup DVAC_doc 
   ///@class DVAC 
   ///@brief is its cavity 
   class DVAC : public AgBlock 
   {  public: 
      DVAC() : AgBlock("DVAC","is its cavity"){ }; 
      ~DVAC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(DVAC,1); 
   }; 
   // ---------------------------------------------------------------------- PUPG -- 
   ///@defgroup PUPG_doc 
   ///@class PUPG 
   ///@brief is the Beam PIPe After the DX magnet Volume 
   class PUPG : public AgBlock 
   {  public: 
      PUPG() : AgBlock("PUPG","is the Beam PIPe After the DX magnet Volume"){ }; 
      ~PUPG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PUPG,1); 
   }; 
   // ---------------------------------------------------------------------- PVAG -- 
   ///@defgroup PVAG_doc 
   ///@class PVAG 
   ///@brief is the Vacuum Volume of the pipe after the DX magnet 
   class PVAG : public AgBlock 
   {  public: 
      PVAG() : AgBlock("PVAG","is the Vacuum Volume of the pipe after the DX magnet"){ }; 
      ~PVAG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PVAG,1); 
   }; 
   /// \class UpstGeo 
   /// \brief  is the geometry  of the UPSTREAM AreA.  
   class UpstGeo : public AgModule 
   { 
      public: 
      UpstGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~UpstGeo(){ }; 
      ClassDef(UpstGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace UpstGeo 
#endif // __UpstGeo__ 
