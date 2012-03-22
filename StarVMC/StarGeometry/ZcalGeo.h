#ifndef __ZcalGeo__ 
#define __ZcalGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace ZCALGEO // $NMSPC 
{ 
   class Calp_t : public AgStructure 
   { 
      ClassDef(Calp_t,1); 
      public: 
      Float_t version; 
      Float_t droutr; 
      Float_t drdz; 
      Float_t phleng; 
      Float_t phinnr; 
      Float_t phoutr; 
      Float_t pltdz; 
      Float_t pltoutr; 
      Float_t houtr; 
      Float_t pjinnr; 
      Float_t pjoutr; 
      Float_t pjleng; 
      Float_t qcdx; 
      Float_t qcdy; 
      Float_t qcdz; 
      Float_t scdz; 
      Float_t sdiv; 
      Calp_t() : AgStructure("Calp_t","User-defined AgML structure") 
      { 
         version=0; 
         droutr=0; 
         drdz=0; 
         phleng=0; 
         phinnr=0; 
         phoutr=0; 
         pltdz=0; 
         pltoutr=0; 
         houtr=0; 
         pjinnr=0; 
         pjoutr=0; 
         pjleng=0; 
         qcdx=0; 
         qcdy=0; 
         qcdz=0; 
         scdz=0; 
         sdiv=0; 
         _index=0; 
      } 
      ~ Calp_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- ZCAL -- 
   ///@defgroup ZCAL_doc 
   ///@class ZCAL 
   ///@brief is the region between the DX and the D0 magnets 
   class ZCAL : public AgBlock 
   {  public: 
      ZCAL() : AgBlock("ZCAL","is the region between the DX and the D0 magnets"){ }; 
      ~ZCAL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ZCAL,1); 
   }; 
   // ---------------------------------------------------------------------- PIPH -- 
   ///@defgroup PIPH_doc 
   ///@class PIPH 
   ///@brief is the Large diameter Pipe before the beam pipes split 
   class PIPH : public AgBlock 
   {  public: 
      PIPH() : AgBlock("PIPH","is the Large diameter Pipe before the beam pipes split"){ }; 
      ~PIPH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PIPH,1); 
   }; 
   // ---------------------------------------------------------------------- PVAH -- 
   ///@defgroup PVAH_doc 
   ///@class PVAH 
   ///@brief is the Vacuum Volume of the large diameter pipe 
   class PVAH : public AgBlock 
   {  public: 
      PVAH() : AgBlock("PVAH","is the Vacuum Volume of the large diameter pipe"){ }; 
      ~PVAH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PVAH,1); 
   }; 
   // ---------------------------------------------------------------------- PLAT -- 
   ///@defgroup PLAT_doc 
   ///@class PLAT 
   ///@brief is the End Plate of the large dia. Pipe 
   class PLAT : public AgBlock 
   {  public: 
      PLAT() : AgBlock("PLAT","is the End Plate of the large dia. Pipe"){ }; 
      ~PLAT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PLAT,1); 
   }; 
   // ---------------------------------------------------------------------- PLVA -- 
   ///@defgroup PLVA_doc 
   ///@class PLVA 
   ///@brief is the Vacuum Volume of the beam pipe holes in the end plate 
   class PLVA : public AgBlock 
   {  public: 
      PLVA() : AgBlock("PLVA","is the Vacuum Volume of the beam pipe holes in the end plate"){ }; 
      ~PLVA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PLVA,1); 
   }; 
   // ---------------------------------------------------------------------- PIPJ -- 
   ///@defgroup PIPJ_doc 
   ///@class PIPJ 
   ///@brief are the final beam Pipes 
   class PIPJ : public AgBlock 
   {  public: 
      PIPJ() : AgBlock("PIPJ","are the final beam Pipes"){ }; 
      ~PIPJ(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PIPJ,1); 
   }; 
   // ---------------------------------------------------------------------- PVAJ -- 
   ///@defgroup PVAJ_doc 
   ///@class PVAJ 
   ///@brief is the Vacuum Volume of the final beam pipes 
   class PVAJ : public AgBlock 
   {  public: 
      PVAJ() : AgBlock("PVAJ","is the Vacuum Volume of the final beam pipes"){ }; 
      ~PVAJ(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PVAJ,1); 
   }; 
   // ---------------------------------------------------------------------- QCAL -- 
   ///@defgroup QCAL_doc 
   ///@class QCAL 
   ///@brief is the Zero degree calorimeter 
   class QCAL : public AgBlock 
   {  public: 
      QCAL() : AgBlock("QCAL","is the Zero degree calorimeter"){ }; 
      ~QCAL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(QCAL,1); 
   }; 
   // ---------------------------------------------------------------------- QDIV -- 
   ///@defgroup QDIV_doc 
   ///@class QDIV 
   ///@brief is one section/layer of the Quartz Calorimeter 
   class QDIV : public AgBlock 
   {  public: 
      QDIV() : AgBlock("QDIV","is one section/layer of the Quartz Calorimeter"){ }; 
      ~QDIV(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(QDIV,1); 
   }; 
   // ---------------------------------------------------------------------- QSCI -- 
   ///@defgroup QSCI_doc 
   ///@class QSCI 
   ///@brief is a sensitive Fiber layer 
   class QSCI : public AgBlock 
   {  public: 
      QSCI() : AgBlock("QSCI","is a sensitive Fiber layer"){ }; 
      ~QSCI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(QSCI,1); 
   }; 
   /// \class ZcalGeo 
   /// \brief  is the geometry of the Zero deg. Quartz Calorimeter  
   class ZcalGeo : public AgModule 
   { 
      public: 
      ZcalGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~ZcalGeo(){ }; 
      ClassDef(ZcalGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace ZcalGeo 
#endif // __ZcalGeo__ 
