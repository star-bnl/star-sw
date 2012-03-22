#ifndef __MutdGeo4__ 
#define __MutdGeo4__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace MUTDGEO4 // $NMSPC 
{ 
   class Mtdg_t : public AgStructure 
   { 
      ClassDef(Mtdg_t,1); 
      public: 
      Float_t version; 
      Int_t config; 
      Array_t<Float_t> blconfig; 
      Mtdg_t() : AgStructure("Mtdg_t","User-defined AgML structure") 
      { 
         version=0; 
         config=0; 
         blconfig = Array_t<Float_t>(30); 
         _index=0; 
      } 
      ~ Mtdg_t(){ /* nada */ }; 
   }; 
   class Mtdd_t : public AgStructure 
   { 
      ClassDef(Mtdd_t,1); 
      public: 
      Float_t rmin; 
      Float_t rmax; 
      Float_t dzmother; 
      Float_t backlegr; 
      Float_t bemcelectboxdx; 
      Float_t bemcelectboxdy; 
      Float_t bemcelectboxdz3; 
      Float_t bemcelectboxdz5; 
      Float_t rgap; 
      Float_t mtdmotherdx; 
      Float_t mtdmotherdy; 
      Float_t mtdmotherdz3; 
      Float_t mtdmotherdz5; 
      Float_t mtdtrayz1; 
      Float_t mtdtrayz2; 
      Float_t mtdtraydx; 
      Float_t mtdtraydy; 
      Float_t mtdtraydz; 
      Float_t mtdchannel; 
      Float_t mtdbplatedx; 
      Float_t mtdtplatedx; 
      Float_t mtdigstackdx; 
      Float_t mtdigstackdy; 
      Float_t mtdigstackdz; 
      Float_t mtdigngap; 
      Float_t mtdigglassdx; 
      Float_t mtdiggasgapdx; 
      Float_t mtdogglassdx; 
      Float_t mtdogglassdy; 
      Float_t mtdogglassdz; 
      Float_t mtdpcbdx; 
      Float_t mtdpcbdy; 
      Float_t mtdpcbdz; 
      Float_t mtdnomexdx; 
      Float_t mtdnomexdy; 
      Float_t mtdnomexdz; 
      Mtdd_t() : AgStructure("Mtdd_t","User-defined AgML structure") 
      { 
         rmin=0; 
         rmax=0; 
         dzmother=0; 
         backlegr=0; 
         bemcelectboxdx=0; 
         bemcelectboxdy=0; 
         bemcelectboxdz3=0; 
         bemcelectboxdz5=0; 
         rgap=0; 
         mtdmotherdx=0; 
         mtdmotherdy=0; 
         mtdmotherdz3=0; 
         mtdmotherdz5=0; 
         mtdtrayz1=0; 
         mtdtrayz2=0; 
         mtdtraydx=0; 
         mtdtraydy=0; 
         mtdtraydz=0; 
         mtdchannel=0; 
         mtdbplatedx=0; 
         mtdtplatedx=0; 
         mtdigstackdx=0; 
         mtdigstackdy=0; 
         mtdigstackdz=0; 
         mtdigngap=0; 
         mtdigglassdx=0; 
         mtdiggasgapdx=0; 
         mtdogglassdx=0; 
         mtdogglassdy=0; 
         mtdogglassdz=0; 
         mtdpcbdx=0; 
         mtdpcbdy=0; 
         mtdpcbdz=0; 
         mtdnomexdx=0; 
         mtdnomexdy=0; 
         mtdnomexdz=0; 
         _index=0; 
      } 
      ~ Mtdd_t(){ /* nada */ }; 
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
   // ---------------------------------------------------------------------- MTMF -- 
   ///@defgroup MTMF_doc 
   ///@class MTMF 
   ///@brief is the backleg mother that encloses five trays 
   class MTMF : public AgBlock 
   {  public: 
      MTMF() : AgBlock("MTMF","is the backleg mother that encloses five trays"){ }; 
      ~MTMF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MTMF,1); 
   }; 
   // ---------------------------------------------------------------------- MMBL -- 
   ///@defgroup MMBL_doc 
   ///@class MMBL 
   ///@brief is the MTD11 group mother 
   class MMBL : public AgBlock 
   {  public: 
      MMBL() : AgBlock("MMBL","is the MTD11 group mother"){ }; 
      ~MMBL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MMBL,1); 
   }; 
   // ---------------------------------------------------------------------- MTRF -- 
   ///@defgroup MTRF_doc 
   ///@class MTRF 
   ///@brief is an MTD11-style tray 
   class MTRF : public AgBlock 
   {  public: 
      MTRF() : AgBlock("MTRF","is an MTD11-style tray"){ }; 
      ~MTRF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MTRF,1); 
   }; 
   // ---------------------------------------------------------------------- MIGF -- 
   ///@defgroup MIGF_doc 
   ///@class MIGF 
   ///@brief is the inner glass stack 
   class MIGF : public AgBlock 
   {  public: 
      MIGF() : AgBlock("MIGF","is the inner glass stack"){ }; 
      ~MIGF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MIGF,1); 
   }; 
   // ---------------------------------------------------------------------- MGAP -- 
   ///@defgroup MGAP_doc 
   ///@class MGAP 
   ///@brief is a gas gap 
   class MGAP : public AgBlock 
   {  public: 
      MGAP() : AgBlock("MGAP","is a gas gap"){ }; 
      ~MGAP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MGAP,1); 
   }; 
   // ---------------------------------------------------------------------- MTLB -- 
   ///@defgroup MTLB_doc 
   ///@class MTLB 
   ///@brief is the longer bemc electronics box 
   class MTLB : public AgBlock 
   {  public: 
      MTLB() : AgBlock("MTLB","is the longer bemc electronics box"){ }; 
      ~MTLB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MTLB,1); 
   }; 
   // ---------------------------------------------------------------------- MTSB -- 
   ///@defgroup MTSB_doc 
   ///@class MTSB 
   ///@brief is the shorter bemc electronics box 
   class MTSB : public AgBlock 
   {  public: 
      MTSB() : AgBlock("MTSB","is the shorter bemc electronics box"){ }; 
      ~MTSB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MTSB,1); 
   }; 
   // ---------------------------------------------------------------------- MTBP -- 
   ///@defgroup MTBP_doc 
   ///@class MTBP 
   ///@brief is the MTD11 bottom plate 
   class MTBP : public AgBlock 
   {  public: 
      MTBP() : AgBlock("MTBP","is the MTD11 bottom plate"){ }; 
      ~MTBP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MTBP,1); 
   }; 
   // ---------------------------------------------------------------------- MTTP -- 
   ///@defgroup MTTP_doc 
   ///@class MTTP 
   ///@brief is the MTD11 top plate 
   class MTTP : public AgBlock 
   {  public: 
      MTTP() : AgBlock("MTTP","is the MTD11 top plate"){ }; 
      ~MTTP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MTTP,1); 
   }; 
   // ---------------------------------------------------------------------- MLCH -- 
   ///@defgroup MLCH_doc 
   ///@class MLCH 
   ///@brief is the MTD11 architectural Channel long side 
   class MLCH : public AgBlock 
   {  public: 
      MLCH() : AgBlock("MLCH","is the MTD11 architectural Channel long side"){ }; 
      ~MLCH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MLCH,1); 
   }; 
   // ---------------------------------------------------------------------- MLAI -- 
   ///@defgroup MLAI_doc 
   ///@class MLAI 
   ///@brief is the air in the MTD11 architectural Channel long side 
   class MLAI : public AgBlock 
   {  public: 
      MLAI() : AgBlock("MLAI","is the air in the MTD11 architectural Channel long side"){ }; 
      ~MLAI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MLAI,1); 
   }; 
   // ---------------------------------------------------------------------- MSCH -- 
   ///@defgroup MSCH_doc 
   ///@class MSCH 
   ///@brief is the MTD11 architectural Channel short side 
   class MSCH : public AgBlock 
   {  public: 
      MSCH() : AgBlock("MSCH","is the MTD11 architectural Channel short side"){ }; 
      ~MSCH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MSCH,1); 
   }; 
   // ---------------------------------------------------------------------- MSAI -- 
   ///@defgroup MSAI_doc 
   ///@class MSAI 
   ///@brief is the air in the MTD11 architectural MtdChannel short side 
   class MSAI : public AgBlock 
   {  public: 
      MSAI() : AgBlock("MSAI","is the air in the MTD11 architectural MtdChannel short side"){ }; 
      ~MSAI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MSAI,1); 
   }; 
   // ---------------------------------------------------------------------- MOGL -- 
   ///@defgroup MOGL_doc 
   ///@class MOGL 
   ///@brief is the outer glass layers 
   class MOGL : public AgBlock 
   {  public: 
      MOGL() : AgBlock("MOGL","is the outer glass layers"){ }; 
      ~MOGL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MOGL,1); 
   }; 
   // ---------------------------------------------------------------------- MPCB -- 
   ///@defgroup MPCB_doc 
   ///@class MPCB 
   ///@brief is the printed circuit boards (readout pads) 
   class MPCB : public AgBlock 
   {  public: 
      MPCB() : AgBlock("MPCB","is the printed circuit boards (readout pads)"){ }; 
      ~MPCB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MPCB,1); 
   }; 
   // ---------------------------------------------------------------------- MNOM -- 
   ///@defgroup MNOM_doc 
   ///@class MNOM 
   ///@brief is the nomex layers 
   class MNOM : public AgBlock 
   {  public: 
      MNOM() : AgBlock("MNOM","is the nomex layers"){ }; 
      ~MNOM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MNOM,1); 
   }; 
   /// \class MutdGeo4 
   /// \brief  is the geometry of the STAR MTD, WMRPC Version  
   class MutdGeo4 : public AgModule 
   { 
      public: 
      MutdGeo4(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~MutdGeo4(){ }; 
      ClassDef(MutdGeo4,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace MutdGeo4 
#endif // __MutdGeo4__ 
