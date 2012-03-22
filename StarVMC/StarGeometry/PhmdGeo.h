#ifndef __PhmdGeo__ 
#define __PhmdGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace PHMDGEO // $NMSPC 
{ 
   class Pmvr_t : public AgStructure 
   { 
      ClassDef(Pmvr_t,1); 
      public: 
      Float_t version; 
      Int_t config; 
      Pmvr_t() : AgStructure("Pmvr_t","User-defined AgML structure") 
      { 
         version=0; 
         config=0; 
         _index=0; 
      } 
      ~ Pmvr_t(){ /* nada */ }; 
   }; 
   class Pmdg_t : public AgStructure 
   { 
      ClassDef(Pmdg_t,1); 
      public: 
      Float_t version; 
      Float_t m_max; 
      Float_t m_min; 
      Array_t<Float_t> zdist; 
      Float_t dpmdx; 
      Float_t dpmdy; 
      Float_t dpmdz; 
      Float_t pargcz; 
      Float_t parscz; 
      Float_t parfez; 
      Float_t parpbz; 
      Float_t cell_radius; 
      Float_t cell_depth; 
      Float_t cell_wall; 
      Float_t boundary; 
      Float_t th_base; 
      Float_t th_air; 
      Float_t th_pcb; 
      Float_t th_lead; 
      Float_t th_steel; 
      Array_t<Int_t> nx; 
      Array_t<Int_t> ny; 
      Array_t<Int_t> mx; 
      Array_t<Int_t> my; 
      Array_t<Float_t> hexd2; 
      Array_t<Float_t> hexd1; 
      Array_t<Float_t> dpara; 
      Pmdg_t() : AgStructure("Pmdg_t","User-defined AgML structure") 
      { 
         version=0; 
         m_max=0; 
         m_min=0; 
         zdist = Array_t<Float_t>(2); 
         dpmdx=0; 
         dpmdy=0; 
         dpmdz=0; 
         pargcz=0; 
         parscz=0; 
         parfez=0; 
         parpbz=0; 
         cell_radius=0; 
         cell_depth=0; 
         cell_wall=0; 
         boundary=0; 
         th_base=0; 
         th_air=0; 
         th_pcb=0; 
         th_lead=0; 
         th_steel=0; 
         nx = Array_t<Int_t>(5); 
         ny = Array_t<Int_t>(5); 
         mx = Array_t<Int_t>(7); 
         my = Array_t<Int_t>(7); 
         hexd2 = Array_t<Float_t>(10); 
         hexd1 = Array_t<Float_t>(10); 
         dpara = Array_t<Float_t>(6); 
         _index=0; 
      } 
      ~ Pmdg_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- PHMD -- 
   ///@defgroup PHMD_doc 
   ///@class PHMD 
   ///@brief the PMD box volume and fill with air 
   class PHMD : public AgBlock 
   {  public: 
      PHMD() : AgBlock("PHMD","the PMD box volume and fill with air"){ }; 
      ~PHMD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PHMD,1); 
   }; 
   // ---------------------------------------------------------------------- PHMS -- 
   ///@defgroup PHMS_doc 
   ///@class PHMS 
   ///@brief the PMD sector volume - 1/3rd of PHMD 
   class PHMS : public AgBlock 
   {  public: 
      PHMS() : AgBlock("PHMS","the PMD sector volume - 1/3rd of PHMD"){ }; 
      ~PHMS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PHMS,1); 
   }; 
   // ---------------------------------------------------------------------- PHSR -- 
   ///@defgroup PHSR_doc 
   ///@class PHSR 
   ///@brief is a detector box made in air 
   class PHSR : public AgBlock 
   {  public: 
      PHSR() : AgBlock("PHSR","is a detector box made in air"){ }; 
      ~PHSR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PHSR,1); 
   }; 
   // ---------------------------------------------------------------------- PMDA -- 
   ///@defgroup PMDA_doc 
   ///@class PMDA 
   ///@brief is a detector box made in aluminium 
   class PMDA : public AgBlock 
   {  public: 
      PMDA() : AgBlock("PMDA","is a detector box made in aluminium"){ }; 
      ~PMDA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PMDA,1); 
   }; 
   // ---------------------------------------------------------------------- AIRA -- 
   ///@defgroup AIRA_doc 
   ///@class AIRA 
   ///@brief is a detector made in air 
   class AIRA : public AgBlock 
   {  public: 
      AIRA() : AgBlock("AIRA","is a detector made in air"){ }; 
      ~AIRA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(AIRA,1); 
   }; 
   // ---------------------------------------------------------------------- PHCA -- 
   ///@defgroup PHCA_doc 
   ///@class PHCA 
   ///@brief is the detector made in air 
   class PHCA : public AgBlock 
   {  public: 
      PHCA() : AgBlock("PHCA","is the detector made in air"){ }; 
      ~PHCA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PHCA,1); 
   }; 
   // ---------------------------------------------------------------------- ASTR -- 
   ///@defgroup ASTR_doc 
   ///@class ASTR 
   ///@brief is the strip 
   class ASTR : public AgBlock 
   {  public: 
      ASTR() : AgBlock("ASTR","is the strip"){ }; 
      ~ASTR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ASTR,1); 
   }; 
   // ---------------------------------------------------------------------- PSTR -- 
   ///@defgroup PSTR_doc 
   ///@class PSTR 
   ///@brief is one pseudo-cell 
   class PSTR : public AgBlock 
   {  public: 
      PSTR() : AgBlock("PSTR","is one pseudo-cell"){ }; 
      ~PSTR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PSTR,1); 
   }; 
   // ---------------------------------------------------------------------- PPBA -- 
   ///@defgroup PPBA_doc 
   ///@class PPBA 
   ///@brief is The lead plates for different modules 
   class PPBA : public AgBlock 
   {  public: 
      PPBA() : AgBlock("PPBA","is The lead plates for different modules"){ }; 
      ~PPBA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PPBA,1); 
   }; 
   // ---------------------------------------------------------------------- PFEA -- 
   ///@defgroup PFEA_doc 
   ///@class PFEA 
   ///@brief is The iron plates for different modules 
   class PFEA : public AgBlock 
   {  public: 
      PFEA() : AgBlock("PFEA","is The iron plates for different modules"){ }; 
      ~PFEA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PFEA,1); 
   }; 
   // ---------------------------------------------------------------------- BASA -- 
   ///@defgroup BASA_doc 
   ///@class BASA 
   ///@brief is the G10 base plate 
   class BASA : public AgBlock 
   {  public: 
      BASA() : AgBlock("BASA","is the G10 base plate"){ }; 
      ~BASA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(BASA,1); 
   }; 
   // ---------------------------------------------------------------------- PCBA -- 
   ///@defgroup PCBA_doc 
   ///@class PCBA 
   ///@brief is the chamber PCB 
   class PCBA : public AgBlock 
   {  public: 
      PCBA() : AgBlock("PCBA","is the chamber PCB"){ }; 
      ~PCBA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PCBA,1); 
   }; 
   // ---------------------------------------------------------------------- PDCU -- 
   ///@defgroup PDCU_doc 
   ///@class PDCU 
   ///@brief is The outer cell in the PMD module 
   class PDCU : public AgBlock 
   {  public: 
      PDCU() : AgBlock("PDCU","is The outer cell in the PMD module"){ }; 
      ~PDCU(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PDCU,1); 
   }; 
   // ---------------------------------------------------------------------- PDGS -- 
   ///@defgroup PDGS_doc 
   ///@class PDGS 
   ///@brief is The inner cell in the PMD module 
   class PDGS : public AgBlock 
   {  public: 
      PDGS() : AgBlock("PDGS","is The inner cell in the PMD module"){ }; 
      ~PDGS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PDGS,1); 
   }; 
   /// \class PhmdGeo 
   /// \brief   is the geometry of photon multiplicity detector  
   class PhmdGeo : public AgModule 
   { 
      public: 
      PhmdGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~PhmdGeo(){ }; 
      ClassDef(PhmdGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace PhmdGeo 
#endif // __PhmdGeo__ 
