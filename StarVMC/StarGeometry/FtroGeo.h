#ifndef __FtroGeo__ 
#define __FtroGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace FTROGEO // $NMSPC 
{ 
   class Ftrg_t : public AgStructure 
   { 
      ClassDef(Ftrg_t,1); 
      public: 
      Float_t version; 
      Float_t n; 
      Float_t ftpcz; 
      Float_t ftpclen; 
      Float_t length; 
      Float_t angoffset; 
      Float_t rin; 
      Float_t rout; 
      Float_t inrin; 
      Float_t inrout; 
      Float_t inrthk; 
      Float_t ofrin; 
      Float_t ofrout; 
      Float_t ofrthk; 
      Float_t ofz; 
      Float_t ofnholes; 
      Float_t ofholerad; 
      Float_t ofholer; 
      Float_t strutlen; 
      Float_t struthgt; 
      Float_t strutwth; 
      Float_t strutthk; 
      Float_t strutholer; 
      Float_t strutnholes; 
      Float_t ftpowth; 
      Float_t ftpothk; 
      Float_t ftpor; 
      Float_t ftpiwth; 
      Float_t ftpithk; 
      Float_t ftpir; 
      Float_t shellthk; 
      Float_t rimthk; 
      Float_t rimwth; 
      Float_t plankwth; 
      Float_t plankthk; 
      Ftrg_t() : AgStructure("Ftrg_t","User-defined AgML structure") 
      { 
         version=0; 
         n=0; 
         ftpcz=0; 
         ftpclen=0; 
         length=0; 
         angoffset=0; 
         rin=0; 
         rout=0; 
         inrin=0; 
         inrout=0; 
         inrthk=0; 
         ofrin=0; 
         ofrout=0; 
         ofrthk=0; 
         ofz=0; 
         ofnholes=0; 
         ofholerad=0; 
         ofholer=0; 
         strutlen=0; 
         struthgt=0; 
         strutwth=0; 
         strutthk=0; 
         strutholer=0; 
         strutnholes=0; 
         ftpowth=0; 
         ftpothk=0; 
         ftpor=0; 
         ftpiwth=0; 
         ftpithk=0; 
         ftpir=0; 
         shellthk=0; 
         rimthk=0; 
         rimwth=0; 
         plankwth=0; 
         plankthk=0; 
         _index=0; 
      } 
      ~ Ftrg_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- FTMO -- 
   ///@defgroup FTMO_doc 
   ///@class FTMO 
   ///@brief is the mother of the single FTPC RO barrel 
   class FTMO : public AgBlock 
   {  public: 
      FTMO() : AgBlock("FTMO","is the mother of the single FTPC RO barrel"){ }; 
      ~FTMO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FTMO,1); 
   }; 
   // ---------------------------------------------------------------------- FTCM -- 
   ///@defgroup FTCM_doc 
   ///@class FTCM 
   ///@brief is the mother of the core struts and PCBs 
   class FTCM : public AgBlock 
   {  public: 
      FTCM() : AgBlock("FTCM","is the mother of the core struts and PCBs"){ }; 
      ~FTCM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FTCM,1); 
   }; 
   // ---------------------------------------------------------------------- FTCD -- 
   ///@defgroup FTCD_doc 
   ///@class FTCD 
   ///@brief is the division of the FTCM 
   class FTCD : public AgBlock 
   {  public: 
      FTCD() : AgBlock("FTCD","is the division of the FTCM"){ }; 
      ~FTCD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FTCD,1); 
   }; 
   // ---------------------------------------------------------------------- FTOF -- 
   ///@defgroup FTOF_doc 
   ///@class FTOF 
   ///@brief is the outer flange 
   class FTOF : public AgBlock 
   {  public: 
      FTOF() : AgBlock("FTOF","is the outer flange"){ }; 
      ~FTOF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FTOF,1); 
   }; 
   // ---------------------------------------------------------------------- FTIF -- 
   ///@defgroup FTIF_doc 
   ///@class FTIF 
   ///@brief is the inner flange 
   class FTIF : public AgBlock 
   {  public: 
      FTIF() : AgBlock("FTIF","is the inner flange"){ }; 
      ~FTIF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FTIF,1); 
   }; 
   // ---------------------------------------------------------------------- FTRM -- 
   ///@defgroup FTRM_doc 
   ///@class FTRM 
   ///@brief is the rim connected to the inner flange 
   class FTRM : public AgBlock 
   {  public: 
      FTRM() : AgBlock("FTRM","is the rim connected to the inner flange"){ }; 
      ~FTRM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FTRM,1); 
   }; 
   // ---------------------------------------------------------------------- FTOH -- 
   ///@defgroup FTOH_doc 
   ///@class FTOH 
   ///@brief is a hole the outer flange 
   class FTOH : public AgBlock 
   {  public: 
      FTOH() : AgBlock("FTOH","is a hole the outer flange"){ }; 
      ~FTOH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FTOH,1); 
   }; 
   // ---------------------------------------------------------------------- FSMO -- 
   ///@defgroup FSMO_doc 
   ///@class FSMO 
   ///@brief is the mother of the strut 
   class FSMO : public AgBlock 
   {  public: 
      FSMO() : AgBlock("FSMO","is the mother of the strut"){ }; 
      ~FSMO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSMO,1); 
   }; 
   // ---------------------------------------------------------------------- FTPL -- 
   ///@defgroup FTPL_doc 
   ///@class FTPL 
   ///@brief is the plank covering the strut 
   class FTPL : public AgBlock 
   {  public: 
      FTPL() : AgBlock("FTPL","is the plank covering the strut"){ }; 
      ~FTPL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FTPL,1); 
   }; 
   // ---------------------------------------------------------------------- FSTL -- 
   ///@defgroup FSTL_doc 
   ///@class FSTL 
   ///@brief is the flat part of the strut 
   class FSTL : public AgBlock 
   {  public: 
      FSTL() : AgBlock("FSTL","is the flat part of the strut"){ }; 
      ~FSTL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSTL,1); 
   }; 
   // ---------------------------------------------------------------------- FSTC -- 
   ///@defgroup FSTC_doc 
   ///@class FSTC 
   ///@brief is the central beam of the strut 
   class FSTC : public AgBlock 
   {  public: 
      FSTC() : AgBlock("FSTC","is the central beam of the strut"){ }; 
      ~FSTC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSTC,1); 
   }; 
   // ---------------------------------------------------------------------- FSHL -- 
   ///@defgroup FSHL_doc 
   ///@class FSHL 
   ///@brief is a hole the beam of the strut 
   class FSHL : public AgBlock 
   {  public: 
      FSHL() : AgBlock("FSHL","is a hole the beam of the strut"){ }; 
      ~FSHL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FSHL,1); 
   }; 
   // ---------------------------------------------------------------------- FTPO -- 
   ///@defgroup FTPO_doc 
   ///@class FTPO 
   ///@brief is the outer PCB 
   class FTPO : public AgBlock 
   {  public: 
      FTPO() : AgBlock("FTPO","is the outer PCB"){ }; 
      ~FTPO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FTPO,1); 
   }; 
   // ---------------------------------------------------------------------- FTPI -- 
   ///@defgroup FTPI_doc 
   ///@class FTPI 
   ///@brief is the inner PCB 
   class FTPI : public AgBlock 
   {  public: 
      FTPI() : AgBlock("FTPI","is the inner PCB"){ }; 
      ~FTPI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FTPI,1); 
   }; 
   // ---------------------------------------------------------------------- FTSH -- 
   ///@defgroup FTSH_doc 
   ///@class FTSH 
   ///@brief is the protective shell 
   class FTSH : public AgBlock 
   {  public: 
      FTSH() : AgBlock("FTSH","is the protective shell"){ }; 
      ~FTSH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(FTSH,1); 
   }; 
   /// \class FtroGeo 
   /// \brief  is the geometry of the readout structure of the FTPC  
   class FtroGeo : public AgModule 
   { 
      public: 
      FtroGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~FtroGeo(){ }; 
      ClassDef(FtroGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace FtroGeo 
#endif // __FtroGeo__ 
