#ifndef __EcalGeo__ 
#define __EcalGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace ECALGEO // $NMSPC 
{ 
   class Emcg_t : public AgStructure 
   { 
      ClassDef(Emcg_t,1); 
      public: 
      Float_t version; 
      Int_t onoff; 
      Int_t fillmode; 
      Emcg_t() : AgStructure("Emcg_t","User-defined AgML structure") 
      { 
         version=0; 
         onoff=0; 
         fillmode=0; 
         _index=0; 
      } 
      ~ Emcg_t(){ /* nada */ }; 
   }; 
   class Emcs_t : public AgStructure 
   { 
      ClassDef(Emcs_t,1); 
      public: 
      Float_t type; 
      Float_t zorig; 
      Float_t zend; 
      Float_t etamin; 
      Float_t etamax; 
      Float_t phimin; 
      Float_t phimax; 
      Float_t offset; 
      Float_t nsupsec; 
      Float_t nsector; 
      Float_t nsection; 
      Float_t nslices; 
      Float_t front; 
      Float_t alincell; 
      Float_t frplast; 
      Float_t bkplast; 
      Float_t pbplate; 
      Float_t lamplate; 
      Float_t bckplate; 
      Float_t hub; 
      Float_t rmshift; 
      Float_t smshift; 
      Float_t gapplt; 
      Float_t gapcel; 
      Float_t gapsmd; 
      Float_t smdcentr; 
      Array_t<Float_t> tierod; 
      Float_t bckfrnt; 
      Float_t gaphalf; 
      Float_t cover; 
      Emcs_t() : AgStructure("Emcs_t","User-defined AgML structure") 
      { 
         type=0; 
         zorig=0; 
         zend=0; 
         etamin=0; 
         etamax=0; 
         phimin=0; 
         phimax=0; 
         offset=0; 
         nsupsec=0; 
         nsector=0; 
         nsection=0; 
         nslices=0; 
         front=0; 
         alincell=0; 
         frplast=0; 
         bkplast=0; 
         pbplate=0; 
         lamplate=0; 
         bckplate=0; 
         hub=0; 
         rmshift=0; 
         smshift=0; 
         gapplt=0; 
         gapcel=0; 
         gapsmd=0; 
         smdcentr=0; 
         tierod = Array_t<Float_t>(2); 
         bckfrnt=0; 
         gaphalf=0; 
         cover=0; 
         _index=0; 
      } 
      ~ Emcs_t(){ /* nada */ }; 
   }; 
   class Eetr_t : public AgStructure 
   { 
      ClassDef(Eetr_t,1); 
      public: 
      Float_t type; 
      Float_t etagr; 
      Float_t phigr; 
      Float_t neta; 
      Array_t<Float_t> etabin; 
      Eetr_t() : AgStructure("Eetr_t","User-defined AgML structure") 
      { 
         type=0; 
         etagr=0; 
         phigr=0; 
         neta=0; 
         etabin = Array_t<Float_t>(13); 
         _index=0; 
      } 
      ~ Eetr_t(){ /* nada */ }; 
   }; 
   class Esec_t : public AgStructure 
   { 
      ClassDef(Esec_t,1); 
      public: 
      Float_t isect; 
      Float_t fplmat; 
      Float_t cell; 
      Float_t scint; 
      Float_t nlayer; 
      Esec_t() : AgStructure("Esec_t","User-defined AgML structure") 
      { 
         isect=0; 
         fplmat=0; 
         cell=0; 
         scint=0; 
         nlayer=0; 
         _index=0; 
      } 
      ~ Esec_t(){ /* nada */ }; 
   }; 
   class Emxg_t : public AgStructure 
   { 
      ClassDef(Emxg_t,1); 
      public: 
      Float_t version; 
      Float_t sapex; 
      Float_t sbase; 
      Float_t rin; 
      Float_t rout; 
      Float_t f4; 
      Emxg_t() : AgStructure("Emxg_t","User-defined AgML structure") 
      { 
         version=0; 
         sapex=0; 
         sbase=0; 
         rin=0; 
         rout=0; 
         f4=0; 
         _index=0; 
      } 
      ~ Emxg_t(){ /* nada */ }; 
   }; 
   class Exse_t : public AgStructure 
   { 
      ClassDef(Exse_t,1); 
      public: 
      Float_t jsect; 
      Float_t zshift; 
      Array_t<Float_t> sectype; 
      Exse_t() : AgStructure("Exse_t","User-defined AgML structure") 
      { 
         jsect=0; 
         zshift=0; 
         sectype = Array_t<Float_t>(6); 
         _index=0; 
      } 
      ~ Exse_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- ECAL -- 
   ///@defgroup ECAL_doc 
   ///@class ECAL 
   ///@brief is one EMC EndCap wheel 
   class ECAL : public AgBlock 
   {  public: 
      ECAL() : AgBlock("ECAL","is one EMC EndCap wheel"){ }; 
      ~ECAL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ECAL,1); 
   }; 
   // ---------------------------------------------------------------------- EAGA -- 
   ///@defgroup EAGA_doc 
   ///@class EAGA 
   ///@brief is half of wheel air volume for the EndCap module 
   class EAGA : public AgBlock 
   {  public: 
      EAGA() : AgBlock("EAGA","is half of wheel air volume for the EndCap module"){ }; 
      ~EAGA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EAGA,1); 
   }; 
   // ---------------------------------------------------------------------- EMSS -- 
   ///@defgroup EMSS_doc 
   ///@class EMSS 
   ///@brief is steel support of the EndCap module 
   class EMSS : public AgBlock 
   {  public: 
      EMSS() : AgBlock("EMSS","is steel support of the EndCap module"){ }; 
      ~EMSS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EMSS,1); 
   }; 
   // ---------------------------------------------------------------------- ECVO -- 
   ///@defgroup ECVO_doc 
   ///@class ECVO 
   ///@brief is one of EndCap Volume with megatiles and radiators 
   class ECVO : public AgBlock 
   {  public: 
      ECVO() : AgBlock("ECVO","is one of EndCap Volume with megatiles and radiators"){ }; 
      ~ECVO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ECVO,1); 
   }; 
   // ---------------------------------------------------------------------- ESHM -- 
   ///@defgroup ESHM_doc 
   ///@class ESHM 
   ///@brief is the SHower Max section 
   class ESHM : public AgBlock 
   {  public: 
      ESHM() : AgBlock("ESHM","is the SHower Max section"){ }; 
      ~ESHM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ESHM,1); 
   }; 
   // ---------------------------------------------------------------------- ECGH -- 
   ///@defgroup ECGH_doc 
   ///@class ECGH 
   ///@brief is air Gap between endcap Half wheels 
   class ECGH : public AgBlock 
   {  public: 
      ECGH() : AgBlock("ECGH","is air Gap between endcap Half wheels"){ }; 
      ~ECGH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ECGH,1); 
   }; 
   // ---------------------------------------------------------------------- ECHC -- 
   ///@defgroup ECHC_doc 
   ///@class ECHC 
   ///@brief is steel EndCap Half Cover 
   class ECHC : public AgBlock 
   {  public: 
      ECHC() : AgBlock("ECHC","is steel EndCap Half Cover"){ }; 
      ~ECHC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ECHC,1); 
   }; 
   // ---------------------------------------------------------------------- ESSP -- 
   ///@defgroup ESSP_doc 
   ///@class ESSP 
   ///@brief is Stainless Steel back Plate 
   class ESSP : public AgBlock 
   {  public: 
      ESSP() : AgBlock("ESSP","is Stainless Steel back Plate"){ }; 
      ~ESSP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ESSP,1); 
   }; 
   // ---------------------------------------------------------------------- EPSB -- 
   ///@defgroup EPSB_doc 
   ///@class EPSB 
   ///@brief is Projectile Stainless steel Bar 
   class EPSB : public AgBlock 
   {  public: 
      EPSB() : AgBlock("EPSB","is Projectile Stainless steel Bar"){ }; 
      ~EPSB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EPSB,1); 
   }; 
   // ---------------------------------------------------------------------- ERCM -- 
   ///@defgroup ERCM_doc 
   ///@class ERCM 
   ///@brief is stainless steel tie Rod in CaloriMeter sections 
   class ERCM : public AgBlock 
   {  public: 
      ERCM() : AgBlock("ERCM","is stainless steel tie Rod in CaloriMeter sections"){ }; 
      ~ERCM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ERCM,1); 
   }; 
   // ---------------------------------------------------------------------- ERSM -- 
   ///@defgroup ERSM_doc 
   ///@class ERSM 
   ///@brief is stainless steel tie Rod in Shower Max 
   class ERSM : public AgBlock 
   {  public: 
      ERSM() : AgBlock("ERSM","is stainless steel tie Rod in Shower Max"){ }; 
      ~ERSM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ERSM,1); 
   }; 
   // ---------------------------------------------------------------------- EMOD -- 
   ///@defgroup EMOD_doc 
   ///@class EMOD 
   ///@brief is one module of the EM EndCap 
   class EMOD : public AgBlock 
   {  public: 
      EMOD() : AgBlock("EMOD","is one module of the EM EndCap"){ }; 
      ~EMOD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EMOD,1); 
   }; 
   // ---------------------------------------------------------------------- ESEC -- 
   ///@defgroup ESEC_doc 
   ///@class ESEC 
   ///@brief is a single EM section 
   class ESEC : public AgBlock 
   {  public: 
      ESEC() : AgBlock("ESEC","is a single EM section"){ }; 
      ~ESEC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ESEC,1); 
   }; 
   // ---------------------------------------------------------------------- EMGT -- 
   ///@defgroup EMGT_doc 
   ///@class EMGT 
   ///@brief is a megatile EM section 
   class EMGT : public AgBlock 
   {  public: 
      EMGT() : AgBlock("EMGT","is a megatile EM section"){ }; 
      ~EMGT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EMGT,1); 
   }; 
   // ---------------------------------------------------------------------- EPER -- 
   ///@defgroup EPER_doc 
   ///@class EPER 
   ///@brief is a EM subsection period (super layer) 
   class EPER : public AgBlock 
   {  public: 
      EPER() : AgBlock("EPER","is a EM subsection period (super layer)"){ }; 
      ~EPER(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EPER,1); 
   }; 
   // ---------------------------------------------------------------------- ETAR -- 
   ///@defgroup ETAR_doc 
   ///@class ETAR 
   ///@brief is one CELL of scintillator, fiber and plastic 
   class ETAR : public AgBlock 
   {  public: 
      ETAR() : AgBlock("ETAR","is one CELL of scintillator, fiber and plastic"){ }; 
      ~ETAR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ETAR,1); 
   }; 
   // ---------------------------------------------------------------------- ESCI -- 
   ///@defgroup ESCI_doc 
   ///@class ESCI 
   ///@brief is the active scintillator (polystyren) layer 
   class ESCI : public AgBlock 
   {  public: 
      ESCI() : AgBlock("ESCI","is the active scintillator (polystyren) layer"){ }; 
      ~ESCI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ESCI,1); 
   }; 
   // ---------------------------------------------------------------------- ERAD -- 
   ///@defgroup ERAD_doc 
   ///@class ERAD 
   ///@brief is radiator 
   class ERAD : public AgBlock 
   {  public: 
      ERAD() : AgBlock("ERAD","is radiator"){ }; 
      ~ERAD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ERAD,1); 
   }; 
   // ---------------------------------------------------------------------- ELED -- 
   ///@defgroup ELED_doc 
   ///@class ELED 
   ///@brief is lead absorber Plate 
   class ELED : public AgBlock 
   {  public: 
      ELED() : AgBlock("ELED","is lead absorber Plate"){ }; 
      ~ELED(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ELED,1); 
   }; 
   // ---------------------------------------------------------------------- EFLP -- 
   ///@defgroup EFLP_doc 
   ///@class EFLP 
   ///@brief is First Aluminium plate 
   class EFLP : public AgBlock 
   {  public: 
      EFLP() : AgBlock("EFLP","is First Aluminium plate"){ }; 
      ~EFLP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EFLP,1); 
   }; 
   // ---------------------------------------------------------------------- EALP -- 
   ///@defgroup EALP_doc 
   ///@class EALP 
   ///@brief is ALuminium Plate in calorimeter cell 
   class EALP : public AgBlock 
   {  public: 
      EALP() : AgBlock("EALP","is ALuminium Plate in calorimeter cell"){ }; 
      ~EALP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EALP,1); 
   }; 
   // ---------------------------------------------------------------------- ESPL -- 
   ///@defgroup ESPL_doc 
   ///@class ESPL 
   ///@brief is one of the Shower max PLanes 
   class ESPL : public AgBlock 
   {  public: 
      ESPL() : AgBlock("ESPL","is one of the Shower max PLanes"){ }; 
      ~ESPL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ESPL,1); 
   }; 
   // ---------------------------------------------------------------------- EXSG -- 
   ///@defgroup EXSG_doc 
   ///@class EXSG 
   ///@brief is the Shower max Gap for scintillator strips 
   class EXSG : public AgBlock 
   {  public: 
      EXSG() : AgBlock("EXSG","is the Shower max Gap for scintillator strips"){ }; 
      ~EXSG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EXSG,1); 
   }; 
   // ---------------------------------------------------------------------- EHMS -- 
   ///@defgroup EHMS_doc 
   ///@class EHMS 
   ///@brief is sHower Max Strip 
   class EHMS : public AgBlock 
   {  public: 
      EHMS() : AgBlock("EHMS","is sHower Max Strip"){ }; 
      ~EHMS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EHMS,1); 
   }; 
   // ---------------------------------------------------------------------- EXGT -- 
   ///@defgroup EXGT_doc 
   ///@class EXGT 
   ///@brief is the G10 layer in the Shower Max 
   class EXGT : public AgBlock 
   {  public: 
      EXGT() : AgBlock("EXGT","is the G10 layer in the Shower Max"){ }; 
      ~EXGT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EXGT,1); 
   }; 
   /// \class EcalGeo 
   /// \brief  is the EM EndCap Calorimeter GEOmetry  
   class EcalGeo : public AgModule 
   { 
      public: 
      EcalGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~EcalGeo(){ }; 
      ClassDef(EcalGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace EcalGeo 
#endif // __EcalGeo__ 
