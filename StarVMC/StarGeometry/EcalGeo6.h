#ifndef __EcalGeo6__ 
#define __EcalGeo6__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace ECALGEO6 // $NMSPC 
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
      Float_t version; 
      Float_t type; 
      Float_t zorg; 
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
      Float_t rtie; 
      Float_t slop; 
      Emcs_t() : AgStructure("Emcs_t","User-defined AgML structure") 
      { 
         version=0; 
         type=0; 
         zorg=0; 
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
         rtie=0; 
         slop=0; 
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
      Float_t deltaz; 
      Array_t<Float_t> jiggle; 
      Esec_t() : AgStructure("Esec_t","User-defined AgML structure") 
      { 
         isect=0; 
         fplmat=0; 
         cell=0; 
         scint=0; 
         nlayer=0; 
         deltaz=0; 
         jiggle = Array_t<Float_t>(18); 
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
   class Esmd_t : public AgStructure 
   { 
      ClassDef(Esmd_t,1); 
      public: 
      Float_t version; 
      Float_t front_layer; 
      Float_t back_layer; 
      Float_t spacer_layer; 
      Float_t base; 
      Float_t apex; 
      Esmd_t() : AgStructure("Esmd_t","User-defined AgML structure") 
      { 
         version=0; 
         front_layer=0; 
         back_layer=0; 
         spacer_layer=0; 
         base=0; 
         apex=0; 
         _index=0; 
      } 
      ~ Esmd_t(){ /* nada */ }; 
   }; 
   class Ecut_t : public AgStructure 
   { 
      ClassDef(Ecut_t,1); 
      public: 
      Float_t version; 
      Float_t absorber; 
      Float_t sensitive; 
      Float_t blah; 
      Ecut_t() : AgStructure("Ecut_t","User-defined AgML structure") 
      { 
         version=0; 
         absorber=0; 
         sensitive=0; 
         blah=0; 
         _index=0; 
      } 
      ~ Ecut_t(){ /* nada */ }; 
   }; 
   class Eabs_t : public AgStructure 
   { 
      ClassDef(Eabs_t,1); 
      public: 
      Float_t version; 
      Float_t cutgam; 
      Float_t cutele; 
      Float_t cutneu; 
      Float_t cuthad; 
      Float_t cutmuo; 
      Float_t dcute; 
      Float_t dcutm; 
      Float_t bcute; 
      Float_t bcutm; 
      Eabs_t() : AgStructure("Eabs_t","User-defined AgML structure") 
      { 
         version=0; 
         cutgam=0; 
         cutele=0; 
         cutneu=0; 
         cuthad=0; 
         cutmuo=0; 
         dcute=0; 
         dcutm=0; 
         bcute=0; 
         bcutm=0; 
         _index=0; 
      } 
      ~ Eabs_t(){ /* nada */ }; 
   }; 
   class Esen_t : public AgStructure 
   { 
      ClassDef(Esen_t,1); 
      public: 
      Float_t version; 
      Float_t cutgam; 
      Float_t cutele; 
      Float_t cutneu; 
      Float_t cuthad; 
      Float_t cutmuo; 
      Float_t dcute; 
      Float_t dcutm; 
      Float_t bcute; 
      Float_t bcutm; 
      Esen_t() : AgStructure("Esen_t","User-defined AgML structure") 
      { 
         version=0; 
         cutgam=0; 
         cutele=0; 
         cutneu=0; 
         cuthad=0; 
         cutmuo=0; 
         dcute=0; 
         dcutm=0; 
         bcute=0; 
         bcutm=0; 
         _index=0; 
      } 
      ~ Esen_t(){ /* nada */ }; 
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
   ///@brief IS HALF OF WHEEL AIR VOLUME FOR THE ENDCAP MODULE 
   class EAGA : public AgBlock 
   {  public: 
      EAGA() : AgBlock("EAGA","IS HALF OF WHEEL AIR VOLUME FOR THE ENDCAP MODULE"){ }; 
      ~EAGA(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EAGA,1); 
   }; 
   // ---------------------------------------------------------------------- EMSS -- 
   ///@defgroup EMSS_doc 
   ///@class EMSS 
   ///@brief is the steel support of the endcap module 
   class EMSS : public AgBlock 
   {  public: 
      EMSS() : AgBlock("EMSS","is the steel support of the endcap module"){ }; 
      ~EMSS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EMSS,1); 
   }; 
   // ---------------------------------------------------------------------- ECVO -- 
   ///@defgroup ECVO_doc 
   ///@class ECVO 
   ///@brief is one of endcap volume with megatiles and radiators 
   class ECVO : public AgBlock 
   {  public: 
      ECVO() : AgBlock("ECVO","is one of endcap volume with megatiles and radiators"){ }; 
      ~ECVO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ECVO,1); 
   }; 
   // ---------------------------------------------------------------------- ESHM -- 
   ///@defgroup ESHM_doc 
   ///@class ESHM 
   ///@brief is the shower max section 
   class ESHM : public AgBlock 
   {  public: 
      ESHM() : AgBlock("ESHM","is the shower max section"){ }; 
      ~ESHM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ESHM,1); 
   }; 
   // ---------------------------------------------------------------------- ECGH -- 
   ///@defgroup ECGH_doc 
   ///@class ECGH 
   ///@brief is air gap between endcap half wheels 
   class ECGH : public AgBlock 
   {  public: 
      ECGH() : AgBlock("ECGH","is air gap between endcap half wheels"){ }; 
      ~ECGH(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ECGH,1); 
   }; 
   // ---------------------------------------------------------------------- ECHC -- 
   ///@defgroup ECHC_doc 
   ///@class ECHC 
   ///@brief is steel endcap half cover 
   class ECHC : public AgBlock 
   {  public: 
      ECHC() : AgBlock("ECHC","is steel endcap half cover"){ }; 
      ~ECHC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ECHC,1); 
   }; 
   // ---------------------------------------------------------------------- ESSP -- 
   ///@defgroup ESSP_doc 
   ///@class ESSP 
   ///@brief is stainless steel back plate 
   class ESSP : public AgBlock 
   {  public: 
      ESSP() : AgBlock("ESSP","is stainless steel back plate"){ }; 
      ~ESSP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ESSP,1); 
   }; 
   // ---------------------------------------------------------------------- EPSB -- 
   ///@defgroup EPSB_doc 
   ///@class EPSB 
   ///@brief IS A PROJECTILE STAINLESS STEEL BAR 
   class EPSB : public AgBlock 
   {  public: 
      EPSB() : AgBlock("EPSB","IS A PROJECTILE STAINLESS STEEL BAR"){ }; 
      ~EPSB(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EPSB,1); 
   }; 
   // ---------------------------------------------------------------------- ERCM -- 
   ///@defgroup ERCM_doc 
   ///@class ERCM 
   ///@brief is stainless steel tie rod in calorimeter sections 
   class ERCM : public AgBlock 
   {  public: 
      ERCM() : AgBlock("ERCM","is stainless steel tie rod in calorimeter sections"){ }; 
      ~ERCM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ERCM,1); 
   }; 
   // ---------------------------------------------------------------------- ERSM -- 
   ///@defgroup ERSM_doc 
   ///@class ERSM 
   ///@brief is stainless steel tie rod in shower max 
   class ERSM : public AgBlock 
   {  public: 
      ERSM() : AgBlock("ERSM","is stainless steel tie rod in shower max"){ }; 
      ~ERSM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ERSM,1); 
   }; 
   // ---------------------------------------------------------------------- EMOD -- 
   ///@defgroup EMOD_doc 
   ///@class EMOD 
   ///@brief (fsect,lsect) IS ONE MODULE OF THE EM ENDCAP 
   class EMOD : public AgBlock 
   {  public: 
      EMOD() : AgBlock("EMOD","(fsect,lsect) IS ONE MODULE OF THE EM ENDCAP"){ }; 
      ~EMOD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EMOD,1); 
   }; 
   // ---------------------------------------------------------------------- ESEC -- 
   ///@defgroup ESEC_doc 
   ///@class ESEC 
   ///@brief is a single em section 
   class ESEC : public AgBlock 
   {  public: 
      ESEC() : AgBlock("ESEC","is a single em section"){ }; 
      ~ESEC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ESEC,1); 
   }; 
   // ---------------------------------------------------------------------- EMGT -- 
   ///@defgroup EMGT_doc 
   ///@class EMGT 
   ///@brief is a 30 degree megatile 
   class EMGT : public AgBlock 
   {  public: 
      EMGT() : AgBlock("EMGT","is a 30 degree megatile"){ }; 
      ~EMGT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EMGT,1); 
   }; 
   // ---------------------------------------------------------------------- EPER -- 
   ///@defgroup EPER_doc 
   ///@class EPER 
   ///@brief is a 5 degree slice of a 30 degree megatile (subsector) 
   class EPER : public AgBlock 
   {  public: 
      EPER() : AgBlock("EPER","is a 5 degree slice of a 30 degree megatile (subsector)"){ }; 
      ~EPER(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EPER,1); 
   }; 
   // ---------------------------------------------------------------------- ETAR -- 
   ///@defgroup ETAR_doc 
   ///@class ETAR 
   ///@brief is a single calorimeter cell, containing scintillator, fiber router, etc... 
   class ETAR : public AgBlock 
   {  public: 
      ETAR() : AgBlock("ETAR","is a single calorimeter cell, containing scintillator, fiber router, etc..."){ }; 
      ~ETAR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ETAR,1); 
   }; 
   // ---------------------------------------------------------------------- ESCI -- 
   ///@defgroup ESCI_doc 
   ///@class ESCI 
   ///@brief is the active scintillator (polystyrene) layer 
   class ESCI : public AgBlock 
   {  public: 
      ESCI() : AgBlock("ESCI","is the active scintillator (polystyrene) layer"){ }; 
      ~ESCI(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ESCI,1); 
   }; 
   // ---------------------------------------------------------------------- ERAD -- 
   ///@defgroup ERAD_doc 
   ///@class ERAD 
   ///@brief is the lead radiator with stainless steel cladding 
   class ERAD : public AgBlock 
   {  public: 
      ERAD() : AgBlock("ERAD","is the lead radiator with stainless steel cladding"){ }; 
      ~ERAD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ERAD,1); 
   }; 
   // ---------------------------------------------------------------------- ELED -- 
   ///@defgroup ELED_doc 
   ///@class ELED 
   ///@brief is a lead absorber plate 
   class ELED : public AgBlock 
   {  public: 
      ELED() : AgBlock("ELED","is a lead absorber plate"){ }; 
      ~ELED(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ELED,1); 
   }; 
   // ---------------------------------------------------------------------- EFLP -- 
   ///@defgroup EFLP_doc 
   ///@class EFLP 
   ///@brief is the aluminum (aluminium) front plate of the endcap 
   class EFLP : public AgBlock 
   {  public: 
      EFLP() : AgBlock("EFLP","is the aluminum (aluminium) front plate of the endcap"){ }; 
      ~EFLP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EFLP,1); 
   }; 
   // ---------------------------------------------------------------------- EALP -- 
   ///@defgroup EALP_doc 
   ///@class EALP 
   ///@brief is the thin aluminium plate in calorimeter cell 
   class EALP : public AgBlock 
   {  public: 
      EALP() : AgBlock("EALP","is the thin aluminium plate in calorimeter cell"){ }; 
      ~EALP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EALP,1); 
   }; 
   // ---------------------------------------------------------------------- ESPL -- 
   ///@defgroup ESPL_doc 
   ///@class ESPL 
   ///@brief is the logical volume containing an SMD plane 
   class ESPL : public AgBlock 
   {  public: 
      ESPL() : AgBlock("ESPL","is the logical volume containing an SMD plane"){ }; 
      ~ESPL(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(ESPL,1); 
   }; 
   // ---------------------------------------------------------------------- EXSG -- 
   ///@defgroup EXSG_doc 
   ///@class EXSG 
   ///@brief Is another logical volume... this one acutally creates the planes 
   class EXSG : public AgBlock 
   {  public: 
      EXSG() : AgBlock("EXSG","Is another logical volume... this one acutally creates the planes"){ }; 
      ~EXSG(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EXSG,1); 
   }; 
   // ---------------------------------------------------------------------- EHMS -- 
   ///@defgroup EHMS_doc 
   ///@class EHMS 
   ///@brief defines the triangular SMD strips 
   class EHMS : public AgBlock 
   {  public: 
      EHMS() : AgBlock("EHMS","defines the triangular SMD strips"){ }; 
      ~EHMS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EHMS,1); 
   }; 
   // ---------------------------------------------------------------------- EFLS -- 
   ///@defgroup EFLS_doc 
   ///@class EFLS 
   ///@brief is the layer of material on the front of the SMD planes 
   class EFLS : public AgBlock 
   {  public: 
      EFLS() : AgBlock("EFLS","is the layer of material on the front of the SMD planes"){ }; 
      ~EFLS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EFLS,1); 
   }; 
   // ---------------------------------------------------------------------- EBLS -- 
   ///@defgroup EBLS_doc 
   ///@class EBLS 
   ///@brief is the layer of material on the back of the SMD planes 
   class EBLS : public AgBlock 
   {  public: 
      EBLS() : AgBlock("EBLS","is the layer of material on the back of the SMD planes"){ }; 
      ~EBLS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EBLS,1); 
   }; 
   // ---------------------------------------------------------------------- EXPS -- 
   ///@defgroup EXPS_doc 
   ///@class EXPS 
   ///@brief is the plastic spacer in the shower maximum section 
   class EXPS : public AgBlock 
   {  public: 
      EXPS() : AgBlock("EXPS","is the plastic spacer in the shower maximum section"){ }; 
      ~EXPS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(EXPS,1); 
   }; 
   /// \class EcalGeo6 
   /// \brief  is the EM EndCap Calorimeter GEOmetry  
   class EcalGeo6 : public AgModule 
   { 
      public: 
      EcalGeo6(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~EcalGeo6(){ }; 
      ClassDef(EcalGeo6,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace EcalGeo6 
#endif // __EcalGeo6__ 
namespace ECALGEO6 // $NMSPC 
{ 
   /// This method has not been documented 
   extern void ecal_get_strip (Float_t &section,Int_t &cut,Int_t &istrip,Float_t &xcenter,Float_t &ycenter,Float_t &length); 
} 
