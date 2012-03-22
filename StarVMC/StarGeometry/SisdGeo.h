#ifndef __SisdGeo__ 
#define __SisdGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace SISDGEO // $NMSPC 
{ 
   class Ssdp_t : public AgStructure 
   { 
      ClassDef(Ssdp_t,1); 
      public: 
      Float_t version; 
      Float_t config; 
      Int_t placement; 
      Ssdp_t() : AgStructure("Ssdp_t","User-defined AgML structure") 
      { 
         version=0; 
         config=0; 
         placement=0; 
         _index=0; 
      } 
      ~ Ssdp_t(){ /* nada */ }; 
   }; 
   class Sfpa_t : public AgStructure 
   { 
      ClassDef(Sfpa_t,1); 
      public: 
      Float_t version; 
      Float_t rmin; 
      Float_t rmax; 
      Float_t len; 
      Float_t rad; 
      Float_t nssd; 
      Float_t dmwid; 
      Float_t dmthk; 
      Float_t dmlen; 
      Float_t smwid; 
      Float_t smthk; 
      Float_t smlen; 
      Float_t sslen; 
      Float_t wplen; 
      Float_t sdlen; 
      Float_t tilt; 
      Float_t cprad; 
      Float_t cpral; 
      Float_t cfrad; 
      Float_t gpthk; 
      Array_t<Int_t> laddermap; 
      Array_t<Float_t> ladderangle; 
      Array_t<Float_t> laddertilt; 
      Array_t<Float_t> ladderradius; 
      Sfpa_t() : AgStructure("Sfpa_t","User-defined AgML structure") 
      { 
         version=0; 
         rmin=0; 
         rmax=0; 
         len=0; 
         rad=0; 
         nssd=0; 
         dmwid=0; 
         dmthk=0; 
         dmlen=0; 
         smwid=0; 
         smthk=0; 
         smlen=0; 
         sslen=0; 
         wplen=0; 
         sdlen=0; 
         tilt=0; 
         cprad=0; 
         cpral=0; 
         cfrad=0; 
         gpthk=0; 
         laddermap = Array_t<Int_t>(20); 
         ladderangle = Array_t<Float_t>(20); 
         laddertilt = Array_t<Float_t>(20); 
         ladderradius = Array_t<Float_t>(20); 
         _index=0; 
      } 
      ~ Sfpa_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- SFMO -- 
   ///@defgroup SFMO_doc 
   ///@class SFMO 
   ///@brief is the mother of all Silicon Strip Detector volumes 
   class SFMO : public AgBlock 
   {  public: 
      SFMO() : AgBlock("SFMO","is the mother of all Silicon Strip Detector volumes"){ }; 
      ~SFMO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFMO,1); 
   }; 
   // ---------------------------------------------------------------------- SFLM -- 
   ///@defgroup SFLM_doc 
   ///@class SFLM 
   ///@brief is the mother of the ladder 
   class SFLM : public AgBlock 
   {  public: 
      SFLM() : AgBlock("SFLM","is the mother of the ladder"){ }; 
      ~SFLM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFLM,1); 
   }; 
   // ---------------------------------------------------------------------- SFDM -- 
   ///@defgroup SFDM_doc 
   ///@class SFDM 
   ///@brief is the mother of the detectors 
   class SFDM : public AgBlock 
   {  public: 
      SFDM() : AgBlock("SFDM","is the mother of the detectors"){ }; 
      ~SFDM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFDM,1); 
   }; 
   // ---------------------------------------------------------------------- SFSW -- 
   ///@defgroup SFSW_doc 
   ///@class SFSW 
   ///@brief is a single wafer container 
   class SFSW : public AgBlock 
   {  public: 
      SFSW() : AgBlock("SFSW","is a single wafer container"){ }; 
      ~SFSW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFSW,1); 
   }; 
   // ---------------------------------------------------------------------- SFSD -- 
   ///@defgroup SFSD_doc 
   ///@class SFSD 
   ///@brief is the strip detector 
   class SFSD : public AgBlock 
   {  public: 
      SFSD() : AgBlock("SFSD","is the strip detector"){ }; 
      ~SFSD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFSD,1); 
   }; 
   // ---------------------------------------------------------------------- SFSM -- 
   ///@defgroup SFSM_doc 
   ///@class SFSM 
   ///@brief is the mother of the ladder struct. 
   class SFSM : public AgBlock 
   {  public: 
      SFSM() : AgBlock("SFSM","is the mother of the ladder struct."){ }; 
      ~SFSM(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFSM,1); 
   }; 
   // ---------------------------------------------------------------------- SFSS -- 
   ///@defgroup SFSS_doc 
   ///@class SFSS 
   ///@brief is the subvolume of the mother struct. 
   class SFSS : public AgBlock 
   {  public: 
      SFSS() : AgBlock("SFSS","is the subvolume of the mother struct."){ }; 
      ~SFSS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFSS,1); 
   }; 
   // ---------------------------------------------------------------------- SFCP -- 
   ///@defgroup SFCP_doc 
   ///@class SFCP 
   ///@brief is the cooling pipe 
   class SFCP : public AgBlock 
   {  public: 
      SFCP() : AgBlock("SFCP","is the cooling pipe"){ }; 
      ~SFCP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFCP,1); 
   }; 
   // ---------------------------------------------------------------------- SFCW -- 
   ///@defgroup SFCW_doc 
   ///@class SFCW 
   ///@brief is the water cylinder in the cooling pipe 
   class SFCW : public AgBlock 
   {  public: 
      SFCW() : AgBlock("SFCW","is the water cylinder in the cooling pipe"){ }; 
      ~SFCW(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFCW,1); 
   }; 
   // ---------------------------------------------------------------------- SFCF -- 
   ///@defgroup SFCF_doc 
   ///@class SFCF 
   ///@brief is the carbon fiber structure container 
   class SFCF : public AgBlock 
   {  public: 
      SFCF() : AgBlock("SFCF","is the carbon fiber structure container"){ }; 
      ~SFCF(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFCF,1); 
   }; 
   // ---------------------------------------------------------------------- SFCT -- 
   ///@defgroup SFCT_doc 
   ///@class SFCT 
   ///@brief is the carbon fiber tube 
   class SFCT : public AgBlock 
   {  public: 
      SFCT() : AgBlock("SFCT","is the carbon fiber tube"){ }; 
      ~SFCT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFCT,1); 
   }; 
   // ---------------------------------------------------------------------- SFCX -- 
   ///@defgroup SFCX_doc 
   ///@class SFCX 
   ///@brief is the carbon fiber tube 
   class SFCX : public AgBlock 
   {  public: 
      SFCX() : AgBlock("SFCX","is the carbon fiber tube"){ }; 
      ~SFCX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(SFCX,1); 
   }; 
   /// \class SisdGeo 
   /// \brief   is the Silicon Strip Detector  
   class SisdGeo : public AgModule 
   { 
      public: 
      SisdGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~SisdGeo(){ }; 
      ClassDef(SisdGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace SisdGeo 
#endif // __SisdGeo__ 
