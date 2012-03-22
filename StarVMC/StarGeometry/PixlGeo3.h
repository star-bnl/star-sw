#ifndef __PixlGeo3__ 
#define __PixlGeo3__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace PIXLGEO3 // $NMSPC 
{ 
   class Pxlv_t : public AgStructure 
   { 
      ClassDef(Pxlv_t,1); 
      public: 
      Int_t version; 
      Float_t ladver; 
      Float_t location; 
      Pxlv_t() : AgStructure("Pxlv_t","User-defined AgML structure") 
      { 
         version=0; 
         ladver=0; 
         location=0; 
         _index=0; 
      } 
      ~ Pxlv_t(){ /* nada */ }; 
   }; 
   class Pxld_t : public AgStructure 
   { 
      ClassDef(Pxld_t,1); 
      public: 
      Float_t version; 
      Float_t totallength; 
      Float_t ladderwidth; 
      Float_t ladderthk; 
      Float_t passivethk; 
      Float_t activethk; 
      Float_t rin; 
      Float_t rout; 
      Pxld_t() : AgStructure("Pxld_t","User-defined AgML structure") 
      { 
         version=0; 
         totallength=0; 
         ladderwidth=0; 
         ladderthk=0; 
         passivethk=0; 
         activethk=0; 
         rin=0; 
         rout=0; 
         _index=0; 
      } 
      ~ Pxld_t(){ /* nada */ }; 
   }; 
   class Pixg_t : public AgStructure 
   { 
      ClassDef(Pixg_t,1); 
      public: 
      Float_t ladder; 
      Float_t r; 
      Float_t a; 
      Float_t poffset; 
      Float_t aoffset; 
      Pixg_t() : AgStructure("Pixg_t","User-defined AgML structure") 
      { 
         ladder=0; 
         r=0; 
         a=0; 
         poffset=0; 
         aoffset=0; 
         _index=0; 
      } 
      ~ Pixg_t(){ /* nada */ }; 
   }; 
   class Pxbg_t : public AgStructure 
   { 
      ClassDef(Pxbg_t,1); 
      public: 
      Float_t version; 
      Float_t length; 
      Float_t rin; 
      Float_t thk; 
      Pxbg_t() : AgStructure("Pxbg_t","User-defined AgML structure") 
      { 
         version=0; 
         length=0; 
         rin=0; 
         thk=0; 
         _index=0; 
      } 
      ~ Pxbg_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- PXMO -- 
   ///@defgroup PXMO_doc 
   ///@class PXMO 
   ///@brief is the mother of the pixel detector volumes 
   class PXMO : public AgBlock 
   {  public: 
      PXMO() : AgBlock("PXMO","is the mother of the pixel detector volumes"){ }; 
      ~PXMO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXMO,1); 
   }; 
   // ---------------------------------------------------------------------- PXBX -- 
   ///@defgroup PXBX_doc 
   ///@class PXBX 
   ///@brief is the exoskeleton of the beampipe 
   class PXBX : public AgBlock 
   {  public: 
      PXBX() : AgBlock("PXBX","is the exoskeleton of the beampipe"){ }; 
      ~PXBX(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PXBX,1); 
   }; 
   // ---------------------------------------------------------------------- PSEC -- 
   ///@defgroup PSEC_doc 
   ///@class PSEC 
   ///@brief is a group of ladders 
   class PSEC : public AgBlock 
   {  public: 
      PSEC() : AgBlock("PSEC","is a group of ladders"){ }; 
      ~PSEC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PSEC,1); 
   }; 
   // ---------------------------------------------------------------------- PLMO -- 
   ///@defgroup PLMO_doc 
   ///@class PLMO 
   ///@brief is the mother of the silicon ladder 
   class PLMO : public AgBlock 
   {  public: 
      PLMO() : AgBlock("PLMO","is the mother of the silicon ladder"){ }; 
      ~PLMO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PLMO,1); 
   }; 
   // ---------------------------------------------------------------------- PLAC -- 
   ///@defgroup PLAC_doc 
   ///@class PLAC 
   ///@brief is the active layer of the ladder 
   class PLAC : public AgBlock 
   {  public: 
      PLAC() : AgBlock("PLAC","is the active layer of the ladder"){ }; 
      ~PLAC(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PLAC,1); 
   }; 
   // ---------------------------------------------------------------------- PLPS -- 
   ///@defgroup PLPS_doc 
   ///@class PLPS 
   ///@brief is the passive layer of the ladder 
   class PLPS : public AgBlock 
   {  public: 
      PLPS() : AgBlock("PLPS","is the passive layer of the ladder"){ }; 
      ~PLPS(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(PLPS,1); 
   }; 
   /// \class PixlGeo3 
   /// \brief  is the the STAR pixel detector and beam pipe support  
   class PixlGeo3 : public AgModule 
   { 
      public: 
      PixlGeo3(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~PixlGeo3(){ }; 
      ClassDef(PixlGeo3,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace PixlGeo3 
#endif // __PixlGeo3__ 
