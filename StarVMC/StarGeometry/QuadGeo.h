#ifndef __QuadGeo__ 
#define __QuadGeo__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace QUADGEO // $NMSPC 
{ 
   class Shlq_t : public AgStructure 
   { 
      ClassDef(Shlq_t,1); 
      public: 
      Float_t version; 
      Float_t q0; 
      Float_t motherr; 
      Float_t motherl; 
      Float_t xoffset; 
      Float_t angle; 
      Float_t dzerol; 
      Float_t dzerori; 
      Float_t dzeroro; 
      Float_t q1; 
      Float_t ri1; 
      Float_t ro1; 
      Float_t dz1; 
      Float_t q2; 
      Float_t ri2; 
      Float_t ro2; 
      Float_t dz2; 
      Float_t q3; 
      Float_t ri3; 
      Float_t ro3; 
      Float_t dz3; 
      Shlq_t() : AgStructure("Shlq_t","User-defined AgML structure") 
      { 
         version=0; 
         q0=0; 
         motherr=0; 
         motherl=0; 
         xoffset=0; 
         angle=0; 
         dzerol=0; 
         dzerori=0; 
         dzeroro=0; 
         q1=0; 
         ri1=0; 
         ro1=0; 
         dz1=0; 
         q2=0; 
         ri2=0; 
         ro2=0; 
         dz2=0; 
         q3=0; 
         ri3=0; 
         ro3=0; 
         dz3=0; 
         _index=0; 
      } 
      ~ Shlq_t(){ /* nada */ }; 
   }; 
   // ---------------------------------------------------------------------- MGMT -- 
   ///@defgroup MGMT_doc 
   ///@class MGMT 
   ///@brief is the magnet mother 
   class MGMT : public AgBlock 
   {  public: 
      MGMT() : AgBlock("MGMT","is the magnet mother"){ }; 
      ~MGMT(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MGMT,1); 
   }; 
   // ---------------------------------------------------------------------- DZER -- 
   ///@defgroup DZER_doc 
   ///@class DZER 
   ///@brief is the D0 yoke 
   class DZER : public AgBlock 
   {  public: 
      DZER() : AgBlock("DZER","is the D0 yoke"){ }; 
      ~DZER(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(DZER,1); 
   }; 
   // ---------------------------------------------------------------------- QONE -- 
   ///@defgroup QONE_doc 
   ///@class QONE 
   ///@brief is the Q1 yoke 
   class QONE : public AgBlock 
   {  public: 
      QONE() : AgBlock("QONE","is the Q1 yoke"){ }; 
      ~QONE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(QONE,1); 
   }; 
   // ---------------------------------------------------------------------- QTWO -- 
   ///@defgroup QTWO_doc 
   ///@class QTWO 
   ///@brief is the Q2 yoke 
   class QTWO : public AgBlock 
   {  public: 
      QTWO() : AgBlock("QTWO","is the Q2 yoke"){ }; 
      ~QTWO(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(QTWO,1); 
   }; 
   // ---------------------------------------------------------------------- QTHR -- 
   ///@defgroup QTHR_doc 
   ///@class QTHR 
   ///@brief is the Q3 yoke 
   class QTHR : public AgBlock 
   {  public: 
      QTHR() : AgBlock("QTHR","is the Q3 yoke"){ }; 
      ~QTHR(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(QTHR,1); 
   }; 
   /// \class QuadGeo 
   /// \brief  is the description of all the magnets upstream inclusive of D0  
   class QuadGeo : public AgModule 
   { 
      public: 
      QuadGeo(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~QuadGeo(){ }; 
      ClassDef(QuadGeo,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace QuadGeo 
#endif // __QuadGeo__ 
