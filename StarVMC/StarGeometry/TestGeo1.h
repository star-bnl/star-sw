#ifndef __TestGeo1__ 
#define __TestGeo1__ 
 
#include "StarVMC/StarAgmlLib/AgModule.h" 
 
namespace TESTGEO1 // $NMSPC 
{ 
   // ---------------------------------------------------------------------- MAIN -- 
   ///@defgroup MAIN_doc 
   ///@class MAIN 
   ///@brief A master volume 
   class MAIN : public AgBlock 
   {  public: 
      MAIN() : AgBlock("MAIN","A master volume"){ }; 
      ~MAIN(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(MAIN,1); 
   }; 
   // ---------------------------------------------------------------------- TUBD -- 
   ///@defgroup TUBD_doc 
   ///@class TUBD 
   ///@brief A division of the mother volume along Y-axis 
   class TUBD : public AgBlock 
   {  public: 
      TUBD() : AgBlock("TUBD","A division of the mother volume along Y-axis"){ }; 
      ~TUBD(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TUBD,1); 
   }; 
   // ---------------------------------------------------------------------- TUBP -- 
   ///@defgroup TUBP_doc 
   ///@class TUBP 
   ///@brief A parent volume for the tubes 
   class TUBP : public AgBlock 
   {  public: 
      TUBP() : AgBlock("TUBP","A parent volume for the tubes"){ }; 
      ~TUBP(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TUBP,1); 
   }; 
   // ---------------------------------------------------------------------- TUBE -- 
   ///@defgroup TUBE_doc 
   ///@class TUBE 
   ///@brief A sub volume setup for shape defintion when positioned 
   class TUBE : public AgBlock 
   {  public: 
      TUBE() : AgBlock("TUBE","A sub volume setup for shape defintion when positioned"){ }; 
      ~TUBE(){ }; 
      virtual void Block( AgCreate c ); 
      virtual void End(){ }; 
      ClassDef(TUBE,1); 
   }; 
   /// \class TestGeo1 
   /// \brief Test of paramterized placement of volumes 
   class TestGeo1 : public AgModule 
   { 
      public: 
      TestGeo1(); 
      virtual void ConstructGeometry( const Char_t *dummy="" ); 
      ~TestGeo1(){ }; 
      ClassDef(TestGeo1,1); 
      public: 
   }; 
   // endElement in class Module 
}; // namespace TestGeo1 
#endif // __TestGeo1__ 
