#include "TutrGeo1.h"  
 // ---------------------------------------------------------------------------------------------------  
 //  
 #include "StarVMC/StarAgmlLib/StarAgmlStacker.h"  
 //  
 #include "StarVMC/StarAgmlLib/AgMaterial.h"  
 #include "StarVMC/StarAgmlLib/AgMedium.h"  
 #include "StarVMC/StarAgmlLib/AgShape.h"  
 #include "StarVMC/StarAgmlLib/AgBlock.h"  
 #include "StarVMC/StarAgmlLib/AgMath.h"  
 #include "StarVMC/StarAgmlLib/AgSTAR.h"  
 //  
 #include "StarVMC/StarAgmlLib/Mortran.h"  
 #include "StarVMC/StarAgmlLib/AgMath.h"  
 #include <iostream>  
 #include <vector>  
 #include <map>  
 const Int_t _printlevel = 0;  
 #define LOG_PRINT if(_printlevel>0) std::cout << GetName() << " -Print- "  
 #define LOG_INFO  if(_printlevel>1) std::cout << GetName() << " -Info-  "  
 #define LOG_DEBUG if(_printlevel>2) std::cout << GetName() << " -Debug- "  
 #define LOG_WARN  if(_printlevel>3) std::cout << GetName() << " -Warn-  "  
 #define printf(fmt,...) LOG_PRINT << Form(fmt,##__VA_ARGS__) << std::endl;  
 #include "StarVMC/Geometry/Helpers.h"  
 //  
 namespace TUTRGEO1 // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
       TutrGeo1::TutrGeo1()     
         : AgModule("TutrGeo1","Tutorial Geometry 1")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void TUTR::Block( AgCreate create )     
          {         
                ///@addtogroup TUTR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Medium Standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("None");              
                            attr.par("seen")=0;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=500.0;              
                            shape.par("dy")=500.0;              
                            shape.par("dz")=500.0;              
                            /// Shape Bbox dx=500.0 dy=500.0 dz=500.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TUTR;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("ABOX");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ABOX              
                            Create("ABOX");               
                      }           
                      { AgPlacement place = AgPlacement("ABOX","TUTR");              
                            /// Add daughter volume ABOX to mother TUTR              
                            _stacker -> Position( AgBlock::Find("ABOX"), place );              
                      } // end placement of ABOX           
                      END_OF_TUTR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TUTR     
          // ---------------------------------------------------------------------------------------------------     
          void ABOX::Block( AgCreate create )     
          {         
                ///@addtogroup ABOX_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("None");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=10;              
                            shape.par("dy")=10;              
                            shape.par("dz")=10;              
                            /// Shape Bbox dx=10 dy=10 dz=10               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ABOX;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_ABOX:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ABOX     
    // ----------------------------------------------------------------------- geoctr
       void TutrGeo1::ConstructGeometry()     
       {        
             ///@addtogroup TutrGeo1_revision        
             ///@{           
                   /// Author: A.N. Author           
             ///@}        
             ///@addtogroup TutrGeo1_revision        
             ///@{           
                   /// Created: Today           
             ///@}        
             AddBlock("TUTR");        
             AddBlock("ABOX");        
             _create = AgCreate("TUTR");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create TUTR           
                   Create("TUTR");            
             }        
             { AgPlacement place = AgPlacement("TUTR","CAVE");           
                   /// Add daughter volume TUTR to mother CAVE           
                   _stacker -> Position( AgBlock::Find("TUTR"), place );           
             } // end placement of TUTR        
       }; // TutrGeo1     
 }; // namespace TutrGeo1  
 