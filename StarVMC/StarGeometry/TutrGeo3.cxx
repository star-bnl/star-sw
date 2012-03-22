#include "TutrGeo3.h"  
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
 namespace TUTRGEO3 // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          ///@addtogroup TutrGeo3_vars     
          ///@{        
                float boxdx=10.0,boxdy=10.0,boxdz=10.0;        
                //        
                /// float boxdx=10.0,boxdy=10.0,boxdz=10.0        
          ///@}     
          //  -----------------------------------------------------     
          /// @defgroup tubg_doc     
          /// \class Tubg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Int_t version;     
          ///Float_t rmin;     
          ///Float_t rmax;     
          ///Float_t dz;     
          ///Int_t _index;     
          //     
          Tubg_t tubg;     
          //     
       TutrGeo3::TutrGeo3()     
         : AgModule("TutrGeo3","Tutorial Geometry 1")     
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
                      boxdy=15.0;;           
                      boxdz= 5.0;;           
                      _create = AgCreate("ABOX");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ABOX              
                            Create("ABOX");               
                      }           
                      { AgPlacement place = AgPlacement("ABOX","TUTR");              
                            /// Add daughter volume ABOX to mother TUTR              
                            place.TranslateX(+25.0);              
                            /// Translate x = +25.0              
                            _stacker -> Position( AgBlock::Find("ABOX"), place );              
                      } // end placement of ABOX           
                      boxdy=5.0;;           
                      boxdz=5.0;;           
                      _create = AgCreate("ABOX");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ABOX              
                            Create("ABOX");               
                      }           
                      { AgPlacement place = AgPlacement("ABOX","TUTR");              
                            /// Add daughter volume ABOX to mother TUTR              
                            place.TranslateX(+50.0);              
                            /// Translate x = +50.0              
                            _stacker -> Position( AgBlock::Find("ABOX"), place );              
                      } // end placement of ABOX           
                      boxdy=15.0;;           
                      boxdz= 5.0;;           
                      _create = AgCreate("ABOX");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ABOX              
                            Create("ABOX");               
                      }           
                      { AgPlacement place = AgPlacement("ABOX","TUTR");              
                            /// Add daughter volume ABOX to mother TUTR              
                            place.TranslateX(-25.0);              
                            /// Translate x = -25.0              
                            _stacker -> Position( AgBlock::Find("ABOX"), place );              
                      } // end placement of ABOX           
                      boxdy=5.0;;           
                      boxdz=5.0;;           
                      _create = AgCreate("ABOX");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ABOX              
                            Create("ABOX");               
                      }           
                      { AgPlacement place = AgPlacement("ABOX","TUTR");              
                            /// Add daughter volume ABOX to mother TUTR              
                            place.TranslateX(-50.0);              
                            /// Translate x = -50.0              
                            _stacker -> Position( AgBlock::Find("ABOX"), place );              
                      } // end placement of ABOX           
                      /// USE tubg version=1;           
                      tubg.Use("version",(Int_t)1);           
                      _create = AgCreate("ATUB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ATUB              
                            Create("ATUB");               
                      }           
                      { AgPlacement place = AgPlacement("ATUB","TUTR");              
                            /// Add daughter volume ATUB to mother TUTR              
                            place.TranslateY(-25.0);              
                            /// Translate y = -25.0              
                            _stacker -> Position( AgBlock::Find("ATUB"), place );              
                      } // end placement of ATUB           
                      /// USE tubg version=2;           
                      tubg.Use("version",(Int_t)2);           
                      _create = AgCreate("ATUB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ATUB              
                            Create("ATUB");               
                      }           
                      { AgPlacement place = AgPlacement("ATUB","TUTR");              
                            /// Add daughter volume ATUB to mother TUTR              
                            place.TranslateX(+25);              
                            /// Translate x = +25              
                            place.TranslateY(-25.0);              
                            /// Translate y = -25.0              
                            _stacker -> Position( AgBlock::Find("ATUB"), place );              
                      } // end placement of ATUB           
                      { AgPlacement place = AgPlacement("ATUB","TUTR");              
                            /// Add daughter volume ATUB to mother TUTR              
                            place.TranslateX(-25);              
                            /// Translate x = -25              
                            place.TranslateY(-25.0);              
                            /// Translate y = -25.0              
                            _stacker -> Position( AgBlock::Find("ATUB"), place );              
                      } // end placement of ATUB           
                      /// USE tubg version=3;           
                      tubg.Use("version",(Int_t)3);           
                      _create = AgCreate("ATUB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ATUB              
                            Create("ATUB");               
                      }           
                      { AgPlacement place = AgPlacement("ATUB","TUTR");              
                            /// Add daughter volume ATUB to mother TUTR              
                            place.TranslateX(+50);              
                            /// Translate x = +50              
                            place.TranslateY(-25.0);              
                            /// Translate y = -25.0              
                            _stacker -> Position( AgBlock::Find("ATUB"), place );              
                      } // end placement of ATUB           
                      { AgPlacement place = AgPlacement("ATUB","TUTR");              
                            /// Add daughter volume ATUB to mother TUTR              
                            place.TranslateX(-50);              
                            /// Translate x = -50              
                            place.TranslateY(-25.0);              
                            /// Translate y = -25.0              
                            _stacker -> Position( AgBlock::Find("ATUB"), place );              
                      } // end placement of ATUB           
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
                            attr.par("trans")=0;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=boxdx;              
                            shape.par("dy")=boxdy;              
                            shape.par("dz")=boxdz;              
                            /// Shape Bbox dx=boxdx dy=boxdy dz=boxdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ABOX;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_ABOX:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ABOX     
          // ---------------------------------------------------------------------------------------------------     
          void ATUB::Block( AgCreate create )     
          {         
                ///@addtogroup ATUB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("None");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.par("trans")=0;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=tubg.rmin;              
                            shape.par("rmax")=tubg.rmax;              
                            shape.par("dz")=tubg.dz;              
                            /// Shape Tube rmin=tubg.rmin rmax=tubg.rmax dz=tubg.dz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ATUB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_ATUB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ATUB     
    // ----------------------------------------------------------------------- geoctr
       void TutrGeo3::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup TutrGeo3_revision        
             ///@{           
                   /// Author: A.N. Author           
             ///@}        
             ///@addtogroup TutrGeo3_revision        
             ///@{           
                   /// Created: Today           
             ///@}        
             AddBlock("TUTR");        
             AddBlock("ABOX");        
             AddBlock("ATUB");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup tubg_doc        
             ///@{           
                   ++tubg._index;           
                   tubg . version = 1; // 1st tube           
                   /// tubg . version = 1; // 1st tube           
                   tubg . rmin = 0; // no hole           
                   /// tubg . rmin = 0; // no hole           
                   tubg . rmax = 5; // 5cm           
                   /// tubg . rmax = 5; // 5cm           
                   tubg . dz = 10; // 10cm           
                   /// tubg . dz = 10; // 10cm           
                   //           
                   tubg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup tubg_doc        
             ///@{           
                   ++tubg._index;           
                   tubg . version = 2; // 2nd tube           
                   /// tubg . version = 2; // 2nd tube           
                   tubg . rmin = 2.5; // a hole           
                   /// tubg . rmin = 2.5; // a hole           
                   tubg . rmax = 7.5; // 7.5cm           
                   /// tubg . rmax = 7.5; // 7.5cm           
                   tubg . dz = 10; // 10cm           
                   /// tubg . dz = 10; // 10cm           
                   //           
                   tubg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup tubg_doc        
             ///@{           
                   ++tubg._index;           
                   tubg . version = 2; // 3rd tube           
                   /// tubg . version = 2; // 3rd tube           
                   tubg . rmin = 5.0; // a hole           
                   /// tubg . rmin = 5.0; // a hole           
                   tubg . rmax = 10.0; // 10cm           
                   /// tubg . rmax = 10.0; // 10cm           
                   tubg . dz = 15; // 15cm           
                   /// tubg . dz = 15; // 15cm           
                   //           
                   tubg.fill();           
             ///@}        
             //        
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
       }; // TutrGeo3     
 }; // namespace TutrGeo3  
 