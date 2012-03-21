#include "TutrGeo2.h"  
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
 namespace TUTRGEO2 // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
       TutrGeo2::TutrGeo2()     
         : AgModule("TutrGeo2","Tutorial Geometry 1")     
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
                      _create = AgCreate("ATUB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ATUB              
                            Create("ATUB");               
                      }           
                      { AgPlacement place = AgPlacement("ATUB","TUTR");              
                            /// Add daughter volume ATUB to mother TUTR              
                            place.TranslateX(+25.0);              
                            /// Translate x = +25.0              
                            _stacker -> Position( AgBlock::Find("ATUB"), place );              
                      } // end placement of ATUB           
                      _create = AgCreate("ATBS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ATBS              
                            Create("ATBS");               
                      }           
                      { AgPlacement place = AgPlacement("ATBS","TUTR");              
                            /// Add daughter volume ATBS to mother TUTR              
                            place.TranslateX(+50.0);              
                            /// Translate x = +50.0              
                            _stacker -> Position( AgBlock::Find("ATBS"), place );              
                      } // end placement of ATBS           
                      _create = AgCreate("ACON");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ACON              
                            Create("ACON");               
                      }           
                      { AgPlacement place = AgPlacement("ACON","TUTR");              
                            /// Add daughter volume ACON to mother TUTR              
                            place.TranslateX(-25.0);              
                            /// Translate x = -25.0              
                            _stacker -> Position( AgBlock::Find("ACON"), place );              
                      } // end placement of ACON           
                      _create = AgCreate("ACNS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create ACNS              
                            Create("ACNS");               
                      }           
                      { AgPlacement place = AgPlacement("ACNS","TUTR");              
                            /// Add daughter volume ACNS to mother TUTR              
                            place.TranslateX(-50.0);              
                            /// Translate x = -50.0              
                            _stacker -> Position( AgBlock::Find("ACNS"), place );              
                      } // end placement of ACNS           
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
          // ---------------------------------------------------------------------------------------------------     
          void ATUB::Block( AgCreate create )     
          {         
                ///@addtogroup ATUB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("ATUB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.par("trans")=0;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=5.0;              
                            shape.par("rmax")=10.0;              
                            shape.par("dz")=12.5;              
                            /// Shape Tube rmin=5.0 rmax=10.0 dz=12.5               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ATUB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_ATUB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ATUB     
          // ---------------------------------------------------------------------------------------------------     
          void ATBS::Block( AgCreate create )     
          {         
                ///@addtogroup ATBS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("ATBS");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.par("trans")=0;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=5.0;              
                            shape.par("rmax")=10.0;              
                            shape.par("phi1")=-120.0;              
                            shape.par("phi2")=+120.0;              
                            shape.par("dz")=12.5;              
                            /// Shape Tubs rmin=5.0 rmax=10.0 phi1=-120.0 phi2=+120.0 dz=12.5               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ATBS;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_ATBS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ATBS     
          // ---------------------------------------------------------------------------------------------------     
          void ACON::Block( AgCreate create )     
          {         
                ///@addtogroup ACON_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("ACON");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.par("trans")=0;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Cone");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=12.5;              
                            shape.par("rmn1")=0.0;              
                            shape.par("rmx1")=10.0;              
                            shape.par("rmn2")=0.0;              
                            shape.par("rmx2")=0.0;              
                            /// Shape Cone dz=12.5 rmn1=0.0 rmx1=10.0 rmn2=0.0 rmx2=0.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ACON;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_ACON:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ACON     
          // ---------------------------------------------------------------------------------------------------     
          void ACNS::Block( AgCreate create )     
          {         
                ///@addtogroup ACNS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("ACNS");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.par("trans")=0;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Cons");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dz")=12.5;              
                            shape.par("rmn1")=0.0;              
                            shape.par("rmx1")=10.0;              
                            shape.par("rmn2")=0.0;              
                            shape.par("rmx2")=0.0;              
                            shape.par("phi1")=-120.0;              
                            shape.par("phi2")=+120.0;              
                            /// Shape Cons dz=12.5 rmn1=0.0 rmx1=10.0 rmn2=0.0 rmx2=0.0 phi1=-120.0 phi2=+120.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_ACNS;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_ACNS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block ACNS     
    // ----------------------------------------------------------------------- geoctr
       void TutrGeo2::ConstructGeometry()     
       {        
             ///@addtogroup TutrGeo2_revision        
             ///@{           
                   /// Author: A.N. Author           
             ///@}        
             ///@addtogroup TutrGeo2_revision        
             ///@{           
                   /// Created: Today           
             ///@}        
             AddBlock("TUTR");        
             AddBlock("ABOX");        
             AddBlock("ATUB");        
             AddBlock("ATBS");        
             AddBlock("ACON");        
             AddBlock("ACNS");        
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
       }; // TutrGeo2     
 }; // namespace TutrGeo2  
 