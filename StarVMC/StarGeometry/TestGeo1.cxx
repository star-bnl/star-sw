#include "TestGeo1.h"  
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
 namespace TESTGEO1 // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
       TestGeo1::TestGeo1()     
         : AgModule("TestGeo1","Test of paramterized placement of volumes")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void MAIN::Block( AgCreate create )     
          {         
                ///@addtogroup MAIN_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=100.0;              
                            shape.par("dy")=100.0;              
                            shape.par("dz")=100.0;              
                            /// Shape Bbox dx=100.0 dy=100.0 dz=100.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MAIN;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("TUBD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create TUBD              
                            Create("TUBD");               
                      }           
                      END_OF_MAIN:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MAIN     
          // ---------------------------------------------------------------------------------------------------     
          void TUBD::Block( AgCreate create )     
          {         
                ///@addtogroup TUBD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      {  AgShape shape = AgShape("Division");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("ndiv")=5;              
                            shape.par("iaxis")=2;              
                            /// Shape Division ndiv=5 iaxis=2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TUBD;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("TUBP");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create TUBP              
                            Create("TUBP");               
                      }           
                      { AgPlacement place = AgPlacement("TUBP","TUBD");              
                            /// Add daughter volume TUBP to mother TUBD              
                            _stacker -> Position( AgBlock::Find("TUBP"), place );              
                      } // end placement of TUBP           
                      END_OF_TUBD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TUBD     
          // ---------------------------------------------------------------------------------------------------     
          void TUBP::Block( AgCreate create )     
          {         
                ///@addtogroup TUBP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dy")=6.0;              
                            shape.par("dz")=50.0;              
                            /// Shape Bbox dy=6.0 dz=50.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TUBP;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("TUBE");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create TUBE              
                            Create("TUBE");               
                      }           
                      { AgPlacement place = AgPlacement("TUBE","TUBP");              
                            /// Add daughter volume TUBE to mother TUBP              
                            place.TranslateX(-100.0);              
                            /// Translate x = -100.0              
                            place.par("rmin")=0.0;              
                            place.par("rmax")=1.0;              
                            _stacker -> Position( AgBlock::Find("TUBE"), place );              
                      } // end placement of TUBE           
                      { AgPlacement place = AgPlacement("TUBE","TUBP");              
                            /// Add daughter volume TUBE to mother TUBP              
                            place.TranslateX(-80.0);              
                            /// Translate x = -80.0              
                            place.par("rmin")=0.0;              
                            place.par("rmax")=2.0;              
                            _stacker -> Position( AgBlock::Find("TUBE"), place );              
                      } // end placement of TUBE           
                      { AgPlacement place = AgPlacement("TUBE","TUBP");              
                            /// Add daughter volume TUBE to mother TUBP              
                            place.TranslateX(-60.0);              
                            /// Translate x = -60.0              
                            place.par("rmin")=0.0;              
                            place.par("rmax")=3.0;              
                            _stacker -> Position( AgBlock::Find("TUBE"), place );              
                      } // end placement of TUBE           
                      { AgPlacement place = AgPlacement("TUBE","TUBP");              
                            /// Add daughter volume TUBE to mother TUBP              
                            place.TranslateX(-40.0);              
                            /// Translate x = -40.0              
                            place.par("rmin")=0.0;              
                            place.par("rmax")=4.0;              
                            _stacker -> Position( AgBlock::Find("TUBE"), place );              
                      } // end placement of TUBE           
                      { AgPlacement place = AgPlacement("TUBE","TUBP");              
                            /// Add daughter volume TUBE to mother TUBP              
                            place.TranslateX(-20.0);              
                            /// Translate x = -20.0              
                            place.par("rmin")=0.0;              
                            place.par("rmax")=5.0;              
                            _stacker -> Position( AgBlock::Find("TUBE"), place );              
                      } // end placement of TUBE           
                      { AgPlacement place = AgPlacement("TUBE","TUBP");              
                            /// Add daughter volume TUBE to mother TUBP              
                            place.TranslateX(  0.0);              
                            /// Translate x =   0.0              
                            place.par("rmin")=0.0;              
                            place.par("rmax")=6.0;              
                            _stacker -> Position( AgBlock::Find("TUBE"), place );              
                      } // end placement of TUBE           
                      { AgPlacement place = AgPlacement("TUBE","TUBP");              
                            /// Add daughter volume TUBE to mother TUBP              
                            place.TranslateX(+20.0);              
                            /// Translate x = +20.0              
                            place.par("rmin")=0.0;              
                            place.par("rmax")=5.0;              
                            _stacker -> Position( AgBlock::Find("TUBE"), place );              
                      } // end placement of TUBE           
                      { AgPlacement place = AgPlacement("TUBE","TUBP");              
                            /// Add daughter volume TUBE to mother TUBP              
                            place.TranslateX(+40.0);              
                            /// Translate x = +40.0              
                            place.par("rmin")=0.0;              
                            place.par("rmax")=4.0;              
                            _stacker -> Position( AgBlock::Find("TUBE"), place );              
                      } // end placement of TUBE           
                      { AgPlacement place = AgPlacement("TUBE","TUBP");              
                            /// Add daughter volume TUBE to mother TUBP              
                            place.TranslateX(+60.0);              
                            /// Translate x = +60.0              
                            place.par("rmin")=0.0;              
                            place.par("rmax")=3.0;              
                            _stacker -> Position( AgBlock::Find("TUBE"), place );              
                      } // end placement of TUBE           
                      { AgPlacement place = AgPlacement("TUBE","TUBP");              
                            /// Add daughter volume TUBE to mother TUBP              
                            place.TranslateX(+80.0);              
                            /// Translate x = +80.0              
                            place.par("rmin")=0.0;              
                            place.par("rmax")=2.0;              
                            _stacker -> Position( AgBlock::Find("TUBE"), place );              
                      } // end placement of TUBE           
                      { AgPlacement place = AgPlacement("TUBE","TUBP");              
                            /// Add daughter volume TUBE to mother TUBP              
                            place.TranslateX(+100.0);              
                            /// Translate x = +100.0              
                            place.par("rmin")=0.0;              
                            place.par("rmax")=1.0;              
                            _stacker -> Position( AgBlock::Find("TUBE"), place );              
                      } // end placement of TUBE           
                      END_OF_TUBP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TUBP     
          // ---------------------------------------------------------------------------------------------------     
          void TUBE::Block( AgCreate create )     
          {         
                ///@addtogroup TUBE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("TUBE");              
                            attr.par("colo")=44;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0.0;              
                            shape.par("rmax")=0.0;              
                            shape.par("dz")=0.0;              
                            /// Shape Tube rmin=0.0 rmax=0.0 dz=0.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_TUBE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_TUBE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block TUBE     
    // ----------------------------------------------------------------------- geoctr
       void TestGeo1::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup TestGeo1_revision        
             ///@{           
                   /// Author: JCW           
             ///@}        
             ///@addtogroup TestGeo1_revision        
             ///@{           
                   /// Created: Today           
             ///@}        
             AddBlock("MAIN");        
             AddBlock("TUBE");        
             AddBlock("TUBP");        
             AddBlock("TUBD");        
             _create = AgCreate("MAIN");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create MAIN           
                   Create("MAIN");            
             }        
             { AgPlacement place = AgPlacement("MAIN","CAVE");           
                   /// Add daughter volume MAIN to mother CAVE           
                   _stacker -> Position( AgBlock::Find("MAIN"), place );           
             } // end placement of MAIN        
       }; // TestGeo1     
 }; // namespace TestGeo1  
 