#include "PipeGeo00.h"  
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
 namespace PIPEGEO00 // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup pipv_doc     
          /// \class Pipv_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t pipeconfig;     
          ///Int_t _index;     
          //     
          Pipv_t pipv;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup pipg_doc     
          /// \class Pipg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t config;     
          ///Float_t beinnr;     
          ///Float_t beoutr;     
          ///Float_t beleng;     
          ///Int_t _index;     
          //     
          Pipg_t pipg;     
          //     
          ///@addtogroup PipeGeo00_vars     
          ///@{        
                Float_t vacuum;        
                //        
                /// Float_t vacuum        
          ///@}     
       PipeGeo00::PipeGeo00()     
         : AgModule("PipeGeo00"," is the SIMPLIFIED geometry  of the STAR beam pipe. ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void PIPE::Block( AgCreate create )     
          {         
                ///@addtogroup PIPE_doc        
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
                      { AgAttribute attr = AgAttribute("Pipe");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=pipg.beoutr;              
                            shape.par("dz")=pipg.beleng/2;              
                            /// Shape Tube rmin=0 rmax=pipg.beoutr dz=pipg.beleng/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PIPE;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PIPC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PIPC              
                            Create("PIPC");               
                      }           
                      { AgPlacement place = AgPlacement("PIPC","PIPE");              
                            /// Add daughter volume PIPC to mother PIPE              
                            _stacker -> Position( AgBlock::Find("PIPC"), place );              
                      } // end placement of PIPC           
                      _create = AgCreate("PVAC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PVAC              
                            Create("PVAC");               
                      }           
                      { AgPlacement place = AgPlacement("PVAC","PIPE");              
                            /// Add daughter volume PVAC to mother PIPE              
                            _stacker -> Position( AgBlock::Find("PVAC"), place );              
                      } // end placement of PVAC           
                      END_OF_PIPE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PIPE     
          // ---------------------------------------------------------------------------------------------------     
          void PIPC::Block( AgCreate create )     
          {         
                ///@addtogroup PIPC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Berillium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Berillium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("Pipc");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=pipg.beinnr;              
                            shape.par("rmax")=pipg.beoutr;              
                            shape.par("dz")=pipg.beleng/2;              
                            /// Shape Tube rmin=pipg.beinnr rmax=pipg.beoutr dz=pipg.beleng/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PIPC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PIPC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PIPC     
          // ---------------------------------------------------------------------------------------------------     
          void PVAC::Block( AgCreate create )     
          {         
                ///@addtogroup PVAC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("Pipc");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material PVacuum dens=(_material.par("dens")) *vacuum absl=(_material.par("absl")) /vacuum radl=(_material.par("radl")) /vacuum            
                      { AgMaterial &mat = AgMaterial::Get("Pvacuum");              
                            mat.par("dens")=(_material.par("dens")) *vacuum;              
                            mat.par("absl")=(_material.par("absl")) /vacuum;              
                            mat.par("radl")=(_material.par("radl")) /vacuum;              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=pipg.beinnr;              
                            shape.par("dz")=pipg.beleng/2.0;              
                            /// Shape Tube rmin=0 rmax=pipg.beinnr dz=pipg.beleng/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PVAC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PVAC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PVAC     
    // ----------------------------------------------------------------------- geoctr
       void PipeGeo00::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup PipeGeo00_revision        
             ///@{           
                   /// Created:  03/17/08            
             ///@}        
             ///@addtogroup PipeGeo00_revision        
             ///@{           
                   /// Author: Gerrit van Nieuwenhuizen           
             ///@}        
             AddBlock("PIPE");        
             AddBlock("PIPC");        
             AddBlock("PVAC");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pipv_doc        
             ///@{           
                   ++pipv._index;           
                   pipv . version = 1; //  geometry version            
                   /// pipv . version = 1; //  geometry version            
                   pipv . pipeconfig = 0; //  pipe version            
                   /// pipv . pipeconfig = 0; //  pipe version            
                   //           
                   pipv.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pipg_doc        
             ///@{           
                   ++pipg._index;           
                   pipg . config = 0; //  both material and geo params            
                   /// pipg . config = 0; //  both material and geo params            
                   pipg . beinnr = 1.9619; //  Berillium section inner radius            
                   /// pipg . beinnr = 1.9619; //  Berillium section inner radius            
                   pipg . beoutr = 2.0381; //  Berillium section outer radius            
                   /// pipg . beoutr = 2.0381; //  Berillium section outer radius            
                   pipg . beleng = 200.0; //  Berillium section length            
                   /// pipg . beleng = 200.0; //  Berillium section length            
                   //           
                   pipg.fill();           
             ///@}        
             //        
             vacuum = 1.0e-5;        
             /// USE pipv _index=1;        
             pipv.Use();        
             /// USE pipg config=pipv.pipeconfig;        
             pipg.Use("config",(Float_t)pipv.pipeconfig);        
             _create = AgCreate("PIPE");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create PIPE           
                   Create("PIPE");            
             }        
             { AgPlacement place = AgPlacement("PIPE","CAVE");           
                   /// Add daughter volume PIPE to mother CAVE           
                   _stacker -> Position( AgBlock::Find("PIPE"), place );           
             } // end placement of PIPE        
       }; // PipeGeo00     
 }; // namespace PipeGeo00  
 