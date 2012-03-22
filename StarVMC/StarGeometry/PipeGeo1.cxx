#include "PipeGeo1.h"  
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
 namespace PIPEGEO1 // $NMSPC  
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
          ///Int_t config;     
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
          ///Float_t version;     
          ///Float_t zoffset;     
          ///Float_t yoffset;     
          ///Float_t xoffset;     
          ///Float_t zoffber;     
          ///Float_t vacdens;     
          ///Int_t _index;     
          //     
          Pipg_t pipg;     
          //     
       PipeGeo1::PipeGeo1()     
         : AgModule("PipeGeo1","Beam pipe in y2013 and beyond")     
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
                      { AgAttribute attr = AgAttribute("PIPE");              
                            attr.par("seen")=0;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pcon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=0;              
                            shape.par("dphi")=360;              
                            shape.par("nz")=14;              
                            shape.Z(0)=-55.71 * inch;              
                            shape.Z(1)=                          -54.71 * inch;              
                            shape.Z(2)=                          -54.71 * inch;              
                            shape.Z(3)=                          -43.71 * inch;              
                            shape.Z(4)=                          -43.71 * inch;              
                            shape.Z(5)=                           -15.75 * inch;              
                            shape.Z(6)=                          -15.75 * inch;              
                            shape.Z(7)=                           31.5  * inch;              
                            shape.Z(8)=                           31.5  * inch;              
                            shape.Z(9)=                           43.72 * inch;              
                            shape.Z(10)=                           43.72 * inch;              
                            shape.Z(11)=                           54.71 * inch;              
                            shape.Z(12)=                           54.71 * inch;              
                            shape.Z(13)=                           55.71 * inch ;              
                            shape.Rmin(0)=0;              
                            shape.Rmin(1)=0;              
                            shape.Rmin(2)=0;              
                            shape.Rmin(3)=0;              
                            shape.Rmin(4)=0;              
                            shape.Rmin(5)=0;              
                            shape.Rmin(6)=0;              
                            shape.Rmin(7)=0;              
                            shape.Rmin(8)=0;              
                            shape.Rmin(9)=0;              
                            shape.Rmin(10)=0;              
                            shape.Rmin(11)=0;              
                            shape.Rmin(12)=0;              
                            shape.Rmin(13)=0;              
                            shape.Rmax(0)=0.7875*inch+0.5875*inch;              
                            shape.Rmax(1)=                          0.7875*inch+0.5875*inch;              
                            shape.Rmax(2)=                          0.7875*inch+0.065 *inch;              
                            shape.Rmax(3)=                          0.7875*inch+0.065 *inch;              
                            shape.Rmax(4)=                          0.7875*inch+0.055 *inch;              
                            shape.Rmax(5)=                          0.7875*inch+0.055 *inch;              
                            shape.Rmax(6)=                         0.7875*inch+0.030 *inch;              
                            shape.Rmax(7)=                          0.7875*inch+0.030 *inch;              
                            shape.Rmax(8)=                          0.7875*inch+0.055 *inch;              
                            shape.Rmax(9)=                          0.7875*inch+0.055 *inch;              
                            shape.Rmax(10)=                         0.7875*inch+0.065 *inch;              
                            shape.Rmax(11)=                          0.7875*inch+0.065 *inch;              
                            shape.Rmax(12)=                          0.7875*inch+0.5875*inch;              
                            shape.Rmax(13)=                          0.7875*inch+0.5875*inch;              
                            /// Shape Pcon phi1=0 dphi=360 nz=14               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PIPE;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PALS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PALS              
                            Create("PALS");               
                      }           
                      { AgPlacement place = AgPlacement("PALS","PIPE");              
                            /// Add daughter volume PALS to mother PIPE              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("PALS"), place );              
                      } // end placement of PALS           
                      _create = AgCreate("PBES");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PBES              
                            Create("PBES");               
                      }           
                      { AgPlacement place = AgPlacement("PBES","PIPE");              
                            /// Add daughter volume PBES to mother PIPE              
                            place.TranslateZ(pipg.zoffber);              
                            /// Translate z = pipg.zoffber              
                            place.par("only")=AgPlacement::kOnly;              
                            /// Overlap: agplacement::konly              
                            _stacker -> Position( AgBlock::Find("PBES"), place );              
                      } // end placement of PBES           
                      END_OF_PIPE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PIPE     
          // ---------------------------------------------------------------------------------------------------     
          void PALS::Block( AgCreate create )     
          {         
                ///@addtogroup PALS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PALS");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pcon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=0;              
                            shape.par("dphi")=360;              
                            shape.par("nz")=14;              
                            shape.Z(0)=-55.71*inch;              
                            shape.Z(1)= -54.71*inch;              
                            shape.Z(2)=                          -54.71*inch;              
                            shape.Z(3)= -43.71*inch;              
                            shape.Z(4)=                          -43.71*inch;              
                            shape.Z(5)= -15.75*inch;              
                            shape.Z(6)=                          -15.75*inch;              
                            shape.Z(7)=  31.5 *inch;              
                            shape.Z(8)=                           31.5 *inch;              
                            shape.Z(9)=  43.72*inch;              
                            shape.Z(10)=                           43.72*inch;              
                            shape.Z(11)=  54.71*inch;              
                            shape.Z(12)=                           54.71*inch;              
                            shape.Z(13)=  55.71*inch ;              
                            shape.Rmin(0)=0.7875*inch;              
                            shape.Rmin(1)= 0.7875*inch;              
                            shape.Rmin(2)=0.7875*inch;              
                            shape.Rmin(3)= 0.7875*inch;              
                            shape.Rmin(4)= 0.7875*inch;              
                            shape.Rmin(5)= 0.7875*inch;              
                            shape.Rmin(6)= 0.7875*inch;              
                            shape.Rmin(7)= 0.7875*inch;              
                            shape.Rmin(8)= 0.7875*inch;              
                            shape.Rmin(9)= 0.7875*inch;              
                            shape.Rmin(10)= 0.7875*inch;              
                            shape.Rmin(11)= 0.7875*inch;              
                            shape.Rmin(12)= 0.7875*inch;              
                            shape.Rmin(13)= 0.7875*inch;              
                            shape.Rmax(0)=0.7875*inch+0.5875*inch;              
                            shape.Rmax(1)= 0.7875*inch+0.5875*inch;              
                            shape.Rmax(2)= 0.7875*inch+0.065*inch;              
                            shape.Rmax(3)= 0.7875*inch+0.065*inch;              
                            shape.Rmax(4)= 0.7875*inch+0.055*inch;              
                            shape.Rmax(5)= 0.7875*inch+0.055*inch;              
                            shape.Rmax(6)= 0.7875*inch+0.030*inch;              
                            shape.Rmax(7)= 0.7875*inch+0.030*inch;              
                            shape.Rmax(8)= 0.7875*inch+0.055*inch;              
                            shape.Rmax(9)= 0.7875*inch+0.055*inch;              
                            shape.Rmax(10)= 0.7875*inch+0.065*inch;              
                            shape.Rmax(11)= 0.7875*inch+0.065*inch;              
                            shape.Rmax(12)= 0.7875*inch+0.5875*inch;              
                            shape.Rmax(13)= 0.7875*inch+0.5875*inch;              
                            /// Shape Pcon phi1=0 dphi=360 nz=14               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PALS;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PALH");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PALH              
                            Create("PALH");               
                      }           
                      { AgPlacement place = AgPlacement("PALH","PIPE");              
                            /// Add daughter volume PALH to mother PIPE              
                            _stacker -> Position( AgBlock::Find("PALH"), place );              
                      } // end placement of PALH           
                      END_OF_PALS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PALS     
          // ---------------------------------------------------------------------------------------------------     
          void PALH::Block( AgCreate create )     
          {         
                ///@addtogroup PALH_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Material vacuum dens=pipg_vacdens            
                      { AgMaterial &mat = AgMaterial::Get("Vacuum");              
                            mat.par("dens")=pipg.vacdens;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PALH");              
                            attr.par("seen")=0;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pcon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=0;              
                            shape.par("dphi")=360;              
                            shape.par("nz")=14;              
                            shape.Z(0)=-55.71*inch;              
                            shape.Z(1)= -54.71*inch;              
                            shape.Z(2)=     -54.71*inch;              
                            shape.Z(3)= -43.71*inch;              
                            shape.Z(4)=                          -43.71*inch;              
                            shape.Z(5)= -15.75*inch;              
                            shape.Z(6)=                          -15.75*inch;              
                            shape.Z(7)=  31.5 *inch;              
                            shape.Z(8)=                           31.5 *inch;              
                            shape.Z(9)=  43.72*inch;              
                            shape.Z(10)=                           43.72*inch;              
                            shape.Z(11)=  54.71*inch;              
                            shape.Z(12)=                           54.71*inch;              
                            shape.Z(13)=  55.71*inch ;              
                            shape.Rmin(0)=0;              
                            shape.Rmin(1)=0;              
                            shape.Rmin(2)=0;              
                            shape.Rmin(3)=0;              
                            shape.Rmin(4)=0;              
                            shape.Rmin(5)=0;              
                            shape.Rmin(6)=0;              
                            shape.Rmin(7)=0;              
                            shape.Rmin(8)=0;              
                            shape.Rmin(9)=0;              
                            shape.Rmin(10)=0;              
                            shape.Rmin(11)=0;              
                            shape.Rmin(12)=0;              
                            shape.Rmin(13)=0;              
                            shape.Rmax(0)=0.7875*inch;              
                            shape.Rmax(1)= 0.7875*inch;              
                            shape.Rmax(2)=0.7875*inch;              
                            shape.Rmax(3)= 0.7875*inch;              
                            shape.Rmax(4)= 0.7875*inch;              
                            shape.Rmax(5)= 0.7875*inch;              
                            shape.Rmax(6)= 0.7875*inch;              
                            shape.Rmax(7)= 0.7875*inch;              
                            shape.Rmax(8)= 0.7875*inch;              
                            shape.Rmax(9)= 0.7875*inch;              
                            shape.Rmax(10)= 0.7875*inch;              
                            shape.Rmax(11)= 0.7875*inch;              
                            shape.Rmax(12)= 0.7875*inch;              
                            shape.Rmax(13)= 0.7875*inch;              
                            /// Shape Pcon phi1=0 dphi=360 nz=14               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PALH;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PALH:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PALH     
          // ---------------------------------------------------------------------------------------------------     
          void PBES::Block( AgCreate create )     
          {         
                ///@addtogroup PBES_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Berillium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Berillium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PBES");              
                            attr.par("seen")=0;              
                            attr.par("colo")=0;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0.7875*inch;              
                            shape.par("rmax")=0.7875*inch + 0.030*inch;              
                            shape.par("dz")=47.25*inch/2;              
                            /// Shape Tube rmin=0.7875*inch rmax=0.7875*inch + 0.030*inch dz=47.25*inch/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PBES;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PBES:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PBES     
    // ----------------------------------------------------------------------- geoctr
       void PipeGeo1::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup PipeGeo1_revision        
             ///@{           
                   /// Author: Amilkar Quintero           
             ///@}        
             ///@addtogroup PipeGeo1_revision        
             ///@{           
                   /// Created: 29/Jun/2011           
             ///@}        
             AddBlock("PIPE");        
             AddBlock("PALS");        
             AddBlock("PALH");        
             AddBlock("PBES");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pipv_doc        
             ///@{           
                   ++pipv._index;           
                   pipv . version = 1.0; // 2.0 version of the beam pipe           
                   /// pipv . version = 1.0; // 2.0 version of the beam pipe           
                   pipv . config = 1; // Configuration is one           
                   /// pipv . config = 1; // Configuration is one           
                   //           
                   pipv.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pipg_doc        
             ///@{           
                   ++pipg._index;           
                   pipg . version = 1.0; // Default position and geometry           
                   /// pipg . version = 1.0; // Default position and geometry           
                   pipg . vacdens = 1.25e-8; // Vacuum density... needs to be verified           
                   /// pipg . vacdens = 1.25e-8; // Vacuum density... needs to be verified           
                   pipg . xoffset = 0.0; // Default x position            
                   /// pipg . xoffset = 0.0; // Default x position            
                   pipg . yoffset = 0.0; // Default y position            
                   /// pipg . yoffset = 0.0; // Default y position            
                   pipg . zoffset = 0.0; // Default z position            
                   /// pipg . zoffset = 0.0; // Default z position            
                   pipg . zoffber = -15.75*inch; // Offset of beryllium as per drawing           
                   /// pipg . zoffber = -15.75*inch; // Offset of beryllium as per drawing           
                   //           
                   pipg.fill();           
             ///@}        
             //        
             _create = AgCreate("PIPE");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create PIPE           
                   Create("PIPE");            
             }        
             { AgPlacement place = AgPlacement("PIPE","CAVE");           
                   /// Add daughter volume PIPE to mother CAVE           
                   place.TranslateX(pipg.xoffset);           
                   /// Translate x = pipg.xoffset           
                   place.TranslateY(pipg.yoffset);           
                   /// Translate y = pipg.yoffset           
                   place.TranslateZ(pipg.zoffset);           
                   /// Translate z = pipg.zoffset           
                   place.AlphaX(180);           
                   /// Rotate: AlphaX = 180           
                   /// G3 Reference: thetax = 90           
                   /// G3 Reference: phix = 0           
                   /// G3 Reference: thetay = 90           
                   /// G3 Reference: phiy = 90           
                   /// G3 Reference: thetaz = 0           
                   /// G3 Reference: phiz = 0           
                   _stacker -> Position( AgBlock::Find("PIPE"), place );           
             } // end placement of PIPE        
       }; // PipeGeo1     
 }; // namespace PipeGeo1  
 