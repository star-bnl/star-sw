#include "PixlGeo4.h"  
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
 namespace PIXLGEO4 // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float angle,anglepos,anglecorr,raddeg;        
                //        
                /// float angle,anglepos,anglecorr,raddeg        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                int nladder,nsector,nextraladder;        
                //        
                /// int nladder,nsector,nextraladder        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float ladderradius,ladderthk,ladderwidth;        
                //        
                /// float ladderradius,ladderthk,ladderwidth        
          ///@}     
          //  -----------------------------------------------------     
          /// @defgroup pxld_doc     
          /// \class Pxld_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t subversion;     
          ///Float_t totallength;     
          ///Float_t passivethk;     
          ///Float_t activethk;     
          ///Float_t layerthk;     
          ///Float_t rin;     
          ///Float_t rout;     
          ///Int_t _index;     
          //     
          Pxld_t pxld;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup pixg_doc     
          /// \class Pixg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t layer;     
          ///Float_t noladders;     
          ///Float_t r;     
          ///Float_t a;     
          ///Float_t poffset;     
          ///Float_t aoffset;     
          ///Int_t _index;     
          //     
          Pixg_t pixg;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup pxbg_doc     
          /// \class Pxbg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t length;     
          ///Float_t rin;     
          ///Float_t thk;     
          ///Int_t _index;     
          //     
          Pxbg_t pxbg;     
          //     
       PixlGeo4::PixlGeo4()     
         : AgModule("PixlGeo4","is the SIMPLIFIED pixel detector ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void PXMO::Block( AgCreate create )     
          {         
                ///@addtogroup PXMO_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PXMO");              
                            attr.par("seen")=0;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=pxld.rin-pxld.layerthk/2.0;              
                            shape.par("rmax")=pxld.rout+pxld.layerthk/2.0;              
                            shape.par("dz")=pxld.totallength/2.0;              
                            /// Shape Tube rmin=pxld.rin-pxld.layerthk/2.0 rmax=pxld.rout+pxld.layerthk/2.0 dz=pxld.totallength/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXMO;              
                            _stacker -> Build(this);              
                      }           
                      /// USE pixg layer=1 ;           
                      pixg.Use("layer",(Float_t)1 );           
                      _create = AgCreate("PXLA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXLA              
                            Create("PXLA");               
                      }           
                      { AgPlacement place = AgPlacement("PXLA","PXMO");              
                            /// Add daughter volume PXLA to mother PXMO              
                            _stacker -> Position( AgBlock::Find("PXLA"), place );              
                      } // end placement of PXLA           
                      /// USE pixg layer=2 ;           
                      pixg.Use("layer",(Float_t)2 );           
                      _create = AgCreate("PXLA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXLA              
                            Create("PXLA");               
                      }           
                      { AgPlacement place = AgPlacement("PXLA","PXMO");              
                            /// Add daughter volume PXLA to mother PXMO              
                            _stacker -> Position( AgBlock::Find("PXLA"), place );              
                      } // end placement of PXLA           
                      END_OF_PXMO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXMO     
          // ---------------------------------------------------------------------------------------------------     
          void PXLA::Block( AgCreate create )     
          {         
                ///@addtogroup PXLA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PXLA");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=pixg.r-pxld.layerthk/2.0;              
                            shape.par("rmax")=pixg.r+pxld.layerthk/2.0;              
                            shape.par("dz")=pxld.totallength/2.0;              
                            /// Shape Tube rmin=pixg.r-pxld.layerthk/2.0 rmax=pixg.r+pxld.layerthk/2.0 dz=pxld.totallength/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXLA;              
                            _stacker -> Build(this);              
                      }           
                      ladderradius = pixg.r+ladderthk/2.0-pxld.activethk/2.0;           
                      ladderwidth = 2.0 * (pixg.r-pxld.activethk/2.0) * tan(raddeg*360.0/pixg.noladders/2.0);           
                      /// Loop on nladder from 1 to pixg.noladders step=1           
                      for ( nladder=1; (1>0)? (nladder<=pixg.noladders):(nladder>=pixg.noladders); nladder+=1 )           
                      {              
                            angle = (360.0/pixg.noladders)*nladder;              
                            anglepos = angle*raddeg;              
                            _create = AgCreate("PLMI");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create PLMI                 
                                  Create("PLMI");                  
                            }              
                            { AgPlacement place = AgPlacement("PLMI","PXLA");                 
                                  /// Add daughter volume PLMI to mother PXLA                 
                                  place.TranslateX(ladderradius*cos(anglepos));                 
                                  /// Translate x = ladderradius*cos(anglepos)                 
                                  place.TranslateY(ladderradius*sin(anglepos));                 
                                  /// Translate y = ladderradius*sin(anglepos)                 
                                  place.TranslateZ(0.0);                 
                                  /// Translate z = 0.0                 
                                  place.AlphaZ(angle+pixg.aoffset);                 
                                  /// Rotate: AlphaZ = angle+pixg.aoffset                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("PLMI"), place );                 
                            } // end placement of PLMI              
                      }           
                      END_OF_PXLA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXLA     
          // ---------------------------------------------------------------------------------------------------     
          void PLMI::Block( AgCreate create )     
          {         
                ///@addtogroup PLMI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PLMI");              
                            attr.par("seen")=0;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=ladderwidth/2.0;              
                            shape.par("dy")=ladderthk/2.0;              
                            shape.par("dz")=pxld.totallength/2.0;              
                            /// Shape Bbox dx=ladderwidth/2.0 dy=ladderthk/2.0 dz=pxld.totallength/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PLMI;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PLAC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PLAC              
                            Create("PLAC");               
                      }           
                      { AgPlacement place = AgPlacement("PLAC","PLMI");              
                            /// Add daughter volume PLAC to mother PLMI              
                            place.TranslateX(0.0);              
                            /// Translate x = 0.0              
                            place.TranslateY(-1.0*ladderthk/2.0+pxld.activethk/2.0);              
                            /// Translate y = -1.0*ladderthk/2.0+pxld.activethk/2.0              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("PLAC"), place );              
                      } // end placement of PLAC           
                      _create = AgCreate("PLPS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PLPS              
                            Create("PLPS");               
                      }           
                      { AgPlacement place = AgPlacement("PLPS","PLMI");              
                            /// Add daughter volume PLPS to mother PLMI              
                            place.TranslateX(0.0);              
                            /// Translate x = 0.0              
                            place.TranslateY(+1.0*ladderthk/2.0-pxld.passivethk/2.0);              
                            /// Translate y = +1.0*ladderthk/2.0-pxld.passivethk/2.0              
                            place.TranslateZ(0.0);              
                            /// Translate z = 0.0              
                            _stacker -> Position( AgBlock::Find("PLPS"), place );              
                      } // end placement of PLPS           
                      END_OF_PLMI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PLMI     
          // ---------------------------------------------------------------------------------------------------     
          void PLPS::Block( AgCreate create )     
          {         
                ///@addtogroup PLPS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Silicon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Silicon");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PLPS");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=ladderwidth/2.0;              
                            shape.par("dy")=pxld.passivethk/2.0;              
                            shape.par("dz")=pxld.totallength/2.0;              
                            /// Shape Bbox dx=ladderwidth/2.0 dy=pxld.passivethk/2.0 dz=pxld.totallength/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PLPS;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PLPS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PLPS     
          // ---------------------------------------------------------------------------------------------------     
          void PLAC::Block( AgCreate create )     
          {         
                ///@addtogroup PLAC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Silicon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Silicon");              
                            _material = mat;              
                      }           
                      /// Material Sensitive isvol=1            
                      { AgMaterial &mat = AgMaterial::Get("Sensitive");              
                            mat.par("isvol")=1;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PLAC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=ladderwidth/2.0;              
                            shape.par("dy")=pxld.activethk/2.0;              
                            shape.par("dz")=pxld.totallength/2.0;              
                            /// Shape Bbox dx=ladderwidth/2.0 dy=pxld.activethk/2.0 dz=pxld.totallength/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PLAC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PLAC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PLAC     
    // ----------------------------------------------------------------------- geoctr
       void PixlGeo4::ConstructGeometry()     
       {        
             ///@addtogroup PixlGeo4_revision        
             ///@{           
                   /// Created:   03/13/08            
             ///@}        
             ///@addtogroup PixlGeo4_revision        
             ///@{           
                   /// Author: Gerrit van Nieuwenhuizen           
             ///@}        
             AddBlock("PXMO");        
             AddBlock("PXLA");        
             AddBlock("PLMI");        
             AddBlock("PLAC");        
             AddBlock("PLPS");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pxld_doc        
             ///@{           
                   ++pxld._index;           
                   pxld . version = 1.0; //  version            
                   /// pxld . version = 1.0; //  version            
                   pxld . subversion = 0.0; //  sub version            
                   /// pxld . subversion = 0.0; //  sub version            
                   pxld . totallength = 20.0; //  Overal length of the detector            
                   /// pxld . totallength = 20.0; //  Overal length of the detector            
                   pxld . passivethk = 0.0280; //  Passive silicon Thickness            
                   /// pxld . passivethk = 0.0280; //  Passive silicon Thickness            
                   pxld . activethk = 0.0020; //  Active  silicon Thickness            
                   /// pxld . activethk = 0.0020; //  Active  silicon Thickness            
                   pxld . rin = 2.5; //  Inner radius            
                   /// pxld . rin = 2.5; //  Inner radius            
                   pxld . rout = 8.0; //  Outer radius            
                   /// pxld . rout = 8.0; //  Outer radius            
                   pxld . layerthk = 0.5; //  Thickness of the layer mother volume            
                   /// pxld . layerthk = 0.5; //  Thickness of the layer mother volume            
                   //           
                   pxld.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pixg_doc        
             ///@{           
                   ++pixg._index;           
                   pixg . layer = 1; //  Layer index             
                   /// pixg . layer = 1; //  Layer index             
                   pixg . noladders = 10; //  Number of ladders            
                   /// pixg . noladders = 10; //  Number of ladders            
                   pixg . r = 2.5; //  1st ladder nominal radius            
                   /// pixg . r = 2.5; //  1st ladder nominal radius            
                   pixg . aoffset = -90.0; //  Angular offset            
                   /// pixg . aoffset = -90.0; //  Angular offset            
                   pixg . poffset = 0.0; //  Position offset (shift)            
                   /// pixg . poffset = 0.0; //  Position offset (shift)            
                   //           
                   pixg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pixg_doc        
             ///@{           
                   ++pixg._index;           
                   pixg . layer = 2; //  Layer index             
                   /// pixg . layer = 2; //  Layer index             
                   pixg . noladders = 30; //  Number of ladders            
                   /// pixg . noladders = 30; //  Number of ladders            
                   pixg . r = 8.0; //  Ladder radius            
                   /// pixg . r = 8.0; //  Ladder radius            
                   pixg . aoffset = -90.0; //  Angular offset            
                   /// pixg . aoffset = -90.0; //  Angular offset            
                   pixg . poffset = 0.0; //  Position offset (shift)            
                   /// pixg . poffset = 0.0; //  Position offset (shift)            
                   //           
                   pixg.fill();           
             ///@}        
             //        
             raddeg=3.14159265/180.0;        
             /// USE pxld version=1 ;        
             pxld.Use("version",(Float_t)1 );        
             ladderthk = pxld.activethk + pxld.passivethk;        
             _create = AgCreate("PXMO");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create PXMO           
                   Create("PXMO");            
             }        
             { AgPlacement place = AgPlacement("PXMO","IDSM");           
                   /// Add daughter volume PXMO to mother IDSM           
                   _stacker -> Position( AgBlock::Find("PXMO"), place );           
             } // end placement of PXMO        
       }; // PixlGeo4     
 }; // namespace PixlGeo4  
 