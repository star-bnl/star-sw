#include "PixlGeo3.h"  
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
 namespace PIXLGEO3 // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          ///@addtogroup PixlGeo3_vars     
          ///@{        
                float angle,anglepos,anglecorr,raddeg;        
                //        
                /// float angle,anglepos,anglecorr,raddeg        
          ///@}     
          ///@addtogroup PixlGeo3_vars     
          ///@{        
                int nladder,nsector,nextraladder;        
                //        
                /// int nladder,nsector,nextraladder        
          ///@}     
          //  -----------------------------------------------------     
          /// @defgroup pxlv_doc     
          /// \class Pxlv_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Int_t version;     
          ///Float_t ladver;     
          ///Float_t location;     
          ///Int_t _index;     
          //     
          Pxlv_t pxlv;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup pxld_doc     
          /// \class Pxld_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t totallength;     
          ///Float_t ladderwidth;     
          ///Float_t ladderthk;     
          ///Float_t passivethk;     
          ///Float_t activethk;     
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
          ///Float_t ladder;     
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
       PixlGeo3::PixlGeo3()     
         : AgModule("PixlGeo3"," is the the STAR pixel detector and beam pipe support ")     
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
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=pxld.rin;              
                            shape.par("rmax")=pxld.rout;              
                            shape.par("dz")=pxld.totallength/2.0;              
                            /// Shape Tube rmin=pxld.rin rmax=pxld.rout dz=pxld.totallength/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXMO;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PSEC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PSEC              
                            Create("PSEC");               
                      }           
                      /// Loop on nsector from 1 to 3 step=1           
                      for ( nsector=1; (1>0)? (nsector<=3):(nsector>=3); nsector+=1 )           
                      {              
                            { AgPlacement place = AgPlacement("PSEC","PXMO");                 
                                  /// Add daughter volume PSEC to mother PXMO                 
                                  place.par("only")=AgPlacement::kMany;                 
                                  /// Overlap: agplacement::kmany                 
                                  place.AlphaZ(120.0*(nsector-1));                 
                                  /// Rotate: AlphaZ = 120.0*(nsector-1)                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("PSEC"), place );                 
                            } // end placement of PSEC              
                      }           
                      END_OF_PXMO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXMO     
          // ---------------------------------------------------------------------------------------------------     
          void PXBX::Block( AgCreate create )     
          {         
                ///@addtogroup PXBX_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Berillium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Berillium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PXBX");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=pxbg.rin;              
                            shape.par("rmax")=pxbg.rin+pxbg.thk;              
                            shape.par("dz")=pxbg.length/2.0;              
                            /// Shape Tube rmin=pxbg.rin rmax=pxbg.rin+pxbg.thk dz=pxbg.length/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXBX;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXBX:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXBX     
          // ---------------------------------------------------------------------------------------------------     
          void PSEC::Block( AgCreate create )     
          {         
                ///@addtogroup PSEC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PSEC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=5;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=pxld.rin;              
                            shape.par("rmax")=pxld.rout;              
                            shape.par("phi1")=-11.0;              
                            shape.par("phi2")=122.0;              
                            shape.par("dz")=pxld.totallength/2.0;              
                            /// Shape Tubs rmin=pxld.rin rmax=pxld.rout phi1=-11.0 phi2=122.0 dz=pxld.totallength/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PSEC;              
                            _stacker -> Build(this);              
                      }           
                      /// Loop on nladder from 1 to 11 step=1           
                      for ( nladder=1; (1>0)? (nladder<=11):(nladder>=11); nladder+=1 )           
                      {              
                            /// USE pixg ladder=nladder ;              
                            pixg.Use("ladder",(Float_t)nladder );              
                            angle = pixg.a;              
                            anglepos = angle*raddeg               ;//  +anglecorr  see above comment;              
                            _create = AgCreate("PLMO");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create PLMO                 
                                  Create("PLMO");                  
                            }              
                            { AgPlacement place = AgPlacement("PLMO","PSEC");                 
                                  /// Add daughter volume PLMO to mother PSEC                 
                                  place.TranslateX(pixg.r*cos(anglepos));                 
                                  /// Translate x = pixg.r*cos(anglepos)                 
                                  place.TranslateY(pixg.r*sin(anglepos));                 
                                  /// Translate y = pixg.r*sin(anglepos)                 
                                  place.TranslateZ(0.0);                 
                                  /// Translate z = 0.0                 
                                  place.AlphaZ(-pixg.aoffset+angle);                 
                                  /// Rotate: AlphaZ = -pixg.aoffset+angle                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("PLMO"), place );                 
                            } // end placement of PLMO              
                      }           
                      END_OF_PSEC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PSEC     
          // ---------------------------------------------------------------------------------------------------     
          void PLMO::Block( AgCreate create )     
          {         
                ///@addtogroup PLMO_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PLMO");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=pxld.ladderwidth/2.0;              
                            shape.par("dy")=pxld.ladderthk/2.0;              
                            shape.par("dz")=pxld.totallength/2.0;              
                            /// Shape Bbox dx=pxld.ladderwidth/2.0 dy=pxld.ladderthk/2.0 dz=pxld.totallength/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PLMO;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PLAC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PLAC              
                            Create("PLAC");               
                      }           
                      { AgPlacement place = AgPlacement("PLAC","PLMO");              
                            /// Add daughter volume PLAC to mother PLMO              
                            place.TranslateY(-pxld.ladderthk/2.0+pxld.activethk/2.0);              
                            /// Translate y = -pxld.ladderthk/2.0+pxld.activethk/2.0              
                            _stacker -> Position( AgBlock::Find("PLAC"), place );              
                      } // end placement of PLAC           
                      _create = AgCreate("PLPS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PLPS              
                            Create("PLPS");               
                      }           
                      { AgPlacement place = AgPlacement("PLPS","PLMO");              
                            /// Add daughter volume PLPS to mother PLMO              
                            place.TranslateY(-pxld.ladderthk/2.0+pxld.activethk+pxld.passivethk/2.0);              
                            /// Translate y = -pxld.ladderthk/2.0+pxld.activethk+pxld.passivethk/2.0              
                            _stacker -> Position( AgBlock::Find("PLPS"), place );              
                      } // end placement of PLPS           
                      END_OF_PLMO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PLMO     
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
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=pxld.ladderwidth/2.0;              
                            shape.par("dy")=pxld.activethk/2.0;              
                            shape.par("dz")=pxld.totallength/2.0;              
                            /// Shape Bbox dx=pxld.ladderwidth/2.0 dy=pxld.activethk/2.0 dz=pxld.totallength/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PLAC;              
                            _stacker -> Build(this);              
                      }           
                      /*{              
                            GSTPAR( %imed,"stra",1. );// CALL GSTPAR              
                      }*/           
                      END_OF_PLAC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PLAC     
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
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=pxld.ladderwidth/2.0;              
                            shape.par("dy")=pxld.passivethk/2.0;              
                            shape.par("dz")=pxld.totallength/2.0;              
                            /// Shape Bbox dx=pxld.ladderwidth/2.0 dy=pxld.passivethk/2.0 dz=pxld.totallength/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PLPS;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PLPS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PLPS     
    // ----------------------------------------------------------------------- geoctr
       void PixlGeo3::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup PixlGeo3_revision        
             ///@{           
                   /// Created:   10/02/06            
             ///@}        
             ///@addtogroup PixlGeo3_revision        
             ///@{           
                   /// Author: Andrew Rose           
             ///@}        
             AddBlock("PXMO");        
             AddBlock("PSEC");        
             AddBlock("PLMO");        
             AddBlock("PLAC");        
             AddBlock("PLPS");        
             AddBlock("PXBX");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pxlv_doc        
             ///@{           
                   ++pxlv._index;           
                   pxlv . version = 1.0; //  config version            
                   /// pxlv . version = 1.0; //  config version            
                   pxlv . ladver = 1.0; //  Ladder Version            
                   /// pxlv . ladver = 1.0; //  Ladder Version            
                   pxlv . location = 1.0; // Location: 1=CAVE, 2=IDSM           
                   /// pxlv . location = 1.0; // Location: 1=CAVE, 2=IDSM           
                   //           
                   pxlv.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pxld_doc        
             ///@{           
                   ++pxld._index;           
                   pxld . version = 1.0; //  version            
                   /// pxld . version = 1.0; //  version            
                   pxld . totallength = 20.0; //  Overal length of the detector            
                   /// pxld . totallength = 20.0; //  Overal length of the detector            
                   pxld . ladderwidth = 2.00; //  Ladder Width            
                   /// pxld . ladderwidth = 2.00; //  Ladder Width            
                   pxld . ladderthk = 0.0240; //  Total ladder Thickness            
                   /// pxld . ladderthk = 0.0240; //  Total ladder Thickness            
                   pxld . passivethk = 0.0220; //  Passive silicon Thickness            
                   /// pxld . passivethk = 0.0220; //  Passive silicon Thickness            
                   pxld . activethk = 0.0020; //  Active  silicon Thickness            
                   /// pxld . activethk = 0.0020; //  Active  silicon Thickness            
                   pxld . rin = 2.4; //  Inner radius            
                   /// pxld . rin = 2.4; //  Inner radius            
                   pxld . rout = 8.3; //  Outer radius            
                   /// pxld . rout = 8.3; //  Outer radius            
                   //           
                   pxld.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pxld_doc        
             ///@{           
                   ++pxld._index;           
                   pxld . version = 2.0; //  version            
                   /// pxld . version = 2.0; //  version            
                   pxld . ladderwidth = 2.00; //  Ladder Width            
                   /// pxld . ladderwidth = 2.00; //  Ladder Width            
                   pxld . ladderthk = 0.0240; //  Total ladder Thickness            
                   /// pxld . ladderthk = 0.0240; //  Total ladder Thickness            
                   pxld . passivethk = 0.0120; //  Passive silicon Thickness            
                   /// pxld . passivethk = 0.0120; //  Passive silicon Thickness            
                   pxld . activethk = 0.0120; //  Active  silicon Thickness            
                   /// pxld . activethk = 0.0120; //  Active  silicon Thickness            
                   pxld . rin = 2.4; //  Inner radius            
                   /// pxld . rin = 2.4; //  Inner radius            
                   pxld . rout = 8.3; //  Outer radius            
                   /// pxld . rout = 8.3; //  Outer radius            
                   //           
                   pxld.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pixg_doc        
             ///@{           
                   ++pixg._index;           
                   pixg . ladder = 1; //  ladder index            
                   /// pixg . ladder = 1; //  ladder index            
                   pixg . r = 2.5; //  1st ladder nominal radius            
                   /// pixg . r = 2.5; //  1st ladder nominal radius            
                   pixg . a = 100.; //  1st ladder nominal position angle            
                   /// pixg . a = 100.; //  1st ladder nominal position angle            
                   pixg . aoffset = 103.; //  Angular offset            
                   /// pixg . aoffset = 103.; //  Angular offset            
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
                   pixg . ladder = 2; //  ladder index            
                   /// pixg . ladder = 2; //  ladder index            
                   pixg . r = 2.5; //  2nd ladder nominal radius            
                   /// pixg . r = 2.5; //  2nd ladder nominal radius            
                   pixg . a = 60.; //  2nd ladder nominal position angle            
                   /// pixg . a = 60.; //  2nd ladder nominal position angle            
                   pixg . aoffset = 103.; //  Angular offset            
                   /// pixg . aoffset = 103.; //  Angular offset            
                   //           
                   pixg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pixg_doc        
             ///@{           
                   ++pixg._index;           
                   pixg . ladder = 3; //  ladder index            
                   /// pixg . ladder = 3; //  ladder index            
                   pixg . r = 2.5; //  2nd ladder nominal radius            
                   /// pixg . r = 2.5; //  2nd ladder nominal radius            
                   pixg . a = 20.; //  2nd ladder nominal position angle            
                   /// pixg . a = 20.; //  2nd ladder nominal position angle            
                   pixg . aoffset = 103.; //  Angular offset            
                   /// pixg . aoffset = 103.; //  Angular offset            
                   //           
                   pixg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pixg_doc        
             ///@{           
                   ++pixg._index;           
                   pixg . ladder = 4; //  ladder index            
                   /// pixg . ladder = 4; //  ladder index            
                   pixg . r = 6.5; //  2nd ladder nominal radius            
                   /// pixg . r = 6.5; //  2nd ladder nominal radius            
                   pixg . a = 105.; //  2nd ladder nominal position angle            
                   /// pixg . a = 105.; //  2nd ladder nominal position angle            
                   pixg . aoffset = 90.; //  Angular offset            
                   /// pixg . aoffset = 90.; //  Angular offset            
                   //           
                   pixg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pixg_doc        
             ///@{           
                   ++pixg._index;           
                   pixg . ladder = 5; //  ladder index            
                   /// pixg . ladder = 5; //  ladder index            
                   pixg . r = 7.5; //  3rd ladder radius            
                   /// pixg . r = 7.5; //  3rd ladder radius            
                   pixg . a = 90.; //  3rd ladder nominal position angle            
                   /// pixg . a = 90.; //  3rd ladder nominal position angle            
                   pixg . aoffset = 90.; //  Angular offset            
                   /// pixg . aoffset = 90.; //  Angular offset            
                   //           
                   pixg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pixg_doc        
             ///@{           
                   ++pixg._index;           
                   pixg . ladder = 6; //  ladder index            
                   /// pixg . ladder = 6; //  ladder index            
                   pixg . r = 6.5; //  4th ladder nominal radius            
                   /// pixg . r = 6.5; //  4th ladder nominal radius            
                   pixg . a = 75.; //  4th ladder nominal position angle            
                   /// pixg . a = 75.; //  4th ladder nominal position angle            
                   pixg . aoffset = 90.; //  Angular offset            
                   /// pixg . aoffset = 90.; //  Angular offset            
                   //           
                   pixg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pixg_doc        
             ///@{           
                   ++pixg._index;           
                   pixg . ladder = 7; //  ladder index            
                   /// pixg . ladder = 7; //  ladder index            
                   pixg . r = 7.5; //  3rd ladder radius            
                   /// pixg . r = 7.5; //  3rd ladder radius            
                   pixg . a = 60.; //  3rd ladder nominal position angle            
                   /// pixg . a = 60.; //  3rd ladder nominal position angle            
                   pixg . aoffset = 90.; //  Angular offset            
                   /// pixg . aoffset = 90.; //  Angular offset            
                   //           
                   pixg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pixg_doc        
             ///@{           
                   ++pixg._index;           
                   pixg . ladder = 8; //  ladder index            
                   /// pixg . ladder = 8; //  ladder index            
                   pixg . r = 6.5; //  4th ladder nominal radius            
                   /// pixg . r = 6.5; //  4th ladder nominal radius            
                   pixg . a = 45.; //  4th ladder nominal position angle            
                   /// pixg . a = 45.; //  4th ladder nominal position angle            
                   pixg . aoffset = 90.; //  Angular offset            
                   /// pixg . aoffset = 90.; //  Angular offset            
                   //           
                   pixg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pixg_doc        
             ///@{           
                   ++pixg._index;           
                   pixg . ladder = 9; //  ladder index            
                   /// pixg . ladder = 9; //  ladder index            
                   pixg . r = 7.5; //  3rd ladder radius            
                   /// pixg . r = 7.5; //  3rd ladder radius            
                   pixg . a = 30.; //  3rd ladder nominal position angle            
                   /// pixg . a = 30.; //  3rd ladder nominal position angle            
                   pixg . aoffset = 90.; //  Angular offset            
                   /// pixg . aoffset = 90.; //  Angular offset            
                   //           
                   pixg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pixg_doc        
             ///@{           
                   ++pixg._index;           
                   pixg . ladder = 10; //  ladder index            
                   /// pixg . ladder = 10; //  ladder index            
                   pixg . r = 6.5; //  4th ladder nominal radius            
                   /// pixg . r = 6.5; //  4th ladder nominal radius            
                   pixg . a = 15.; //  4th ladder nominal position angle            
                   /// pixg . a = 15.; //  4th ladder nominal position angle            
                   pixg . aoffset = 90.; //  Angular offset            
                   /// pixg . aoffset = 90.; //  Angular offset            
                   //           
                   pixg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pixg_doc        
             ///@{           
                   ++pixg._index;           
                   pixg . ladder = 11; //  ladder index            
                   /// pixg . ladder = 11; //  ladder index            
                   pixg . r = 7.5; //  3rd ladder radius            
                   /// pixg . r = 7.5; //  3rd ladder radius            
                   pixg . a = 0.; //  3rd ladder nominal position angle            
                   /// pixg . a = 0.; //  3rd ladder nominal position angle            
                   pixg . aoffset = 90.; //  Angular offset            
                   /// pixg . aoffset = 90.; //  Angular offset            
                   //           
                   pixg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup pxbg_doc        
             ///@{           
                   ++pxbg._index;           
                   pxbg . version = 2; //  Version            
                   /// pxbg . version = 2; //  Version            
                   pxbg . length = 48.0; //  Total Length            
                   /// pxbg . length = 48.0; //  Total Length            
                   pxbg . rin = 8.5; //  Inner Radius            
                   /// pxbg . rin = 8.5; //  Inner Radius            
                   pxbg . thk = 0.1; //  Thickness            
                   /// pxbg . thk = 0.1; //  Thickness            
                   //           
                   pxbg.fill();           
             ///@}        
             //        
             /// USE pxlv _index=1;        
             pxlv.Use();        
             /// USE pxld version=pxlv.ladver ;        
             pxld.Use("version",(Float_t)pxlv.ladver );        
             raddeg=3.14159265/180.0;        
             _create = AgCreate("PXMO");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create PXMO           
                   Create("PXMO");            
             }        
             if ( pxlv.location==1.0 )        
             {           
                   { AgPlacement place = AgPlacement("PXMO","CAVE");              
                         /// Add daughter volume PXMO to mother CAVE              
                         _stacker -> Position( AgBlock::Find("PXMO"), place );              
                   } // end placement of PXMO           
             }        
             else        
             {           
                   { AgPlacement place = AgPlacement("PXMO","IDSM");              
                         /// Add daughter volume PXMO to mother IDSM              
                         _stacker -> Position( AgBlock::Find("PXMO"), place );              
                   } // end placement of PXMO           
             }        
             _create = AgCreate("PXBX");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create PXBX           
                   Create("PXBX");            
             }        
             if ( pxlv.location==1.0 )        
             {           
                   { AgPlacement place = AgPlacement("PXBX","CAVE");              
                         /// Add daughter volume PXBX to mother CAVE              
                         _stacker -> Position( AgBlock::Find("PXBX"), place );              
                   } // end placement of PXBX           
             }        
             else        
             {           
                   { AgPlacement place = AgPlacement("PXBX","IDSM");              
                         /// Add daughter volume PXBX to mother IDSM              
                         _stacker -> Position( AgBlock::Find("PXBX"), place );              
                   } // end placement of PXBX           
             }        
       }; // PixlGeo3     
 }; // namespace PixlGeo3  
 