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
                float halfladright=2.50864;        
                //        
                /// float halfladright=2.50864        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float halfladbottom=0.73364;        
                //        
                /// float halfladbottom=0.73364        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float halfladleft=2.38864;        
                //        
                /// float halfladleft=2.38864        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float halfladthk=0.0135;        
                //        
                /// float halfladthk=0.0135        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float halfladzlen=19.59;        
                //        
                /// float halfladzlen=19.59        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float halfladtopr=0.623359;        
                //        
                /// float halfladtopr=0.623359        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float halfladtopm=0.680382;        
                //        
                /// float halfladtopm=0.680382        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float halfladtopl=0.661607;        
                //        
                /// float halfladtopl=0.661607        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float halfladmid=0.0384251;        
                //        
                /// float halfladmid=0.0384251        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float arctmin=0.123;        
                //        
                /// float arctmin=0.123        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float arctmax=0.15;        
                //        
                /// float arctmax=0.15        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float arctbmin=0.15;        
                //        
                /// float arctbmin=0.15        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float arctbmax=0.177;        
                //        
                /// float arctbmax=0.177        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float arcbmin=0.173;        
                //        
                /// float arcbmin=0.173        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float arcbmax=0.2;        
                //        
                /// float arcbmax=0.2        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float halfpixr=0.96;        
                //        
                /// float halfpixr=0.96        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float halfpixthk=0.0025;        
                //        
                /// float halfpixthk=0.0025        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float halfpixz=10;        
                //        
                /// float halfpixz=10        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                int nsector=10;        
                //        
                /// int nsector=10        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float minradsec=2.35;        
                //        
                /// float minradsec=2.35        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float maxradsec=9.5;        
                //        
                /// float maxradsec=9.5        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float maxzsec=21;        
                //        
                /// float maxzsec=21        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float minrvol=2.3;        
                //        
                /// float minrvol=2.3        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float maxrvol=10;        
                //        
                /// float maxrvol=10        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float maxzvol=22;        
                //        
                /// float maxzvol=22        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float halfmaxz=20.0;        
                //        
                /// float halfmaxz=20.0        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float minphisec=88.0;        
                //        
                /// float minphisec=88.0        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float maxphisec=128.0;        
                //        
                /// float maxphisec=128.0        
          ///@}     
          ///@addtogroup PixlGeo4_vars     
          ///@{        
                float xpos,ypos,zpos,angle,anglepos,sector;        
                //        
                /// float xpos,ypos,zpos,angle,anglepos,sector        
          ///@}     
       PixlGeo4::PixlGeo4()     
         : AgModule("PixlGeo4","Pixel Detector Geometry  ")     
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
                            shape.par("rmin")=minrvol;              
                            shape.par("rmax")=maxrvol;              
                            shape.par("dz")=maxzvol;              
                            /// Shape Tube rmin=minrvol rmax=maxrvol dz=maxzvol               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXMO;              
                            _stacker -> Build(this);              
                      }           
                      /// Loop on sector from 1 to nsector step=1           
                      for ( sector=1; (1>0)? (sector<=nsector):(sector>=nsector); sector+=1 )           
                      {              
                            angle=(360.0/nsector)*(sector-1);              
                            anglepos=angle*(pi/180.0);              
                            xpos=.00001*sin(anglepos);              
                            ypos=.00001*cos(anglepos);              
                            zpos =0.0;              
                            _create = AgCreate("PXLA");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create PXLA                 
                                  Create("PXLA");                  
                            }              
                            { AgPlacement place = AgPlacement("PXLA","PXMO");                 
                                  /// Add daughter volume PXLA to mother PXMO                 
                                  place.TranslateX(xpos);                 
                                  /// Translate x = xpos                 
                                  place.TranslateY(ypos);                 
                                  /// Translate y = ypos                 
                                  place.TranslateZ(zpos);                 
                                  /// Translate z = zpos                 
                                  place.par("only")=AgPlacement::kMany;                 
                                  /// Overlap: agplacement::kmany                 
                                  place.AlphaZ(-1*angle);                 
                                  /// Rotate: AlphaZ = -1*angle                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("PXLA"), place );                 
                            } // end placement of PXLA              
                      }           
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
                      { AgAttribute attr = AgAttribute("PXMO");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=minradsec;              
                            shape.par("rmax")=maxradsec;              
                            shape.par("phi1")=minphisec;              
                            shape.par("phi2")=maxphisec;              
                            shape.par("dz")=halfmaxz;              
                            /// Shape Tubs rmin=minradsec rmax=maxradsec phi1=minphisec phi2=maxphisec dz=halfmaxz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXLA;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("PXRB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXRB              
                            Create("PXRB");               
                      }           
                      { AgPlacement place = AgPlacement("PXRB","PXLA");              
                            /// Add daughter volume PXRB to mother PXLA              
                            place.TranslateX(-0.1135);              
                            /// Translate x = -0.1135              
                            place.TranslateY(5.26084);              
                            /// Translate y = 5.26084              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("PXRB"), place );              
                      } // end placement of PXRB           
                      _create = AgCreate("PXLB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXLB              
                            Create("PXLB");               
                      }           
                      { AgPlacement place = AgPlacement("PXLB","PXLA");              
                            /// Add daughter volume PXLB to mother PXLA              
                            place.TranslateX(-3.26838);              
                            /// Translate x = -3.26838              
                            place.TranslateY(4.69164);              
                            /// Translate y = 4.69164              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            place.AlphaZ(216);              
                            /// Rotate: AlphaZ = 216              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PXLB"), place );              
                      } // end placement of PXLB           
                      _create = AgCreate("PXIB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXIB              
                            Create("PXIB");               
                      }           
                      { AgPlacement place = AgPlacement("PXIB","PXLA");              
                            /// Add daughter volume PXIB to mother PXLA              
                            place.TranslateX(-0.989084);              
                            /// Translate x = -0.989084              
                            place.TranslateY(2.64917);              
                            /// Translate y = 2.64917              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            place.AlphaZ(267.396-270);              
                            /// Rotate: AlphaZ = 267.396-270              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PXIB"), place );              
                      } // end placement of PXIB           
                      _create = AgCreate("PXTR");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXTR              
                            Create("PXTR");               
                      }           
                      { AgPlacement place = AgPlacement("PXTR","PXLA");              
                            /// Add daughter volume PXTR to mother PXLA              
                            place.TranslateX(-0.840029);              
                            /// Translate x = -0.840029              
                            place.TranslateY(8.01253);              
                            /// Translate y = 8.01253              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            place.AlphaZ(79.963-90);              
                            /// Rotate: AlphaZ = 79.963-90              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PXTR"), place );              
                      } // end placement of PXTR           
                      _create = AgCreate("PXTM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXTM              
                            Create("PXTM");               
                      }           
                      { AgPlacement place = AgPlacement("PXTM","PXLA");              
                            /// Add daughter volume PXTM to mother PXLA              
                            place.TranslateX(-2.43058);              
                            /// Translate x = -2.43058              
                            place.TranslateY(7.66474);              
                            /// Translate y = 7.66474              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            place.AlphaZ(91.963-90);              
                            /// Rotate: AlphaZ = 91.963-90              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PXTM"), place );              
                      } // end placement of PXTM           
                      _create = AgCreate("PXTL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXTL              
                            Create("PXTL");               
                      }           
                      { AgPlacement place = AgPlacement("PXTL","PXLA");              
                            /// Add daughter volume PXTL to mother PXLA              
                            place.TranslateX(-3.95284);              
                            /// Translate x = -3.95284              
                            place.TranslateY(6.99643);              
                            /// Translate y = 6.99643              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            place.AlphaZ(103.693-90);              
                            /// Rotate: AlphaZ = 103.693-90              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PXTL"), place );              
                      } // end placement of PXTL           
                      _create = AgCreate("PXTJ");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXTJ              
                            Create("PXTJ");               
                      }           
                      { AgPlacement place = AgPlacement("PXTJ","PXLA");              
                            /// Add daughter volume PXTJ to mother PXLA              
                            place.TranslateX(-3.21568);              
                            /// Translate x = -3.21568              
                            place.TranslateY(7.41527);              
                            /// Translate y = 7.41527              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            place.AlphaZ(203.54-270);              
                            /// Rotate: AlphaZ = 203.54-270              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PXTJ"), place );              
                      } // end placement of PXTJ           
                      _create = AgCreate("PXTJ");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXTJ              
                            Create("PXTJ");               
                      }           
                      { AgPlacement place = AgPlacement("PXTJ","PXLA");              
                            /// Add daughter volume PXTJ to mother PXLA              
                            place.TranslateX(-1.60369);              
                            /// Translate x = -1.60369              
                            place.TranslateY(7.92181);              
                            /// Translate y = 7.92181              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            place.AlphaZ(191.54-270);              
                            /// Rotate: AlphaZ = 191.54-270              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PXTJ"), place );              
                      } // end placement of PXTJ           
                      _create = AgCreate("PXCA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXCA              
                            Create("PXCA");               
                      }           
                      { AgPlacement place = AgPlacement("PXCA","PXLA");              
                            /// Add daughter volume PXCA to mother PXLA              
                            place.TranslateX(-0.25);              
                            /// Translate x = -0.25              
                            place.TranslateY(2.7522);              
                            /// Translate y = 2.7522              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("PXCA"), place );              
                      } // end placement of PXCA           
                      _create = AgCreate("PXCC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXCC              
                            Create("PXCC");               
                      }           
                      { AgPlacement place = AgPlacement("PXCC","PXLA");              
                            /// Add daughter volume PXCC to mother PXLA              
                            place.TranslateX(-0.25);              
                            /// Translate x = -0.25              
                            place.TranslateY(7.76948);              
                            /// Translate y = 7.76948              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("PXCC"), place );              
                      } // end placement of PXCC           
                      _create = AgCreate("PXCD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXCD              
                            Create("PXCD");               
                      }           
                      { AgPlacement place = AgPlacement("PXCD","PXLA");              
                            /// Add daughter volume PXCD to mother PXLA              
                            place.TranslateX(-1.47764);              
                            /// Translate x = -1.47764              
                            place.TranslateY(7.98676);              
                            /// Translate y = 7.98676              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("PXCD"), place );              
                      } // end placement of PXCD           
                      _create = AgCreate("PXCE");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXCE              
                            Create("PXCE");               
                      }           
                      { AgPlacement place = AgPlacement("PXCE","PXLA");              
                            /// Add daughter volume PXCE to mother PXLA              
                            place.TranslateX(-1.7562);              
                            /// Translate x = -1.7562              
                            place.TranslateY(7.85145);              
                            /// Translate y = 7.85145              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("PXCE"), place );              
                      } // end placement of PXCE           
                      _create = AgCreate("PXCF");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXCF              
                            Create("PXCF");               
                      }           
                      { AgPlacement place = AgPlacement("PXCF","PXLA");              
                            /// Add daughter volume PXCF to mother PXLA              
                            place.TranslateX(-3.10589);              
                            /// Translate x = -3.10589              
                            place.TranslateY(7.50501);              
                            /// Translate y = 7.50501              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("PXCF"), place );              
                      } // end placement of PXCF           
                      _create = AgCreate("PXCG");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXCG              
                            Create("PXCG");               
                      }           
                      { AgPlacement place = AgPlacement("PXCG","PXLA");              
                            /// Add daughter volume PXCG to mother PXLA              
                            place.TranslateX(-3.35023);              
                            /// Translate x = -3.35023              
                            place.TranslateY(7.31474);              
                            /// Translate y = 7.31474              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("PXCG"), place );              
                      } // end placement of PXCG           
                      _create = AgCreate("PXCH");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXCH              
                            Create("PXCH");               
                      }           
                      { AgPlacement place = AgPlacement("PXCH","PXLA");              
                            /// Add daughter volume PXCH to mother PXLA              
                            place.TranslateX(-4.56196);              
                            /// Translate x = -4.56196              
                            place.TranslateY(6.70432);              
                            /// Translate y = 6.70432              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("PXCH"), place );              
                      } // end placement of PXCH           
                      _create = AgCreate("PXCB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PXCB              
                            Create("PXCB");               
                      }           
                      { AgPlacement place = AgPlacement("PXCB","PXLA");              
                            /// Add daughter volume PXCB to mother PXLA              
                            place.TranslateX(-1.71349);              
                            /// Translate x = -1.71349              
                            place.TranslateY(2.86881);              
                            /// Translate y = 2.86881              
                            place.TranslateZ(-3.07);              
                            /// Translate z = -3.07              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("PXCB"), place );              
                      } // end placement of PXCB           
                      _create = AgCreate("PLAC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PLAC              
                            Create("PLAC");               
                      }           
                      { AgPlacement place = AgPlacement("PLAC","PXLA");              
                            /// Add daughter volume PLAC to mother PXLA              
                            place.TranslateX(-1.12166);              
                            /// Translate x = -1.12166              
                            place.TranslateY(8.16292);              
                            /// Translate y = 8.16292              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            place.AlphaZ(79.963-90);              
                            /// Rotate: AlphaZ = 79.963-90              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PLAC"), place );              
                      } // end placement of PLAC           
                      _create = AgCreate("PLAC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PLAC              
                            Create("PLAC");               
                      }           
                      { AgPlacement place = AgPlacement("PLAC","PXLA");              
                            /// Add daughter volume PLAC to mother PXLA              
                            place.TranslateX(-2.79431);              
                            /// Translate x = -2.79431              
                            place.TranslateY(7.75133);              
                            /// Translate y = 7.75133              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            place.AlphaZ(91.963-90);              
                            /// Rotate: AlphaZ = 91.963-90              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PLAC"), place );              
                      } // end placement of PLAC           
                      _create = AgCreate("PLAC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PLAC              
                            Create("PLAC");               
                      }           
                      { AgPlacement place = AgPlacement("PLAC","PXLA");              
                            /// Add daughter volume PLAC to mother PXLA              
                            place.TranslateX(-4.34484);              
                            /// Translate x = -4.34484              
                            place.TranslateY(7.00098);              
                            /// Translate y = 7.00098              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            place.AlphaZ(103.693-90);              
                            /// Rotate: AlphaZ = 103.693-90              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PLAC"), place );              
                      } // end placement of PLAC           
                      _create = AgCreate("PLAC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create PLAC              
                            Create("PLAC");               
                      }           
                      { AgPlacement place = AgPlacement("PLAC","PXLA");              
                            /// Add daughter volume PLAC to mother PXLA              
                            place.TranslateX(-0.84554);              
                            /// Translate x = -0.84554              
                            place.TranslateY(2.53854);              
                            /// Translate y = 2.53854              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            place.AlphaZ(87.396-90);              
                            /// Rotate: AlphaZ = 87.396-90              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("PLAC"), place );              
                      } // end placement of PLAC           
                      END_OF_PXLA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXLA     
          // ---------------------------------------------------------------------------------------------------     
          void PXRB::Block( AgCreate create )     
          {         
                ///@addtogroup PXRB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PXRB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=halfladthk;              
                            shape.par("dy")=halfladright;              
                            shape.par("dz")=halfladzlen;              
                            /// Shape Bbox dx=halfladthk dy=halfladright dz=halfladzlen               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXRB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXRB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXRB     
          // ---------------------------------------------------------------------------------------------------     
          void PXLB::Block( AgCreate create )     
          {         
                ///@addtogroup PXLB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PXLB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=halfladthk;              
                            shape.par("dy")=halfladleft;              
                            shape.par("dz")=halfladzlen;              
                            /// Shape Bbox dx=halfladthk dy=halfladleft dz=halfladzlen               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXLB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXLB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXLB     
          // ---------------------------------------------------------------------------------------------------     
          void PXIB::Block( AgCreate create )     
          {         
                ///@addtogroup PXIB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PXIB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=halfladbottom;              
                            shape.par("dy")=halfladthk;              
                            shape.par("dz")=halfladzlen;              
                            /// Shape Bbox dx=halfladbottom dy=halfladthk dz=halfladzlen               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXIB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXIB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXIB     
          // ---------------------------------------------------------------------------------------------------     
          void PXTR::Block( AgCreate create )     
          {         
                ///@addtogroup PXTR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PXTR");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=halfladtopr;              
                            shape.par("dy")=halfladthk;              
                            shape.par("dz")=halfladzlen;              
                            /// Shape Bbox dx=halfladtopr dy=halfladthk dz=halfladzlen               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXTR;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXTR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXTR     
          // ---------------------------------------------------------------------------------------------------     
          void PXTM::Block( AgCreate create )     
          {         
                ///@addtogroup PXTM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PXTM");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=halfladtopm;              
                            shape.par("dy")=halfladthk;              
                            shape.par("dz")=halfladzlen;              
                            /// Shape Bbox dx=halfladtopm dy=halfladthk dz=halfladzlen               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXTM;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXTM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXTM     
          // ---------------------------------------------------------------------------------------------------     
          void PXTL::Block( AgCreate create )     
          {         
                ///@addtogroup PXTL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PXTL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=halfladtopl;              
                            shape.par("dy")=halfladthk;              
                            shape.par("dz")=halfladzlen;              
                            /// Shape Bbox dx=halfladtopl dy=halfladthk dz=halfladzlen               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXTL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXTL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXTL     
          // ---------------------------------------------------------------------------------------------------     
          void PXTJ::Block( AgCreate create )     
          {         
                ///@addtogroup PXTJ_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PXTJ");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=halfladmid;              
                            shape.par("dy")=halfladthk;              
                            shape.par("dz")=halfladzlen;              
                            /// Shape Bbox dx=halfladmid dy=halfladthk dz=halfladzlen               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXTJ;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXTJ:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXTJ     
          // ---------------------------------------------------------------------------------------------------     
          void PXCA::Block( AgCreate create )     
          {         
                ///@addtogroup PXCA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PXCA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=arctmin;              
                            shape.par("rmax")=arctmax;              
                            shape.par("phi1")=267.396;              
                            shape.par("phi2")=360;              
                            shape.par("dz")=halfladzlen;              
                            /// Shape Tubs rmin=arctmin rmax=arctmax phi1=267.396 phi2=360 dz=halfladzlen               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXCA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXCA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXCA     
          // ---------------------------------------------------------------------------------------------------     
          void PXCC::Block( AgCreate create )     
          {         
                ///@addtogroup PXCC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PXCC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=arctmin;              
                            shape.par("rmax")=arctmax;              
                            shape.par("phi1")=0;              
                            shape.par("phi2")=79.963;              
                            shape.par("dz")=halfladzlen;              
                            /// Shape Tubs rmin=arctmin rmax=arctmax phi1=0 phi2=79.963 dz=halfladzlen               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXCC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXCC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXCC     
          // ---------------------------------------------------------------------------------------------------     
          void PXCD::Block( AgCreate create )     
          {         
                ///@addtogroup PXCD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PXCD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=arctmin;              
                            shape.par("rmax")=arctmax;              
                            shape.par("phi1")=79.963;              
                            shape.par("phi2")=191.54;              
                            shape.par("dz")=halfladzlen;              
                            /// Shape Tubs rmin=arctmin rmax=arctmax phi1=79.963 phi2=191.54 dz=halfladzlen               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXCD;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXCD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXCD     
          // ---------------------------------------------------------------------------------------------------     
          void PXCE::Block( AgCreate create )     
          {         
                ///@addtogroup PXCE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PXCE");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=arctbmin;              
                            shape.par("rmax")=arctbmax;              
                            shape.par("phi1")=271.963;              
                            shape.par("phi2")=371.54;              
                            shape.par("dz")=halfladzlen;              
                            /// Shape Tubs rmin=arctbmin rmax=arctbmax phi1=271.963 phi2=371.54 dz=halfladzlen               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXCE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXCE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXCE     
          // ---------------------------------------------------------------------------------------------------     
          void PXCF::Block( AgCreate create )     
          {         
                ///@addtogroup PXCF_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PXCF");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=arctmin;              
                            shape.par("rmax")=arctmax;              
                            shape.par("phi1")=91.963;              
                            shape.par("phi2")=203.54;              
                            shape.par("dz")=halfladzlen;              
                            /// Shape Tubs rmin=arctmin rmax=arctmax phi1=91.963 phi2=203.54 dz=halfladzlen               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXCF;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXCF:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXCF     
          // ---------------------------------------------------------------------------------------------------     
          void PXCG::Block( AgCreate create )     
          {         
                ///@addtogroup PXCG_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PXCG");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=arctbmin;              
                            shape.par("rmax")=arctbmax;              
                            shape.par("phi1")=283.963;              
                            shape.par("phi2")=383.54;              
                            shape.par("dz")=halfladzlen;              
                            /// Shape Tubs rmin=arctbmin rmax=arctbmax phi1=283.963 phi2=383.54 dz=halfladzlen               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXCG;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXCG:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXCG     
          // ---------------------------------------------------------------------------------------------------     
          void PXCH::Block( AgCreate create )     
          {         
                ///@addtogroup PXCH_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PXCH");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=arctmin;              
                            shape.par("rmax")=arctmax;              
                            shape.par("phi1")=103.963;              
                            shape.par("phi2")=216;              
                            shape.par("dz")=halfladzlen;              
                            /// Shape Tubs rmin=arctmin rmax=arctmax phi1=103.963 phi2=216 dz=halfladzlen               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXCH;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXCH:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXCH     
          // ---------------------------------------------------------------------------------------------------     
          void PXCB::Block( AgCreate create )     
          {         
                ///@addtogroup PXCB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      { AgAttribute attr = AgAttribute("PXCB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Carbon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Carbon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Tubs");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=arcbmin;              
                            shape.par("rmax")=arcbmax;              
                            shape.par("phi1")=216;              
                            shape.par("phi2")=267.396;              
                            shape.par("dz")=halfladzlen;              
                            /// Shape Tubs rmin=arcbmin rmax=arcbmax phi1=216 phi2=267.396 dz=halfladzlen               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_PXCB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_PXCB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block PXCB     
          // ---------------------------------------------------------------------------------------------------     
          void PLAC::Block( AgCreate create )     
          {         
                ///@addtogroup PLAC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Sensitive isvol=1            
                      { AgMaterial &mat = AgMaterial::Get("Sensitive");              
                            mat.par("isvol")=1;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("PLAC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      /// Material Silicon            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Silicon");              
                            _material = mat;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=halfpixr;              
                            shape.par("dy")=halfpixthk;              
                            shape.par("dz")=halfpixz;              
                            /// Shape Bbox dx=halfpixr dy=halfpixthk dz=halfpixz               
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
       void PixlGeo4::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup PixlGeo4_revision        
             ///@{           
                   /// Author: JB           
             ///@}        
             ///@addtogroup PixlGeo4_revision        
             ///@{           
                   /// Created: Dec.20 2011           
             ///@}        
             AddBlock("PXMO");        
             AddBlock("PXLA");        
             AddBlock("PXRB");        
             AddBlock("PXLB");        
             AddBlock("PXIB");        
             AddBlock("PXTR");        
             AddBlock("PXTM");        
             AddBlock("PXTL");        
             AddBlock("PXTJ");        
             AddBlock("PXCA");        
             AddBlock("PXCB");        
             AddBlock("PXCC");        
             AddBlock("PXCD");        
             AddBlock("PXCE");        
             AddBlock("PXCF");        
             AddBlock("PXCG");        
             AddBlock("PXCH");        
             AddBlock("PLAC");        
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
 