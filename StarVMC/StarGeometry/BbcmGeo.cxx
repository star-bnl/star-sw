#include "BbcmGeo.h"  
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
 namespace BBCMGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup bbcg_doc     
          /// \class Bbcg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Array_t<Float_t> onoff;     
          ///Array_t<Float_t> zdis;     
          ///Int_t _index;     
          //     
          Bbcg_t bbcg;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup hexg_doc     
          /// \class Hexg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t type;     
          ///Float_t irad;     
          ///Float_t clad;     
          ///Float_t thick;     
          ///Float_t zoffset;     
          ///Float_t xoffset;     
          ///Float_t yoffset;     
          ///Int_t _index;     
          //     
          Hexg_t hexg;     
          //     
          ///@addtogroup BbcmGeo_vars     
          ///@{        
                float actr,srad,lrad,ztotal,x0,y0,theta0,phi0,xtrip,ytrip,rtrip,thetrip,rsing,thesing;        
                //        
                /// float actr,srad,lrad,ztotal,x0,y0,theta0,phi0,xtrip,ytrip,rtrip,thetrip,rsing,thesing        
          ///@}     
          ///@addtogroup BbcmGeo_vars     
          ///@{        
                int i_trip=0,j_sing=0;        
                //        
                /// int i_trip=0,j_sing=0        
          ///@}     
       BbcmGeo::BbcmGeo()     
         : AgModule("BbcmGeo"," is the Beam Beam Counter Modules GEOmetry ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void BBCM::Block( AgCreate create )     
          {         
                ///@addtogroup BBCM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Medium standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("BBCM");              
                            attr.par("seen")=0;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=lrad;              
                            shape.par("dz")=ztotal/2;              
                            /// Shape Tube rmin=0 rmax=lrad dz=ztotal/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BBCM;              
                            _stacker -> Build(this);              
                      }           
                      /// USE hexg type=1 ;           
                      hexg.Use("type",(Float_t)1 );           
                      if ( bbcg.onoff(2)==1||bbcg.onoff(2)==3 )           
                      {              
                            _create = AgCreate("BBCA");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create BBCA                 
                                  Create("BBCA");                  
                            }              
                            { AgPlacement place = AgPlacement("BBCA","BBCM");                 
                                  /// Add daughter volume BBCA to mother BBCM                 
                                  place.TranslateX(hexg.xoffset);                 
                                  /// Translate x = hexg.xoffset                 
                                  place.TranslateY(hexg.yoffset);                 
                                  /// Translate y = hexg.yoffset                 
                                  place.TranslateZ(hexg.zoffset);                 
                                  /// Translate z = hexg.zoffset                 
                                  _stacker -> Position( AgBlock::Find("BBCA"), place );                 
                            } // end placement of BBCA              
                      }           
                      /// USE hexg type=2 ;           
                      hexg.Use("type",(Float_t)2 );           
                      if ( bbcg.onoff(3)==1||bbcg.onoff(3)==3 )           
                      {              
                            _create = AgCreate("BBCA");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create BBCA                 
                                  Create("BBCA");                  
                            }              
                            { AgPlacement place = AgPlacement("BBCA","BBCM");                 
                                  /// Add daughter volume BBCA to mother BBCM                 
                                  place.TranslateX(hexg.xoffset);                 
                                  /// Translate x = hexg.xoffset                 
                                  place.TranslateY(hexg.yoffset);                 
                                  /// Translate y = hexg.yoffset                 
                                  place.TranslateZ(hexg.zoffset);                 
                                  /// Translate z = hexg.zoffset                 
                                  _stacker -> Position( AgBlock::Find("BBCA"), place );                 
                            } // end placement of BBCA              
                      }           
                      END_OF_BBCM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BBCM     
          // ---------------------------------------------------------------------------------------------------     
          void BBCA::Block( AgCreate create )     
          {         
                ///@addtogroup BBCA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Medium standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("BBCA");              
                            attr.par("seen")=0;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=hexg.irad;              
                            shape.par("rmax")=hexg.irad*6.0;              
                            shape.par("dz")=hexg.thick/2;              
                            /// Shape Tube rmin=hexg.irad rmax=hexg.irad*6.0 dz=hexg.thick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BBCA;              
                            _stacker -> Build(this);              
                      }           
                      x0=hexg.irad*tan(pi/6.0);           
                      y0=hexg.irad*3.0;           
                      rtrip = sqrt(x0*x0+y0*y0);           
                      theta0 = atan(y0/x0);           
                      /// Loop on i_trip from 0 to 5 step=1           
                      for ( i_trip=0; (1>0)? (i_trip<=5):(i_trip>=5); i_trip+=1 )           
                      {              
                            phi0 = i_trip*60;              
                            thetrip = theta0+i_trip*pi/3.0;              
                            xtrip = rtrip*cos(thetrip);              
                            ytrip = rtrip*sin(thetrip);              
                            _create = AgCreate("THXM");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create THXM                 
                                  Create("THXM");                  
                            }              
                            { AgPlacement place = AgPlacement("THXM","BBCA");                 
                                  /// Add daughter volume THXM to mother BBCA                 
                                  place.TranslateX(xtrip);                 
                                  /// Translate x = xtrip                 
                                  place.TranslateY(ytrip);                 
                                  /// Translate y = ytrip                 
                                  place.TranslateZ(0);                 
                                  /// Translate z = 0                 
                                  place.par("only")=AgPlacement::kMany;                 
                                  /// Overlap: agplacement::kmany                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = phi0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90+phi0                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  Double_t _thetax=90,_phix=phi0,_thetay=90,_phiy=90+phi0,_thetaz=0,_phiz=0;                 
                                  place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );                 
                                  _stacker -> Position( AgBlock::Find("THXM"), place );                 
                            } // end placement of THXM              
                      }           
                      END_OF_BBCA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BBCA     
          // ---------------------------------------------------------------------------------------------------     
          void THXM::Block( AgCreate create )     
          {         
                ///@addtogroup THXM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Medium standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("THXM");              
                            attr.par("seen")=0;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=hexg.irad*2.0/sin(pi/3.0);              
                            shape.par("dz")=hexg.thick/2;              
                            /// Shape Tube rmin=0 rmax=hexg.irad*2.0/sin(pi/3.0) dz=hexg.thick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_THXM;              
                            _stacker -> Build(this);              
                      }           
                      /// Loop on j_sing from 0 to 2 step=1           
                      for ( j_sing=0; (1>0)? (j_sing<=2):(j_sing>=2); j_sing+=1 )           
                      {              
                            rsing=hexg.irad/sin(pi/3.0);              
                            thesing=j_sing*pi*2.0/3.0;              
                            _create = AgCreate("SHXT");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create SHXT                 
                                  Create("SHXT");                  
                            }              
                            { AgPlacement place = AgPlacement("SHXT","THXM");                 
                                  /// Add daughter volume SHXT to mother THXM                 
                                  place.TranslateX(rsing*cos(thesing));                 
                                  /// Translate x = rsing*cos(thesing)                 
                                  place.TranslateY(rsing*sin(thesing));                 
                                  /// Translate y = rsing*sin(thesing)                 
                                  place.TranslateZ(0);                 
                                  /// Translate z = 0                 
                                  _stacker -> Position( AgBlock::Find("SHXT"), place );                 
                            } // end placement of SHXT              
                      }           
                      END_OF_THXM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block THXM     
          // ---------------------------------------------------------------------------------------------------     
          void SHXT::Block( AgCreate create )     
          {         
                ///@addtogroup SHXT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      /// Medium standard           
                      {  AgMedium med = AgMedium::CopyMedium("Standard");              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("SHXT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pgon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=0;              
                            shape.par("dphi")=360;              
                            shape.par("npdiv")=6;              
                            shape.par("nz")=2;              
                            shape.Z(0)=-hexg.thick/2;              
                            shape.Z(1)=hexg.thick/2;              
                            shape.Rmin(0)=0;              
                            shape.Rmin(1)=0;              
                            shape.Rmax(0)=hexg.irad;              
                            shape.Rmax(1)=hexg.irad;              
                            /// Shape Pgon phi1=0 dphi=360 npdiv=6 nz=2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_SHXT;              
                            _stacker -> Build(this);              
                      }           
                      actr = hexg.irad-hexg.clad;           
                      _create = AgCreate("CLAD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create CLAD              
                            Create("CLAD");               
                      }           
                      { AgPlacement place = AgPlacement("CLAD","SHXT");              
                            /// Add daughter volume CLAD to mother SHXT              
                            place.TranslateX(0);              
                            /// Translate x = 0              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("CLAD"), place );              
                      } // end placement of CLAD           
                      _create = AgCreate("BPOL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create BPOL              
                            Create("BPOL");               
                      }           
                      { AgPlacement place = AgPlacement("BPOL","SHXT");              
                            /// Add daughter volume BPOL to mother SHXT              
                            place.TranslateX(0);              
                            /// Translate x = 0              
                            place.TranslateY(0);              
                            /// Translate y = 0              
                            place.TranslateZ(0);              
                            /// Translate z = 0              
                            _stacker -> Position( AgBlock::Find("BPOL"), place );              
                      } // end placement of BPOL           
                      END_OF_SHXT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block SHXT     
          // ---------------------------------------------------------------------------------------------------     
          void CLAD::Block( AgCreate create )     
          {         
                ///@addtogroup CLAD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material ALKAP            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Alkap");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("CLAD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pgon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=0;              
                            shape.par("dphi")=360;              
                            shape.par("npdiv")=6;              
                            shape.par("nz")=2;              
                            shape.Z(0)=-hexg.thick/2;              
                            shape.Z(1)=hexg.thick/2;              
                            shape.Rmin(0)=actr;              
                            shape.Rmin(1)=actr;              
                            shape.Rmax(0)=hexg.irad;              
                            shape.Rmax(1)=hexg.irad;              
                            /// Shape Pgon phi1=0 dphi=360 npdiv=6 nz=2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_CLAD;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_CLAD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block CLAD     
          // ---------------------------------------------------------------------------------------------------     
          void BPOL::Block( AgCreate create )     
          {         
                ///@addtogroup BPOL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material POLYSTYREN            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Polystyren");              
                            _material = mat;              
                      }           
                      /// Material Cpolystyren isvol=1            
                      { AgMaterial &mat = AgMaterial::Get("Cpolystyren");              
                            mat.par("isvol")=1;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("BPOL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pgon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=0;              
                            shape.par("dphi")=360;              
                            shape.par("npdiv")=6;              
                            shape.par("nz")=2;              
                            shape.Z(0)=-hexg.thick/2;              
                            shape.Z(1)=hexg.thick/2;              
                            shape.Rmin(0)=0;              
                            shape.Rmin(1)=0;              
                            shape.Rmax(0)=actr;              
                            shape.Rmax(1)=actr;              
                            /// Shape Pgon phi1=0 dphi=360 npdiv=6 nz=2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_BPOL;              
                            _stacker -> Build(this);              
                      }           
                      // _medium.par("CUTGAM") = 0.00008;           
                      // _medium.par("CUTELE") = 0.001;           
                      // _medium.par("BCUTE") = 0.0001;           
                      // _medium.par("CUTNEU") = 0.001;           
                      // _medium.par("CUTHAD") = 0.001;           
                      // _medium.par("CUTMUO") = 0.001;           
                      // _medium.par("BIRK1") = 1.000;           
                      // _medium.par("BIRK2") = 0.013;           
                      // _medium.par("BIRK3") = 9.6E-6;           
                      END_OF_BPOL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block BPOL     
    // ----------------------------------------------------------------------- geoctr
       void BbcmGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup BbcmGeo_revision        
             ///@{           
                   /// Created: 15 march 2002           
             ///@}        
             ///@addtogroup BbcmGeo_revision        
             ///@{           
                   /// Author: Yiqun Wang           
             ///@}        
             AddBlock("BBCM");        
             AddBlock("BBCA");        
             AddBlock("THXM");        
             AddBlock("SHXT");        
             AddBlock("BPOL");        
             AddBlock("CLAD");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup bbcg_doc        
             ///@{           
                   ++bbcg._index;           
                   bbcg . version = 1.0; //  Geometry version            
                   /// bbcg . version = 1.0; //  Geometry version            
                   bbcg . onoff.at(0) = 3; //  0 off, 1 west on, 2 east on, 3 both on: for BBC,Small tiles,Large tiles            
                   ///bbcg . onoff.at(0) = 3; //  0 off, 1 west on, 2 east on, 3 both on: for BBC,Small tiles,Large tiles            
                   bbcg . onoff.at(1) = 3; //  0 off, 1 west on, 2 east on, 3 both on: for BBC,Small tiles,Large tiles            
                   ///bbcg . onoff.at(1) = 3; //  0 off, 1 west on, 2 east on, 3 both on: for BBC,Small tiles,Large tiles            
                   bbcg . onoff.at(2) = 3; //  0 off, 1 west on, 2 east on, 3 both on: for BBC,Small tiles,Large tiles            
                   ///bbcg . onoff.at(2) = 3; //  0 off, 1 west on, 2 east on, 3 both on: for BBC,Small tiles,Large tiles            
                   bbcg . zdis.at(0) = 374.24; //  z-coord from center in STAR (715/2+6*2.54+1=373.8)            
                   ///bbcg . zdis.at(0) = 374.24; //  z-coord from center in STAR (715/2+6*2.54+1=373.8)            
                   bbcg . zdis.at(1) = -374.24; //  z-coord from center in STAR (715/2+6*2.54+1=373.8)            
                   ///bbcg . zdis.at(1) = -374.24; //  z-coord from center in STAR (715/2+6*2.54+1=373.8)            
                   //           
                   bbcg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup hexg_doc        
             ///@{           
                   ++hexg._index;           
                   hexg . type = 1; // 1 for small hex tile, 2 for large tile            
                   /// hexg . type = 1; // 1 for small hex tile, 2 for large tile            
                   hexg . irad = 4.174; // inscribing circle radius =9.64/2*sin(60)=4.174            
                   /// hexg . irad = 4.174; // inscribing circle radius =9.64/2*sin(60)=4.174            
                   hexg . clad = 0.1; // cladding thickness            
                   /// hexg . clad = 0.1; // cladding thickness            
                   hexg . thick = 1.0; // thickness of tile            
                   /// hexg . thick = 1.0; // thickness of tile            
                   hexg . zoffset = 1.5; // z-offset from center of BBCW (1), or BBCE (2)            
                   /// hexg . zoffset = 1.5; // z-offset from center of BBCW (1), or BBCE (2)            
                   hexg . xoffset = 0.0; // x-offset center from beam for BBCW (1), or BBCE (2)            
                   /// hexg . xoffset = 0.0; // x-offset center from beam for BBCW (1), or BBCE (2)            
                   hexg . yoffset = 0.0; // y-offset center from beam for BBCW (1), or BBCE (2)            
                   /// hexg . yoffset = 0.0; // y-offset center from beam for BBCW (1), or BBCE (2)            
                   //           
                   hexg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup hexg_doc        
             ///@{           
                   ++hexg._index;           
                   hexg . type = 2; // 1 for small hex tile, 2 for large tile            
                   /// hexg . type = 2; // 1 for small hex tile, 2 for large tile            
                   hexg . irad = 16.697; // inscribing circle radius (4x that of small one)            
                   /// hexg . irad = 16.697; // inscribing circle radius (4x that of small one)            
                   hexg . clad = 0.1; // cladding of tile            
                   /// hexg . clad = 0.1; // cladding of tile            
                   hexg . thick = 1.0; // thickness of tile            
                   /// hexg . thick = 1.0; // thickness of tile            
                   hexg . zoffset = -1.5; // z-offset from center of BBCW (1), or BBCE (2)            
                   /// hexg . zoffset = -1.5; // z-offset from center of BBCW (1), or BBCE (2)            
                   hexg . xoffset = 0.0; // x-offset center from beam for BBCW (1), or BBCE (2)            
                   /// hexg . xoffset = 0.0; // x-offset center from beam for BBCW (1), or BBCE (2)            
                   hexg . yoffset = 0.0; // y-offset center from beam for BBCW (1), or BBCE (2)            
                   /// hexg . yoffset = 0.0; // y-offset center from beam for BBCW (1), or BBCE (2)            
                   //           
                   hexg.fill();           
             ///@}        
             //        
             /// USE bbcg _index=1;        
             bbcg.Use();        
             // Print<level=%i> fmt=%s fortran format statements not supported        
             /// Component C5	a=12	z=6	w=5      *10000        
             /// Component H4	a=1	z=1	w=4      *10000        
             /// Component O2	a=16	z=8	w=2      *10000        
             /// Component Al	a=27	z=13	w=0.2302 *10000        
             /// Mixture ALKAP dens=1.432        
             {  AgMaterial &mix = AgMaterial::Get("Alkap");           
                   mix.Component("C5",12,6,5      *10000);           
                   mix.Component("H4",1,1,4      *10000);           
                   mix.Component("O2",16,8,2      *10000);           
                   mix.Component("Al",27,13,0.2302 *10000);           
                   mix.par("dens")=1.432;           
                   mix.lock();           
                   _material = mix;           
                   _material.lock();           
             }        
             /// USE hexg type=1 ;        
             hexg.Use("type",(Float_t)1 );        
             srad   = hexg.irad*6.0;;        
             ztotal = hexg.thick+2*abs(hexg.zoffset);;        
             /// USE hexg type=2 ;        
             hexg.Use("type",(Float_t)2 );        
             lrad   = hexg.irad*6.0;;        
             ztotal = ztotal+hexg.thick+2*abs(hexg.zoffset);;        
             _create = AgCreate("BBCM");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create BBCM           
                   Create("BBCM");            
             }        
             if ( bbcg.onoff(1)==1||bbcg.onoff(1)==3 )        
             {           
                   { AgPlacement place = AgPlacement("BBCM","CAVE");              
                         /// Add daughter volume BBCM to mother CAVE              
                         place.TranslateX(0);              
                         /// Translate x = 0              
                         place.TranslateY(0);              
                         /// Translate y = 0              
                         place.TranslateZ(bbcg.zdis(1));              
                         /// Translate z = bbcg.zdis(1)              
                         _stacker -> Position( AgBlock::Find("BBCM"), place );              
                   } // end placement of BBCM           
             }        
             if ( bbcg.onoff(1)==2||bbcg.onoff(1)==3 )        
             {           
                   { AgPlacement place = AgPlacement("BBCM","CAVE");              
                         /// Add daughter volume BBCM to mother CAVE              
                         place.TranslateX(0);              
                         /// Translate x = 0              
                         place.TranslateY(0);              
                         /// Translate y = 0              
                         place.TranslateZ(bbcg.zdis(2));              
                         /// Translate z = bbcg.zdis(2)              
                         place.AlphaY(180);              
                         /// Rotate: AlphaY = 180              
                         /// G3 Reference: thetax = 90              
                         /// G3 Reference: phix = 0              
                         /// G3 Reference: thetay = 90              
                         /// G3 Reference: phiy = 90              
                         /// G3 Reference: thetaz = 0              
                         /// G3 Reference: phiz = 0              
                         _stacker -> Position( AgBlock::Find("BBCM"), place );              
                   } // end placement of BBCM           
             }        
             // Print<level=%i> fmt=%s fortran format statements not supported        
       }; // BbcmGeo     
 }; // namespace BbcmGeo  
 