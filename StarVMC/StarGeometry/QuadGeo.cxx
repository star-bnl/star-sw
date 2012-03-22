#include "QuadGeo.h"  
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
 namespace QUADGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup shlq_doc     
          /// \class Shlq_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t q0;     
          ///Float_t motherr;     
          ///Float_t motherl;     
          ///Float_t xoffset;     
          ///Float_t angle;     
          ///Float_t dzerol;     
          ///Float_t dzerori;     
          ///Float_t dzeroro;     
          ///Float_t q1;     
          ///Float_t ri1;     
          ///Float_t ro1;     
          ///Float_t dz1;     
          ///Float_t q2;     
          ///Float_t ri2;     
          ///Float_t ro2;     
          ///Float_t dz2;     
          ///Float_t q3;     
          ///Float_t ri3;     
          ///Float_t ro3;     
          ///Float_t dz3;     
          ///Int_t _index;     
          //     
          Shlq_t shlq;     
          //     
          ///@addtogroup QuadGeo_vars     
          ///@{        
                Float_t zquad;        
                //        
                /// Float_t zquad        
          ///@}     
       QuadGeo::QuadGeo()     
         : AgModule("QuadGeo"," is the description of all the magnets upstream inclusive of D0 ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void MGMT::Block( AgCreate create )     
          {         
                ///@addtogroup MGMT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("MGMT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0.0;              
                            shape.par("rmax")=shlq.motherr;              
                            shape.par("dz")=shlq.motherl/2.0;              
                            /// Shape Tube rmin=0.0 rmax=shlq.motherr dz=shlq.motherl/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_MGMT;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("DZER");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create DZER              
                            Create("DZER");               
                      }           
                      { AgPlacement place = AgPlacement("DZER","MGMT");              
                            /// Add daughter volume DZER to mother MGMT              
                            place.TranslateZ(-shlq.motherl/2.0+shlq.dzerol/2.0);              
                            /// Translate z = -shlq.motherl/2.0+shlq.dzerol/2.0              
                            _stacker -> Position( AgBlock::Find("DZER"), place );              
                      } // end placement of DZER           
                      _create = AgCreate("QONE");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create QONE              
                            Create("QONE");               
                      }           
                      { AgPlacement place = AgPlacement("QONE","MGMT");              
                            /// Add daughter volume QONE to mother MGMT              
                            place.TranslateZ(-shlq.motherl/2.0+(shlq.q1+shlq.dzerol)+shlq.dz1/2.0);              
                            /// Translate z = -shlq.motherl/2.0+(shlq.q1+shlq.dzerol)+shlq.dz1/2.0              
                            _stacker -> Position( AgBlock::Find("QONE"), place );              
                      } // end placement of QONE           
                      _create = AgCreate("QTWO");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create QTWO              
                            Create("QTWO");               
                      }           
                      { AgPlacement place = AgPlacement("QTWO","MGMT");              
                            /// Add daughter volume QTWO to mother MGMT              
                            place.TranslateZ(-shlq.motherl/2.0+(shlq.q2+shlq.dzerol)+shlq.dz2/2.0);              
                            /// Translate z = -shlq.motherl/2.0+(shlq.q2+shlq.dzerol)+shlq.dz2/2.0              
                            _stacker -> Position( AgBlock::Find("QTWO"), place );              
                      } // end placement of QTWO           
                      _create = AgCreate("QTHR");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create QTHR              
                            Create("QTHR");               
                      }           
                      { AgPlacement place = AgPlacement("QTHR","MGMT");              
                            /// Add daughter volume QTHR to mother MGMT              
                            place.TranslateZ(-shlq.motherl/2.0+(shlq.q3+shlq.dzerol)+shlq.dz3/2.0);              
                            /// Translate z = -shlq.motherl/2.0+(shlq.q3+shlq.dzerol)+shlq.dz3/2.0              
                            _stacker -> Position( AgBlock::Find("QTHR"), place );              
                      } // end placement of QTHR           
                      END_OF_MGMT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block MGMT     
          // ---------------------------------------------------------------------------------------------------     
          void DZER::Block( AgCreate create )     
          {         
                ///@addtogroup DZER_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("DZER");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=shlq.dzerori;              
                            shape.par("rmax")=shlq.dzeroro;              
                            shape.par("dz")=shlq.dzerol/2.0;              
                            /// Shape Tube rmin=shlq.dzerori rmax=shlq.dzeroro dz=shlq.dzerol/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_DZER;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_DZER:           
                      mCurrent = _save;           
                ///@}        
          } // End Block DZER     
          // ---------------------------------------------------------------------------------------------------     
          void QONE::Block( AgCreate create )     
          {         
                ///@addtogroup QONE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("QONE");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=shlq.ri1;              
                            shape.par("rmax")=shlq.ro1;              
                            shape.par("dz")=shlq.dz1/2.0;              
                            /// Shape Tube rmin=shlq.ri1 rmax=shlq.ro1 dz=shlq.dz1/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_QONE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_QONE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block QONE     
          // ---------------------------------------------------------------------------------------------------     
          void QTWO::Block( AgCreate create )     
          {         
                ///@addtogroup QTWO_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("QTWO");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=shlq.ri2;              
                            shape.par("rmax")=shlq.ro2;              
                            shape.par("dz")=shlq.dz2/2.0;              
                            /// Shape Tube rmin=shlq.ri2 rmax=shlq.ro2 dz=shlq.dz2/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_QTWO;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_QTWO:           
                      mCurrent = _save;           
                ///@}        
          } // End Block QTWO     
          // ---------------------------------------------------------------------------------------------------     
          void QTHR::Block( AgCreate create )     
          {         
                ///@addtogroup QTHR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Iron            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Iron");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("QTHR");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=shlq.ri3;              
                            shape.par("rmax")=shlq.ro3;              
                            shape.par("dz")=shlq.dz3/2.0;              
                            /// Shape Tube rmin=shlq.ri3 rmax=shlq.ro3 dz=shlq.dz3/2.0               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_QTHR;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_QTHR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block QTHR     
    // ----------------------------------------------------------------------- geoctr
       void QuadGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup QuadGeo_revision        
             ///@{           
                   /// Created:   02-Sep-2005             
             ///@}        
             ///@addtogroup QuadGeo_revision        
             ///@{           
                   /// Author: Maxim Potekhin           
             ///@}        
             AddBlock("MGMT");        
             AddBlock("DZER");        
             AddBlock("QONE");        
             AddBlock("QTWO");        
             AddBlock("QTHR");        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup shlq_doc        
             ///@{           
                   ++shlq._index;           
                   shlq . version = 1; //  geometry version              
                   /// shlq . version = 1; //  geometry version              
                   shlq . q0 = 2485.26; //  offset point that corresponds to 1505.92 in CAD notation, end of D0            
                   /// shlq . q0 = 2485.26; //  offset point that corresponds to 1505.92 in CAD notation, end of D0            
                   shlq . motherr = 19; //  radius of the mother containing D0,Q1,Q2,Q3            
                   /// shlq . motherr = 19; //  radius of the mother containing D0,Q1,Q2,Q3            
                   shlq . motherl = 1610; //  length of the mother containing D0,Q1,Q2,Q3            
                   /// shlq . motherl = 1610; //  length of the mother containing D0,Q1,Q2,Q3            
                   shlq . xoffset = 26; //  Offset ot the mother            
                   /// shlq . xoffset = 26; //  Offset ot the mother            
                   shlq . angle = 0.3; //  Angle to the symmetry axis            
                   /// shlq . angle = 0.3; //  Angle to the symmetry axis            
                   shlq . dzerol = 385.26; //  D0 length            
                   /// shlq . dzerol = 385.26; //  D0 length            
                   shlq . dzerori = 4.775; //  D0 inner radius            
                   /// shlq . dzerori = 4.775; //  D0 inner radius            
                   shlq . dzeroro = 15.995; //  D0 outer radius            
                   /// shlq . dzeroro = 15.995; //  D0 outer radius            
                   shlq . q1 = 88.59; //  offset 1            
                   /// shlq . q1 = 88.59; //  offset 1            
                   shlq . ri1 = 6.355; //  inner 1            
                   /// shlq . ri1 = 6.355; //  inner 1            
                   shlq . ro1 = 18.28; //  outer 1            
                   /// shlq . ro1 = 18.28; //  outer 1            
                   shlq . dz1 = 170.92; //  full length 1            
                   /// shlq . dz1 = 170.92; //  full length 1            
                   shlq . q2 = 324.90; //  offset 2            
                   /// shlq . q2 = 324.90; //  offset 2            
                   shlq . ri2 = 6.355; //  inner 2            
                   /// shlq . ri2 = 6.355; //  inner 2            
                   shlq . ro2 = 18.28; //  outer 2            
                   /// shlq . ro2 = 18.28; //  outer 2            
                   shlq . dz2 = 444.02; //  full length 2            
                   /// shlq . dz2 = 444.02; //  full length 2            
                   shlq . q3 = 823.94; //  offset 3            
                   /// shlq . q3 = 823.94; //  offset 3            
                   shlq . ri3 = 6.355; //  inner 3            
                   /// shlq . ri3 = 6.355; //  inner 3            
                   shlq . ro3 = 18.28; //  outer 3            
                   /// shlq . ro3 = 18.28; //  outer 3            
                   shlq . dz3 = 399.55; //  full length 3            
                   /// shlq . dz3 = 399.55; //  full length 3            
                   //           
                   shlq.fill();           
             ///@}        
             //        
             /// USE shlq _index=1;        
             shlq.Use();        
             zquad = shlq.q0-shlq.dzerol+shlq.motherl/2.0;        
             _create = AgCreate("MGMT");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create MGMT           
                   Create("MGMT");            
             }        
             { AgPlacement place = AgPlacement("MGMT","CAVE");           
                   /// Add daughter volume MGMT to mother CAVE           
                   place.TranslateX(shlq.xoffset);           
                   /// Translate x = shlq.xoffset           
                   place.TranslateY(0);           
                   /// Translate y = 0           
                   place.TranslateZ(zquad);           
                   /// Translate z = zquad           
                   place.AlphaY(shlq.angle);           
                   /// Rotate: AlphaY = shlq.angle           
                   /// G3 Reference: thetax = 90           
                   /// G3 Reference: phix = 0           
                   /// G3 Reference: thetay = 90           
                   /// G3 Reference: phiy = 90           
                   /// G3 Reference: thetaz = 0           
                   /// G3 Reference: phiz = 0           
                   _stacker -> Position( AgBlock::Find("MGMT"), place );           
             } // end placement of MGMT        
             { AgPlacement place = AgPlacement("MGMT","CAVE");           
                   /// Add daughter volume MGMT to mother CAVE           
                   place.TranslateX(-shlq.xoffset);           
                   /// Translate x = -shlq.xoffset           
                   place.TranslateY(0);           
                   /// Translate y = 0           
                   place.TranslateZ(zquad);           
                   /// Translate z = zquad           
                   place.AlphaY(-shlq.angle);           
                   /// Rotate: AlphaY = -shlq.angle           
                   /// G3 Reference: thetax = 90           
                   /// G3 Reference: phix = 0           
                   /// G3 Reference: thetay = 90           
                   /// G3 Reference: phiy = 90           
                   /// G3 Reference: thetaz = 0           
                   /// G3 Reference: phiz = 0           
                   _stacker -> Position( AgBlock::Find("MGMT"), place );           
             } // end placement of MGMT        
             { AgPlacement place = AgPlacement("MGMT","CAVE");           
                   /// Add daughter volume MGMT to mother CAVE           
                   place.TranslateX(shlq.xoffset);           
                   /// Translate x = shlq.xoffset           
                   place.TranslateY(0);           
                   /// Translate y = 0           
                   place.TranslateZ(-zquad);           
                   /// Translate z = -zquad           
                   place.AlphaY(180-shlq.angle);           
                   /// Rotate: AlphaY = 180-shlq.angle           
                   /// G3 Reference: thetax = 90           
                   /// G3 Reference: phix = 0           
                   /// G3 Reference: thetay = 90           
                   /// G3 Reference: phiy = 90           
                   /// G3 Reference: thetaz = 0           
                   /// G3 Reference: phiz = 0           
                   _stacker -> Position( AgBlock::Find("MGMT"), place );           
             } // end placement of MGMT        
             { AgPlacement place = AgPlacement("MGMT","CAVE");           
                   /// Add daughter volume MGMT to mother CAVE           
                   place.TranslateX(-shlq.xoffset);           
                   /// Translate x = -shlq.xoffset           
                   place.TranslateY(0);           
                   /// Translate y = 0           
                   place.TranslateZ(-zquad);           
                   /// Translate z = -zquad           
                   place.AlphaY(180+shlq.angle);           
                   /// Rotate: AlphaY = 180+shlq.angle           
                   /// G3 Reference: thetax = 90           
                   /// G3 Reference: phix = 0           
                   /// G3 Reference: thetay = 90           
                   /// G3 Reference: phiy = 90           
                   /// G3 Reference: thetaz = 0           
                   /// G3 Reference: phiz = 0           
                   _stacker -> Position( AgBlock::Find("MGMT"), place );           
             } // end placement of MGMT        
       }; // QuadGeo     
 }; // namespace QuadGeo  
 