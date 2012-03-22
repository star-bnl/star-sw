#include "FtpcGeo.h"  
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
 namespace FTPCGEO // $NMSPC  
 {     
       //     
       // ---------------------------------------------------------------------------------------------------     
          //  -----------------------------------------------------     
          /// @defgroup ftpg_doc     
          /// \class Ftpg_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t rinnerms;     
          ///Float_t routerms;     
          ///Float_t rgasout;     
          ///Float_t rrom;     
          ///Float_t relcard;     
          ///Float_t rcooplm;     
          ///Float_t rcoople;     
          ///Float_t zstart;     
          ///Float_t totlen;     
          ///Float_t laylen;     
          ///Float_t hitlay;     
          ///Float_t drinall1;     
          ///Float_t drinall2;     
          ///Float_t drinisol;     
          ///Float_t dzkapton;     
          ///Float_t drifr;     
          ///Float_t dzifr;     
          ///Float_t dzer;     
          ///Float_t dzrom;     
          ///Float_t dzsura;     
          ///Float_t dzsurb;     
          ///Float_t dzsmpr;     
          ///Float_t dzbipr;     
          ///Float_t msrdz;     
          ///Float_t serhole;     
          ///Float_t risring;     
          ///Float_t isringdz;     
          ///Float_t sbsrdx;     
          ///Float_t sbsrdy;     
          ///Float_t sbsrdz;     
          ///Float_t gasvoldz;     
          ///Int_t _index;     
          //     
          Ftpg_t ftpg;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup ffcc_doc     
          /// \class Ffcc_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t stileng;     
          ///Float_t stidia;     
          ///Float_t stirpos;     
          ///Float_t rithick;     
          ///Float_t ridr;     
          ///Float_t rigap;     
          ///Float_t barleng;     
          ///Float_t barwidt;     
          ///Float_t barthik;     
          ///Int_t _index;     
          //     
          Ffcc_t ffcc;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup frbd_doc     
          /// \class Frbd_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t phi1;     
          ///Float_t phi2;     
          ///Float_t phi3;     
          ///Float_t phi4;     
          ///Float_t phi5;     
          ///Float_t phi6;     
          ///Float_t phi7;     
          ///Float_t phi8;     
          ///Float_t phi9;     
          ///Float_t phi10;     
          ///Float_t phi11;     
          ///Float_t phi12;     
          ///Float_t phi13;     
          ///Float_t xrom;     
          ///Float_t yrom;     
          ///Float_t zrom;     
          ///Float_t rahol;     
          ///Float_t xehol;     
          ///Float_t yehol;     
          ///Float_t xlhol;     
          ///Float_t ylhol;     
          ///Float_t boffset;     
          ///Float_t zoffb;     
          ///Float_t modleng;     
          ///Float_t electrdx;     
          ///Float_t electrdy;     
          ///Float_t electrdz;     
          ///Float_t coolpldx;     
          ///Float_t coolpldy;     
          ///Float_t coolpldz;     
          ///Float_t eclpldx;     
          ///Float_t eclpldy;     
          ///Float_t eclpldz;     
          ///Float_t cakehir;     
          ///Float_t cakehor;     
          ///Float_t cakehwz;     
          ///Float_t boxhx;     
          ///Float_t boxhy;     
          ///Float_t boxhz;     
          ///Float_t eboxhx;     
          ///Float_t eboxhy;     
          ///Float_t eboxhz;     
          ///Float_t lboxhx;     
          ///Float_t lboxhy;     
          ///Float_t lboxhz;     
          ///Int_t _index;     
          //     
          Frbd_t frbd;     
          //     
          //  -----------------------------------------------------     
          /// @defgroup fssd_doc     
          /// \class Fssd_t     
          /// \brief User-defined structure     
          ///                             
          /// AgML structure members:     
          ///                             
          ///Float_t version;     
          ///Float_t eringrmn;     
          ///Float_t eringrmx;     
          ///Float_t eringdz;     
          ///Float_t oeringdz;     
          ///Float_t erposz;     
          ///Float_t meringrm;     
          ///Float_t meringdz;     
          ///Float_t erpolyrm;     
          ///Float_t trapr;     
          ///Float_t polyr;     
          ///Float_t polydz;     
          ///Float_t polyir;     
          ///Float_t polyor;     
          ///Float_t trapx1;     
          ///Float_t trapx2;     
          ///Float_t trapdy;     
          ///Float_t trapdz;     
          ///Float_t pgonpdz;     
          ///Float_t sbsdy;     
          ///Int_t _index;     
          //     
          Fssd_t fssd;     
          //     
          ///@addtogroup FtpcGeo_vars     
          ///@{        
                Int_t k,n,jj,ww,gg,hh,iring;        
                //        
                /// Int_t k,n,jj,ww,gg,hh,iring        
          ///@}     
          ///@addtogroup FtpcGeo_vars     
          ///@{        
                Int_t krueck;        
                //        
                /// Int_t krueck        
          ///@}     
          ///@addtogroup FtpcGeo_vars     
          ///@{        
                Float_t position,temp1,temp2,temp3;        
                //        
                /// Float_t position,temp1,temp2,temp3        
          ///@}     
          ///@addtogroup FtpcGeo_vars     
          ///@{        
                Float_t frob_x1;        
                //        
                /// Float_t frob_x1        
          ///@}     
          ///@addtogroup FtpcGeo_vars     
          ///@{        
          ///@}     
          ///@addtogroup FtpcGeo_vars     
          ///@{        
                Array_t<Int_t> iflaga(5);        
                /// iflaga(5) : array of Int_t        
                Array_t<Int_t> iflagb(5);        
                /// iflagb(5) : array of Int_t        
          ///@}     
       FtpcGeo::FtpcGeo()     
         : AgModule("FtpcGeo","  is the geometry of the Forward TPC in STAR ")     
       {        
       }     
          // ---------------------------------------------------------------------------------------------------     
          void FTPC::Block( AgCreate create )     
          {         
                ///@addtogroup FTPC_doc        
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
                      { AgAttribute attr = AgAttribute("FTPC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftpg.rinnerms;              
                            shape.par("rmax")=ftpg.routerms;              
                            shape.par("dz")=ftpg.totlen/2;              
                            /// Shape Tube rmin=ftpg.rinnerms rmax=ftpg.routerms dz=ftpg.totlen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FTPC;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FIAL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FIAL              
                            Create("FIAL");               
                      }           
                      { AgPlacement place = AgPlacement("FIAL","FTPC");              
                            /// Add daughter volume FIAL to mother FTPC              
                            _stacker -> Position( AgBlock::Find("FIAL"), place );              
                      } // end placement of FIAL           
                      _create = AgCreate("FMPT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FMPT              
                            Create("FMPT");               
                      }           
                      { AgPlacement place = AgPlacement("FMPT","FTPC");              
                            /// Add daughter volume FMPT to mother FTPC              
                            _stacker -> Position( AgBlock::Find("FMPT"), place );              
                      } // end placement of FMPT           
                      _create = AgCreate("FOAL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FOAL              
                            Create("FOAL");               
                      }           
                      { AgPlacement place = AgPlacement("FOAL","FTPC");              
                            /// Add daughter volume FOAL to mother FTPC              
                            _stacker -> Position( AgBlock::Find("FOAL"), place );              
                      } // end placement of FOAL           
                      _create = AgCreate("FPAD");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FPAD              
                            Create("FPAD");               
                      }           
                      { AgPlacement place = AgPlacement("FPAD","FTPC");              
                            /// Add daughter volume FPAD to mother FTPC              
                            _stacker -> Position( AgBlock::Find("FPAD"), place );              
                      } // end placement of FPAD           
                      _create = AgCreate("FGAS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FGAS              
                            Create("FGAS");               
                      }           
                      { AgPlacement place = AgPlacement("FGAS","FTPC");              
                            /// Add daughter volume FGAS to mother FTPC              
                            _stacker -> Position( AgBlock::Find("FGAS"), place );              
                      } // end placement of FGAS           
                      _create = AgCreate("FIFR");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FIFR              
                            Create("FIFR");               
                      }           
                      { AgPlacement place = AgPlacement("FIFR","FTPC");              
                            /// Add daughter volume FIFR to mother FTPC              
                            place.TranslateZ((ftpg.totlen/2)-(ftpg.dzifr/2)-ftpg.dzkapton);              
                            /// Translate z = (ftpg.totlen/2)-(ftpg.dzifr/2)-ftpg.dzkapton              
                            _stacker -> Position( AgBlock::Find("FIFR"), place );              
                      } // end placement of FIFR           
                      { AgPlacement place = AgPlacement("FIFR","FTPC");              
                            /// Add daughter volume FIFR to mother FTPC              
                            place.TranslateZ(-((ftpg.totlen/2)-(ftpg.dzifr/2)-ftpg.dzkapton));              
                            /// Translate z = -((ftpg.totlen/2)-(ftpg.dzifr/2)-ftpg.dzkapton)              
                            _stacker -> Position( AgBlock::Find("FIFR"), place );              
                      } // end placement of FIFR           
                      _create = AgCreate("FKWI");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FKWI              
                            Create("FKWI");               
                      }           
                      { AgPlacement place = AgPlacement("FKWI","FTPC");              
                            /// Add daughter volume FKWI to mother FTPC              
                            place.TranslateZ((ftpg.totlen/2)-ftpg.dzkapton/2);              
                            /// Translate z = (ftpg.totlen/2)-ftpg.dzkapton/2              
                            _stacker -> Position( AgBlock::Find("FKWI"), place );              
                      } // end placement of FKWI           
                      { AgPlacement place = AgPlacement("FKWI","FTPC");              
                            /// Add daughter volume FKWI to mother FTPC              
                            place.TranslateZ(-((ftpg.totlen/2)-ftpg.dzkapton/2));              
                            /// Translate z = -((ftpg.totlen/2)-ftpg.dzkapton/2)              
                            _stacker -> Position( AgBlock::Find("FKWI"), place );              
                      } // end placement of FKWI           
                      _create = AgCreate("FSER");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSER              
                            Create("FSER");               
                      }           
                      { AgPlacement place = AgPlacement("FSER","FTPC");              
                            /// Add daughter volume FSER to mother FTPC              
                            place.TranslateZ((ftpg.totlen/2.)-(ftpg.dzer/2.));              
                            /// Translate z = (ftpg.totlen/2.)-(ftpg.dzer/2.)              
                            _stacker -> Position( AgBlock::Find("FSER"), place );              
                      } // end placement of FSER           
                      { AgPlacement place = AgPlacement("FSER","FTPC");              
                            /// Add daughter volume FSER to mother FTPC              
                            place.TranslateZ(-((ftpg.totlen/2.)-(ftpg.dzer/2.)));              
                            /// Translate z = -((ftpg.totlen/2.)-(ftpg.dzer/2.))              
                            place.AlphaX(180);              
                            /// Rotate: AlphaX = 180              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FSER"), place );              
                      } // end placement of FSER           
                      _create = AgCreate("FSSM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSSM              
                            Create("FSSM");               
                      }           
                      { AgPlacement place = AgPlacement("FSSM","FTPC");              
                            /// Add daughter volume FSSM to mother FTPC              
                            place.TranslateZ(ftpg.dzsurb);              
                            /// Translate z = ftpg.dzsurb              
                            _stacker -> Position( AgBlock::Find("FSSM"), place );              
                      } // end placement of FSSM           
                      _create = AgCreate("FSSM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSSM              
                            Create("FSSM");               
                      }           
                      { AgPlacement place = AgPlacement("FSSM","FTPC");              
                            /// Add daughter volume FSSM to mother FTPC              
                            place.TranslateZ(ftpg.dzsura);              
                            /// Translate z = ftpg.dzsura              
                            _stacker -> Position( AgBlock::Find("FSSM"), place );              
                      } // end placement of FSSM           
                      _create = AgCreate("FSSM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSSM              
                            Create("FSSM");               
                      }           
                      { AgPlacement place = AgPlacement("FSSM","FTPC");              
                            /// Add daughter volume FSSM to mother FTPC              
                            place.TranslateZ(-(ftpg.dzsura));              
                            /// Translate z = -(ftpg.dzsura)              
                            _stacker -> Position( AgBlock::Find("FSSM"), place );              
                      } // end placement of FSSM           
                      _create = AgCreate("FSSM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSSM              
                            Create("FSSM");               
                      }           
                      { AgPlacement place = AgPlacement("FSSM","FTPC");              
                            /// Add daughter volume FSSM to mother FTPC              
                            place.TranslateZ(-(ftpg.dzsurb));              
                            /// Translate z = -(ftpg.dzsurb)              
                            _stacker -> Position( AgBlock::Find("FSSM"), place );              
                      } // end placement of FSSM           
                      { AgPlacement place = AgPlacement("FKWI","FTPC");              
                            /// Add daughter volume FKWI to mother FTPC              
                            place.TranslateZ(-((ftpg.totlen/2)-ftpg.dzkapton/2));              
                            /// Translate z = -((ftpg.totlen/2)-ftpg.dzkapton/2)              
                            _stacker -> Position( AgBlock::Find("FKWI"), place );              
                      } // end placement of FKWI           
                      temp3=(ftpg.totlen/2)-(ftpg.dzer)-((frbd.zrom-2.)/2.);           
                      _create = AgCreate("FROS");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FROS              
                            Create("FROS");               
                      }           
                      /// Loop on n from 1 to 5 step=1           
                      for ( n=1; (1>0)? (n<=5):(n>=5); n+=1 )           
                      {              
                            { AgPlacement place = AgPlacement("FROS","FTPC");                 
                                  /// Add daughter volume FROS to mother FTPC                 
                                  place.TranslateZ(temp3-((ftpg.dzrom)*(n-1)));                 
                                  /// Translate z = temp3-((ftpg.dzrom)*(n-1))                 
                                  _stacker -> Position( AgBlock::Find("FROS"), place );                 
                            } // end placement of FROS              
                      }           
                      END_OF_FTPC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FTPC     
          // ---------------------------------------------------------------------------------------------------     
          void FIAL::Block( AgCreate create )     
          {         
                ///@addtogroup FIAL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FIAL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftpg.rinnerms;              
                            shape.par("rmax")=ftpg.rinnerms+ftpg.drinall1;              
                            shape.par("dz")=(ftpg.totlen/2)-ftpg.dzifr-ftpg.dzkapton;              
                            /// Shape Tube rmin=ftpg.rinnerms rmax=ftpg.rinnerms+ftpg.drinall1 dz=(ftpg.totlen/2)-ftpg.dzifr-ftpg.dzkapton               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FIAL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FIAL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FIAL     
          // ---------------------------------------------------------------------------------------------------     
          void FMPT::Block( AgCreate create )     
          {         
                ///@addtogroup FMPT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material POLYETHYLENE            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Polyethylene");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FMPT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftpg.rinnerms+ftpg.drinall1;              
                            shape.par("rmax")=ftpg.rinnerms+ftpg.drinall1+ftpg.drinisol;              
                            shape.par("dz")=(ftpg.totlen/2)-ftpg.dzifr-ftpg.dzkapton;              
                            /// Shape Tube rmin=ftpg.rinnerms+ftpg.drinall1 rmax=ftpg.rinnerms+ftpg.drinall1+ftpg.drinisol dz=(ftpg.totlen/2)-ftpg.dzifr-ftpg.dzkapton               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FMPT;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FMPT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FMPT     
          // ---------------------------------------------------------------------------------------------------     
          void FOAL::Block( AgCreate create )     
          {         
                ///@addtogroup FOAL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FMPT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      temp1=ftpg.rinnerms+ftpg.drinall1+               ftpg.drinisol+ftpg.drinall2;           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftpg.rinnerms+ftpg.drinall1+ftpg.drinisol;              
                            shape.par("rmax")=temp1;              
                            shape.par("dz")=(ftpg.totlen/2)-ftpg.dzifr-ftpg.dzkapton;              
                            /// Shape Tube rmin=ftpg.rinnerms+ftpg.drinall1+ftpg.drinisol rmax=temp1 dz=(ftpg.totlen/2)-ftpg.dzifr-ftpg.dzkapton               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FOAL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FOAL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FOAL     
          // ---------------------------------------------------------------------------------------------------     
          void FGAS::Block( AgCreate create )     
          {         
                ///@addtogroup FGAS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Argon_gas            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Argon_gas");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FGAS");              
                            attr.par("seen")=1;              
                            attr.par("colo")=7;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      temp1=ftpg.rinnerms+ftpg.drinall1+               ftpg.drinisol+ftpg.drinall2;           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=temp1;              
                            shape.par("rmax")=ftpg.rgasout;              
                            shape.par("dz")=ftpg.totlen/2-ftpg.dzkapton;              
                            /// Shape Tube rmin=temp1 rmax=ftpg.rgasout dz=ftpg.totlen/2-ftpg.dzkapton               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FGAS;              
                            _stacker -> Build(this);              
                      }           
                      temp1=ftpg.totlen-2*(ftpg.dzkapton);           
                      _create = AgCreate("FSEN");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSEN              
                            Create("FSEN");               
                      }           
                      /// Loop on k from 1 to nint(ftpg.hitlay)/2 step=1           
                      for ( k=1; (1>0)? (k<=nint(ftpg.hitlay)/2):(k>=nint(ftpg.hitlay)/2); k+=1 )           
                      {              
                            krueck=nint(ftpg.hitlay)/2+1-k;              
                            { AgPlacement place = AgPlacement("FSEN","FGAS");                 
                                  /// Add daughter volume FSEN to mother FGAS                 
                                  place.TranslateZ(-((ftpg.dzsmpr/2.)+(ftpg.dzsmpr*iflagb(krueck))+(ftpg.dzbipr*iflaga(krueck))));                 
                                  /// Translate z = -((ftpg.dzsmpr/2.)+(ftpg.dzsmpr*iflagb(krueck))+(ftpg.dzbipr*iflaga(krueck)))                 
                                  _stacker -> Position( AgBlock::Find("FSEN"), place );                 
                            } // end placement of FSEN              
                      }           
                      /// Loop on k from 1 to nint(ftpg.hitlay)/2 step=1           
                      for ( k=1; (1>0)? (k<=nint(ftpg.hitlay)/2):(k>=nint(ftpg.hitlay)/2); k+=1 )           
                      {              
                            { AgPlacement place = AgPlacement("FSEN","FGAS");                 
                                  /// Add daughter volume FSEN to mother FGAS                 
                                  place.TranslateZ((ftpg.dzsmpr/2.)+(ftpg.dzsmpr*iflagb(k))+(ftpg.dzbipr*iflaga(k)));                 
                                  /// Translate z = (ftpg.dzsmpr/2.)+(ftpg.dzsmpr*iflagb(k))+(ftpg.dzbipr*iflaga(k))                 
                                  _stacker -> Position( AgBlock::Find("FSEN"), place );                 
                            } // end placement of FSEN              
                      }           
                      _create = AgCreate("FFSL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FFSL              
                            Create("FFSL");               
                      }           
                      { AgPlacement place = AgPlacement("FFSL","FGAS");              
                            /// Add daughter volume FFSL to mother FGAS              
                            place.TranslateY(ffcc.stirpos);              
                            /// Translate y = ffcc.stirpos              
                            place.TranslateZ((ftpg.totlen/2)-5);              
                            /// Translate z = (ftpg.totlen/2)-5              
                            place.AlphaZ(frbd.phi4);              
                            /// Rotate: AlphaZ = frbd.phi4              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            place.Ortho( "yzx" ); // ORT=yzx              
                            /// Axis substitution: XYZ --> yzx              
                            _stacker -> Position( AgBlock::Find("FFSL"), place );              
                      } // end placement of FFSL           
                      { AgPlacement place = AgPlacement("FFSL","FGAS");              
                            /// Add daughter volume FFSL to mother FGAS              
                            place.TranslateX(-ffcc.stirpos*cos(pi/6.));              
                            /// Translate x = -ffcc.stirpos*cos(pi/6.)              
                            place.TranslateY(-ffcc.stirpos*sin(pi/6.));              
                            /// Translate y = -ffcc.stirpos*sin(pi/6.)              
                            place.TranslateZ((ftpg.totlen/2)-5);              
                            /// Translate z = (ftpg.totlen/2)-5              
                            place.AlphaZ(frbd.phi8);              
                            /// Rotate: AlphaZ = frbd.phi8              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            place.Ortho( "yzx" ); // ORT=yzx              
                            /// Axis substitution: XYZ --> yzx              
                            _stacker -> Position( AgBlock::Find("FFSL"), place );              
                      } // end placement of FFSL           
                      { AgPlacement place = AgPlacement("FFSL","FGAS");              
                            /// Add daughter volume FFSL to mother FGAS              
                            place.TranslateX(ffcc.stirpos*cos(pi/6.));              
                            /// Translate x = ffcc.stirpos*cos(pi/6.)              
                            place.TranslateY(-ffcc.stirpos*sin(pi/6.));              
                            /// Translate y = -ffcc.stirpos*sin(pi/6.)              
                            place.TranslateZ((ftpg.totlen/2)-5);              
                            /// Translate z = (ftpg.totlen/2)-5              
                            place.AlphaZ(-(frbd.phi2));              
                            /// Rotate: AlphaZ = -(frbd.phi2)              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            place.Ortho( "yzx" ); // ORT=yzx              
                            /// Axis substitution: XYZ --> yzx              
                            _stacker -> Position( AgBlock::Find("FFSL"), place );              
                      } // end placement of FFSL           
                      { AgPlacement place = AgPlacement("FFSL","FGAS");              
                            /// Add daughter volume FFSL to mother FGAS              
                            place.TranslateY(ffcc.stirpos);              
                            /// Translate y = ffcc.stirpos              
                            place.TranslateZ(-ftpg.totlen/2+5);              
                            /// Translate z = -ftpg.totlen/2+5              
                            place.AlphaZ(frbd.phi4);              
                            /// Rotate: AlphaZ = frbd.phi4              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            place.Ortho( "yzx" ); // ORT=yzx              
                            /// Axis substitution: XYZ --> yzx              
                            _stacker -> Position( AgBlock::Find("FFSL"), place );              
                      } // end placement of FFSL           
                      { AgPlacement place = AgPlacement("FFSL","FGAS");              
                            /// Add daughter volume FFSL to mother FGAS              
                            place.TranslateX(-ffcc.stirpos*cos(pi/6.));              
                            /// Translate x = -ffcc.stirpos*cos(pi/6.)              
                            place.TranslateY(-ffcc.stirpos*sin(pi/6.));              
                            /// Translate y = -ffcc.stirpos*sin(pi/6.)              
                            place.TranslateZ(-ftpg.totlen/2+5);              
                            /// Translate z = -ftpg.totlen/2+5              
                            place.AlphaZ(frbd.phi8);              
                            /// Rotate: AlphaZ = frbd.phi8              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            place.Ortho( "yzx" ); // ORT=yzx              
                            /// Axis substitution: XYZ --> yzx              
                            _stacker -> Position( AgBlock::Find("FFSL"), place );              
                      } // end placement of FFSL           
                      { AgPlacement place = AgPlacement("FFSL","FGAS");              
                            /// Add daughter volume FFSL to mother FGAS              
                            place.TranslateX(ffcc.stirpos*cos(pi/6.));              
                            /// Translate x = ffcc.stirpos*cos(pi/6.)              
                            place.TranslateY(-ffcc.stirpos*sin(pi/6.));              
                            /// Translate y = -ffcc.stirpos*sin(pi/6.)              
                            place.TranslateZ(-ftpg.totlen/2+5);              
                            /// Translate z = -ftpg.totlen/2+5              
                            place.AlphaZ(-(frbd.phi2));              
                            /// Rotate: AlphaZ = -(frbd.phi2)              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            place.Ortho( "yzx" ); // ORT=yzx              
                            /// Axis substitution: XYZ --> yzx              
                            _stacker -> Position( AgBlock::Find("FFSL"), place );              
                      } // end placement of FFSL           
                      _create = AgCreate("FFCE");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FFCE              
                            Create("FFCE");               
                      }           
                      temp1 = ftpg.rinnerms+ftpg.drinall1+ftpg.drinisol+                 ftpg.drinall2;           
                      temp2 = ((ftpg.rgasout-temp1)/2)+temp1;           
                      { AgPlacement place = AgPlacement("FFCE","FGAS");              
                            /// Add daughter volume FFCE to mother FGAS              
                            place.TranslateY(temp2);              
                            /// Translate y = temp2              
                            place.TranslateZ((ftpg.totlen/2+ffcc.stidia/2+ffcc.barwidt/2)-5);              
                            /// Translate z = (ftpg.totlen/2+ffcc.stidia/2+ffcc.barwidt/2)-5              
                            _stacker -> Position( AgBlock::Find("FFCE"), place );              
                      } // end placement of FFCE           
                      { AgPlacement place = AgPlacement("FFCE","FGAS");              
                            /// Add daughter volume FFCE to mother FGAS              
                            place.TranslateX(-temp2*cos(pi/6.));              
                            /// Translate x = -temp2*cos(pi/6.)              
                            place.TranslateY(-temp2*sin(pi/6.));              
                            /// Translate y = -temp2*sin(pi/6.)              
                            place.TranslateZ((ftpg.totlen/2+ffcc.stidia/2+ffcc.barwidt/2)-5);              
                            /// Translate z = (ftpg.totlen/2+ffcc.stidia/2+ffcc.barwidt/2)-5              
                            place.AlphaZ(-(frbd.phi3));              
                            /// Rotate: AlphaZ = -(frbd.phi3)              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FFCE"), place );              
                      } // end placement of FFCE           
                      { AgPlacement place = AgPlacement("FFCE","FGAS");              
                            /// Add daughter volume FFCE to mother FGAS              
                            place.TranslateX(temp2*cos(pi/6.));              
                            /// Translate x = temp2*cos(pi/6.)              
                            place.TranslateY(-temp2*sin(pi/6.));              
                            /// Translate y = -temp2*sin(pi/6.)              
                            place.TranslateZ((ftpg.totlen/2+ffcc.stidia/2+ffcc.barwidt/2)-5);              
                            /// Translate z = (ftpg.totlen/2+ffcc.stidia/2+ffcc.barwidt/2)-5              
                            place.AlphaZ(frbd.phi3);              
                            /// Rotate: AlphaZ = frbd.phi3              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FFCE"), place );              
                      } // end placement of FFCE           
                      /// Loop on iring from 17 to 1 step=-1           
                      for ( iring=17; (-1>0)? (iring<=1):(iring>=1); iring+=-1 )           
                      {              
                            _create = AgCreate("FFRA");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FFRA                 
                                  Create("FFRA");                  
                            }              
                            { AgPlacement place = AgPlacement("FFRA","FGAS");                 
                                  /// Add daughter volume FFRA to mother FGAS                 
                                  place.TranslateZ((ftpg.totlen/2)-5-ffcc.stidia/2);                 
                                  /// Translate z = (ftpg.totlen/2)-5-ffcc.stidia/2                 
                                  _stacker -> Position( AgBlock::Find("FFRA"), place );                 
                            } // end placement of FFRA              
                            { AgPlacement place = AgPlacement("FFRA","FGAS");                 
                                  /// Add daughter volume FFRA to mother FGAS                 
                                  place.TranslateZ(-ftpg.totlen/2+5+ffcc.stidia/2);                 
                                  /// Translate z = -ftpg.totlen/2+5+ffcc.stidia/2                 
                                  _stacker -> Position( AgBlock::Find("FFRA"), place );                 
                            } // end placement of FFRA              
                      }           
                      END_OF_FGAS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FGAS     
          // ---------------------------------------------------------------------------------------------------     
          void FSEN::Block( AgCreate create )     
          {         
                ///@addtogroup FSEN_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Argon_gas            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Argon_gas");              
                            _material = mat;              
                      }           
                      /// Medium sensitive           
                      ///  isvol = 1            
                      {  AgMedium &med = AgMedium::Get("Sensitive");              
                               med.Inherit(this);              
                            med.par("isvol")=1 ;              
                            _medium = med;              
                      }           
                      { AgAttribute attr = AgAttribute("FSEN");              
                            attr.par("seen")=1;              
                            attr.par("colo")=4;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      temp1=ftpg.rinnerms+ftpg.drinall1+             ftpg.drinisol+ftpg.drinall2;           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=temp1;              
                            shape.par("rmax")=ftpg.rgasout;              
                            shape.par("dz")=ftpg.laylen/2;              
                            /// Shape Tube rmin=temp1 rmax=ftpg.rgasout dz=ftpg.laylen/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSEN;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FSEC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSEC              
                            Create("FSEC");               
                      }           
                      END_OF_FSEN:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSEN     
          // ---------------------------------------------------------------------------------------------------     
          void FSEC::Block( AgCreate create )     
          {         
                ///@addtogroup FSEC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      {  AgShape shape = AgShape("Division");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("ndiv")=6;              
                            shape.par("iaxis")=2;              
                            shape.par("c0")=30;              
                            /// Shape Division ndiv=6 iaxis=2 c0=30               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSEC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FSEC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSEC     
          // ---------------------------------------------------------------------------------------------------     
          void FIFR::Block( AgCreate create )     
          {         
                ///@addtogroup FIFR_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FMPT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftpg.rinnerms;              
                            shape.par("rmax")=ftpg.rinnerms+ftpg.drifr;              
                            shape.par("dz")=ftpg.dzifr/2;              
                            /// Shape Tube rmin=ftpg.rinnerms rmax=ftpg.rinnerms+ftpg.drifr dz=ftpg.dzifr/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FIFR;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FIFR:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FIFR     
          // ---------------------------------------------------------------------------------------------------     
          void FKWI::Block( AgCreate create )     
          {         
                ///@addtogroup FKWI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material MYLAR            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Mylar");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FMPT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftpg.rinnerms;              
                            shape.par("rmax")=ftpg.rgasout;              
                            shape.par("dz")=ftpg.dzkapton/2;              
                            /// Shape Tube rmin=ftpg.rinnerms rmax=ftpg.rgasout dz=ftpg.dzkapton/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FKWI;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FKWI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FKWI     
          // ---------------------------------------------------------------------------------------------------     
          void FFSL::Block( AgCreate create )     
          {         
                ///@addtogroup FFSL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material PYREX a=20.719 z=10.307 dens=2.23 absl=50.7 radl=12.6            
                      { AgMaterial &mat = AgMaterial::Get("Pyrex");              
                            mat.par("a")=20.719;              
                            mat.par("z")=10.307;              
                            mat.par("dens")=2.23;              
                            mat.par("absl")=50.7;              
                            mat.par("radl")=12.6;              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FFSL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=0;              
                            shape.par("rmax")=ffcc.stidia/2;              
                            shape.par("dz")=ffcc.stileng/2;              
                            /// Shape Tube rmin=0 rmax=ffcc.stidia/2 dz=ffcc.stileng/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FFSL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FFSL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FFSL     
          // ---------------------------------------------------------------------------------------------------     
          void FFCE::Block( AgCreate create )     
          {         
                ///@addtogroup FFCE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FFCE");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      temp1 = ftpg.rinnerms+ftpg.drinall1+ftpg.drinisol+                 ftpg.drinall2;           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=ffcc.barthik/2;              
                            shape.par("dy")=(ftpg.rgasout-temp1)/2;              
                            shape.par("dz")=ffcc.barwidt/2;              
                            /// Shape Bbox dx=ffcc.barthik/2 dy=(ftpg.rgasout-temp1)/2 dz=ffcc.barwidt/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FFCE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FFCE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FFCE     
          // ---------------------------------------------------------------------------------------------------     
          void FROS::Block( AgCreate create )     
          {         
                ///@addtogroup FROS_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FROS");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftpg.rinnerms;              
                            shape.par("rmax")=ftpg.routerms;              
                            shape.par("dz")=ftpg.rinnerms;              
                            /// Shape Tube rmin=ftpg.rinnerms rmax=ftpg.routerms dz=ftpg.rinnerms               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FROS;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FROM");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FROM              
                            Create("FROM");               
                      }           
                      _create = AgCreate("FREL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FREL              
                            Create("FREL");               
                      }           
                      _create = AgCreate("FRCC");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FRCC              
                            Create("FRCC");               
                      }           
                      _create = AgCreate("FRCE");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FRCE              
                            Create("FRCE");               
                      }           
                      _create = AgCreate("FROT");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FROT              
                            Create("FROT");               
                      }           
                      /// Loop on gg from frbd.phi2 to frbd.phi12 step=frbd.phi3           
                      for ( gg=frbd.phi2; (frbd.phi3>0)? (gg<=frbd.phi12):(gg>=frbd.phi12); gg+=frbd.phi3 )           
                      {              
                            { AgPlacement place = AgPlacement("FROT","FROS");                 
                                  /// Add daughter volume FROT to mother FROS                 
                                  place.TranslateX(-fssd.trapr*cos(degrad*gg));                 
                                  /// Translate x = -fssd.trapr*cos(degrad*gg)                 
                                  place.TranslateY(-fssd.trapr*sin(degrad*gg));                 
                                  /// Translate y = -fssd.trapr*sin(degrad*gg)                 
                                  place.AlphaZ(gg);                 
                                  /// Rotate: AlphaZ = gg                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  place.Ortho( "yzx" ); // ORT=yzx                 
                                  /// Axis substitution: XYZ --> yzx                 
                                  _stacker -> Position( AgBlock::Find("FROT"), place );                 
                            } // end placement of FROT              
                      }           
                      _create = AgCreate("FROP");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FROP              
                            Create("FROP");               
                      }           
                      /// Loop on hh from frbd.phi1 to frbd.phi11 step=frbd.phi3           
                      for ( hh=frbd.phi1; (frbd.phi3>0)? (hh<=frbd.phi11):(hh>=frbd.phi11); hh+=frbd.phi3 )           
                      {              
                            { AgPlacement place = AgPlacement("FROP","FROS");                 
                                  /// Add daughter volume FROP to mother FROS                 
                                  place.TranslateX(fssd.polyr*(sin(degrad*hh)));                 
                                  /// Translate x = fssd.polyr*(sin(degrad*hh))                 
                                  place.TranslateY(fssd.polyr*(cos(degrad*hh)));                 
                                  /// Translate y = fssd.polyr*(cos(degrad*hh))                 
                                  place.AlphaZ(hh);                 
                                  /// Rotate: AlphaZ = hh                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FROP"), place );                 
                            } // end placement of FROP              
                      }           
                      /// Loop on ww from frbd.phi2 to frbd.phi12 step=frbd.phi3           
                      for ( ww=frbd.phi2; (frbd.phi3>0)? (ww<=frbd.phi12):(ww>=frbd.phi12); ww+=frbd.phi3 )           
                      {              
                            { AgPlacement place = AgPlacement("FROM","FROS");                 
                                  /// Add daughter volume FROM to mother FROS                 
                                  place.TranslateX(-29.42*sin(degrad*ww));                 
                                  /// Translate x = -29.42*sin(degrad*ww)                 
                                  place.TranslateY(ftpg.rrom*cos(degrad*ww));                 
                                  /// Translate y = ftpg.rrom*cos(degrad*ww)                 
                                  place.par("only")=AgPlacement::kMany;                 
                                  /// Overlap: agplacement::kmany                 
                                  place.AlphaZ(ww+180);                 
                                  /// Rotate: AlphaZ = ww+180                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FROM"), place );                 
                            } // end placement of FROM              
                            { AgPlacement place = AgPlacement("FREL","FROS");                 
                                  /// Add daughter volume FREL to mother FROS                 
                                  place.TranslateX(-(ftpg.relcard+frbd.electrdy)*sin(degrad*ww));                 
                                  /// Translate x = -(ftpg.relcard+frbd.electrdy)*sin(degrad*ww)                 
                                  place.TranslateY((ftpg.relcard+frbd.electrdy)*cos(degrad*ww));                 
                                  /// Translate y = (ftpg.relcard+frbd.electrdy)*cos(degrad*ww)                 
                                  place.AlphaZ(ww);                 
                                  /// Rotate: AlphaZ = ww                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FREL"), place );                 
                            } // end placement of FREL              
                            { AgPlacement place = AgPlacement("FRCC","FROS");                 
                                  /// Add daughter volume FRCC to mother FROS                 
                                  place.TranslateX(-ftpg.rcooplm*sin(degrad*ww));                 
                                  /// Translate x = -ftpg.rcooplm*sin(degrad*ww)                 
                                  place.TranslateY(ftpg.rcooplm*cos(degrad*ww));                 
                                  /// Translate y = ftpg.rcooplm*cos(degrad*ww)                 
                                  place.AlphaZ(ww);                 
                                  /// Rotate: AlphaZ = ww                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FRCC"), place );                 
                            } // end placement of FRCC              
                            { AgPlacement place = AgPlacement("FRCE","FROS");                 
                                  /// Add daughter volume FRCE to mother FROS                 
                                  place.TranslateX(-ftpg.rcoople*sin(degrad*(ww+30-6.5)));                 
                                  /// Translate x = -ftpg.rcoople*sin(degrad*(ww+30-6.5))                 
                                  place.TranslateY(ftpg.rcoople*cos(degrad*(ww+30-6.5)));                 
                                  /// Translate y = ftpg.rcoople*cos(degrad*(ww+30-6.5))                 
                                  place.AlphaZ((ww+22));                 
                                  /// Rotate: AlphaZ = (ww+22)                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FRCE"), place );                 
                            } // end placement of FRCE              
                            { AgPlacement place = AgPlacement("FRCE","FROS");                 
                                  /// Add daughter volume FRCE to mother FROS                 
                                  place.TranslateX(-ftpg.rcoople*sin(degrad*(ww-30+6.5)));                 
                                  /// Translate x = -ftpg.rcoople*sin(degrad*(ww-30+6.5))                 
                                  place.TranslateY(ftpg.rcoople*cos(degrad*(ww-30+6.5)));                 
                                  /// Translate y = ftpg.rcoople*cos(degrad*(ww-30+6.5))                 
                                  place.AlphaZ((ww-22));                 
                                  /// Rotate: AlphaZ = (ww-22)                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FRCE"), place );                 
                            } // end placement of FRCE              
                      }           
                      END_OF_FROS:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FROS     
          // ---------------------------------------------------------------------------------------------------     
          void FROM::Block( AgCreate create )     
          {         
                ///@addtogroup FROM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FROM");              
                            attr.par("seen")=1;              
                            attr.par("colo")=6;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=frbd.xrom/2.;              
                            shape.par("dy")=frbd.yrom/2.;              
                            shape.par("dz")=frbd.zrom/2.;              
                            /// Shape Bbox dx=frbd.xrom/2. dy=frbd.yrom/2. dz=frbd.zrom/2.               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FROM;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FROE");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FROE              
                            Create("FROE");               
                      }           
                      { AgPlacement place = AgPlacement("FROE","FROM");              
                            /// Add daughter volume FROE to mother FROM              
                            place.TranslateX(frbd.xehol);              
                            /// Translate x = frbd.xehol              
                            place.TranslateY(frbd.yehol);              
                            /// Translate y = frbd.yehol              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("FROE"), place );              
                      } // end placement of FROE           
                      _create = AgCreate("FROE");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FROE              
                            Create("FROE");               
                      }           
                      { AgPlacement place = AgPlacement("FROE","FROM");              
                            /// Add daughter volume FROE to mother FROM              
                            place.TranslateX(-(frbd.xehol));              
                            /// Translate x = -(frbd.xehol)              
                            place.TranslateY(frbd.yehol);              
                            /// Translate y = frbd.yehol              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("FROE"), place );              
                      } // end placement of FROE           
                      _create = AgCreate("FROL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FROL              
                            Create("FROL");               
                      }           
                      { AgPlacement place = AgPlacement("FROL","FROM");              
                            /// Add daughter volume FROL to mother FROM              
                            place.TranslateY(frbd.ylhol);              
                            /// Translate y = frbd.ylhol              
                            place.TranslateZ(frbd.xlhol);              
                            /// Translate z = frbd.xlhol              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("FROL"), place );              
                      } // end placement of FROL           
                      _create = AgCreate("FROL");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FROL              
                            Create("FROL");               
                      }           
                      { AgPlacement place = AgPlacement("FROL","FROM");              
                            /// Add daughter volume FROL to mother FROM              
                            place.TranslateY(frbd.ylhol);              
                            /// Translate y = frbd.ylhol              
                            place.TranslateZ(-(frbd.xlhol));              
                            /// Translate z = -(frbd.xlhol)              
                            place.par("only")=AgPlacement::kMany;              
                            /// Overlap: agplacement::kmany              
                            _stacker -> Position( AgBlock::Find("FROL"), place );              
                      } // end placement of FROL           
                      frob_x1 = -(frbd.boffset);           
                      /// Loop on jj from 1 to 5 step=1           
                      for ( jj=1; (1>0)? (jj<=5):(jj>=5); jj+=1 )           
                      {              
                            _create = AgCreate("FROB");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FROB                 
                                  Create("FROB");                  
                            }              
                            { AgPlacement place = AgPlacement("FROB","FROM");                 
                                  /// Add daughter volume FROB to mother FROM                 
                                  place.TranslateX(frob_x1);                 
                                  /// Translate x = frob_x1                 
                                  place.TranslateZ(frbd.zoffb);                 
                                  /// Translate z = frbd.zoffb                 
                                  place.par("only")=AgPlacement::kMany;                 
                                  /// Overlap: agplacement::kmany                 
                                  place.AlphaY(frbd.phi1);                 
                                  /// Rotate: AlphaY = frbd.phi1                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FROB"), place );                 
                            } // end placement of FROB              
                            _create = AgCreate("FROB");              
                            {                 
                                  AgShape myshape; // undefined shape                 
                                  ///Create FROB                 
                                  Create("FROB");                  
                            }              
                            { AgPlacement place = AgPlacement("FROB","FROM");                 
                                  /// Add daughter volume FROB to mother FROM                 
                                  place.TranslateX(frob_x1);                 
                                  /// Translate x = frob_x1                 
                                  place.TranslateZ(-(frbd.zoffb));                 
                                  /// Translate z = -(frbd.zoffb)                 
                                  place.par("only")=AgPlacement::kMany;                 
                                  /// Overlap: agplacement::kmany                 
                                  place.AlphaY(frbd.phi1);                 
                                  /// Rotate: AlphaY = frbd.phi1                 
                                  /// G3 Reference: thetax = 90                 
                                  /// G3 Reference: phix = 0                 
                                  /// G3 Reference: thetay = 90                 
                                  /// G3 Reference: phiy = 90                 
                                  /// G3 Reference: thetaz = 0                 
                                  /// G3 Reference: phiz = 0                 
                                  _stacker -> Position( AgBlock::Find("FROB"), place );                 
                            } // end placement of FROB              
                            frob_x1 = frob_x1 + 5.96;              
                      }           
                      END_OF_FROM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FROM     
          // ---------------------------------------------------------------------------------------------------     
          void FROB::Block( AgCreate create )     
          {         
                ///@addtogroup FROB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FROB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=frbd.boxhx;              
                            shape.par("dy")=frbd.boxhy;              
                            shape.par("dz")=frbd.boxhz;              
                            /// Shape Bbox dx=frbd.boxhx dy=frbd.boxhy dz=frbd.boxhz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FROB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FROB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FROB     
          // ---------------------------------------------------------------------------------------------------     
          void FROE::Block( AgCreate create )     
          {         
                ///@addtogroup FROE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FROE");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=frbd.eboxhx;              
                            shape.par("dy")=frbd.eboxhy;              
                            shape.par("dz")=frbd.eboxhz;              
                            /// Shape Bbox dx=frbd.eboxhx dy=frbd.eboxhy dz=frbd.eboxhz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FROE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FROE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FROE     
          // ---------------------------------------------------------------------------------------------------     
          void FROL::Block( AgCreate create )     
          {         
                ///@addtogroup FROL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FROL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=frbd.lboxhx;              
                            shape.par("dy")=frbd.lboxhy;              
                            shape.par("dz")=frbd.lboxhz;              
                            /// Shape Bbox dx=frbd.lboxhx dy=frbd.lboxhy dz=frbd.lboxhz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FROL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FROL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FROL     
          // ---------------------------------------------------------------------------------------------------     
          void FROP::Block( AgCreate create )     
          {         
                ///@addtogroup FROP_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FROP");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pgon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=frbd.phi2;              
                            shape.par("dphi")=frbd.phi13;              
                            shape.par("npdiv")=3;              
                            shape.par("nz")=2;              
                            shape.Z(0)=-(fssd.polydz);              
                            shape.Z(1)=fssd.polydz;              
                            shape.Rmin(0)=fssd.polyir;              
                            shape.Rmin(1)=fssd.polyir;              
                            shape.Rmax(0)=fssd.polyor;              
                            shape.Rmax(1)=fssd.polyor;              
                            /// Shape Pgon phi1=frbd.phi2 dphi=frbd.phi13 npdiv=3 nz=2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FROP;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FROP:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FROP     
          // ---------------------------------------------------------------------------------------------------     
          void FROT::Block( AgCreate create )     
          {         
                ///@addtogroup FROT_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FROT");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Trd1");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx1")=fssd.trapx1;              
                            shape.par("dx2")=fssd.trapx2;              
                            shape.par("dy")=fssd.trapdy;              
                            shape.par("dz")=fssd.trapdz;              
                            /// Shape Trd1 dx1=fssd.trapx1 dx2=fssd.trapx2 dy=fssd.trapdy dz=fssd.trapdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FROT;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FROT:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FROT     
          // ---------------------------------------------------------------------------------------------------     
          void FREL::Block( AgCreate create )     
          {         
                ///@addtogroup FREL_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Component Si	a=28.08	z=14	w=0.6*1*28./60.           
                      /// Component O	a=16	z=8	w=0.6*2*16./60. + 0.4*4*16./174.           
                      /// Component C	a=12	z=6	w=0.4*8*12./174.           
                      /// Component H	a=1	z=1	w=0.4*14*1./174.           
                      /// Mixture G10 dens=1.7           
                      {  AgMaterial &mix = AgMaterial::Get("G10");              
                            mix.Component("Si",28.08,14,0.6*1*28./60.);              
                            mix.Component("O",16,8,0.6*2*16./60. + 0.4*4*16./174.);              
                            mix.Component("C",12,6,0.4*8*12./174.);              
                            mix.Component("H",1,1,0.4*14*1./174.);              
                            mix.par("dens")=1.7;              
                            mix.lock();              
                            _material = mix;              
                            _material.lock();              
                      }           
                      /// Material G10            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("G10");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FREL");              
                            attr.par("seen")=1;              
                            attr.par("colo")=3;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=frbd.electrdx;              
                            shape.par("dy")=frbd.electrdy;              
                            shape.par("dz")=frbd.electrdz;              
                            /// Shape Bbox dx=frbd.electrdx dy=frbd.electrdy dz=frbd.electrdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FREL;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FREL:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FREL     
          // ---------------------------------------------------------------------------------------------------     
          void FRCC::Block( AgCreate create )     
          {         
                ///@addtogroup FRCC_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Copper            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Copper");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FRCC");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=frbd.coolpldx;              
                            shape.par("dy")=frbd.coolpldy;              
                            shape.par("dz")=frbd.coolpldz;              
                            /// Shape Bbox dx=frbd.coolpldx dy=frbd.coolpldy dz=frbd.coolpldz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FRCC;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FRCC:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FRCC     
          // ---------------------------------------------------------------------------------------------------     
          void FRCE::Block( AgCreate create )     
          {         
                ///@addtogroup FRCE_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Copper            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Copper");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FRCE");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=frbd.eclpldx;              
                            shape.par("dy")=frbd.eclpldy;              
                            shape.par("dz")=frbd.eclpldz;              
                            /// Shape Bbox dx=frbd.eclpldx dy=frbd.eclpldy dz=frbd.eclpldz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FRCE;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FRCE:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FRCE     
          // ---------------------------------------------------------------------------------------------------     
          void FSER::Block( AgCreate create )     
          {         
                ///@addtogroup FSER_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FSER");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=fssd.eringrmn;              
                            shape.par("rmax")=fssd.eringrmx;              
                            shape.par("dz")=fssd.eringdz;              
                            /// Shape Tube rmin=fssd.eringrmn rmax=fssd.eringrmx dz=fssd.eringdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSER;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FSRA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSRA              
                            Create("FSRA");               
                      }           
                      { AgPlacement place = AgPlacement("FSRA","FSER");              
                            /// Add daughter volume FSRA to mother FSER              
                            place.TranslateZ(fssd.erposz);              
                            /// Translate z = fssd.erposz              
                            _stacker -> Position( AgBlock::Find("FSRA"), place );              
                      } // end placement of FSRA           
                      _create = AgCreate("FSRB");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSRB              
                            Create("FSRB");               
                      }           
                      { AgPlacement place = AgPlacement("FSRB","FSER");              
                            /// Add daughter volume FSRB to mother FSER              
                            _stacker -> Position( AgBlock::Find("FSRB"), place );              
                      } // end placement of FSRB           
                      _create = AgCreate("FSPG");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSPG              
                            Create("FSPG");               
                      }           
                      { AgPlacement place = AgPlacement("FSPG","FSER");              
                            /// Add daughter volume FSPG to mother FSER              
                            place.TranslateZ(-(fssd.erposz));              
                            /// Translate z = -(fssd.erposz)              
                            _stacker -> Position( AgBlock::Find("FSPG"), place );              
                      } // end placement of FSPG           
                      END_OF_FSER:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSER     
          // ---------------------------------------------------------------------------------------------------     
          void FSRA::Block( AgCreate create )     
          {         
                ///@addtogroup FSRA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FSRA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=fssd.eringrmn;              
                            shape.par("rmax")=fssd.eringrmx;              
                            shape.par("dz")=fssd.oeringdz;              
                            /// Shape Tube rmin=fssd.eringrmn rmax=fssd.eringrmx dz=fssd.oeringdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSRA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FSRA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSRA     
          // ---------------------------------------------------------------------------------------------------     
          void FSRB::Block( AgCreate create )     
          {         
                ///@addtogroup FSRB_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FSRB");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=fssd.eringrmn;              
                            shape.par("rmax")=fssd.meringrm;              
                            shape.par("dz")=fssd.meringdz;              
                            /// Shape Tube rmin=fssd.eringrmn rmax=fssd.meringrm dz=fssd.meringdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSRB;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FSRB:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSRB     
          // ---------------------------------------------------------------------------------------------------     
          void FSSM::Block( AgCreate create )     
          {         
                ///@addtogroup FSSM_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FSSM");              
                            attr.par("seen")=0;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftpg.rinnerms;              
                            shape.par("rmax")=ftpg.routerms;              
                            shape.par("dz")=ftpg.msrdz;              
                            /// Shape Tube rmin=ftpg.rinnerms rmax=ftpg.routerms dz=ftpg.msrdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSSM;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FSPG");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSPG              
                            Create("FSPG");               
                      }           
                      { AgPlacement place = AgPlacement("FSPG","FSSM");              
                            /// Add daughter volume FSPG to mother FSSM              
                            place.TranslateZ(fssd.pgonpdz);              
                            /// Translate z = fssd.pgonpdz              
                            _stacker -> Position( AgBlock::Find("FSPG"), place );              
                      } // end placement of FSPG           
                      _create = AgCreate("FSRI");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSRI              
                            Create("FSRI");               
                      }           
                      { AgPlacement place = AgPlacement("FSRI","FSSM");              
                            /// Add daughter volume FSRI to mother FSSM              
                            _stacker -> Position( AgBlock::Find("FSRI"), place );              
                      } // end placement of FSRI           
                      _create = AgCreate("FSPG");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSPG              
                            Create("FSPG");               
                      }           
                      { AgPlacement place = AgPlacement("FSPG","FSSM");              
                            /// Add daughter volume FSPG to mother FSSM              
                            place.TranslateZ(-(fssd.pgonpdz));              
                            /// Translate z = -(fssd.pgonpdz)              
                            _stacker -> Position( AgBlock::Find("FSPG"), place );              
                      } // end placement of FSPG           
                      _create = AgCreate("FSBA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSBA              
                            Create("FSBA");               
                      }           
                      { AgPlacement place = AgPlacement("FSBA","FSSM");              
                            /// Add daughter volume FSBA to mother FSSM              
                            place.TranslateY(fssd.sbsdy);              
                            /// Translate y = fssd.sbsdy              
                            place.AlphaZ(frbd.phi1);              
                            /// Rotate: AlphaZ = frbd.phi1              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FSBA"), place );              
                      } // end placement of FSBA           
                      _create = AgCreate("FSBA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSBA              
                            Create("FSBA");               
                      }           
                      { AgPlacement place = AgPlacement("FSBA","FSSM");              
                            /// Add daughter volume FSBA to mother FSSM              
                            place.TranslateY(-(fssd.sbsdy));              
                            /// Translate y = -(fssd.sbsdy)              
                            place.AlphaZ(frbd.phi1);              
                            /// Rotate: AlphaZ = frbd.phi1              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FSBA"), place );              
                      } // end placement of FSBA           
                      _create = AgCreate("FSBA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSBA              
                            Create("FSBA");               
                      }           
                      { AgPlacement place = AgPlacement("FSBA","FSSM");              
                            /// Add daughter volume FSBA to mother FSSM              
                            place.TranslateX(fssd.sbsdy*cos(frbd.phi2*degrad));              
                            /// Translate x = fssd.sbsdy*cos(frbd.phi2*degrad)              
                            place.TranslateY(fssd.sbsdy*sin(frbd.phi2*degrad));              
                            /// Translate y = fssd.sbsdy*sin(frbd.phi2*degrad)              
                            place.AlphaZ(frbd.phi11);              
                            /// Rotate: AlphaZ = frbd.phi11              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FSBA"), place );              
                      } // end placement of FSBA           
                      _create = AgCreate("FSBA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSBA              
                            Create("FSBA");               
                      }           
                      { AgPlacement place = AgPlacement("FSBA","FSSM");              
                            /// Add daughter volume FSBA to mother FSSM              
                            place.TranslateX(-(fssd.sbsdy*cos(frbd.phi2*degrad)));              
                            /// Translate x = -(fssd.sbsdy*cos(frbd.phi2*degrad))              
                            place.TranslateY(fssd.sbsdy*sin(frbd.phi2*degrad));              
                            /// Translate y = fssd.sbsdy*sin(frbd.phi2*degrad)              
                            place.AlphaZ(frbd.phi3);              
                            /// Rotate: AlphaZ = frbd.phi3              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FSBA"), place );              
                      } // end placement of FSBA           
                      _create = AgCreate("FSBA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSBA              
                            Create("FSBA");               
                      }           
                      { AgPlacement place = AgPlacement("FSBA","FSSM");              
                            /// Add daughter volume FSBA to mother FSSM              
                            place.TranslateX(-(fssd.sbsdy*cos(frbd.phi2*degrad)));              
                            /// Translate x = -(fssd.sbsdy*cos(frbd.phi2*degrad))              
                            place.TranslateY(-(fssd.sbsdy*sin(frbd.phi2*degrad)));              
                            /// Translate y = -(fssd.sbsdy*sin(frbd.phi2*degrad))              
                            place.AlphaZ(frbd.phi5);              
                            /// Rotate: AlphaZ = frbd.phi5              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FSBA"), place );              
                      } // end placement of FSBA           
                      _create = AgCreate("FSBA");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSBA              
                            Create("FSBA");               
                      }           
                      { AgPlacement place = AgPlacement("FSBA","FSSM");              
                            /// Add daughter volume FSBA to mother FSSM              
                            place.TranslateX((fssd.sbsdy*cos(frbd.phi2*degrad)));              
                            /// Translate x = (fssd.sbsdy*cos(frbd.phi2*degrad))              
                            place.TranslateY(-(fssd.sbsdy*sin(frbd.phi2*degrad)));              
                            /// Translate y = -(fssd.sbsdy*sin(frbd.phi2*degrad))              
                            place.AlphaZ(frbd.phi9);              
                            /// Rotate: AlphaZ = frbd.phi9              
                            /// G3 Reference: thetax = 90              
                            /// G3 Reference: phix = 0              
                            /// G3 Reference: thetay = 90              
                            /// G3 Reference: phiy = 90              
                            /// G3 Reference: thetaz = 0              
                            /// G3 Reference: phiz = 0              
                            _stacker -> Position( AgBlock::Find("FSBA"), place );              
                      } // end placement of FSBA           
                      END_OF_FSSM:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSSM     
          // ---------------------------------------------------------------------------------------------------     
          void FSPG::Block( AgCreate create )     
          {         
                ///@addtogroup FSPG_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FSPG");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Pgon");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("phi1")=frbd.phi4;              
                            shape.par("dphi")=frbd.phi13;              
                            shape.par("npdiv")=6;              
                            shape.par("nz")=2;              
                            shape.Z(0)=-0.5;              
                            shape.Z(1)=0.5;              
                            shape.Rmin(0)=fssd.polyir;              
                            shape.Rmin(1)=fssd.polyir;              
                            shape.Rmax(0)=fssd.erpolyrm;              
                            shape.Rmax(1)=fssd.erpolyrm;              
                            /// Shape Pgon phi1=frbd.phi4 dphi=frbd.phi13 npdiv=6 nz=2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSPG;              
                            _stacker -> Build(this);              
                      }           
                      _create = AgCreate("FSPI");           
                      {              
                            AgShape myshape; // undefined shape              
                            ///Create FSPI              
                            Create("FSPI");               
                      }           
                      { AgPlacement place = AgPlacement("FSPI","FSPG");              
                            /// Add daughter volume FSPI to mother FSPG              
                            _stacker -> Position( AgBlock::Find("FSPI"), place );              
                      } // end placement of FSPI           
                      END_OF_FSPG:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSPG     
          // ---------------------------------------------------------------------------------------------------     
          void FSPI::Block( AgCreate create )     
          {         
                ///@addtogroup FSPI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Air            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Air");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FSPI");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftpg.rinnerms;              
                            shape.par("rmax")=ftpg.rgasout;              
                            shape.par("dz")=ftpg.serhole;              
                            /// Shape Tube rmin=ftpg.rinnerms rmax=ftpg.rgasout dz=ftpg.serhole               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSPI;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FSPI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSPI     
          // ---------------------------------------------------------------------------------------------------     
          void FSRI::Block( AgCreate create )     
          {         
                ///@addtogroup FSRI_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FSRI");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftpg.rgasout;              
                            shape.par("rmax")=ftpg.risring;              
                            shape.par("dz")=ftpg.isringdz;              
                            /// Shape Tube rmin=ftpg.rgasout rmax=ftpg.risring dz=ftpg.isringdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSRI;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FSRI:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSRI     
          // ---------------------------------------------------------------------------------------------------     
          void FSBA::Block( AgCreate create )     
          {         
                ///@addtogroup FSBA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FSBA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=1;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Bbox");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("dx")=ftpg.sbsrdx;              
                            shape.par("dy")=ftpg.sbsrdy;              
                            shape.par("dz")=ftpg.sbsrdz;              
                            /// Shape Bbox dx=ftpg.sbsrdx dy=ftpg.sbsrdy dz=ftpg.sbsrdz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FSBA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FSBA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FSBA     
          // ---------------------------------------------------------------------------------------------------     
          void FPAD::Block( AgCreate create )     
          {         
                ///@addtogroup FPAD_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material MYLAR            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Mylar");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FPAD");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=ftpg.rgasout-0.25;              
                            shape.par("rmax")=ftpg.rgasout;              
                            shape.par("dz")=ftpg.gasvoldz;              
                            /// Shape Tube rmin=ftpg.rgasout-0.25 rmax=ftpg.rgasout dz=ftpg.gasvoldz               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FPAD;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FPAD:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FPAD     
          // ---------------------------------------------------------------------------------------------------     
          void FFRA::Block( AgCreate create )     
          {         
                ///@addtogroup FFRA_doc        
                ///@{           
                        AgBlock *_save = mCurrent;           
                        mCurrent = this;           
                        Bool_t _same_shape = true;           
                      /// Material Aluminium            
                      {  AgMaterial mat = AgMaterial::CopyMaterial("Aluminium");              
                            _material = mat;              
                      }           
                      { AgAttribute attr = AgAttribute("FFRA");              
                            attr.par("seen")=1;              
                            attr.par("colo")=2;              
                            attr.Inherit( AgBlock::previous() );               
                            _attribute = attr;              
                      }           
                      temp1 = (ftpg.rgasout-(ftpg.rinnerms+ftpg.drinall1+                 ftpg.drinisol+ftpg.drinall2))/18;           
                      temp2 = ftpg.rinnerms+ftpg.drinall1+ftpg.drinisol+                 ftpg.drinall2;           
                      {  AgShape shape = AgShape("Tube");              
                            shape     .Inherit( AgBlock::previous() );              
                            create     .SetParameters(shape);              
                            shape.par("rmin")=temp2+iring*temp1-ffcc.ridr/2;              
                            shape.par("rmax")=temp2+iring*temp1+ffcc.ridr/2;              
                            shape.par("dz")=ffcc.rithick/2;              
                            /// Shape Tube rmin=temp2+iring*temp1-ffcc.ridr/2 rmax=temp2+iring*temp1+ffcc.ridr/2 dz=ffcc.rithick/2               
                            _same_shape &= _stacker->SearchVolume( shape, _attribute );              
                            _shape = shape;              
                            if (_same_shape) goto END_OF_FFRA;              
                            _stacker -> Build(this);              
                      }           
                      END_OF_FFRA:           
                      mCurrent = _save;           
                ///@}        
          } // End Block FFRA     
    // ----------------------------------------------------------------------- geoctr
       void FtpcGeo::ConstructGeometry( const Char_t *dummy )     
       {        
             ///@addtogroup FtpcGeo_revision        
             ///@{           
                   /// Author: Andreas Schuettauf           
             ///@}        
             ///@addtogroup FtpcGeo_revision        
             ///@{           
                   /// Created:   03-Apr-98            
             ///@}        
             AddBlock("FTPC");        
             AddBlock("FIAL");        
             AddBlock("FMPT");        
             AddBlock("FOAL");        
             AddBlock("FGAS");        
             AddBlock("FSEN");        
             AddBlock("FSEC");        
             AddBlock("FIFR");        
             AddBlock("FKWI");        
             AddBlock("FFSL");        
             AddBlock("FFCE");        
             AddBlock("FROS");        
             AddBlock("FROM");        
             AddBlock("FROB");        
             AddBlock("FROE");        
             AddBlock("FROL");        
             AddBlock("FROP");        
             AddBlock("FROT");        
             AddBlock("FREL");        
             AddBlock("FRCC");        
             AddBlock("FRCE");        
             AddBlock("FSER");        
             AddBlock("FSRA");        
             AddBlock("FSRB");        
             AddBlock("FSPG");        
             AddBlock("FSPI");        
             AddBlock("FSSM");        
             AddBlock("FSRI");        
             AddBlock("FSBA");        
             AddBlock("FPAD");        
             AddBlock("FFRA");        
             iflaga.at(0) = 0;        
             iflaga.at(1) = 1;        
             iflaga.at(2) = 1;        
             iflaga.at(3) = 2;        
             iflaga.at(4) = 2;        
             iflagb.at(0) = 0;        
             iflagb.at(1) = 0;        
             iflagb.at(2) = 1;        
             iflagb.at(3) = 1;        
             iflagb.at(4) = 2;        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup ftpg_doc        
             ///@{           
                   ++ftpg._index;           
                   ftpg . version = 1; //  geometry Version            
                   /// ftpg . version = 1; //  geometry Version            
                   ftpg . rinnerms = 7.55; //  innermost radius of envelope            
                   /// ftpg . rinnerms = 7.55; //  innermost radius of envelope            
                   ftpg . routerms = 36.4; //  outermost radius of envelope            
                   /// ftpg . routerms = 36.4; //  outermost radius of envelope            
                   ftpg . rgasout = 30.6; //  outer radius of the gas-volume            
                   /// ftpg . rgasout = 30.6; //  outer radius of the gas-volume            
                   ftpg . rrom = 29.42; //  outer radius for one readout module in a ring            
                   /// ftpg . rrom = 29.42; //  outer radius for one readout module in a ring            
                   ftpg . relcard = 32.4; //  outer radius for the electronic card              
                   /// ftpg . relcard = 32.4; //  outer radius for the electronic card              
                   ftpg . rcooplm = 33.7; //  outer radius for the cooling plate middle            
                   /// ftpg . rcooplm = 33.7; //  outer radius for the cooling plate middle            
                   ftpg . rcoople = 36.0; //  outer radius for the cooling plate ends            
                   /// ftpg . rcoople = 36.0; //  outer radius for the cooling plate ends            
                   ftpg . zstart = 150; //  distance from the interaction point            
                   /// ftpg . zstart = 150; //  distance from the interaction point            
                   ftpg . totlen = 119; //  overall length            
                   /// ftpg . totlen = 119; //  overall length            
                   ftpg . laylen = 2.0; //  thickness of the sensitive Layer            
                   /// ftpg . laylen = 2.0; //  thickness of the sensitive Layer            
                   ftpg . hitlay = 10; //  # of padrows in one FTPC : 10            
                   /// ftpg . hitlay = 10; //  # of padrows in one FTPC : 10            
                   ftpg . drinall1 = 0.05; //  thickness of inner Al-Layer of inner Tube            
                   /// ftpg . drinall1 = 0.05; //  thickness of inner Al-Layer of inner Tube            
                   ftpg . drinall2 = 0.05; //  thickness of outer Al-Layer of inner Tube            
                   /// ftpg . drinall2 = 0.05; //  thickness of outer Al-Layer of inner Tube            
                   ftpg . drinisol = 0.4; //  thickness of plastic insulation of inner tube            
                   /// ftpg . drinisol = 0.4; //  thickness of plastic insulation of inner tube            
                   ftpg . dzkapton = 0.02; //  thickness of a double kapton-windows            
                   /// ftpg . dzkapton = 0.02; //  thickness of a double kapton-windows            
                   ftpg . drifr = 1.15; //  thickness (r) of inner flange ring            
                   /// ftpg . drifr = 1.15; //  thickness (r) of inner flange ring            
                   ftpg . dzifr = 0.4; //  thickness (z) of inner flange ring            
                   /// ftpg . dzifr = 0.4; //  thickness (z) of inner flange ring            
                   ftpg . dzer = 10.35; //  thickness (z) of Endring            
                   /// ftpg . dzer = 10.35; //  thickness (z) of Endring            
                   ftpg . dzrom = 21.3; //  Distance of one Readout module Ring to coor.             
                   /// ftpg . dzrom = 21.3; //  Distance of one Readout module Ring to coor.             
                   ftpg . dzsura = 10.65; //  Distance to inner(a) Support Ring from coor.             
                   /// ftpg . dzsura = 10.65; //  Distance to inner(a) Support Ring from coor.             
                   ftpg . dzsurb = 31.95; //  Distance to outer(b) Support Ring from coor.            
                   /// ftpg . dzsurb = 31.95; //  Distance to outer(b) Support Ring from coor.            
                   ftpg . dzsmpr = 8.5; //  Distance Small between to Pad Rows            
                   /// ftpg . dzsmpr = 8.5; //  Distance Small between to Pad Rows            
                   ftpg . dzbipr = 12.80; //  Distance Big between to Pad Rows              
                   /// ftpg . dzbipr = 12.80; //  Distance Big between to Pad Rows              
                   ftpg . msrdz = 4.1; //  half length of main support ring            
                   /// ftpg . msrdz = 4.1; //  half length of main support ring            
                   ftpg . serhole = 0.5; //  half Support End Ring Hole length Z            
                   /// ftpg . serhole = 0.5; //  half Support End Ring Hole length Z            
                   ftpg . risring = 30.8; //  outer Radius for innner Support Ring            
                   /// ftpg . risring = 30.8; //  outer Radius for innner Support Ring            
                   ftpg . isringdz = 3.1; //  Half length for inner Support Ring            
                   /// ftpg . isringdz = 3.1; //  Half length for inner Support Ring            
                   ftpg . sbsrdx = 0.7; //  Half width of Stabil. Block for Supp.Ring             
                   /// ftpg . sbsrdx = 0.7; //  Half width of Stabil. Block for Supp.Ring             
                   ftpg . sbsrdy = 2.8; //  Half thick. of Stabil. Block for Supp.Ring             
                   /// ftpg . sbsrdy = 2.8; //  Half thick. of Stabil. Block for Supp.Ring             
                   ftpg . sbsrdz = 3.1; //  Half length of Stabil. Block for Supp.Ring            
                   /// ftpg . sbsrdz = 3.1; //  Half length of Stabil. Block for Supp.Ring            
                   ftpg . gasvoldz = 59.5; //  Half length of active volume            
                   /// ftpg . gasvoldz = 59.5; //  Half length of active volume            
                   //           
                   ftpg.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup ffcc_doc        
             ///@{           
                   ++ffcc._index;           
                   ffcc . version = 1; //  geometry Version            
                   /// ffcc . version = 1; //  geometry Version            
                   ffcc . stileng = 21; //  lenth of the ceramic-holders            
                   /// ffcc . stileng = 21; //  lenth of the ceramic-holders            
                   ffcc . stidia = .8; //  diameter of ceramic-holders            
                   /// ffcc . stidia = .8; //  diameter of ceramic-holders            
                   ffcc . stirpos = 19.25; //  r-pos. of ceramic-holders            
                   /// ffcc . stirpos = 19.25; //  r-pos. of ceramic-holders            
                   ffcc . rithick = .06; //  thickness of the FC rings            
                   /// ffcc . rithick = .06; //  thickness of the FC rings            
                   ffcc . ridr = 1; //  width (r) if the FC rings            
                   /// ffcc . ridr = 1; //  width (r) if the FC rings            
                   ffcc . rigap = .3; //  gap between two FC rings            
                   /// ffcc . rigap = .3; //  gap between two FC rings            
                   ffcc . barleng = 29; //  length of stabilizer bar for FC             
                   /// ffcc . barleng = 29; //  length of stabilizer bar for FC             
                   ffcc . barwidt = 2; //  width of stabilizer bar for FC            
                   /// ffcc . barwidt = 2; //  width of stabilizer bar for FC            
                   ffcc . barthik = .8; //  thickness of stabilizer bar for FC              
                   /// ffcc . barthik = .8; //  thickness of stabilizer bar for FC              
                   //           
                   ffcc.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup frbd_doc        
             ///@{           
                   ++frbd._index;           
                   frbd . version = 1; //  geometry Version            
                   /// frbd . version = 1; //  geometry Version            
                   frbd . phi1 = 0; //  phi 1            
                   /// frbd . phi1 = 0; //  phi 1            
                   frbd . phi2 = 30; //  phi 2            
                   /// frbd . phi2 = 30; //  phi 2            
                   frbd . phi3 = 60; //  phi 3            
                   /// frbd . phi3 = 60; //  phi 3            
                   frbd . phi4 = 90; //  phi 4              
                   /// frbd . phi4 = 90; //  phi 4              
                   frbd . phi5 = 120; //  phi 5            
                   /// frbd . phi5 = 120; //  phi 5            
                   frbd . phi6 = 150; //  phi 6            
                   /// frbd . phi6 = 150; //  phi 6            
                   frbd . phi7 = 180; //  phi 7            
                   /// frbd . phi7 = 180; //  phi 7            
                   frbd . phi8 = 210; //  phi 8            
                   /// frbd . phi8 = 210; //  phi 8            
                   frbd . phi9 = 240; //  phi 9            
                   /// frbd . phi9 = 240; //  phi 9            
                   frbd . phi10 = 270; //  phi 10            
                   /// frbd . phi10 = 270; //  phi 10            
                   frbd . phi11 = 300; //  phi 11            
                   /// frbd . phi11 = 300; //  phi 11            
                   frbd . phi12 = 330; //  phi 12            
                   /// frbd . phi12 = 330; //  phi 12            
                   frbd . phi13 = 360; //  phi 13             
                   /// frbd . phi13 = 360; //  phi 13             
                   frbd . xrom = 32.6; //  X lenght of one Readout Module             
                   /// frbd . xrom = 32.6; //  X lenght of one Readout Module             
                   frbd . yrom = 5.77; //  Y lenght of one Readout Module             
                   /// frbd . yrom = 5.77; //  Y lenght of one Readout Module             
                   frbd . zrom = 15.1; //  Z lenght of one Readout Module             
                   /// frbd . zrom = 15.1; //  Z lenght of one Readout Module             
                   frbd . rahol = 29.508; //  Radius for the circle cut in RoM            
                   /// frbd . rahol = 29.508; //  Radius for the circle cut in RoM            
                   frbd . xehol = 15.8; //  X length end hole in RoM              
                   /// frbd . xehol = 15.8; //  X length end hole in RoM              
                   frbd . yehol = 0.275; //  Y length end hole in RoM              
                   /// frbd . yehol = 0.275; //  Y length end hole in RoM              
                   frbd . xlhol = 7.05; //  X length long hole in RoM              
                   /// frbd . xlhol = 7.05; //  X length long hole in RoM              
                   frbd . ylhol = 0.275; //  Y length long hole in RoM              
                   /// frbd . ylhol = 0.275; //  Y length long hole in RoM              
                   frbd . boffset = 11.92; //  Box Offset            
                   /// frbd . boffset = 11.92; //  Box Offset            
                   frbd . zoffb = 3.15; //  Z Offset for Box hole            
                   /// frbd . zoffb = 3.15; //  Z Offset for Box hole            
                   frbd . modleng = 16.6; //  length (z) of the module            
                   /// frbd . modleng = 16.6; //  length (z) of the module            
                   frbd . electrdx = 12.7; //  electronics width            
                   /// frbd . electrdx = 12.7; //  electronics width            
                   frbd . electrdy = 0.5; //  electronics thickness            
                   /// frbd . electrdy = 0.5; //  electronics thickness            
                   frbd . electrdz = 8.5; //  electronics length            
                   /// frbd . electrdz = 8.5; //  electronics length            
                   frbd . coolpldx = 12.7; //  cooling plate width            
                   /// frbd . coolpldx = 12.7; //  cooling plate width            
                   frbd . coolpldy = 0.025; //  cooling plate thickness            
                   /// frbd . coolpldy = 0.025; //  cooling plate thickness            
                   frbd . coolpldz = 6.6; //  cooling plate length            
                   /// frbd . coolpldz = 6.6; //  cooling plate length            
                   frbd . eclpldx = 1.8; //  cooling end plates width            
                   /// frbd . eclpldx = 1.8; //  cooling end plates width            
                   frbd . eclpldy = 0.025; //  cooling end plates thickness            
                   /// frbd . eclpldy = 0.025; //  cooling end plates thickness            
                   frbd . eclpldz = 6.6; //  cooling end plates length            
                   /// frbd . eclpldz = 6.6; //  cooling end plates length            
                   frbd . cakehir = 25.5; //  Cake Hole Inner Radius            
                   /// frbd . cakehir = 25.5; //  Cake Hole Inner Radius            
                   frbd . cakehor = 30.5; //  Cake Hole Outer Radius            
                   /// frbd . cakehor = 30.5; //  Cake Hole Outer Radius            
                   frbd . cakehwz = 7.55; //  Half Cake Hole Width in Z                
                   /// frbd . cakehwz = 7.55; //  Half Cake Hole Width in Z                
                   frbd . boxhx = 2.88; //  Half Box Hole Length in X            
                   /// frbd . boxhx = 2.88; //  Half Box Hole Length in X            
                   frbd . boxhy = 2.845; //  Half Box Hole Length in Y             
                   /// frbd . boxhy = 2.845; //  Half Box Hole Length in Y             
                   frbd . boxhz = 2.95; //  Half Box Hole Length in Z            
                   /// frbd . boxhz = 2.95; //  Half Box Hole Length in Z            
                   frbd . eboxhx = 0.5; //  Half End Box Hole Length in X            
                   /// frbd . eboxhx = 0.5; //  Half End Box Hole Length in X            
                   frbd . eboxhy = 2.626; //  Half End Box Hole Length in Y             
                   /// frbd . eboxhy = 2.626; //  Half End Box Hole Length in Y             
                   frbd . eboxhz = 7.55; //  Half End Box Hole Length in Z            
                   /// frbd . eboxhz = 7.55; //  Half End Box Hole Length in Z            
                   frbd . lboxhx = 15.3; //  Half Long Box Hole Length in X            
                   /// frbd . lboxhx = 15.3; //  Half Long Box Hole Length in X            
                   frbd . lboxhy = 2.628; //  Half Long Box Hole Length in Y             
                   /// frbd . lboxhy = 2.628; //  Half Long Box Hole Length in Y             
                   frbd . lboxhz = 0.5; //  Half Long Box Hole Length in Z                    
                   /// frbd . lboxhz = 0.5; //  Half Long Box Hole Length in Z                    
                   //           
                   frbd.fill();           
             ///@}        
             //        
             // ---------------------------------------------------------------------------------------------------        
             ///@addtogroup fssd_doc        
             ///@{           
                   ++fssd._index;           
                   fssd . version = 1; //  geometry Version            
                   /// fssd . version = 1; //  geometry Version            
                   fssd . eringrmn = 30.6; //  endring inner radius            
                   /// fssd . eringrmn = 30.6; //  endring inner radius            
                   fssd . eringrmx = 36.4; //  endring outer radius            
                   /// fssd . eringrmx = 36.4; //  endring outer radius            
                   fssd . eringdz = 5.175; //  half endring thickness (z)             
                   /// fssd . eringdz = 5.175; //  half endring thickness (z)             
                   fssd . oeringdz = 0.5; //  outer endring thickness (z)             
                   /// fssd . oeringdz = 0.5; //  outer endring thickness (z)             
                   fssd . erposz = 4.675; //  Position of inner and outer Ring in End Ring            
                   /// fssd . erposz = 4.675; //  Position of inner and outer Ring in End Ring            
                   fssd . meringrm = 31.1; //  endring medium outer radius                    
                   /// fssd . meringrm = 31.1; //  endring medium outer radius                    
                   fssd . meringdz = 4.175; //  medium endring thickness (z)             
                   /// fssd . meringdz = 4.175; //  medium endring thickness (z)             
                   fssd . erpolyrm = 31.75; //  End ring Polygon max. radius            
                   /// fssd . erpolyrm = 31.75; //  End ring Polygon max. radius            
                   fssd . trapr = 35.823; //  Trapezoid Radius             
                   /// fssd . trapr = 35.823; //  Trapezoid Radius             
                   fssd . polyr = 33.791; //  Polygon Radius            
                   /// fssd . polyr = 33.791; //  Polygon Radius            
                   fssd . polydz = 7.05; //  half Polygon length in Z            
                   /// fssd . polydz = 7.05; //  half Polygon length in Z            
                   fssd . polyir = 0.0; //  Polygon Inner Radius              
                   /// fssd . polyir = 0.0; //  Polygon Inner Radius              
                   fssd . polyor = 1.515; //  Polygon Outer Radius            
                   /// fssd . polyor = 1.515; //  Polygon Outer Radius            
                   fssd . trapx1 = 0.4585; //  half small lenght of trapezoid in X            
                   /// fssd . trapx1 = 0.4585; //  half small lenght of trapezoid in X            
                   fssd . trapx2 = 2.625; //  half large lenght of trapezoid in X            
                   /// fssd . trapx2 = 2.625; //  half large lenght of trapezoid in X            
                   fssd . trapdy = 7.05; //  half hight of trapezoid in Y            
                   /// fssd . trapdy = 7.05; //  half hight of trapezoid in Y            
                   fssd . trapdz = 0.576; //  half width of trapezoid in Z             
                   /// fssd . trapdz = 0.576; //  half width of trapezoid in Z             
                   fssd . pgonpdz = 3.6; //  Position of Polygon in Supp. Struc FSSM (z)            
                   /// fssd . pgonpdz = 3.6; //  Position of Polygon in Supp. Struc FSSM (z)            
                   fssd . sbsdy = 33.6; //  Position for Stabi. Blocks in (y)             
                   /// fssd . sbsdy = 33.6; //  Position for Stabi. Blocks in (y)             
                   //           
                   fssd.fill();           
             ///@}        
             //        
             /// USE ftpg version=1 ;        
             ftpg.Use("version",(Float_t)1 );        
             /// USE ffcc version=1 ;        
             ffcc.Use("version",(Float_t)1 );        
             /// USE frbd version=1 ;        
             frbd.Use("version",(Float_t)1 );        
             /// USE fssd version=1 ;        
             fssd.Use("version",(Float_t)1 );        
             position=ftpg.zstart+ftpg.totlen/2;        
             _create = AgCreate("FTPC");        
             {           
                   AgShape myshape; // undefined shape           
                   ///Create FTPC           
                   Create("FTPC");            
             }        
             if ( agexist("svtt")!=0 )        
             {           
                   { AgPlacement place = AgPlacement("FTPC","SVTT");              
                         /// Add daughter volume FTPC to mother SVTT              
                         place.TranslateZ(position);              
                         /// Translate z = position              
                         place.par("only")=AgPlacement::kMany;              
                         /// Overlap: agplacement::kmany              
                         _stacker -> Position( AgBlock::Find("FTPC"), place );              
                   } // end placement of FTPC           
                   { AgPlacement place = AgPlacement("FTPC","SVTT");              
                         /// Add daughter volume FTPC to mother SVTT              
                         place.TranslateZ(-position);              
                         /// Translate z = -position              
                         place.par("only")=AgPlacement::kMany;              
                         /// Overlap: agplacement::kmany              
                         /// G3 Reference: thetax = 90              
                         /// G3 Reference: phix = 0              
                         /// G3 Reference: thetay = 90              
                         /// G3 Reference: phiy = 90              
                         /// G3 Reference: thetaz = 180              
                         /// G3 Reference: phiz = 0              
                         Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;              
                         place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );              
                         _stacker -> Position( AgBlock::Find("FTPC"), place );              
                   } // end placement of FTPC           
             }        
             else        
             {           
                   { AgPlacement place = AgPlacement("FTPC","CAVE");              
                         /// Add daughter volume FTPC to mother CAVE              
                         place.TranslateZ(position);              
                         /// Translate z = position              
                         place.par("only")=AgPlacement::kMany;              
                         /// Overlap: agplacement::kmany              
                         _stacker -> Position( AgBlock::Find("FTPC"), place );              
                   } // end placement of FTPC           
                   { AgPlacement place = AgPlacement("FTPC","CAVE");              
                         /// Add daughter volume FTPC to mother CAVE              
                         place.TranslateZ(-position);              
                         /// Translate z = -position              
                         place.par("only")=AgPlacement::kMany;              
                         /// Overlap: agplacement::kmany              
                         /// G3 Reference: thetax = 90              
                         /// G3 Reference: phix = 0              
                         /// G3 Reference: thetay = 90              
                         /// G3 Reference: phiy = 90              
                         /// G3 Reference: thetaz = 180              
                         /// G3 Reference: phiz = 0              
                         Double_t _thetax=90,_phix=0,_thetay=90,_phiy=90,_thetaz=180,_phiz=0;              
                         place.Reference( _thetax, _phix, _thetay, _phiy, _thetaz, _phiz );              
                         _stacker -> Position( AgBlock::Find("FTPC"), place );              
                   } // end placement of FTPC           
             }        
       }; // FtpcGeo     
 }; // namespace FtpcGeo  
 