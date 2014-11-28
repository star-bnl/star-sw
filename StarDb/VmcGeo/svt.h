// $Id: svt.h,v 1.10 2009/03/11 22:57:46 fisyak Exp $
// $Log: svt.h,v $
// Revision 1.10  2009/03/11 22:57:46  fisyak
// swap y <=> z in order to match with Alignment
//
// Revision 1.9  2009/02/26 16:42:59  fisyak
// Fix active detector thickness
//
// Revision 1.8  2009/02/25 20:23:20  fisyak
// Add the first version for misalignment
//
// Revision 1.7  2009/01/16 23:03:46  fisyak
// Add year2000 configuration for single ladder
//
// Revision 1.6  2009/01/16 16:01:14  fisyak
// Adjust SVT geometry topology for alignment
//
// Revision 1.5  2009/01/14 16:31:53  fisyak
// Freeze conversion from mortran to Cint for SVT
//
// Revision 1.4  2009/01/07 23:35:25  fisyak
// Merge all Svt configuretion in 3 base ones
//
// Revision 1.3  2008/08/27 21:48:17  fisyak
//
// all SVT volumes
// svtConfig ==  1 one ladder 
// svtConfig == 13 whole SVT
// svtConfig == 16 no SVT (upgrade)
/* 
   SVTT->SIRP
       ->SIRT
       ->SOER
       ->SBRG->SBRM->SBRI
       ->SOES->SOSM->SCRW
       ->SIES->SISM->SCRW
       ->SWMM->SWMB
             ->SWMT
             ->SWMS
	      ->SWMW
	      ->SOTB
 	      ->SITB
       ->SBWC->SWCM->SXAI
                   ->SXBI
#if 0
       ->SCON->SGRA
             ->STAP
	     ->STAC
	     ->SHLA->SHMA->SWHO->SHWA
	     ->SHLB->SHMB->SWHO->SHWA
	     ->SCMY
	     ->SCAL
#endif
       ->SBSP->SAKM->SCKM
             ->SBMM->SMRD
	           ->SBMO
		   ->SBMI
	     ->SBRL
	     ->SBRX
	     ->SBSR->SBCR
       ->SROD
       ->SISH
       ->SOSH
       ->SALM
       ->SSSH
       ->SCBM->SCBL   [SCBL ilayer = [1,4]]
             ->SFED   [SFED ilayer = [1,3]]
	     ->SPLS   [SPLS   -"-         ]
       ->SXRL->SWRP
             ->SYRU
       ->SOUM->SOUR
       ->ClamShell->Layer->Ladder->SELE->SWCH   [ClamShell->Layer ilayer = [1, 6]]
                                       ->SWCS			       		  
		  		       ->SWCW			 
		  		       ->SBOI			 
		  		       ->SGLA			 
		  		       ->SAGP			 
		  		       ->SDYE			 
		  		       ->SECA			 
                  		 ->SLDI->SBER	a ladder volume		 
		  		       ->STAB			 
		  		       ->STRU			 
		  		       ->SRHC			 
		  		       ->SPCB			 
                  		       ->SVTD->STRA remove STLI and STSI a trapezoid of triangular shape
		  		             ->SSID a non-sensitive left-right border of the wafer
		  		             ->SSIR a non-sensitive up-down border of the wafer
  Numbering scheme g2t_volumeId
 --------------------------------
  lnumber    = numbv(1),  ladder = numbv(2), wafer = numbv(3)   ! 
  nladder =  1;  wafer   = lnumber;  ladder  = 12;  lnumber = 4 ! This is the year 1 ladder
  volume_id = 3000 + 100*wafer + 12;

  nladder =  8;  nwafer  = 4                                    ! Set First barrel ids
  nladder = 12;  nwafer  = 6	                                ! Set 2nd barrel ids
  nladder = 16;  nwafer  = 7                                    ! Set 3rd barrel ids
* PN: change geant numbering (CCW) to STAR numbering(CW):
           if (nladder>1) then
              lsub    = mod(lnumber-1,2)
*             NEW: 12 o'clock is geant's first and STAR last element:
                ladder=nladder-(ladder-1)*2-lsub
           endif
        volume_id  = 1000*lnumber+100*wafer+ladder
 */
#ifndef svtConfig
#define svtConfig 13
#endif
#ifdef svtConfig
#if !defined(__CINT__)
#include "Riostream.h"
#include "TMath.h"
#include "TGeoManager.h"
#include "Rotations.h"
#include "Material.h"
#include "Media.h"
#include "CreateGeometry.h"
#include "TGeoMatrix.h"
//#include "TGeoPcon.h"
#include "TGeoPgon.h"
#include "TGeoPara.h"
#include "TGeoCompositeShape.h"
#endif
//#define DEBUG
TGeoVolume *svt() {
  Double_t degrad = TMath::Pi()/180;
  // svttgeo11.g
  struct SVTG_t { 
    Int_t    Version;    // Version   = geometry Version				  
    Int_t    Nlayer;     // Nlayer    = number of svt layers (was 7)			  
    Double_t RsizeMin;	 // RsizeMin  = STV innermost radius				  
    Double_t RsizeMax;	 // RsizeMax  = STV outermost radius				  
    Double_t ZsizeMax;	 // ZsizeMax  = SVT+FTPC length				  
    Double_t Angoff;	 // Angoff    = angular offset x1 for slayer 2 x2 for slayer 3  
    Double_t SupportVer; // SupportVer= Versioning of the shield			  
    Double_t ConeVer;	 // ConeVer   = Versioning of the support cone		  
    Double_t ifMany;	 // ifMany    = whether we use the geant MANY option		  
    Int_t    Nmin;	 // Nmin      = the index of the innermost layer                
  };
  SVTG_t SVTG = {// Basic SVT dimensions 
       2      ,// Version   = geometry Version				  
       7      ,// Nlayer    = number of svt layers (was 7)			  
       4.100  ,// RsizeMin  = STV innermost radius				  
       46.107 ,// RsizeMax  = STV outermost radius				  
       270    ,// ZsizeMax  = SVT+FTPC length				  
       0      ,// Angoff    = angular offset x1 for slayer 2 x2 for slayer 3  
       1      ,// SupportVer= Versioning of the shield			  
       1      ,// ConeVer   = Versioning of the support cone		  
       0      ,// ifMany    = whether we use the geant MANY option		  
       1       // Nmin      = the index of the innermost layer                
  };
  struct SWCA_t { // Wafer Carrier
    Int_t    Version;  // Version   =geometry Version				       
    Double_t Length;   // Length    =ladder length					       
    Double_t WaferWid; // WaferWid  =wafer width					       
    Double_t WaferLen; // WaferLen  =wafer length					       
    Double_t WaferThk; // WaferThk  =wafer thickness				       
    Double_t RohaThk;  // RohaThk   =Roha cell plus glue thickness 			       
    Double_t WafCarWd; // WafCarWd  =wafer carrier rails width			       
    Double_t WafCarTh; // WafCarTh  =wafer carrier thickness 			       
    Double_t WaferGap; // WaferGap  =inter-wafer gap - there is a 50 micron uncertainty    
    Double_t Drift;    // Drift     =drift direction				       
    Double_t strutlen; // strutlen  =len (z) of strut between waf car. rails (approx)      
    Double_t SensWid;  // SensWid   =Define the sensitive square width  (new)	       
    Double_t SensLen;  // SensLen   =Define the sensitive square length (new)	       
    Double_t SensGir;  // SensGir   =Girth (for lack of a better word)                     
  };
  SWCA_t SWCA = {// Wafer Carrier
       1          ,// Version   =geometry Version				       
       56.04      ,// Length    =ladder length					       
       6.305      ,// WaferWid  =wafer width					       
       6.305      ,// WaferLen  =wafer length					       
       0.0300     ,// WaferThk  =wafer thickness				       
       0.0750     ,// RohaThk   =Roha cell plus glue thickness 			       
       1.5        ,// WafCarWd  =wafer carrier rails width			       
       0.0300     ,// WafCarTh  =wafer carrier thickness 			       
       0.0        ,// WaferGap  =inter-wafer gap - there is a 50 micron uncertainty    
       1          ,// Drift     =drift direction				       
       1.0        ,// strutlen  =len (z) of strut between waf car. rails (approx)      
       6.000      ,// SensWid   =Define the sensitive square width  (new)	       
       6.000      ,// SensLen   =Define the sensitive square length (new)	       
       5.775       // SensGir   =Girth (for lack of a better word)                     
  };
  struct SSUP_t { // Support structures
    Int_t    Version;       // Version   = geometry Version				   
    Double_t CabThk;	    // CabThk    = thickness of layer of cables on support cone	   
    Double_t HosRmn;	    // HosRmn    = inner radius of water hoses on support cone	   
    Double_t HosRmx;	    // HosRmx    = outer radius of water hoses on support cone	   
    Double_t Nhoses;	    // Nhoses    = number of water hoses				   
    Double_t WrpMyThk;	    // WrpMyThk  = thickness of mylar wrap around cone (guess)	   
    Double_t WrpAlThk;	    // WrpAlThk  = thickness of Al on mylar wrap (guess)		   
    Double_t GrphThk;	    // GrphThk   = support cone thickness			   
    Double_t Cone1Zmn;	    // Cone1zmn  = Cone z min (parts 1,2,3,4 in increasing z)	   
    Double_t RodLen;	    // RodLen    = Length of support rods			   
    Double_t RodDist;	    // RodDist   = Distance of support rod od from beam axis 	   
    Double_t RodID;	    // RodID     = ID of Carbon support rods (approx)		   
    Double_t RodOD;	    // RodOD     = OD of Carbon support rods (approx)		   
    Double_t Con1IdMn;	    // Con1IdMn  = Minimum id of cone 1 				   
    Double_t Con3IdMn;	    // Con3IdMn  = Minimum id of cone 3 (TBD)			   
    Double_t Con4IdMn;	    // Con4IdMn  = Minimum id of cone 4 (TBD)			   
    Double_t Con4IdMx;	    // Con4IdMx  = Maximum id of cone 4 (TBD)			   
    Double_t Cone3zmx;	    // Cone3zmx  = Maximum z of cone 3 (TBD)			   
    Double_t Cone4zmx;	    // Cone4zmx  = Maximum z of cone 4 (TBD)			   
    Double_t BraThk;	    // BraThk    = thickness of Al brackets 			   
    Double_t ERJThk;	    // ERJThk    = (z) thickness of end ring joining brackets	   
    Double_t ERJWid;	    // ERJWid    = (azimuthal) width of end ring joining brackets   
    Double_t ERJLen;	    // ERJLen    = (radial) length of end ring joining brackets	   
    Double_t ERJzdis;	    // ERJzdis   = dist from ladder ends to ERJ (guess)		   
    Double_t ERJ1x;	    // ERJ1x     = ERJ screw 1 x position (radial)		   
    Double_t ERJ2x;	    // ERJ2x     = ERJ screw 2 x position			   
    Double_t ERJ2y;	    // ERJ2y     = ERJ screw 2 y position			   
    Double_t ERJrad;	    // ERJrad    = distance of ERJ center from beam axis		   
    Double_t ERJdia;	    // ERJdia    = ERJ screw diameter                               
  };
  SSUP_t SSUP = {
      2          ,//1      Version   = geometry Version				   
      0.21       ,//0.05   CabThk    = thickness of layer of cables on support cone	   
      0.75       ,//0.75   HosRmn    = inner radius of water hoses on support cone	   
      0.95       ,//0.95   HosRmx    = outer radius of water hoses on support cone	   
      10         ,//10     Nhoses    = number of water hoses				   
      0.10       ,//0.10   WrpMyThk  = thickness of mylar wrap around cone (guess)	   
      0.01       ,//0.01   WrpAlThk  = thickness of Al on mylar wrap (guess)		   
      0.16	 ,//0.16   GrphThk   = support cone thickness			   
      52.23      ,//52.23  Cone1zmn  = Cone z min (parts 1,2,3,4 in increasing z)	   
      110.8      ,//110.8  RodLen    = Length of support rods			   
      17.5       ,//17.5   RodDist   = Distance of support rod od from beam axis 	   
      2.5        ,//2.5    RodID     = ID of Carbon support rods (approx)		   
      3.05       ,//3.05   RodOD     = OD of Carbon support rods (approx)		   
      15.67      ,//15.67  Con1IdMn  = Minimum id of cone 1 				   
      21.67      ,//21.67  Con3IdMn  = Minimum id of cone 3 (TBD)			   
      37.4       ,//37.4   Con4IdMn  = Minimum id of cone 4 (TBD)			   
      37.4       ,//37.4   Con4IdMx  = Maximum id of cone 4 (TBD)			   
      150.0      ,//150.0  Cone3zmx  = Maximum z of cone 3 (TBD)			   
      229.36     ,//229.36 Cone4zmx  = Maximum z of cone 4 (TBD)			   
      .2         ,//.2     BraThk    = thickness of Al brackets 			   
      .1         ,//.1     ERJThk    = (z) thickness of end ring joining brackets	   
      2.07       ,//2.07   ERJWid    = (azimuthal) width of end ring joining brackets   
      5.19       ,//5.19   ERJLen    = (radial) length of end ring joining brackets	   
      2.0        ,//2.0    ERJzdis   = dist from ladder ends to ERJ (guess)		   
      0.31       ,//0.31   ERJ1x     = ERJ screw 1 x position (radial)		   
      1.15       ,//1.15   ERJ2x     = ERJ screw 2 x position			   
      0.72       ,//0.72   ERJ2y     = ERJ screw 2 y position			   
      10.80      ,//10.80  ERJrad    = distance of ERJ center from beam axis		   
      0.17        //0.17   ERJdia    = ERJ screw diameter                               
  };
  struct SSUB_t { // beampipe support
    Int_t    Version;    // Version   = geometry Version			   
    Double_t KMountId;	 // KMountId  = id of beampipe support kinematic mount	   
    Double_t KMountOd;	 // KMountOd  = od of beampipe support kinematic mount	   
    Double_t KMntThk;	 // KMntThk   = thickness of support kinematic mount	   
    Double_t KMCutOd;	 // KMCutOd   = od of cutout in kinematic mount	   
    Double_t KMCutId;	 // KMCutId   = id of cutout in kinematic mount	   
    Double_t KMCutOA;	 // KMCutOA   = opening angle of cutout		   
    Double_t KMCutOff;	 // KMCutOff  = offset of cutout center from axis	   
    Double_t SRingId;	 // SRingId   = id of beampipe support ring 		   
    Double_t SRingOd;	 // SRingOd   = od of beampipe support ring		   
    Double_t SRingThk;	 // SRingThk  = thichkness of beampipe support ring	   
    Double_t SRCutPhi;	 // SRCutPhi  = support ring cutout angle to z-axis	   
    Double_t SRCutWid;	 // SRCutWid  = support ring cutout width		   
    Double_t SRCutOut;	 // SRCutOut  = support ring cutout depth		   
    Double_t SRCutIn;	 // SRCutIn   = support ring cutout start		   
    Double_t SRollId;	 // SRollId   = support roller Id			   
    Double_t SRollOd;	 // SRollOd   = support roller Od			   
    Double_t SRollLen;	 // SRollLen  = support roller length			   
    Double_t SWireLen;	 // SWireLen  = support roller axis length		   
    Double_t MBlkHgh;	 // MBlkHgh   = mounting block height			   
    Double_t MBlkOWid;	 // MBlkOWid  = outer mounting block width		   
    Double_t MBlkOLen;	 // MBlkOLen  = outer mounting block length		   
    Double_t MBlkIWid;	 // MBlkIWid  = inner mounting block width		   
    Double_t MBlkILen;	 // MBlkILen  = inner mounting block length		   
    Double_t MBlkORad;	 // MBlkORad  = outer mounting block at radius		   
    Double_t MBlkIRad;	 // MBlkIRad  = inner mounting block at radius		   
    Double_t MRodDia;	 // MRodDia   = mounting support rod diameter             
  };
  SSUB_t SSUB = {
      1          ,// Version   = geometry Version			   
      31.34      ,// KMountId  = id of beampipe support kinematic mount	   
      38.96      ,// KMountOd  = od of beampipe support kinematic mount	   
      0.64       ,// KMntThk   = thickness of support kinematic mount	   
      18.31      ,// KMCutOd   = od of cutout in kinematic mount	   
      14         ,// KMCutId   = id of cutout in kinematic mount	   
      38         ,// KMCutOA   = opening angle of cutout		   
      26.58      ,// KMCutOff  = offset of cutout center from axis	   
      8.47       ,// SRingId   = id of beampipe support ring 		   
      12.78      ,// SRingOd   = od of beampipe support ring		   
      0.64       ,// SRingThk  = thichkness of beampipe support ring	   
      38         ,// SRCutPhi  = support ring cutout angle to z-axis	   
      3.63       ,// SRCutWid  = support ring cutout width		   
      5.08       ,// SRCutOut  = support ring cutout depth		   
      3.5        ,// SRCutIn   = support ring cutout start		   
      0.2        ,// SRollId   = support roller Id			   
      0.62       ,// SRollOd   = support roller Od			   
      2.54       ,// SRollLen  = support roller length			   
      5.08       ,// SWireLen  = support roller axis length		   
      0.97       ,// MBlkHgh   = mounting block height			   
      2.54       ,// MBlkOWid  = outer mounting block width		   
      1.27       ,// MBlkOLen  = outer mounting block length		   
      3.175      ,// MBlkIWid  = inner mounting block width		   
      1.27       ,// MBlkILen  = inner mounting block length		   
      17.4       ,// MBlkORad  = outer mounting block at radius		   
      5.42       ,// MBlkIRad  = inner mounting block at radius		   
      0.32        // MRodDia   = mounting support rod diameter             
  };  
  struct SWAM_t { // water manifold
    Int_t    Version; // Version  = geometry Version					
    Double_t Zmin;    // Zmin     = minimum z for positioning water manifold		
    Double_t Len;     // Len      = water manifold full length 			
    Double_t Rmin;    // Rmin     = water manifold rmin (not including trans. brds)	
    Double_t Rmax;    // Rmax     = water manifold rmax (not including trans. brds)	
    Double_t TbrdThk; // TbrdThk  = transition board thickness			
    Double_t WallThk; // WallThk  = water manifold wall thickness                     
  };
  SWAM_t SWAM = { // water manifold
      1        ,// Version  = geometry Version					
      33.9     ,// Zmin     = minimum z for positioning water manifold		
      16.0     ,// Len      = water manifold full length 			
      15.24    ,// Rmin     = water manifold rmin (not including trans. brds)	
      16.83    ,// Rmax     = water manifold rmax (not including trans. brds)	
      0.1      ,// TbrdThk  = transition board thickness			
      0.1       // WallThk  = water manifold wall thickness                     
  };
  struct SERG_t { // end rings
    Int_t    Version;  // Version   = geometry Version			   
    Double_t IrngTrMx; // IrngTrMx  = Inner end ring tube maximum radius 	   
    Double_t IrngPrMn; // IrngPrMn  = Inner end ring polygon minimum radius  
    Double_t OrngRmin; // OrngRmin  = Outer end ring minimum radius	   
    Double_t OrngRmax; // OrngRmax  = Outer end ring maximum radius	   
    Double_t EndRngTh; // EndRngTh  = End ring thickness			   
    Double_t EndRngZm; // EndRngZm  = minimum z for end rings                
  };
  SERG_t SERG = { // end rings
    1     ,// Version   = geometry Version			   
    9.703 ,// IrngTrMx  = Inner end ring tube maximum radius 	   
    7.671 ,// IrngPrMn  = Inner end ring polygon minimum radius  
    11.900,// OrngRmin  = Outer end ring minimum radius	   
    13.805,// OrngRmax  = Outer end ring maximum radius	   
    0.2   ,// EndRngTh  = End ring thickness			   
    23.01  // EndRngZm  = minimum z for end rings                
  };
  struct SELC_t { // electronics carrier data
    Int_t    Version;  // Version  = geometry Version				     
    Double_t BeThk;    // BeThk    = thickness of Berillium water channel walls	     
    Double_t WatThk;   // WatThk   = thickness of water channel			     
    Double_t BeOThk;   // BeOThk   = thickness of Berrillia (BeO) substra		     
    Double_t DyeThk;   // DyeThk   = to give .11% of a radiation length of Si 	     
    Double_t DyeWid;   // DyeWid   = width of ic chips (2 covers 0.3 of area)	     
    Double_t DyeSpc;   // DyeSpc   = space ic chips				     
    Double_t ElcaWid;  // ElcaWid  = electronics carrier width			     
    Double_t AgPdThk;  // AgPdThk  = thickness of Ag-Pd conductor			     
    Double_t GlassThk; // GlassThk = thickness of glass insulator			     
    Double_t CabThk;   // CabThk   = Cu for cables of .23% rad len (weighted average)  
    Double_t CabWid;   // CabWid   = cable width                                       
  };
  SELC_t SELC = { // electronics carrier data
      1     ,// Version  = geometry Version				     
      0.0500,// BeThk    = thickness of Berillium water channel walls	     
      0.0750,// WatThk   = thickness of water channel			     
      0.0500,// BeOThk   = thickness of Berrillia (BeO) substra		     
      0.0340,// DyeThk   = to give .11% of a radiation length of Si 	     
      0.3   ,// DyeWid   = width of ic chips (2 covers 0.3 of area)	     
      0.1   ,// DyeSpc   = space ic chips				     
      2.0   ,// ElcaWid  = electronics carrier width			     
      0.0030,// AgPdThk  = thickness of Ag-Pd conductor			     
      0.2000,// GlassThk = thickness of glass insulator			     
      0.0033,// CabThk   = Cu for cables of .23% rad len (weighted average)  
      0.6    // CabWid   = cable width                                       
  };
  struct SVTL_t { // single layer data
    Int_t    Layer;     // layer    = layer number						      
    Int_t    Nladder;	// Nladder  = number of ladder					      
    Int_t    Nwafer;	// Nwafer   = number of wafers					      
    Double_t Radius;	// Radius   = layer radius (center of wafer position)		      
    Double_t BareEdge;	// BareEdge = the strip of bare Be which makes the PCB shorter	      
    Double_t PcbLen;	// PcbLen   = PCB Length						      
    Double_t PcbWidth;	// PcbWidth = PCB Width						      
    Double_t PcbThk;	// PcbThk   = PCB Thickness -- should be 0.09, but we add extra for copper 
    Double_t PcbGap;	// PcbGap   = Gap between the PCB and the Si Wafer                         
  };
  Double_t radii[6] = { 6.37, 7.38, 10.38, 11.27, 14.19, 15.13};
  /** Important: the radial offset -- we position by the center of the volume,
   ** hence half the wafer thicknes should be added. Remember to change it if
   ** it will ever be anyhting other than 300 um. */
  Double_t rad_offset=0.015;
  Double_t shield_phi[4] = { 9.0, 27.0, 45.0, 63.0};
  SVTL_t SVTL[6] = { // single layer data
    {1                    ,// layer    = layer number						      
     4                    ,// Nladder  = number of ladder					      
     4                    ,// Nwafer   = number of wafers					      
     radii[0]+rad_offset  ,// Radius   = layer radius (center of wafer position)		      
     1.0                  ,// BareEdge = the strip of bare Be which makes the PCB shorter	      
     14.9                 ,// PcbLen   = PCB Length						      
     6.3                  ,// PcbWidth = PCB Width						      
     0.1                  ,// PcbThk   = PCB Thickness -- should be 0.09, but we add extra for copper 
     0.2                 },// PcbGap   = Gap between the PCB and the Si Wafer                         
    {2                    ,// layer    = layer number
     4                    ,// Nladder  = number of ladder
     4                    ,// Nwafer   = number of wafers
     radii[1]+rad_offset  ,// Radius   = layer radius (center of wafer position)
     1.0                  ,// BareEdge = the strip of bare Be which makes the PCB shorter
     14.9                 ,// PcbLen   = PCB Length
     6.3                  ,// PcbWidth = PCB Width
     0.1                  ,// PcbThk   = PCB Thickness -- should be 0.09, but we add extra for copper
     0.2                 },// PcbGap   = Gap between the PCB and the Si Wafer
    {3                    ,// layer    = layer number
     6                    ,// Nladder  = number of ladder
     6                    ,// Nwafer   = number of wafers
     radii[2]+rad_offset  ,// Radius   = layer radius (center of wafer position)
     1.0                  ,// BareEdge = the strip of bare Be which makes the PCB shorter
     7.5                  ,// PcbLen   = PCB Length
     6.3                  ,// PcbWidth = PCB Width
     0.1                  ,// PcbThk   = PCB Thickness -- should be 0.09, but we add extra for copper
     0.2                 },// PcbGap   = Gap between the PCB and the Si Wafer
    {4                    ,// layer    = layer number
     6                    ,// Nladder  = number of ladder
     6                    ,// Nwafer   = number of wafers
     radii[3]+rad_offset  ,// Radius   = layer radius (center of wafer position)
     1.0                  ,// BareEdge = the strip of bare Be which makes the PCB shorter
     7.5                  ,// PcbLen   = PCB Length
     6.3                  ,// PcbWidth = PCB Width
     0.1                  ,// PcbThk   = PCB Thickness -- should be 0.09, but we add extra for copper
     0.2                 },// PcbGap   = Gap between the PCB and the Si Wafer
    {5                    ,// layer    = layer number
     8                    ,// Nladder  = number of ladder
     7                    ,// Nwafer   = number of wafers
     radii[4]+rad_offset  ,// Radius   = layer radius (center of wafer position)
     1.0                  ,// BareEdge = the strip of bare Be which makes the PCB shorter
     4.4                  ,// PcbLen   = PCB Length
     6.3                  ,// PcbWidth = PCB Width
     0.1                  ,// PcbThk   = PCB Thickness -- should be 0.09, but we add extra for copper
     0.2                 },// PcbGap   = Gap between the PCB and the Si Wafer
    {6                    ,// layer    = layer number
     8                    ,// Nladder  = number of ladder
     7                    ,// Nwafer   = number of wafers
     radii[5]+rad_offset  ,// Radius   = layer radius (center of wafer position)
     1.0                  ,// BareEdge = the strip of bare Be which makes the PCB shorter
     4.4                  ,// PcbLen   = PCB Length
     6.3                  ,// PcbWidth = PCB Width
     0.1                  ,// PcbThk   = PCB Thickness -- should be 0.09, but we add extra for copper
     0.2                  }// PcbGap   = Gap between the PCB and the Si Wafer
  };
  struct SSLD_t { // shielding parameters
    Int_t    Version;   // Version  = geometry Version			  
    Double_t SInRInn;	// SInRInn  = inner shield cylinder, inner radius	  
    Double_t SInROut;	// SInROut  = inner shield cylinder, outer radius	  
    Double_t SInLen;	// SInLen   = inner shield cylinder, half length	  
    Double_t SSepRInn;	// SSepRInn = separation shield cylinder, inner radius  
    Double_t SSepROut;	// SSepROut = separation shield cylinder, outer radius  
    Double_t SSepLen;	// SSepLen  = separation shield cylinder, half length	  
    Double_t SOutRInn;	// SOutRInn = outer shield cylinder, inner radius	  
    Double_t SOutROut;	// SOutROut = outer shield cylinder, outer radius	  
    Double_t SOutLen;	// SOutLen  = outer shield cylinder, half length	  
    Double_t AlMeshId;	// AlMeshId = Aluminum shield mesh inner diameter	  
    Double_t AlMeshOd;	// AlMeshOd = Aluminum shield mesh outer diameter	  
    Double_t AlMshThk;	// AlMshThk = Aluminum shield mesh effective thickness  
    Double_t AlMshPos;	// AlMshPos = Aluminum shield mesh z position           
  }; 
  SSLD_t SSLD = { // shielding parameters
      3          ,//2          ,//1          ,// Version  = geometry Version
      5          ,//5.9        ,//5          ,// SInRInn  = inner shield cylinder, inner radius
      5.008      ,//5.908      ,//5.008      ,// SInROut  = inner shield cylinder, outer radius
      53.5       ,//53.5       ,//53.5       ,// SInLen   = inner shield cylinder, half length
      22         ,//22         ,//22         ,// SSepRInn = separation shield cylinder, inner radius
      22.018     ,//22.018     ,//22.018     ,// SSepROut = separation shield cylinder, outer radius
      55.4       ,//55.4       ,//55.4       ,// SSepLen  = separation shield cylinder, half length
      31.80      ,//29.5       ,//29.5       ,// SOutRInn = outer shield cylinder, inner radius
      31.82      ,//29.52      ,//29.52      ,// SOutROut = outer shield cylinder, outer radius
      65.4       ,//65.4       ,//65.4       ,// SOutLen  = outer shield cylinder, half length
      9.7        ,//9.7        ,//9.7        ,// AlMeshId = Aluminum shield mesh inner diameter
      44         ,//44         ,//44         ,// AlMeshOd = Aluminum shield mesh outer diameter
      0.03       ,//0.03       ,//0.03       ,// AlMshThk = Aluminum shield mesh effective thickness
      53.5        //53.5        //53.5        // AlMshPos = Aluminum shield mesh z position           
  };

  /* the following Version of the shield will accomodate the PIXEL detector,
   * so it needs to be slightly bigger
   */
  struct SCBP_t { // Cabling
    Int_t    Layer; // Layer = Layer				
    Double_t Len;   // Len   = Length			
    Double_t Rmin1; // Rmin1 = Min radius closer to wafers	
    //    Double_t Rmax1; 
    Double_t Rmin2; // Rmin2 = Min radius further from wafers
    //    Double_t Rmax2; 
    Double_t Vol;   // Vol   = Volume of copper, LV+HV cables
  };
  Double_t rad_cones_in[4], rad_cones_out[4];
  for (Int_t i = 1; i <= 4; i++) {
    rad_cones_in[i-1] = 8.5+2.60*(i-1);
    rad_cones_out[i-1]=15.0+0.85*(i-1);
  }
  SCBP_t SCBP[4] = {// Cabling
    {1               ,// Layer = Layer				
     1.85            ,// Len   = Length			
     rad_cones_in[0] ,// Rmin1 = Min radius closer to wafers	
     rad_cones_out[0],// Rmin2 = Min radius further from wafers
     7.24+3.21      },// Vol   = Volume of copper, LV+HV cables
    {2               ,// Layer = Layer				
     1.85            ,// Len   = Length			
     rad_cones_in[1] ,// Rmin1 = Min radius closer to wafers	
     rad_cones_out[1],// Rmin2 = Min radius further from wafers
     15.54+5.7      },// Vol   = Volume of copper, LV+HV cables
    {3               ,// Layer = Layer				
     1.85            ,// Len   = Length			
     rad_cones_in[2] ,// Rmin1 = Min radius closer to wafers	
     rad_cones_out[2],// Rmin2 = Min radius further from wafers
     4.05+2.02+3.67+1.69},// Vol   = Volume of copper, LV+HV cables -- 3+4 layers coalesce
    {4               ,// Layer = Layer				
     1.85            ,// Len   = Length			
     rad_cones_in[3] ,// Rmin1 = Min radius closer to wafers	
     rad_cones_out[3],// Rmin2 = Min radius further from wafers
     6.95+2.43       }// Vol   = Volume of copper, LV+HV cables
  };
  struct SFEP_t { // Water feed
    Int_t    Layer;        // Layer=Layer 
    Double_t Len;	   // Len  =Length
    Double_t Rmin1;        // Rmin1=Min radius closer to wafers   
    //    Double_t Rmax1;	   
    Double_t Rmin2;        // Rmin2=Min radius further from wafers
    //    Double_t Rmax2;  
    Double_t Vol;          // Vol  =Volume of water	     
    Double_t VolPlast;	   // VolPlast  =Volume of plastic
  };
  SFEP_t SFEP[3] = {// Water feed
    {1               ,// Layer=Layer 
     1.85            ,// Len  =Length
     0.5*(rad_cones_in[0] +rad_cones_in[1])    ,// Rmin1=Min radius closer to wafers   
     0.5*(rad_cones_out[0]+rad_cones_out[1])   ,// Rmin2=Min radius further from wafers
     16.0            ,// Vol  =Volume of water	     
     38.4           },// VolPlast  =Volume of plastic
    {2               ,// Layer=Layer 
     1.85            ,// Len  =Length
     0.5*(rad_cones_in[1] +rad_cones_in[2])    ,// Rmin1=Min radius closer to wafers   
     0.5*(rad_cones_out[1]+rad_cones_out[2])   ,// Rmin2=Min radius further from wafers
     24.0            ,// Vol  =Volume of water	     
     57.6           },// VolPlast  =Volume of plastic
    {3               ,// Layer=Layer 
     1.85            ,// Len  =Length
     0.5*(rad_cones_in[2] +rad_cones_in[3])    ,// Rmin1=Min radius closer to wafers   
     0.5*(rad_cones_out[2]+rad_cones_out[3])   ,// Rmin2=Min radius further from wafers
     32.0            ,// Vol  =Volume of water	     
     76.8           } // VolPlast  =Volume of plastic
  };
  struct SWCX_t {   // Segments of the water distribution pipes
    Int_t    Version;   // Version = Version					    
    Double_t Length;	// Length  = of the ring in the Z direction		    
    Double_t dR;	// dR      = thickness of the mother layer			    
    Double_t rad;	// rad     = inner plastic tube radius			    
    Double_t offset;	// offset  = from the edge of the ladder support, inward	    
    Double_t rOffset;	// rOffset = Radial offset					    
    Double_t wall;	// wall    = thickness of the plastic pipe wall                
  };
  SWCX_t SWCX = {// Segments of the water distribution pipes
    1             ,// Version = Version					    
    2.8           ,// Length  = of the ring in the Z direction		    
    0.72          ,// dR      = thickness of the mother layer			    
    0.2           ,// rad     = inner plastic tube radius			    
    -2.0          ,// offset  = from the edge of the ladder support, inward	    
    1.0           ,// rOffset = Radial offset					    
    0.16           // wall    = thickness of the plastic pipe wall                
  };
  struct SOUP_t { // Mother of the outer shielding cage, parameters
    Int_t    Version; // Version = Version								     
    Double_t Length;  // Length  = Length								     
    Double_t Rout;    // Rout    = Outer radius of the shield					     
    Double_t dR;      // dR      = Diameter of the tubes constituting the cage (also dR of the mother)  
    Double_t Phi1;    // Phi1    = Starting angle of the mother					     
    Double_t Phi2;    // Phi2    = Finishing angle of the mother					     
    Double_t DiamOut; // DiamOut = Outer diam of the carbon tube					     
    Double_t DiamIn;  // DiamIn  = Inner diam of the carbon tube                                        
  };
  SOUP_t SOUP = {            // Mother of the outer shielding cage, parameters
      1           ,// Version = Version								     
      82.5        ,// Length  = Length								     
      19.41       ,// Rout    = Outer radius of the shield					     
      0.711       ,// dR      = Diameter of the tubes constituting the cage (also dR of the mother)  
      0.0         ,// Phi1    = Starting angle of the mother					     
      70.0        ,// Phi2    = Finishing angle of the mother					     
      0.711       ,// DiamOut = Outer diam of the carbon tube					     
      0.620        // DiamIn  = Inner diam of the carbon tube                                        
  };
#ifdef DEBUG
  cout << "SVTG ====================" << endl;
  cout << " Version   = " << SVTG.Version    << " geometry Version				  " << endl;
  cout << " Nlayer    = " << SVTG.Nlayer     << " number of svt layers (was 7)			  " << endl;
  cout << " RsizeMin  = " << SVTG.RsizeMin   << " STV innermost radius				  " << endl;
  cout << " RsizeMax  = " << SVTG.RsizeMax   << " STV outermost radius				  " << endl;
  cout << " ZsizeMax  = " << SVTG.ZsizeMax   << " SVT+FTPC length				  " << endl;
  cout << " Angoff    = " << SVTG.Angoff     << " angular offset x1 for slayer 2 x2 for slayer 3  " << endl;
  cout << " SupportVer= " << SVTG.SupportVer << " Versioning of the shield			  " << endl;
  cout << " ConeVer   = " << SVTG.ConeVer    << " Versioning of the support cone		  " << endl;
  cout << " ifMany    = " << SVTG.ifMany     << " whether we use the geant MANY option		  " << endl;
  cout << " Nmin      = " << SVTG.Nmin       << " the index of the innermost layer                " << endl;
#endif  
#ifdef DEBUG
  cout << "SWCA ====================" << endl;
  cout << " Version   = " <<  SWCA.Version  << " geometry Version				       " << endl;
  cout << " Length    = " <<  SWCA.Length   << " ladder length					       " << endl;
  cout << " WaferWid  = " <<  SWCA.WaferWid << " wafer width					       " << endl;
  cout << " WaferLen  = " <<  SWCA.WaferLen << " wafer length					       " << endl;
  cout << " WaferThk  = " <<  SWCA.WaferThk << " wafer thickness				       " << endl;
  cout << " RohaThk   = " <<  SWCA.RohaThk  << " Roha cell plus glue thickness 			       " << endl;
  cout << " WafCarWd  = " <<  SWCA.WafCarWd << " wafer carrier rails width			       " << endl;
  cout << " WafCarTh  = " <<  SWCA.WafCarTh << " wafer carrier thickness 			       " << endl;
  cout << " WaferGap  = " <<  SWCA.WaferGap << " inter-wafer gap - there is a 50 micron uncertainty    " << endl;
  cout << " Drift     = " <<  SWCA.Drift    << " drift direction				       " << endl;
  cout << " strutlen  = " <<  SWCA.strutlen << " len (z) of strut between waf car. rails (approx)      " << endl;
  cout << " SensWid   = " <<  SWCA.SensWid  << " Define the sensitive square width  (new)	       " << endl;
  cout << " SensLen   = " <<  SWCA.SensLen  << " Define the sensitive square length (new)	       " << endl;
  cout << " SensGir   = " <<  SWCA.SensGir  << " Girth (for lack of a better word)                     " << endl;
#endif
#ifdef DEBUG
  cout << "SSUP ====================" << endl;
  cout << " Version   = " <<  SSUP.Version << " geometry Version				   " << endl;
  cout << " CabThk    = " <<  SSUP.CabThk  << " thickness of layer of cables on support cone	   " << endl;
  cout << " HosRmn    = " <<  SSUP.HosRmn  << " inner radius of water hoses on support cone	   " << endl;
  cout << " HosRmx    = " <<  SSUP.HosRmx  << " outer radius of water hoses on support cone	   " << endl;
  cout << " Nhoses    = " <<  SSUP.Nhoses  << " number of water hoses				   " << endl;
  cout << " WrpMyThk  = " <<  SSUP.WrpMyThk<< " thickness of mylar wrap around cone (guess)	   " << endl;
  cout << " WrpAlThk  = " <<  SSUP.WrpAlThk<< " thickness of Al on mylar wrap (guess)		   " << endl;
  cout << " GrphThk   = " <<  SSUP.GrphThk << " support cone thickness			   " << endl;
  cout << " Cone1zmn  = " <<  SSUP.Cone1Zmn<< " Cone z min (parts 1,2,3,4 in increasing z)	   " << endl;
  cout << " RodLen    = " <<  SSUP.RodLen  << " Length of support rods			   " << endl;
  cout << " RodDist   = " <<  SSUP.RodDist << " Distance of support rod od from beam axis 	   " << endl;
  cout << " RodID     = " <<  SSUP.RodID   << "	ID of Carbon support rods (approx)		   " << endl;
  cout << " RodOD     = " <<  SSUP.RodOD   << "	OD of Carbon support rods (approx)		   " << endl;
  cout << " Con1IdMn  = " <<  SSUP.Con1IdMn<< " Minimum id of cone 1 				   " << endl;
  cout << " Con3IdMn  = " <<  SSUP.Con3IdMn<< " Minimum id of cone 3 (TBD)			   " << endl;
  cout << " Con4IdMn  = " <<  SSUP.Con4IdMn<< " Minimum id of cone 4 (TBD)			   " << endl;
  cout << " Con4IdMx  = " <<  SSUP.Con4IdMx<< " Maximum id of cone 4 (TBD)			   " << endl;
  cout << " Cone3zmx  = " <<  SSUP.Cone3zmx<< " Maximum z of cone 3 (TBD)			   " << endl;
  cout << " Cone4zmx  = " <<  SSUP.Cone4zmx<< " Maximum z of cone 4 (TBD)			   " << endl;
  cout << " BraThk    = " <<  SSUP.BraThk  << " thickness of Al brackets 			   " << endl;
  cout << " ERJThk    = " <<  SSUP.ERJThk  << " (z) thickness of end ring joining brackets	   " << endl;
  cout << " ERJWid    = " <<  SSUP.ERJWid  << " (azimuthal) width of end ring joining brackets   " << endl;
  cout << " ERJLen    = " <<  SSUP.ERJLen  << " (radial) length of end ring joining brackets	   " << endl;
  cout << " ERJzdis   = " <<  SSUP.ERJzdis << " dist from ladder ends to ERJ (guess)		   " << endl;
  cout << " ERJ1x     = " <<  SSUP.ERJ1x   << "	ERJ screw 1 x position (radial)		   " << endl;
  cout << " ERJ2x     = " <<  SSUP.ERJ2x   << "	ERJ screw 2 x position			   " << endl;
  cout << " ERJ2y     = " <<  SSUP.ERJ2y   << "	ERJ screw 2 y position			   " << endl;
  cout << " ERJrad    = " <<  SSUP.ERJrad  << " distance of ERJ center from beam axis		   " << endl;
  cout << " ERJdia    = " <<  SSUP.ERJdia  << " ERJ screw diameter                               " << endl;
#endif
#ifdef DEBUG
  cout << "SSUB ====================" << endl;
  cout << " Version   = " <<   SSUB.Version << "   geometry Version			   " << endl;
  cout << " KMountId  = " <<   SSUB.KMountId << "  id of beampipe support kinematic mount	   " << endl;
  cout << " KMountOd  = " <<   SSUB.KMountOd << "  od of beampipe support kinematic mount	   " << endl;
  cout << " KMntThk   = " <<   SSUB.KMntThk << "   thickness of support kinematic mount	   " << endl;
  cout << " KMCutOd   = " <<   SSUB.KMCutOd << "   od of cutout in kinematic mount	   " << endl;
  cout << " KMCutId   = " <<   SSUB.KMCutId << "   id of cutout in kinematic mount	   " << endl;
  cout << " KMCutOA   = " <<   SSUB.KMCutOA << "   opening angle of cutout		   " << endl;
  cout << " KMCutOff  = " <<   SSUB.KMCutOff << "  offset of cutout center from axis	   " << endl;
  cout << " SRingId   = " <<   SSUB.SRingId << "   id of beampipe support ring 		   " << endl;
  cout << " SRingOd   = " <<   SSUB.SRingOd << "   od of beampipe support ring		   " << endl;
  cout << " SRingThk  = " <<   SSUB.SRingThk << "  thichkness of beampipe support ring	   " << endl;
  cout << " SRCutPhi  = " <<   SSUB.SRCutPhi << "  support ring cutout angle to z-axis	   " << endl;
  cout << " SRCutWid  = " <<   SSUB.SRCutWid << "  support ring cutout width		   " << endl;
  cout << " SRCutOut  = " <<   SSUB.SRCutOut << "  support ring cutout depth		   " << endl;
  cout << " SRCutIn   = " <<   SSUB.SRCutIn << "   support ring cutout start		   " << endl;
  cout << " SRollId   = " <<   SSUB.SRollId << "   support roller Id			   " << endl;
  cout << " SRollOd   = " <<   SSUB.SRollOd << "   support roller Od			   " << endl;
  cout << " SRollLen  = " <<   SSUB.SRollLen << "  support roller length			   " << endl;
  cout << " SWireLen  = " <<   SSUB.SWireLen << "  support roller axis length		   " << endl;
  cout << " MBlkHgh   = " <<   SSUB.MBlkHgh << "   mounting block height			   " << endl;
  cout << " MBlkOWid  = " <<   SSUB.MBlkOWid << "  outer mounting block width		   " << endl;
  cout << " MBlkOLen  = " <<   SSUB.MBlkOLen << "  outer mounting block length		   " << endl;
  cout << " MBlkIWid  = " <<   SSUB.MBlkIWid << "  inner mounting block width		   " << endl;
  cout << " MBlkILen  = " <<   SSUB.MBlkILen << "  inner mounting block length		   " << endl;
  cout << " MBlkORad  = " <<   SSUB.MBlkORad << "  outer mounting block at radius		   " << endl;
  cout << " MBlkIRad  = " <<   SSUB.MBlkIRad << "  inner mounting block at radius		   " << endl;
  cout << " MRodDia   = " <<   SSUB.MRodDia << "   mounting support rod diameter             " << endl;
 #endif
#ifdef DEBUG
  cout << "SWAM ===========================" << endl; 
  cout << " Version  = " <<  SWAM.Version << "  geometry Version					" << endl;
  cout << " Zmin     = " <<  SWAM.Zmin << "     minimum z for positioning water manifold		" << endl;
  cout << " Len      = " <<  SWAM.Len << "      water manifold full length 			" << endl;
  cout << " Rmin     = " <<  SWAM.Rmin << "     water manifold rmin (not including trans. brds)	" << endl;
  cout << " Rmax     = " <<  SWAM.Rmax << "     water manifold rmax (not including trans. brds)	" << endl;
  cout << " TbrdThk  = " <<  SWAM.TbrdThk << "  transition board thickness			" << endl;
  cout << " WallThk  = " <<  SWAM.WallThk << "  water manifold wall thickness                     " << endl;
#endif 
#ifdef DEBUG
  cout << "SERG ===========================" << endl;
  cout << " Version   = " <<  SERG.Version << "   geometry Version			   " << endl;
  cout << " IrngTrMx  = " <<  SERG.IrngTrMx << "  Inner end ring tube maximum radius 	   " << endl;
  cout << " IrngPrMn  = " <<  SERG.IrngPrMn << "  Inner end ring polygon minimum radius  " << endl;
  cout << " OrngRmin  = " <<  SERG.OrngRmin << "  Outer end ring minimum radius	   " << endl;
  cout << " OrngRmax  = " <<  SERG.OrngRmax << "  Outer end ring maximum radius	   " << endl;
  cout << " EndRngTh  = " <<  SERG.EndRngTh << "  End ring thickness			   " << endl;
  cout << " EndRngZm  = " <<  SERG.EndRngZm << "  minimum z for end rings                " << endl;
#endif
#ifdef DEBUG
  cout << "SELC ===========================" << endl;
  cout << " Version  = " <<   SELC.Version << "  geometry Version				     " << endl;
  cout << " BeThk    = " <<   SELC.BeThk << "    thickness of Berillium water channel walls	     " << endl;
  cout << " WatThk   = " <<   SELC.WatThk << "   thickness of water channel			     " << endl;
  cout << " BeOThk   = " <<   SELC.BeOThk << "   thickness of Berrillia (BeO) substra		     " << endl;
  cout << " DyeThk   = " <<   SELC.DyeThk << "   to give .11% of a radiation length of Si 	     " << endl;
  cout << " DyeWid   = " <<   SELC.DyeWid << "   width of ic chips (2 covers 0.3 of area)	     " << endl;
  cout << " DyeSpc   = " <<   SELC.DyeSpc << "   space ic chips				     " << endl;
  cout << " ElcaWid  = " <<   SELC.ElcaWid << "  electronics carrier width			     " << endl;
  cout << " AgPdThk  = " <<   SELC.AgPdThk << "  thickness of Ag-Pd conductor			     " << endl;
  cout << " GlassThk = " <<   SELC.GlassThk << " thickness of glass insulator			     " << endl;
  cout << " CabThk   = " <<   SELC.CabThk << "   Cu for cables of .23% rad len (weighted average)  " << endl;
  cout << " CabWid   = " <<   SELC.CabWid << "   cable width                                       " << endl;
#endif
#ifdef DEBUG
  cout << "SVTL[6] ===========================" << endl;
  Int_t l;
  for (l = 0; l < 6; l++) {
    cout << " layer    = " <<   SVTL[l].Layer << "    layer number						      " << endl;
    cout << " Nladder  = " <<   SVTL[l].Nladder << "  number of ladder					      " << endl;
    cout << " Nwafer   = " <<   SVTL[l].Nwafer << "   number of wafers					      " << endl;
    cout << " Radius   = " <<   SVTL[l].Radius << "   layer radius (center of wafer position)		      " << endl;
    cout << " BareEdge = " <<   SVTL[l].BareEdge << " the strip of bare Be which makes the PCB shorter	      " << endl;
    cout << " PcbLen   = " <<   SVTL[l].PcbLen << "   PCB Length						      " << endl;
    cout << " PcbWidth = " <<   SVTL[l].PcbWidth << " PCB Width						      " << endl;
    cout << " PcbThk   = " <<   SVTL[l].PcbThk << "   PCB Thickness -- should be 0.09, but we add extra for copper " << endl;
    cout << " PcbGap   = " <<   SVTL[l].PcbGap << "   Gap between the PCB and the Si Wafer                         " << endl;
  }
#endif
#ifdef DEBUG
  cout << "SSLD ===========================" << endl;
  cout << " Version  = " <<   SSLD.Version << "  geometry Version			  " << endl;
  cout << " SInRInn  = " <<   SSLD.SInRInn << "  inner shield cylinder, inner radius	  " << endl;
  cout << " SInROut  = " <<   SSLD.SInROut << "  inner shield cylinder, outer radius	  " << endl;
  cout << " SInLen   = " <<   SSLD.SInLen << "   inner shield cylinder, half length	  " << endl;
  cout << " SSepRInn = " <<   SSLD.SSepRInn << " separation shield cylinder, inner radius  " << endl;
  cout << " SSepROut = " <<   SSLD.SSepROut << " separation shield cylinder, outer radius  " << endl;
  cout << " SSepLen  = " <<   SSLD.SSepLen << "  separation shield cylinder, half length	  " << endl;
  cout << " SOutRInn = " <<   SSLD.SOutRInn << " outer shield cylinder, inner radius	  " << endl;
  cout << " SOutROut = " <<   SSLD.SOutROut << " outer shield cylinder, outer radius	  " << endl;
  cout << " SOutLen  = " <<   SSLD.SOutLen << "  outer shield cylinder, half length	  " << endl;
  cout << " AlMeshId = " <<   SSLD.AlMeshId << " Aluminum shield mesh inner diameter	  " << endl;
  cout << " AlMeshOd = " <<   SSLD.AlMeshOd << " Aluminum shield mesh outer diameter	  " << endl;
  cout << " AlMshThk = " <<   SSLD.AlMshThk << " Aluminum shield mesh effective thickness  " << endl;
  cout << " AlMshPos = " <<   SSLD.AlMshPos << " Aluminum shield mesh z position           " << endl;
#endif
#ifdef DEBUG
  cout << "SCBP[4] ===========================" << endl;
  for (l = 0; l < 4; l++) {
    cout << " Layer = " <<   SCBP[l].Layer << " Layer				" << endl;
    cout << " Len   = " <<   SCBP[l].Len << "   Length			" << endl;
    cout << " Rmin1 = " <<   SCBP[l].Rmin1 << " Min radius closer to wafers	" << endl;
    cout << " Rmin2 = " <<   SCBP[l].Rmin2 << " Min radius further from wafers" << endl;
    cout << " Vol   = " <<   SCBP[l].Vol << "   Volume of copper, LV+HV cables" << endl;
  }
#endif
#ifdef DEBUG
  cout << "SFEP[3] ===========================" << endl;
  for (l = 0; l < 3; l++) {
    cout << " Layer     = " << SFEP[l].Layer << "    Layer " << endl;
    cout << " Len       = " << SFEP[l].Len << "      Length" << endl;
    cout << " Rmin1     = " << SFEP[l].Rmin1 << "    Min radius closer to wafers   " << endl;
    cout << " Rmin2     = " << SFEP[l].Rmin2 << "    Min radius further from wafers" << endl;
    cout << " Vol       = " << SFEP[l].Vol << "      Volume of water	     " << endl;
    cout << " VolPlast  = " << SFEP[l].VolPlast << " Volume of plastic" << endl;
  }
#endif
#ifdef DEBUG
  cout << "SWCX ===========================" << endl;
  cout << " Version = " <<   SWCX.Version << "  Version					    " << endl;
  cout << " Length  = " <<   SWCX.Length << "   of the ring in the Z direction		    " << endl;
  cout << " dR      = " <<   SWCX.dR << " 	  thickness of the mother layer			    " << endl;
  cout << " rad     = " <<   SWCX.rad << " 	  inner plastic tube radius			    " << endl;
  cout << " offset  = " <<   SWCX.offset << "   from the edge of the ladder support, inward	    " << endl;
  cout << " rOffset = " <<   SWCX.rOffset << "  Radial offset					    " << endl;
  cout << " wall    = " <<   SWCX.wall << " 	  thickness of the plastic pipe wall                " << endl;
#endif
#ifdef DEBUG
  cout << "SOUP ===========================" << endl;
  cout << " Version = " <<  SOUP.Version << " Version								     " << endl;
  cout << " Length  = " <<  SOUP.Length << "  Length								     " << endl;
  cout << " Rout    = " <<  SOUP.Rout << "    Outer radius of the shield					     " << endl;
  cout << " dR      = " <<  SOUP.dR << "      Diameter of the tubes constituting the cage (also dR of the mother)  " << endl;
  cout << " Phi1    = " <<  SOUP.Phi1 << "    Starting angle of the mother					     " << endl;
  cout << " Phi2    = " <<  SOUP.Phi2 << "    Finishing angle of the mother					     " << endl;
  cout << " DiamOut = " <<  SOUP.DiamOut << " Outer diam of the carbon tube					     " << endl;
  cout << " DiamIn  = " <<  SOUP.DiamIn << "  Inner diam of the carbon tube                                        " << endl;
#endif
  //________________________________________________________________________________
  Double_t RsizeMax=SSUP.Con4IdMx;
  RsizeMax += SSUP.GrphThk+SSUP.CabThk+2.0*SSUP.HosRmx;
  RsizeMax += SSUP.WrpMyThk+SSUP.WrpAlThk;
  Double_t Clearance=SVTG.RsizeMax-RsizeMax;
  if (Clearance<0) cout << "SVTT max size error, Clearance= " << Clearance << endl;
  //  TGeoPcon *pcon = 0;
  TGeoPgon *pgon = 0;
  //________________________________________________________________________________  
  Double_t rmin = SVTG.RsizeMin;
  Double_t rmax = SVTG.RsizeMax;
  Double_t dz   = SVTG.ZsizeMax;
  Double_t Z    = 0;
  Double_t x = 0, y = 0, z = 0;
  Double_t phi1 = 0, phi2 = 0;
  // SVTT
  TGeoVolume *SVTT = gGeoManager->MakeTube("SVTT",GetMed("SVTT_AIR"),rmin, rmax,dz); 
  SVTT->SetTitle("the mother of all SVT volumes");
  SVTT->SetVisibility(0);
  // SVTT/SIRP
  TGeoVolume *SIRP = gGeoManager->MakePgon("SIRP",GetMed("SVTT_BERILLIUM"),0,360,8,2); 
  SIRP->SetTitle("the SVT inner end ring polycone (overlaps tube)");
  pgon = (TGeoPgon*) SIRP->GetShape();
  Double_t tx = TMath::Tan(TMath::Pi()/8.);
  Double_t sx = TMath::Sin(TMath::Pi()/8.);
  Double_t cx = TMath::Cos(TMath::Pi()/8.);
  Double_t rou=SERG.IrngTrMx/(cx + TMath::Sqrt(tx*tx - sx*sx));
  pgon->DefineSection(0,-SERG.EndRngTh/2,  SERG.IrngPrMn, rou);
  pgon->DefineSection(1, SERG.EndRngTh/2,  SERG.IrngPrMn, rou);
  pgon->DefineSection(1,0.1,7.671,8.964404);
  SIRP->SetLineColor(kRed);
  Z = SERG.EndRngZm+SERG.EndRngTh/2;
  SVTT->AddNode(SIRP,1,new TGeoCombiTrans(0,0, Z,GetRot("22.5")));
  SVTT->AddNode(SIRP,2,new TGeoCombiTrans(0,0,-Z,GetRot("22.5")));
  // SVTT/SIRT
  Double_t ir_rmin=SERG.IrngPrMn*(cx+TMath::Sqrt(tx*tx-sx*sx));
  rmin = ir_rmin;
  rmax=SERG.IrngTrMx;
  dz=SERG.EndRngTh/2;
  TGeoVolume *SIRT = gGeoManager->MakeTube("SIRT",GetMed("SVTT_BERILLIUM"),rmin,rmax,dz); 
  SIRT->SetTitle("the SVT inner end ring tube");
  SIRT->SetLineColor(kRed);
  Z=SERG.EndRngZm+SERG.EndRngTh/2;
  SVTT->AddNode(SIRT,1,new TGeoCombiTrans(0,0, Z,GetRot("22.5")));
  SVTT->AddNode(SIRT,2,new TGeoCombiTrans(0,0,-Z,GetRot("22.5")));
  // SVTT/SOER
  rmin=SERG.OrngRmin;
  rmax=SERG.OrngRmax;
  dz=SERG.EndRngTh/2;
  TGeoVolume *SOER = gGeoManager->MakeTube("SOER",GetMed("SVTT_BERILLIUM"),rmin,rmax,dz); 
  SOER->SetTitle("the SVT outer end ring");
  SOER->SetLineColor(kRed);
  Z=SERG.EndRngZm+SERG.EndRngTh/2;
  SVTT->AddNode(SOER,1,new TGeoTranslation(0,0, Z));
  SVTT->AddNode(SOER,2,new TGeoTranslation(0,0,-Z));
  // SVTT/SBRG
  rmin=SERG.IrngPrMn;
  rmax=SERG.OrngRmax;
  dz=SSUP.ERJThk/2;
  TGeoVolume *SBRG = gGeoManager->MakeTube("SBRG",GetMed("SVTT_AIR"),rmin,rmax,dz); 
  SBRG->SetTitle("the bracket joining the end rings");
  SBRG->SetVisibility(0);
  Z=  SWCA.Length/2+SSUP.ERJzdis+SSUP.ERJThk/2;
  SVTT->AddNode(SBRG,1,new TGeoTranslation(0,0, Z));
  SVTT->AddNode(SBRG,2,new TGeoTranslation(0,0,-Z));
  // SVTT/SBRG/SBRM
  TGeoVolume *SBRM = SBRG->Divide("SBRM",2,4,45,90); 
  SBRM->SetTitle("a the mother of a single bracket joining the end rings");
  // SVTT/SBRG/SBRM/SBRI
  Double_t dx=SSUP.ERJLen/2;
  Double_t dy=SSUP.ERJWid/2;
  dz=SSUP.ERJThk/2;
  TGeoVolume *SBRI = gGeoManager->MakeBox("SBRI",GetMed("SVTT_BERILLIUM"),dx,dy,dz); 
  SBRI->SetTitle("the bracket which joins the rings");
  SBRI->SetLineColor(kRed);
  x=SSUP.ERJrad;
  SBRM->AddNode(SBRI,1,new TGeoTranslation(x,0,0));
  // SVTT/SOES  
  // Mother volumes for the screws joining the bracket to the end ring
  Double_t endrng_z=SERG.EndRngZm+SERG.EndRngTh;
  Double_t brack_z=SWCA.Length/2+SSUP.ERJzdis;
  Double_t screw_z=endrng_z+0.5*(brack_z-endrng_z);
  rmin=SERG.OrngRmin; rmax=SERG.OrngRmax; dz=0.5*(brack_z-endrng_z);
  TGeoVolume *SOES = gGeoManager->MakeTube("SOES",GetMed("SVTT_AIR"),rmin,rmax,dz); 
  SOES->SetTitle("the volume to hold outer endring screws");
  SOES->SetVisibility(0);
  Z = screw_z;
  SVTT->AddNodeOverlap(SOES,1,new TGeoTranslation(0,0, Z));
  SVTT->AddNodeOverlap(SOES,2,new TGeoTranslation(0,0,-Z));
  // SVTT/SIES
  rmin=ir_rmin;  rmax=SERG.IrngTrMx; dz=0.5*(brack_z-endrng_z);
  TGeoVolume *SIES = gGeoManager->MakeTube("SIES",GetMed("SVTT_AIR"),rmin,rmax,dz); 
  SIES->SetTitle("the volume to hold inner endring screws");
  SIES->SetVisibility(0);
  SVTT->AddNodeOverlap(SIES,1,new TGeoTranslation(0,0, Z));
  SVTT->AddNodeOverlap(SIES,2,new TGeoTranslation(0,0,-Z));
  TGeoVolume *SISM = SIES->Divide("SISM",2,4,45,90); 
  SISM->SetTitle("Mother volume for inner end ring screws");
  TGeoVolume *SOSM = SOES->Divide("SOSM",2,4,45,90); 
  SOSM->SetTitle("the mother volume division for the outer end ring screws");
  // SVTT/SIES/SISM/SCRW && SVTT/SOES/SOSM/SCRW
  TGeoVolume *SCRW = gGeoManager->MakeTube("SCRW",GetMed("SVTT_BERILLIUM"),0,SSUP.ERJdia/2,0.5*(brack_z-endrng_z)); 
  SCRW->SetTitle("the screw which attaches the end ring to the end ring bracket");
  SCRW->SetLineColor(kRed);
  x=SSUP.ERJrad-SSUP.ERJLen/2+SSUP.ERJ1x;
  SISM->AddNode(SCRW,1,new TGeoTranslation(x,0,0));
  x=SSUP.ERJrad+SSUP.ERJLen/2-SSUP.ERJ1x;
  SOSM->AddNode(SCRW,1,new TGeoTranslation(x,0,0));
  x=SSUP.ERJrad+SSUP.ERJLen/2-SSUP.ERJ2x;
  SOSM->AddNode(SCRW,2,new TGeoTranslation(x,0,0));
  x=SSUP.ERJrad+SSUP.ERJLen/2-SSUP.ERJ2x; y=SSUP.ERJ2y;
  SOSM->AddNode(SCRW,3,new TGeoTranslation(x,y,0));
  x=SSUP.ERJrad+SSUP.ERJLen/2-SSUP.ERJ2x; y=-SSUP.ERJ2y;
  SOSM->AddNode(SCRW,4,new TGeoTranslation(x,y,0));
  // SVTT/SWMM
  // Water manifold
  if (SWAM.Len > 0) {
    rmin=SWAM.Rmin-SWAM.TbrdThk;
    rmax=SWAM.Rmax+SWAM.TbrdThk;
    dz = SWAM.Len/2;
    TGeoVolume *SWMM = gGeoManager->MakePgon("SWMM",GetMed("SVTT_AIR"),0,360,18,2); 
    SWMM->SetTitle("the water manifold mother");
    pgon = (TGeoPgon*)SWMM->GetShape();
    pgon->DefineSection(0,-dz,rmin,rmax);
    pgon->DefineSection(1, dz,rmin,rmax);
    SWMM->SetVisibility(0);
    z= SWAM.Zmin+SWAM.Len/2;
    SVTT->AddNode(SWMM,1,new TGeoTranslation(0,0, z));
    SVTT->AddNode(SWMM,2,new TGeoTranslation(0,0,-z));
    // SVTT/SWMM/SWMB
    TGeoVolume *SWMB = gGeoManager->MakePgon("SWMB",GetMed("SVTT_ALUMINIUM"),0,360,18,2); 
    SWMB->SetTitle("the water manifold bottom piece (small r)");
    pgon = (TGeoPgon*)SWMB->GetShape();
    dz = SWAM.Len/2-SWAM.WallThk;
    rmin = SWAM.Rmin;
    rmax = SWAM.Rmin+SWAM.WallThk;
    pgon->DefineSection(0,-dz,rmin,rmax);
    pgon->DefineSection(1, dz,rmin,rmax);
    SWMB->SetLineColor(kRed);
    SWMM->AddNode(SWMB,1,gGeoIdentity);
    // SVTT/SWMM/SWMT
    TGeoVolume *SWMT = gGeoManager->MakePgon("SWMT",GetMed("SVTT_ALUMINIUM"),0,360,18,2); 
    SWMT->SetTitle("the water manifold top piece (big r)");
    pgon = (TGeoPgon*)SWMT->GetShape();
    dz =  SWAM.Len/2-SWAM.WallThk;
    rmin= SWAM.Rmax-SWAM.WallThk;
    rmax= SWAM.Rmax;
    pgon->DefineSection(0,-dz,rmin,rmax);
    pgon->DefineSection(1, dz,rmin,rmax);
    SWMT->SetLineColor(kRed);
    SWMM->AddNode(SWMT,1,gGeoIdentity);
    // SVTT/SWMM/SWMS
    TGeoVolume *SWMS = gGeoManager->MakePgon("SWMS",GetMed("SVTT_ALUMINIUM"),0,360,18,2); 
    SWMS->SetTitle("the water manifold side pieces");
    pgon = (TGeoPgon*)SWMS->GetShape();
    dz   = SWAM.WallThk/2;
    rmin = SWAM.Rmin;
    rmax = SWAM.Rmax;
    pgon->DefineSection(0,-dz,rmin,rmax);
    pgon->DefineSection(1, dz,rmin,rmax);
    SWMS->SetLineColor(kRed);
    z = SWAM.Len/2-SWAM.WallThk/2;
    SWMM->AddNode(SWMS,1,new TGeoTranslation(0,0,-z));
    SWMM->AddNode(SWMS,2,new TGeoTranslation(0,0, z));
    // SVTT/SWMM/SWMW
    TGeoVolume *SWMW = gGeoManager->MakePgon("SWMW",GetMed("SVTT_WATER"),0,360,18,2); 
    SWMW->SetTitle("the water in the water manifold");
    pgon = (TGeoPgon*)SWMW->GetShape();
    dz   = SWAM.Len/2-SWAM.WallThk;
    rmin = SWAM.Rmin+SWAM.WallThk;
    rmax = SWAM.Rmax-SWAM.WallThk;
    pgon->DefineSection(0,-dz,rmin,rmax);
    pgon->DefineSection(1, dz,rmin,rmax);
    SWMW->SetLineColor(kPink);
    SWMM->AddNode(SWMW,1,gGeoIdentity);
    // SVTT/SWMM/SOTB
    TGeoVolume *SOTB = gGeoManager->MakePgon("SOTB",GetMed("SVTT_G10"),0,360,18,2); 
    SOTB->SetTitle("the outer transition board (large r)");
    pgon = (TGeoPgon*)SOTB->GetShape();
    dz   = SWAM.Len/2;
    rmin = SWAM.Rmax;
    rmax = SWAM.Rmax+SWAM.TbrdThk;
    pgon->DefineSection(0,-dz,rmin,rmax);
    pgon->DefineSection(1, dz,rmin,rmax);
    SOTB->SetLineColor(kGreen);
    SWMM->AddNode(SOTB,1,gGeoIdentity);
    // SVTT/SWMM/SITB
    TGeoVolume *SITB = gGeoManager->MakePgon("SITB",GetMed("SVTT_CH2"),0,360,18,2); 
    SITB->SetTitle("the inner transition board (small r)");
    pgon = (TGeoPgon*)SITB->GetShape();
    dz   = SWAM.Len/2;
    rmin = SWAM.Rmin-SWAM.TbrdThk;
    rmax = SWAM.Rmin;
    pgon->DefineSection(0,-dz,rmin,rmax);
    pgon->DefineSection(1, dz,rmin,rmax);
    SITB->SetLineColor(kGreen);
    SWMM->AddNode(SITB,1,gGeoIdentity);
    // SVTT/SBWC
    rmin = SWAM.Rmin; rmax = SSUP.Con1IdMn; dz = (SSUP.Cone1Zmn-(SWAM.Zmin+SWAM.Len))/2;
    TGeoVolume *SBWC = gGeoManager->MakeTube("SBWC",GetMed("SVTT_AIR"),rmin,rmax,dz); 
    SBWC->SetTitle("the bracket connecting the water manifold to the cone");
    SBWC->SetVisibility(0);
    z= (SWAM.Zmin+SWAM.Len + (SSUP.Cone1Zmn-(SWAM.Zmin+SWAM.Len))/2);
    SVTT->AddNode(SBWC,1,new TGeoTranslation(0,0, z));
    SVTT->AddNode(SBWC,2,new TGeoCombiTrans(0,0, -z,GetRot("180R")));   
    // SVTT/SBWC/SWCM
    TGeoVolume *SWCM = SBWC->Divide("SWCM",2,3,0,120); 
    SWCM->SetTitle("a single bracket mother between mani and cone");
    // SVTT/SBWC/SWCM/SXAI
    rmin=SWAM.Rmin; rmax=SSUP.Con1IdMn;  dz=SSUP.BraThk/2;  phi1=-5;  phi2=5;
    TGeoVolume *SXAI = gGeoManager->MakeTubs("SXAI",GetMed("SVTT_ALUMINIUM"),rmin,rmax,dz,phi1,phi2); 
    SXAI->SetTitle("a first piece (A) of the bracket between mani and cone (X)");
    SXAI->SetLineColor(kRed);
    Z=(-(SSUP.Cone1Zmn-(SWAM.Zmin+SWAM.Len))/2 +SSUP.BraThk/2);
    SWCM->AddNode(SXAI,1,new TGeoTranslation(0,0,Z));
    // SVTT/SBWC/SWCM/SXBI
    rmin=SSUP.Con1IdMn-SSUP.BraThk; rmax=SSUP.Con1IdMn; dz=((SSUP.Cone1Zmn-(SWAM.Zmin+SWAM.Len)-SSUP.BraThk)/2);
    TGeoVolume *SXBI = gGeoManager->MakeTubs("SXBI",GetMed("SVTT_ALUMINIUM"),rmin,rmax,dz,phi1,phi2); 
    SXBI->SetTitle("a second piece (B) of the bracket between mani and cone (X)");
    SXBI->SetLineColor(kRed);
    Z=(SSUP.BraThk/2);
    SWCM->AddNode(SXBI,1,new TGeoTranslation(0,0,Z));
  }
  // SVTT/SBSP
  rmin=SVTG.RsizeMin; rmax=SSUB.KMountOd/2; dz=SSUB.KMntThk/2+SSUB.MBlkHgh;
  TGeoVolume *SBSP = gGeoManager->MakeTube("SBSP",GetMed("SVTT_AIR"),rmin,rmax,dz); 
  SBSP->SetTitle("the beampipe support mother volume");
  SBSP->SetVisibility(0);
  z = (SSUP.RodLen/2- SSUB.KMntThk/2);
  SVTT->AddNodeOverlap(SBSP,1,new TGeoTranslation(0,0, z));
  SVTT->AddNodeOverlap(SBSP,2,new TGeoCombiTrans(0,0, -z,GetRot("180R")));
  // SVTT/SBSP/SAKM
  rmin=SSUB.KMountId/2; rmax=SSUB.KMountOd/2; dz=SSUB.KMntThk/2;
  TGeoVolume *SAKM = gGeoManager->MakeTube("SAKM",GetMed("SVTT_ALUMINIUM"),rmin,rmax,dz); 
  SAKM->SetTitle("the beampipe support aluminum kinematic mount");
  SAKM->SetLineColor(kRed);
  SBSP->AddNode(SAKM,1,gGeoIdentity);
  // SVTT/SBSP/SAKM/SCKM  
  rmin=SSUB.KMCutId/2; rmax=SSUB.KMCutOd/2; dz=SSUB.KMntThk/2;
  phi1=270-SSUB.KMCutOA; phi2=270+SSUB.KMCutOA;
  TGeoVolume *SCKM = gGeoManager->MakeTubs("SCKM",GetMed("SVTT_AIR"),rmin,rmax,dz,phi1,phi2); 
  SCKM->SetTitle("the cutout in the aluminum kinematic mount");
  y=SSUB.KMCutOff;
  SAKM->AddNodeOverlap(SCKM,1,new TGeoTranslation(0, y,0));
  SAKM->AddNodeOverlap(SCKM,2,new TGeoCombiTrans(0,-y,0,GetRot("R180")));
  // SVTT/SBSP/SBMM
  dx=SSUB.MBlkIWid/2; dy=(SSUB.KMountOd/2-SVTG.RsizeMin-SSUB.SRollOd)/2; dz=SSUB.MBlkHgh/2;
  TGeoVolume *SBMM = gGeoManager->MakeBox("SBMM",GetMed("SVTT_AIR"),dx,dy,dz); 
  SBMM->SetTitle("the beampipe support mounting mother volume");
  SBMM->SetVisibility(0);
  // SVTT/SBSP/SBMM/SMRD
  rmin=0.0; rmax=SSUB.MRodDia/2; dz=(SSUB.MBlkORad-SSUB.MBlkIRad+SSUB.MBlkOLen)/2;
  TGeoVolume *SMRD = gGeoManager->MakeTube("SMRD",GetMed("SVTT_ALUMINIUM"),rmin,rmax,dz); 
  SMRD->SetTitle("the aluminum rod carrying the beampipe support");
  SMRD->SetLineColor(kRed);
  Double_t xbuf=-(SSUB.MBlkORad+SSUB.MBlkIRad)/2+SVTG.RsizeMin+SSUB.SRollOd;
  y=xbuf+(SSUB.KMountOd/2-SVTG.RsizeMin-SSUB.SRollOd)/2;
  SBMM->AddNode(SMRD,1,new TGeoCombiTrans(0,y,0,GetRot("D180")));
  // SVTT/SBSP/SBMM/SBMO
  dx=SSUB.MBlkOWid/2;
  dy=SSUB.MBlkOLen/2;
  dz=SSUB.MBlkHgh/2;
  TGeoVolume *SBMO = gGeoManager->MakeBox("SBMO",GetMed("SVTT_G10"),dx,dy,dz); 
  SBMO->SetTitle("the outer beampipe support mounting block");
  SBMO->SetLineColor(kGreen);
  xbuf=-SSUB.MBlkORad+SVTG.RsizeMin+SSUB.SRollOd;
  y=xbuf+(SSUB.KMountOd/2-SVTG.RsizeMin-SSUB.SRollOd)/2;
  SBMM->AddNode(SBMO,1,new TGeoTranslation(0,y,0));
  // SVTT/SBSP/SBMM/SBMI  
  dx=SSUB.MBlkIWid/2;
  dy=SSUB.MBlkILen/2;
  dz=SSUB.MBlkHgh/2;
  TGeoVolume *SBMI = gGeoManager->MakeBox("SBMI",GetMed("SVTT_G10"),dx,dy,dz); 
  SBMI->SetTitle("the inner beampipe support mounting block");
  SBMI->SetLineColor(kGreen);
  xbuf=-SSUB.MBlkIRad+SVTG.RsizeMin+SSUB.SRollOd;
  y=xbuf+(SSUB.KMountOd/2-SVTG.RsizeMin-SSUB.SRollOd)/2;
  SBMM->AddNode(SBMI,1,new TGeoTranslation(0,y,0));
  // SVTT/SBSP/SBRL
  rmin=SSUB.SRollId/2;  rmax=SSUB.SRollOd/2;  dy=SSUB.SRollLen/2;
  TGeoVolume *SBRL = gGeoManager->MakeTube("SBRL",GetMed("SVTT_PYREX"),rmin,rmax,dz); 
  SBRL->SetTitle("the ceramic roller supporting the beampipe");
  SBRL->SetLineColor(kPink);
  // SVTT/SBSP/SBRX  
  rmin=0.0; rmax=SSUB.SRollId/2;
  dz=SSUB.SWireLen/2;
  TGeoVolume *SBRX = gGeoManager->MakeTube("SBRX",GetMed("SVTT_IRON"),rmin,rmax,dz); 
  SBRX->SetTitle("the stainless steel roller axis");
  SBRX->SetLineColor(kRed);
  Double_t phi = 0;
  Int_t i,j;
  Int_t k = 0;
  TGeoRotation *rot;
  for (i = -1; i <= 1; i += 2) {
    for (j = 0; j <= 1; j++) {
      phi=i*SSUB.SRCutPhi+180*j;
      Double_t xbuf1=(SSUB.KMountOd/4.0+(SVTG.RsizeMin+SSUB.SRollOd)/2.0);
      rot = new TGeoRotation("next",90,phi,90,90+phi,0,0);
      x= xbuf1*TMath::Sin(degrad*phi);
      y=-xbuf1*TMath::Cos(degrad*phi);
      z=-SSUB.KMntThk/2-SSUB.MBlkHgh/2;
      k++;
      SBSP->AddNode(SBMM,k,new TGeoCombiTrans(x,y,z,rot));
      Double_t xbuf2=SVTG.RsizeMin+SSUB.SRollOd/2;
      x= xbuf2*TMath::Sin(degrad*phi);
      y=-xbuf2*TMath::Cos(degrad*phi);
      z=SSUB.SRingThk/2+SSUB.SRollId/2;
      rot = new TGeoRotation("next",0,0,90,phi-90,90,phi);
      SBSP->AddNode(SBRL,k,new TGeoCombiTrans(x,y,z,rot));
      SBSP->AddNode(SBRX,k,new TGeoCombiTrans(x,y,z,rot));
    }
  }
  // SVTT/SBSP/SBSR
  rmin=SSUB.SRingId/2;  rmax=SSUB.SRingOd/2;
  dz=SSUB.SRingThk/2;
  TGeoVolume *SBSR = gGeoManager->MakeTube("SBSR",GetMed("SVTT_G10"),rmin,rmax,dz); 
  SBSR->SetTitle("the beampipe support G10 ring");
  SBSR->SetLineColor(kGreen);
  SBSP->AddNode(SBSR,1,gGeoIdentity);
  // SVTT/SBSP/SBSR/SBCR
  dx=SSUB.SRCutWid/2;
  dy=(SSUB.SRCutOut-SSUB.SRCutIn)/2;
  dz=SSUB.SRingThk/2;
  TGeoVolume *SBCR = gGeoManager->MakeBox("SBCR",GetMed("SVTT_AIR"),dx,dy,dz); 
  SBCR->SetTitle("the cutout in the beampipe support G10 ring");
  xbuf=SSUB.SRCutIn+(SSUB.SRCutOut-SSUB.SRCutIn)/2;
  k = 0;
  for (i = -1; i <= 1; i += 2) {
    for (j = 0; j <= 1; j++) {
      k++;
      phi=i*SSUB.SRCutPhi+180*j;
      x=xbuf*TMath::Sin(degrad*phi);  y=-xbuf*TMath::Cos(degrad*phi);
      rot = new TGeoRotation("next",90,phi,90,90+phi,0,0);
      SBSR->AddNodeOverlap(SBCR,k,new TGeoCombiTrans(x,y,0,rot));
    }
  }
  // SVTT/SROD  
  rmin=SSUP.RodID/2;
  rmax=SSUP.RodOD/2;
  dz=SSUP.RodLen/2;
  TGeoVolume *SROD = gGeoManager->MakeTube("SROD",GetMed("SVTT_CARBON"),rmin,rmax,dz); 
  SROD->SetTitle("the SVT Be support rod");
  SROD->SetLineColor(kRed);
  y = SSUP.RodDist+SSUP.RodOD/2;
  SVTT->AddNode(SROD,1,new TGeoTranslation(0, y,0));
  SVTT->AddNode(SROD,2,new TGeoTranslation(0,-y,0));
  // SVTT/SISH
  rmin=SSLD.SInRInn; rmax=SSLD.SInROut; dz=SSLD.SInLen;
  TGeoVolume *SISH = gGeoManager->MakeTube("SISH",GetMed("SVTT_ALKAP"),rmin,rmax,dz); 
  SISH->SetTitle("the inner shield cylinder");
  SISH->SetLineColor(kGreen);
  SVTT->AddNode(SISH,1,gGeoIdentity);
  // SVTT/SOSH
  rmin=SSLD.SOutRInn; rmax=SSLD.SOutROut;
  dz=SSLD.SOutLen;
  TGeoVolume *SOSH = gGeoManager->MakeTube("SOSH",GetMed("SVTT_SSDAMY"),rmin,rmax,dz); 
  SOSH->SetTitle("the separation shield cylinder");
  SOSH->SetLineColor(kGreen);
  SVTT->AddNodeOverlap(SOSH,1,gGeoIdentity);
  // SVTT/SALM
  rmin=SSLD.AlMeshId/2; rmax=SSLD.AlMeshOd/2;
  dz=SSLD.AlMshThk/2;
  TGeoVolume *SALM = gGeoManager->MakeTube("SALM",GetMed("SVTT_ALUMINIUM"),rmin,rmax,dz); 
  SALM->SetTitle("the aluminum shield mesh");
  SALM->SetLineColor(kRed);
  z=  SSLD.AlMshPos-SSLD.AlMshThk/2;
  SVTT->AddNode(SALM,1,new TGeoTranslation(0,0, z));
  SVTT->AddNode(SALM,2,new TGeoTranslation(0,0,-z));
  // SVTT/SSSH
  if (SVTG.Nlayer>6) {
    rmin=SSLD.SSepRInn; rmax=SSLD.SSepROut;
    dz=SSLD.SSepLen;
    TGeoVolume *SSSH = gGeoManager->MakeTube("SSSH",GetMed("SVTT_SSDALMY"),rmin,rmax,dz); 
    SSSH->SetTitle("the separation shield cylinder");
    SSSH->SetLineColor(kGreen);
    SVTT->AddNodeOverlap(SSSH,1,gGeoIdentity);
  }
  // SVTT/SCBM
  rmin=radii[0]; rmax=SWAM.Rmax; 
  TGeoVolumeMulti *SCBM = gGeoManager->MakeVolumeMulti("SCBM",GetMed("SVTT_AIR"));
  // The bundles of cables connecting PCBs with the transition boards:
  // SVTT/SCBM/SCBL
  TGeoVolumeMulti *SCBL = gGeoManager->MakeVolumeMulti("SCBL",GetMed("SVTT_COPPER"));
  SCBL->SetTitle("the bundle of cables going from PCBs to manifolds");
  SCBL->SetLineColor(kGreen);
  Double_t sq, A, CuThk;
  Int_t ilayer = 0;
  Double_t rmn1;
  Double_t rmx1;
  Double_t rmn2;
  Double_t rmx2;
  for (ilayer = 1; ilayer <= 4; ilayer++) {
    dz=SCBP[ilayer-1].Len;
    TGeoVolume *scbm = gGeoManager->MakeTube("SCBM",GetMed("SVTT_AIR"),rmin,rmax,dz); 
    scbm->SetTitle("the mother for the bundle of cables going from PCBs");
    scbm->SetVisibility(0);
    SCBM->AddVolume(scbm);
    z = SWAM.Zmin-SCBP[ilayer-1].Len;
    SVTT->AddNode(scbm,2*scbm->GetUniqueID()-1,new TGeoTranslation(0,0,z));
    SVTT->AddNode(scbm,2*scbm->GetUniqueID()  ,new TGeoCombiTrans(0,0,-z,GetRot("180R")));
    Double_t dr = SCBP[ilayer-1].Rmin2-SCBP[ilayer-1].Rmin1;
    sq=SCBP[ilayer-1].Len*SCBP[ilayer-1].Len/(dr*dr);
    A=TMath::Pi()*(SCBP[ilayer-1].Rmin1*SCBP[ilayer-1].Rmin1 +
		   SCBP[ilayer-1].Rmin2*SCBP[ilayer-1].Rmin2)*TMath::Sqrt(1+sq);
    CuThk=(SCBP[ilayer-1].Vol/A)*TMath::Sqrt(1.0+1.0/sq);
    dz  =SCBP[ilayer-1].Len;
    Double_t rmn1=SCBP[ilayer-1].Rmin1;
    Double_t rmx1=SCBP[ilayer-1].Rmin1+CuThk;
    Double_t rmn2=SCBP[ilayer-1].Rmin2;
    Double_t rmx2=SCBP[ilayer-1].Rmin2+CuThk;
    TGeoVolume *scbl = gGeoManager->MakeCone("SCBL",GetMed("SVTT_COPPER"),dz,rmn1,rmx1,rmn2,rmx2);
    scbl->SetTitle("the bundle of cables going from PCBs to manifolds");
    scbl->SetLineColor(kGreen);
    SCBL->AddVolume(scbl);
    scbm->AddNode(scbl,scbl->GetUniqueID(),gGeoIdentity);
  }
  // SVTT/SCBM/SFED
  TGeoVolumeMulti *SFED = gGeoManager->MakeVolumeMulti("SFED",GetMed("SVTT_WATER"));
  SFED->SetTitle("the watrer in the bundle of pipes going to manifolds");
  // SVTT/SCBM/SPLS
  TGeoVolumeMulti *SPLS = gGeoManager->MakeVolumeMulti("SPLS",GetMed("SVTT_CH2"));
  SPLS->SetTitle("the plastic walls of the bundle of pipes going to manifolds");
  for (ilayer = 1; ilayer <= 3; ilayer++) {
    Double_t r1 = SFEP[ilayer-1].Rmin1;
    Double_t r2 = SFEP[ilayer-1].Rmin2;
    Double_t dr = r2-r1;
    sq=SFEP[ilayer-1].Len*SFEP[ilayer-1].Len/(dr*dr);
    A=TMath::Pi()*(r1*r1+r2*r2)*TMath::Sqrt(1+sq);
    CuThk=(SFEP[ilayer-1].Vol/A)*TMath::Sqrt(1.0+1.0/sq);
    dz  =SFEP[ilayer-1].Len;
    rmn1=r1;
    rmx1=r1+CuThk;
    rmn2=r2;
    rmx2=r2+CuThk;
    TGeoVolume *sfed = gGeoManager->MakeCone("SFED",GetMed("SVTT_WATER"),dz,rmn1,rmx1,rmn2,rmx2);
    sfed->SetTitle("the bundle of cables going from PCBs to manifolds");
    SFED->AddVolume(sfed);
    SCBM->AddNode(sfed,sfed->GetUniqueID(),gGeoIdentity);
  
    sq=SFEP[ilayer-1].Len*SFEP[ilayer-1].Len/(dr*dr);
    A=TMath::Pi()*(r1*r1+r2*r2)*TMath::Sqrt(1+sq);
    CuThk=(SFEP[ilayer-1].Vol/A)*TMath::Sqrt(1.0+1.0/sq);
    dz  =SFEP[ilayer-1].Len;
    rmn1=r1+0.2;
    rmx1=r1+0.2+CuThk;
    rmn2=r2+0.2;
    rmx2=r2+0.2+CuThk;
    TGeoVolume *spls = gGeoManager->MakeCone("SPLS",GetMed("SVTT_CH2"),dz,rmn1,rmx1,rmn2,rmx2);
    spls->SetTitle("the plastic walls of the bundle of pipes going to manifolds");
    spls->SetLineColor(kPink);
    SPLS->AddVolume(spls);
    SCBM->AddNode(spls,spls->GetUniqueID(),gGeoIdentity);
  }
  // SVTT/SXRL
  TGeoVolumeMulti *SXRL = gGeoManager->MakeVolumeMulti("SXRL",GetMed("SVTT_AIR"));
  TGeoVolume *SWRP = 0;
  TGeoVolume *SYRU = 0;
  Int_t ncopy = 0;
  for (ilayer = 4; ilayer <= 2; ilayer -= 2) {
    rmin=SWCX.rOffset+SVTL[ilayer-1].Radius;
    rmax=SWCX.rOffset+SVTL[ilayer-1].Radius+SWCX.dR;
    dz=SWCX.Length/2.0;
    TGeoVolume *sxrl = gGeoManager->MakeTube("SXRL",GetMed("SVTT_AIR"),rmin,rmax,dz);
    sxrl->SetTitle("the mother of the circular water pipes");
    sxrl->SetVisibility(0);
    SXRL->AddVolume(sxrl);
    //    SVTT->AddNode(sxrl,sxrl->GetUniqueID(),gGeoIdentity);
    z= SWCA.Length/2.0-SWCX.Length/2.0-SWCX.offset;
    SVTT->AddNodeOverlap(sxrl,1,new TGeoTranslation(0,0, z));
    SVTT->AddNodeOverlap(sxrl,2,new TGeoTranslation(0,0,-z));
    if (! SWRP) {
      rmin=0.0; rmax=SWCX.rad; dz=SWCX.Length/2.0;
      SWRP = gGeoManager->MakeTube("SWRP",GetMed("SVTT_WATER"),rmin,rmax,dz); 
      SWRP->SetTitle("an approximation of water in the circular pipe, a rectangular one");
      SWRP->SetLineColor(kBlue);
    }
    if (! SYRU) {
      rmin=SWCX.rad; rmax=SWCX.rad+SWCX.wall; dz=SWCX.Length/2.0;
      SYRU = gGeoManager->MakeTube("SYRU",GetMed("SVTT_CH2"),rmin,rmax,dz); 
      SYRU->SetTitle("the  wall of the water pipe");
      SYRU->SetLineColor(kBlue);
    }
    for (Int_t i_phi=1; i_phi <= 4*SVTL[ilayer-1].Nladder; i_phi++) {
      Double_t tube_angle=(TMath::Pi()/(2.0*SVTL[ilayer-1].Nladder))*(i_phi-0.5);
      ncopy++;
      x=TMath::Cos(tube_angle)*(SVTL[ilayer-1].Radius+SWCX.rOffset+SWCX.dR/2.0);
      y=TMath::Sin(tube_angle)*(SVTL[ilayer-1].Radius+SWCX.rOffset+SWCX.dR/2.0);
      z=0.0;
      sxrl->AddNode(SWRP,ncopy,new TGeoTranslation(x,y,z));
      x=TMath::Cos(tube_angle)*(SVTL[ilayer-1].Radius+SWCX.rOffset+SWCX.dR/2.0);
      y=TMath::Sin(tube_angle)*(SVTL[ilayer-1].Radius+SWCX.rOffset+SWCX.dR/2.0);
      z=0.0;
      sxrl->AddNode(SYRU,1,new TGeoTranslation(x,y,z));
    }
  }
  // SVTT/SOUM
  rmin=SOUP.Rout-SOUP.dR; rmax=SOUP.Rout;
  dz=SOUP.Length/2.0; phi1=SOUP.Phi1; phi2=SOUP.Phi2;
  TGeoVolume *SOUM = gGeoManager->MakeTubs("SOUM",GetMed("SVTT_AIR"),rmin,rmax,dz,phi1,phi2);
  SOUM->SetTitle("the mother of the array of the outer shileding support tubes");
  SOUM->SetVisibility(0);
  SVTT->AddNode(SOUM,1,gGeoIdentity);
  SVTT->AddNode(SOUM,2,GetRot("P270"));
  SVTT->AddNode(SOUM,3,GetRot("N270"));
  SVTT->AddNode(SOUM,4,GetRot("R180"));
  // SVTT/SOUM/SOUR
  rmin=SOUP.DiamIn/2.0;
  rmax=SOUP.DiamOut/2.0;
  dz=SOUP.Length/2.0;
  TGeoVolume *SOUR = gGeoManager->MakeTube("SOUR",GetMed("SVTT_CARBON"),rmin,rmax,dz);
  SOUR->SetTitle("the outer shileding support tubes (rods)");
  SOUR->SetLineColor(kBlue);
  for (Int_t i_phi = 1; i_phi <= 4; i_phi++) {
    x=TMath::Cos(shield_phi[i_phi-1]*TMath::Pi()/180.0)*(SOUP.Rout-SOUP.dR/2.0);
    y=TMath::Sin(shield_phi[i_phi-1]*TMath::Pi()/180.0)*(SOUP.Rout-SOUP.dR/2.0);
    z=0.0;
    SOUM->AddNode(SOUR,i_phi,new TGeoTranslation(x, y, z));
  }
  // SVTT/ClamShell/Layer
  TGeoVolumeAssembly *ClamShells[2] = {0,0};
  //__________ The SVT layers ______________________________________________________________________
  Double_t radmax=SVTG.RsizeMax;
  TGeoVolumeMulti *SLDI = gGeoManager->MakeVolumeMulti("SLDI",GetMed("SVTT_AIR")); 
  SLDI->SetTitle("a ladder volume");
  TGeoVolume *SBER = 0;
  TGeoVolume *STAB = 0;
  TGeoVolume *STRU = 0;
  TGeoVolumeMulti *SRHC = gGeoManager->MakeVolumeMulti("SRHC",GetMed("SVTT_GLASS")); 
  SRHC->SetTitle("the roha cell wafer support");
  TGeoVolume *SRHCs[3] = {0,0,0};
  TGeoVolumeMulti *SPCB = gGeoManager->MakeVolumeMulti("SPCB",GetMed("SVTT_G10")); SPCB->SetTitle("the G10 PCB");
  TGeoVolume *SPCBs[3] = {0,0,0};
  TGeoVolumeMulti *SELE = gGeoManager->MakeVolumeMulti("SELE",GetMed("SVTT_AIR"));
  SELE->SetTitle("the electronics mother volume");

  dx=SELC.ElcaWid/2;
  dz=SWCA.Length/2;
  dy=SELC.BeThk/2;
  TGeoVolume *SWCH = gGeoManager->MakeBox("SWCH",GetMed("SVTT_CARBON"),dx,dy,dz); 
  SWCH->SetTitle("the Be top and bottom of the water channel");
  SWCH->SetLineColor(kRed);
  
  dx=SELC.BeThk/2;
  dz=SWCA.Length/2;
  dy=SELC.WatThk/2;
  TGeoVolume *SWCS = gGeoManager->MakeBox("SWCS",GetMed("SVTT_CARBON"),dx,dy,dz); 
  SWCS->SetTitle("the Be side of the water channel");
  SWCS->SetLineColor(kRed);
  
  dx=(SELC.ElcaWid-2.0*SELC.BeThk)/2;
  dz=SWCA.Length/2;
  dy=(SELC.WatThk/2);
  TGeoVolume *SWCW = gGeoManager->MakeBox("SWCW",GetMed("SVTT_WATER"),dx,dy,dz); 
  SWCW->SetTitle("the water channel water (probably Evian?)");
  SWCW->SetLineColor(kPink);
  
  TGeoVolumeMulti *SBOI = gGeoManager->MakeVolumeMulti("SBOI",GetMed("SVTT_BEO"));
  SBOI->SetTitle("the Berillia layer");
  SBOI->SetLineColor(kPink);
  
  TGeoVolumeMulti *SGLA = gGeoManager->MakeVolumeMulti("SGLA",GetMed("SVTT_GLASS"));
  SGLA->SetTitle("Glass insulating plane");
  SGLA->SetLineColor(kPink);
  
  TGeoVolumeMulti *SAGP = gGeoManager->MakeVolumeMulti("SAGP",GetMed("SVTT_AGPD"));
  SAGP->SetTitle("the Silver-Palladium plane");
  SAGP->SetLineColor(kRed);
  
  TGeoVolumeMulti *SDYE = gGeoManager->MakeVolumeMulti("SDYE",GetMed("SVTT_SILICON"));
  SDYE->SetTitle("the ic chip on the hybrid");
  SDYE->SetLineColor(kPink);
  
  dx=SELC.CabWid/2;
  dz=SWCA.Length/2;
  dy=SELC.CabThk/2;
  TGeoVolume *SECA = gGeoManager->MakeBox("SECA",GetMed("SVTT_COPPER"),dx,dy,dz); 
  SECA->SetTitle("the cable on the electronics carrier");
  SECA->SetLineColor(kRed);
  
  TGeoVolume *SVTD = 0;
  
  TGeoVolume *Layers[2][6] = {
    {0,0,0,0,0,0},
    {0,0,0,0,0,0}
  };
  Int_t L1 = SVTG.Nmin;
  Int_t L2 = TMath::Min(6,SVTG.Nlayer);
#if svtConfig == 1
  L1 = L2 = 3;
#endif
  for (ilayer = L1; ilayer <= L2; ilayer++) {
    Int_t Barrel = 1 + (ilayer - 1)/2;
    Int_t Nladder = SVTL[ilayer-1].Nladder;
    if (ilayer < 6) {
      radmax=SVTL[ilayer].Radius;
    } else {
      radmax=SVTG.RsizeMax;
    }
    /*     Note: clearance between layers is very small. If you have to change
     *     any of the thicknesses in ladthk or elethk, you should make SLYD
     *     visible and check clearances. (wkw) */
    Double_t ladthk = SWCA.WaferThk/2+SWCA.RohaThk+SWCA.WafCarTh;
    Double_t elethk=2.0*SELC.BeThk+SELC.WatThk+SELC.BeOThk+SELC.AgPdThk;
    elethk += SELC.GlassThk+SELC.DyeThk;
    Double_t deg=180.0/Nladder;
    Double_t rad=TMath::Pi()/Nladder;

   /* Note: in case of layer conflict (outer layer starts too early),
    *       we can neglect some minor material loss in the corner of electronics,
    *       but not in the center of sensitive wafers (this would change dE/dx). 
    *       Make sure here that a layer ends up before the next layer starts */
  // SVTT/Layer   
    TGeoVolumeAssembly *Ladder = new TGeoVolumeAssembly("Ladder");
    //    dz = SVTL[ilayer-1].Nwafer*(SWCA.WaferLen/2+SWCA.WaferGap);
    dz = SWCA.Length/2;
    Double_t phi, phiT;
    Int_t l, l1, l2;
    Int_t lmin = 1;
    Int_t lmax = 2*Nladder;
#if svtConfig == 1
    lmin = lmax = 12;
#endif
    for (l = lmin; l <= lmax; l++) {
      // Last ladder always at 12 o'clock
      phi = 90 + deg*(2*Nladder - l);
      if (phi <   0) phi += 360;
      if (phi > 360) phi -= 360;
      if (l%2 != (ilayer-1)%2) continue;
      Int_t shell = 0; // Sourth ClamShell
      if (l > Nladder) shell = 1; // North ClamShell
      if (! ClamShells[shell]) {
	ClamShells[shell] = new TGeoVolumeAssembly("ClamShell"); 
	if (shell == 0)   ClamShells[shell]->SetTitle("SVT ClamShell Sourth");
	else              ClamShells[shell]->SetTitle("SVT ClamShell North");
	//	SVTT->AddNodeOverlap(ClamShells[shell],shell+1,gGeoIdentity);
	SVTT->AddNode(ClamShells[shell],shell+1,gGeoIdentity);
      }
      if (! Layers[shell][ilayer-1]) {
	if (shell == 1) {
	  if (ilayer == 1) {l1 =  6; l2 =  8;}
	  if (ilayer == 2) {l1 =  5; l2 =  7;}
	  if (ilayer == 3) {l1 =  8; l2 = 12;}
	  if (ilayer == 4) {l1 =  7; l2 = 11;}
	  if (ilayer == 5) {l1 = 10; l2 = 16;}
	  if (ilayer == 6) {l1 =  9; l2 = 15;}
	} else {
	  if (ilayer == 1) {l1 =  2; l2 =  4;}
	  if (ilayer == 2) {l1 =  1; l2 =  3;}
	  if (ilayer == 3) {l1 =  2; l2 =  6;}
	  if (ilayer == 4) {l1 =  1; l2 =  5;}
	  if (ilayer == 5) {l1 =  2; l2 =  8;}
	  if (ilayer == 6) {l1 =  1; l2 =  7;}
	}
	Layers[shell][ilayer-1] = new TGeoVolumeAssembly("Layer");
	Layers[shell][ilayer-1]->SetTitle("a single SVT layer ");
	ClamShells[shell]->AddNode(Layers[shell][ilayer-1],ilayer,gGeoIdentity);
      }
      Char_t *R = 0;
      phiT = phi - 90;
      if (phiT < 0) phiT += 360;
      if (TMath::Nint(phiT) != phiT ) R = Form("R%3.1f",phiT);
      else                            R = Form("R%03.f",phiT);
      x = SVTL[ilayer-1].Radius*TMath::Cos(degrad*phi);
      y = SVTL[ilayer-1].Radius*TMath::Sin(degrad*phi);
      Layers[shell][ilayer-1]->AddNode(Ladder,l, new TGeoCombiTrans(x,y,0, GetRot(R))); // <<
    }
    // SVTT/ClamShell/Layer/Ladder/SELE   
    dx=SELC.ElcaWid/2;  dz=SWCA.Length/2;  dy=elethk/2;
    TGeoVolume *sele = gGeoManager->MakeBox("SELE",GetMed("SVTT_AIR"),dx,dy,dz); 
    sele->SetTitle("the electronics mother volume");
    sele->SetVisibility(0);
    SELE->AddVolume(sele);
    // SVTT/ClamShell/Layer/Ladder/SELE/SWCH   
    y=-elethk/2+SELC.BeThk/2;
    sele->AddNode(SWCH,1,new TGeoTranslation(0,y,0));
    y=-elethk/2+SELC.BeThk+SELC.WatThk+SELC.BeThk/2;
    sele->AddNode(SWCH,2,new TGeoTranslation(0,y,0));
    // SVTT/ClamShell/Layer/Ladder/SELE/SWCS
    y=-elethk/2+SELC.BeThk+SELC.WatThk/2;
    x=-SELC.ElcaWid/2+SELC.BeThk/2;
    sele->AddNode(SWCS,1,new TGeoTranslation(-x,y,0));
    sele->AddNode(SWCS,2,new TGeoTranslation( x,y,0));
    // SVTT/ClamShell/Layer/Ladder/SELE/SWCW
    y=-elethk/2+SELC.BeThk+SELC.WatThk/2;
    sele->AddNode(SWCW,1,new TGeoTranslation(0,y,0));
    // SVTT/ClamShell/Layer/Ladder/SELE/SBOI
    dx=SELC.ElcaWid/2;
    dz=SVTL[ilayer-1].Nwafer*(SWCA.WaferLen/2+SWCA.WaferGap);
    dy=SELC.BeOThk/2;
    TGeoVolume *sboi = gGeoManager->MakeBox("SBOI",GetMed("SVTT_BEO"),dx,dy,dz);
    sboi->SetTitle("the Berillia layer");
    sboi->SetLineColor(kPink);
    SBOI->AddVolume(sboi);
    y=(elethk/2-SELC.DyeThk-SELC.AgPdThk - SELC.GlassThk-SELC.BeOThk/2);
    sele->AddNode(sboi,1,new TGeoTranslation(0,y,0));
    // SVTT/ClamShell/Layer/Ladder/SELE/SGLA
    dx=SELC.ElcaWid/2;
    dz=SVTL[ilayer-1].Nwafer*(SWCA.WaferLen/2+SWCA.WaferGap);
    dy=SELC.GlassThk/2;
    TGeoVolume *sgla = gGeoManager->MakeBox("SGLA",GetMed("SVTT_GLASS"),dx,dy,dz); 
    sgla->SetTitle("Glass insulating plane");
    sgla->SetLineColor(kPink);
    SGLA->AddVolume(sgla);
    y=elethk/2-SELC.DyeThk-SELC.AgPdThk-SELC.GlassThk/2;
    sele->AddNode(sgla,1,new TGeoTranslation(0,y,0));
    // SVTT/ClamShell/Layer/Ladder/SELE/SAGP
    dx=SELC.ElcaWid/2;
    dz=SVTL[ilayer-1].Nwafer*(SWCA.WaferLen/2+SWCA.WaferGap);
    dy=SELC.AgPdThk/2;
    TGeoVolume *sagp = gGeoManager->MakeBox("SAGP",GetMed("SVTT_AGPD"),dx,dy,dz); 
    sagp->SetTitle("the Silver-Palladium plane");
    sagp->SetLineColor(kRed);
    SAGP->AddVolume(sagp);
    y=elethk/2-SELC.DyeThk-SELC.AgPdThk/2;
    sele->AddNode(sagp,1,new TGeoTranslation(0,y,0));
    // SVTT/ClamShell/Layer/Ladder/SELE/SDYE
    dx=SELC.DyeWid/2;
    dz=SVTL[ilayer-1].Nwafer*(SWCA.WaferLen/2+SWCA.WaferGap);
    dy=SELC.DyeThk/2;
    TGeoVolume *sdye = gGeoManager->MakeBox("SDYE",GetMed("SVTT_SILICON"),dx,dy,dz); 
    sdye->SetTitle("the ic chip on the hybrid");
    sdye->SetLineColor(kPink);
    SDYE->AddVolume(sdye);
    y=elethk/2-SELC.DyeThk/2;
    x=SELC.ElcaWid/2-SELC.DyeSpc-SELC.DyeWid/2;
    sele->AddNode(sdye,1,new TGeoTranslation(x,y,0));
    y=elethk/2-SELC.DyeThk/2;
    x=SELC.ElcaWid/2-2.0*SELC.DyeSpc-3.0*SELC.DyeWid/2;
    sele->AddNode(sdye,2,new TGeoTranslation(x,y,0));
    // SVTT/ClamShell/Layer/Ladder/SELE/SECA
    y=elethk/2-SELC.DyeThk+SELC.CabThk/2;
    x=-SELC.ElcaWid/2+SELC.CabWid/2;
    sele->AddNode(SECA,1,new TGeoTranslation(x,y,0));
    //     Position electronics carrier on left side of ladder (y pos)
    Double_t ypos=TMath::Sin(rad)*SELC.ElcaWid/2-TMath::Cos(rad)*elethk/2;
    Double_t xpos=TMath::Cos(rad)*SELC.ElcaWid/2+TMath::Sin(rad)*elethk/2;
    for (Int_t s = -1, i = 1; s <= 1; s+=2, i++) {
      y=-ladthk-ypos; x=s*(SWCA.WaferWid/2+xpos);
      phiT = -s*deg;
      if (phiT < 0) phiT += 360;
      if (TMath::Nint(phiT) != phiT ) R = Form("R%3.1f",phiT);
      else                            R = Form("R%03.f",phiT);
      Ladder->AddNode(sele,i,new TGeoCombiTrans(x,y,0,GetRot(R)));
    }
    // SVTT/ClamShell/Layer/Ladder/SLDI      
    /* This contains the active wafer, Roha cell support, and the Be waffer carrier
     * The center of the active wafer is at the center of this cell, the cell is
     * centered on SVTL[ilayer-1].Radius. */
    Double_t tabLen=SWCA.Length/2-7*(SWCA.WaferWid/2+SWCA.WaferGap);
    dx=SWCA.WaferWid/2; dz=SWCA.Length/2;  dy=ladthk;
    TGeoVolume *sldi = gGeoManager->MakeBox("SLDI",GetMed("SVTT_AIR"),dx,dy,dz); 
    sldi->SetTitle("a ladder volume");
    SLDI->AddVolume(sldi);
    //    Ladder->AddNode(sldi,1,GetRot("90XD")); // "90XD"  (x,y,z) = > ( z, x, y)
    Ladder->AddNode(sldi,1); // "90XD"  (x,y,z) = > ( z, x, y)
    // SVTT/ClamShell/Layer/Ladder/SLDI/SBER         
    if (! SBER) {
      dx=SWCA.WafCarWd/2;
      dz=SWCA.Length/2;  
      dy=SWCA.WafCarTh/2;
      SBER = gGeoManager->MakeBox("SBER",GetMed("SVTT_CARBON"),dx,dy,dz); 
      SBER->SetTitle("the Carbon wafer carrier rails");
      SBER->SetLineColor(kRed);
    }
    x=+SWCA.WaferWid/2-SWCA.WafCarWd/2;
    y=-ladthk+SWCA.WafCarTh/2; 
    sldi->AddNode(SBER,1,new TGeoTranslation( x,y,0));
    sldi->AddNode(SBER,2,new TGeoTranslation(-x,y,0));
    // SVTT/ClamShell/Layer/Ladder/SLDI/STAB
    if (! STAB) {
      dx=SWCA.WaferWid/2-SWCA.WafCarWd;
      dz=tabLen/2;
      dy=SWCA.WafCarTh/2;
      STAB = gGeoManager->MakeBox("STAB",GetMed("SVTT_CARBON"),dx,dy,dz); 
      STAB->SetTitle("the Carbon wafer carrier end tabs");
      STAB->SetLineColor(kRed);
    }
    z=SWCA.Length/2-tabLen/2;
    y=-ladthk+SWCA.WafCarTh/2;
    sldi->AddNode(STAB,1,new TGeoTranslation(0, y, z));
    sldi->AddNode(STAB,2,new TGeoTranslation(0, y,-z));
    // SVTT/ClamShell/Layer/Ladder/SLDI/STRU
    if (! STRU) {
      dx=SWCA.WaferWid/2-SWCA.WafCarWd;
      dz=SWCA.strutlen/2;
      dy=SWCA.WafCarTh/2;
      STRU = gGeoManager->MakeBox("STRU",GetMed("SVTT_CARBON"),dx,dy,dz); 
      STRU->SetTitle("the Carbon struts between the wafer carrier rails");
      STRU->SetLineColor(kRed);
    }
    z= (SVTL[ilayer-1].Nwafer*(SWCA.WaferLen/2+SWCA.WaferGap)+SWCA.WaferGap+SWCA.strutlen/2);
    y=-ladthk+SWCA.WafCarTh/2;
    sldi->AddNode(STRU,1,new TGeoTranslation(0,y,-z));
    sldi->AddNode(STRU,2,new TGeoTranslation(0,y,-z));
    // SVTT/ClamShell/Layer/Ladder/SLDI/SRHC   
    if (! SRHCs[Barrel-1]) {
      dx=SWCA.WafCarWd/2;
      dz=SVTL[ilayer-1].Nwafer*(SWCA.WaferLen/2+SWCA.WaferGap);
      dy=SWCA.RohaThk/2;
      SRHCs[Barrel-1] = gGeoManager->MakeBox("SRHC",GetMed("SVTT_GLASS"),dx,dy,dz); 
      SRHCs[Barrel-1]->SetTitle("the roha cell wafer support");
      SRHCs[Barrel-1]->SetLineColor(kGreen);
      SRHC->AddVolume(SRHCs[Barrel-1]);
    }
    x=+SWCA.WaferWid/2-SWCA.WafCarWd/2;
    y=-ladthk+2.0*SWCA.WafCarTh/2+SWCA.RohaThk/2;
    sldi->AddNode(SRHCs[Barrel-1],1,new TGeoTranslation( x,y,0));
    sldi->AddNode(SRHCs[Barrel-1],2,new TGeoTranslation(-x,y,0));
    // SVTT/ClamShell/Layer/Ladder/SLDI/SPCB
    if (! SPCBs[Barrel-1]) {
      dx=SVTL[ilayer-1].PcbWidth/2.0;
      dz=SVTL[ilayer-1].PcbLen/2.0;
      dy=SVTL[ilayer-1].PcbThk/2.0;
      SPCBs[Barrel-1] = gGeoManager->MakeBox("SPCB",GetMed("SVTT_G10"),dx,dy,dz); 
      SPCBs[Barrel-1]->SetTitle("the G10 PCB");
      SPCBs[Barrel-1]->SetLineColor(kYellow);
    }
    
    Double_t zPCB = SVTL[ilayer-1].Nwafer*(SWCA.WaferLen+SWCA.WaferGap)/2.0 + 
      SVTL[ilayer-1].PcbGap + SVTL[ilayer-1].PcbLen/2.0;
    y = SVTL[ilayer-1].PcbThk/2.0;
    sldi->AddNode(SPCBs[Barrel-1],1,new TGeoTranslation(0,y, zPCB));
    sldi->AddNode(SPCBs[Barrel-1],2,new TGeoCombiTrans (0,y,-zPCB,GetRot("R180")));
    // SVTT/ClamShell/Layer/Ladder/SLDI/svtd
    if (! SVTD) {
      dx=SWCA.WaferWid/2;
      dz=SWCA.WaferLen/2;
      dy=SWCA.WaferThk/2;
      SVTD = gGeoManager->MakeBox("svtd",GetMed("SVTT_SENSITIVE"),dx,dy,dz); 
      SVTD->SetTitle("an active wafer volume");
      SVTD->SetLineColor(kBlue);
      // SVTT/ClamShell/Layer/Ladder/SLDI/SVTD/STRA
      Double_t trapX=SWCA.SensWid/2.0-(SWCA.SensWid-SWCA.SensGir)/2.0;
      Double_t dx1=0.0;
      Double_t dx2=SWCA.SensWid/2.0;
      dy =SWCA.WaferThk/2.0;
      dz =(SWCA.SensLen-SWCA.SensGir)/2.0;
      TGeoVolume *STRA = gGeoManager->MakeTrd1("STRA",GetMed("SVTT_NONSENSSILI"),dx1,dx2,dy,dz); 
      STRA->SetTitle("a trapezoid of triangular shape");
      STRA->SetLineColor(kRed);
      SVTD->AddNode(STRA,1,new TGeoTranslation(0,0, trapX));
      STRA = gGeoManager->MakeTrd1("STRA",GetMed("SVTT_NONSENSSILI"),dx2,dx1,dy,dz); 
      STRA->SetTitle("a trapezoid of triangular shape");
      STRA->SetLineColor(kRed);
      SVTD->AddNode(STRA,2,new TGeoTranslation(0,0,-trapX));
      // SVTT/ClamShell/Layer/Ladder/SLDI/SVTD/SSID
      dx=(SWCA.WaferLen-SWCA.SensLen)/4.0; dz=SWCA.WaferWid/2.0; dy=SWCA.WaferThk/2;
      TGeoVolume *SSID = gGeoManager->MakeBox("SSID",GetMed("SVTT_NONSENSSI"),dx,dy,dz); 
      SSID->SetTitle("a non-sensitive left-right border of the wafer");
      Double_t ssidX=SWCA.WaferLen/2.0-(SWCA.WaferLen-SWCA.SensLen)/4.0;
      SVTD->AddNode(SSID,1,new TGeoTranslation( ssidX,0,0));
      SVTD->AddNode(SSID,2,new TGeoTranslation(-ssidX,0,0));
      // SVTT/ClamShell/Layer/Ladder/SLDI/SVTD/SSIR
      dx=SWCA.SensLen/2.0; dz=(SWCA.WaferWid-SWCA.SensWid)/4.0; dy=SWCA.WaferThk/2;
      TGeoVolume *SSIR = gGeoManager->MakeBox("SSIR",GetMed("SVTT_NONSENSSIL"),dx,dy,dz); 
      SSIR->SetTitle("a non-sensitive up-down border of the wafer");
      Double_t ssirZ=SWCA.WaferWid/2.0-(SWCA.WaferWid-SWCA.SensWid)/4.0;
      SVTD->AddNode(SSIR,1,new TGeoTranslation(0,0, ssirZ));
      SVTD->AddNode(SSIR,2,new TGeoTranslation(0,0,-ssirZ));
    }
    dz = SWCA.WaferLen/2+SWCA.WaferGap; dy=SWCA.WaferThk/2;
    for (Int_t k = 1; k <= SVTL[ilayer-1].Nwafer; k++) {
      sldi->AddNode(SVTD,k,new TGeoTranslation(0, 0, dz*(2*k -1 - SVTL[ilayer-1].Nwafer)));
    }
  }
  return SVTT;
}
#endif
