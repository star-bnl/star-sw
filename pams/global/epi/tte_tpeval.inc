C tte_tpeval.inc
C This file was made by the idl compiler "stic". Do not edit.
C Instead, edit the source idl file and then re-run the compiler.
C For help, type contact Craig Tull or Herb Ward.
C COMMENTS FROM IDL FILE:
C  tte_tpeval.idl
C      (onetasth.ace)
C  
C    Table: tte_tpeval
C    Evaluation of the tracking package      
C                                            
#ifndef TTE_TPEVAL_INC
#define TTE_TPEVAL_INC
#endif
	STRUCTURE /TTE_TPEVAL_ST/
	INTEGER*4 inhit !  hit id of innermost hit on rec. track 0 
	INTEGER*4 mtrk !  mc track # for he dominant contribution 0 
	INTEGER*4 nfit !  Number of points included in fit 0 
	INTEGER*4 ngood !  Number of points from the dominant track 0 
	INTEGER*4 nrec !  Number of reconstructed points 0 
	INTEGER*4 qg !  Generated charge 0 
	INTEGER*4 qr !  Reconstructed charge 0 
	INTEGER*4 rtrk !  Reconstructed track # 0 
	INTEGER*4 vid !  Vertex id 0 
	   REAL*4 abpullav(2) !  Average of the absolute values of pulls 0 
	   REAL*4 chisq(2) !  Chi**2 for the helix fit 0 
	   REAL*4 dpt !  error on reconstructed pt 0 
	   REAL*4 dpx !  error on reconstructed px 0 
	   REAL*4 dpy !  error on reconstructed py 0 
	   REAL*4 dpz !  error on reconstructed pz 0 
	   REAL*4 pid !  Particle id 0 
	   REAL*4 prob(2) !  Probability for a given chisq 0 
	   REAL*4 ptg !  Transverse momentum at the first pnt(mc) 0 
	   REAL*4 ptr !  Reconstructed pt at the first point 0 
	   REAL*4 ptvg !  Transverse momentum at the vertex (mc). 0 
	   REAL*4 ptvr !  Transverse momentum rec'd at the mc vtx 0 
	   REAL*4 pullav(2) !  Average pull per track 0 
	   REAL*4 pullmx(2) !  Maximum pull 0 
	   REAL*4 pxg !  Generated px at the same point as pxr 0 
	   REAL*4 pxr !  reconstructed px 0 
	   REAL*4 pxvg !  Generated px at the vertex 0 
	   REAL*4 pxvr !  Reconstructed px at the mc vertex 0 
	   REAL*4 pyg !  Generated py at the same point as pyr 0 
	   REAL*4 pyr !  Reconstructed py 0 
	   REAL*4 pyvg !  Generated py at the vertex 0 
	   REAL*4 pyvr !  Reconstructed py at the mc vertex 0 
	   REAL*4 pzg !  Generated pz at the same point as pzr 0 
	   REAL*4 pzr !  Reconstructed pz 0 
	   REAL*4 pzvg !  Generated pz at the vertex 0 
	   REAL*4 pzvr !  Reconstructed pz at the mc vertex 0 
	   REAL*4 qfact !  % of points from the dominant mc track 0 
	   REAL*4 rdca !  Dist. of closest approach vtx (radial) 0 
	   REAL*4 zdca !  Dist. of closest approach vtx (drift) 0 
	END STRUCTURE
C
      CHARACTER*(*) TTE_TPEVAL_SPEC
      PARAMETER    (TTE_TPEVAL_SPEC=
     + ' struct tte_tpeval {'               //
     + ' long inhit;'                       //
     + ' long mtrk;'                        //
     + ' long nfit;'                        //
     + ' long ngood;'                       //
     + ' long nrec;'                        //
     + ' long qg;'                          //
     + ' long qr;'                          //
     + ' long rtrk;'                        //
     + ' long vid;'                         //
     + ' float abpullav[2];'                //
     + ' float chisq[2];'                   //
     + ' float dpt;'                        //
     + ' float dpx;'                        //
     + ' float dpy;'                        //
     + ' float dpz;'                        //
     + ' float pid;'                        //
     + ' float prob[2];'                    //
     + ' float ptg;'                        //
     + ' float ptr;'                        //
     + ' float ptvg;'                       //
     + ' float ptvr;'                       //
     + ' float pullav[2];'                  //
     + ' float pullmx[2];'                  //
     + ' float pxg;'                        //
     + ' float pxr;'                        //
     + ' float pxvg;'                       //
     + ' float pxvr;'                       //
     + ' float pyg;'                        //
     + ' float pyr;'                        //
     + ' float pyvg;'                       //
     + ' float pyvr;'                       //
     + ' float pzg;'                        //
     + ' float pzr;'                        //
     + ' float pzvg;'                       //
     + ' float pzvr;'                       //
     + ' float qfact;'                      //
     + ' float rdca;'                       //
     + ' float zdca;'                       //
     + ' };')
C
