// $Id: St_geom_Maker.cxx,v 1.5 1998/12/16 20:56:24 fisyak Exp $
// $Log: St_geom_Maker.cxx,v $
// Revision 1.5  1998/12/16 20:56:24  fisyak
// Add gstar to ROOT
//
// Revision 1.4  1998/12/12 00:21:15  fisyak
// Remove gstar for the moment
//
// Revision 1.3  1998/12/12 00:18:00  fisyak
// Remove gstar for the moment
//
// Revision 1.2  1998/12/04 19:36:47  fisyak
// Add Pavel/Ruben gstar interface
//
// Revision 1.1  1998/10/31 00:28:31  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:29  perev
// cleanup
//
// Revision 1.5  1998/10/02 13:46:08  fine
// DataSet->DataSetIter
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_geom_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_geom_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include <stdio.h>
#include <string.h>

#include "TGeometry.h"
#include "TNode.h"

#include "TBRIK.h"
#include "TTRD1.h"
#include "TTRD2.h"
#include "TTRAP.h"
#include "TTUBE.h"
#include "TTUBS.h"
#include "TCONE.h"
#include "TCONS.h"
#include "TSPHE.h"
#include "TPARA.h"
#include "TPGON.h"
#include "TPCON.h"
#include "TELTU.h"
//     #include "THYPE.h"
#include "TGTRA.h"
#include "TCTUB.h"

extern "C" void geant_     ();
extern "C" void agmain_    (Int_t*,Int_t*);
extern "C" void agxuser_   ();
extern "C" void agxinit_   ();
extern "C" void geometry_  ();
extern "C" Int_t  agvolume_  (TNode**,Float_t**,Float_t**,Float_t**,Int_t*);
extern "C" void kuexel_    (const Char_t*,Int_t);
extern "C" void set_kupatl_(const Char_t*,Int_t*,Int_t);
extern "C" void dzddiv_    (Int_t*,Int_t*,Char_t*,Char_t*,Int_t*,Int_t*,Int_t*,Int_t*,Int_t,Int_t);
extern "C" void gfrotm_    (Int_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*);
extern "C" void gfxzrm_    (Int_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*,Float_t*);

Float_t theta1, phi1, theta2, phi2, theta3, phi3, type;
Int_t   nlev;

ClassImp(St_geom_Maker)

//_____________________________________________________________________________
St_geom_Maker::St_geom_Maker(const Char_t *name, const Char_t *title):
StMaker(name,title){
  drawinit=kFALSE;
}
//_____________________________________________________________________________
St_geom_Maker::~St_geom_Maker(){
}
//_____________________________________________________________________________
Int_t St_geom_Maker::Init(){
// Create tables
   St_DataSetIter       local(gStChain->DataSet("params"));
   printf (" calling agmain \n");
   Int_t nwg = 100000;
   Int_t nwp = 0;
   agmain_(&nwg,&nwp); 
   Do("detp geometry field_only");

   geometry_();
#if 0
   //   Draw();
   Do("dcut cave x 0.1 10 10 0.03 0.03");
   Work();
#endif
// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_geom_Maker::Make(){
//  PrintInfo();
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
}
 return kStOK;
}
//_____________________________________________________________________________
void St_geom_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_geom_Maker.cxx,v 1.5 1998/12/16 20:56:24 fisyak Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

#if 1
//Geometry *pGeometry = new Geometry();


void St_geom_Maker::Draw()
{ 
     int    idiv=2,Ldummy,one=1,zero=0,iw=1;
     Char_t   *path=" ",*opt="IN";
     dzddiv_ (&idiv,&Ldummy,path,opt,&one,&zero,&one,&iw,1,2);
}

void St_geom_Maker::Do(const Char_t *job)
{  
     int l=strlen(job);
     kuexel_(job,l);
}

void St_geom_Maker::Work()
{  
     TNode*   node=0;
     TRotMatrix* rotm=0;
     TRotMatrix* rotd=0;
     Float_t* volu=0, *position=0, *mother=0;
     Int_t copy=0;
     Char_t ss[12], ssd[12];
     int     icopy   = 0;
     Int_t   irot;
     Int_t   mrot    = 126;
     float te1[700], fi1[700], te2[700], fi2[700], te3[700], fi3[700];

     typedef enum {BOX=1,TRD1,TRD2,TRAP,TUBE,TUBS,CONE,CONS,SPHE,PARA,PGON,PCON,ELTU,HYPE,GTRA=28,CTUB} shapes;

     for (irot=1; irot<=126; irot++)
     {   
         gfrotm_ (&irot, &theta1,&phi1, &theta2,&phi2, &theta3,&phi3);
         sprintf(ss,"rotm%i",irot);
         rotm=new TRotMatrix(ss,ss,  theta1,phi1, theta2,phi2, theta3,phi3);
         te1[irot]=theta1; fi1[irot]=phi1;
         te2[irot]=theta2; fi2[irot]=phi2;
         te3[irot]=theta3; fi3[irot]=phi3;
         //if(irot >= 100) printf("%i %f %f %f %f %f %f \n", irot, te1[irot],fi1[irot], te2[irot],fi2[irot], te3[irot],fi3[irot]);
     }
     printf(" found %d rotation matrices \n",irot);
     printf(" looping on agvolume \n");

     while (agvolume_(&node,&volu,&position,&mother,&copy))
     { 
           TShape*  t;
           TRotMatrix* rotm;
           TRotMatrix* rotd;
           shapes   shape  = (shapes) volu[1];
           Int_t    nin    = 0;
           Int_t    np     = volu[4];
           Float_t* p      = volu+6;
           Int_t    irot   = 0;
           Float_t* xyz    = 0;
           Float_t* att    = volu+6+np; 
           Char_t     name[] = {0,0,0,0,0};
           float    xx[3]  = {0.,0.,0.};
           Int_t       j   = 0;
           Int_t   check   = 0;

           if (mother) nin = mother[2];
           if (node) node->cd();

           strncpy(name,(const Char_t*)(volu-5),4);
           t=(TShape*)gGeometry->GetListOfShapes()->FindObject(name);
           if (!t)
           {   switch (shape)
               { 
                  case BOX:  t=new TBRIK(name,"BRIK","void",p[0],p[1],p[2]);                                                 break;
                  case TRD1: t=new TTRD1(name,"TRD1","void",p[0],p[1],p[2],p[3]);                                            break;
                  case TRD2: t=new TTRD2(name,"TRD2","void",p[0],p[1],p[2],p[3],p[4]);                                       break;
                  case TRAP: t=new TTRAP(name,"TRAP","void",p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10]);        break;
                  case TUBE: t=new TTUBE(name,"TUBE","void",p[0],p[1],p[2]);                                                 break;
                  case TUBS: t=new TTUBS(name,"TUBS","void",p[0],p[1],p[2],p[3],p[4]);                                       break;
                  case CONE: t=new TCONE(name,"CONE","void",p[0],p[1],p[2],p[3],p[4]);                                       break;
                  case CONS: t=new TCONS(name,"CONS","void",p[0],p[1],p[2],p[3],p[4],p[5],p[6]);                             break;
                  case SPHE: t=new TSPHE(name,"SPHE","void",p[0],p[1],p[2],p[3],p[4],p[5]);                                  break;
                  case PARA: t=new TPARA(name,"PARA","void",p[0],p[1],p[2],p[3],p[4],p[5]);                                  break;
                  case PGON: t=new TPGON(name,"PGON","void",p[0],p[1],p[2],p[3]);                                            break;
                  case PCON: t=new TPCON(name,"PCON","void",p[0],p[1],p[2]);                                                 break;
                  case ELTU: t=new TELTU(name,"ELTU","void",p[0],p[1],p[2]);                                                 break;
//                  case HYPE: t=new THYPE(name,"HYPE","void",p[0],p[1],p[2],p[3]);                                          break;
                  case GTRA: t=new TGTRA(name,"GTRA","void",p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11]);  break;
                  case CTUB: t=new TCTUB(name,"CTUB","void",p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10]);        break;
//                  default:   t=new TBRIK(name,"BRIK","void",p[0],p[1],p[2]);                                               break;
               };
               t->SetLineColor(att[4]);
           };

           irot  = *(position+3);
           xyz   =   position+4;
           if (nin<0) 
           {  
	         //if (name[0]=='M' && name[1]=='S' && name[2]=='E' && name[3]=='C')
	         //{
                 gfxzrm_ (&nlev, &xx[0],&xx[1],&xx[2], &theta1,&phi1, &theta2,&phi2, &theta3,&phi3, &type);

                 check = 0;
                 for (j=1; j<=mrot; j++)
		 {
		      if (theta1==te1[j] && phi1==fi1[j] && theta2==te2[j] && phi2==fi2[j] && theta3==te3[j] && phi3==fi3[j]) 
		      {
		         irot = j; 
                         check = 1;
                         break; 
		      };
		 };

                 if (check == 0)
		 {
                     icopy = icopy + 1;
                     irot = 126 + icopy;
                     mrot = irot;
                     te1[irot]=theta1; fi1[irot]=phi1;
                     te2[irot]=theta2; fi2[irot]=phi2;
                     te3[irot]=theta3; fi3[irot]=phi3;
                 };
                 xyz  = xx;
                 sprintf(ss,"rotm%i",irot);
                 if(check == 0) rotm=new TRotMatrix(ss,ss,  theta1,phi1, theta2,phi2, theta3,phi3);
                 if(check == 0) printf(" check=%i, icopy=%i, irot=%i, mrot=%i, ss=%s \n", check, icopy, irot, mrot, ss);
                 //printf("%i %f %f %f %f %f %f \n", nlev, theta1, phi1, theta2,phi2, theta3,phi3);
		 //}
           }

           sprintf(ss,"rotm%i",irot);

           if (irot>0)   node = new TNode(name,"NODE",t,xyz[0],xyz[1],xyz[2], gGeometry->GetRotMatrix(ss));
           else          node = new TNode(name,"NODE",t,xyz[0],xyz[1],xyz[2]);
           node -> SetVisibility(att[1]);
     };
     fNode=node;
}
#endif


