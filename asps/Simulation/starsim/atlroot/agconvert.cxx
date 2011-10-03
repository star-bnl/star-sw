#include "agconvert.h"
#include "TApplication.h"
#include "TDataSetIter.h"
#include "TInterpreter.h"
#include "TVolumeView.h"
#include "TClassTable.h"    
#include "TGeometry.h"
#include "TMaterial.h"
#include "TMixture.h"
#include "TString.h"
#include "TVolume.h"
#include "TFile.h"
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
#include "TGTRA.h"
#include "TCTUB.h"
#include "TROOT.h"
#include "GtHash.h"
#include <stdio.h>
// #include "THYPE.h"
extern "C" int agvoluma_
           (void*,void*,void*,void*,void*,void*,void*,void*,void*,void*);
extern "C" void gfxzrm_ 
           (void*,void*,void*,void*,void*,void*,void*,void*,void*,void*,void*);

//----------------------------------------------------------------------
 // interface for fortran and CINT
agconvert ag;  agconvert &Ag = ag;  // create class and export it

extern "C" TVolume*     ag2root_() { return Ag.ag2root(); }
extern "C" TVolumeView* ag2view_() { return Ag.ag2view(); }
ClassImp(agconvert)

//----------------------------------------------------------------------
TVolume *agconvert ::  ag2root ()
{
  if (!gApplication) return 0;

  // make one copy of geometry:
  gROOT->GetListOfBrowsables()->Remove(fView);
  delete fView;     fView=0;

  gROOT->GetListOfBrowsables()->Remove(fTop);
  delete fTop;      fTop=0;

  delete gGeometry; gGeometry=0; 
  fTop = convert ();
  if (!fTop) { printf(" ag2root: no geometry available \n"); return 0; }

  // make geometry visible in any TBrowser
  TString   s    = fTop->GetName();  s += " tree";
  gROOT->GetListOfBrowsables()->Add(fTop,s.Data());
  return fTop;
}
//----------------------------------------------------------------------
TVolumeView* agconvert :: ag2view (TVolume *node, Int_t first, Int_t last)
{

   if (!gApplication) return 0;

   Int_t  nmark = 0, mmark = 0;
   TVolume   *volu = 0;
   if (!node)   node = fTop; 
   if (!node) { printf(" ag2view: no geometry available \n"); return 0; }

   gROOT->GetListOfBrowsables()->Remove(fView);
   delete fView;     fView=0;

   TDataSetIter t (node,last);

   while ((volu = (TVolume *) t()))
   {  mmark++;  Int_t ndep = t.GetDepth();  
      // const char *name = volu->GetName();
      if (first<=ndep&ndep<=last) 
      { volu->SetVisibility(TVolume::kBothVisible); volu->Mark(); nmark++; }
      else             
      { volu->SetVisibility(TVolume::kThisUnvisible); volu->UnMark(); }
   }
      fView = new TVolumeView (*node,last-first+1,0,TDataSet::kMarked); 
      // fView = new TVolumeView (*node,last-first+1,0,kMarked); 

   TString   s = node->GetName();  s += " view";
   gROOT->GetListOfBrowsables()->Add(fView,s.Data());
   printf(" %d objects marked (out of %d) \n",nmark,mmark);
   return fView;
}
//----------------------------------------------------------------------

TVolume* agconvert :: convert()
{ 
  //struct Medium { Char name[20]; Int nmat, isvol, ifield; Float fieldm;}
  //struct Volume { Char name[4], nick[4]; Int npar; Float par[50]; }
  typedef enum {BOX=1,TRD1,TRD2,TRAP,TUBE,TUBS,CONE,CONS,SPHE,PARA,
                      PGON,PCON,ELTU,HYPE,GTRA=28,CTUB} shapes;

  //  if (!myApplication) audi_init();

  GtHash     M,N,*H = 0;
  TList      droplist;
  TVolume    *node=0;
  Float_t    z, rmin, rmax, *volu=0, *position=0, *mother=0, *p=0;
  Int_t      who=0, copy=0, npar=0, ntot=0, nshape=0;
  TRotMatrix *matrix = 0;
  Int_t      nl,jn=0,jo=0;
  Char_t     name[]  = {0,0,0,0,0};
  Char_t     nick[]  = {0,0,0,0,0};
  
   // =========================================================================
  while(agvoluma_(&node,&volu,&position,&mother,&who,&copy,&p,&npar,nick,name))
  {// =========================================================================
    ntot += 1;
    TShape  *t;
    TVolume *newNode = 0, *Hp = 0;
    shapes   shape   = (shapes) volu[1];
    // Int_t medium  = (Int_t)  volu[3]; 
    Int_t    np      = (Int_t)  volu[4];
    Float_t *att     = volu+6+np;
    Float_t  xx[3]   = {0.,0.,0.};
    Float_t *pp      = 0;
    Int_t    nin     = 0;
    if (mother)  nin = (Int_t) mother[2];
    // printf(" got nick %s name %s npar=%d %d \n",nick,name,np,npar);

    if (!gGeometry) new TGeometry(nick,name);

    H = (GtHash*) N.GetPointer(nick,1);
    if (!H) 
    {	//new name: 
       H = new GtHash();   N.SetPointer(H);  droplist.Add(H);
    }
    
    Hp = (TVolume *) H->GetPointer(p,npar);
    if (Hp) newNode = Hp;
    else
    { nshape += 1;
      switch (shape) 
      { case BOX:  t=new TBRIK(nick,"BRIK","void",
                         p[0],p[1],p[2]);                         break;
        case TRD1: t=new TTRD1(nick,"TRD1","void",
                         p[0],p[1],p[2],p[3]);                    break;
        case TRD2: t=new TTRD2(nick,"TRD2","void",
                         p[0],p[1],p[2],p[3],p[4]);               break;
        case TRAP: t=new TTRAP(nick,"TRAP","void",
                         p[0],p[1],p[2],p[3],p[4],p[5],
                         p[6],p[7],p[8],p[9],p[10]);              break;
        case TUBE: t=new TTUBE(nick,"TUBE","void",
                         p[0],p[1],p[2]);                         break;
        case TUBS: t=new TTUBS(nick,"TUBS","void",
                         p[0],p[1],p[2],p[3],p[4]);               break;
        case CONE: t=new TCONE(nick,"CONE","void",
                         p[0],p[1],p[2],p[3],p[4]);               break;
        case CONS: t=new TCONS(nick,"CONS","void",   
                         p[0],p[1],p[2],p[3],p[4],p[5],p[6]);     break;
        case SPHE: t=new TSPHE(nick,"SPHE","void",
                         p[0],p[1],p[2],p[3],p[4],p[5]);          break;
        case PARA: t=new TPARA(nick,"PARA","void",
                         p[0],p[1],p[2],p[3],p[4],p[5]);          break;
        case PGON: t=new TPGON(nick,"PGON","void",p[0],p[1],(int)p[2],(int)p[3]);  

//      because of a compiler bug on Linux we cant write this (VF 030699)
//      (( TPGON*)t)->DefineSection(i,*pp++,*pp++,*pp++);
                   { pp = p+4; for (Int_t i=0; i<p[3]; i++) 
                     {   z = *pp++; rmin = *pp++; rmax = *pp++;
                         ((TPGON *)t)->DefineSection(i,z,rmin,rmax);
                   } }                                            break;
        case PCON: t=new TPCON(nick,"PCON","void",p[0],p[1],(int)p[2]);
                   { pp = p+3; for (Int_t i=0; i<p[2]; i++) 
                     {   z = *pp++; rmin = *pp++; rmax = *pp++;
                         ((TPCON *)t)->DefineSection(i,z,rmin,rmax);
                   } }                                            break;
        case ELTU: t=new TELTU(nick,"ELTU","void",
                         p[0],p[1],p[2]);                         break;
//      case HYPE: t=new THYPE(nick,"HYPE","void",
//                       p[0],p[1],p[2],p[3]);                    break;
        case GTRA: t=new TGTRA(nick,"GTRA","void",
                         p[0],p[1],p[2],p[3],p[4],p[5],
                         p[6],p[7],p[8],p[9],p[10],p[11]);        break;
        case CTUB: t=new TCTUB(nick,"CTUB","void",
                         p[0],p[1],p[2],p[3],p[4],p[5],
                         p[6],p[7],p[8],p[9],p[10]);              break;
        default:   t=new TBRIK(nick,"BRIK","void",
                         p[0],p[1],p[2]);                         break;
      };
      t->SetLineColor((short int)att[4]);

      // to build a compressed tree, name should be checked for repetition
      newNode = new TVolume(name,nick,t);
      newNode -> SetVisibility((TVolume::ENodeSEEN) 
                                TVolume::MapGEANT2StNodeVis((int)att[1]));
      H->SetPointer(newNode);
               gGeometry->GetListOfShapes()->Clear();
      // t->Print(); printf (" matrix new %d old %d \n",jn,jo,nm);
    }

    if (node)
    { 
       gfxzrm_(&nl,xx,xx+1,xx+2,p,p+1,p+2,p+3,p+4,p+5,p+6);
       matrix = (TRotMatrix *) M.GetPointer(p,6);
       if (!matrix)
       {  jn++; char mname[10]; sprintf (mname,"%d",jn);
          matrix=new TRotMatrix(mname,"r",p[0],p[1],p[2],p[3],p[4],p[5]); 
	          gGeometry->GetListOfMatrices()->Clear();
          M.SetPointer(matrix);
       }
       else { jo++; }
       node->Add(newNode,xx[0],xx[1],xx[2],matrix,UInt_t(copy));

       // node->ls();
    }
    node=newNode; 
  };
  printf(" found %d objects (%d different)\n",ntot,nshape);
  droplist.Delete();
  return node;
}
//___________________________________________________________________________



