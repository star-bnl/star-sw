//////////////////////////////////////////////////////////////////////////
//                                                                      //
//               St_geant_Maker class for Makers                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_geant_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include <stdio.h>
#include <string.h>

#include "TGeometry.h"
#include "St_Node.h"
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
#include "TGeant3.h"
#include "St_g2t_run_Table.h"
#include "St_g2t_event_Table.h"
#include "St_g2t_gepart_Table.h"
#include "St_g2t_vertex_Table.h"
#include "St_g2t_track_Table.h"
#include "St_g2t_svt_hit_Table.h"
#include "g2r/St_g2t_get_kine_Module.h"
#include "g2r/St_g2t_tpc_Module.h"
#include "g2r/St_g2t_ftp_Module.h"
#include "g2r/St_g2t_svt_Module.h"
#include "St_g2t_ctf_hit_Table.h"
#include "St_g2t_eem_hit_Table.h"
#include "St_g2t_emc_hit_Table.h"
#include "St_g2t_esm_hit_Table.h"
#include "St_g2t_mwc_hit_Table.h"
#include "St_g2t_smd_hit_Table.h"
#include "St_g2t_vpd_hit_Table.h"

common_gcbank *cbank;
common_quest  *cquest; 
common_gclink *clink; 
common_gccuts *ccuts; 
common_gcflag *cflag; 
common_gckine *ckine; 
common_gcking *cking; 
common_gcmate *cmate; 
common_gctmed *ctmed; 
common_gctrak *ctrak; 
common_gctpol *ctpol; 
common_gcvolu *cvolu; 
common_gcnum  *cnum; 
common_gcsets *csets; 

Int_t *z_iq, *z_lq; 
Float_t *z_q; 

Float_t theta1, phi1, theta2, phi2, theta3, phi3, type;
Int_t   nlev;
#ifdef F77_NAME
#define gfnhit_ F77_NAME(gfnhit,GFNHIT)
#define csjcal_ F77_NAME(csjcal,CSJCAL)
#define csaddr_ F77_NAME((csaddr,CSADDR)
#endif
#define gfnhit hfnhit_
#define csaddr csaddr_
#define csjcal csjcal_

typedef long int (*addrfun)(); 
extern "C" void     type_of_call *csaddr_(char *name, int l77name=0);
extern "C" long int type_of_call  csjcal_(addrfun *fun,int  *narg,...);
extern "C" void     type_of_call  gfnhit_(char*,char*,int*,int,int);

ClassImp(St_geant_Maker)

class matry:public TObject 
{

  private: 
  static TObjArray *list;
  float angle[6];

  public:
  matry  (float *vector) 
  { 
    memcpy(angle,vector,6*sizeof(float)); 
  }
  
  Bool_t Compare (float *vector) 
  { for (int i=0; i<6; i++)  if (vector[i]!=angle[i]) return kFALSE;
    return kTRUE;
  }
  static Bool_t Insert(float *vector)
  { matry *current;
    if (!list) list=new TObjArray;
    TIter next(list);
    while (current=(matry *)next()) if (current->Compare(vector)) return kTRUE;
    list->Add(new matry(vector));   return kFALSE;
  }
};


//_____________________________________________________________________________
St_geant_Maker::St_geant_Maker(const Char_t *name, const Char_t *title):
StMaker(name,title){
  drawinit= kFALSE;
  nwgeant = 2000000;
  nwpaw   =       0;
  iwtype  =       0;
}
//_____________________________________________________________________________
St_geant_Maker::~St_geant_Maker(){
}
//_____________________________________________________________________________
Int_t St_geant_Maker::Init(){
// Initialize GEANT
  PrintInfo();
  if (! geant) {
    geant  = new TGeant3("Geant","C++ Interface to Geant3",nwgeant,nwpaw,iwtype); 
  }
// Create Histograms    
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_geant_Maker::Make()
{ if (!m_DataSet->GetList()) 
 {
  Int_t nhits;
  gtrig();
  St_g2t_vertex  *g2t_vertex  = new St_g2t_vertex("g2t_vertex",cnum->nvertx);
  m_DataSet->Add(g2t_vertex);
  St_g2t_track   *g2t_track   = new St_g2t_track ("g2t_track",cnum->ntrack);
  m_DataSet->Add(g2t_track);
  Int_t Res_kine = g2t_get_kine(g2t_vertex,g2t_track);

  gfnhit_ ("SVTH","SVTD", &nhits, 4,4);
  if (nhits>0) 
  { St_g2t_svt_hit *g2t_svt_hit = new St_g2t_svt_hit("g2t_svt_hit",nhits);
    m_DataSet->Add(g2t_svt_hit);
    Int_t Res_svt = g2t_svt(g2t_track,g2t_svt_hit);
  }
  gfnhit_ ("TPCH","TPAD", &nhits, 4,4);
  if (nhits>0)
  { St_g2t_tpc_hit *g2t_tpc_hit = new St_g2t_tpc_hit("g2t_tpc_hit",nhits);
    m_DataSet->Add(g2t_tpc_hit);
    Int_t Res_tpc = g2t_tpc(g2t_track,g2t_tpc_hit);
  }
  gfnhit_ ("FTPH","FSEC", &nhits, 4,4);
  if (nhits>0)
  { St_g2t_ftp_hit *g2t_ftp_hit = new St_g2t_ftp_hit("g2t_ftp_hit",nhits);
    m_DataSet->Add(g2t_ftp_hit);
    Int_t Res_ftp = g2t_ftp(g2t_track,g2t_ftp_hit);
  }

#if 0
  Char_t *g2t = "g2t_";
  Int_t  narg = 0;
  addrfun address  = (addrfun ) csaddr(g2t,strlen(g2t));
  if (address) csjcal(&address,&narg);
#endif
 }
 return kStOK;
}
//_____________________________________________________________________________
void St_geant_Maker::LoadGeometry(Char_t *option){
  Init(); 
  Do (option); 
  geometry_();
}
//_____________________________________________________________________________
void St_geant_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_geant_Maker.cxx,v 1.10 1999/02/02 01:03:39 nevski Exp $\n");
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
void St_geant_Maker::Draw()
{ 
  int    idiv=2,Ldummy,one=1,zero=0,iw=1;
  Char_t   *path=" ",*opt="IN";
  dzddiv_ (&idiv,&Ldummy,path,opt,&one,&zero,&one,&iw,1,2);
}
//_____________________________________________________________________________
void St_geant_Maker::Do(const Char_t *job)
{  
  Init();
  int l=strlen(job);
  if (l) kuexel_(job,l);
}
//_____________________________________________________________________________
void St_geant_Maker::Work()
{  
  St_Node*    node=0;
  Float_t    *volu=0, *position=0, *mother=0;
  Int_t       copy=0;
  int         icopy=0;

  typedef enum {BOX=1,TRD1,TRD2,TRAP,TUBE,TUBS,CONE,CONS,SPHE,PARA,
                      PGON,PCON,ELTU,HYPE,GTRA=28,CTUB} shapes;

  printf(" looping on agvolume \n");
  //   ==================================================
  while (agvolume_(&node,&volu,&position,&mother,&copy)) 
  { // ==================================================

    TShape*  t;
    shapes   shape  = (shapes) volu[1];
    Int_t    nin    = 0;
    Int_t    np     = volu[4];
    Float_t* p      = volu+6;
    Int_t    irot   = 0;
    Float_t* xyz    = 0;
    Float_t* att    = volu+6+np; 
    Char_t   name[] = {0,0,0,0,0};
    float    xx[3]  = {0.,0.,0.};
    Int_t       j   = 0;
    Int_t   check   = 0;

    if (mother) nin = mother[2];

    strncpy(name,(const Char_t*)(volu-5),4);
    t=(TShape*)gGeometry->GetListOfShapes()->FindObject(name);
    printf(" found object %s %d \n",name,t);

    if (!t) 
    { switch (shape) 
      { case BOX:  t=new TBRIK(name,"BRIK","void",
                         p[0],p[1],p[2]);                         break;
        case TRD1: t=new TTRD1(name,"TRD1","void",
                         p[0],p[1],p[2],p[3]);                    break;
        case TRD2: t=new TTRD2(name,"TRD2","void",
                         p[0],p[1],p[2],p[3],p[4]);               break;
        case TRAP: t=new TTRAP(name,"TRAP","void",
                         p[0],p[1],p[2],p[3],p[4],p[5],
                         p[6],p[7],p[8],p[9],p[10]);              break;
        case TUBE: t=new TTUBE(name,"TUBE","void",
                         p[0],p[1],p[2]);                         break;
        case TUBS: t=new TTUBS(name,"TUBS","void",
                         p[0],p[1],p[2],p[3],p[4]);               break;
        case CONE: t=new TCONE(name,"CONE","void",
                         p[0],p[1],p[2],p[3],p[4]);               break;
        case CONS: t=new TCONS(name,"CONS","void",
                         p[0],p[1],p[2],p[3],p[4],p[5],p[6]);     break;
        case SPHE: t=new TSPHE(name,"SPHE","void",
                         p[0],p[1],p[2],p[3],p[4],p[5]);          break;
        case PARA: t=new TPARA(name,"PARA","void",
                         p[0],p[1],p[2],p[3],p[4],p[5]);          break;
        case PGON: t=new TPGON(name,"PGON","void",
                         p[0],p[1],p[2],p[3]);                    break;
        case PCON: t=new TPCON(name,"PCON","void",
                         p[0],p[1],p[2]);                         break;
        case ELTU: t=new TELTU(name,"ELTU","void",
                         p[0],p[1],p[2]);                         break;
//      case HYPE: t=new THYPE(name,"HYPE","void",
//                       p[0],p[1],p[2],p[3]);                    break;
        case GTRA: t=new TGTRA(name,"GTRA","void",
                         p[0],p[1],p[2],p[3],p[4],p[5],
                         p[6],p[7],p[8],p[9],p[10],p[11]);        break;
        case CTUB: t=new TCTUB(name,"CTUB","void",
                         p[0],p[1],p[2],p[3],p[4],p[5],
                         p[6],p[7],p[8],p[9],p[10]);              break;
        default:   t=new TBRIK(name,"BRIK","void",
                         p[0],p[1],p[2]);                         break;
      };
      t->SetLineColor(att[4]);
    };
    irot  = *(position+3);
    xyz   =   position+4;

    gfxzrm_ (&nlev, &xx[0],&xx[1],&xx[2], &theta1,&phi1, 
                    &theta2,&phi2, &theta3,&phi3, &type);
    float vect[] = {theta1,phi1,theta2,phi2,theta3,phi3};

    //    matry::Insert(vect)    
 
    xyz   = xx;

    St_Node *newNode = new St_Node(name,"NODE",t);
    newNode -> SetVisibility(att[1]);
  
    if (node) 
    {  TRotMatrix *matrix=GetMatrix(theta1,phi1,theta2,phi2,theta3,phi3);
       node->Add(newNode,xyz[0],xyz[1],xyz[2],matrix);
    }
    node = newNode;
  };
  fNode=node;
}

//------------------------------------------------------------------------
static Bool_t CompareMatrix(TRotMatrix &a,TRotMatrix &b)
{  double *pa=a.GetMatrix(); double *pb=b.GetMatrix();
   for (int i=0; i<9; i++)  if (pa[i]!=pb[i]) return kFALSE;
   return kTRUE;
}

TRotMatrix *St_geant_Maker::GetMatrix(float thet1, float phii1,
                                      float thet2, float phii2,
                                      float thet3, float phii3)
{  char mname[20];
   // return 0;
   THashList *list = gGeometry->GetListOfMatrices();
   int n=list->GetSize(); sprintf(mname,"matrix%d",n+1);
   TRotMatrix *pattern=new TRotMatrix(mname,mname,
                                      thet1,phii1,thet2,phii2,thet3,phii3);
   
   TRotMatrix *matrix=0; TIter nextmatrix(list);
   while (matrix=(TRotMatrix *) nextmatrix())  
   { if (matrix!=pattern) 
     { if (CompareMatrix(*matrix,*pattern))
       { list->Remove(pattern); delete pattern; return matrix; }
   } }
   return pattern;
}


