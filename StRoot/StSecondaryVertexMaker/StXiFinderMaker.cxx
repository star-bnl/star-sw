//////////////////////////////////////////////////////////////////////////
///                                                                    ///
/// StXiFinderMaker class (finds Xi secondary vertices)                ///
///                                                                    ///
//////////////////////////////////////////////////////////////////////////

#include "StXiFinderMaker.h"
#include "StMessMgr.h"
#include "StEvent/StEventTypes.h"
#include "TMath.h"
#include "TVector2.h"
#include "tables/St_exi_exipar_Table.h"
#include "PhysicalConstants.h"



#include "math_constants.h"
#include "phys_constants.h"






//Copied from Scratch/BkupXifinder20020522/ancien_XiFinderSL01i_22032002/StRoot/St_dst_Maker/lmv.cc
#include "StarCallf77.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)





StSPtrVecXiVertex* vecXi=0;

ClassImp(StXiFinderMaker)










///_____________________________________________________________________________
StXiFinderMaker::StXiFinderMaker(const char *name):StV0FinderMaker(name),
exipar(0),parsXi(0),xiVertex(0)
{
}










///_____________________________________________________________________________
StXiFinderMaker::~StXiFinderMaker() {
}









///_____________________________________________________________________________
Int_t StXiFinderMaker::Init()
{
  TDataSet* dbDataSet = GetDataBase("global/vertices");
  if (!dbDataSet) {
    gMessMgr->Error("StXiFinderMaker::Init(): could not find appropriate database");
    return kStErr;
  }
  exipar = (St_exi_exipar*) (dbDataSet->FindObject("exipar"));
  if (!exipar) {
    gMessMgr->Error("StXiFinderMaker::Init(): could not find exipar in database");
    return kStErr;
  }
  //AddRunCont(exipar);

  return StMaker::Init();
}












///_____________________________________________________________________________
Int_t StXiFinderMaker::Make() {

  Int_t iRes;

  /// Prepare event and track variables
  iRes = Prepare();
  if (iRes != kStOk) return iRes;

  StSPtrVecXiVertex& xiVertices = event->xiVertices();
  vecXi = &xiVertices;

  /// Call the V0-finding, which will in turn call
  /// the UseV0() member function for each V0 found
  if (useExistingV0s) {
    StSPtrVecV0Vertex& v0Vertices = event->v0Vertices();
    unsigned int nV0s = v0Vertices.size();
    det_id_v0 = 1; /// for lack of any further information
    for (unsigned int i=0; i<nV0s; i++) {
      v0Vertex = v0Vertices[i];
      if (v0Vertex) UseV0();
    }
  } else {
    iRes = StV0FinderMaker::Make();
    if (iRes != kStOk) return iRes;
  }

  gMessMgr->Info() << "StXiFinderMaker: Found " << xiVertices.size() <<
                      " Xi candidates" << endm;

  return kStOk;
}












///_____________________________________________________________________________
Bool_t StXiFinderMaker::UseV0() {

  // Variables:
  StPhysicalHelixD /*trkHelix,*/tmpHelix;
  StThreeVectorD xk,/*pk,*/xpp,pp,impact,tmp3V;
  TVector2 /*rk,xck,*/tmp2V;
  StLorentzVectorD posVec,negVec;
  double dca_k,rmin;
  double mlam,mala;
  unsigned int k;
  //pairD paths,path2;
  //Julien
  double tmpcalc;
  StThreeVectorF xV0, pV0;
  double monTab[3],storeV0Coords[3];
  
  // Subroutine casc_geom
  double xd, yd, atmp, btmp, ctmp, dtmp, abtmp, xOut[2], yOut[2], dist;
  double xc, yc, rsq;
  
  //Subroutine update_track_param
  double arg, rr, xi, yi, axb, ds, dz;
  StThreeVectorF xOrig;
  
  //Subroutine track_mom
  double pt;
  
  //After subroutine track_mom
  double dv0dotdb, denom, s2, valid, xAns, yAns, yy, zz, s1, check;
  StThreeVectorF pBach, diffc, batv, v0atv;
  
  //Bloc 5bis
  double dca, bxi,ptot_b2,epi,ek,ptot_v02,ela,ptot_2,ptot,exi,eom,bdotx,vdotx,ppar,npar,pper,bBach,bV0;
  StThreeVectorF /*xpp,*/ pXi;
  
  //Function helixDCA
  double pt_tmp, bcharge_tmp, curvature_tmp, dip_tmp, phase_tmp;
  int h_tmp;
  StThreeVectorD origin_tmp;
  
  //End Julien
  

  StSPtrVecXiVertex& xiVertices   = *vecXi;
  
  
  //Julien
  int iflag=0,iflag1=0,printV0Coords;
  int i;
  int charge;
  int tries;
  float gufldX[3], gufldB[3];
  double rv,massV0;
  StThreeVectorF xPvx, bfield;
  StV0Vertex *v0Vtx;
  StHelixModel *bachGeom2;
  xPvx=mainv;
  gufldX[0]=xPvx.x();
  gufldX[1]=xPvx.y();
  gufldX[2]=xPvx.z();
  gufld(gufldX,gufldB);
  double tesla=0.1; //To replace...
  bfield.setX(gufldB[0]*tesla);
  bfield.setY(gufldB[1]*tesla);
  bfield.setZ(gufldB[2]*tesla);
  //End Julien

  Bool_t usedV0 = kFALSE;
  charge=0;

  StTrack *negDg, *posDg;
  StTrackGeometry *negDgGeom, *posDgGeom;
  negDg=v0Vertex->daughter(negative);
  posDg=v0Vertex->daughter(positive);
  if (negDg == NULL) {printf("CAUTION : pointer negDg is null.\n");return usedV0;}
  if (posDg == NULL) {printf("CAUTION : pointer posDg is null.\n");return usedV0;}
  negDgGeom=negDg->geometry();
  posDgGeom=posDg->geometry();
  if (negDgGeom == NULL) {printf("CAUTION : pointer negDgGeom is null.\n");return usedV0;}
  if (posDgGeom == NULL) {printf("CAUTION : pointer posDgGeom is null.\n");return usedV0;}
  
  /// Xi cut parameters using detector id from V0
  parsXi  = exipar->GetTable(det_id_v0-1);

  xV0=v0Vertex->position();//Julien
  pV0=v0Vertex->momentum();//Julien
  impact = xV0-mainv;
  if (impact.mag2() < (parsXi->rv_v0*parsXi->rv_v0)) return usedV0;


  /// Calculates Lambda invariant mass and decides if Lam or antiLam.
  const StThreeVectorF& posVec3 = v0Vertex->momentumOfDaughter(positive);
  const StThreeVectorF& negVec3 = v0Vertex->momentumOfDaughter(negative);
  posVec.setVect(posVec3);
  negVec.setVect(negVec3);
  float pVmag2 = posVec3.mag2();
  float nVmag2 = negVec3.mag2();

  posVec.setE(TMath::Sqrt(pVmag2+proton_mass_c2*proton_mass_c2));
  negVec.setE(TMath::Sqrt(nVmag2+pion_minus_mass_c2*pion_minus_mass_c2));
  mlam = (posVec+negVec).m();
  Bool_t lamCand = (TMath::Abs(mlam-lambda_mass_c2) < parsXi->dmass);

  posVec.setE(TMath::Sqrt(pVmag2+pion_plus_mass_c2*pion_plus_mass_c2));
  negVec.setE(TMath::Sqrt(nVmag2+proton_mass_c2*proton_mass_c2));
  mala = (posVec+negVec).m();
  Bool_t alaCand = (TMath::Abs(mala-lambda_mass_c2) < parsXi->dmass);
  
  AssignSign:
  if (alaCand)
     {charge=1;
      if (v0Vertex->dcaDaughterToPrimaryVertex(positive) < parsXi->bpn_v0) 
         {alaCand = kFALSE;
          goto AssignSign;
          }
      }
      else if (lamCand)
     {charge=-1;
      if (v0Vertex->dcaDaughterToPrimaryVertex(negative) < parsXi->bpn_v0)
         {lamCand = kFALSE;
          goto AssignSign;
          }
      }
      else
     {return usedV0;
      }

  //Julien
  v0Vtx=v0Vertex;//To be replaced
  storeV0Coords[0]=v0Vtx->position().x();
  storeV0Coords[1]=v0Vtx->position().y();
  storeV0Coords[2]=v0Vtx->position().z();
  printV0Coords=1;
  if (charge < 0) massV0=mlam;
  if (charge > 0) massV0=mala;
  //End Julien
  

  /// Loop over tracks (bachelors) to find Xis

  for (k=0; k<trks; k++)
     {StTrackGeometry* bachGeom=trk[k]->geometry();/////Added Julien.
      if (bachGeom == NULL) {printf("CAUTION : pointer bachGeom is null.\n");return usedV0;}
      if (charge*bachGeom->charge() > 0)
         {///Need to check here that this track is not used in the V0.
          /*pk=bachGeom->momentum();
          if (pk == NULL) {printf("CAUTION : pointer pk is null.\n");return usedV0;}*/
          //trkHelix=heli[k];
          //"heli" and "trk" : cf StRoot/St_dst_Maker/StV0FinderMaker.cxx (STAT).
          
          ///Determine detector id of pair for parsXi
          det_id_xi=TMath::Min(det_id_v0,detId[k]);
          ///Xi cut parameters
          parsXi=exipar->GetTable(det_id_xi-1);
          
          ///Cut on number of hits
          //if (hits[k] >= parsXi->n_point)
          if (true) //`struct exi_exipar_st' has no member named `n_point'
             {











          /// Beginning of the big(est) block inserted from StXiVertexFinder.cxx
          /// Every line in this block should be shifted by 4 spaces to the right.

          StHelixModel *bachGeom2 = new StHelixModel(bachGeom->charge(),bachGeom->psi(),bachGeom->curvature(),bachGeom->dipAngle(),bachGeom->origin(),bachGeom->momentum(),bachGeom->helicity());

          if ((bachGeom->origin().x()==0) && (bachGeom->origin().y()==0) && (bachGeom->origin().z()==0))
             {printf("CAUTION : bachelor candidate has all parameters = 0.\n");
              continue;
              }
          

          ///Calculation of the 2 intersection points between bachelor circle and V0 straight line
          
          /// Subroutine casc_geom
          iflag1=0;
          xc=bachGeom->origin().x()-bachGeom->helicity()*TMath::Sin(bachGeom->psi())/bachGeom->curvature();
          yc=bachGeom->origin().y()+bachGeom->helicity()*TMath::Cos(bachGeom->psi())/bachGeom->curvature();
          xd=xV0.x()-xc;
          yd=xV0.y()-yc;
          rsq=1/(bachGeom->curvature()*bachGeom->curvature());
          xOut[0]=0;
          yOut[0]=0;
          xOut[1]=0;
          yOut[1]=0;
          if (pV0.x() != 0)
             {atmp=pV0.y()/pV0.x();
              btmp=-atmp*xd+yd;
              dtmp=atmp*atmp+1.;
              ctmp=dtmp*rsq-btmp*btmp;
              if (ctmp < 0)
                 {dist=fabs(xd*pV0.y()-yd*pV0.x())/sqrt(pV0.x()*pV0.x()+pV0.y()*pV0.y());
                  if (dist >= (1/bachGeom->curvature()+parsXi->dca_max)) iflag1=5;
                     else
                    {iflag1=3;
                     ctmp=sqrt(rsq/(atmp*atmp+1.));
                     dtmp=-atmp*ctmp;
                     btmp=dtmp*xd+ctmp*yd;
                     if (btmp > 0.)
                        {xOut[0]=dtmp+xc;
                         yOut[0]=ctmp+yc;
                         }
                         else
                        {xOut[0]=-dtmp+xc;
                         yOut[0]=-ctmp+yc;
                         }
                     }
                  }
                  else
                 {if (ctmp == 0) iflag1=3;
                  ctmp=sqrt(ctmp);
                  abtmp=atmp*btmp;
                  btmp=btmp+yc;
                  xOut[0]=(-abtmp+ctmp)/dtmp+xc;
                  xOut[1]=(-abtmp-ctmp)/dtmp+xc;
                  yOut[0]=atmp*(xOut[0]-xc)+btmp;
                  yOut[1]=atmp*(xOut[1]-xc)+btmp;
                  }
              }
              else /// pV0.x()==0
             {xOut[0]=xV0.x();
              xOut[1]=xV0.x();
              ctmp=rsq-xd*xd;
              if (ctmp <= 0)
                 {dist=fabs(xd*pV0.y()-yd*pV0.x())/sqrt(pV0.x()*pV0.x()+pV0.y()*pV0.y());
                  if (dist >= (1/bachGeom->curvature()+parsXi->dca_max)) iflag1=5;
                     else
                    {iflag1=3;
                     yOut[0]=yc;
                     if (xV0.x() > xc) xOut[0]=xc+1/bachGeom->curvature();
                        else xOut[0]=xc-1/bachGeom->curvature();
                     }
                  }
                  else
                 {if (ctmp == 0) iflag1=3;
                  ctmp=sqrt(ctmp);
                  yOut[0]=yc+ctmp;
                  yOut[1]=yc-ctmp;
                  }
              }
          /// End of casc_geom
          tmpcalc=TMath::ATan(bachGeom->origin().y()/bachGeom->origin().x());
          if (bachGeom->origin().x() < 0) tmpcalc+=C_PI;
          if (tmpcalc < 0) tmpcalc+=2*C_PI;
          if (tmpcalc > 2*C_PI) tmpcalc-=2*C_PI;
          monTab[0]=sqrt(bachGeom->origin().x()*bachGeom->origin().x()+bachGeom->origin().y()*bachGeom->origin().y());
          monTab[1]=tmpcalc;
          monTab[2]=bachGeom->origin().z();
          if (iflag1 == 5) continue; ///No intersection points

         
          /// Loop over the 2 intersection points between bachelor circle and V0 straight line
          for (i=0;i<2;i++)
             {tries=1;
              StThreeVectorF dpV0;
              dpV0.setX(pV0.x()/abs(pV0));
              dpV0.setY(pV0.y()/abs(pV0));
              dpV0.setZ(pV0.z()/abs(pV0));
              
              
              ///Subroutine update_track_param
              rr=sqrt(bachGeom->origin().x()*bachGeom->origin().x()+bachGeom->origin().y()*bachGeom->origin().y());
              xi=bachGeom->origin().x();
              yi=bachGeom->origin().y();
              axb=(xi-xc)*(yOut[i]-yc)-(yi-yc)*(xOut[i]-xc);
              arg=axb/rsq;
              if (arg > 1.) arg=1.;
              if (arg < -1.) arg=-1.;
              ds=TMath::ASin(arg)/bachGeom->curvature();
              dz=ds*TMath::Tan(bachGeom->dipAngle());
              xOrig.setX(xOut[i]);
              if (xOut[i] == 0.) xOrig.setX(0.01);
              xOrig.setY(yOut[i]);
              xOrig.setZ(bachGeom->origin().z()-(bachGeom->charge()*bfield.z()/fabs(bachGeom->charge()*bfield.z()))*dz);
              bachGeom2->setOrigin(xOrig);
              bachGeom2->setPsi(bachGeom->psi()+TMath::ASin(arg));
              ///End of update_track_param

              
              ///Subroutine track_mom
              xOrig=bachGeom2->momentum();
              pt=sqrt(xOrig.x()*xOrig.x()+xOrig.y()*xOrig.y());
              xOrig.setX(pt*TMath::Cos(bachGeom2->psi()));
              xOrig.setY(pt*TMath::Sin(bachGeom2->psi()));
              ///End of track_mom
              

              pBach.setX(xOrig.x()/abs(xOrig));
              pBach.setY(xOrig.y()/abs(xOrig));
              pBach.setZ(xOrig.z()/abs(xOrig));
              dv0dotdb=dpV0.x()*pBach.x()+dpV0.y()*pBach.y()+dpV0.z()*pBach.z();
              diffc.setX(xV0.x()-xOut[i]);
              diffc.setY(xV0.y()-yOut[i]);
              diffc.setZ(xV0.z()-bachGeom2->origin().z());
              ///s1 and s2 are the distances from a point on the lines to the
              /// closest distance of approach of the lines in space
              denom=dv0dotdb*dv0dotdb-1.;
              s2=(dpV0.x()*dv0dotdb-pBach.x())*diffc.x() + (dpV0.y()*dv0dotdb-pBach.y())*diffc.y() + (dpV0.z()*dv0dotdb-pBach.z())*diffc.z();
              s2=s2/denom;
              ///Check validity of linear approx. (distance moved in x and y << r1)
              /// If only mildly invalid, re-try starting with new point (up to 3 tries)
              valid=fabs(s2*sqrt(pBach.x()*pBach.x()+pBach.y()*pBach.y()));
              while ((valid < (0.02/bachGeom->curvature())) && (tries < 4) && (valid > (0.001/bachGeom->curvature())))
                 {tries++;
                  batv.setX(pBach.x()*s2+xOut[i]);
                  batv.setY(pBach.y()*s2+yOut[i]);
                  batv.setZ(pBach.z()*s2+bachGeom2->origin().z());
                  
                  
                  ///Subroutine ev0_project_track
                  dtmp=xc-batv.x();
                  atmp=yc-batv.y();
                  if (atmp == 0.)
                     {if (dtmp >= 0) xAns=xc-1/bachGeom->curvature();
                         else xAns=xc+1/bachGeom->curvature();
                      yAns=yc;
                      }
                      else
                     {ctmp=dtmp/atmp;
                      yy=1/(bachGeom->curvature()*sqrt(ctmp*ctmp+1.));
                      zz=ctmp*yy;
                      if (atmp > 0.)
                         {xAns=-zz+xc;
                          yAns=-yy+yc;
                          }
                          else
                         {xAns=zz+xc;
                          yAns=yy+yc;
                          }
                      }
                  ///End of ev0_project_track
                  xOut[i]=xAns;
                  yOut[i]=yAns;
                  
                  
                  ///Subroutine update_track_param
                  rr=sqrt(bachGeom->origin().x()*bachGeom->origin().x()+bachGeom->origin().y()*bachGeom->origin().y());
                  xi=bachGeom->origin().x();
                  yi=bachGeom->origin().y();
                  axb=(xi-xc)*(yOut[i]-yc)-(yi-yc)*(xOut[i]-xc);
                  arg=axb/rsq;
                  if (arg > 1.) arg=1.;
                  if (arg < -1.) arg=-1.;
                  ds=TMath::ASin(arg)/bachGeom->curvature();
                  dz=ds*TMath::Tan(bachGeom->dipAngle());
                  xOrig.setX(xOut[i]);
                  if (xOut[i] == 0.) xOrig.setX(0.01);
                  xOrig.setY(yOut[i]);
                  xOrig.setZ(bachGeom->origin().z()-(bachGeom->charge()*bfield.z()/fabs(bachGeom->charge()*bfield.z()))*dz);
                  bachGeom2->setOrigin(xOrig);
                  bachGeom2->setPsi(bachGeom->psi()+TMath::ASin(arg));
                  ///End of update_track_param                  

                  
                  ///Subroutine track_mom
                  xOrig=bachGeom2->momentum();
                  pt=sqrt(xOrig.x()*xOrig.x()+xOrig.y()*xOrig.y());
                  xOrig.setX(pt*TMath::Cos(bachGeom2->psi()));
                  xOrig.setY(pt*TMath::Sin(bachGeom2->psi()));
                  ///End of track_mom
                  
                  pBach.setX(xOrig.x()/abs(xOrig));
                  pBach.setY(xOrig.y()/abs(xOrig));
                  pBach.setZ(xOrig.z()/abs(xOrig));
                  dv0dotdb=dpV0.x()*pBach.x()+dpV0.y()*pBach.y()+dpV0.z()*pBach.z();
                  diffc.setX(xV0.x()-xOut[i]);
                  diffc.setY(xV0.y()-yOut[i]);
                  diffc.setZ(xV0.z()-bachGeom2->origin().z());
                  ///s1 and s2 are the distances from a point on the lines to the
                  /// closest distance of approach of the lines in space
                  denom=dv0dotdb*dv0dotdb-1.;
                  s2=(dpV0.x()*dv0dotdb-pBach.x())*diffc.x() + (dpV0.y()*dv0dotdb-pBach.y())*diffc.y() + (dpV0.z()*dv0dotdb-pBach.z())*diffc.z();
                  s2=s2/denom;
                  ///Check validity of linear approx. (distance moved in x and y << r1)
                  /// If only mildly invalid, re-try starting with new point (up to 3 tries)
                  valid=fabs(s2*sqrt(pBach.x()*pBach.x()+pBach.y()*pBach.y()));
                  }///End of the while-loop.
              if ((valid < (0.02/bachGeom->curvature())) && (tries < 4))
                 {batv.setX(pBach.x()*s2+xOut[i]);
                  batv.setY(pBach.y()*s2+yOut[i]);
                  batv.setZ(pBach.z()*s2+bachGeom2->origin().z());
                  ///Beginning of block 5.
                  s1=(pBach.x()*dv0dotdb-dpV0.x())*diffc.x() + (pBach.y()*dv0dotdb-dpV0.y())*diffc.y() + (pBach.z()*dv0dotdb-dpV0.z())*diffc.z();
                  s1=-s1/denom;
                  v0atv.setX(dpV0.x()*s1+xV0.x());
                  v0atv.setY(dpV0.y()*s1+xV0.y());
                  v0atv.setZ(dpV0.z()*s1+xV0.z());
                  ///Check that V0 points away from Xi vertex
                  check=(xV0.x()-v0atv.x())*pV0.x() + (xV0.y()-v0atv.y())*pV0.y() + (xV0.z()-v0atv.z())*pV0.z();
                  ///End of block 5 and beginning of block 5bis.
                  
                  if (check > 0.0)
                     {dca=sqrt((v0atv.x()-batv.x())*(v0atv.x()-batv.x()) + (v0atv.y()-batv.y())*(v0atv.y()-batv.y()) + (v0atv.z()-batv.z())*(v0atv.z()-batv.z()));
                      xpp.setX((v0atv.x()+batv.x())/2.);
                      xpp.setY((v0atv.y()+batv.y())/2.);
                      xpp.setZ((v0atv.z()+batv.z())/2.);
                      rv=sqrt((xpp.x()-xPvx.x())*(xpp.x()-xPvx.x())+(xpp.y()-xPvx.y())*(xpp.y()-xPvx.y())+(xpp.z()-xPvx.z())*(xpp.z()-xPvx.z()));
                      ///decide here if it is a good candidate
                      if ((dca<=parsXi->dca_max) && (rv>parsXi->rv_xi))
                         {///calculate xi impact parameter
                          pXi.setX(pV0.x()+xOrig.x());
                          pXi.setY(pV0.y()+xOrig.y());
                          pXi.setZ(pV0.z()+xOrig.z());
                          ///Check that Xi points away from primary vertex
                          check=(xpp.x()-xPvx.x())*pXi.x()+(xpp.y()-xPvx.y())*pXi.y()+(xpp.z()-xPvx.z())*pXi.z();
                          if (check < 0.0)
                              iflag=2;
                              else
                             {///helixDCA(charge,xpp,pXi,bxi);
                              ///helixDCA is defined in exi_c_utils.cc (pams/global/exi/).
                              pt_tmp = sqrt(pXi.x()*pXi.x()+pXi.y()*pXi.y());
                              bcharge_tmp = charge*bfield.z()/tesla;
                              curvature_tmp = TMath::Abs(bcharge_tmp)*C_D_CURVATURE/pt_tmp;
                              dip_tmp = atan(pXi.z()/pt_tmp);
                              h_tmp = ((bcharge_tmp > 0) ? -1 : 1);
                              phase_tmp = atan2(pXi.y(),pXi.x())-(h_tmp*C_PI_2);
                              origin_tmp.setX(xpp.x());
                              origin_tmp.setY(xpp.y());
                              origin_tmp.setZ(xpp.z());
                              StHelixD *globHelix = new StHelixD(curvature_tmp, dip_tmp, phase_tmp, origin_tmp, h_tmp);
                              bxi=globHelix->distance(xPvx);
                              delete globHelix;
                              globHelix=0;
                              if (bxi <= parsXi->bxi_max) iflag=0;
                                 else iflag=1;
                              }
                          if (iflag == 0)
                             {///calculate parent and daughter kinematics
                              ptot_b2=xOrig.x()*xOrig.x()+xOrig.y()*xOrig.y()+xOrig.z()*xOrig.z();
                              epi=sqrt(ptot_b2+M_PION_MINUS*M_PION_MINUS);
                              ek=sqrt(ptot_b2+M_KAON_MINUS*M_KAON_MINUS);
                              ptot_v02=pV0.x()*pV0.x()+pV0.y()*pV0.y()+pV0.z()*pV0.z();
                              ela=sqrt(ptot_v02+M_LAMBDA*M_LAMBDA);
                              ptot_2=pXi.x()*pXi.x()+pXi.y()*pXi.y()+pXi.z()*pXi.z();
                              ptot=sqrt(ptot_2);
                              exi=sqrt(ptot_2+M_XI_MINUS*M_XI_MINUS);
                              eom=sqrt(ptot_2+M_OMEGA_MINUS*M_OMEGA_MINUS);
                              ///calculate Armenteros variables
                              bdotx=xOrig.x()*pXi.x()+xOrig.y()*pXi.y()+xOrig.z()*pXi.z();
                              vdotx=pV0.x()*pXi.x()+pV0.y()*pXi.y()+pV0.z()*pXi.z();
                              if (bachGeom->charge() > 0)
                                 {ppar=bdotx/ptot;
                                  npar=vdotx/ptot;
                                  pper=sqrt(ptot_b2-ppar*ppar);
                                  }
                                  else
                                 {ppar=vdotx/ptot;
                                  npar=bdotx/ptot;
                                  pper=sqrt(ptot_v02-ppar*ppar);
                                  }
                              ///calculate daughter impact parameters
                              bBach=trk[k]->impactParameter();
                              bV0=fabs(v0Vtx->dcaParentToPrimaryVertex());
                              xiVertex = new StXiVertex();
                              xiVertex->setPosition(xpp);
                              xiVertex->addDaughter(trk[k]);
                              xiVertex->setDcaBachelorToPrimaryVertex(trk[k]->impactParameter());
                              xiVertex->setMomentumOfBachelor(bachGeom->momentum());
                              xiVertex->setDcaDaughters(dca_k);
                              xiVertex->setDcaParentToPrimaryVertex(sqrt(rmin));
                              xiVertex->setV0Vertex(v0Vertex);
                              xiVertices.push_back(xiVertex);
                              usedV0 = kTRUE;
                              } ///End if (bxi and iflag check)
                          } ///End if (dca and rv)
                      } ///End if (check>0 : V0 pointing away from Xi)
                  } ///End if (valid && tries<4)
              if (iflag1 == 3) break; ///There is only 1 intersection point
              } ///End loop over 2 intersection points
          delete bachGeom2;
          bachGeom2=0;
          /// End of the big(est) block inserted from StXiVertexFinder.cxx











              
              } /// track n_point cut
          } /// charge sign
      } /// k-Loop

  if (alaCand) {
    alaCand = kFALSE;
    goto AssignSign;
  }

  return usedV0;
}
//_____________________________________________________________________________
// $Id: StXiFinderMaker.cxx,v 1.1 2003/04/09 16:44:27 faivre Exp $
// $Log: StXiFinderMaker.cxx,v $
// Revision 1.1  2003/04/09 16:44:27  faivre
// First version of xxx
//
//
