//////////////////////////////////////////////////////////////////////////
///                                                                    ///
/// StXiFinderMaker class (finds Xi secondary vertices)                ///
///                                                                    ///
//////////////////////////////////////////////////////////////////////////
//
//  Cuts can be found in the code by comments beginning with "Cut:"
//
//


#include "StXiFinderMaker.h"
#include "StMessMgr.h"
#include "StEvent/StEventTypes.h"
#include "TMath.h"
#include "TVector2.h"
#include "tables/St_exi_exipar_Table.h"
#include "PhysicalConstants.h"
#include "phys_constants.h"


StSPtrVecXiVertex* vecXi=0;



//_____________________________________________________________________________
StXiFinderMaker::StXiFinderMaker(const char *name):StV0FinderMaker(name),
exipar(0),parsXi(0),xiVertex(0),det_id_xi(0){}

//_____________________________________________________________________________
StXiFinderMaker::~StXiFinderMaker() {}
//_____________________________________________________________________________
Int_t StXiFinderMaker::Init()
{bool a,b,c;
 if ((useTracker!=kTrackerUseTPT) && (useTracker!=kTrackerUseITTF) && (useTracker!=kTrackerUseBOTH))
    {gMessMgr->Error("StXiFinderMaker::Init() : wrong TrackerUsage parameter set.");
     return kStErr;
     }
 if ((useSVT!=kNoSVT) && (useSVT!=kUseSVT))
    {gMessMgr->Error("StXiFinderMaker::Init() : wrong SVTUsage parameter set.");
     return kStErr;
     }
 if ((useEventModel!=kUseStEvent) && (useEventModel!=kUseMuDst))
    {gMessMgr->Error("StXiFinderMaker::Init() : wrong EventModelUsage parameter set.");
     return kStErr;
     }
 if ((useLikesign!=kLikesignUseStandard) && (useLikesign!=kLikesignUseLikesign))
    {gMessMgr->Error("StXiFinderMaker::Init() : wrong LikesignUsage parameter set.");
     return kStErr;
     }
 if ((useRotating!=kRotatingUseStandard) && (useRotating!=kRotatingUseRotating) && (useRotating!=kRotatingUseSymmetry) && (useRotating!=kRotatingUseRotatingAndSymmetry))
    {gMessMgr->Error("StXiFinderMaker::Init() : wrong RotatingUsage parameter set.");
     return kStErr;
     }
 
 if (useTracker == kTrackerUseTPT) gMessMgr->Info()<<"StXiFinderMaker : use TPT tracks."<<endm;
 if (useTracker == kTrackerUseITTF) gMessMgr->Info()<<"StXiFinderMaker : use ITTF tracks."<<endm;
 if (useTracker == kTrackerUseBOTH) gMessMgr->Info()<<"StXiFinderMaker : use TPT *and* ITTF tracks."<<endm;
 if (useSVT == kUseSVT) gMessMgr->Info()<<"StXiFinderMaker : use SVT points if possible."<<endm;
 if (useSVT == kNoSVT) gMessMgr->Info()<<"StXiFinderMaker : do not use SVT points."<<endm;
 if (useEventModel == kUseStEvent) gMessMgr->Info()<<"StXiFinderMaker : expect StEvent files in input."<<endm;
 if (useEventModel == kUseMuDst)  gMessMgr->Info()<<"StXiFinderMaker : expect MuDst files in input."<<endm;
 if (useLikesign == kLikesignUseLikesign) gMessMgr->Info()<<"StXiFinderMaker : does like-sign finding."<<endm;
 if (useRotating == kRotatingUseRotating) gMessMgr->Info()<<"StXiFinderMaker : does rotating finding."<<endm;
 if (useRotating == kRotatingUseSymmetry) gMessMgr->Info()<<"StXiFinderMaker : does symmetry finding."<<endm;
 if (useRotating == kRotatingUseRotatingAndSymmetry) gMessMgr->Info()<<"StXiFinderMaker : does rotating + symmetry finding."<<endm;
 
 if (useLanguage != kLanguageUseSpecial)
    {a=(bool)(1&(useLanguage>>2));
     b=(bool)(1&(useLanguage>>1));
     c=(bool)(1&useLanguage);
     useV0Language=2*(!(a^c))+(a|c);
     useXiLanguage=4*(b&(!(a^c)))+2*(a&b&(!c))+(a|c);
     }
 switch (useLanguage)
    {case kLanguageUseOldRun : gMessMgr->Info()<<"StXiFinderMaker : Fortran run."<<endm;
                               break;
     case kLanguageUseRun : gMessMgr->Info()<<"StXiFinderMaker : C++ run."<<endm;
                            break;
     case kLanguageUseTestV0Finder : gMessMgr->Info()<<"StXiFinderMaker : Test V0Finder."<<endm;
                                     break;
     case kLanguageUseTestXiFinder : gMessMgr->Info()<<"StXiFinderMaker : Test XiFinder."<<endm;
                                     break;
     case kLanguageUseTestBothFinders : gMessMgr->Info()<<"StXiFinderMaker : Test V0Finder and XiFinder."<<endm;
                                        break;
     case kLanguageUseSpecial : break;
     default : gMessMgr->Error("StXiFinderMaker::Init() : wrong LanguageUsage parameter set.");
               return kStErr;
     }
 if ((useXiLanguage!=kXiLanguageUseFortran) && (useXiLanguage!=kXiLanguageUseCppOnFortranV0) && (useXiLanguage!=kXiLanguageUseCppOnCppV0) && (useXiLanguage!=kXiLanguageUseFortranAndCppOnFortranV0) && (useXiLanguage!=kXiLanguageUseFortranAndCppOnCppV0) && (useXiLanguage!=kXiLanguageUseBothCpp) && (useXiLanguage!=kXiLanguageUseAll))
    {gMessMgr->Error("StXiFinderMaker::Init() : wrong XiLanguageUsage parameter set.");
     return kStErr;
     }
 switch (useV0Language)
    {case kV0LanguageUseFortran : if ((useXiLanguage!=kXiLanguageUseFortran) && (useXiLanguage!=kXiLanguageUseCppOnFortranV0) && (useXiLanguage!=kXiLanguageUseFortranAndCppOnFortranV0))
                                     {gMessMgr->Info()<<"StXiFinderMaker : BE CAREFUL : impossible combination asked."<<endm;
                                      gMessMgr->Info()<<"StXiFinderMaker :    Set it to testXiFinder."<<endm;
                                      useXiLanguage=kXiLanguageUseFortranAndCppOnFortranV0;
                                      }
                                  break;
     case kV0LanguageUseCpp : if (useXiLanguage!=kXiLanguageUseCppOnCppV0)
                                 {gMessMgr->Info()<<"StXiFinderMaker : BE CAREFUL : impossible combination asked."<<endm;
                                  gMessMgr->Info()<<"StXiFinderMaker :    Set it to normalRun."<<endm;
                                  useXiLanguage=kXiLanguageUseCppOnCppV0;
                                  }
                              break;
     case kV0LanguageUseBoth : break;
     default : gMessMgr->Error("StXiFinderMaker::Init() : wrong V0LanguageUsage parameter set.");
               return kStErr;
     }
 if (1&useV0Language) gMessMgr->Info()<<"StXiFinderMaker :    Will store Fortran V0s."<<endm;
 if (2&useV0Language) gMessMgr->Info()<<"StXiFinderMaker :    Will store C++ V0s."<<endm;
 if (1&useXiLanguage) gMessMgr->Info()<<"StXiFinderMaker :    Will store Fortran Xis."<<endm;
 if (2&useXiLanguage) gMessMgr->Info()<<"StXiFinderMaker :    Will store C++ Xis made with Fortran V0s."<<endm;
 if (4&useXiLanguage) gMessMgr->Info()<<"StXiFinderMaker :    Will store C++ Xis made with C++ V0s."<<endm;

 if (useEventModel) //initialize mMuDstMaker
    {mMuDstMaker = (StMuDstMaker*)GetMaker("myMuDstMaker");
     if(!mMuDstMaker) gMessMgr->Warning("StXiFinderMaker::Init can't find a valid MuDst.");
     }
 
 return StMaker::Init();
 }
//=============================================================================
Int_t StXiFinderMaker::InitRun(int runumber) {
 exipar = (St_exi_exipar*) GetDataBase("global/vertices/exipar");
 if (!exipar)
    {gMessMgr->Error("StXiFinderMaker::Init() : could not find exipar in database.");
     return kStErr;
     }
  return StMaker::InitRun(runumber);
 }
//=============================================================================
//_____________________________________________________________________________
Int_t StXiFinderMaker::Make() {

  Int_t iRes;

  // Prepare event and track variables
  iRes = Prepare();
  if (iRes != kStOk) return iRes;

  StSPtrVecXiVertex& xiVertices = event->xiVertices();
  gMessMgr->Info()<<"StXiFinderMaker : coming in I have "<<xiVertices.size()<<" Xis."<<endm;
  /**if (dontZapV0s && !dontZapXis)
     {// Erase existing Xis
      // Already done if erasing V0s
      StSPtrVecXiVertex xiVertices2;
      xiVertices = xiVertices2;
      }
     else if (dontZapXis && !dontZapV0s)
     {gMessMgr->Warning() << "StXiFinderMaker: must not zap V0s if not zapping Xis\n"
      << "      Automatically switching to keep V0s." << endm;
      DontZapV0s();
      }*/

  if (!(1&useXiLanguage) && !((4&useXiLanguage) && (!(1&useV0Language))))
     {// Erase existing Xis
      gMessMgr->Info() << "StXiFinderMaker : pre-existing Xis deleted." << endm;
      StSPtrVecXiVertex xiVertices2;
      xiVertices = xiVertices2;
      }
  vecXi = &xiVertices;

  // Call the V0-finding, which will in turn call
  // the UseV0() member function for each V0 found
  
  if (2&useXiLanguage)
     {StSPtrVecV0Vertex& v0Vertices = event->v0Vertices();
     unsigned int nV0s = v0Vertices.size();
     det_id_v0 = 1; // for lack of any further information
     for (unsigned int i=0; i<nV0s; i++)
        {v0Vertex = v0Vertices[i];
         if (v0Vertex) UseV0();
         }
     }
  if ((4&useXiLanguage) || (2&useV0Language))
    {iRes = StV0FinderMaker::Make();
     if (iRes != kStOk) return iRes;
     }

  gMessMgr->Info()<<"StXiFinderMaker : now I have "<<xiVertices.size()<<" Xis."<<endm;

  return kStOk;
}
//_____________________________________________________________________________
Bool_t StXiFinderMaker::UseV0() {
  
  Bool_t usedV0 = kFALSE;
  
  if ((!(2&useXiLanguage)) && (!(4&useXiLanguage))) return usedV0;
  if (useXiLanguage<6)
    {
      if ((2&useXiLanguage) && (v0Vertex->chiSquared()<0)) return usedV0;
      if ((4&useXiLanguage) && (v0Vertex->chiSquared()>=0)) return usedV0;
    }
  
  long myChi = -1*(long)v0Vertex->chiSquared();
  
  if( !useSVT && (myChi&(( long)1<<3))) return usedV0;
  if( useSVT && !(myChi&(( long)1<<3))) return usedV0;


  /// Variables:
  unsigned int k;
  int iflag1,i,charge,tries,negKey,posKey;
  double mlam,mala,ptV0,ptBach,radiusBach,rsq;
  StPhysicalHelixD /**trkHelix,*/tmpHelix;
  StThreeVectorF xPvx;
  StThreeVectorD xpp,pp,impact,tmp3V,xV0,pV0,dpV0;
  StLorentzVectorD posVec,negVec;
  
  //Subroutine casc_geom
  double xc,yc,xd,yd,atmp,btmp,ctmp,dtmp,abtmp,xOut[2],yOut[2],dist;
  
  //Subroutine update_track_param
  double arg,axb,ds,dz;
  StThreeVectorF xOrig;
  
  //After subroutine track_mom
  double dv0dotdb,denom,s2,valid,xAns,yAns,yy,zz,s1;
  StThreeVectorF pBach,diffc,batv,v0atv;
  
  //Bloc 5bis
  double dca,bxi;
  double ptot_v02,bdotx,vdotx,ppar,pper;
  StThreeVectorF pXi;
  
  //Function helixDCA
  int h_tmp;
  double pt_tmp,bcharge_tmp,curvature_tmp,dip_tmp,phase_tmp;
  StThreeVectorD origin_tmp;
  
  //Rotating
  double epsDipAngle,cstPsi;
  StThreeVectorF epsOrigin,epsMomentum,cstOrigin;
  

  // All the stuff without which you can't even begin to work
  StSPtrVecXiVertex& xiVertices = *vecXi;
  charge=0;
  xPvx=mainv;
  negKey=v0Vertex->daughter(negative)->key();
  posKey=v0Vertex->daughter(positive)->key();
  parsXi = exipar->GetTable(det_id_v0-1); //Xi cut parameters using detector id from V0
  xV0=v0Vertex->position();
  impact = xV0-xPvx;
  //Cut: V0 decay length
  if (impact.mag2() < (parsXi->rv_v0*parsXi->rv_v0)) return usedV0;
  pV0=v0Vertex->momentum();
  dpV0.setX(pV0.x()/abs(pV0));
  dpV0.setY(pV0.y()/abs(pV0));
  dpV0.setZ(pV0.z()/abs(pV0));
  ptV0=::sqrt(pV0.x()*pV0.x()+pV0.y()*pV0.y());
  
  // All the stuff needed to make rotating simpler inside the loop
  epsDipAngle=1.;
  epsOrigin.setX(1.);
  epsOrigin.setY(1.);
  epsOrigin.setZ(1.);
  epsMomentum.setX(1.);
  epsMomentum.setY(1.);
  epsMomentum.setZ(1.);
  cstPsi=0.;
  cstOrigin.setX(0.);
  cstOrigin.setY(0.);
  cstOrigin.setZ(0.);
  if (1&useRotating)
     {epsOrigin.setX(-1.);
      epsOrigin.setY(-1.);
      epsMomentum.setX(-1.);
      epsMomentum.setY(-1.);
      cstPsi=M_PI;
      cstOrigin.setX(2*xPvx.x());
      cstOrigin.setY(2*xPvx.y());
      }
  if (2&useRotating)
     {epsDipAngle=-1.;
      epsOrigin.setZ(-1.);
      epsMomentum.setZ(-1.);
      cstOrigin.setZ(2*xPvx.z());
      }

  // Calculates Lambda invariant mass and decides if Lam or antiLam.
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
  
  //Cut: Lambda invariant mass
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
  charge=-(useLikesign-1)*charge;

  StHelixModel* bachGeom = new StHelixModel;
  if (bachGeom == NULL) {gMessMgr->Info()<<"StXiFinderMaker : CAUTION : pointer bachGeom is null."<<endm; return usedV0;}
  StHelixModel* bachGeom2 = new StHelixModel;
  if (bachGeom2 == NULL) {gMessMgr->Info()<<"StXiFinderMaker : CAUTION : pointer bachGeom2 is null."<<endm; return usedV0;}
  
  // Loop over tracks (bachelors) to find Xis
  for (k=0; k<trks; k++)
     {bachGeom->setCharge(trk[k]->geometry()->charge());
      bachGeom->setHelicity(trk[k]->geometry()->helicity());
      bachGeom->setCurvature(trk[k]->geometry()->curvature());
      bachGeom->setPsi(trk[k]->geometry()->psi()+cstPsi);
      bachGeom->setDipAngle(epsDipAngle*trk[k]->geometry()->dipAngle());
      bachGeom->setOrigin(cstOrigin+epsOrigin.pseudoProduct(trk[k]->geometry()->origin()));
      bachGeom->setMomentum(epsMomentum.pseudoProduct(trk[k]->geometry()->momentum()));
      //Check that the sign of the bachelor is the one we want.
      if (charge*bachGeom->charge() < 0) continue;
      //Check that ITTF and TPT tracks/V0's are not combined together.
      if (GetTrackerUsage() == kTrackerUseBOTH)
         {
	   //if ((v0Vertex->dcaDaughters() <= 0) && (trk[k]->fittingMethod() == TPTflag)) continue;
	   if ((v0Vertex->dcaDaughters() <= 0) && (trk[k]->fittingMethod() != ITTFflag)) continue;
	   if ((v0Vertex->dcaDaughters() >= 0) && (trk[k]->fittingMethod() == ITTFflag)) continue;
	 }
      //Check that the bachelor is not one of the V0's daughters.
      if (trkID[k] == negKey) continue;
      if (trkID[k] == posKey) continue;
      ///trkHelix=heli[k];
      
      //Determine detector id of pair for parsXi
      det_id_xi=TMath::Max(det_id_v0,detId[k]);
      //Xi cut parameters
      parsXi=exipar->GetTable(det_id_xi-1);
      
      //Book a temporary clone of the bachelor helix, whose origin will be moved
      bachGeom2->setCharge(bachGeom->charge());
      bachGeom2->setHelicity(bachGeom->helicity());
      bachGeom2->setCurvature(bachGeom->curvature());
      bachGeom2->setPsi(bachGeom->psi());
      bachGeom2->setDipAngle(bachGeom->dipAngle());
      bachGeom2->setOrigin(bachGeom->origin());
      bachGeom2->setMomentum(bachGeom->momentum());
      if ((bachGeom->origin().x()==0) && (bachGeom->origin().y()==0) && (bachGeom->origin().z()==0))
         {gMessMgr->Info()<<"StXiFinderMaker : CAUTION : bachelor candidate has all parameters = 0."<<endm;
          continue;
          }
      ptBach=pt[k];
      radiusBach=1./bachGeom->curvature();
      rsq=radiusBach*radiusBach;
      
      // Calculation of the 2 intersection points between bachelor circle and V0 straight line
      //Subroutine casc_geom
      iflag1=2;
      xc=bachGeom->origin().x()-bachGeom->helicity()*radiusBach*TMath::Sin(bachGeom->psi());
      yc=bachGeom->origin().y()+bachGeom->helicity()*radiusBach*TMath::Cos(bachGeom->psi());
      xd=xV0.x()-xc;
      yd=xV0.y()-yc;
      xOut[0]=0.;
      yOut[0]=0.;
      xOut[1]=0.;
      yOut[1]=0.;
      if (pV0.x() != 0)
         {atmp=pV0.y()/pV0.x();
          btmp=-atmp*xd+yd;
          dtmp=atmp*atmp+1.;
          ctmp=dtmp*rsq-btmp*btmp;
          if (ctmp < 0)
             {dist=fabs(xd*pV0.y()-yd*pV0.x())/ptV0;
              if (dist >= (radiusBach+parsXi->dca_max)) iflag1=0;
                  else
                 {iflag1=1;
                  ctmp=::sqrt(rsq/(atmp*atmp+1.));
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
             {if (ctmp == 0) iflag1=1;
              ctmp=::sqrt(ctmp);
              abtmp=atmp*btmp;
              btmp=btmp+yc;
              xOut[0]=(-abtmp+ctmp)/dtmp+xc;
              xOut[1]=(-abtmp-ctmp)/dtmp+xc;
              yOut[0]=atmp*(xOut[0]-xc)+btmp;
              yOut[1]=atmp*(xOut[1]-xc)+btmp;
              }
          }
          else //pV0.x()==0
         {xOut[0]=xV0.x();
          xOut[1]=xV0.x();
          ctmp=rsq-xd*xd;
          if (ctmp <= 0)
             {dist=fabs(xd*pV0.y()-yd*pV0.x())/ptV0;
              if (dist >= (radiusBach+parsXi->dca_max)) iflag1=0;
                  else
                 {iflag1=1;
                  yOut[0]=yc;
                  if (xV0.x() > xc) xOut[0]=xc+radiusBach;
                     else xOut[0]=xc-radiusBach;
                  }
              }
              else
             {if (ctmp == 0) iflag1=1;
              ctmp=::sqrt(ctmp);
              yOut[0]=yc+ctmp;
              yOut[1]=yc-ctmp;
              }
          }
      //End of casc_geom
      //Cut: drop candidate if no intersection point in 2D between V0 and bachelor trajectories
      if (iflag1 == 0) continue; //No intersection points
      
      // Loop over the (1 or) 2 intersection points between bachelor circle and V0 straight line
      for (i=0;i<iflag1;i++)
         {//Subroutine update_track_param
          axb=(bachGeom->origin().x()-xc)*(yOut[i]-yc)-(bachGeom->origin().y()-yc)*(xOut[i]-xc);
          arg=axb/rsq;
          if (arg > 1.) arg=1.;
          if (arg < -1.) arg=-1.;
          ds=radiusBach*TMath::ASin(arg);
          dz=ds*TMath::Tan(bachGeom->dipAngle());
          xOrig.setX(xOut[i]);
          if (xOut[i] == 0.) xOrig.setX(0.01);
          xOrig.setY(yOut[i]);
          xOrig.setZ(bachGeom->origin().z()-(bachGeom->charge()*(Bfield/tesla)/fabs(bachGeom->charge()*(Bfield/tesla)))*dz); //Field wanted in tesla
          bachGeom2->setOrigin(xOrig);
          bachGeom2->setPsi(bachGeom->psi()+TMath::ASin(arg));
          //End of update_track_param
          
          //Subroutine track_mom
          xOrig=bachGeom2->momentum();
          xOrig.setX(ptBach*TMath::Cos(bachGeom2->psi()));
          xOrig.setY(ptBach*TMath::Sin(bachGeom2->psi()));
          //End of track_mom
          
          pBach.setX(xOrig.x()/abs(xOrig));
          pBach.setY(xOrig.y()/abs(xOrig));
          pBach.setZ(xOrig.z()/abs(xOrig));
          dv0dotdb=dpV0.x()*pBach.x()+dpV0.y()*pBach.y()+dpV0.z()*pBach.z();
          diffc.setX(xV0.x()-xOut[i]);
          diffc.setY(xV0.y()-yOut[i]);
          diffc.setZ(xV0.z()-bachGeom2->origin().z());
          //s1 and s2 are the distances from a point on the lines to the
          // closest distance of approach of the lines in space
          denom=dv0dotdb*dv0dotdb-1.;
          s2=(dpV0.x()*dv0dotdb-pBach.x())*diffc.x() + (dpV0.y()*dv0dotdb-pBach.y())*diffc.y() + (dpV0.z()*dv0dotdb-pBach.z())*diffc.z();
          s2=s2/denom;
          //Check validity of linear approx. (distance moved in x and y << r1)
          // If only mildly invalid, re-try starting with new point (up to 3 tries)
          tries=1;
          valid=fabs(s2*s2*(pBach.x()*pBach.x()+pBach.y()*pBach.y()));
          //Cut: drop candidate if linear approximation too bad to determine the dca
          while ((valid < (0.0004*rsq)) && (tries < 4) && (valid > (rsq*1.e-6)))
             {tries++;
              batv.setX(pBach.x()*s2+xOut[i]);
              batv.setY(pBach.y()*s2+yOut[i]);
              batv.setZ(pBach.z()*s2+bachGeom2->origin().z());
              
              //Subroutine ev0_project_track
              dtmp=xc-batv.x();
              atmp=yc-batv.y();
              if (atmp == 0.)
                 {if (dtmp >= 0) xAns=xc-radiusBach;
                     else xAns=xc+radiusBach;
                  yAns=yc;
                  }
                  else
                 {ctmp=dtmp/atmp;
                  yy=radiusBach/::sqrt(ctmp*ctmp+1.);
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
              //End of ev0_project_track
              xOut[i]=xAns;
              yOut[i]=yAns;
              
              //Subroutine update_track_param
              axb=(bachGeom->origin().x()-xc)*(yOut[i]-yc)-(bachGeom->origin().y()-yc)*(xOut[i]-xc);
              arg=axb/rsq;
              if (arg > 1.) arg=1.;
              if (arg < -1.) arg=-1.;
              ds=radiusBach*TMath::ASin(arg);
              dz=ds*TMath::Tan(bachGeom->dipAngle());
              xOrig.setX(xOut[i]);
              if (xOut[i] == 0.) xOrig.setX(0.01);
              xOrig.setY(yOut[i]);
              xOrig.setZ(bachGeom->origin().z()-(bachGeom->charge()*(Bfield/tesla)/fabs(bachGeom->charge()*(Bfield/tesla)))*dz); //Field wanted in tesla
              bachGeom2->setOrigin(xOrig);
              bachGeom2->setPsi(bachGeom->psi()+TMath::ASin(arg));
              //End of update_track_param                  

              //Subroutine track_mom
              xOrig=bachGeom2->momentum();
              xOrig.setX(ptBach*TMath::Cos(bachGeom2->psi()));
              xOrig.setY(ptBach*TMath::Sin(bachGeom2->psi()));
              //End of track_mom
              
              pBach.setX(xOrig.x()/abs(xOrig));
              pBach.setY(xOrig.y()/abs(xOrig));
              pBach.setZ(xOrig.z()/abs(xOrig));
              dv0dotdb=dpV0.x()*pBach.x()+dpV0.y()*pBach.y()+dpV0.z()*pBach.z();
              diffc.setX(xV0.x()-xOut[i]);
              diffc.setY(xV0.y()-yOut[i]);
              diffc.setZ(xV0.z()-bachGeom2->origin().z());
              denom=dv0dotdb*dv0dotdb-1.;
              s2=(dpV0.x()*dv0dotdb-pBach.x())*diffc.x() + (dpV0.y()*dv0dotdb-pBach.y())*diffc.y() + (dpV0.z()*dv0dotdb-pBach.z())*diffc.z();
              s2=s2/denom;
              valid=fabs(s2*s2*(pBach.x()*pBach.x()+pBach.y()*pBach.y()));
              }//End of the while-loop.
          //Cut: (same as above) drop candidate if linear approximation too bad to determine the dca
          if ((valid < (0.0004*rsq)) && (tries < 4))
             {batv.setX(pBach.x()*s2+xOut[i]);
              batv.setY(pBach.y()*s2+yOut[i]);
              batv.setZ(pBach.z()*s2+bachGeom2->origin().z());
              //Beginning of block 5.
              s1=(pBach.x()*dv0dotdb-dpV0.x())*diffc.x() + (pBach.y()*dv0dotdb-dpV0.y())*diffc.y() + (pBach.z()*dv0dotdb-dpV0.z())*diffc.z();
              s1=-s1/denom;
              v0atv.setX(dpV0.x()*s1+xV0.x());
              v0atv.setY(dpV0.y()*s1+xV0.y());
              v0atv.setZ(dpV0.z()*s1+xV0.z());
              //End of block 5 and beginning of block 5bis.
              //Cut: remove if V0 points away from Xi vertex
              if (((xV0.x()-v0atv.x())*pV0.x() + (xV0.y()-v0atv.y())*pV0.y() + (xV0.z()-v0atv.z())*pV0.z()) <= 0.) continue;
              dca=(v0atv.x()-batv.x())*(v0atv.x()-batv.x()) + (v0atv.y()-batv.y())*(v0atv.y()-batv.y()) + (v0atv.z()-batv.z())*(v0atv.z()-batv.z());
              //Cut: dca Xi daughters
              if (dca > (parsXi->dca_max*parsXi->dca_max)) continue;
              xpp.setX((v0atv.x()+batv.x())/2.);
              xpp.setY((v0atv.y()+batv.y())/2.);
              xpp.setZ((v0atv.z()+batv.z())/2.);
              //Cut: Xi decay length
              if (((xpp.x()-xPvx.x())*(xpp.x()-xPvx.x())+(xpp.y()-xPvx.y())*(xpp.y()-xPvx.y())+(xpp.z()-xPvx.z())*(xpp.z()-xPvx.z())) <= (parsXi->rv_xi*parsXi->rv_xi)) continue;
              //Calculate xi impact parameter
              pXi.setX(pV0.x()+xOrig.x());
              pXi.setY(pV0.y()+xOrig.y());
              pXi.setZ(pV0.z()+xOrig.z());
              //Cut: remove if Xi points away from primary vertex
              if (((xpp.x()-xPvx.x())*pXi.x()+(xpp.y()-xPvx.y())*pXi.y()+(xpp.z()-xPvx.z())*pXi.z()) < 0.0) continue;
              //Calculate pt-Armenteros
              ptot_v02=ptV0*ptV0+pV0.z()*pV0.z();
              bdotx=xOrig.x()*pXi.x()+xOrig.y()*pXi.y()+xOrig.z()*pXi.z();
              vdotx=pV0.x()*pXi.x()+pV0.y()*pXi.y()+pV0.z()*pXi.z();
              if (bachGeom->charge() > 0)
                 {ppar=bdotx/pXi.mag();
                  pper=(ptot[k]*ptot[k]-ppar*ppar);
                  pper=(pper>0)? ::sqrt(ptot[k]*ptot[k]-ppar*ppar):0;
                 }
                  else
                 {ppar=vdotx/pXi.mag();
                  pper=(ptot_v02-ppar*ppar);
                  pper=(pper>0)? ::sqrt(ptot_v02-ppar*ppar):0;
                 }
              //Cut: pt-Armanteros
              if (pper > 0.33) continue;
              //Function helixDCA(charge,xpp,pXi,bxi);
              //helixDCA is defined in exi_c_utils.cc (pams/global/exi/).
              pt_tmp = ::sqrt(pXi.x()*pXi.x()+pXi.y()*pXi.y());
              bcharge_tmp = charge*(Bfield/kilogauss); //Field wanted in kGauss
              curvature_tmp = TMath::Abs(bcharge_tmp)*C_D_CURVATURE/pt_tmp;
              dip_tmp = atan(pXi.z()/pt_tmp);
              h_tmp = ((bcharge_tmp > 0) ? -1 : 1);
              phase_tmp = atan2(pXi.y(),pXi.x())-(h_tmp*M_PI_2);
              origin_tmp.setX(xpp.x());
              origin_tmp.setY(xpp.y());
              origin_tmp.setZ(xpp.z());
              StHelixD *globHelix = new StHelixD(curvature_tmp, dip_tmp, phase_tmp, origin_tmp, h_tmp);
              bxi=globHelix->distance(xPvx);
              //End of helixDCA
              //Cut: dca Xi to primary vertex
              if (bxi > parsXi->bxi_max)
                 {delete globHelix;
                  globHelix=0;
                  continue;
                  }
              StThreeVectorD p1 = globHelix->at(globHelix->pathLength(xPvx));
              StThreeVectorD p2(p1.x()-globHelix->xcenter(),p1.y()-globHelix->ycenter(),0);
              StThreeVectorD p3(xPvx.x()-globHelix->xcenter(),xPvx.y()-globHelix->ycenter(),0);
              if (p3.mag2() > p2.mag2()) bxi=-bxi;
              delete globHelix;
              globHelix=0;
              xiVertex = new StXiVertex();
              xiVertex->setPosition(xpp);
              xiVertex->addDaughter(trk[k]);
              xiVertex->setDcaBachelorToPrimaryVertex(trk[k]->impactParameter());
              xiVertex->setMomentumOfBachelor(xOrig);
              xiVertex->setDcaDaughters(::sqrt(dca));
              xiVertex->setDcaParentToPrimaryVertex(bxi);
              xiVertex->setV0Vertex(v0Vertex);
              
              ///Begin Betty
              //Set chi2 to trace SVT usage
              long int v0ChiSq = -1*(long int)v0Vertex->chiSquared();
              if(detId[k]==2 || detId[k]==3)
                 {//if an SVT track was used on bachelor set the last bit to 1
                  v0ChiSq |=((long int)1 << 0);
                  }
              v0ChiSq *=-1;
              xiVertex->setChiSquared((float)v0ChiSq);
              ///End Betty
              
              xiVertices.push_back(xiVertex);
              usedV0 = kTRUE;
              } //End if (valid && tries<4)
          } //End loop over 2 intersection points
      } // k-Loop
  delete bachGeom;
  bachGeom=0;
  delete bachGeom2;
  bachGeom2=0;

  if (alaCand)
     {alaCand = kFALSE;
      goto AssignSign;
      }

  return usedV0;
}
//_____________________________________________________________________________
// $Id: StXiFinderMaker.cxx,v 1.25 2017/01/06 21:01:50 smirnovd Exp $
// $Log: StXiFinderMaker.cxx,v $
// Revision 1.25  2017/01/06 21:01:50  smirnovd
// Use pi constant from standard library, s/C_PI/M_PI/
//
// Revision 1.24  2016/12/12 17:18:04  smirnovd
// Removed outdated ClassImp ROOT macro
//
// Revision 1.23  2008/04/03 19:58:36  fisyak
// move parameters initialization from Init into InitRun
//
// Revision 1.22  2005/07/19 22:10:14  perev
// STARFPE
//
// Revision 1.21  2004/04/15 20:15:53  jeromel
// Forgot one of them
//
// Revision 1.20  2004/04/02 08:57:34  faivre
// Use actual TPT flag rather than "not ITTF" for TPT tracks.
//
// Revision 1.19  2004/02/05 16:52:28  faivre
// Add cut pt-Armanteros > 0.33 ; small cleanup.
//
// Revision 1.18  2004/02/04 14:53:36  faivre
// Remove bug introduced in previous version (confusion dca/bxi when copy-pasting from helixDCA).
//
// Revision 1.17  2004/02/04 14:24:58  faivre
// Add sign of dcaXiDaughters, slightly move cuts, small cleanup.
//
// Revision 1.16  2004/02/03 14:29:36  faivre
// Spring-cleaning 2 months early ;-)  Algo strictly equivalent to previous one although huge reshaping.
//
// Revision 1.15  2004/02/02 12:10:54  faivre
// XiFinder now able to run on muDsts as well :-)   + update user-friendliness.
//
// Revision 1.14  2003/11/08 18:26:00  faivre
// Bfield + consistency int/short
//
// Revision 1.13  2003/09/17 12:00:23  faivre
// RH8 : initialize everything in constructor.
//
// Revision 1.12  2003/09/02 17:58:59  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.11  2003/08/22 17:47:14  caines
// Get sign AND magnitude of mag field correctly for Xi and V0 finder
//
// Revision 1.10  2003/07/15 17:39:16  faivre
// Doesn't take Bfield from gufld anymore (takes Bfield calculated in V0Finder).
//
// Revision 1.9  2003/07/04 17:52:46  faivre
// Use SVT cuts if any dg has a SVT hit.
//
// Revision 1.8  2003/06/24 16:20:11  faivre
// Uses SVT tracks. Fixed bool calculations. Exits when bad param. Reshaping.
//
// Revision 1.7  2003/05/14 19:15:36  faivre
// Fancy choices Fortran/C++ V0's and Xi's. Xi rotating and like-sign.
//
// Revision 1.6  2003/05/07 10:51:42  faivre
// Use brand new StHelixModel::setMomentum to solve memory leaks.
//
// Revision 1.4  2003/05/02 21:21:08  lbarnby
// Now identify ITTF tracks by fittingMethod() equal to  kITKalmanFitId
//
// Revision 1.3  2003/04/30 20:38:22  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.2  2003/04/30 19:16:04  faivre
// Fix storage part. ITTF vs TPT Xis.
//
//
