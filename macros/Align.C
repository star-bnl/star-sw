#if !defined(__CINT__)
#include "Riostream.h"
#include "TMath.h"
#include "TGeoManager.h"
#include "TGeoMatrix.h"
#include "TGeoPgon.h"
#include "TGeoPhysicalNode.h"
#endif
void Align() {
  struct DetAlign_t {
    Char_t *Name; 
    Char_t *Fmt;
    Double_t dx, dy, dz;
    Int_t  N;
    Int_t  n1, n2, n3, n4;
  };
  const Int_t Ndet = 4;
  DetAlign_t DetAlign[4] = {//                        dx dy dz   N n1  n2  n3 n4
    {"Main-%d", "/TOP_1/MAIN_%d",                     1, 0, 0,   1, 1,  0,  0, 0},
    {"SX-%d",   "/TOP_1/MAIN_%d/SX_%d",               0, 2, 0,   2, 1,  2,  0, 0},
    {"SY-%d",   "/TOP_1/MAIN_%d/SX_%d/SY_%d",         0, 0, 3,   3, 1,  2,  2, 0},
    {"CELL-%d", "/TOP_1/MAIN_%d/SX_%d/SY_%d/CELL_%d",.1,.1,.1,   4, 1,  2,  2, 1},
  };
  Int_t DX = 100, DY = 100, DZ = 10;
  if (gGeoManager) delete gGeoManager;
  new TGeoManager("alignment", "Ideal geometry");
  TGeoMaterial *mat = new TGeoMaterial("Al", 26.98,13,2.7);
  TGeoMedium   *med = new TGeoMedium("MED",1,mat);
  TGeoVolume    *top = gGeoManager->MakeBox("TOP",med,DX,DY,DZ);
  gGeoManager->SetTopVolume(top);
  TGeoVolume *main = gGeoManager->MakeBox("MAIN",med,DX,DY,DZ);
  top->AddNode(main,1);
  //  TGeoVolume *slicex =   main->Divide("SX",1,DetAlign[1].n2,0,0);
  TGeoVolume *slicex = gGeoManager->MakeBox("SX",med,DX/DetAlign[2].n2,DY,DZ);
  for (Int_t i = 0; i < DetAlign[2].n2; i++) {
    main->AddNode(slicex,i+1,new TGeoTranslation(-DX + DX/DetAlign[2].n2*(2*i+1), 0, 0));
  }
  //  TGeoVolume *slicey = slicex->Divide("SY",2,DetAlign[2].n3,0,0);
  TGeoVolume *slicey = gGeoManager->MakeBox("SY",med,DX/DetAlign[3].n2,DY/DetAlign[3].n3,DZ);
  for (Int_t i = 0; i <  DetAlign[3].n3; i++) {
    slicex->AddNode(slicey,i+1,new TGeoTranslation(0, -DY + DY/DetAlign[3].n3*(2*i+1), 0));
  }
  TGeoVolume *vol = gGeoManager->MakePgon("CELL",med,0.,360.,6,2);
  TGeoPgon *pgon = (TGeoPgon*)(vol->GetShape());
  pgon->DefineSection(0,-5,0.,2.);
  pgon->DefineSection(1, 5,0.,2.);
  vol->SetLineColor(2);
  slicey->AddNode(vol,1);
  gGeoManager->CloseGeometry();
#if 1
  Int_t indx[4];
  TString path("");
  TGeoPhysicalNode *nodeP;
  TGeoTranslation *tr = 0;
  for (Int_t idet = 0; idet < Ndet; idet++) {
  //  for (Int_t idet = Ndet-1; idet >= 0; idet--) {
    Int_t Ntot = 1;
    Int_t *ns = &DetAlign[idet].n1;
    for (Int_t i = 0; i < DetAlign[idet].N; i++) {
      Ntot *= ns[i];
    }
    memset(indx, 0, 4*sizeof(Int_t));
    for (Int_t j = 0; j < Ntot; j++) {
      Int_t ind = j;
      Int_t N = DetAlign[idet].N;
      for (Int_t k =  N - 1; k >= 0; k--) {
	ns = &DetAlign[idet].n1;
	indx[k] = ind%ns[k]+1; ind /= ns[k];
      }
      Int_t id = 0;
      switch (N) {
      case 1: path = Form(DetAlign[idet].Fmt,indx[0]); id = indx[0]; break;
      case 2: path = Form(DetAlign[idet].Fmt,indx[0],indx[1]); id = 10*indx[0] + indx[1]; break;
      case 3: path = Form(DetAlign[idet].Fmt,indx[0],indx[1],indx[2]); id = 10*(10*indx[0] + indx[1]) + indx[2]; break;
      case 4: path = Form(DetAlign[idet].Fmt,indx[0],indx[1],indx[2],indx[3]); id = 10*(10*(10*indx[0] + indx[1]) + indx[2]) + indx[3]; break;
      default: id = -1; break;
      };
      if (id < 0) continue;
      //      if (! gGeoManager->CheckPath(path)) continue;
      if (! gGeoManager->cd(path)) continue;
      TObjArray *objs = gGeoManager->GetListOfPhysicalNodes();
      if (objs) nodeP = (TGeoPhysicalNode *) objs->FindObject(path);
      if (nodeP) {
	if (nodeP->IsAligned()) {
	  cout << nodeP->GetName() << " has been aligned (?)" << endl;
	  continue;
	}
      } else {
	nodeP = gGeoManager->MakePhysicalNode(path);
      }
      cout << "before: " << nodeP->GetName() << "\t"; nodeP->GetMatrix()->Print();
      //      nodeP->Print();
      tr = new TGeoTranslation();
      tr->SetTranslation(DetAlign[idet].dx,DetAlign[idet].dy,DetAlign[idet].dz);
      *tr = (*(nodeP->GetNode()->GetMatrix())) * (*tr);
      tr->SetName(Form(DetAlign[idet].Name,id));
      tr->Print();
      nodeP->Align(tr);
      cout << "after : " << nodeP->GetName() << "\t"; nodeP->GetMatrix()->Print();
      //      nodeP->Print();
      cout << "==================================================" << endl;
    }
  }
#endif
#if 1
  TObjArray *objs = gGeoManager->GetListOfPhysicalNodes();
  TIter      next(objs);
  while ((nodeP = (TGeoPhysicalNode *) next())) {
    cout << "===========================" << endl;
    nodeP->Print();
     cout << nodeP->GetName() << "\t"; nodeP->GetMatrix()->Print();
     cout << "==================================================" << endl;
  }
#endif
}
