class TIVFile {

private:
  static FILE *File;

public:
   TIVFile(void) {}
   void Open(char *filename) {
      File = fopen(filename, "w");
   }
   void Close(void) {
      if(File) fclose(File);
   }
   void Header(void) {
      fprintf(
         File,
         "#Inventor V2.1 ascii \n\n"
         "DrawStyle {\n"
         "    style FILLED\n"
         "    pointSize 0\n"
         "    lineWidth 0\n"
         "    linePattern 0xffff\n"
         "} \n\n"
         );
   }
};

//_______________________________________________________________________________________________________
class TIVShape : public TIVFile {
private:
   TVolumeView *VolumeView;
   static Int_t Cut;
   TVolume *Volume;
   TShape *Shape;
   Float_t Vi[6][4];
   Float_t V[8][3];
   Float_t N[6][4][3];

public:
   TIVShape(TVolumeView *vv) {
      VolumeView = vv;
      Volume = vv->GetNode();
      Shape = Volume->GetShape();

      Vi[0][0] = 0; Vi[0][1] = 1; Vi[0][2] = 3; Vi[0][3] = 2; 		
      Vi[1][0] = 0; Vi[1][1] = 1; Vi[1][2] = 5; Vi[1][3] = 4; 		
      Vi[2][0] = 2; Vi[2][1] = 3; Vi[2][2] = 7; Vi[2][3] = 6; 		
      Vi[3][0] = 1; Vi[3][1] = 3; Vi[3][2] = 7; Vi[3][3] = 5; 		
      Vi[4][0] = 0; Vi[4][1] = 2; Vi[4][2] = 6; Vi[4][3] = 4; 		
      Vi[5][0] = 4; Vi[5][1] = 5; Vi[5][2] = 7; Vi[5][3] = 6; 		

   }
   //_______________________________________________________________________________________________________
   static void SetCut(Int_t cut) { Cut = cut ? cut : 1; cout << "Cut  " << cut << endl; }
   //_______________________________________________________________________________________________________
   static int GetCut() {return  Cut;}

   void ShapeHeader(void) {
      TVolumePosition *p =  (*GlobalIter)[GlobalIter->GetDepth()];

      Float_t ObjectX = p->GetX();
      Float_t ObjectY = p->GetY();
      Float_t ObjectZ = p->GetZ();
      Double_t* ObjectMatrix = p->GetMatrix()->GetMatrix() ;

      fprintf(
         TIVFile::File,
         "\tMatrixTransform { \n"
         "\t    matrix %f\t%f\t%f\t%f \n" 
         "\t           %f\t%f\t%f\t%f \n"
         "\t           %f\t%f\t%f\t%f \n"
         "\t           %f\t%f\t%f\t%f \n"
         "\t}\n\n",
         ObjectMatrix[0], ObjectMatrix[1], ObjectMatrix[2], 0,
         ObjectMatrix[3], ObjectMatrix[4], ObjectMatrix[5], 0,
         ObjectMatrix[6], ObjectMatrix[7], ObjectMatrix[8], 0,
         ObjectX, ObjectY, ObjectZ, 1
         ); 

      Color_t ColorOfObject = Volume->GetLineColor();
      TColor *colorObject = gROOT->GetColor(ColorOfObject);
      Float_t Objectred, Objectgreen, Objectblue;
      colorObject->GetRGB(Objectred, Objectgreen, Objectblue); 


      fprintf(
         TIVFile::File,
         "\tMaterial {\n"
         "\t    ambientColor %f %f %f \n"
         "\t}\n", 
         Objectred, Objectgreen, Objectblue 
         ); 
   }
   //_______________________________________________________________________________________________________
   void Draw2File(void) {
      fprintf(
         TIVFile::File,
         "\t    DEF TubeUpperSector IndexedFaceSet {\n"
         "\t        vertexProperty VertexProperty { \n"
         "\t            vertex [ \n"
         );
      for(int i = 0; i < 8; i++) fprintf(
         TIVFile::File,
         "\t                %f\t%f\t%f%s\n",
         V[i][0], V[i][1], V[i][2],
         (i < 7 ? "," : "\n\t            ]")

         );
      fprintf(
         TIVFile::File,
         "\t            normal [\n"
         );
      for(int i = 0; i < 6; i++) fprintf(
         TIVFile::File,
         "\t                %f\t%f\t%f, \t%f\t%f\t%f, \t%f\t%f\t%f, \t%f\t%f\t%f%s\n",
         N[i][0][0], N[i][0][1], N[i][0][2], 
         N[i][1][0], N[i][1][1], N[i][1][2], 
         N[i][2][0], N[i][2][1], N[i][2][2], 
         N[i][3][0], N[i][3][1], N[i][3][2],
         (i < 5 ? "," : "\n\t            ]")
         );	
      fprintf(
         TIVFile::File,
         "\t            texCoord [ ]\n"
         "\t            orderedRGBA [ ]\n"
         "\t            normalBinding PER_VERTEX\n"
         "\t        }\n"
         "\t        coordIndex [\n"
         "\t            0, 1, 3, 2, -1, \n"
         "\t            0, 1, 5, 4, -1, \n"
         "\t            2, 3, 7, 6, -1, \n"
         "\t            1, 3, 7, 5, -1, \n"
         "\t            0, 2, 6, 4, -1, \n"
         "\t            4, 5, 7, 6, -1\n"
         "\t        ]\n"	
         "\t    }\n"
         );

   }
   //_______________________________________________________________________________________________________
   void GetNormals(void) {
      for(int i = 0; i < 6; i++) {
         for(int j = 0; j < 4; j++) {
            TMath::Normal2Plane(V[Vi[i][0]], V[Vi[i][1]], V[Vi[i][2]], N[i][j]);
            if(N[i][j][0] == 0 && N[i][j][1] == 0 && N[i][j][2] == 0) 	
               TMath::Normal2Plane(V[Vi[i][1]], V[Vi[i][2]], V[Vi[i][3]], N[i][j]);
            if(i%2) {
               N[i][j][0] = -N[i][j][0];
               N[i][j][1] = -N[i][j][1];
               N[i][j][2] = -N[i][j][2];
            }	 
         }
      }	
   }

   //_______________________________________________________________________________________________________
   virtual void Draw(void) {}	

};

//_______________________________________________________________________________________________________
static Int_t TIVShape::Cut = 1;


//_______________________________________________________________________________________________________
class TIVTube : public TIVShape {
private:
   TTUBE *Tube;
   Float_t Rtable[4];
   Float_t Dz;
   Float_t NDiv;
   Float_t NCDiv;
   Float_t Xa;

public:
   TIVTube(TVolumeView* vv) : TIVShape(vv) {
      Tube = (TTUBE *)TIVShape::Shape;
   }     
   void Prep(void) {
      Rtable[0] = Rtable[2] = Tube->GetRmin(); 
      Rtable[1] = Rtable[3] = Tube->GetRmax(); 
      Dz = Tube->GetDz();
      NDiv = Tube->GetNumberOfDivisions();		
      NCDiv = NDiv / TIVShape::Cut;	
      Xa = TMath::Pi() * 2 / NDiv; 		
   }
   void GetVertexes() {
      for(int i = 0; i < 4; i++) {
         TIVShape::V[i][0] = Rtable[i];
         TIVShape::V[i][1] = 0;
         TIVShape::V[i][2] = (i < 2 ? -Dz : Dz);

         TIVShape::V[i+4][0] = Rtable[i] * TMath::Cos(Xa);
         TIVShape::V[i+4][1] = Rtable[i] * TMath::Sin(Xa);
         TIVShape::V[i+4][2] = TIVShape::V[i][2];
      }

   }
   void NormalCorrection(void) {
      for(int i = 0; i < 2; i++) {
         TIVShape::N[3][i][0] = 1;
         TIVShape::N[3][i][1] = 0;
         TIVShape::N[3][i][2] = 0;

         TIVShape::N[4][i][0] = -1;
         TIVShape::N[4][i][1] = 0;
         TIVShape::N[4][i][2] = 0;
      }
      for(int i = 2; i < 4; i++) {
         TIVShape::N[3][i][0] = TMath::Cos(Xa);
         TIVShape::N[3][i][1] = TMath::Sin(Xa);
         TIVShape::N[3][i][2] = 0;

         TIVShape::N[4][i][0] = -TMath::Cos(Xa);
         TIVShape::N[4][i][1] = -TMath::Sin(Xa);
         TIVShape::N[4][i][2] = 0;
      }

   }
   void Draw(void) {
      Prep();
      ShapeHeader();
      GetVertexes();
      GetNormals();	
      NormalCorrection();	

      fprintf(TIVFile::File, "\tSeparator {\n");

      Draw2File();

      for(int i = 1; i < NCDiv; i ++) fprintf(
         TIVFile::File,
         "\t    Rotation { rotation 0 0 1 %f }\n"
         "\t    Separator { USE TubeUpperSector }\n",
         Xa
         );
      fprintf(TIVFile::File, "\t}\n");

   }   	
};

class TIVTubs : public TIVTube {
private:
   TTUBS *Tubs;
   Float_t Phi1, Phi2;

public:
   TIVTubs(TVolumeView* vv) : TIVTube(vv) {
      Tubs = (TTUBS *)TIVShape::Shape;	
   }     
   void Prep(void) {
      TIVTube::Prep();	
      Phi1 = Tubs->GetPhi1() * TMath::Pi() / 180;
      Phi2 = Tubs->GetPhi2() * TMath::Pi() / 180;

      TIVTube::Xa = (Phi2 - Phi1) / TIVTube::NDiv; 
   }
   void Draw(void) {
      Prep();
      ShapeHeader();
      GetVertexes();
      GetNormals();	
      NormalCorrection();	

      fprintf(TIVFile::File, "\tRotation { rotation 0 0 1 %f }\n", Phi1);
      fprintf(TIVFile::File, "\tSeparator {\n");

      Draw2File();

      for(int i = 1; i < TIVTube::NCDiv; i ++) fprintf(
         TIVFile::File,
         "\t    Rotation { rotation 0 0 1 %f }\n"
         "\t    Separator { USE TubeUpperSector }\n",
         TIVTube::Xa
         );
      fprintf(TIVFile::File, "\t}\n");

   }   	
};

class TIVCtub : public TIVTubs {
private:
   TCTUB *Ctub;

public:
   TIVCtub(TVolumeView *vv) : TIVTubs(vv) {
      Ctub = (TCTUB *)TIVShape::Shape;
   }		

   void GetVertexes(Int_t sn) {
      Float_t buf[1000];

      Ctub->SetPoints(buf);
      Int_t ndiv = Ctub->GetNumberOfDivisions() + 1;

      for(int j = 0; j < 4; j++)
         for(int i = 0; i < 3; i++) {
            TIVShape::V[0 + j * 2][i] = buf[3 * j * (ndiv) + i + sn * 3];
            TIVShape::V[1 + j * 2][i] = buf[3 * j * (ndiv) + i + 3 + sn * 3];
         }

   }
   void GetNormals(Int_t sn) {
      TIVShape::GetNormals();

      Float_t normal[3];
      Float_t a = TIVTube::Xa * sn;
      Float_t a1 = TIVTube::Xa * (sn + 1);

      TIVShape::N[2][0][0] = TMath::Cos(a);
      TIVShape::N[2][0][1] = TMath::Sin(a);
      TIVShape::N[2][0][2] = 0;

      TIVShape::N[2][1][0] = TMath::Cos(a1);
      TIVShape::N[2][1][1] = TMath::Sin(a1);
      TIVShape::N[2][1][2] = 0;

      TIVShape::N[2][2][0] = TMath::Cos(a1);
      TIVShape::N[2][2][1] = TMath::Sin(a1);
      TIVShape::N[2][2][2] = 0;

      TIVShape::N[2][3][0] = TMath::Cos(a);
      TIVShape::N[2][3][1] = TMath::Sin(a);
      TIVShape::N[2][3][2] = 0;


      for(int i = 0; i < 4; i++) for(int j = 0; j < 3; j++)
         TIVShape::N[1][i][j] = -TIVShape::N[2][i][j];

   }
   void Draw(void) {
      TIVTubs::Prep();


      for(int i = 0; i < TIVTube::NDiv; i++) {
         fprintf(TIVFile::File, "\tSeparator {\n");

         ShapeHeader();
         GetVertexes(i);
         GetNormals(i);
         //	    NormalCorrection();


         fprintf(TIVFile::File, "\tSeparator {\n");

         Draw2File();				

         fprintf(TIVFile::File, "\t}\n");
         fprintf(TIVFile::File, "\t}\n");
      } 
   }

};


class TIVCone : public TIVTube {
private:
   TCONE *Cone;
   Float_t Za1, Za2;
public:
   TIVCone(TVolumeView* vv) : TIVTube(vv) {
      Cone = (TCONE *)TIVShape::Shape;
   }    


   void Prep() {
      TIVTube::Prep();	
      TIVTube::Rtable[2] = Cone->GetRmin2(); 
      TIVTube::Rtable[3] = Cone->GetRmax2();

      Za1 = TMath::ATan((TIVTube::Rtable[2] - TIVTube::Rtable[0]) / (2 * TIVTube::Dz));
      Za2 = TMath::ATan((TIVTube::Rtable[3] - TIVTube::Rtable[1]) / (2 * TIVTube::Dz));
   }

   void NormalCorrection(void) {
      for(int i = 0; i < 2; i++) {
         TIVShape::N[3][i][0] = TMath::Cos(Za2);
         TIVShape::N[3][i][1] = 0;
         TIVShape::N[3][i][2] = -TMath::Sin(Za2);

         TIVShape::N[4][i][0] = -TMath::Cos(Za1);
         TIVShape::N[4][i][1] = 0;
         TIVShape::N[4][i][2] = TMath::Sin(Za1);
      }
      for(int i = 2; i < 4; i++) {
         TIVShape::N[3][i][0] = TMath::Cos(Za2) * TMath::Cos(TIVTube::Xa);
         TIVShape::N[3][i][1] = TMath::Cos(Za2) * TMath::Sin(TIVTube::Xa);
         TIVShape::N[3][i][2] = -TMath::Sin(Za2);

         TIVShape::N[4][i][0] = -TMath::Cos(Za1) * TMath::Cos(TIVTube::Xa);
         TIVShape::N[4][i][1] = -TMath::Cos(Za1) * TMath::Sin(TIVTube::Xa);
         TIVShape::N[4][i][2] = TMath::Sin(Za1);
      }
   }
   void Draw(void) {
      Prep();	
      ShapeHeader();
      GetVertexes();
      GetNormals();	
      NormalCorrection();	

      fprintf(TIVFile::File, "\tSeparator {\n");

      Draw2File();

      for(int i = 1; i < TIVTube::NCDiv; i ++) fprintf(
         TIVFile::File,
         "\t    Rotation { rotation 0 0 1 %f }\n"
         "\t    Separator { USE TubeUpperSector }\n",
         TIVTube::Xa
         );
      fprintf(TIVFile::File, "\t}\n");

   }   	

};

class TIVPcon : public TIVCone {
private:
   TPCON *Pcon;

   Int_t Nz;
   Float_t *vDz;
   Float_t *vRmin;
   Float_t *vRmax;

public:	
   TIVPcon(TVolumeView* vv) : TIVCone(vv) {
      Pcon = (TPCON *)TIVShape::Shape;
      Nz = Pcon->GetNz();
      vDz = Pcon->GetDz();
      vRmin = Pcon->GetRmin();
      vRmax = Pcon->GetRmax();
   }

   void GetVertexes(Int_t sn) {
      for(int i = 0; i < 4; i++) {
         TIVShape::V[i][0] = TIVTube::Rtable[i];
         TIVShape::V[i][1] = 0;

         TIVShape::V[i+4][0] = TIVTube::Rtable[i] * TMath::Cos(TIVTube::Xa);
         TIVShape::V[i+4][1] = TIVTube::Rtable[i] * TMath::Sin(TIVTube::Xa);

         TIVShape::V[i][2] = (i < 2 ? vDz[sn] : vDz[sn+1]);

         TIVShape::V[i+4][2] = TIVShape::V[i][2];

      }

   }
   void Prep(Int_t sn) {
      if(sn == 0)	{
         TIVTube::Rtable[0] = vRmin[sn];
         TIVTube::Rtable[1] = vRmax[sn]; 	 
         TIVTube::Rtable[2] = vRmin[sn + 1];
         TIVTube::Rtable[3] = vRmax[sn + 1];

         TIVCone::Za1 = TMath::ATan((TIVTube::Rtable[2] - TIVTube::Rtable[0]) / (vDz[sn] - vDz[sn + 1]));
         TIVCone::Za2 = TMath::ATan((TIVTube::Rtable[3] - TIVTube::Rtable[1]) / (vDz[sn] - vDz[sn + 1]));
      } 	 

      TIVTube::NDiv = Pcon->GetNumberOfDivisions();
      TIVTube::Xa = TMath::Pi() * 2 / TIVTube::NDiv; 		
      TIVTube::NCDiv = TIVTube::NDiv / TIVShape::Cut;	

   }
   void Draw(void) {
      for(int i = 0; i < Nz - 1; i++) {
         if(vDz[i] == vDz[i + 1]) continue;  
         Prep(i);
         GetVertexes(i);
         GetNormals();	
         TIVCone::NormalCorrection();	

         fprintf(TIVFile::File, "\tSeparator {\n");

         Draw2File();

         for(int j = 1; j < TIVTube::NCDiv; j++) fprintf(
            TIVFile::File,
            "\t    Rotation { rotation 0 0 1 %f }\n"
            "\t    Separator { USE TubeUpperSector }\n",
            TIVTube::Xa
            );
         fprintf(TIVFile::File, "\t}\n");

      }

   }
};

class TIVBrik:public TIVShape {
private:
   TBRIK *Brik;
public:
   TIVBrik(TVolumeView* vv) : TIVShape(vv) {
      Brik = (TBRIK *)TIVShape::Shape;
   }     
   void GetVertexes() {

      TIVShape::V[0][0] = -Brik->GetDx();	
      TIVShape::V[0][1] = -Brik->GetDy();	
      TIVShape::V[0][2] = -Brik->GetDz();	

      TIVShape::V[1][0] = Brik->GetDx();	
      TIVShape::V[1][1] = -Brik->GetDy();	
      TIVShape::V[1][2] = -Brik->GetDz();	

      TIVShape::V[2][0] = -Brik->GetDx();	
      TIVShape::V[2][1] = -Brik->GetDy();	
      TIVShape::V[2][2] = Brik->GetDz();	

      TIVShape::V[3][0] = Brik->GetDx();	
      TIVShape::V[3][1] = -Brik->GetDy();	
      TIVShape::V[3][2] = Brik->GetDz();	


      TIVShape::V[4][0] = -Brik->GetDx();	
      TIVShape::V[4][1] = Brik->GetDy();	
      TIVShape::V[4][2] = -Brik->GetDz();	

      TIVShape::V[5][0] = Brik->GetDx();	
      TIVShape::V[5][1] = Brik->GetDy();	
      TIVShape::V[5][2] = -Brik->GetDz();	

      TIVShape::V[6][0] = -Brik->GetDx();	
      TIVShape::V[6][1] = Brik->GetDy();	
      TIVShape::V[6][2] = Brik->GetDz();	

      TIVShape::V[7][0] = Brik->GetDx();	
      TIVShape::V[7][1] = Brik->GetDy();	
      TIVShape::V[7][2] = Brik->GetDz();	


   }
   void Draw(void) {

      ShapeHeader();
      GetVertexes();

      GetNormals();	

      fprintf(TIVFile::File, "\tSeparator {\n");

      Draw2File();

      fprintf(TIVFile::File, "\t}\n");

   }   	
};

//_______________________________________________________________________________________________________
class TIVTrd1:public TIVShape {
private:
   TTRD1 *Trd1;
public:
   TIVTrd1(TVolumeView* vv) : TIVShape(vv) {
      Trd1 = (TTRD1 *)TIVShape::Shape;
   }     
   void GetVertexes() {

      TIVShape::V[0][0] = -Trd1->GetDx();	
      TIVShape::V[0][1] = -Trd1->GetDy();	
      TIVShape::V[0][2] = -Trd1->GetDz();	

      TIVShape::V[1][0] = Trd1->GetDx();	
      TIVShape::V[1][1] = -Trd1->GetDy();	
      TIVShape::V[1][2] = -Trd1->GetDz();	

      TIVShape::V[2][0] = -Trd1->GetDx2();	
      TIVShape::V[2][1] = -Trd1->GetDy();	
      TIVShape::V[2][2] = Trd1->GetDz();	

      TIVShape::V[3][0] = Trd1->GetDx2();	
      TIVShape::V[3][1] = -Trd1->GetDy();	
      TIVShape::V[3][2] = Trd1->GetDz();	


      TIVShape::V[4][0] = -Trd1->GetDx();	
      TIVShape::V[4][1] = Trd1->GetDy();	
      TIVShape::V[4][2] = -Trd1->GetDz();	

      TIVShape::V[5][0] = Trd1->GetDx();	
      TIVShape::V[5][1] = Trd1->GetDy();	
      TIVShape::V[5][2] = -Trd1->GetDz();	

      TIVShape::V[6][0] = -Trd1->GetDx2();	
      TIVShape::V[6][1] = Trd1->GetDy();	
      TIVShape::V[6][2] = Trd1->GetDz();	

      TIVShape::V[7][0] = Trd1->GetDx2();	
      TIVShape::V[7][1] = Trd1->GetDy();	
      TIVShape::V[7][2] = Trd1->GetDz();	


   }
//_______________________________________________________________________________________________________
   void Draw(void) {

      ShapeHeader();
      GetVertexes();

      GetNormals();	

      fprintf(TIVFile::File, "\tSeparator {\n");

      Draw2File();

      fprintf(TIVFile::File, "\t}\n");

   }   	
};

//_______________________________________________________________________________________________________
class TR2iv : public TIVFile {
private:
   FILE *File = 0;
   TVolumeView *Geom;	
   TVolumeViewIter *GeomIter = 0;
   Int_t Sdepth;	

public:  
   TR2iv(TVolumeView *geom, Int_t sdepth, Int_t edepth) {
      Geom = geom;
      GeomIter = new TVolumeViewIter(geom, edepth);
      GlobalIter = GeomIter;
      Sdepth = sdepth;
   }
   void Draw(void);

   ~TR2iv() { 
      if(GeomIter) delete GeomIter; 
   }	    

};


//_______________________________________________________________________________________________________
TVolumeViewIter *GlobalIter = 0;

//_______________________________________________________________________________________________________
void TR2iv::Draw(char *filename) {

   Open(filename);	
   Header();
   fprintf(TIVFile::File, "Separator {\n"); 	

   TVolumeView *vv = Geom;


   for(int i = 0; i < 10000; i++) {

      TIVShape *ivs = 0;
      TVolume *v = vv->GetNode();
      TShape  *s = v->GetShape();

//      if(v->IsMarked() && !v->GetVisibility()) {
      if(!v->GetVisibility()) {

         // cout << "sdepth=" << Sdepth << " : " <<  GeomIter->GetDepth() << "   " <<  i << "  " << s->ClassName() << "   " << s->GetName() << endl;
         if(GeomIter->GetDepth() >= Sdepth)
            if( 
               !strcmp(s->IsA()->GetName(), "TTUBE") ||
               //		    !strcmp(s->IsA()->GetName(), "TTUBS") || 
               !strcmp(s->IsA()->GetName(), "TCONE") ||
               !strcmp(s->IsA()->GetName(), "TPCON") ||
               !strcmp(s->IsA()->GetName(), "TBRIK") ||
               !strcmp(s->IsA()->GetName(), "TTRD1") ||
               !strcmp(s->IsA()->GetName(), "TCTUB")
               ) { 

                  // cout << "\t\t" << GeomIter->GetDepth() << "   " <<  i << "  " << s->IsA()->GetName() << "   " << s->GetName() << endl;
                  if(!strcmp(s->IsA()->GetName(), "TCONE")) ivs = new TIVCone(vv);
                  if(!strcmp(s->IsA()->GetName(), "TTUBE")) ivs = new TIVTube(vv);
                  if(!strcmp(s->IsA()->GetName(), "TTUBS")) ivs = new TIVTubs(vv);
                  if(!strcmp(s->IsA()->GetName(), "TPCON")) ivs = new TIVPcon(vv);
                  if(!strcmp(s->IsA()->GetName(), "TBRIK")) ivs = new TIVBrik(vv);
                  if(!strcmp(s->IsA()->GetName(), "TTRD1")) ivs = new TIVTrd1(vv);
                  if(!strcmp(s->IsA()->GetName(), "TCTUB")) ivs = new TIVCtub(vv);
                  fprintf(TIVFile::File, "\tSeparator {\n");

                  ivs->Draw();

                  fprintf(TIVFile::File, "\t}\n");
               }

      }

      vv = (TVolumeView *)GeomIter->Next();
      if(!vv) break;	
      if(ivs) delete ivs;		    
   }
   fprintf(TIVFile::File, "}\n");	
   Close();

}
//_______________________________________________________________________________________________________
// TVolumeView *Oute = ATLS->FindByName("OUTE");

//_______________________________________________________________________________________________________
int r2iv_1 (void) {

   //    TVolumeView *view = ATLS->FindByName("OUTE");
   TR2iv riv(Oute, 1, 2);
   TIVShape::SetCut(3);

   riv.Draw("qqq.iv");	


   return 0;	
}
