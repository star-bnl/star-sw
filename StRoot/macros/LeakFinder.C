enum {kCtr      =1,kClear   = 2,kMake   = 3,
      startClear=2,contClear=12,endClear=22,
      startMake =3,contMake =13,endMake =23,
      kMaxMake=100,kMaxMakeSQ = kMaxMake*kMaxMake,
      kMaxData =kMaxMake+3
     };

class ReadLog {
public:
int fNMakers;		  
FILE *inp;		 
THashList hash;
TObjArray mArray;
char line[500];
// methods
ReadLog(const char *file) {inp = fopen(file,"r");fNMakers=0;}	  
const char *Name(int idx) {
  if (!idx) return "__ALL__";
  TObject *tn = mArray.At(idx); return (tn)? tn->GetName():0;}		  

const char *Next(int &kind,int &idx,double &exe,double &heap,double &free,double &inc) {		  
  if(!inp) return 0;
  kind=0;idx=0;exe=0;heap=0;free=0,inc=0;
  TString ts;
  char *endMaker,*clear,*ctr,*doPs,*fr,*to,*and;
  while(fgets(line,500,inp)) {
    doPs = strstr(line,"doPs for");
    if (!doPs) 				continue;
    endMaker = strstr(line,"EndMaker");
    clear    = strstr(line,"Clear");
    ctr      = strstr(line,"constructor");
    kind = 0;
    if (ctr     ) kind = 1;
    if (clear   ) kind = 2;
    if (endMaker) kind = 3;
    if (!kind)				continue;
    fr = doPs+8 +strspn(doPs+8," \t");
    to = strstr(fr,":");
    ts = ""; ts.Append(fr,to-fr);
    if (!ts.Length()) 			continue;
    fr = strstr(fr,"total");  if (!fr) 	continue;
    fr = strstr(fr,"="    );  if (!fr) 	continue;
    exe = atof(fr+1);
    fr  = strstr(fr,"heap" ); if (!fr) 	continue;
    fr  = strstr(fr,"="    ); if (!fr) 	continue;
    heap = atof(fr+1);
    and  = strstr(fr,"and");
    if (and) free = atof(and+3);
    fr  = strstr(fr,"("    ); if (!fr) 	continue;
    inc = atof(fr+1);
    if (kind==1) 			continue;
    TNamed *tn = (TNamed*)hash.FindObject(ts.Data());
    if (!tn) {
      tn = new TNamed(ts.Data(),"");
      hash.Add(tn);
      fNMakers++;
      tn->SetUniqueID((UInt_t)fNMakers);
      mArray.AddAtAndExpand(tn,fNMakers);
    }
    idx = tn->GetUniqueID();
    return tn->GetName();
  }
  fclose(inp); inp = 0;
  return 0;  
}};
//______________________________________________________________________________
class  Data4Fit  
{
public:

Data4Fit(ReadLog &rr);
int fNEvts;
int fNMk;
TArrayD fDat;

double *GetDat(int evt)	        {return fDat.GetArray()+evt*(kMaxData);}	
double *GetEvt(int evt)	        {return GetDat(evt)+2;}	
double &GetEvtRes(int evt)	{return GetEvt(evt)[-1];}	
double &GetEvtSiz(int evt)	{return GetEvt(evt)[-2];}	
};	
//______________________________________________________________________________
Data4Fit::Data4Fit(ReadLog &rr)
{	
  double QQ[kMaxData];
  memset(QQ,0,sizeof(double)*kMaxData);
  double *Q = QQ+2;
  const char *name;
  int kind,idx; double exe,heap,free,inc;
  fNEvts=0,fNMk=0;
  int nkind[4] = {0,0,0,0};  
  int kase = 0,oldKind=0,num = 0,nmk=0,narr;

  while ((name = rr.Next(kind,idx,exe,heap,free,inc))) { 
    nkind[kind]++;
    if (kind<kClear) continue;
    num++;
//    printf("%4d - %20s \t%d %d \t %g %g %+g\n",num,name,kind,idx,exe,heap,inc);

         if (!oldKind)      { kase = kind;      }
    else if (kind==oldKind) { kase = kind   +10;}
    else                    { kase = oldKind+20;}
    fNMk = rr.fNMakers;
SWIT: switch (kase) {
    
    case startClear: 
      kase = contClear;

    case contClear: 
      if (!nmk) break;
      Q[-1] +=inc;
      break;
      
    case endClear: 
      kase = startMake;
      if(!nmk) goto SWIT;
      narr = fDat.GetSize();
      if (narr< (fNEvts+1)*kMaxData) fDat.Set((fNEvts+10)*kMaxData*2);
//  printf("Q="); for (int j=0;j<=fNMk;j++) {printf("%12.6g ",Q[j]);}; printf("\n");

      memcpy(GetDat(fNEvts),QQ,kMaxData*sizeof(double));
//    printf("Q="); for (int j=0;j<=fNMk;j++) {printf("%12.6g ",GetEvt(fNEvts)[j]);}; printf("\n");
      fNEvts++;

    case startMake: 
      memset(QQ,0,sizeof(double)*kMaxData);
      Q[0]=1.;
      kase = contMake;

    case contMake: 
      nmk++;
      Q[idx]  = inc;
      Q[-1 ] += inc;
      Q[-2 ] += inc;
      break;

    case endMake: 
      kase = startClear;
      goto SWIT;

    }//EndSwitch
    oldKind = kind;
  }//end LOOP
  fNMk = rr.fNMakers;
}
//______________________________________________________________________________
void LeakGraph(char *file); 
void LeakFit  (char *file); 
//______________________________________________________________________________
void LeakFinder(char *file,int kase=1,const char *par1=0) 
{
   switch (kase) {
   case 0: LeakGraph(file); break;
   case 1: if (!TClassTable::GetDict("TCL")) {gSystem->Load("libTable");}
           LeakFit  (file); break;
   case 2: LeakCount(file,par1); break;
   }
}

//______________________________________________________________________________
void LeakGraph(char *file) 
{
  TArrayF X[4],XX[2];

  ReadLog rr(file);
  const char *name;
  int kind,idx; double exe,heap,free,inc;
  int num = 0,nXX=0;
  float yMax =0, yMin = 1000000.,xMax;
  while ((name = rr.Next(kind,idx,exe,heap,free,inc))) {
//    if (kind!=3) continue;
    if (kind==kCtr) continue;

    int n = X[0].GetSize();
    if (num>=n) { for (int i=0;i<4;i++){X[i].Set(n*2+10);}}
    X[0][num] = num;
    if (yMax < exe ) yMax = exe;
    if (yMax < heap) yMax = heap;
    if (yMax < free) yMax = free;
    if (yMin > exe ) yMin = exe;
    if (yMin > heap) yMin = heap;
    if (yMin > free) yMin = free;
    X[1][num] = exe;
    X[2][num] = heap;
    X[3][num] = free;
//    printf("YYYYYYYYYYYYYY %g %g %g\n",X[num],Y[0][num],Y[1][num]);
    num++;
//    printf("%4d - %20s \t%d %d \t %g %g %g %+g\n",num,name,kind,idx,exe,heap,free,inc);
    if (kind!=kClear) 		continue;
    if (strcmp("bfc",name)) 	continue;
    n = XX[0].GetSize();
    if (nXX>=n) { for (int i=0;i<2;i++){XX[i].Set(n*2+10);}}
    XX[0][nXX]=num;
    XX[1][nXX]=heap;
    nXX++;
  }
  int nPoints = num;
  yMax *=1.1;
  yMin -=0.1*yMax;
  xMax = nPoints;
  float fact = float(nXX)/nPoints;
  xMax = nPoints*fact;
  for (int i=0;i<num;i++) {X [0][i] = X [0][i]*fact;}
  for (int i=0;i<nXX;i++) {XX[0][i] = XX[0][i]*fact;}

  TCanvas *C1 = new TCanvas("LEAK","Memory grow",1);
  TGraph *pl[4];
  for (int i=0;i<3;i++){pl[i] = new TGraph(nPoints, X[0].GetArray(), X[i+1].GetArray());}
                        pl[3] = new TGraph(nXX    ,XX[0].GetArray(),XX[1  ].GetArray());
  pl[0]->SetLineColor(kBlack);
  pl[1]->SetLineColor(kBlue );
  pl[2]->SetLineColor(kGreen);
  pl[3]->SetLineColor(kRed  );

  pl[0]->SetName("Exe");
  pl[1]->SetName("Heap");
  pl[2]->SetName("Free");
  pl[3]->SetName("Clear");

  C1->DrawFrame(0.,yMin,xMax,yMax);
  pl[3]->Draw("LP");
  //pl[1]=0;
  for (int i=0;i<3;i++) {if (pl[i]) pl[i]->Draw("LP");}
  C1->Modified();
  C1->Update();
 
}

//______________________________________________________________________________
void LeakFit(char *file) 
{

  int ignore[kMaxMake];
  memset(ignore,0,kMaxMake*sizeof(int));
  double A[kMaxMake][kMaxMake],B[kMaxMake],Q[kMaxMake],V[kMaxMake];
  TCL::vzero(A[0],(kMaxMake)*(kMaxMake));
  TCL::vzero(B   ,kMaxMake);
  TCL::vzero(V   ,kMaxMake);

  double Corr[5][kMaxMake];
  TCL::vzero(Corr[0],5*(kMaxMake));
  int nCorr=0;

  TCanvas *C1 = new TCanvas("LeakPlot","Leak vs Size",1);
  C1->Divide(1,2);
  
  TH2F    *H1  = new TH2F("LeakPlot", "Leak vs Size", 100, 0, 50., 100, -0.1, 3.);
  TH2F    *H2  = new TH2F("LeagPlot", "Leag vs Size", 100, 0, 50., 100, -0.1, 3.);
//  H->SetBit(TH1::kCanRebin);  
  C1->cd(1); H1->Draw();
  C1->cd(2); H2->Draw();

  ReadLog RR(file);
  Data4Fit FF(RR);
  const char *name;
  int num = 0,nEvents=0,nMk=0,nFill=0,iWeight=0;

  double emptySize = 10;
  int skipEvents   = 10;
  iWeight=0;
  nMk = FF.fNMk;
  nEvents = FF.fNEvts;
  for (int ievt=0;ievt<nEvents;ievt++) {
     double diff = FF.GetEvtRes(ievt);
     double eSize = FF.GetEvtSiz(ievt);
     if (ievt<=skipEvents) 	continue;
     H1->Fill(eSize,diff);
     if (eSize < emptySize+0.1)	continue;
     nFill++;
     TCL::ucopy(FF.GetEvt(ievt),Q,nMk+1);
//printf("Q="); for (int j=0;j<=nMk;j++) {printf("%d=%12.6g ",j,Q[j]);}; printf("\n");
     TCL::vadd(V,Q,V,nMk+1);
     double Qmax = 0;
     for (int i=1;i<=nMk;i++){
       if (fabs(Q[i])>Qmax) Qmax=fabs(Q[i]);
       if (Q[i]< 0.1*diff) Q[i]=0.;
     };
     
     double wt = 1;
     if (iWeight==1) wt =1./pow(diff,2);
     if (iWeight==2) wt =1./pow(eSize-emptySize,2);
     if (iWeight==3) wt =1./pow(Qmax,2);

     for (int i1=0;i1<=nMk;i1++){
	B[i1]+= diff*Q[i1]*wt;
//      printf("B[%d] = %12.6g*%12.6g*%12.6g =%12.6g \n",i1,diff,Q[i1],wt,B[i1]);
	for (int i2=0;i2<=nMk;i2++){ A[i1][i2] += Q[i1]*Q[i2]*wt;}
     }  
//     Second Method
     nCorr++;
     for (int i=1;i<=nMk;i++) {
       Corr[0][i] += Q[i];
       Corr[1][i] += diff;
       Corr[2][i] += Q[i]*Q[i];
       Corr[3][i] += diff*diff;
       Corr[4][i] += diff*Q[i];
     }
#

  }//end LOOP
  int nX = nMk+1;
  double scale = 1./nFill;
  TCL::vscale(B,scale,B,nX);
  TCL::vscale(A[0],scale,A[0],(kMaxMake)*(kMaxMake));

// Regularisation
  int nneg=1;  

  while (nneg) {
    printf("%d makers %d events filled %d times\n\n",nMk,nEvents,nFill);

    for (int ign=0;ign<nX;ign++) {
      if(!ignore[ign])  	continue;
      if(ignore[ign]>0)  {//Too big
	for (int i=0;i<nX;i++) B[i]-=A[i][ign];
	for (int i=0;i<nX;i++) {A[i][ign]=0;A[ign][i]=0;}
	A[ign][ign]=1; B[ign]=1;
      } else  {
	for (int i=0;i<nX;i++) {A[i][ign]=0;A[ign][i]=0;}
	A[ign][ign]=1; B[ign]=0;
      }
    }
    double reg = 0;
    for (int i=0;i<nX;i++) {
      if (ignore[i]) 	continue;
      if (A[i][i]>reg) reg = A[i][i];
    }
    reg /=10000.;

    TArrayD AA(nX*nX),BB(nX);
    for (int i1=0;i1<nX;i1++) {
  //    printf("B[%d]== %g\n",i1,B[i1]);
      BB[i1] = B[i1]; 
      for (int i2=0;i2<nX;i2++) {
  //      printf("A[%d][%d]== %g\n",i1,i2,A[i1][i2]);
	AA[i1+nX*i2] = A[i1][i2];
	if(i1==i2)  AA[i1+nX*i2]+=reg;
  //      printf("AA[%d][%d] = %12.6g\n",i1,i2,AA[i1+nX*i2]);
    }}
    TCL::trsequ(AA.GetArray(),nX,BB.GetArray(), 1);
    nneg=0;  
    for (int i=1;i<nX;i++) {
       if (ignore[i]) 		continue;
       double bj = BB[i];
       if (bj <  0.01) {nneg++; ignore[i]=-1;}
       if (bj >  1.00) {nneg++; ignore[i]= 1;}
    }
  }//End while

  for (int i=1;i<nX;i++) {//print results
     if (ignore[i]<0) 		continue;
     double bj = BB[i];
     double est = bj*V[i];
     if (fabs(bj ) <= 0.0)	continue;
     if (fabs(est) <= 0.0)	continue;
//     if ((bj ) < 0.001) 	continue;
//     if ((est) < 0.010)		continue;
     printf ("Maker(%d) %s \tCorr =%g \tLeak =%g\n",i,RR.Name(i),bj,est);
  }//end for print results

// Test result

  for (int ievt=0;ievt<nEvents;ievt++) {
     double diff = FF.GetEvtRes(ievt);
     double eSize = FF.GetEvtSiz(ievt);
     if (ievt<=skipEvents) continue;
     double dot = TCL::vdot(FF.GetEvt(ievt),BB.GetArray(),nX);
     H2->Fill(eSize,diff-dot);

  }//end LOOP
//  for (int i=1;i<nX;i++) {printf ("Maker(%d) %s \tV=%g\n",i,RR.Name(i),V[i]);}

//		Second Method

  for (int i=1;i<=nMk;i++) {
    for (int j=0;j<5;j++) {Corr[j][i]/=nCorr;}
      double x  = Corr[0][i];
      double r  = Corr[1][i];
      double xx = Corr[2][i];
      double rr = Corr[3][i];
      double xr = Corr[4][i];
      Corr[2][i]=1.e+10;
//printf("%4d - x,r,xx,rr,xr = %g %g %g %g %g \n",i,x,r,xx,rr,xr);
      double Dxx = xx - x*x;
      if (Dxx< 1.e-10) continue;
      double Drr = rr - r*r;
      double Dxr = xr - x*r;
      double chisq = 1.e+10;
      double b = Dxr/Dxx;
      double a = r-b*x;
      double chisq = a*a+b*b*xx+rr+2.*a*b*x-2*a*r-2*b*xr;
      Corr[0][i]=a;
      Corr[1][i]=b;
      if (b<0. || b> 1.2) chisq+=1.e+6;
      Corr[2][i]=chisq;
   }
    int *Idx= (int*)Corr[5];
    double *bb = Corr[1];
    double *sq = Corr[2];
    TMath::Sort(nMk,sq+1,Idx+1,0);
   int j;double leak;
   for (int i=1;i<=nMk;i++) {
     j = Idx[i]+1;
     assert(j>0 && j<=nMk);
     if (sq[j]>=1.e+10) break;
     leak = V[j]*bb[j]/nFill;
     printf("%4d - %12s(%d) Chisq =%g Corr = %g Leak=%g\n",i,RR.Name(j),j,sq[j],bb[j],leak);
   }
   printf("===================End Of LeakFit========================");

}//end LeakFit
//______________________________________________________________________________
void LeakCount(char *file, const char *sele) 
{

  ReadLog rr(file);
  const char *name;
  int kind,idx; double exe,heap,free,inc;
  int num = 0;  
  double summ = 0;


  while ((name = rr.Next(kind,idx,exe,heap,free,inc))) { 
    if (kind<2) 				continue;
    if (sele && *sele && strcmp(name,sele))	continue;
    num++;
    summ +=inc;
  printf("%4d - %20s \t%d %d \t %12.6f %12.6f %+12.6f\t%12.6f\n",num,name,kind,idx,exe,heap,inc,summ);
  }
  
  printf("%s = %d %g\n",sele,num,summ);
} 
