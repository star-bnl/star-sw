#include "TROOT.h"
#include "TApplication.h"
#include "TLorentzVector.h"
#include "TFileIter.h"
#include "TTableMap.h"
#include "TDataSet.h"
#include "TGenericTable.h"
#include "atlsim/hepevt61.h"
//#include "roread.h"

//gcc3.2 
#include <iostream>
 using namespace std;

typedef    TGenericTable RootParticle;
typedef    TGenericTable RootVertex;
typedef    char          RootParticle_t;
typedef    char          RootVertex_t;
extern "C" int  islflag_ (const char*, const char*, int, int);
extern "C" int  iagflag_ (const char*, int);
TFileIter    *ATF      = 0;

//----------------------------------------------------------------------
extern "C" int roclose_()   { delete ATF; ATF=0; return 0; }
//----------------------------------------------------------------------
extern "C" int roopen_(char* FileName,int)
{  roclose_(); 
   gROOT->ProcessLine("typedef TGenericTable RootParticle;");
   gROOT->ProcessLine("typedef TGenericTable RootVertex;");
   gROOT->ProcessLine("typedef TGenericTable EventInfoRoot;");
   gROOT->ProcessLine("typedef TGenericTable RootGenEventProperties;");
   ATF = new TFileIter (FileName); 
   if (ATF->GetTFile()->IsZombie()) roclose_();
   return ATF?-1:0;
}   
//----------------------------------------------------------------------
extern "C" int ronext_()
{  
   int Iprin=islflag_("INPU","PRIN",4,4);
   int Iskip=iagflag_("SKIP",4);
   memset (&hepevt_,0,sizeof(hepevt_));
   hepevtd_.lmxhep    = NMXHEP;  
   hepevtd_.nbytespw  = 8; 
   TDataSet  *NextEv = 0;
   RootVertex *vTable = 0;
   RootParticle_t *iPart =0;
   int start=0, stop=0, np=0; 

   for ( RootParticle *pTable=0;  !pTable;  delete NextEv, ++(*ATF) )
   {
     const char *title = (const char *) *ATF;
     if (title==0)           return  1;                // end_of_file
     if (Iskip>0)            { ++(*ATF); return 0; }   // skip
     NextEv = (TDataSet*) * (*ATF);
     if (!NextEv)            return -1;                // error

     TDataSet *pEvent = NextEv->FindByName("MCEvent");
     if (pEvent) 
     { pTable = (RootParticle *) pEvent->FindByName("particle");
       vTable = (RootVertex *)   pEvent->FindByName("vertex");
     }
     if (!pTable || !vTable) continue;
 
     strncpy(hepevtd_.cevgen,pEvent->GetTitle(),8);
     hepevt_.NEVHEP    = -1;
     hepevtd_.irunnum  = -1;
     const char *run   = strchr(title,'.');
     if (run) 
     {  hepevtd_.irunnum  = atoi(run+1);
        const char *evt   = strchr(run+1,'.');
        if (evt) hepevt_.NEVHEP    = atoi(evt+1);
     }
     int nVert         = vTable->GetNRows();
     int nPart         = pTable->GetNRows();

     if (Iprin>0) cout <<" RoNext: generator "<<pEvent->GetTitle()
        <<" run#"<<hepevtd_.irunnum<<" event#"<<hepevt_.NEVHEP
        <<" has "<<nPart<<" particles in "<<nVert<<" verteces "<<endl;

     int barcode  = pTable->GetOffset("barcode");
     int status   = pTable->GetOffset("status");
     int pdg_id   = pTable->GetOffset("pdg_id"); 
     int momentum = pTable->GetOffset("momentum"); 
     int position = vTable->GetOffset("position");
     int vertex   = pTable->GetOffset("vertex"); 
     TTable::EColumnType v_type = pTable->GetColumnType("vertex"); 

     for (int i=0; i<nPart; ++i) 
     { 
       iPart = pTable->GetTable(i);

       int m = i; 
       if (barcode>0) m = *(int *)(iPart+barcode)-1; 
       if (0>m||m>=NMXHEP) {cout<<"RoNEXT: wrong BarCode "<<m<<endl; continue;}
       if (Iprin>0 && i!=m) cout <<" RoNext: ipart "<<i<<" code "<<m<<endl;
       np    = TMath::Max(np,m+1);
       hepevt_.ISTHEP[m]   = *(int*) (iPart+status); 
       hepevt_.IDHEP [m]   = *(int*) (iPart+pdg_id); 

       for (int j=0; j<4; j++) 
           hepevt_.PHEP[m][j]  = *((float*)(iPart+momentum)+j);

       TLorentzVector P((float*)(iPart+momentum));
       double m2 = P*P;
       if (m2>0.000001) hepevt_.PHEP[m][4] = sqrt(m2);
       
       if (v_type==TTable::kShort) 
	 { start=*(short*)(iPart+vertex); stop =*((short*)(iPart+vertex)+1); }
       else
	 { TTableMap & vertices = **(TTableMap**)(iPart+vertex);
	 //       vertices->Dump();
	 //================================================
           start= vertices[0];     stop= vertices[1];
         }

       if (start>0) 
       { RootVertex_t *iVert = vTable->GetTable(start);
         for (int j=0; j<3; j++) 
             hepevt_.VHEP[m][j]=*((float*)(iVert+position)+j);
       }
       if (*(int*)(iPart+status)==1 && stop>=0)
        cout <<" RoNEXT: bad part "<<i<<" start "<<start<<" stop "<<stop<<endl;
     }

     // now restore mother-daughter cross-reference
     for (int k=0; k<nVert; ++k)
     { //RootVertex_t &iVert = *vTable->GetTable(k);

       // for selected vertex find its parents and children
       int i1=0, i2=0, i3=0, i4=0, is=1;
       for (int i=0; i<nPart; ++i) 
       { 
         iPart  = pTable->GetTable(i);

         if (v_type==TTable::kShort) 
	   { start=*(short*)(iPart+vertex); stop =*((short*)(iPart+vertex)+1);}
         else
	   { TTableMap & vertices = **(TTableMap**)(iPart+vertex);
             start= vertices[0];     stop= vertices[1];
           }

         int id = i+1; if (barcode>0) id=*(int *)(iPart+barcode); 

         // if the particle stopped here, it is the vertex's mother
         if (stop==k) 
	 { if (i1==0) { i1=id; } else
           { if (i2>0) is=-1; i1=TMath::Min(i1,id); i2=TMath::Max(i2,id); }
         }        
         // remember the list of the vertex's daughters
         if (start==k)
         { if (i3==0) { i3=id; } else
	   { i3=TMath::Min(i3,id); i4=TMath::Max(i4,id); }          
         }
       } 

       // set parentship to any particle that refers this vertex
       for (int i=0; i<nPart; ++i) 
       { 
         iPart  = pTable->GetTable(i);

         if (v_type==TTable::kShort) 
	   { start=*(short*)(iPart+vertex); stop =*((short*)(iPart+vertex)+1);}
         else
	   { TTableMap & vertices = **(TTableMap**)(iPart+vertex);
             start= vertices[0];     stop= vertices[1];
           }

         if(start==k) {hepevt_.JMOHEP[i][0]=i1; hepevt_.JMOHEP[i][1]=is*i2;}
         if(stop==k)  {hepevt_.JDAHEP[i][0]=i3; hepevt_.JDAHEP[i][1]=i4;   }
       }      
     }
     hepevt_.NHEP=np;
   }
   return 0;
}





