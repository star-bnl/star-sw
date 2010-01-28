#include <time.h>

TH1F* makeToySpin(int nEvents=10000, 
	      float AL=-.25, float P1=0.35, float P2=0.25, 
	      float AN=0., float dQ1=0.0,float dQ2=0.0,
	      float ALL=-0.4)
{

  float n[4]; // blue=1, yellow=2
  n[0]=1 +AL*P1 +AL*P2 +AN*dQ1 -AN*dQ2 +ALL*P1*P2; // STAR pol B+ Y +
  n[1]=1 +AL*P1 -AL*P2 +AN*dQ1 +AN*dQ2 -ALL*P1*P2; // STAR pol B+ Y -
  n[2]=1 -AL*P1 +AL*P2 -AN*dQ1 -AN*dQ2 -ALL*P1*P2; // STAR pol B- Y +
  n[3]=1 -AL*P1 -AL*P2 -AN*dQ1 +AN*dQ2 +ALL*P1*P2; // STAR pol B- Y -

  //assuming the luminosities of all four configurations are the same, the likelihood of a given W event being from configuration i is:
  float prob[4], total_prob, cumulative_prob[4];
  for (int i=0;i<4;i++)
    {
      prob[i]=n[i]/(n[0]+n[1]+n[2]+n[3]);
      total_prob+=prob[i];
      printf("i=%d prob=%f\n",i, prob[i]);
      cumulative_prob[i]=total_prob;
    }
  //sanity check:
  for (int i=0,;i<4;i++)
    assert(prob[i]>0);

  time_t t;
  time(&t);
  TRandom3 *rand=new TRandom3(t);

char tit[1000];
 sprintf(tit,"toy spin4 : AL=%.2f  P1=%.2f P2=%.2f AN=%.2f dQ1=%.2f dQ2=%.2f ALL=%.2f;spin4 state;counts",AL,P1,P2,AN,dQ1,dQ2,ALL);

TH1F* toySpin=new TH1F("toySpin4",tit,16,-0.5,15.5);

 for (int i=0;i<nEvents;i++)
   {
     float rnd=rand->Rndm();
     if (rnd<cumulative_prob[0]) toySpin->Fill(10);  // STAR pol B+ Y +
     else if (rnd<cumulative_prob[1]) toySpin->Fill(9); // STAR pol B+ Y -
     else if (rnd<cumulative_prob[2]) toySpin->Fill(6); // STAR pol B- Y +
     else if (rnd<cumulative_prob[3]) toySpin->Fill(5); // STAR pol B- Y -
   }

 toySpin->Draw();

 TFile *output=new TFile ("toySpin.hist.root","RECREATE");
 toySpin->Write();
 output->Close();
 return toySpin;
}
