{
   TCanvas c1("c1","fit with gap",10,10,600,900);
   c1.Divide(1,3);
   c1.cd(1);
   TF1 fgap("fgap","(1+x+x*x)*(abs(x-3)>0.5)",0,5);
   fgap.Draw();
   fgap.SetNpx(1000);
   c1.cd(2);
   TF1 f1("f1","[0]*exp(-0.5*((x-[1])/[2])**2)+([3]+[4]*x+[5]*x*x)",0,5);
   f1.SetParameters(60,3,.3,1,1,1);
   f1.Draw();
   c1.cd(3);
   TH1F h("h","test",100,0,5);
   h.FillRandom("f1",10000); 
   TF1
f2("f2","[0]*exp(-0.5*((x-[1])/[2])**2)*(abs(x-3)<0.5)+([3]+[4]*x+[5]*x*x)*(abs(x-3)>0.5)",0,5);
   f2.SetParameters(60,3,.7,1,1,1);
   h->Fit("f2");
}
