//read root histogram file and send all histograms to a postscript file
{
TFile file1("/diskA/star/kathy/output/psc0049_08_40evts_3EV.root");  
file1.ls();
TPostScript p5("myps.ps");
     QaGlobtrkNPoint->Draw();
     QaDstV0VertexK0Mass->Draw();
p5.Close();
}
