void res()
{
  c1 = new TCanvas(); 
  c1->Clear();
  c1->Divide(2,4);
  c1->cd(1);R_dzVsTanL_L0_S0->Draw("ZCOL");c1->cd(2);R_dz_L0_S0->Draw();
  c1->cd(3);R_dzVsTanL_L0_S1->Draw("ZCOL");c1->cd(4);R_dz_L0_S1->Draw();
  c1->cd(5);R_dzVsTanL_L0_S2->Draw("ZCOL");c1->cd(6);R_dz_L0_S2->Draw();
  c1->cd(7);R_dzVsTanL_L0_S3->Draw("ZCOL");c1->cd(8);R_dz_L0_S3->Draw();
  c1->Print("R_dz_L0.gif");

  c1->Clear();
  c1->Divide(2,4);
  c1->cd(1);R_dzVsTanL_L1_S0->Draw("ZCOL");c1->cd(2);R_dz_L1_S0->Draw();
  c1->cd(3);R_dzVsTanL_L1_S1->Draw("ZCOL");c1->cd(4);R_dz_L1_S1->Draw();
  c1->cd(5);R_dzVsTanL_L1_S2->Draw("ZCOL");c1->cd(6);R_dz_L1_S2->Draw();
  c1->cd(7);R_dzVsTanL_L1_S3->Draw("ZCOL");c1->cd(8);R_dz_L1_S3->Draw();
  c1->Print("R_dz_L1.gif");

  c1->Clear();
  c1->Divide(2,6);
  c1->cd(1);R_dzVsTanL_L2_S0->Draw("ZCOL");c1->cd(2);R_dz_L2_S0->Draw();
  c1->cd(3);R_dzVsTanL_L2_S1->Draw("ZCOL");c1->cd(4);R_dz_L2_S1->Draw();
  c1->cd(5);R_dzVsTanL_L2_S2->Draw("ZCOL");c1->cd(6);R_dz_L2_S2->Draw();
  c1->cd(7);R_dzVsTanL_L2_S3->Draw("ZCOL");c1->cd(8);R_dz_L2_S3->Draw();
  c1->cd(9);R_dzVsTanL_L2_S3->Draw("ZCOL");c1->cd(10);R_dz_L2_S3->Draw();
  c1->cd(11);R_dzVsTanL_L2_S3->Draw("ZCOL");c1->cd(12);R_dz_L2_S3->Draw();
  c1->Print("R_dz_L2.gif");

  c1->Clear();
  c1->Divide(2,6);
  c1->cd(1);R_dzVsTanL_L3_S0->Draw("ZCOL");c1->cd(2);R_dz_L3_S0->Draw();
  c1->cd(3);R_dzVsTanL_L3_S1->Draw("ZCOL");c1->cd(4);R_dz_L3_S1->Draw();
  c1->cd(5);R_dzVsTanL_L3_S2->Draw("ZCOL");c1->cd(6);R_dz_L3_S2->Draw();
  c1->cd(7);R_dzVsTanL_L3_S3->Draw("ZCOL");c1->cd(8);R_dz_L3_S3->Draw();
  c1->cd(9);R_dzVsTanL_L3_S3->Draw("ZCOL");c1->cd(10);R_dz_L3_S3->Draw();
  c1->cd(11);R_dzVsTanL_L3_S3->Draw("ZCOL");c1->cd(12);R_dz_L3_S3->Draw();
  c1->Print("R_dz_L3.gif");

 c1->Clear();
  c1->Divide(2,8);
  c1->cd(1);R_dzVsTanL_L4_S0->Draw("ZCOL");c1->cd(2);R_dz_L4_S0->Draw();
  c1->cd(3);R_dzVsTanL_L4_S1->Draw("ZCOL");c1->cd(4);R_dz_L4_S1->Draw();
  c1->cd(5);R_dzVsTanL_L4_S2->Draw("ZCOL");c1->cd(6);R_dz_L4_S2->Draw();
  c1->cd(7);R_dzVsTanL_L4_S3->Draw("ZCOL");c1->cd(8);R_dz_L4_S3->Draw();
  c1->cd(9);R_dzVsTanL_L4_S3->Draw("ZCOL");c1->cd(10);R_dz_L4_S3->Draw();
  c1->cd(11);R_dzVsTanL_L4_S3->Draw("ZCOL");c1->cd(12);R_dz_L4_S3->Draw();
  c1->cd(13);R_dzVsTanL_L4_S3->Draw("ZCOL");c1->cd(14);R_dz_L4_S3->Draw();
  c1->cd(15);R_dzVsTanL_L4_S3->Draw("ZCOL");c1->cd(16);R_dz_L4_S3->Draw();
  c1->Print("R_dz_L4.gif");

 c1->Clear();
  c1->Divide(2,8);
  c1->cd(1);R_dzVsTanL_L5_S0->Draw("ZCOL");c1->cd(2);R_dz_L5_S0->Draw();
  c1->cd(3);R_dzVsTanL_L5_S1->Draw("ZCOL");c1->cd(4);R_dz_L5_S1->Draw();
  c1->cd(5);R_dzVsTanL_L5_S2->Draw("ZCOL");c1->cd(6);R_dz_L5_S2->Draw();
  c1->cd(7);R_dzVsTanL_L5_S3->Draw("ZCOL");c1->cd(8);R_dz_L5_S3->Draw();
  c1->cd(9);R_dzVsTanL_L5_S3->Draw("ZCOL");c1->cd(10);R_dz_L5_S3->Draw();
  c1->cd(11);R_dzVsTanL_L5_S3->Draw("ZCOL");c1->cd(12);R_dz_L5_S3->Draw();
  c1->cd(13);R_dzVsTanL_L5_S3->Draw("ZCOL");c1->cd(14);R_dz_L5_S3->Draw();
  c1->cd(15);R_dzVsTanL_L5_S3->Draw("ZCOL");c1->cd(16);R_dz_L5_S3->Draw();
  c1->Print("R_dz_L5.gif");



  c1->Clear();
  c1->Divide(2,4);
  c1->cd(1);R_dyVsTanCA_L0_S0->Draw("ZCOL");c1->cd(2);R_dy_L0_S0->Draw();
  c1->cd(3);R_dyVsTanCA_L0_S1->Draw("ZCOL");c1->cd(4);R_dy_L0_S1->Draw();
  c1->cd(5);R_dyVsTanCA_L0_S2->Draw("ZCOL");c1->cd(6);R_dy_L0_S2->Draw();
  c1->cd(7);R_dyVsTanCA_L0_S3->Draw("ZCOL");c1->cd(8);R_dy_L0_S3->Draw();
  c1->Print("R_dy_L0.gif");

  c1->Clear();
  c1->Divide(2,4);
  c1->cd(1);R_dyVsTanCA_L1_S0->Draw("ZCOL");c1->cd(2);R_dy_L1_S0->Draw();
  c1->cd(3);R_dyVsTanCA_L1_S1->Draw("ZCOL");c1->cd(4);R_dy_L1_S1->Draw();
  c1->cd(5);R_dyVsTanCA_L1_S2->Draw("ZCOL");c1->cd(6);R_dy_L1_S2->Draw();
  c1->cd(7);R_dyVsTanCA_L1_S3->Draw("ZCOL");c1->cd(8);R_dy_L1_S3->Draw();
  c1->Print("R_dy_L1.gif");

  c1->Clear();
  c1->Divide(2,6);
  c1->cd(1);R_dyVsTanCA_L2_S0->Draw("ZCOL");c1->cd(2);R_dy_L2_S0->Draw();
  c1->cd(3);R_dyVsTanCA_L2_S1->Draw("ZCOL");c1->cd(4);R_dy_L2_S1->Draw();
  c1->cd(5);R_dyVsTanCA_L2_S2->Draw("ZCOL");c1->cd(6);R_dy_L2_S2->Draw();
  c1->cd(7);R_dyVsTanCA_L2_S3->Draw("ZCOL");c1->cd(8);R_dy_L2_S3->Draw();
  c1->cd(9);R_dyVsTanCA_L2_S3->Draw("ZCOL");c1->cd(10);R_dy_L2_S3->Draw();
  c1->cd(11);R_dyVsTanCA_L2_S3->Draw("ZCOL");c1->cd(12);R_dy_L2_S3->Draw();
  c1->Print("R_dy_L2.gif");

  c1->Clear();
  c1->Divide(2,6);
  c1->cd(1);R_dyVsTanCA_L3_S0->Draw("ZCOL");c1->cd(2);R_dy_L3_S0->Draw();
  c1->cd(3);R_dyVsTanCA_L3_S1->Draw("ZCOL");c1->cd(4);R_dy_L3_S1->Draw();
  c1->cd(5);R_dyVsTanCA_L3_S2->Draw("ZCOL");c1->cd(6);R_dy_L3_S2->Draw();
  c1->cd(7);R_dyVsTanCA_L3_S3->Draw("ZCOL");c1->cd(8);R_dy_L3_S3->Draw();
  c1->cd(9);R_dyVsTanCA_L3_S3->Draw("ZCOL");c1->cd(10);R_dy_L3_S3->Draw();
  c1->cd(11);R_dyVsTanCA_L3_S3->Draw("ZCOL");c1->cd(12);R_dy_L3_S3->Draw();
  c1->Print("R_dy_L3.gif");

 c1->Clear();
  c1->Divide(2,8);
  c1->cd(1);R_dyVsTanCA_L4_S0->Draw("ZCOL");c1->cd(2);R_dy_L4_S0->Draw();
  c1->cd(3);R_dyVsTanCA_L4_S1->Draw("ZCOL");c1->cd(4);R_dy_L4_S1->Draw();
  c1->cd(5);R_dyVsTanCA_L4_S2->Draw("ZCOL");c1->cd(6);R_dy_L4_S2->Draw();
  c1->cd(7);R_dyVsTanCA_L4_S3->Draw("ZCOL");c1->cd(8);R_dy_L4_S3->Draw();
  c1->cd(9);R_dyVsTanCA_L4_S3->Draw("ZCOL");c1->cd(10);R_dy_L4_S3->Draw();
  c1->cd(11);R_dyVsTanCA_L4_S3->Draw("ZCOL");c1->cd(12);R_dy_L4_S3->Draw();
  c1->cd(13);R_dyVsTanCA_L4_S3->Draw("ZCOL");c1->cd(14);R_dy_L4_S3->Draw();
  c1->cd(15);R_dyVsTanCA_L4_S3->Draw("ZCOL");c1->cd(16);R_dy_L4_S3->Draw();
  c1->Print("R_dy_L4.gif");

 c1->Clear();
  c1->Divide(2,8);
  c1->cd(1);R_dyVsTanCA_L5_S0->Draw("ZCOL");c1->cd(2);R_dy_L5_S0->Draw();
  c1->cd(3);R_dyVsTanCA_L5_S1->Draw("ZCOL");c1->cd(4);R_dy_L5_S1->Draw();
  c1->cd(5);R_dyVsTanCA_L5_S2->Draw("ZCOL");c1->cd(6);R_dy_L5_S2->Draw();
  c1->cd(7);R_dyVsTanCA_L5_S3->Draw("ZCOL");c1->cd(8);R_dy_L5_S3->Draw();
  c1->cd(9);R_dyVsTanCA_L5_S3->Draw("ZCOL");c1->cd(10);R_dy_L5_S3->Draw();
  c1->cd(11);R_dyVsTanCA_L5_S3->Draw("ZCOL");c1->cd(12);R_dy_L5_S3->Draw();
  c1->cd(13);R_dyVsTanCA_L5_S3->Draw("ZCOL");c1->cd(14);R_dy_L5_S3->Draw();
  c1->cd(15);R_dyVsTanCA_L5_S3->Draw("ZCOL");c1->cd(16);R_dy_L5_S3->Draw();
  c1->Print("R_dy_L5.gif");


}
