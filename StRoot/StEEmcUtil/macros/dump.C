    // -------------- E X T R A C T    C T B   H I T S   --------------------
    St_DataSet *gds=head->GetDataSet("geant");  assert(gds);
    St_g2t_ctf_hit *g2t_ctb_hit = (St_g2t_ctf_hit *) gds->Find("g2t_ctb_hit");
    assert(g2t_ctb_hit);

    printf("All CTB hits=%d\n",(int)g2t_ctb_hit->GetNRows());

    g2t_ctf_hit_st *ctb_hit = g2t_ctb_hit->GetTable();  assert(ctb_hit);
    for (i = 0; i < g2t_ctb_hit->GetNRows(); i++,ctb_hit++){
      float de_mev=ctb_hit->de*1000.;
      float tof_ns=ctb_hit->tof*1.e9;
      int id=ctb_hit->volume_id;
      printf(" CTB hit volume=%d, de=%f, tof=%f\n",id,de_mev,tof_ns);
    }

