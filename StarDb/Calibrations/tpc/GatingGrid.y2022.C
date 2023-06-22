TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_GatingGrid")) return 0;
/* 
 Table: GatingGrid : parameters from Timothy Camarda, 08/09/21
 Old Gating Grid for Run < 18: t0 = 320 ns, setting time = 2.5  us, tau = 2.5  / 4.6  = 543 ns
 New Gating Grid for Run   18: t0 = 240 ns, setting time = 1.43 us, tau = 1.43 / 4.6  = 311 ns      
 Old Gating Grid for Runs 19 - 21
 New Gating Grid for Run > 21: t0 = 240 ns, setting time = 2.0  us, tau = 2.0  / 4.6  = 435 ns.
 setting time = time of reaching transperency 99%
 tau = setting time/4.6 => exp(-4.6) = 1%
 description: Gating Grid transperancy = 0, for t < t0, and 1 - exp(-(t-t0)/tau), for t > t0
*/ 
  St_GatingGrid *tableSet = new St_GatingGrid("GatingGrid",1);
  GatingGrid_st row = {0.24, 2.0  };
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
