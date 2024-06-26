#include "tables/St_Survey_Table.h"

TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Geometry/pxl/.pxlLadderOnSector/pxlLadderOnSectorMisalign Allocated rows: 40  Used rows: 40  Row size: 180 bytes
//  Table: Survey_st[0]--> Survey_st[39]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_Survey")) return 0;
Survey_st row;
St_Survey *tableSet = new St_Survey("pxlLadderOnSectorMisalign",40);
//
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          1; // ;
    row.r00	 =  0.9994715; // ;
    row.r01	 = 0.0008704342; // -gamma ;
    row.r02	 = 0.0003113637; // beta  ;
    row.r10	 = -0.001040789; // gamma ;
    row.r11	 =  0.9991744; // ;
    row.r12	 = -0.0002015136; // -alpha ;
    row.r20	 = -0.0002434218; // -beta  ;
    row.r21	 = -0.0002077138; // alpha ;
    row.r22	 =  0.9994585; // ;
    row.t0	 = -0.005874201; // ;
    row.t1	 = -0.02995291; // ;
    row.t2	 = 0.03392109; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          2; // ;
    row.r00	 =  0.9994089; // ;
    row.r01	 = -0.01240364; // -gamma ;
    row.r02	 = -0.0002771866; // beta  ;
    row.r10	 = 0.01228829; // gamma ;
    row.r11	 =   0.999086; // ;
    row.r12	 = -0.0007085575; // -alpha ;
    row.r20	 = 0.0001844822; // -beta  ;
    row.r21	 = 0.001107287; // alpha ;
    row.r22	 =  0.9994579; // ;
    row.t0	 = -0.01843921; // ;
    row.t1	 = 0.04739192; // ;
    row.t2	 = 0.007138973; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          3; // ;
    row.r00	 =  0.9994479; // ;
    row.r01	 = -3.742032e-05; // -gamma ;
    row.r02	 = -0.001512795; // beta  ;
    row.r10	 = -0.00019697; // gamma ;
    row.r11	 =  0.9991986; // ;
    row.r12	 = -0.0006992775; // -alpha ;
    row.r20	 = 0.001492008; // -beta  ;
    row.r21	 = 0.001113827; // alpha ;
    row.r22	 =  0.9994563; // ;
    row.t0	 = -0.003672032; // ;
    row.t1	 = 0.04390356; // ;
    row.t2	 = 0.009344134; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          4; // ;
    row.r00	 =  0.9993766; // ;
    row.r01	 = 0.005330516; // -gamma ;
    row.r02	 = 3.649917e-05; // beta  ;
    row.r10	 = -0.005644842; // gamma ;
    row.r11	 =  0.9992404; // ;
    row.r12	 = -5.361717e-05; // -alpha ;
    row.r20	 = 2.671367e-05; // -beta  ;
    row.r21	 = 0.0004631218; // alpha ;
    row.r22	 =  0.9994585; // ;
    row.t0	 = -0.01640474; // ;
    row.t1	 = 0.04653654; // ;
    row.t2	 = 0.04619534; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          5; // ;
    row.r00	 =  0.9992876; // ;
    row.r01	 = 0.0002339351; // -gamma ;
    row.r02	 = -0.00199988; // beta  ;
    row.r10	 = -0.0005711978; // gamma ;
    row.r11	 =  0.9993585; // ;
    row.r12	 = -0.0008061549; // -alpha ;
    row.r20	 = 0.001814556; // -beta  ;
    row.r21	 = 0.0004365975; // alpha ;
    row.r22	 =  0.9994562; // ;
    row.t0	 = -0.0534223; // ;
    row.t1	 = -0.01594906; // ;
    row.t2	 = 0.02072373; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          6; // ;
    row.r00	 =  0.9992804; // ;
    row.r01	 = -0.008934781; // -gamma ;
    row.r02	 = 0.001625907; // beta  ;
    row.r10	 = 0.008591308; // gamma ;
    row.r11	 =  0.9992889; // ;
    row.r12	 = 0.0004978472; // -alpha ;
    row.r20	 = -0.001477722; // -beta  ;
    row.r21	 = -9.851552e-05; // alpha ;
    row.r22	 =  0.9994568; // ;
    row.t0	 = 0.03961354; // ;
    row.t1	 = 0.04530329; // ;
    row.t2	 = 0.02605865; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          7; // ;
    row.r00	 =  0.9992513; // ;
    row.r01	 = 0.0003877072; // -gamma ;
    row.r02	 = 0.001782519; // beta  ;
    row.r10	 = -0.000697858; // gamma ;
    row.r11	 =  0.9993955; // ;
    row.r12	 = -0.0002570324; // -alpha ;
    row.r20	 = -0.001556622; // -beta  ;
    row.r21	 = 0.0006031311; // alpha ;
    row.r22	 =  0.9994566; // ;
    row.t0	 =  0.0286807; // ;
    row.t1	 = 0.03069906; // ;
    row.t2	 = -0.01285051; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x08",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          8; // ;
    row.r00	 =  0.9991719; // ;
    row.r01	 = 0.006532398; // -gamma ;
    row.r02	 = 0.001958048; // beta  ;
    row.r10	 = -0.00676007; // gamma ;
    row.r11	 =  0.9994299; // ;
    row.r12	 = -9.510187e-05; // -alpha ;
    row.r20	 = -0.001667303; // -beta  ;
    row.r21	 = 0.0003763176; // alpha ;
    row.r22	 =  0.9994564; // ;
    row.t0	 = 0.04592797; // ;
    row.t1	 = 0.01889329; // ;
    row.t2	 = 0.01438148; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          9; // ;
    row.r00	 =  0.9991371; // ;
    row.r01	 = 0.001716776; // -gamma ;
    row.r02	 = -0.005272146; // beta  ;
    row.r10	 = -0.0017569; // gamma ;
    row.r11	 =  0.9994918; // ;
    row.r12	 = -0.0005641534; // -alpha ;
    row.r20	 = 0.004905031; // -beta  ;
    row.r21	 = 0.0003819628; // alpha ;
    row.r22	 =  0.9994452; // ;
    row.t0	 = 0.01378198; // ;
    row.t1	 = 0.01378764; // ;
    row.t2	 = -0.002752911; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         10; // ;
    row.r00	 =  0.9991253; // ;
    row.r01	 = -0.008041377; // -gamma ;
    row.r02	 = 0.001259126; // beta  ;
    row.r10	 = 0.007942658; // gamma ;
    row.r11	 =  0.9994554; // ;
    row.r12	 = 0.0003602325; // -alpha ;
    row.r20	 = -0.0009123806; // -beta  ;
    row.r21	 = -0.0001279579; // alpha ;
    row.r22	 =   0.999456; // ;
    row.t0	 = 0.02694869; // ;
    row.t1	 = 0.01339353; // ;
    row.t2	 = 0.007582102; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         11; // ;
    row.r00	 =  0.9991508; // ;
    row.r01	 = 0.001918645; // -gamma ;
    row.r02	 = 0.001360005; // beta  ;
    row.r10	 = -0.001877765; // gamma ;
    row.r11	 =   0.999492; // ;
    row.r12	 = 3.952373e-05; // -alpha ;
    row.r20	 = -0.0009732427; // -beta  ;
    row.r21	 = 0.0001063804; // alpha ;
    row.r22	 =  0.9994569; // ;
    row.t0	 = 0.02882467; // ;
    row.t1	 = 0.003543128; // ;
    row.t2	 = 0.00493628; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         12; // ;
    row.r00	 =  0.9991742; // ;
    row.r01	 = 0.001415759; // -gamma ;
    row.r02	 = 0.001434615; // beta  ;
    row.r10	 = -0.001238814; // gamma ;
    row.r11	 =  0.9994696; // ;
    row.r12	 = 6.066667e-05; // -alpha ;
    row.r20	 = -0.001025387; // -beta  ;
    row.r21	 = 2.051062e-06; // alpha ;
    row.r22	 =  0.9994568; // ;
    row.t0	 = 0.03253407; // ;
    row.t1	 = -0.002929702; // ;
    row.t2	 = 0.008584991; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         13; // ;
    row.r00	 =   0.999249; // ;
    row.r01	 = 0.002470255; // -gamma ;
    row.r02	 = -0.0003592433; // beta  ;
    row.r10	 = -0.002156963; // gamma ;
    row.r11	 =  0.9993915; // ;
    row.r12	 = 0.0004904232; // -alpha ;
    row.r20	 = -4.953452e-05; // -beta  ;
    row.r21	 = -0.0004286325; // alpha ;
    row.r22	 =  0.9994589; // ;
    row.t0	 = 0.0007950227; // ;
    row.t1	 = 0.03754923; // ;
    row.t2	 = 0.02389889; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         14; // ;
    row.r00	 =  0.9991485; // ;
    row.r01	 =  -0.012278; // -gamma ;
    row.r02	 = 8.005027e-05; // beta  ;
    row.r10	 = 0.01256119; // gamma ;
    row.r11	 =  0.9993435; // ;
    row.r12	 = -0.0008991163; // -alpha ;
    row.r20	 = 0.0003446376; // -beta  ;
    row.r21	 = 0.000872337; // alpha ;
    row.r22	 =  0.9994584; // ;
    row.t0	 = -0.0009109689; // ;
    row.t1	 = -0.01710807; // ;
    row.t2	 = 0.01196063; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         15; // ;
    row.r00	 =  0.9992893; // ;
    row.r01	 = 0.001605901; // -gamma ;
    row.r02	 = -0.0002626686; // beta  ;
    row.r10	 = -0.001268852; // gamma ;
    row.r11	 =  0.9993571; // ;
    row.r12	 = -0.0005969581; // -alpha ;
    row.r20	 = 0.0006621276; // -beta  ;
    row.r21	 = 0.0004897464; // alpha ;
    row.r22	 =  0.9994585; // ;
    row.t0	 = -0.02577793; // ;
    row.t1	 = -0.01798527; // ;
    row.t2	 = 0.006117531; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         16; // ;
    row.r00	 =  0.9993538; // ;
    row.r01	 = 0.003756574; // -gamma ;
    row.r02	 = -0.0006708502; // beta  ;
    row.r10	 = -0.003421119; // gamma ;
    row.r11	 =  0.9992795; // ;
    row.r12	 = -0.0006241738; // -alpha ;
    row.r20	 = 0.001038222; // -beta  ;
    row.r21	 = 0.0004388728; // alpha ;
    row.r22	 =  0.9994583; // ;
    row.t0	 = -0.02417224; // ;
    row.t1	 = -0.01269119; // ;
    row.t2	 = 0.002257769; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"i",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         17; // ;
    row.r00	 =  0.9994494; // ;
    row.r01	 = -0.0001155805; // -gamma ;
    row.r02	 = -0.001934532; // beta  ;
    row.r10	 = 0.0003461524; // gamma ;
    row.r11	 =  0.9991971; // ;
    row.r12	 = 0.0001217719; // -alpha ;
    row.r20	 =  0.0016394; // -beta  ;
    row.r21	 = 0.0001686914; // alpha ;
    row.r22	 =   0.999457; // ;
    row.t0	 = -6.701782e-05; // ;
    row.t1	 = 0.02721294; // ;
    row.t2	 = 0.02238843; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         18; // ;
    row.r00	 =  0.9992819; // ;
    row.r01	 = -0.01694127; // -gamma ;
    row.r02	 = 0.001850319; // beta  ;
    row.r10	 = 0.01721313; // gamma ;
    row.r11	 =  0.9990731; // ;
    row.r12	 = -0.000452148; // -alpha ;
    row.r20	 = -0.001525333; // -beta  ;
    row.r21	 = 0.0002169993; // alpha ;
    row.r22	 =  0.9994571; // ;
    row.t0	 = -0.001121842; // ;
    row.t1	 = -0.004888067; // ;
    row.t2	 = 0.009981077; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         19; // ;
    row.r00	 =  0.9994711; // ;
    row.r01	 = -0.00149665; // -gamma ;
    row.r02	 = 0.002183733; // beta  ;
    row.r10	 = 0.001665105; // gamma ;
    row.r11	 =  0.9991732; // ;
    row.r12	 = -0.0008704285; // -alpha ;
    row.r20	 = -0.001922701; // -beta  ;
    row.r21	 = 0.0005511399; // alpha ;
    row.r22	 =  0.9994563; // ;
    row.t0	 = 6.716547e-05; // ;
    row.t1	 = -0.01199871; // ;
    row.t2	 = -0.02648933; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         20; // ;
    row.r00	 =  0.9994501; // ;
    row.r01	 = 0.009202609; // -gamma ;
    row.r02	 = 0.001907599; // beta  ;
    row.r10	 = -0.009166762; // gamma ;
    row.r11	 =    0.99911; // ;
    row.r12	 = -0.00103425; // -alpha ;
    row.r20	 = -0.001726172; // -beta  ;
    row.r21	 = 0.0006489493; // alpha ;
    row.r22	 =  0.9994566; // ;
    row.t0	 = -0.002639106; // ;
    row.t1	 = -0.008599657; // ;
    row.t2	 = 0.04132399; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         21; // ;
    row.r00	 =  0.9995261; // ;
    row.r01	 = 0.0002587074; // -gamma ;
    row.r02	 = 0.00193691; // beta  ;
    row.r10	 = -0.0004734771; // gamma ;
    row.r11	 =  0.9991238; // ;
    row.r12	 = 0.00415551; // -alpha ;
    row.r20	 = -0.002137923; // -beta  ;
    row.r21	 = -0.003732072; // alpha ;
    row.r22	 =  0.9992915; // ;
    row.t0	 = 0.008777813; // ;
    row.t1	 = 0.05887766; // ;
    row.t2	 = 0.02239282; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         22; // ;
    row.r00	 =  0.9995044; // ;
    row.r01	 = -0.008720442; // -gamma ;
    row.r02	 = -0.001682061; // beta  ;
    row.r10	 = 0.008579445; // gamma ;
    row.r11	 =  0.9990673; // ;
    row.r12	 = -0.004895777; // -alpha ;
    row.r20	 = 0.001962802; // -beta  ;
    row.r21	 = 0.004476087; // alpha ;
    row.r22	 =  0.9992888; // ;
    row.t0	 = -0.02376743; // ;
    row.t1	 = -0.03516072; // ;
    row.t2	 = 0.03246935; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         23; // ;
    row.r00	 =  0.9994963; // ;
    row.r01	 = -0.000556895; // -gamma ;
    row.r02	 = -0.002583481; // beta  ;
    row.r10	 = 0.0002542923; // gamma ;
    row.r11	 =  0.9991511; // ;
    row.r12	 = -0.004492741; // -alpha ;
    row.r20	 = 0.002738026; // -beta  ;
    row.r21	 = 0.004047267; // alpha ;
    row.r22	 =  0.9992888; // ;
    row.t0	 = -0.03098668; // ;
    row.t1	 = -0.03295259; // ;
    row.t2	 = -0.01372619; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         24; // ;
    row.r00	 =  0.9994075; // ;
    row.r01	 = -0.005104249; // -gamma ;
    row.r02	 = -0.003201498; // beta  ;
    row.r10	 = 0.004688457; // gamma ;
    row.r11	 =  0.9992173; // ;
    row.r12	 = -0.003569786; // -alpha ;
    row.r20	 = 0.00327381; // -beta  ;
    row.r21	 = 0.003087266; // alpha ;
    row.r22	 =  0.9992906; // ;
    row.t0	 = -0.04270721; // ;
    row.t1	 = -0.03228788; // ;
    row.t2	 = 0.01082607; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         25; // ;
    row.r00	 =  0.9992933; // ;
    row.r01	 = 0.000489076; // -gamma ;
    row.r02	 = -0.0001295809; // beta  ;
    row.r10	 = -0.0009286586; // gamma ;
    row.r11	 =  0.9993651; // ;
    row.r12	 = 0.001717027; // -alpha ;
    row.r20	 = 0.0002162449; // -beta  ;
    row.r21	 = -0.001253842; // alpha ;
    row.r22	 =  0.9993018; // ;
    row.t0	 = -0.01566098; // ;
    row.t1	 = 0.02702046; // ;
    row.t2	 = 0.02151594; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         26; // ;
    row.r00	 =  0.9992702; // ;
    row.r01	 = -0.01155961; // -gamma ;
    row.r02	 = -0.0001586626; // beta  ;
    row.r10	 = 0.01111331; // gamma ;
    row.r11	 =  0.9992614; // ;
    row.r12	 = -0.002314121; // -alpha ;
    row.r20	 = 0.0001384957; // -beta  ;
    row.r21	 = 0.001843656; // alpha ;
    row.r22	 =  0.9993008; // ;
    row.t0	 = 0.002317048; // ;
    row.t1	 = -0.002024981; // ;
    row.t2	 = 0.01812599; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         27; // ;
    row.r00	 =  0.9992431; // ;
    row.r01	 = 0.001856613; // -gamma ;
    row.r02	 = -0.0008164454; // beta  ;
    row.r10	 = -0.002270536; // gamma ;
    row.r11	 =  0.9994113; // ;
    row.r12	 = -0.002165236; // -alpha ;
    row.r20	 = 0.0006751944; // -beta  ;
    row.r21	 = 0.001716457; // alpha ;
    row.r22	 =  0.9993009; // ;
    row.t0	 = -0.00266516; // ;
    row.t1	 = -0.001872341; // ;
    row.t2	 = 0.002381369; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"+\x01",2);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         28; // ;
    row.r00	 =  0.9991644; // ;
    row.r01	 = -0.002711181; // -gamma ;
    row.r02	 = -0.0006568043; // beta  ;
    row.r10	 = 0.002404229; // gamma ;
    row.r11	 =  0.9994877; // ;
    row.r12	 = -0.001699193; // -alpha ;
    row.r20	 = 0.000431683; // -beta  ;
    row.r21	 = 0.001286341; // alpha ;
    row.r22	 =  0.9993016; // ;
    row.t0	 = 0.006347117; // ;
    row.t1	 = -0.007291944; // ;
    row.t2	 = 0.01172001; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x11",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         29; // ;
    row.r00	 =  0.9991074; // ;
    row.r01	 = 8.147884e-05; // -gamma ;
    row.r02	 = -0.002446764; // beta  ;
    row.r10	 = -0.0001420895; // gamma ;
    row.r11	 =  0.9995495; // ;
    row.r12	 = 0.001514489; // -alpha ;
    row.r20	 = 0.00278907; // -beta  ;
    row.r21	 = -0.001189871; // alpha ;
    row.r22	 =  0.9992957; // ;
    row.t0	 = -0.04763201; // ;
    row.t1	 = 0.04450868; // ;
    row.t2	 = -0.004263502; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         30; // ;
    row.r00	 =  0.9990375; // ;
    row.r01	 = -0.01281002; // -gamma ;
    row.r02	 = 0.001706941; // beta  ;
    row.r10	 = 0.01267165; // gamma ;
    row.r11	 =  0.9994592; // ;
    row.r12	 = -0.001773741; // -alpha ;
    row.r20	 = -0.001998182; // -beta  ;
    row.r21	 = 0.001443809; // alpha ;
    row.r22	 =  0.9992973; // ;
    row.t0	 =  0.0525531; // ;
    row.t1	 = -0.01280127; // ;
    row.t2	 = 0.01956131; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x04\x04\x05\x05\x06",5);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         31; // ;
    row.r00	 =  0.9991073; // ;
    row.r01	 = -0.001493888; // -gamma ;
    row.r02	 = 0.00163348; // beta  ;
    row.r10	 = 0.001534962; // gamma ;
    row.r11	 =  0.9995496; // ;
    row.r12	 = -0.00223929; // -alpha ;
    row.r20	 = -0.002007222; // -beta  ;
    row.r21	 = 0.001958893; // alpha ;
    row.r22	 =  0.9992963; // ;
    row.t0	 = 0.03907461; // ;
    row.t1	 = -0.03563906; // ;
    row.t2	 = 0.01935668; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         32; // ;
    row.r00	 =  0.9990685; // ;
    row.r01	 = 0.01160331; // -gamma ;
    row.r02	 = 0.001485999; // beta  ;
    row.r10	 = -0.01139054; // gamma ;
    row.r11	 =  0.9994598; // ;
    row.r12	 = -0.002238764; // -alpha ;
    row.r20	 = -0.001936776; // -beta  ;
    row.r21	 = 0.002017796; // alpha ;
    row.r22	 =  0.9992963; // ;
    row.t0	 = 0.02986999; // ;
    row.t1	 = -0.03958499; // ;
    row.t2	 = 0.03977459; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"uestTime\x20/*\x20RUSR:\x20qiuh\x20|\x20SUSR:\x20q",32);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         33; // ;
    row.r00	 =  0.9992268; // ;
    row.r01	 = 0.00162774; // -gamma ;
    row.r02	 = -0.003037325; // beta  ;
    row.r10	 = -0.00122339; // gamma ;
    row.r11	 =  0.9994279; // ;
    row.r12	 = 0.001735309; // -alpha ;
    row.r20	 = 0.003507111; // -beta  ;
    row.r21	 = -0.001669584; // alpha ;
    row.r22	 =  0.9992925; // ;
    row.t0	 = -0.05078499; // ;
    row.t1	 = 0.05053603; // ;
    row.t2	 = 0.02191079; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         34; // ;
    row.r00	 =  0.9991403; // ;
    row.r01	 = -0.01002933; // -gamma ;
    row.r02	 = 0.003368286; // beta  ;
    row.r10	 = 0.01039321; // gamma ;
    row.r11	 =    0.99941; // ;
    row.r12	 = -0.001918201; // -alpha ;
    row.r20	 = -0.003809138; // -beta  ;
    row.r21	 = 0.001852138; // alpha ;
    row.r22	 =   0.999291; // ;
    row.t0	 =  0.0502178; // ;
    row.t1	 = -0.01993677; // ;
    row.t2	 = 0.02421544; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         35; // ;
    row.r00	 =  0.9992732; // ;
    row.r01	 = 0.002149142; // -gamma ;
    row.r02	 = 0.002886055; // beta  ;
    row.r10	 = -0.00170776; // gamma ;
    row.r11	 =  0.9993765; // ;
    row.r12	 = -0.002761492; // -alpha ;
    row.r20	 = -0.003362786; // -beta  ;
    row.r21	 = 0.002747755; // alpha ;
    row.r22	 =  0.9992906; // ;
    row.t0	 = 0.04290707; // ;
    row.t1	 = -0.03250496; // ;
    row.t2	 = -0.01573965; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         36; // ;
    row.r00	 =  0.9993466; // ;
    row.r01	 = 0.006877754; // -gamma ;
    row.r02	 = 0.002205992; // beta  ;
    row.r10	 = -0.006433234; // gamma ;
    row.r11	 =  0.9992647; // ;
    row.r12	 = -0.002877846; // -alpha ;
    row.r20	 = -0.002688244; // -beta  ;
    row.r21	 = 0.002950916; // alpha ;
    row.r22	 =   0.999292; // ;
    row.t0	 =  0.0380245; // ;
    row.t1	 = -0.04045353; // ;
    row.t2	 = 0.02900954; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         37; // ;
    row.r00	 =  0.9994779; // ;
    row.r01	 = 0.003578807; // -gamma ;
    row.r02	 = -0.00386454; // beta  ;
    row.r10	 = -0.003255102; // gamma ;
    row.r11	 =   0.999164; // ;
    row.r12	 = 0.002696752; // -alpha ;
    row.r20	 = 0.004287822; // -beta  ;
    row.r21	 = -0.00290802; // alpha ;
    row.r22	 =   0.999287; // ;
    row.t0	 = -0.04767707; // ;
    row.t1	 = 0.07733682; // ;
    row.t2	 = 0.02766126; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         38; // ;
    row.r00	 =  0.9994529; // ;
    row.r01	 = -0.0003626343; // -gamma ;
    row.r02	 = 0.003352879; // beta  ;
    row.r10	 = 0.0007374808; // gamma ;
    row.r11	 =  0.9992019; // ;
    row.r12	 = -0.002557169; // -alpha ;
    row.r20	 = -0.00378451; // -beta  ;
    row.r21	 = 0.002744593; // alpha ;
    row.r22	 =  0.9992896; // ;
    row.t0	 = 0.04338934; // ;
    row.t1	 = -0.03691828; // ;
    row.t2	 = 0.03593504; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         39; // ;
    row.r00	 =  0.9995099; // ;
    row.r01	 = -0.004127432; // -gamma ;
    row.r02	 = 0.003297134; // beta  ;
    row.r10	 = 0.004368937; // gamma ;
    row.r11	 =  0.9991276; // ;
    row.r12	 = -0.003340327; // -alpha ;
    row.r20	 = -0.003666788; // -beta  ;
    row.r21	 = 0.003627382; // alpha ;
    row.r22	 =  0.9992871; // ;
    row.t0	 = 0.03127428; // ;
    row.t1	 = -0.04883976; // ;
    row.t2	 = 0.006231238; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =         40; // ;
    row.r00	 =  0.9995469; // ;
    row.r01	 = 0.002477523; // -gamma ;
    row.r02	 = 0.002669223; // beta  ;
    row.r10	 = -0.002409924; // gamma ;
    row.r11	 =  0.9991019; // ;
    row.r12	 = -0.003527883; // -alpha ;
    row.r20	 = -0.002998828; // -beta  ;
    row.r21	 = 0.003866038; // alpha ;
    row.r22	 =  0.9992885; // ;
    row.t0	 = 0.01982234; // ;
    row.t1	 = -0.05209051; // ;
    row.t2	 = 0.02539924; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
