MySQL [Calibrations_tpc]> select entryTime,elementID,beginTime,flavor,deactive from TpcSecRowB where elementID=1 and beginTime > "2019" and deactive = 0 order by beginTime;
+---------------------+---------------------+
| entryTime           | beginTime           |
+---------------------+---------------------+
| 2019-04-16 19:41:11 | 2019-02-01 00:00:03 |
| 2019-04-28 20:49:12 | 2019-02-01 00:07:07 |

| 2020-07-08 16:59:47 | 2019-02-25 20:23:29 |
| 2021-05-10 20:35:27 | 2019-02-25 23:00:10 |
| 2021-08-18 13:35:21 | 2019-02-25 23:00:25 |x

| 2021-05-10 20:35:31 | 2019-04-04 00:00:10 |
| 2021-08-18 13:35:24 | 2019-04-04 00:00:25 |x

| 2021-02-11 15:47:50 | 2019-11-20 19:00:02 |
| 2021-02-11 15:47:52 | 2019-11-21 00:00:53 |

| 2021-02-11 15:47:55 | 2019-12-08 09:13:08 |x

| 2021-02-11 15:47:57 | 2019-12-21 15:40:21 |x
| 2021-02-11 15:48:00 | 2019-12-21 19:00:32 |x

| 2021-02-11 15:48:02 | 2020-01-28 18:29:12 |x

| 2021-02-11 15:48:05 | 2020-01-30 00:58:40 |x

| 2021-02-11 15:48:08 | 2020-01-31 01:21:12 |x
| 2021-02-11 15:48:10 | 2020-01-31 05:03:28 |x

| 2021-02-11 15:48:13 | 2020-02-01 19:19:04 |
| 2021-02-11 15:48:15 | 2020-02-02 16:04:09 |
| 2021-02-11 15:48:18 | 2020-02-03 20:25:34 |
| 2021-02-11 15:48:21 | 2020-02-04 05:35:18 |
| 2021-02-11 15:48:23 | 2020-02-05 14:46:26 |
| 2021-02-11 15:48:26 | 2020-02-10 22:04:28 |
| 2021-02-11 15:48:29 | 2020-02-13 15:23:39 |
| 2021-02-11 15:48:31 | 2020-02-14 14:37:42 |
| 2021-02-11 15:48:34 | 2020-02-24 23:07:40 |
| 2021-02-11 15:48:36 | 2020-06-17 23:32:08 |
| 2021-02-11 15:48:39 | 2020-07-30 01:53:54 |
| 2021-02-11 15:48:42 | 2020-07-30 04:28:36 |
| 2021-02-11 15:48:44 | 2020-08-13 21:32:51 |
| 2021-02-11 15:48:47 | 2020-08-19 19:22:53 |
| 2021-02-11 15:48:50 | 2020-08-20 05:54:02 |
| 2021-02-11 15:48:53 | 2020-08-27 14:35:07 |
| 2021-02-11 15:48:55 | 2020-08-28 06:42:50 |
| 2021-02-11 15:48:58 | 2020-09-02 10:54:40 |
| 2021-02-11 15:49:00 | 2020-09-03 17:11:15 |
| 2021-02-11 15:49:03 | 2020-09-07 22:11:41 |
| 2021-02-11 15:49:05 | 2020-09-10 04:16:39 |
| 2021-02-11 15:49:08 | 2020-09-12 10:20:33 |
| 2021-02-09 00:07:12 | 2021-01-29 00:00:06 |
+---------------------+---------------------+
40 rows in set, 1 warning (0.01 sec)

#[l409] ~/bin $ CreateRunXIXLinks.pl
ln -sf TpcSecRowB.19GeV_2019.C                              TpcSecRowB.20190225.202320.C #  2019-02-25 23:00:25
ln -sf TpcSecRowB.14p5GeV_2019.C                            TpcSecRowB.20190404.094405.C #  2019-04-04 00:00:25
ln -sf TpcSecRowB.7.3GeV_fixedTarget_2019.C                 TpcSecRowB.20190417.133401.C
ln -sf TpcSecRowB.14p5GeV_2019.C                            TpcSecRowB.20190417.141246.C
ln -sf TpcSecRowB.7.3GeV_fixedTarget_2019.C                 TpcSecRowB.20190423.155418.C
ln -sf TpcSecRowB.14p5GeV_2019.C                            TpcSecRowB.20190423.181158.C
ln -sf TpcSecRowB.7p7GeV_2019.C                             TpcSecRowB.20190603.223655.C
ln -sf TpcSecRowB.3p85GeV_fixedTarget_2019.C                TpcSecRowB.20190607.174702.C
ln -sf TpcSecRowB.7p7GeV_2019.C                             TpcSecRowB.20190607.202244.C
ln -sf TpcSecRowB.3p85GeV_fixedTarget_2019.C                TpcSecRowB.20190609.124528.C
ln -sf TpcSecRowB.7p7GeV_2019.C                             TpcSecRowB.20190610.024109.C
ln -sf TpcSecRowB.7.3GeV_fixedTarget_2019.C                 TpcSecRowB.20190618.143658.C
ln -sf TpcSecRowB.7p7GeV_2019.C                             TpcSecRowB.20190619.025126.C
ln -sf TpcSecRowB.9p2GeV_2019.C                             TpcSecRowB.20190628.081931.C
ln -sf TpcSecRowB.4p59GeV_fixedTarget_2019.C                TpcSecRowB.20190629.032834.C
ln -sf TpcSecRowB.9p2GeV_2019.C                             TpcSecRowB.20190702.144151.C
ln -sf TpcSecRowB.31GeV_fixedTarget_2019.C                  TpcSecRowB.20190709.032312.C
ln -sf TpcSecRowB.AuAu200_2019.C                            TpcSecRowB.20190710.110157.C
ln -sf TpcSecRowB.9p2GeV_2019.C                             TpcSecRowB.20190715.085141.C
#[l409] ~/bin $ CreateRunXXLinks.pl
ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191208.091308.C # 2019-12-08 09:13:08
ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191210.185902.C
ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191210.194613.C
ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191210.213703.C
ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191211.001242.C
ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191211.074956.C
ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191211.150037.C
ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191211.181332.C
ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191211.211826.C
ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191212.024611.C
ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191212.055939.C
ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191212.120033.C
ln -sf TpcSecRowB.5p75GeV_fixedTarget_2020.C                TpcSecRowB.20191221.154021.C # 2019-12-21 15:40:21
ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20191221.190032.C # 2019-12-21 19:00:32

ln -sf TpcSecRowB.31p2GeV_fixedTarget_2020.C                TpcSecRowB.20200128.182912.C # 2020-01-28 18:29:12
ln -sf TpcSecRowB.9p8GeV_fixedTarget_2020.C                 TpcSecRowB.20200130.005840.C # 2020-01-30 00:58:40
ln -sf TpcSecRowB.9p2GeV_2020.C                             TpcSecRowB.20200131.012112.C # 2020-01-31 01:21:12
ln -sf TpcSecRowB.9p8GeV_fixedTarget_2020.C                 TpcSecRowB.20200131.050328.C # 2020-01-31 05:03:28
ln -sf TpcSecRowB.19p5GeV_fixedTarget_2020.C                TpcSecRowB.20200201.191904.C # 2020-02-01 19:19:04
ln -sf TpcSecRowB.13p5GeV_fixedTarget_2020.C                TpcSecRowB.20200202.160409.C # 2020-02-02 16:04:09
ln -sf TpcSecRowB.9p2GeV_2020.C                             TpcSecRowB.20200203.202534.C # 2020-02-03 20:25:34
ln -sf TpcSecRowB.7p3GeV_fixedTarget_2020.C                 TpcSecRowB.20200204.053518.C # 2020-02-04 05:35:18
ln -sf TpcSecRowB.9p2GeV_2020.C                             TpcSecRowB.20200205.144626.C # 2020-02-05 14:46:26
ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20200210.220428.C # 2020-02-10 22:04:28
ln -sf TpcSecRowB.5p75GeV_fixedTarget_2020.C                TpcSecRowB.20200213.152339.C # 2020-02-13 15:23:39
ln -sf TpcSecRowB.11p5GeV_2020.C                            TpcSecRowB.20200214.143742.C # 2020-02-14 14:37:42
ln -sf TpcSecRowB.9p2GeVb_2020.C                            TpcSecRowB.20200224.230740.C # 2020-02-24 23:07:40
ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200617.233208.C # 2020-06-17 23:32:08
ln -sf TpcSecRowB.26p5GeV_fixedTarget_2020.C                TpcSecRowB.20200730.015354.C # 2020-07-30 01:53:54
ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200730.042836.C # 2020-07-30 04:28:36
                                                                                         2020-08-13 21:32:51
ln -sf TpcSecRowB.26p5GeV_fixedTarget_2020.C                TpcSecRowB.20200819.192253.C # 2020-08-19 19:22:53  
ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200820.055402.C # 2020-08-20 05:54:02 
ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200821.223324.C  
ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200823.042812.C  
ln -sf TpcSecRowB.26p5GeV_fixedTarget_2020.C                TpcSecRowB.20200813.213251.C  
ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200814.054357.C  
ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200826.052959.C  
ln -sf TpcSecRowB.26p5GeV_fixedTarget_2020.C                TpcSecRowB.20200827.143507.C # 2020-08-27 14:35:07 
ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200828.064250.C # 2020-08-28 06:42:50 
ln -sf TpcSecRowB.9p2GeVc_2020.C                            TpcSecRowB.20200830.174301.C  
ln -sf TpcSecRowB.7p7GeV_2020.C                             TpcSecRowB.20200902.105440.C # 2020-09-02 10:54:40 
ln -sf TpcSecRowB.7p7GeV_2020.C                             TpcSecRowB.20200903.171115.C # 2020-09-03 17:11:15
ln -sf TpcSecRowB.7p7GeV_2020.C                             TpcSecRowB.20200905.043324.C 
ln -sf TpcSecRowB.26p5GeV_fixedTarget_2020.C                TpcSecRowB.20200907.221141.C # 2020-09-07 22:11:41
ln -sf TpcSecRowB.7p7GeV_2020.C                             TpcSecRowB.20200910.041639.C # 2020-09-10 04:16:39
ln -sf TpcSecRowB.7p7GeV_2020.C                             TpcSecRowB.20200911.195640.C 
ln -sf TpcSecRowB.26p5GeV_fixedTarget_2020.C                TpcSecRowB.20200912.102033.C 
                                                                                           2020-09-12 10:20:33
											   2021-01-29 00:00:06
