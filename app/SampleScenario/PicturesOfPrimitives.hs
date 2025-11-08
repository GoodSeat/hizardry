module SampleScenario.PicturesOfPrimitives (picOfPrimitive) where

import Control.CUI

picOfPrimitive :: Int -> Craphic
picOfPrimitive 0 = fromTexts 'X' $ replicate 100 (replicate 100 ' ')
picOfPrimitive n = fromTextsA ' ' 'w' ts'
  where
    ts  = txtOf n
    ts' = (if length ts < 40 then replicate (40 - length ts) "" else []) ++ ts

--  123456789012345678901234567890123456789012345678901234567890123456789012345
--           1         2         3         4         5         6         7

txtOf 0001 = -- treasure chest
  ["                                                                           "   --   1
  ,"                                                                           "   --   2
  ,"                                                                           "   --   3
  ,"                                                                           "   --   4
  ,"                                                                           "   --   5
  ,"                                                                           "   --   6
  ,"                                                                           "   --   7
  ,"                                                                           "   --   8
  ,"                                                                           "   --   9
  ,"                                                                           "   --   10
  ,"                                                                           "   --   11
  ,"                                                                           "   --   12
  ,"                                                                           "   --   13
  ,"                                                                           "   --   14
  ,"                                                                           "   --   15
  ,"                                                                           "   --   16
  ,"                                                                           "   --   17
  ,"                                                                           "   --   18
  ,"                                                                           "   --   19
  ,"                                                                           "   --   20
  ,"                                                                           "   --   21
  ,"                                            HmxvT                          "   --   22
  ,"                                MM@MHHbkk+?4bbWHh_7                        "   --   23
  ,"                    MNggK7TH@@Hm+JWHWfWpfWW,,yWWWW,d                       "   --   24
  ,"                  NNN#HHHHa-?pHWfhYdkfWWfWWW2dpWWpb_                       "   --   25
  ,"                  #H#MHHHHMN,,WfWpk1dHHppWpbN-jkWkM~,                      "   --   26
  ,"                 MMHH#H@HHH@N??pbHq[-WHNAJjZ9-,1uggl(                      "   --   27
  ,"                 M#MMMkMMMMM#[.WH9Y9..JdW3JWM-(HHpbr-                      "   --   28
  ,"                 MHMMHHN#MNNH8?1QggH_(HMNW+WH<WWkWW$(                      "   --   29
  ,"                 #MHMMMMMM##MH+jkWbW:(MHWHWWH_ZHHHW]-                      "   --   30
  ,"                 #HHMMNNMMM#MMHdKHpW<(HbWVVWHdwHNkWHM                      "   --   31
  ,"                 #H#MHMMHHHHHXjdWWWHe(HkfWHWH0xMMHHSd                      "   --   32
  ,"                 M#H#HMHMH@@@VWdQHkHzWNHpWHMM(RSm2C~J                      "   --   33
  ,"                   ##M@MMM@@M0HdMNHHjXHBkg?7>((NggggggH                    "   --   34
  ,"                     NKMMMMMM0HdMMkd9RQQ.gN      NNNN                      "   --   35
  ,"                        NNMHM#WHdmg  gggg                                  "   --   36
  ,"                          Ngggggggggggggmgm                                "   --   37
  ,"                               NNgggggHHggggg                              "   --   38
  ,"                                                                           "   --   39
  ,"                                                                           "]  --   40

txtOf 0002 = -- treasure
  ["                                                                           "   --   1
  ,"                                                                           "   --   2
  ,"                                                                           "   --   3
  ,"                                                                           "   --   4
  ,"                                                                           "   --   5
  ,"                                                                           "   --   6
  ,"                                                                           "   --   7
  ,"                                                                           "   --   8
  ,"                                                                           "   --   9
  ,"                                                                           "   --   10
  ,"                                                                           "   --   11
  ,"                                                                           "   --   12
  ,"                                                                           "   --   13
  ,"                                                                           "   --   14
  ,"                                                                           "   --   15
  ,"                                                                           "   --   16
  ,"                                                                           "   --   17
  ,"                                                                           "   --   18
  ,"                                                                           "   --   19
  ,"                                                                           "   --   20
  ,"                                                                           "   --   21
  ,"                                                                           "   --   22
  ,"                                                                           "   --   23
  ,"                                                                           "   --   24
  ,"                                                                           "   --   25
  ,"                                                                           "   --   26
  ,"                                         .JMN...                           "   --   27
  ,"                                        ..MMMMMMNn,                        "   --   28
  ,"                                     .MMMMMMMMMHUd#                        "   --   29
  ,"                                     .NM#MMMMMB#XyR                        "   --   30
  ,"                            (Wk..  ` .MM@MMMMMNkM#H,                       "   --   31
  ,"                             (HHW&+ggmMMHMMMMMMM#dkr                       "   --   32
  ,"                          ..JJMMNMMNMMMNNMMMMMMMQgKX:                      "   --   33
  ,"                   qgHMMMMMNNMHMMMNMMNMNHMMNmQe    ?M@Wm-                  "   --   34
  ,"                  7TNMMMMMMMMMNWHHMMMMMMMMMMMNNmNk:w?SdMN,                 "   --   35
  ,"              7HY^ 7'= dNKoJMMBVMMMNMMMNNMNMMNKYWMMMMHMMM'4Nm              "   --   36
  ,"                    dMNMMNWN#5MMMHWHMMNMNNMMNMMNMMMNMMmJ, ....             "   --   37
  ,"                `         ''! jNgMMMNMMMmMMNMMMMMHN#B77?                   "   --   38
  ,"                            JMMMMMMMMMMHMMMHMMM                            "   --   39
  ,"                             _TMM=?MMMNQ}`     gMN+                        "   --   40
  ,"                                   (WM#'`      7''`                        "]  --   41
     
txtOf _ = []
