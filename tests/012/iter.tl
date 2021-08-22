(load "../common")

(mtest
  [mapcar identity "A".."D"] ("A" "B" "C" "D")
  [mapcar identity "A1".."C2"] ("A1" "A2" "B1" "B2" "C1" "C2")
  [mapcar identity "D".."A"] ("D" "C" "B" "A")
  [mapcar identity "C2".."A1"] ("C2" "C1" "B2" "B1" "A2" "A1"))

(test
  [maprod append "AA".."DD" "01".."19"]
  ("AA01" "AA02" "AA03" "AA04" "AA05" "AA06" "AA07" "AA08" "AA09"
   "AA11" "AA12" "AA13" "AA14" "AA15" "AA16" "AA17" "AA18" "AA19"
   "AB01" "AB02" "AB03" "AB04" "AB05" "AB06" "AB07" "AB08" "AB09"
   "AB11" "AB12" "AB13" "AB14" "AB15" "AB16" "AB17" "AB18" "AB19"
   "AC01" "AC02" "AC03" "AC04" "AC05" "AC06" "AC07" "AC08" "AC09"
   "AC11" "AC12" "AC13" "AC14" "AC15" "AC16" "AC17" "AC18" "AC19"
   "AD01" "AD02" "AD03" "AD04" "AD05" "AD06" "AD07" "AD08" "AD09"
   "AD11" "AD12" "AD13" "AD14" "AD15" "AD16" "AD17" "AD18" "AD19"
   "BA01" "BA02" "BA03" "BA04" "BA05" "BA06" "BA07" "BA08" "BA09"
   "BA11" "BA12" "BA13" "BA14" "BA15" "BA16" "BA17" "BA18" "BA19"
   "BB01" "BB02" "BB03" "BB04" "BB05" "BB06" "BB07" "BB08" "BB09"
   "BB11" "BB12" "BB13" "BB14" "BB15" "BB16" "BB17" "BB18" "BB19"
   "BC01" "BC02" "BC03" "BC04" "BC05" "BC06" "BC07" "BC08" "BC09"
   "BC11" "BC12" "BC13" "BC14" "BC15" "BC16" "BC17" "BC18" "BC19"
   "BD01" "BD02" "BD03" "BD04" "BD05" "BD06" "BD07" "BD08" "BD09"
   "BD11" "BD12" "BD13" "BD14" "BD15" "BD16" "BD17" "BD18" "BD19"
   "CA01" "CA02" "CA03" "CA04" "CA05" "CA06" "CA07" "CA08" "CA09"
   "CA11" "CA12" "CA13" "CA14" "CA15" "CA16" "CA17" "CA18" "CA19"
   "CB01" "CB02" "CB03" "CB04" "CB05" "CB06" "CB07" "CB08" "CB09"
   "CB11" "CB12" "CB13" "CB14" "CB15" "CB16" "CB17" "CB18" "CB19"
   "CC01" "CC02" "CC03" "CC04" "CC05" "CC06" "CC07" "CC08" "CC09"
   "CC11" "CC12" "CC13" "CC14" "CC15" "CC16" "CC17" "CC18" "CC19"
   "CD01" "CD02" "CD03" "CD04" "CD05" "CD06" "CD07" "CD08" "CD09"
   "CD11" "CD12" "CD13" "CD14" "CD15" "CD16" "CD17" "CD18" "CD19"
   "DA01" "DA02" "DA03" "DA04" "DA05" "DA06" "DA07" "DA08" "DA09"
   "DA11" "DA12" "DA13" "DA14" "DA15" "DA16" "DA17" "DA18" "DA19"
   "DB01" "DB02" "DB03" "DB04" "DB05" "DB06" "DB07" "DB08" "DB09"
   "DB11" "DB12" "DB13" "DB14" "DB15" "DB16" "DB17" "DB18" "DB19"
   "DC01" "DC02" "DC03" "DC04" "DC05" "DC06" "DC07" "DC08" "DC09"
   "DC11" "DC12" "DC13" "DC14" "DC15" "DC16" "DC17" "DC18" "DC19"
   "DD01" "DD02" "DD03" "DD04" "DD05" "DD06" "DD07" "DD08" "DD09"
   "DD11" "DD12" "DD13" "DD14" "DD15" "DD16" "DD17" "DD18" "DD19"))

(mtest
  [maprod append "A".."F" (drop 1 "00".."15")]
  ("A01" "A02" "A03" "A04" "A05" "A10" "A11" "A12" "A13" "A14" "A15"
   "B01" "B02" "B03" "B04" "B05" "B10" "B11" "B12" "B13" "B14" "B15"
   "C01" "C02" "C03" "C04" "C05" "C10" "C11" "C12" "C13" "C14" "C15"
   "D01" "D02" "D03" "D04" "D05" "D10" "D11" "D12" "D13" "D14" "D15"
   "E01" "E02" "E03" "E04" "E05" "E10" "E11" "E12" "E13" "E14" "E15"
   "F01" "F02" "F03" "F04" "F05" "F10" "F11" "F12" "F13" "F14" "F15")
  [maprod append "A".."F" ["00".."99" 1..11]]
  ("A01" "A02" "A03" "A04" "A05" "A06" "A07" "A08" "A09" "A10" "B01"
   "B02" "B03" "B04" "B05" "B06" "B07" "B08" "B09" "B10" "C01" "C02"
   "C03" "C04" "C05" "C06" "C07" "C08" "C09" "C10" "D01" "D02" "D03"
   "D04" "D05" "D06" "D07" "D08" "D09" "D10" "E01" "E02" "E03" "E04"
   "E05" "E06" "E07" "E08" "E09" "E10" "F01" "F02" "F03" "F04" "F05"
   "F06" "F07" "F08" "F09" "F10"))

(test
  [mapcar identity [0..10 3 6]] (3 4 5))
