module Problems (problems, ioProblems) where

import Group1
import Group2
import Group3
import Group4

pxx :: (Integral a) => a
pxx = fromIntegral (-1)

problems :: (Integral a) => [a]
problems = [
    p1,  p2,  p3,  p4,  p5,  p6,  p7,  p8,  p9,  p10,
    p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
    p21, pxx, p23, p24, p25, p26, p27, p28, p29, p30,
    p31, p32, p33, p34, p35, p36, p37, p38, p39
    -- p31, p32, p33, p34, p35, p36, p37, p38, p39, p40
  ]

ioProblems :: (Integral a) => Int -> String -> a
ioProblems 22 = p22
