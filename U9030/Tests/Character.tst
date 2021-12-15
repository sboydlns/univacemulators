# MVI

MVI $10,$F0
TEST MEM $10C=0

# MVC

# Overlapping operands
SET MEM $14B=0
MVI $10,$40
MVC $11(3),$10
TEST MEM $10W=$40404040
TEST MEM $14B=0

# Non-overlapping
SET MEM $10C=ABCD
SET MEM $18B=0
MVC $14(4),$10
TEST MEM $14C=ABCD
TEST MEM $18B=0

# MVN

# Overlapping
SET MEM $10C=1234
SET MEM $14B=0
MVN $11(3),$10
TEST MEM $10C=1111
TEST MEM $14B=0

# Non-overlapping
SET MEM $10C=1234
SET MEM $14C=BCDE
SET MEM $18B=0
MVN $14(4),$10
TEST MEM $14C=ABCD
TEST MEM $18B=0

# MVZ

# Overlapping
SET MEM $10C=A234
SET MEM $14B=0
MVZ $11(3),$10
TEST MEM $10C=ABCD
TEST MEM $14B=0

# Non-overlapping
SET MEM $10C=1234
SET MEM $14C=BCDE
SET MEM $18B=0
MVZ $14(4),$10
TEST MEM $14C=2345
TEST MEM $18B=0

# CLC

# Operands equal
SET MEM $10C=ABCDE
SET MEM $14C=ABCDF
CLC $10(4),$14
TEST MEM $10C=ABCD
TEST MEM $14C=ABCD
TEST CC=0

# op1 < op2
SET MEM $10C=ABCDE
SET MEM $14C=ABCEF
CLC $10(4),$14
TEST MEM $10C=ABCD
TEST MEM $14C=ABCE
TEST CC=1

# op1 < op2
SET MEM $10C=ABCEE
SET MEM $14C=ABCDF
CLC $10(4),$14
TEST MEM $10C=ABCE
TEST MEM $14C=ABCD
TEST CC=2

# NC

# Zero result
SET MEM $10W=$01010101
SET MEM $14W=$02020202
NC $10(4),$14
TEST MEM $10W=0
TEST MEM $14W=$02020202
TEST CC=0

# Non-zero result
SET MEM $10W=$01010101
SET MEM $14W=$02020201
NC $10(4),$14
TEST MEM $10W=1
TEST MEM $14W=$02020201
TEST CC=1

# OC

# Zero result
SET MEM $10W=0
SET MEM $14W=0
OC $10(4),$14
TEST MEM $10W=0
TEST MEM $14W=0
TEST CC=0

# Non-zero result
SET MEM $10W=0
SET MEM $14W=$01010101
OC $10(4),$14
TEST MEM $10W=$01010101
TEST MEM $14W=$01010101
TEST CC=1

# XC

# Zero result
SET MEM $10W=$01010101
SET MEM $14W=$01010101
XC $10(4),$14
TEST MEM $10W=0
TEST MEM $14W=$01010101
TEST CC=0

# Non-zero result
SET MEM $10W=$02020201
SET MEM $14W=$01010101
XC $10(4),$14
TEST MEM $10W=$03030300
TEST MEM $14W=$01010101
TEST CC=1

# TR

SET MEM $10C=ABCDEFG
SET MEM $D5C=BCDEFGH
TR $10(7),$14
TEST MEM $10C=BCDEFGH

SET MEM $10C=ABCDEFG
SET MEM $D5C=BCDEFGH
TR $10(6),$14
TEST MEM $10C=BCDEFGG

# TRT

SET R1=$FFFFFFFF
SET R2=$FFFFFFFF
SET MEM $10C=ABCDEFG
SET MEM $D4W=$00000100
SET MEM $D8W=$02000300
TRT $10(7),$14
TEST R1=$FF000011
TEST R2=$FFFFFF01
TEST CC=1

SET R2=0
LA R1,1(0,R1)
TEST R1=$12
TRT 0(5,R1),$14
TEST R1=$13
TEST R2=2
TEST CC=1

LA R1,1(0,R1)
TEST R1=$14
TRT 0(3,R1),$14
TEST R1=$15
TEST R2=3
TEST CC=1

LA R1,1(0,R1)
TEST R1=$16
TRT 0(1,R1),$14
TEST R1=$16
TEST R2=3
TEST CC=0

TRT $10(2),$14
TEST R1=$11
TEST R2=1
TEST CC=2