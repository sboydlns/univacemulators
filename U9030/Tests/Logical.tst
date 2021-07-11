#
# Logical instructions test
#

# N

# Zero result
SET R1=$AA000000
SET MEM $10W=$55000000
N R1,$10
TEST R1=0
TEST CC=0

# Non zero result
SET R1=$55000000
SET MEM $10W=$05000000
N R1,$10
TEST R1=$05000000
TEST CC=1

# NI

# Zero result
SET MEM $10B=$55
NI $10,$AA
TEST MEM $10B=0
TEST CC=0

# Non zero result
SET MEM $10B=$55
NI $10,$05
TEST MEM $10B=$05
TEST CC=1
# NR

# Zero result
SET R1=$AA000000
SET R2=$55000000
NR R1,R2
TEST R1=0
TEST CC=0

# Non zero result
SET R1=$55000000
SET R2=$05000000
NR R1,R2
TEST R1=$05000000
TEST CC=1

# O

# Zero result
SET R1=0
SET MEM $10W=0
O R1,$10
TEST R1=0
TEST CC=0

# Non zero result
SET R1=$55000000
SET MEM $10W=$AA000000
O R1,$10
TEST R1=$FF000000
TEST CC=1

# OI

# Zero result
SET MEM $10B=0
OI $10,0
TEST MEM $10B=0
TEST CC=0

# Non zero result
SET MEM $10B=$55
OI $10,$AA
TEST MEM $10B=$FF
TEST CC=1

# OR

# Zero result
SET R1=0
SET R2=0
OR R1,R2
TEST R1=0
TEST CC=0

# Non zero result
SET R1=$55000000
SET R2=$AA000000
OR R1,R2
TEST R1=$FF000000
TEST CC=1

# X

# Zero result
SET R1=$55000000
SET MEM $10W=$55000000
X R1,$10
TEST R1=0
TEST CC=0

# Non zero result
SET R1=$55000000
SET MEM $10W=$A5000000
X R1,$10
TEST R1=$F0000000
TEST CC=1

# XI

# Zero result
SET MEM $10B=$55
XI $10,$55
TEST MEM $10B=0
TEST CC=0

# Non zero result
SET MEM $10B=$55
XI $10,$A5
TEST MEM $10B=$F0
TEST CC=1

# XR

# Zero result
SET R1=$55000000
SET R2=$55000000
XR R1,R2
TEST R1=0
TEST CC=0

# Non zero result
SET R1=$55000000
SET R2=$A5000000
XR R1,R2
TEST R1=$F0000000
TEST CC=1

# TM

# All zero
SET MEM $10B=$55
TM $10,$AA
TEST CC=0

# Mixed ones and zeros
SET MEM $10B=$55
TM $10,$77
TEST CC=1

# All ones
SET MEM $10B=$55
TM $10,$55
TEST CC=3

#SLA

# Positive non zero result
SET R1=$01000000
SLA R1,4
TEST R1=$10000000
TEST CC=2

# Zero result
SET R1=0
SLA R1,1
TEST R1=0
TEST CC=0

# Positive zero result, overflow
SET R1=$01
SLA R1,32
TEST R1=0
TEST CC=3

# Negative non zero result
SET R1=-1
SLA R1,4
TEST R1=-16
TEST CC=1

# Negative min overflow
SET R1=-1
SLA R1,32
TEST R1=$80000000
TEST CC=3

# Zero shift
SET R1=$FF000000
SLA R1,0
TEST R1=$FF000000
TEST CC=1

# > 31 shift positive
SET R1=1
SLA R1,32
TEST R1=0
TEST CC=3

# > 31 shift negative
SET R1=-11
SLA R1,32
TEST R1=$80000000
TEST CC=3

#SLDA

# Positive non zero result
SET R0=$FF
SET R1=$FF000000
SLDA R0,4
TEST R0=$FFF
TEST R1=$F0000000
TEST CC=2

# Zero result
SET R0=0
SET R1=0
SLDA R0,1
TEST R0=0
TEST R1=0
TEST CC=0

# Positive result, overflow
SET R0=$7FFFFFFF
SET R1=$FF000000
SLDA R0,4
TEST R0=$7FFFFFFF
TEST R1=$F0000000
TEST CC=3

# Negative non zero result
SET R0=-1
SET R1=-1
SLDA R0,4
TEST R0=-1
TEST R1=-16
TEST CC=1

# Negative min overflow
SET R0=$80000000
SET R1=1
SLDA R0,1
TEST R0=$80000000
TEST R1=$2
TEST CC=3

# Zero shift
SET R0=$80000000
SET R1=1
SLDA R0,0
TEST R0=$80000000
TEST R1=$1
TEST CC=1

# SLDL

SET R0=1
SET R1=1
SLDL R0,1
TEST R0=2
TEST R1=2

# SLL

# Non zero shift
SET R1=$FF000000
SLL R1,4
TEST R1=$F0000000

# zero shift
SET R1=$FF000000
SLL R1,0
TEST R1=$FF000000

# >31 shift
SET R1=1
SLL R1,32
TEST R1=0

# SRA

# Zero result
SET R1=$FF
SRA R1,8
TEST R1=0
TEST CC=0

# Positive result
SET R1=$7FFFFFFF
SRA R1,4
TEST R1=$7FFFFFF
TEST CC=2

# Negative result
SET R1=$F0FFFF0F
SRA R1,4
TEST R1=$FF0FFFF0
TEST CC=1

# Max shift
SET R1=$FFFFFFFF
SRA R1,63
TEST R1=$FFFFFFFF
TEST CC=1

# Zero shift
SET R1=$F0F0F0F0
SRA R1,0
TEST R1=$F0F0F0F0
TEST CC=1

# > 31 shift positive
SET R1=$7FFFFFFF
SRA R1,32
TEST R1=0
TEST CC=0

# > 31 shift negative
SET R1=$80000000
SRA R1,32
TEST R1=-1
TEST CC=1

#SRDA

# Positive result
SET R0=$FF
SET R1=$FF000000
SRDA 0,4
TEST R0=$0F
TEST R1=$FFF00000
TEST CC=2

# Negative result
SET R0=-1
SET R1=$FF000000
SRDA R0,4
TEST R0=-1
TEST R1=$FFF00000
TEST CC=1

# Zero result
SET R0=0
SET R1=$FF
SRDA 0,8
TEST R0=0
TEST R1=0
TEST CC=0

# Zero Shift
SET R0=$FF
SET R1=$FF
SRDA R0,0
TEST R0=$FF
TEST R1=$FF
TEST CC=2

# SRDL

SET R0=2
SET R1=2
SRDL R0,1
TEST R0=1
TEST R1=1

# SRL

# Non zero shift
SET R1=$FF000000
SRL R1,4
TEST R1=$0FF00000

# zero shift
SET R1=$FF000000
SRL R1,0
TEST R1=$FF000000

# > 31 shift
SET R1=$FF000000
SRL R1,32
TEST R1=0

# TS

# test cell not set
SET MEM $10B=0
TS $10,0
TEST MEM $10B=$FF
TEST CC=0

# test cell set
SET MEM $10B=$FF
TS $10,0
TEST MEM $10B=$FF
TEST CC=1

# LCR

# Positive argument
SET R1=1
LCR R1,R1
TEST R1=-1
TEST CC=1

# Negative argument
SET R1=-1
LCR R1,R1
TEST R1=1
TEST CC=2

# Zero argument
SET R1=0
LCR R1,R1
TEST R1=0
TEST CC=0

# Overflow
SET R1=$80000000
LCR R1,R1
TEST R1=$80000000
TEST CC=3

# LPR

# Positive argument
SET R1=1
LPR R1,R1
TEST R1=1
TEST CC=2

# Negative argument
SET R1=-1
LPR R1,R1
TEST R1=1
TEST CC=2

# Zero argument
SET R1=0
LPR R1,R1
TEST R1=0
TEST CC=0

# Overflow
SET R1=$80000000
LPR R1,R1
TEST R1=$80000000
TEST CC=3
