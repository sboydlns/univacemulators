#
# Floating point tests
#

# LD / STD
SET MEM $10G=1.75
LD R0,$10
STD R0,$20
TEST MEM $20G=1.75
TEST MEM $20D=$411C000000000000

SET MEM $10G=-1.75
LD R0,$10
STD R0,$20
TEST MEM $20G=-1.75
TEST MEM $20D=$C11C000000000000

SET MEM $10G=0
LD R0,$10
STD R0,$20
TEST MEM $20G=0
TEST MEM $20D=$0000000000000000

SET MEM $10G=1.1
LD R0,$10
STD R0,$20
TEST MEM $20G=1.1
TEST MEM $20D=$411199999999999A

# LDR
SET MEM $10G=1.75
LD R0,$10
LDR R2,R0
STD R2,$20
TEST MEM $20G=1.75
TEST MEM $20D=$411C000000000000

# LE / STE
SET MEM $10F=1.75
LE R0,$10
STE R0,$20
TEST MEM $20F=1.75
TEST MEM $20W=$411C0000

SET MEM $10F=1.1
LE R0,$10
LER R2,R0
STE R0,$20
TEST MEM $20F=1.1
TEST MEM $20W=$41119999

# CHECK LOW WORD REMAINS UNCHANGED
SET MEM $10D=$FFFFFFFFFFFFFFF
SET MEM $18F=1.5
LD R0,$10
LE R0,$18
STD R0,$20
TEST MEM $20F=1.5
TEST MEM $20D=$41180000FFFFFFFF

# LTDR

SET MEM $10G=1.5
LD R0,$10
LTDR R2,R0
STD R2,$20
TEST MEM $20G=1.5
TEST CC=2

SET MEM $10G=-1.5
LD R0,$10
LTDR R2,R0
STD R2,$20
TEST MEM $20G=-1.5
TEST CC=1

SET MEM $10G=0
LD R0,$10
LTDR R2,R0
STD R2,$20
TEST MEM $20G=0
TEST CC=0

# LTER

SET MEM $10F=1.5
LE R0,$10
LTER R2,R0
STE R2,$20
TEST MEM $20F=1.5
TEST CC=2

SET MEM $10F=-1.5
LE R0,$10
LTER R2,R0
STE R2,$20
TEST MEM $20F=-1.5
TEST CC=1

SET MEM $10F=0
LE R0,$10
LTER R2,R0
STE R2,$20
TEST MEM $20F=0
TEST CC=0

# LCDR

SET MEM $10G=1.5
LD R0,$10
LCDR R2,R0
STD R2,$20
TEST MEM $20G=-1.5
TEST CC=1

SET MEM $10G=-1.5
LD R0,$10
LCDR R2,R0
STD R2,$20
TEST MEM $20G=1.5
TEST CC=2

SET MEM $10G=0
LD R0,$10
LCDR R2,R0
STD R2,$20
TEST MEM $20D=$8000000000000000
TEST CC=0

# LCER

SET MEM $10F=1.5
LE R0,$10
LCER R2,R0
STE R2,$20
TEST MEM $20F=-1.5
TEST CC=1

SET MEM $10F=-1.5
LE R0,$10
LCER R2,R0
STE R2,$20
TEST MEM $20F=1.5
TEST CC=2

SET MEM $10F=0
LE R0,$10
LCER R2,R0
STE R2,$20
TEST MEM $20W=$80000000
TEST CC=0

# LPDR

SET MEM $10G=1.5
LD R0,$10
LPDR R2,R0
STD R2,$20
TEST MEM $20G=1.5
TEST CC=2

SET MEM $10G=-1.5
LD R0,$10
LPDR R2,R0
STD R2,$20
TEST MEM $20G=1.5
TEST CC=2

SET MEM $10G=0
LD R0,$10
LPDR R2,R0
STD R2,$20
TEST MEM $20D=$0000000000000000
TEST CC=0

# LPER

SET MEM $10F=1.5
LE R0,$10
LPER R2,R0
STE R2,$20
TEST MEM $20F=1.5
TEST CC=2

SET MEM $10F=-1.5
LE R0,$10
LPER R2,R0
STE R2,$20
TEST MEM $20F=1.5
TEST CC=2

SET MEM $10F=0
LE R0,$10
LPER R2,R0
STE R2,$20
TEST MEM $20W=$00000000
TEST CC=0

# LNDR

SET MEM $10G=1.5
LD R0,$10
LNDR R2,R0
STD R2,$20
TEST MEM $20G=-1.5
TEST CC=1

SET MEM $10G=-1.5
LD R0,$10
LNDR R2,R0
STD R2,$20
TEST MEM $20G=-1.5
TEST CC=1

SET MEM $10G=0
LD R0,$10
LNDR R2,R0
STD R2,$20
TEST MEM $20D=$8000000000000000
TEST CC=0

# LNER

SET MEM $10F=1.5
LE R0,$10
LNER R2,R0
STE R2,$20
TEST MEM $20F=-1.5
TEST CC=1

SET MEM $10F=-1.5
LE R0,$10
LNER R2,R0
STE R2,$20
TEST MEM $20F=-1.5
TEST CC=1

SET MEM $10F=0
LE R0,$10
LNER R2,R0
STE R2,$20
TEST MEM $20W=$80000000
TEST CC=0

# HDR

SET MEM $10G=1
LD R0,$10
HDR R2,R0
STD R2,$20
TEST MEM $20D=$4108000000000000
ADR R2,R2
STD R2,$28
TEST MEM $28G=1.0

SET MEM $10G=-1
LD R0,$10
HDR R2,R0
STD R2,$20
TEST MEM $20D=$C108000000000000
ADR R2,R2
STD R2,$28
TEST MEM $28G=-1.0

# HER

SET MEM $10F=1
LE R0,$10
HER R2,R0
STE R2,$20
TEST MEM $20W=$41080000
AER R2,R2
STE R2,$28
TEST MEM $28W=$41100000
TEST MEM $28F=1.0

SET MEM $10F=-1
LE R0,$10
HER R2,R0
STE R2,$20
TEST MEM $20W=$C1080000
AER R2,R2
STE R2,$28
TEST MEM $28W=$C1100000
TEST MEM $28F=-1.0

# AD

SET MEM $10G=1.5
SET MEM $18G=1.25
LD R0,$10
AD R0,$18
STD R0,$20
TEST MEM $20D=$412C000000000000
TEST MEM $20G=2.75
TEST CC=2

SET MEM $10G=1.5
SET MEM $18G=-1.5
LD R0,$10
AD R0,$18
STD R0,$20
TEST MEM $20D=0
TEST MEM $20G=0
TEST CC=0

SET MEM $10G=1.5
SET MEM $18G=-1.25
LD R0,$10
AD R0,$18
STD R0,$20
TEST MEM $20D=$4040000000000000
TEST MEM $20G=0.25
TEST CC=2

SET MEM $10G=1.5
SET MEM $18G=-1.75
LD R0,$10
AD R0,$18
STD R0,$20
TEST MEM $20D=$C040000000000000
TEST MEM $20G=-0.25
TEST CC=1

SET MEM $10G=15000000.0
SET MEM $18G=15000000.5
LD R0,$10
AD R0,$18
STD R0,$20
TEST MEM $20D=$471C9C3808000000
TEST MEM $20G=30000000.5
TEST CC=2

# AE

SET MEM $10F=1.5
SET MEM $18F=1.25
LE R0,$10
AE R0,$18
STE R0,$20
TEST MEM $20W=$412C0000
TEST MEM $20F=2.75
TEST CC=2

SET MEM $10F=1.5
SET MEM $18F=-1.5
LE R0,$10
AE R0,$18
STE R0,$20
TEST MEM $20W=0
TEST MEM $20F=0
TEST CC=0

SET MEM $10F=1.5
SET MEM $18F=-1.25
LE R0,$10
AE R0,$18
STE R0,$20
TEST MEM $20W=$40400000
TEST MEM $20F=0.25
TEST CC=2

SET MEM $10F=1.5
SET MEM $18F=-1.75
LE R0,$10
AE R0,$18
STE R0,$20
TEST MEM $20W=$C0400000
TEST MEM $20F=-0.25
TEST CC=1

# ADR

SET MEM $10G=1.5
SET MEM $18G=1.25
LD R0,$10
LD R2,$18
ADR R0,R2
STD R0,$20
TEST MEM $20D=$412C000000000000
TEST MEM $20G=2.75
TEST CC=2

SET MEM $10G=1.5
SET MEM $18G=-1.5
LD R0,$10
LD R2,$18
ADR R0,R2
STD R0,$20
TEST MEM $20D=0
TEST MEM $20G=0
TEST CC=0

SET MEM $10G=1.5
SET MEM $18G=-1.25
LD R0,$10
LD R2,$18
ADR R0,R2
STD R0,$20
TEST MEM $20D=$4040000000000000
TEST MEM $20G=0.25
TEST CC=2

SET MEM $10G=1.5
SET MEM $18G=-1.75
LD R0,$10
LD R2,$18
ADR R0,R2
STD R0,$20
TEST MEM $20D=$C040000000000000
TEST MEM $20G=-0.25
TEST CC=1

SET MEM $10G=15000000.0
SET MEM $18G=15000000.5
LD R0,$10
LD R2,$18
ADR R0,R2
STD R0,$20
TEST MEM $20D=$471C9C3808000000
TEST MEM $20G=30000000.5
TEST CC=2

# AER

SET MEM $10F=1.5
SET MEM $18F=1.25
LE R0,$10
LE R2,$18
AER R0,R2
STE R0,$20
TEST MEM $20W=$412C0000
TEST MEM $20F=2.75
TEST CC=2

SET MEM $10F=1.5
SET MEM $18F=-1.5
LE R0,$10
LE R2,$18
AER R0,R2
STE R0,$20
TEST MEM $20W=0
TEST MEM $20F=0
TEST CC=0

SET MEM $10F=1.5
SET MEM $18F=-1.25
LE R0,$10
LE R2,$18
AER R0,R2
STE R0,$20
TEST MEM $20W=$40400000
TEST MEM $20F=0.25
TEST CC=2

SET MEM $10F=1.5
SET MEM $18F=-1.75
LE R0,$10
LE R2,$18
AER R0,R2
STE R0,$20
TEST MEM $20W=$C0400000
TEST MEM $20F=-0.25
TEST CC=1

# SD

SET MEM $10G=1.5
SET MEM $18G=1.25
LD R0,$10
SD R0,$18
STD R0,$20
TEST MEM $20G=0.25
TEST CC=2

SET MEM $10G=1.5
SET MEM $18G=1.5
LD R0,$10
SD R0,$18
STD R0,$20
TEST MEM $20G=0
TEST CC=0

SET MEM $10G=1.5
SET MEM $18G=-1.25
LD R0,$10
SD R0,$18
STD R0,$20
TEST MEM $20G=2.75
TEST CC=2

SET MEM $10G=1.5
SET MEM $18G=-1.75
LD R0,$10
SD R0,$18
STD R0,$20
TEST MEM $20G=3.25
TEST CC=2

SET MEM $10G=15000000.0
SET MEM $18G=15000000.5
LD R0,$10
SD R0,$18
STD R0,$20
TEST MEM $20G=-0.5
TEST CC=1

# SE

SET MEM $10F=1.5
SET MEM $18F=1.25
LE R0,$10
SE R0,$18
STE R0,$20
TEST MEM $20F=0.25
TEST CC=2

SET MEM $10F=1.5
SET MEM $18F=1.5
LE R0,$10
SE R0,$18
STE R0,$20
TEST MEM $20F=0
TEST CC=0

SET MEM $10F=1.5
SET MEM $18F=-1.25
LE R0,$10
SE R0,$18
STE R0,$20
TEST MEM $20F=2.75
TEST CC=2

SET MEM $10F=1.5
SET MEM $18F=-1.75
LE R0,$10
SE R0,$18
STE R0,$20
TEST MEM $20F=3.25
TEST CC=2

# SDR

SET MEM $10G=1.5
SET MEM $18G=1.25
LD R0,$10
LD R2,$18
SDR R0,R2
STD R0,$20
TEST MEM $20G=0.25
TEST CC=2

SET MEM $10G=1.5
SET MEM $18G=1.5
LD R0,$10
LD R2,$18
SDR R0,R2
STD R0,$20
TEST MEM $20G=0
TEST CC=0

SET MEM $10G=1.5
SET MEM $18G=-1.25
LD R0,$10
LD R2,$18
SDR R0,R2
STD R0,$20
TEST MEM $20G=2.75
TEST CC=2

SET MEM $10G=1.5
SET MEM $18G=-1.75
LD R0,$10
LD R2,$18
SDR R0,R2
STD R0,$20
TEST MEM $20G=3.25
TEST CC=2

SET MEM $10G=15000000.0
SET MEM $18G=15000000.5
LD R0,$10
LD R2,$18
SDR R0,R2
STD R0,$20
TEST MEM $20G=-0.5
TEST CC=1

# SER

SET MEM $10F=1.5
SET MEM $18F=1.25
LE R0,$10
LE R2,$18
SER R0,R2
STE R0,$20
TEST MEM $20F=0.25
TEST CC=2

SET MEM $10F=1.5
SET MEM $18F=1.5
LE R0,$10
LE R2,$18
SER R0,R2
STE R0,$20
TEST MEM $20F=0
TEST CC=0

SET MEM $10F=1.5
SET MEM $18F=-1.25
LE R0,$10
LE R2,$18
SER R0,R2
STE R0,$20
TEST MEM $20F=2.75
TEST CC=2

SET MEM $10F=1.5
SET MEM $18F=-1.75
LE R0,$10
LE R2,$18
SER R0,R2
STE R0,$20
TEST MEM $20F=3.25
TEST CC=2

# CD

SET MEM $10G=1
SET MEM $18G=2
LD R0,$10
CD R0,$10
TEST CC=0

CD R0,$18
TEST CC=1

LD R0,$18
CD R0,$10
TEST CC=2

# CDR

SET MEM $10G=1
SET MEM $18G=2
LD R0,$10
LD R2,$10
CDR R0,R2
TEST CC=0

LD R2,$18
CDR R0,R2
TEST CC=1

CDR R2,R0
TEST CC=2

# CE

SET MEM $10F=1
SET MEM $18F=2
LE R0,$10
CE R0,$10
TEST CC=0

CE R0,$18
TEST CC=1

LE R0,$18
CE R0,$10
TEST CC=2

# CER

SET MEM $10F=1
SET MEM $18F=2
LE R0,$10
LE R2,$10
CER R0,R2
TEST CC=0

LE R2,$18
CER R0,R2
TEST CC=1

CER R2,R0
TEST CC=2

# MD

SET MEM $10G=123.45
SET MEM $18G=2
LD R0,$10
MD R0,$18
STD R0,$20
TEST MEM $20G=246.9

SET MEM $10G=123.45
SET MEM $18G=-2.5
LD R0,$10
MD R0,$18
STD R0,$20
TEST MEM $20G=-308.625

SET MEM $10G=123.45
SET MEM $18G=0
LD R0,$10
MD R0,$18
STD R0,$20
TEST MEM $20G=0

# MDR

SET MEM $10G=123.45
SET MEM $18G=2
LD R0,$10
LD R2,$18
MDR R0,R2
STD R0,$20
TEST MEM $20G=246.9

SET MEM $10G=123.45
SET MEM $18G=-2.5
LD R0,$10
LD R2,$18
MDR R0,R2
STD R0,$20
TEST MEM $20G=-308.625

SET MEM $10G=123.45
SET MEM $18G=0
LD R0,$10
LD R2,$18
MDR R0,R2
STD R0,$20
TEST MEM $20G=0

# ME

SET MEM $10F=123.45
SET MEM $18F=2
LE R0,$10
ME R0,$18
STE R0,$20
TEST MEM $20F=246.9

SET MEM $10F=123.45
SET MEM $18F=-2.5
LE R0,$10
ME R0,$18
STE R0,$20
TEST MEM $20F=-308.625

SET MEM $10F=123.45
SET MEM $18F=0
LE R0,$10
ME R0,$18
STE R0,$20
TEST MEM $20F=0

# MER

SET MEM $10F=123.45
SET MEM $18F=2
LE R0,$10
LE R2,$18
MER R0,R2
STE R0,$20
TEST MEM $20F=246.9

SET MEM $10F=123.45
SET MEM $18F=-2.5
LE R0,$10
LE R2,$18
MER R0,R2
STE R0,$20
TEST MEM $20F=-308.625

SET MEM $10F=123.45
SET MEM $18F=0
LE R0,$10
LE R2,$18
MER R0,R2
STE R0,$20
TEST MEM $20F=0

# DD

SET MEM $10G=249
SET MEM $18G=2
LD R0,$10
DD R0,$18
STD R0,$20
TEST MEM $20G=124.5

SET MEM $10G=249
SET MEM $18G=-2
LD R0,$10
DD R0,$18
STD R0,$20
TEST MEM $20G=-124.5

# DDR

SET MEM $10G=249
SET MEM $18G=2
LD R0,$10
LD R2,$18
DDR R0,R2
STD R0,$20
TEST MEM $20G=124.5

SET MEM $10G=249
SET MEM $18G=-2
LD R0,$10
LD R2,$18
DDR R0,R2
STD R0,$20
TEST MEM $20G=-124.5

# DE

SET MEM $10F=249
SET MEM $18F=2
LE R0,$10
DE R0,$18
STE R0,$20
TEST MEM $20F=124.5

SET MEM $10F=249
SET MEM $18F=-2
LE R0,$10
DE R0,$18
STE R0,$20
TEST MEM $20F=-124.5

# DER

SET MEM $10F=249
SET MEM $18F=2
LE R0,$10
LE R2,$18
DER R0,R2
STE R0,$20
TEST MEM $20F=124.5

SET MEM $10F=249
SET MEM $18F=-2
LE R0,$10
LE R2,$18
DER R0,R2
STE R0,$20
TEST MEM $20F=-124.5

